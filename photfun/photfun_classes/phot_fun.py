import os
import sys
import time
import shutil
from itertools import count
from .phot_table import PhotTable
from .phot_fits import PhotFits
from .phot_psf import PhotPSF
from ..daophot_wrap.docker_handler import init_docker, docker_stop_async
from ..daophot_wrap import (find, phot, pick, create_psf, 
                        sub_fits, allstar, daomatch, daomaster, allframe,
                        create_master, psf_preview)
from ..daophot_opt import opt_daophot_dict, opt_photo_dict, opt_allstar_dict
from ..daophot_opt import opt_daophot_labels, opt_photo_labels, opt_allstar_labels
from ..misc_tools import temp_mkdir, copy_file_noreplace
import numpy as np
import pandas as pd
from joblib import Parallel, delayed, parallel_backend
import nest_asyncio
import asyncio
from itertools import cycle
from tqdm import tqdm
from skopt import gp_minimize
from skopt.space import Real
from datetime import timedelta
import numpy as np
from scipy.optimize import curve_fit
from scipy.stats import pearsonr


nest_asyncio.apply()
LOOP = asyncio.get_event_loop()


def pair_args(*lists, err_msg="Tables must be unique or be the same shape"):
    # Verificar que al menos haya una lista
    if not lists:
        return []
    
    # Longitudes de todas las listas
    lengths = [len(lst) for lst in lists]
    
    # Encontrar la longitud objetivo (la de las listas con más de 1 elemento)
    target_len = None
    for l in lengths:
        if l > 1:
            if target_len is not None and l != target_len:
                raise ValueError(err_msg)
            target_len = l
    
    # Si todas las listas son de longitud 1, target_len será None (se asume 1)
    if target_len is None:
        target_len = 1
    
    # Expandir las listas de longitud 1
    expanded_lists = []
    for lst in lists:
        if len(lst) == 1:
            expanded_lists.append(lst * target_len)
        else:
            expanded_lists.append(lst)
    
    # Hacer zip de las listas expandidas
    return list(zip(*expanded_lists))



def delayed_wrap(func):
    def error_handler_wrap(*args, **kwargs):
        # Verificar si hay algún input path con basename "ERROR.ext"
        error_output = tuple(f"ERROR{ext}" for ext in func.__annotations__['return'])
        error_output = error_output[0] if len(error_output)==1 else error_output
        for arg in args:
            if os.path.basename(str(arg)).startswith("ERROR."):
                print(f"ERROR: input corrupted -> {args} {kwargs}")
                return error_output
        try:
            # Ejecutar la función normalmente
            result = func(*args, **kwargs)
            return result
        except FileNotFoundError as e:
            print(f"ERROR: {e} -> {args} {kwargs}")
            return error_output
        except MemoryError as e:
            print(f"ERROR: {e} -> {args} {kwargs}")
            return error_output
        except RuntimeError as e:
            print(f"ERROR: {e} -> {args} {kwargs}")
            return error_output
    return delayed(error_handler_wrap)


def gaussianity_metric(psf):
    # Crear máscara circular
    n = psf.shape[0]
    radius = ((n-1)/2 - 1)/2
    coords = np.indices((n,n))
    c = (n-1)/2
    mask = ((coords[1]-c)**2 + (coords[0]-c)**2) <= radius**2
    
    x = coords[1][mask].ravel()
    y = coords[0][mask].ravel()
    z = psf[mask].ravel()
    
    # Ajuste de Gaussiana 2D
    def gauss2d(coords, A, x0, y0, sx, sy, B):
        x, y = coords
        return (A * np.exp(-((x-x0)**2/(2*sx**2) + (y-y0)**2/(2*sy**2))) + B).ravel()
    
    p0 = [z.max(), c, c, radius/2, radius/2, z.min()]
    popt, _ = curve_fit(gauss2d, (x, y), z, p0=p0)
    G = gauss2d((x,y), *popt)
    
    # Métricas
    residuals = z - G
    N = len(z)
    rmse = np.sqrt(np.mean(residuals**2))
    nrmse = rmse / popt[0]  # normalizado por amplitud
    rho, _ = pearsonr(z, G)
    
    return pd.DataFrame({
        'A':        [popt[0]],
        'x0,y0':   [(popt[1], popt[2])],
        'sigma_x': [popt[3]],
        'sigma_y': [popt[4]],
        'B':        [popt[5]],
        'RMSE':     [rmse],
        'NRMSE':    [1-nrmse],
        'Pearson':  [1-rho]
    })

class PhotFun:
    def __init__(self):
        self.n_jobs = -1
        self.id_counter = count(start=0, step=1)
        self.tables = []
        self.fits_files = []
        self.psf_files = []

        # Almacenar los diccionarios de opciones como atributos
        self.daophot_opt = opt_daophot_dict.copy()
        self.photo_opt = opt_photo_dict.copy()
        self.allstar_opt = opt_allstar_dict.copy()

        # Verbose en DAOPHOT
        self.daophot_verbose = True

        # Crear la carpeta temporal
        self.working_dir = os.path.abspath(temp_mkdir("photfun_working_dir_0"))

        # Definicion del log
        self.logs = []  # Lista para almacenar logs como tuplas (timestamp, mensaje)
        self._original_stdout = sys.stdout
        sys.stdout = self.Logger(self)  # Redirigir stdout

        # Guardar los diccionarios como archivos de texto
        self._save_opt_files()

        # --- FIND parameters ---
        self.find_sum = 1
        self.find_average = 1

        # --- PICK parameters ---
        self.pick_max_stars = 200
        self.pick_min_mag = 20

        # --- ALLSTAR parameters ---
        self.allstar_recentering = True

        # --- DAOMASTER parameters ---
        self.new_id = False
        self.out_mag = True
        self.out_cor = False
        self.out_raw = False
        self.out_mch = False
        self.out_tfr = False
        self.out_coo = False
        self.out_mtr = False

        self.minimum_frames = 1
        self.minimum_fraction = 0
        self.enough_frames = 1

        self.max_sig = 99
        self.degrees_freedom = 4
        self.critical_radius = 6

        # intiaite Docker (if it can)
        self._force_docker = False
        self.docker_container = init_docker(self.working_dir, self.n_jobs)

        # daophot timeout protection
        self.daophot_timeout = 30

    def reconnect_docker(self):
        # intiaite Docker (if it can)
        self.docker_container = init_docker(self.working_dir, self.n_jobs, 
                                                prev=self.docker_container,
                                                    force_docker=True)

    def disconnect_docker(self):
        docker_stop_async(self.docker_container)
        self.docker_container = [False]

    def add_table(self, path, *args, **kwargs):
        table = PhotTable(path, *args, **kwargs)
        table.id = next(self.id_counter)
        self.tables.append(table)
        if self.daophot_verbose:
            print(f"  -> {table.alias}")
        return table

    def add_fits(self, path, *args, **kwargs):
        fits_file = PhotFits(path, *args, **kwargs)
        fits_file.id = next(self.id_counter)
        self.fits_files.append(fits_file)
        if self.daophot_verbose:
            print(f"  -> {fits_file.alias}")
        return fits_file

    def add_psf(self, path):
        psf_file = PhotPSF(path)
        psf_file.id = next(self.id_counter)
        self.psf_files.append(psf_file)
        # self.psf_preview(psf_file.id, pbar=tqdm)
        if self.daophot_verbose:
            print(f"  -> {psf_file.alias}")
        return psf_file

    def find(self, fits_id, pbar=iter, param_updates=None, pbar_params=iter):
        fits_obj = next(filter(lambda f: f.id==fits_id, self.fits_files), None)
        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")

        # 3. Obtener lista de archivos de opciones
        if param_updates:
            opt_paths = self._generate_opt_paths(param_updates, pbar_params)
        else:
            base_opt = self._save_opt_files(overwrite=True)
            opt_paths = [base_opt]


        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self.docker_container)
        arg_list = []
        for opt_path in opt_paths:
            arg_list += [
                    (fits_path, 
                    opt_path['daophot.opt'],
                    os.path.join(self.working_dir, f"{os.path.basename(fits_path).replace('.fits', '.coo')}"),
                    f"{int(self.find_sum)},{int(self.find_average)}",
                    self.daophot_verbose if len(fits_obj.path)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    10 + self.daophot_timeout if self.daophot_timeout else None
                    )  
                    for fits_path in fits_obj.path
                ]
        # Crear un nuevo event loop para el thread
        asyncio.set_event_loop(LOOP)
        with parallel_backend('loky'):  # Usar loky para mejor compatibilidad
            final_out_coo = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                delayed_wrap(find)(*args) for args in pbar(arg_list)
                                            )
        out_obj_table = self.add_table(final_out_coo)
        return out_obj_table

    def phot(self, fits_id, coo_id, pbar=iter, param_updates=None, pbar_params=iter):
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        coo_table = next(filter(lambda f: f.id == coo_id, self.tables), None)

        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not coo_table:
            raise ValueError(f"No se encontró una tabla con ID {coo_id}")


        # 3. Obtener lista de archivos de opciones
        if param_updates:
            opt_paths = self._generate_opt_paths(param_updates, pbar_params)
        else:
            base_opt = self._save_opt_files(overwrite=True)
            opt_paths = [base_opt]


        input_args = pair_args(fits_obj.path, coo_table.path, 
                    err_msg="La cantidad de archivos FITS y COO no coincide.")

        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self.docker_container)
        arg_list = []
        for opt_path in opt_paths:
            arg_list += [
                    (fits_path, coo_path,
                    opt_path['daophot.opt'],
                    opt_path['photo.opt'],
                    os.path.join(self.working_dir, f"{os.path.basename(fits_path).replace('.fits', '.ap')}"),
                    self.daophot_verbose if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    10 + self.daophot_timeout if self.daophot_timeout else None
                    )  
                    for fits_path, coo_path in input_args
                ]
        # Crear un nuevo event loop para el thread
        asyncio.set_event_loop(LOOP)
        with parallel_backend('loky'):  # Usar loky para mejor compatibilidad
            final_out_ap = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                delayed_wrap(phot)(*args) for args in pbar(arg_list)
                                            )

        return self.add_table(final_out_ap)

    def pick(self, fits_id, ap_id, pbar=iter, param_updates=None, pbar_params=iter):
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        ap_table = next(filter(lambda f: f.id == ap_id, self.tables), None)

        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not ap_table:
            raise ValueError(f"No se encontró una tabla con ID {ap_id}")


        # 3. Obtener lista de archivos de opciones
        if param_updates:
            opt_paths = self._generate_opt_paths(param_updates, pbar_params)
        else:
            base_opt = self._save_opt_files(overwrite=True)
            opt_paths = [base_opt]

        input_args = pair_args(fits_obj.path, ap_table.path, 
                err_msg="La cantidad de archivos FITS y AP no coincide.")

        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self.docker_container)
        arg_list = []
        for opt_path in opt_paths:
            arg_list += [
                    (fits_path, ap_path,
                    opt_path['daophot.opt'],
                    os.path.join(self.working_dir, f"{os.path.basename(fits_path).replace('.fits', '.lst')}"),
                    f"{int(self.pick_max_stars)},{int(self.pick_min_mag)}",
                    self.daophot_verbose if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    10 + self.daophot_timeout if self.daophot_timeout else None
                    )  
                    for fits_path, ap_path in input_args
                ]
        # Crear un nuevo event loop para el thread
        asyncio.set_event_loop(LOOP)
        with parallel_backend('loky'):  # Usar loky para mejor compatibilidad
            final_out_lst = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                delayed_wrap(pick)(*args) for args in pbar(arg_list)
                                            )
            
        return self.add_table(final_out_lst)

    def psf(self, fits_id, ap_id, lst_id, pbar=iter, param_updates=None, pbar_params=iter):
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        ap_table = next(filter(lambda f: f.id == ap_id, self.tables), None)
        lst_table = next(filter(lambda f: f.id == lst_id, self.tables), None)

        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not ap_table:
            raise ValueError(f"No se encontró una tabla con ID {ap_id}")
        if not lst_table:
            raise ValueError(f"No se encontró una tabla con ID {lst_id}")

        # 3. Obtener lista de archivos de opciones
        if param_updates:
            opt_paths = self._generate_opt_paths(param_updates, pbar_params)
        else:
            base_opt = self._save_opt_files(overwrite=True)
            opt_paths = [base_opt]

        output_dir = self.working_dir
        input_args = pair_args(fits_obj.path, ap_table.path, lst_table.path, 
                        err_msg="La cantidad de archivos FITS/AP/LST no coincide.")
      
        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self.docker_container)
        arg_list = []
        for opt_path in opt_paths:
            arg_list += [
                        (fits_path, ap_path, lst_path,
                        opt_path["daophot.opt"],
                        os.path.join(output_dir, f"{os.path.basename(fits_path).replace('.fits', '.psf')}"),
                        os.path.join(output_dir, f"{os.path.basename(fits_path).replace('.fits', '.nei')}"),
                        self.daophot_verbose if len(input_args)<4 else False, # verbose=False
                        next(docker_cycle),
                        self.working_dir,
                        10 + self.daophot_timeout if self.daophot_timeout else None
                        )  
                        for fits_path, ap_path, lst_path in input_args
                    ]

        # Crear un nuevo event loop para el thread
        asyncio.set_event_loop(LOOP)
        with parallel_backend('loky'):  # Usar loky para mejor compatibilidad
            final_out = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                delayed_wrap(create_psf)(*args) for args in pbar(arg_list)
                                            )
        final_out_psf = [r[0] for r in final_out]
        final_out_nei = [r[1] for r in final_out]
            
        return self.add_psf(final_out_psf), self.add_table(final_out_nei)

    def sub(self, fits_id, psf_id, nei_id, lst_id=False, pbar=iter, param_updates=None, pbar_params=iter):
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        psf_obj = next(filter(lambda f: f.id == psf_id, self.psf_files), None)
        nei_table = next(filter(lambda f: f.id == nei_id, self.tables), None)

        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not psf_obj:
            raise ValueError(f"No se encontró una PSF con ID {psf_id}")
        if not nei_table:
            raise ValueError(f"No se encontró una tabla con ID {nei_id}")

        lst_table = None
        if lst_id:
            lst_table = next(filter(lambda f: f.id == lst_id, self.tables), None)
            if not lst_table:
                raise ValueError(f"No se encontró una tabla con ID {lst_id}")
            if nei_table == lst_table:
                raise ValueError(f"No pueden ser iguales la tabla de targets y excepciones")


        # 3. Obtener lista de archivos de opciones
        if param_updates:
            opt_paths = self._generate_opt_paths(param_updates, pbar_params)
        else:
            base_opt = self._save_opt_files(overwrite=True)
            opt_paths = [base_opt]

        input_args = pair_args(fits_obj.path, psf_obj.path, 
                                nei_table.path, lst_table.path if lst_id else [False],
                                    err_msg="La cantidad de archivos FITS/PSF/NEI/LST no coincide.")
        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self.docker_container)
        arg_list = []
        for opt_path in opt_paths:
            arg_list += [
                    (fits_path, psf_path, nei_path,
                    opt_path['daophot.opt'],
                    os.path.join(self.working_dir, f"{os.path.splitext(os.path.basename(fits_path))[0]}_sub.fits"),
                    lst_path,
                    self.daophot_verbose if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    10 + self.daophot_timeout if self.daophot_timeout else None
                    )  
                    for fits_path, psf_path, nei_path, lst_path in input_args
                ]
        # Crear un nuevo event loop para el thread
        asyncio.set_event_loop(LOOP)
        with parallel_backend('loky'):  # Usar loky para mejor compatibilidad
            final_out_subfits = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                delayed_wrap(sub_fits)(*args) for args in pbar(arg_list)
                                            )
        return self.add_fits(final_out_subfits)

    def allstar(self, fits_id, psf_id, ap_id, pbar=iter, param_updates=None, pbar_params=iter):
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        psf_obj = next(filter(lambda f: f.id == psf_id, self.psf_files), None)
        ap_table = next(filter(lambda f: f.id == ap_id, self.tables), None)

        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not psf_obj:
            raise ValueError(f"No se encontró una PSF con ID {psf_id}")
        if not ap_table:
            raise ValueError(f"No se encontró una tabla con ID {ap_id}")

        # 3. Obtener lista de archivos de opciones
        if param_updates:
            opt_paths = self._generate_opt_paths(param_updates, pbar_params)
        else:
            base_opt = self._save_opt_files(overwrite=True)
            opt_paths = [base_opt]

        input_args = pair_args(fits_obj.path, psf_obj.path, ap_table.path,
                                err_msg="La cantidad de archivos FITS/PSF/AP no coincide.")

        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self.docker_container)
        arg_list = []
        for opt_path in opt_paths:
            arg_list += [
                    (fits_path, psf_path, ap_path,
                    opt_path['daophot.opt'],
                    opt_path['allstar.opt'],
                    os.path.join(self.working_dir, f"{os.path.basename(fits_path).replace('.fits', '.als')}"),
                    os.path.join(self.working_dir, f"{os.path.splitext(os.path.basename(fits_path))[0]}_als_sub.fits"),
                    self.allstar_recentering,
                    self.daophot_verbose if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    15 + self.daophot_timeout if self.daophot_timeout else None
                    )  
                    for fits_path, psf_path, ap_path in input_args
                ]
        # Crear un nuevo event loop para el thread
        asyncio.set_event_loop(LOOP)
        with parallel_backend('loky'):  # Usar loky para mejor compatibilidad
            final_out = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                delayed_wrap(allstar)(*args) for args in pbar(arg_list)
                                            )
        final_out_als = [r[0] for r in final_out]
        final_out_subfits = [r[1] for r in final_out]

        return self.add_table(final_out_als), self.add_fits(final_out_subfits)

    def daomatch(self, master_id, id_table_list):
        table_obj = next(filter(lambda f: f.id==master_id, self.tables), None)
        table_list_obj = next(filter(lambda f: f.id==id_table_list, self.tables), None)
        if not table_obj:
            raise ValueError(f"No se encontró la tabla maestra con ID {master_id}")
        if not table_list_obj:
            raise ValueError(f"No se encontró la tabla con ID {table_list_obj}")
        if table_obj==table_list_obj:
            raise ValueError(f"No pueden ser iguales la tabla master y la de sub targets")
          
        table_name = os.path.splitext(os.path.basename(table_obj.path[0]))[0]
        table_list_name = os.path.splitext(os.path.basename(table_list_obj.path[0]))[0]

        output_dir = self.working_dir
        out_mch = os.path.join(output_dir, f"{table_name}_{table_list_name}.mch")
        final_out_mch = daomatch(table_obj.path[0], table_list_obj.path, out_mch, 
                                    self.daophot_verbose if len(table_list_obj.path)<4 else False,
                                    self.docker_container[0], 
                                    self.working_dir,
                                    30 + self.daophot_timeout if self.daophot_timeout else None)
        out_mch_table = self.add_table(final_out_mch)
        return out_mch_table

    def daomaster(self, master_id, mch_id, id_table_list):
        mch_obj = next(filter(lambda f: f.id == mch_id, self.tables), None)
        master_obj = next(filter(lambda f: f.id == master_id, self.tables), None)
        table_list_obj = next(filter(lambda f: f.id == id_table_list, self.tables), None)

        if not mch_obj:
            raise ValueError(f"No se encontró la tabla maestra con ID {mch_id}")
        if not table_list_obj:
            raise ValueError(f"No se encontró la tabla con ID {id_table_list}")
        if master_obj:
            in_path_list = master_id.path + table_list_obj.path
        else:
            in_path_list = table_list_obj.path
        if master_obj == table_list_obj:
            raise ValueError("No pueden ser iguales la tabla master y la de sub targets")

        out_prefix = f"{os.path.basename(mch_obj.path[0]).replace('.mch', '')}"  # vacío como se indica

        # Preparar valores desde atributos del objeto
        min_min_fr = f"{self.minimum_frames},{self.minimum_fraction},{self.enough_frames}"

        # Llamar a daomaster
        result_dict = daomaster(
            in_mch=mch_obj.path[0],
            in_path_list=in_path_list,
            out_dir=self.working_dir,
            out_prefix=out_prefix,
            new_id=self.new_id,
            out_mag=self.out_mag,
            out_cor=self.out_cor,
            out_raw=self.out_raw,
            out_mch=self.out_mch,
            out_tfr=self.out_tfr,
            out_coo=self.out_coo,
            out_mtr=self.out_mtr,
            min_min_fr=min_min_fr,
            max_sig=str(self.max_sig),
            deg_free=str(self.degrees_freedom),
            crit_rad=str(self.critical_radius),
            verbose=self.daophot_verbose if len(table_list_obj.path) < 4 else False,
            use_docker=self.docker_container[0],
            working_dir=self.working_dir,
            timeout=30 + self.daophot_timeout if self.daophot_timeout else None,
        )

        # Agregar cada archivo final como tabla si aplica (solo los .mag, .cor, .raw, .mch, .tfr)
        output_keys = ["out_mag", "out_cor", "out_raw", "out_mch", "out_tfr", "out_coo", "out_mtr"]
        out_tables = []
        for key in output_keys:
            if key in result_dict:
                out_tables.append(self.add_table(result_dict[key]))

        return out_tables

    def allframe(self, fits_id, psf_id, als_id, mch_id, master_id):
        # Obtener listas de entrada
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        psf_obj = next(filter(lambda f: f.id == psf_id, self.psf_files), None)
        als_obj = next(filter(lambda t: t.id == als_id, self.tables), None)
        mch_obj = next(filter(lambda t: t.id == mch_id, self.tables), None)
        master_obj = next(filter(lambda t: t.id == master_id, self.tables), None)

        if not all([fits_obj, psf_obj, als_obj, mch_obj, master_obj]):
            raise ValueError("Uno o más IDs no fueron encontrados en las listas del objeto.")

        # Ejecutar ALLFRAME
        alf_list, fits_list, tfr_file, nmg_file = allframe(
            in_fits=fits_obj.path,
            in_psf=psf_obj.path,
            in_als=als_obj.path,
            in_mch=mch_obj.path[0],
            in_master=master_obj.path[0],
            out_prefix="",
            out_dir=self.working_dir,
            verbose=self.daophot_verbose,
            use_docker=self.docker_container[0] if self.docker_container else None,
            working_dir=self.working_dir,
            timeout=None
        )

        # Registrar resultados
        out_alf = self.add_table(alf_list)
        out_fits = self.add_fits(fits_list)
        out_tfr = self.add_table(tfr_file)
        out_nmg = self.add_table(nmg_file)

        return out_alf, out_fits, out_tfr, out_nmg


    def create_master(self, master_id, mch_id):
        master_obj = next(filter(lambda f: f.id==master_id, self.tables), None)
        mch_obj = next(filter(lambda f: f.id==mch_id, self.tables), None)
        if not master_obj:
            raise ValueError(f"No se encontró la tabla maestra con ID {master_id}")
        if not mch_obj:
            raise ValueError(f"No se encontró la tabla con ID {mch_obj}")
        
        output_dir = self.working_dir
        final_out_path_list = create_master(master_obj.path[0], mch_obj.path[0], 
        						output_dir)
        out_obj_table = self.add_table(final_out_path_list)
        return out_obj_table

    def psf_preview(self, psf_id, pbar=iter):
        psf_obj = next(filter(lambda f: f.id == psf_id, self.psf_files), None)

        if not psf_obj:
            raise ValueError(f"PSF not found: ID {psf_id}")
        if psf_obj.preview_path:
            raise ValueError(f"Preview is already created {psf_id}")
        filename = os.path.splitext(os.path.basename(psf_obj.path[0]))[0]
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(self.working_dir, f"{filename}_PSF_PNG_0")))
        

        base_opt = self._save_opt_files(overwrite=True)
        opt_paths = [base_opt]

        input_args = pair_args(psf_obj.path, 
                                err_msg="La cantidad de archivos FITS/PSF/AP no coincide.")
        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self.docker_container)
        arg_list = []
        for opt_path in opt_paths:
            arg_list += [
                    (psf_path,
                    opt_path['daophot.opt'],
                    os.path.join(temp_dir, f"{os.path.basename(psf_path).replace('.psf', '.png')}"),
                    self.daophot_verbose if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    15 + self.daophot_timeout if self.daophot_timeout else None
                    )  
                    for psf_path,  in input_args
                ]
        # Crear un nuevo event loop para el thread
        asyncio.set_event_loop(LOOP)
        with parallel_backend('loky'):  # Usar loky para mejor compatibilidad
            final_out = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                delayed_wrap(psf_preview)(*args) for args in pbar(arg_list)
                                            )
        psf_obj.preview_path = final_out
        return final_out

    def _save_opt_files(self, overwrite=True):
        opt_files = {
            "daophot.opt": self.daophot_opt,
            "photo.opt": self.photo_opt,
            "allstar.opt": self.allstar_opt,
        }

        saved_paths = {}

        for filename, opt_dict in opt_files.items():
            new_content = "\n".join(f"{key} = {value}" for key, value in opt_dict.items()) + "\n"
            base_path = os.path.join(self.working_dir, filename)
            file_path = base_path

            if overwrite:
                pass  # always write
            elif os.path.exists(file_path):
                # Leer contenido actual
                with open(file_path, "r") as f:
                    current_content = f.read()
                # Comparar con el nuevo contenido
                if current_content == new_content:
                    saved_paths[filename] = os.path.abspath(file_path)
                    continue  # no se necesita crear ni renombrar
                else:
                    # Generar nombre alternativo si el contenido difiere
                    counter = 1
                    name, ext = os.path.splitext(filename)
                    while True:
                        new_filename = f"{name}_{counter}{ext}"
                        file_path = os.path.join(self.working_dir, new_filename)
                        if not os.path.exists(file_path):
                            break
                        counter += 1

            # Escribir archivo
            with open(file_path, "w") as f:
                f.write(new_content)

            saved_paths[filename] = os.path.abspath(file_path)

        return saved_paths

    def _generate_opt_paths(self, param_updates, pbar_params=iter):
        original_daophot_opt = self.daophot_opt.copy()
        original_photo_opt = self.photo_opt.copy()
        original_allstar_opt = self.allstar_opt.copy()
        opt_paths = []
        for upd in pbar_params(param_updates):
            # Aplicar actualizaciones a los diccionarios de opciones
            for key, val in upd.items():
                if key in self.daophot_opt:
                    self.daophot_opt[key] = val
                if key in self.photo_opt:
                    self.photo_opt[key] = val
                if key in self.allstar_opt:
                    self.allstar_opt[key] = val
            # Guardar archivo de opciones modificado sin sobrescribir
            opt_file = self._save_opt_files(overwrite=False)
            opt_paths.append(opt_file)
        self.allstar_opt = original_allstar_opt
        self.daophot_opt = original_daophot_opt
        self.photo_opt = original_photo_opt
        return opt_paths

    def export_file(self, obj_id, output_dir):
        objs = self.fits_files + self.tables + self.psf_files
        out_obj = next(filter(lambda f: f.id==obj_id, objs), None)
        out_paths = [
            os.path.join(output_dir, os.path.basename(p)) 
                for p in out_obj.path 
                    if not os.path.basename(p).startswith("ERROR.")
        ]
        og_paths = [
                p for p in out_obj.path 
                    if not os.path.basename(p).startswith("ERROR.")
                ]
        for og_path, out_path in zip(og_paths, out_paths):
            out_path = copy_file_noreplace(og_path, out_path)
            print(f"export: {os.path.basename(og_path)}\n -> {out_path}")

    def grid_search(self, fits_id, find_id, master_id, coords_id, fw_range, 
                            fi_range, ps_range, is_range, os_range, 
                                n_calls=25, random_state=0,
                                    pbar=None):
        print("[Photfun] Executing GridSearch.")
        # Verificar objetos
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        find_obj = next(filter(lambda f: f.id == find_id, self.tables), None)
        master_obj = next(filter(lambda f: f.id == master_id, self.tables), None)
        coords_obj = next(filter(lambda f: f.id == coords_id, self.tables), None)
        
        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not find_obj:
            raise ValueError(f"No se encontró una PSF con ID {find_id}")
        if not master_obj:
            raise ValueError(f"No se encontró una PSF con ID {master_id}")
        if not coords_obj:
            raise ValueError(f"No se encontró una tabla con ID {coords_id}")
        
        # Verificar rangos
        for name, rng in [("fw", fw_range), ("fi", fi_range), ("ps", ps_range),
                        ("is", is_range), ("os", os_range)]:
            if len(rng) < 2:
                raise ValueError(f"The range {name} should have 2 entries")
        
        # Definir el espacio de búsqueda
        space = [
            Real(min(fw_range), max(fw_range), name="fw"),
            Real(min(fi_range), max(fi_range), name="fi"),
            Real(min(ps_range), max(ps_range), name="ps"),
            Real(min(is_range), max(is_range), name="is"),
            Real(min(os_range), max(os_range), name="os"),
        ]
        
        # Guardar los valores originales para restaurarlos después
        original_daophot_opt = self.daophot_opt.copy()
        original_photo_opt = self.photo_opt.copy()
        original_allstar_opt = self.allstar_opt.copy()
        def mag2flux(mag):
            return 10**(-2/5 * (mag-25))
        

        pbar.set(message=f"Executing grid search", detail="Starting...")
        start_time = time.time()
        counter = 0
        # Progress bar
        def pbar_callback(score):
            nonlocal counter
            nonlocal pbar
            counter += 1
            amount = 1/n_calls
            elapsed_time = time.time() - start_time
            time_per_iter = elapsed_time / counter
            remaining_time = time_per_iter * (n_calls - counter)
            remaining_time_str = (str(timedelta(seconds=int(remaining_time))) 
                    if counter > 1 else "Estimating...")
            pbar.inc(amount,
                    message=f"Grid search",
                    detail=f"Progress: {counter}/{n_calls} | Time left: {remaining_time_str}")
            
        
        # Función objetivo a minimizar
        def objective(params):
            fw_val, fi_val, ps_val, is_val, os_val = params
            
            
            # Actualizar opciones
            self.daophot_opt.update({'fw': fw_val, 'fi': fi_val, 'ps': ps_val})
            self.photo_opt.update({'IS': is_val, 'OS': os_val})
            self.allstar_opt.update({'fi': fi_val, 'is': is_val, 'os': os_val})
            
            # Guardar las nuevas opciones
            self._save_opt_files()
            
            # Params
            # print(f"[GridSearch]  Params: fw={fw_val:.2f}, fi={fi_val:.2f}, ps={ps_val:.2f}, is={is_val:.2f}, os={os_val:.2f}")

            try:
                phot_table = self.phot(
                    fits_obj.id, find_obj.id
                    )
                psf_model, neighbor_list = self.psf(
                        fits_obj.id, phot_table.id, coords_obj.id
                        )
                subtracted_fits = self.sub(
                        fits_obj.id, psf_model.id, phot_table.id, coords_obj.id
                        )
                refined_psf, _ = self.psf(
                        subtracted_fits.id, phot_table.id, coords_obj.id
                        )
                master_phot_table = self.phot(
                    fits_obj.id, master_obj.id
                    )                
                final_allstar_table, final_subtracted = self.allstar(
                        fits_obj.id, refined_psf.id, master_phot_table.id
                        )
                
                # Analizar resultados usando final_table
                df = pd.concat([final_allstar_table.df(i)
                                    for i in range(len(final_allstar_table.path))],
                                        axis=0)
                df_psf = pd.concat([gaussianity_metric(refined_psf.model(i))
                                    for i in range(len(refined_psf.path))],
                                        axis=0)

                # Calcular métrica compuesta
                flux = mag2flux(df["MAG"].values)
                flux_sig = mag2flux(df["MAG"].values - df["merr"].values) - flux
                res_snr = (100 - np.mean(flux/flux_sig))/60
                merr_mean = df["merr"].mean()/0.05
                chi_mean = np.abs(1-df["chi"].mean())/0.2
                chi_std = df["chi"].std() / 0.1
                sharp_mean = np.abs(df["sharpness"]).mean()
                image_data = np.array(final_subtracted.image(0).data)
                image_data = np.nan_to_num(image_data, nan=0)
                image_data[image_data <= 0] = 0.0000001
                image_data = image_data[np.isfinite(image_data)]
                p10, p90 = np.percentile(image_data, [10, 90])
                mask = (image_data >= p10) & (image_data <= p90)
                res_flux_std = np.std(image_data[mask], ddof=0)
                psf_metric = df_psf["Pearson"].mean()**2 + df_psf["NRMSE"].mean()**2
                score = (chi_mean**2 + chi_std**2 + res_snr**2 + res_flux_std**2 + psf_metric)**0.5
                print(f"[GridSearch]  Score: {score:.4f} (merr={merr_mean:.4f}, chi={chi_mean:.4f}")
                print(f"[GridSearch]       chi_std={chi_std:.4f}, res_flux={res_flux_std:.4f}, res_snr={res_snr:.4f})")
                print(f"[GridSearch]       psf_pear={df_psf['Pearson'].mean():.4f}, psf_gauss={df_psf['NRMSE'].mean():.4f}")
                return score
                
            except Exception as e:
                print(f"[GridSearch]  Score: 999 - Error en la iteración: {str(e)}")
                return 999  # Penalizar fuertemente en caso de error

        
        try:
            self.daophot_verbose = False
            # Ejecutar optimización bayesiana
            result = gp_minimize(
                objective,
                space,
                acq_func="gp_hedge",  # Expected Improvement
                n_calls=n_calls,
                random_state=random_state,
                callback=pbar_callback if pbar else None,
                n_jobs=self.n_jobs,
            )
            
            # Extraer mejores resultados
            best_params = result.x
            best_score = result.fun
            
            # Crear diccionario con los mejores resultados
            best_result = {
                'score': best_score,
                'parameters': {
                    'fw': round(best_params[0], 2),
                    'fi': round(best_params[1], 2),
                    'ps': round(best_params[2], 2),
                    'is': round(best_params[3], 2),
                    'os': round(best_params[4], 2),
                },
                'convergence': result.specs['args'].get('convergence', None)
            }
            
            print(f"[GridSearch] Búsqueda bayesiana completada:")
            print(f"[GridSearch]   Best score: {best_score:.4f}")
            print(f"[GridSearch]   Best params: fw={best_params[0]:.2f}, fi={best_params[1]:.2f}")
            print(f"[GridSearch]       ps={best_params[2]:.2f}, is={best_params[3]:.2f}, os={best_params[4]:.2f}")
            
            # Aplicar los mejores parámetros
            self.allstar_opt.update({'fi': round(best_params[1], 2), 
                                    'is': round(best_params[3], 2), 
                                    'os': round(best_params[4], 2)})
            self.daophot_opt.update({'fw': round(best_params[0], 2), 
                                        'fi': round(best_params[1], 2), 
                                        'ps': round(best_params[2], 2)})
            self.photo_opt.update({'IS': round(best_params[3], 2), 
                                    'OS': round(best_params[4], 2)})
            self._save_opt_files()

            
            best_params_obj = self.save_current_parameters()
            best_params_obj.alias = "[>] Best OPT GridSearch"
            return best_params_obj

        except RuntimeError as e:
            print(f"ERROR: {e}")
            return "ERROR.csv"
        finally:
            # Restaurar los valores originales si es necesario
            if original_daophot_opt and original_photo_opt and original_allstar_opt:
                self.allstar_opt = original_allstar_opt
                self.daophot_opt = original_daophot_opt
                self.photo_opt = original_photo_opt
                self._save_opt_files()
            self.daophot_verbose = True

    def load_parameters(self, table_id):
        # Buscar la tabla
        table_obj = next((t for t in self.tables if t.id == table_id), None)
        if not table_obj:
            raise ValueError(f"Table not found: {table_id}")
        
        # Verificar estructura
        if len(table_obj.path) != 3:
            raise ValueError("3 parameter files expected (allstar, daophot)")
        
        # Diccionarios temporales para validación
        temp_allstar = {}
        temp_daophot = {}
        temp_photo = {}
        
        try:
            # Cargar parámetros allstar (tercer dataframe)
            for _, row in table_obj.df(0).iterrows():
                temp_allstar[row['param']] = round(row['value'], 2)
                
            # Cargar parámetros daophot (primer dataframe)
            for _, row in table_obj.df(1).iterrows():
                temp_daophot[row['param']] = round(row['value'], 2)
                
            # Cargar parámetros photo (segundo dataframe)
            for _, row in table_obj.df(2).iterrows():
                temp_photo[row['param']] = round(row['value'], 2)
                
        except KeyError as e:
            raise ValueError(f"Invalid format: missing row {str(e)}")
        
        # Validar claves existentes en los diccionarios originales
        valid_keys = {
            'allstar': set(self.allstar_opt.keys()),
            'daophot': set(self.daophot_opt.keys()),
            'photo': set(self.photo_opt.keys()),
        }
        
        # Verificar coincidencia de parámetros
        for key in temp_allstar:
            if key not in valid_keys['allstar']:
                raise ValueError(f"ALLSTAR invalid: {key}")
        
        for key in temp_daophot:
            if key not in valid_keys['daophot']:
                raise ValueError(f"DAOPHOT invalid: {key}")
                
        for key in temp_photo:
            if key not in valid_keys['photo']:
                raise ValueError(f"PHOTO invalid: {key}")
                
        # Actualizar diccionarios principales
        self.allstar_opt.update(temp_allstar)
        self.daophot_opt.update(temp_daophot)
        self.photo_opt.update(temp_photo)
        
        # Guardar archivos de configuración
        self._save_opt_files()
        
        print(f"[Photfun] Paremeter files loaded {table_id}")
        return True

    def save_current_parameters(self):

        def _generate_unique_path(base_name):
            """Genera nombres de archivo únicos con sufijo incremental"""
            counter = 0
            while True:
                suffix = f"_{counter}" if counter > 0 else ""
                filename = f"{base_name}{suffix}.csv"
                full_path = os.path.join(self.working_dir, filename)
                if not os.path.exists(full_path):
                    return full_path
                counter += 1
    
        """Guarda los parámetros actuales en archivos CSV con nombres únicos"""
        # Crear dataframes
        allstar_df = pd.DataFrame([
            {
                'alias': opt_allstar_labels[key].replace(' ', '_'),
                'param': key,
                'value': round(value, 2)
            } for key, value in self.allstar_opt.items()
        ])
        
        daophot_df = pd.DataFrame([
            {
                'alias': opt_daophot_labels[key].replace(' ', '_'),
                'param': key,
                'value': round(value, 2)
            } for key, value in self.daophot_opt.items()
        ])
        
        photo_df = pd.DataFrame([
            {
                'alias': opt_photo_labels[key].replace(' ', '_'),
                'param': key,
                'value': round(value, 2)
            } for key, value in self.photo_opt.items()
        ])
        
        # Generar nombres únicos
        allstar_path = _generate_unique_path("current_allstar_opt")
        daophot_path = _generate_unique_path("current_daophot_opt")
        photo_path = _generate_unique_path("current_photo_opt")
        
        # Guardar archivos
        allstar_df.to_csv(allstar_path, index=False)
        daophot_df.to_csv(daophot_path, index=False)
        photo_df.to_csv(photo_path, index=False)
        
        table_obj = self.add_table([allstar_path, daophot_path, photo_path])
        table_obj.alias = "[>] Current OPT config"
        # Registrar en las tablas
        return table_obj

    def __repr__(self):
        fits_repr = "\n".join(f"  ID {fits_.id}: {fits_.alias}" for fits_ in self.fits_files)
        tables_repr = "\n".join(f"  ID {table.id}: {table.alias}" for table in self.tables)
        psf_repr = "\n".join(f"  ID {psf_.id}: {psf_.alias}" for psf_ in self.psf_files)

        return (
            "PhotFun Instance:\n"
            "FITS Files:\n" + (fits_repr if fits_repr else "  None") + "\n"
            "Tables:\n" + (tables_repr if tables_repr else "  None") + "\n"
            "PSFs:\n" + (psf_repr if psf_repr else "  None")
        )

    class Logger:
        def __init__(self, photfun_instance):
            self.photfun = photfun_instance

        def write(self, message):
            if message.strip():  # Ignorar líneas vacías
                timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
                lines = message.strip().split('\n')
                for line in lines:
                    self.photfun.logs.append((timestamp, line))  # Guardar en atributo
                    self.photfun._original_stdout.write(f"[{timestamp}] {line}\n")  # Opcional: mantener salida en consola

        def flush(self):
            self.photfun._original_stdout.flush()

    def clean_up(self):
        docker_stop_async(self.docker_container)
        if os.path.exists(self.working_dir):
            shutil.rmtree(self.working_dir)
        sys.stdout = self._original_stdout  # Restaurar stdout original