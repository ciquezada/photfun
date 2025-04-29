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
                        sub_fits, allstar, daomatch, 
                        create_master)
from ..daophot_opt import opt_daophot_dict, opt_photo_dict, opt_allstar_dict
from ..misc_tools import temp_mkdir, copy_file_noreplace
import numpy as np
from joblib import Parallel, delayed, parallel_backend
import nest_asyncio
import asyncio
from itertools import cycle
from tqdm import tqdm

nest_asyncio.apply()

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
                print("ERROR: ", args, kwargs)
                return error_output
        try:
            # Ejecutar la función normalmente
            result = func(*args, **kwargs)
            return result
        except FileNotFoundError as e:
            print("ERROR: ", e, args, kwargs)
            return error_output
        except MemoryError as e:
            print("ERROR: ", e, args, kwargs)
            return error_output
        except RuntimeError as e:
            print("ERROR: ", e, args, kwargs)
            return error_output
    return delayed(error_handler_wrap)


class PhotFun:
    def __init__(self):
        self.n_jobs = -1
        self._id_counter = count(start=0, step=1)
        self.tables = []
        self.fits_files = []
        self.psf_files = []

        # Almacenar los diccionarios de opciones como atributos
        self.daophot_opt = opt_daophot_dict.copy()
        self.photo_opt = opt_photo_dict.copy()
        self.allstar_opt = opt_allstar_dict.copy()

        # Crear la carpeta temporal
        self.working_dir = os.path.abspath(temp_mkdir("photfun_working_dir_0"))

        # Definicion del log
        self.logs = []  # Lista para almacenar logs como tuplas (timestamp, mensaje)
        self._original_stdout = sys.stdout
        sys.stdout = self.Logger(self)  # Redirigir stdout

        # Guardar los diccionarios como archivos de texto
        self._save_opt_files()

        # Parametros de find
        self.find_sum = 1
        self.find_average = 1

        # Parametros de pick
        self.pick_max_stars = 200
        self.pick_min_mag = 20

        # Parametros de allstar
        self.allstar_recentering = True

        # intiaite Docker (if it can)
        self._docker_container = init_docker(self.working_dir, self.n_jobs)

    def reconnect_docker(self):
        # intiaite Docker (if it can)
        self._docker_container = init_docker(self.working_dir, self.n_jobs, prev=self._docker_container)

    def add_table(self, path, *args, **kwargs):
        table = PhotTable(path, *args, **kwargs)
        table.id = next(self._id_counter)
        self.tables.append(table)
        print(f"  -> {table.alias}")
        return table

    def add_fits(self, path, *args, **kwargs):
        fits_file = PhotFits(path, *args, **kwargs)
        fits_file.id = next(self._id_counter)
        self.fits_files.append(fits_file)
        print(f"  -> {fits_file.alias}")
        return fits_file

    def add_psf(self, path):
        psf_file = PhotPSF(path)
        psf_file.id = next(self._id_counter)
        self.psf_files.append(psf_file)
        print(f"  -> {psf_file.alias}")
        return psf_file

    def find(self, fits_id, pbar=iter):
        fits_obj = next(filter(lambda f: f.id==fits_id, self.fits_files), None)
        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        self._save_opt_files()

        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self._docker_container)
        arg_list = [
                    (fits_path, 
                    os.path.join(self.working_dir, 'daophot.opt'),
                    os.path.join(self.working_dir, f"{os.path.basename(fits_path).replace('.fits', '.coo')}"),
                    f"{int(self.find_sum)},{int(self.find_average)}",
                    True if len(fits_obj.path)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    )  
                    for fits_path in fits_obj.path
                ]
        # Crear un nuevo event loop para el thread
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            with parallel_backend('threading'):  # Usar threading para mejor compatibilidad
                final_out_coo = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                    delayed_wrap(find)(*args) for args in pbar(arg_list)
                                                )
        finally:
            loop.close()
        out_obj_table = self.add_table(final_out_coo)
        return out_obj_table

    def phot(self, fits_id, coo_id, pbar=iter):
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        coo_table = next(filter(lambda f: f.id == coo_id, self.tables), None)

        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not coo_table:
            raise ValueError(f"No se encontró una tabla con ID {coo_id}")

        self._save_opt_files()

        input_args = pair_args(fits_obj.path, coo_table.path, 
                    err_msg="La cantidad de archivos FITS y COO no coincide.")

        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self._docker_container)
        arg_list = [
                    (fits_path, coo_path,
                    os.path.join(self.working_dir, 'daophot.opt'),
                    os.path.join(self.working_dir, 'photo.opt'),
                    os.path.join(self.working_dir, f"{os.path.basename(fits_path).replace('.fits', '.ap')}"),
                    True if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    )  
                    for fits_path, coo_path in input_args
                ]
        # Crear un nuevo event loop para el thread
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            with parallel_backend('threading'):  # Usar threading para mejor compatibilidad
                final_out_ap = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                    delayed_wrap(phot)(*args) for args in pbar(arg_list)
                                                )
        finally:
            loop.close()

        return self.add_table(final_out_ap)

    def pick(self, fits_id, ap_id, pbar=iter):
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        ap_table = next(filter(lambda f: f.id == ap_id, self.tables), None)

        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not ap_table:
            raise ValueError(f"No se encontró una tabla con ID {ap_id}")

        self._save_opt_files()
        input_args = pair_args(fits_obj.path, ap_table.path, 
                err_msg="La cantidad de archivos FITS y AP no coincide.")

        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self._docker_container)
        arg_list = [
                    (fits_path, ap_path,
                    os.path.join(self.working_dir, 'daophot.opt'),
                    os.path.join(self.working_dir, f"{os.path.basename(fits_path).replace('.fits', '.lst')}"),
                    f"{int(self.pick_max_stars)},{int(self.pick_min_mag)}",
                    True if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    )  
                    for fits_path, ap_path in input_args
                ]
        # Crear un nuevo event loop para el thread
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            with parallel_backend('threading'):  # Usar threading para mejor compatibilidad
                final_out_lst = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                    delayed_wrap(pick)(*args) for args in pbar(arg_list)
                                                )
        finally:
            loop.close()
            
        return self.add_table(final_out_lst)

    def psf(self, fits_id, ap_id, lst_id, pbar=iter):
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        ap_table = next(filter(lambda f: f.id == ap_id, self.tables), None)
        lst_table = next(filter(lambda f: f.id == lst_id, self.tables), None)

        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not ap_table:
            raise ValueError(f"No se encontró una tabla con ID {ap_id}")
        if not lst_table:
            raise ValueError(f"No se encontró una tabla con ID {lst_id}")

        self._save_opt_files()
        output_dir = self.working_dir
        daophot_opt = os.path.join(output_dir, 'daophot.opt')
        input_args = pair_args(fits_obj.path, ap_table.path, lst_table.path, 
                        err_msg="La cantidad de archivos FITS/AP/LST no coincide.")
      
        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self._docker_container)
        arg_list = [
                    (fits_path, ap_path, lst_path,
                    os.path.join(self.working_dir, 'daophot.opt'),
                    os.path.join(output_dir, f"{os.path.basename(fits_path).replace('.fits', '.psf')}"),
                    os.path.join(output_dir, f"{os.path.basename(fits_path).replace('.fits', '.nei')}"),
                    True if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    )  
                    for fits_path, ap_path, lst_path in input_args
                ]
        # Crear un nuevo event loop para el thread
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            with parallel_backend('threading'):  # Usar threading para mejor compatibilidad
                final_out = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                    delayed_wrap(create_psf)(*args) for args in pbar(arg_list)
                                                )
            final_out_psf = [r[0] for r in final_out]
            final_out_nei = [r[1] for r in final_out]
        finally:
            loop.close()
            
        return self.add_psf(final_out_psf), self.add_table(final_out_nei)

    def sub(self, fits_id, psf_id, nei_id, lst_id=False, pbar=iter):
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

        self._save_opt_files()

        input_args = pair_args(fits_obj.path, psf_obj.path, 
                                nei_table.path, lst_table.path if lst_id else [False],
                                    err_msg="La cantidad de archivos FITS/PSF/NEI/LST no coincide.")
        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self._docker_container)
        arg_list = [
                    (fits_path, psf_path, nei_path,
                    os.path.join(self.working_dir, 'daophot.opt'),
                    os.path.join(self.working_dir, f"{os.path.splitext(os.path.basename(fits_path))[0]}_sub.fits"),
                    lst_path,
                    True if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    )  
                    for fits_path, psf_path, nei_path, lst_path in input_args
                ]
        # Crear un nuevo event loop para el thread
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            with parallel_backend('threading'):  # Usar threading para mejor compatibilidad
                final_out_subfits = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                    delayed_wrap(sub_fits)(*args) for args in pbar(arg_list)
                                                )
        finally:
            loop.close()
        return self.add_fits(final_out_subfits)

    def allstar(self, fits_id, psf_id, ap_id, pbar=iter):
        fits_obj = next(filter(lambda f: f.id == fits_id, self.fits_files), None)
        psf_obj = next(filter(lambda f: f.id == psf_id, self.psf_files), None)
        ap_table = next(filter(lambda f: f.id == ap_id, self.tables), None)

        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not psf_obj:
            raise ValueError(f"No se encontró una PSF con ID {psf_id}")
        if not ap_table:
            raise ValueError(f"No se encontró una tabla con ID {ap_id}")

        self._save_opt_files()
        output_dir = self.working_dir
        daophot_opt = os.path.join(output_dir, 'daophot.opt')
        allstar_opt = os.path.join(output_dir, 'allstar.opt')

        input_args = pair_args(fits_obj.path, psf_obj.path, ap_table.path,
                                err_msg="La cantidad de archivos FITS/PSF/AP no coincide.")

        # Preparar la lista de argumentos para cada tarea
        docker_cycle = cycle(self._docker_container)
        arg_list = [
                    (fits_path, psf_path, ap_path,
                    os.path.join(self.working_dir, 'daophot.opt'),
                    os.path.join(self.working_dir, 'allstar.opt'),
                    os.path.join(self.working_dir, f"{os.path.basename(fits_path).replace('.fits', '.als')}"),
                    os.path.join(self.working_dir, f"{os.path.splitext(os.path.basename(fits_path))[0]}_als_sub.fits"),
                    self.allstar_recentering,
                    True if len(input_args)<4 else False, # verbose=False
                    next(docker_cycle),
                    self.working_dir,
                    )  
                    for fits_path, psf_path, ap_path in input_args
                ]
        # Crear un nuevo event loop para el thread
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            with parallel_backend('threading'):  # Usar threading para mejor compatibilidad
                final_out = Parallel(n_jobs=min(self.n_jobs, len(arg_list)), verbose=0)(
                                                    delayed_wrap(allstar)(*args) for args in pbar(arg_list)
                                                )
            final_out_als = [r[0] for r in final_out]
            final_out_subfits = [r[1] for r in final_out]
        finally:
            loop.close()

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
        final_out_mch = daomatch(table_obj.path[0], table_list_obj.path, 
        							out_mch, self._docker_container[0])
        out_mch_table = self.add_table(final_out_mch)
        return out_mch_table

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

    def _save_opt_files(self):
        opt_files = {
            "daophot.opt": self.daophot_opt,
            "photo.opt": self.photo_opt,
            "allstar.opt": self.allstar_opt,
        }
        for filename, opt_dict in opt_files.items():
            file_path = os.path.join(self.working_dir, filename)
            new_content = "\n".join(f"{key} = {value}" for key, value in opt_dict.items()) + "\n"

            # compare
            if os.path.exists(file_path):
                with open(file_path, "r") as f:
                    existing_content = f.read()
                if existing_content == new_content:
                    continue  
            with open(file_path, "w") as f:
                f.write(new_content)

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
        docker_stop_async(self._docker_container)
        if os.path.exists(self.working_dir):
            shutil.rmtree(self.working_dir)
        sys.stdout = self._original_stdout  # Restaurar stdout original