import sys
sys.path.append("/data/ciquezada/Projects/py_photsuite")
### ERASE AFTER
import os
import shutil
from .phot_table import PhotTable
from .phot_fits import PhotFits
from .phot_psf import PhotPSF
from daophot_wrap import find, phot, pick, create_psf, sub_fits, allstar
from photfun.daophot_opt import daophot_dict, photo_dict, allstar_dict
from misc_tools import temp_mkdir


class PhotFun:
    def __init__(self):
        self.tables = []
        self.fits_files = []
        self.psf_files = []

        # Almacenar los diccionarios de opciones como atributos
        self.daophot_opt = daophot_dict.copy()
        self.photo_opt = photo_dict.copy()
        self.allstar_opt = allstar_dict.copy()

        # Crear la carpeta temporal
        self.working_dir = os.path.abspath(temp_mkdir("photfun_working_dir"))

        # Guardar los diccionarios como archivos de texto
        self._save_opt_files()

    def add_table(self, path):
        table = PhotTable(path)
        self.tables.append(table)

    def add_fits(self, path):
        fits_file = PhotFits(path)
        self.fits_files.append(fits_file)

    def add_psf(self, path):
        psf_file = PhotPSF(path)
        self.psf_files.append(psf_file)

    def find(self, fits_id):
        fits_obj = next(filter(lambda f: f.id==fits_id, self.fits_files), None)
        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        self._save_opt_files()

        output_dir = os.path.dirname(fits_obj.path)
        out_coo = os.path.join(output_dir, f"{os.path.basename(fits_obj.path).replace('.fits', '.coo')}")
        final_out_coo = find(fits_obj.path, os.path.join(self.working_dir, 'daophot.opt'), out_coo)
        self.add_table(final_out_coo)

    def phot(self, fits_id, coo_id):
        fits_obj = next(filter(lambda f: f.id==fits_id, self.fits_files), None)
        coo_table = next(filter(lambda f: f.id==coo_id, self.tables), None)
        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not coo_table:
            raise ValueError(f"No se encontró una tabla con ID {coo_id}")
        self._save_opt_files()

        output_dir = os.path.dirname(fits_obj.path)
        out_ap = os.path.join(output_dir, f"{os.path.basename(fits_obj.path).replace('.fits', '.ap')}")
        final_out_ap = phot(fits_obj.path, coo_table.path, 
                                os.path.join(self.working_dir, 'daophot.opt'), 
                                os.path.join(self.working_dir, 'photo.opt'), 
                                out_ap)
        self.add_table(final_out_ap)

    def pick(self, fits_id, ap_id):
        fits_obj = next(filter(lambda f: f.id==fits_id, self.fits_files), None)
        ap_table = next(filter(lambda f: f.id==ap_id, self.tables), None)
        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not ap_table:
            raise ValueError(f"No se encontró una tabla con ID {ap_id}")
        self._save_opt_files()

        output_dir = os.path.dirname(fits_obj.path)
        out_lst = os.path.join(output_dir, f"{os.path.basename(fits_obj.path).replace('.fits', '.lst')}")
        final_out_lst = pick(fits_obj.path, ap_table.path, 
                                os.path.join(self.working_dir, 'daophot.opt'), 
                                out_lst)
        self.add_table(final_out_lst)

    def psf(self, fits_id, ap_id, lst_id):
        fits_obj = next(filter(lambda f: f.id==fits_id, self.fits_files), None)
        ap_table = next(filter(lambda f: f.id==ap_id, self.tables), None)
        lst_table = next(filter(lambda f: f.id==lst_id, self.tables), None)
        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not ap_table:
            raise ValueError(f"No se encontró una tabla con ID {ap_id}")
        if not lst_table:
            raise ValueError(f"No se encontró una tabla con ID {lst_id}")
        self._save_opt_files()

        output_dir = os.path.dirname(fits_obj.path)
        out_psf = os.path.join(output_dir, f"{os.path.basename(fits_obj.path).replace('.fits', '.psf')}")
        out_nei = os.path.join(output_dir, f"{os.path.basename(fits_obj.path).replace('.fits', '.nei')}")
        final_out_psf, final_out_nei = create_psf(fits_obj.path, ap_table.path, lst_table.path,
                                                    os.path.join(self.working_dir, 'daophot.opt'), 
                                                    out_psf, out_nei)
        self.add_psf(final_out_psf)
        self.add_table(final_out_nei)

    def sub(self, fits_id, psf_id, nei_id):
        fits_obj = next(filter(lambda f: f.id==fits_id, self.fits_files), None)
        psf_obj = next(filter(lambda f: f.id==psf_id, self.psf_files), None)
        nei_table = next(filter(lambda f: f.id==nei_id, self.tables), None)
        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not psf_obj:
            raise ValueError(f"No se encontró una PSF con ID {psf_id}")
        if not nei_table:
            raise ValueError(f"No se encontró una tabla con ID {nei_id}")
        self._save_opt_files()

        output_dir = os.path.dirname(fits_obj.path)
        out_subfits = os.path.join(output_dir, f"{os.path.splitext(os.path.basename(fits_obj.path))[0]}_sub.fits")
        final_out_subfits = sub_fits(fits_obj.path, psf_obj.path, nei_table.path,
                                        os.path.join(self.working_dir, 'daophot.opt'), 
                                        out_subfits)
        self.add_fits(final_out_subfits)

    def allstar(self, fits_id, psf_id, ap_id):
        fits_obj = next(filter(lambda f: f.id==fits_id, self.fits_files), None)
        psf_obj = next(filter(lambda f: f.id==psf_id, self.psf_files), None)
        ap_table = next(filter(lambda f: f.id==ap_id, self.tables), None)
        if not fits_obj:
            raise ValueError(f"No se encontró un FITS con ID {fits_id}")
        if not psf_obj:
            raise ValueError(f"No se encontró una PSF con ID {psf_id}")
        if not ap_table:
            raise ValueError(f"No se encontró una tabla con ID {ap_id}")
        self._save_opt_files()

        output_dir = os.path.dirname(fits_obj.path)
        out_als = os.path.join(output_dir, f"{os.path.basename(fits_obj.path).replace('.fits', '.als')}")
        out_subfits = os.path.join(output_dir, f"{os.path.splitext(os.path.basename(fits_obj.path))[0]}_als_sub.fits")
        final_out_als, final_out_subfits = allstar(fits_obj.path, psf_obj.path, ap_table.path,
                                                        os.path.join(self.working_dir, 'daophot.opt'), 
                                                        os.path.join(self.working_dir, 'allstar.opt'), 
                                                        out_als, out_subfits)
        self.add_table(final_out_als)
        self.add_fits(final_out_subfits)

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

    def clean_up(self):
        if os.path.exists(self.working_dir):
            shutil.rmtree(self.working_dir)
        
        self.tables.clear()
        self.fits_files.clear()
        self.psf_files.clear()
        PhotTable.reset_id_counter()
        PhotFits.reset_id_counter()
        PhotPSF.reset_id_counter()