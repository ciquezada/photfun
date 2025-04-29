from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from .docker_handler import run_proc, docker_run


def phot(in_fits, in_coo, in_daophot, in_photo, out_ap, verbose=True, 
                	use_docker=None, working_dir=".") -> [".ap"]:
    try:
        # Copiar archivos necesarios a la carpeta temporal
        filename = os.path.splitext(os.path.basename(in_fits))[0]
        # Crear carpeta temporal
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_PHOT_0")))
        temp_fits = os.path.join(temp_dir, os.path.basename(in_fits))
        temp_coo = os.path.join(temp_dir, os.path.basename(in_coo))
        temp_daophot  = os.path.join(temp_dir, os.path.basename(in_daophot))
        temp_photo  = os.path.join(temp_dir, os.path.basename(in_photo))
        temp_ap  = os.path.join(temp_dir, "out_ap_file.ap")
        temp_log  = os.path.join(temp_dir, "phot.log")
        out_log = os.path.join(os.path.dirname(out_ap), "phot.log")

        shutil.copy(in_fits, temp_fits)
        shutil.copy(in_coo, temp_coo)
        shutil.copy(in_daophot, temp_daophot)
        shutil.copy(in_photo, temp_photo)
        
        out_ap_filename = os.path.basename(temp_ap)
        temp_coo_filename = os.path.basename(in_coo)

        if verbose:
            print(f"daophot: phot({filename})")
        check_file(temp_fits, "fits file input: ")
        check_file(temp_daophot, "opt file input: ")
        check_file(temp_photo, "opt file input: ")
        check_file(temp_coo, "positions file input: ")
        if os.path.isfile(os.path.join(temp_dir, f"{filename}.psf")):
            if verbose:
                print("  PSF file found")
            raise NotImplementedError(f"PSF phot: remove {filename}.psf")
        overwrite = [""] if os.path.isfile(f"{out_ap_filename}") else []
        cmd_list = ['daophot << EOF >> phot.log', f'at {filename}', 
                        'phot', f'photo.opt\n', 
                        temp_coo_filename, out_ap_filename,
                        *overwrite,
                        'exit', 'EOF']
        cmd = '\n'.join(cmd_list)

              
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | daophot >> phot.log'"
            runner(cmd, os.path.relpath(temp_dir, start=working_dir))
        else:
            runner = run_proc
            cmd = cmd
            runner(cmd, temp_dir)

        # Mover el archivo de salida a la ubicación final
        final_out_ap = move_file_noreplace(temp_ap, out_ap)
        move_file_noreplace(temp_log, out_log)

        check_file(final_out_ap, "ap not created: ")
        if verbose:
            print(f"  -> {final_out_ap}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return final_out_ap