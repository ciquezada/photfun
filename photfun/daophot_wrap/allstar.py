from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from .docker_handler import run_proc, docker_run


def allstar(in_fits, in_psf, in_ap, in_daophot, in_allstar,
                out_als, out_fits, RE=True, verbose=True, 
                	use_docker=None, working_dir=".") -> [".als", ".fits"]:
    try:
        # Copiar archivos necesarios a la carpeta temporal
        filename = os.path.splitext(os.path.basename(in_fits))[0]
        out_fits_filename = "allstar_substracted"
        out_als_filename = "allstar_out_file"
        # Crear carpeta temporal
        temp_dir      = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_ALLSTAR_0")))
        temp_fits     = os.path.join(temp_dir, os.path.basename(in_fits))
        temp_psf      = os.path.join(temp_dir, os.path.basename(in_psf))
        temp_ap       = os.path.join(temp_dir, os.path.basename(in_ap))
        temp_daophot  = os.path.join(temp_dir, os.path.basename(in_daophot))
        temp_allstar  = os.path.join(temp_dir, os.path.basename(in_allstar))
        temp_out_als  = os.path.join(temp_dir, f"{out_als_filename}.als")
        temp_out_fits = os.path.join(temp_dir, f"{out_fits_filename}.fits")
        temp_log      = os.path.join(temp_dir, "allstar.log")
        out_log       = os.path.join(os.path.dirname(out_als), "allstar.log")

        shutil.copy(in_fits, temp_fits)
        shutil.copy(in_psf, temp_psf)
        shutil.copy(in_ap, temp_ap)
        shutil.copy(in_daophot, temp_daophot)
        shutil.copy(in_allstar, temp_allstar)
        
        temp_psf_filename = os.path.basename(in_psf)
        temp_ap_filename = os.path.basename(in_ap)

        if verbose:
            print(f"allstar: {filename}")
        check_file(temp_allstar, "opt file input: ")
        check_file(temp_fits, "fits file input: ")
        check_file(temp_psf, "psf file input")
        check_file(temp_ap, "positions list file input")
        overwrite_als = [""] if os.path.isfile(temp_out_als) else []
        do_RE = [""] if RE else ["RE=0\n"]
        cmd_list = ['allstar << EOF >> allstar.log',
                    *do_RE,
                    filename, temp_psf_filename, temp_ap_filename, 
                    out_als_filename, *overwrite_als, out_fits_filename, 
                    'exit', "EOF"]
        cmd = '\n'.join(cmd_list)
       
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | allstar >> allstar.log'"
        else:
            runner = run_proc
            cmd = cmd
        runner(cmd, temp_dir)

        #     runner(cmd, os.path.relpath(temp_dir, start=working_dir))
        # else:
        #     runner = run_proc
        #     cmd = cmd
        #     runner(cmd, temp_dir)

        # Mover el archivo de salida a la ubicación final
        final_out_als = move_file_noreplace(temp_out_als, out_als)
        final_out_fits = move_file_noreplace(temp_out_fits, out_fits)
        move_file_noreplace(temp_log, out_log)

        check_file(f"{final_out_als}", "als not created: ")
        check_file(f"{final_out_fits}", "sub fits not created: ")
        if verbose:
            print(f"  -> {final_out_als}")
            print(f"  -> {final_out_fits}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return final_out_als, final_out_fits