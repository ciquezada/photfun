from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from .docker_handler import run_proc, docker_run


def sub_fits(in_fits, in_psf, in_sub, in_daophot, 
				out_fits, in_lst=False, verbose=True, 
                	use_docker=None, working_dir=".") -> [".fits"]:
    try:
        # Copiar archivos necesarios a la carpeta temporal
        filename = os.path.splitext(os.path.basename(in_fits))[0]
        out_filename = "out_subtracted_fits"
        # Crear carpeta temporal
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_SUB_0")))
        temp_fits = os.path.join(temp_dir, os.path.basename(in_fits))
        temp_psf = os.path.join(temp_dir, os.path.basename(in_psf))
        temp_sub = os.path.join(temp_dir, os.path.basename(in_sub))
        temp_daophot  = os.path.join(temp_dir, os.path.basename(in_daophot))
        temp_out_fits  =  os.path.join(temp_dir, f"{out_filename}.fits")
        temp_log  = os.path.join(temp_dir, "sub.log")
        out_log = os.path.join(os.path.dirname(out_fits), "sub.log")

        shutil.copy(in_fits, temp_fits)
        shutil.copy(in_psf, temp_psf)
        shutil.copy(in_sub, temp_sub)
        shutil.copy(in_daophot, temp_daophot)
        
        temp_psf_filename = os.path.basename(in_psf)
        temp_sub_filename = os.path.basename(in_sub)

        if verbose:
            print(f"daophot: sub({filename})")
        check_file(temp_daophot, "opt file input: ")
        check_file(temp_psf, "psf file input")
        check_file(temp_sub, "substraction list file input")
        check_file(temp_fits, "fits file input")
        add_exceptions = []
        if in_lst:
            temp_lst = os.path.join(temp_dir, os.path.basename(in_lst))
            shutil.copy(in_lst, temp_lst)
            temp_lst_filename = os.path.basename(in_lst)
            check_file(temp_lst, "exception list file input")
            add_exceptions = ["y", temp_lst_filename]
        else:
            add_exceptions = ["n"]

        cmd_list = ['daophot << EOF >> sub.log', 
                    f'at {filename}', 
                    'sub', temp_psf_filename, temp_sub_filename, 
                    *add_exceptions,
                    out_filename, 
                    'exit', 'EOF']
        cmd = '\n'.join(cmd_list)
       
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | daophot >> sub.log'"
            runner(cmd, os.path.relpath(temp_dir, start=working_dir))
        else:
            runner = run_proc
            cmd = cmd
            runner(cmd, temp_dir)

        # Mover el archivo de salida a la ubicación final
        final_out_fits = move_file_noreplace(temp_out_fits, out_fits)
        move_file_noreplace(temp_log, out_log)

        check_file(f"{final_out_fits}", "fits not created: ")
        if verbose:
            print(f"  -> {final_out_fits}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return final_out_fits