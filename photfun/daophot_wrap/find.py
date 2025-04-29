from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from .docker_handler import run_proc, docker_run


def find(in_fits, in_daophot, out_coo, sum_aver="1,1", verbose=True, 
                	use_docker=None, working_dir=".") -> [".coo"]: 
    try:
        # Copiar archivos necesarios a la carpeta temporal
        filename = os.path.splitext(os.path.basename(in_fits))[0]
        # Crear carpeta temporal
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_FIND_0")))
        temp_fits = os.path.join(temp_dir, os.path.basename(in_fits))
        temp_opt  = os.path.join(temp_dir, os.path.basename(in_daophot))
        temp_coo  = os.path.join(temp_dir, os.path.basename(out_coo))
        temp_log  = os.path.join(temp_dir, "find.log")
        out_log = os.path.join(os.path.dirname(out_coo), "find.log")

        shutil.copy(in_fits, temp_fits)
        shutil.copy(in_daophot, temp_opt)
        
        out_coo_filename = os.path.basename(temp_coo)

        if verbose:
            print(f"daophot: find({filename})")
        check_file(temp_opt, "opt file input: ")
        check_file(temp_fits, "fits file input: ")
        overwrite = [""] if os.path.isfile(out_coo_filename) else []
        cmd_list = ['daophot << EOF >> find.log', f'at {filename}', 
                        'find', f'{sum_aver}', out_coo_filename,
                        *overwrite,
                        'y', 
                        'exit', 'EOF']
        cmd = '\n'.join(cmd_list)

        
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | daophot >> find.log'"
            runner(cmd, os.path.relpath(temp_dir, start=working_dir))
        else:
            runner = run_proc
            cmd = cmd
            runner(cmd, temp_dir)

        # Mover el archivo de salida a la ubicación final
        final_out_coo = move_file_noreplace(temp_coo, out_coo)
        move_file_noreplace(temp_log, out_log)

        check_file(final_out_coo, "coo not created: ")
        if verbose:
            print(f"  -> {final_out_coo}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return final_out_coo