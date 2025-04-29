from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from .docker_handler import run_proc, docker_run


def daomatch(in_master_als, in_path_list, out_mch, verbose=True, 
                	use_docker=None, working_dir=".") -> [".mch"]: 
    try:
        # Copiar archivos necesarios a la carpeta temporal
        filename = os.path.splitext(os.path.basename(in_master_als))[0]
        # Crear carpeta temporal
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_DAOMATCH_0")))
        temp_master = os.path.join(temp_dir, os.path.basename(in_master_als))
        temp_path_list = [os.path.join(temp_dir, os.path.basename(in_path)) for in_path in in_path_list]
        temp_mch  = os.path.join(temp_dir, "out_match.mch")
        temp_log  = os.path.join(temp_dir, "daomatch.log")
        out_log = os.path.join(os.path.dirname(out_mch), "daomatch.log")

        shutil.copy(in_master_als, temp_master)
        for in_path, temp_path in zip(in_path_list, temp_path_list):
            shutil.copy(in_path, temp_path)

        
        in_master_name = os.path.basename(temp_master)
        in_file_list = [os.path.basename(in_path) for in_path in temp_path_list]
        out_mch_filename = os.path.basename(temp_mch)

        if verbose:
            print(f"daomatch: find({filename})")

        check_file(temp_master, "master file input: ")
        overwrite = [""] if os.path.isfile(out_mch_filename) else []
        cmd_list = ['daomatch << EOF >> daomatch.log', in_master_name, 
                        out_mch_filename, *overwrite,
                        *in_file_list, '', 
                        'EOF']
        cmd = '\n'.join(cmd_list)

               
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | daomatch >> daomatch.log'"
            runner(cmd, os.path.relpath(temp_dir, start=working_dir))
        else:
            runner = run_proc
            cmd = cmd
            runner(cmd, temp_dir)

        # Mover el archivo de salida a la ubicación final
        move_file_noreplace(temp_log, out_log)
        final_out_mch = move_file_noreplace(temp_mch, out_mch)

        check_file(final_out_mch, "coo not created: ")
        if verbose:
            print(f"  -> {final_out_mch}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return final_out_mch



