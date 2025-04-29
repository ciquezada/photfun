from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from .docker_handler import run_proc, docker_run


def pick(in_fits, in_ap, in_daophot, out_lst, 
			stars_minmag="200,20", verbose=True, 
                	use_docker=None, working_dir=".") -> [".lst"]:
    try:
        # Copiar archivos necesarios a la carpeta temporal
        filename = os.path.splitext(os.path.basename(in_fits))[0]
        # Crear carpeta temporal
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_PICK_0")))
        temp_fits = os.path.join(temp_dir, os.path.basename(in_fits))
        temp_ap = os.path.join(temp_dir, os.path.basename(in_ap))
        temp_daophot  = os.path.join(temp_dir, os.path.basename(in_daophot))
        temp_lst  = os.path.join(temp_dir, "out_lst_file.lst")
        temp_log  = os.path.join(temp_dir, "pick.log")
        out_log = os.path.join(os.path.dirname(out_lst), "pick.log")

        shutil.copy(in_fits, temp_fits)
        shutil.copy(in_ap, temp_ap)
        shutil.copy(in_daophot, temp_daophot)
        
        out_lst_filename = os.path.basename(temp_lst)
        temp_ap_filename = os.path.basename(in_ap)

        if verbose:
            print(f"daophot: pick({filename})")
        check_file(temp_daophot, "opt file input: ")
        check_file(temp_fits, "fits file input: ")
        check_file(temp_ap, "aperture file input: ")
        overwrite = [""] if os.path.isfile(temp_lst) else []
        cmd_list = ['daophot << EOF >> pick.log', f'at {filename}', 
                        'pick', temp_ap_filename, f'{stars_minmag}', 
                        out_lst_filename, *overwrite,
                        'exit', 'EOF']
        cmd = '\n'.join(cmd_list)

                 
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | daophot >> pick.log'"
            runner(cmd, os.path.relpath(temp_dir, start=working_dir))
        else:
            runner = run_proc
            cmd = cmd
            runner(cmd, temp_dir)

        # Mover el archivo de salida a la ubicación final
        final_out_lst = move_file_noreplace(temp_lst, out_lst)
        move_file_noreplace(temp_log, out_log)

        check_file(final_out_lst, "lst not created: ")
        if verbose:
            print(f"  -> {final_out_lst}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return final_out_lst