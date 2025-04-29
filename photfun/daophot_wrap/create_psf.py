from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from .docker_handler import run_proc, docker_run


def create_psf(in_fits, in_ap, in_lst, in_daophot, 
				out_psf, out_nei, verbose=True, 
                	use_docker=None, working_dir=".") -> [".psf", ".nei"]:
    try:
        # Copiar archivos necesarios a la carpeta temporal
        filename = os.path.splitext(os.path.basename(in_fits))[0]
        # Crear carpeta temporal
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_PSF_0")))
        temp_fits = os.path.join(temp_dir, os.path.basename(in_fits))
        temp_ap = os.path.join(temp_dir, os.path.basename(in_ap))
        temp_lst = os.path.join(temp_dir, os.path.basename(in_lst))
        temp_daophot  = os.path.join(temp_dir, os.path.basename(in_daophot))
        temp_psf  = os.path.join(temp_dir, "out_psf_file.psf")
        temp_nei  =  os.path.join(temp_dir, 
                    f"{os.path.splitext(os.path.basename(temp_psf))[0]}.nei")
        temp_log  = os.path.join(temp_dir, "psf.log")
        out_log = os.path.join(os.path.dirname(out_psf), "psf.log")

        shutil.copy(in_fits, temp_fits)
        shutil.copy(in_ap, temp_ap)
        shutil.copy(in_lst, temp_lst)
        shutil.copy(in_daophot, temp_daophot)
        
        out_psf_filename = os.path.basename(temp_psf)
        temp_ap_filename = os.path.basename(in_ap)
        temp_lst_filename = os.path.basename(in_lst)

        if verbose:
            print(f"daophot: psf({filename})")
        check_file(temp_daophot, "opt file input: ")
        check_file(temp_fits, "fits file input: ")
        check_file(temp_ap, "ap file input")
        check_file(temp_lst, "lst file input")
        overwrite_psf = [""] if os.path.isfile(temp_psf) else []
        overwrite_nei = [""] if os.path.isfile(temp_nei) else []

        cmd_list = ['daophot << EOF >> psf.log', f'at {filename}', 
                    'psf', temp_ap_filename, temp_lst_filename, out_psf_filename,
                    *overwrite_psf, *overwrite_nei,
                    'exit', 'EOF']
        cmd = '\n'.join(cmd_list)
               
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | daophot >> psf.log'"
            runner(cmd, os.path.relpath(temp_dir, start=working_dir))
        else:
            runner = run_proc
            cmd = cmd
            runner(cmd, temp_dir)

        check_file(temp_nei, "nei not created: ")
        # Mover el archivo de salida a la ubicación final
        final_out_nei = move_file_noreplace(temp_nei, out_nei)
        final_out_psf = move_file_noreplace(temp_psf, out_psf)
        move_file_noreplace(temp_log, out_log)
        check_file(final_out_nei, "psf not created: ")
        if verbose:
                print(f"  -> {final_out_nei}")
        check_file(final_out_psf, "psf not created: ")
        if verbose:
            print(f"  -> {final_out_psf}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return final_out_psf, final_out_nei