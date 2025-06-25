from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from .docker_handler import run_proc, docker_run


def allframe(in_fits, in_psf, in_als, in_mch, in_master,
                out_dir, out_prefix="", verbose=True,
                use_docker=None, working_dir=".", timeout=None) -> [".alf", ".fits", ".tfr", ".nmg"]:
    try:
        filename = os.path.splitext(os.path.basename(in_mch))[0]
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_ALLFRAME_0")))

        temp_mch = os.path.join(temp_dir, os.path.basename(in_mch))
        temp_mag = os.path.join(temp_dir, os.path.basename(in_master))

        temp_log = os.path.join(temp_dir, "allframe.log")
        out_log = os.path.join(out_dir, "allframe.log")

        shutil.copy(in_mch, temp_mch)
        shutil.copy(in_master, temp_mag)

        temp_fits = []
        temp_psf = []
        temp_als = []

        for f, p, a in zip(in_fits, in_psf, in_als):
            temp_f = os.path.join(temp_dir, os.path.basename(f))
            temp_p = os.path.join(temp_dir, os.path.basename(p))
            temp_a = os.path.join(temp_dir, os.path.basename(a))
            shutil.copy(f, temp_f)
            shutil.copy(p, temp_p)
            shutil.copy(a, temp_a)
            temp_fits.append(temp_f)
            temp_psf.append(temp_p)
            temp_als.append(temp_a)

        check_file(temp_mch, "mch file missing")
        check_file(temp_mag, "master mag file missing")


        cmd_list = [
            f"allframe << EOF >> allframe.log",
            "",
            os.path.basename(temp_mch),
            os.path.basename(temp_mag),
            "EOF"
        ]
        cmd = '\n'.join(cmd_list)
       
        if verbose:
            print(f"allframe: {filename}")
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | allstar >> allstar.log'"
            runner(cmd, os.path.relpath(temp_dir, start=working_dir), timeout)
        else:
            runner = run_proc
            cmd = cmd
            runner(cmd, temp_dir, timeout)

        # Recolectar salidas
        alf_paths = []
        fits_paths = []
        for als_path in temp_als:
            name = os.path.splitext(os.path.basename(als_path))[0]
            temp_alf = os.path.join(temp_dir, f"{name}.alf")
            temp_subfits = os.path.join(temp_dir, f"{name}j.fits")
            final_alf = move_file_noreplace(temp_alf, os.path.join(out_dir, f"{out_prefix}{name}.alf"))
            final_fits = move_file_noreplace(temp_subfits, os.path.join(out_dir, f"{out_prefix}{name}_framed.fits"))
            check_file(final_alf, f"alf not created for {name}")
            check_file(final_fits, f"sub fits not created for {name}")
            alf_paths.append(final_alf)
            fits_paths.append(final_fits)

        temp_log = os.path.join(temp_dir, "allframe.log")
        out_log = os.path.join(out_dir, "allframe.log")
        move_file_noreplace(temp_log, out_log)

        temp_tfr = os.path.join(temp_dir, f"{filename}.tfr")
        final_tfr = move_file_noreplace(temp_tfr, os.path.join(out_dir, f"{out_prefix}{filename}.tfr"))

        temp_nmg = os.path.join(temp_dir, f"{filename}.nmg")
        final_nmg = move_file_noreplace(temp_nmg, os.path.join(out_dir, f"{out_prefix}{filename}.nmg"))
        check_file(final_nmg, "nmg no creado")

        if verbose:
            print(f"  -> {len(alf_paths)} .alf files")
            print(f"  -> {len(fits_paths)} .sub.fits files")
            print(f"  -> {final_tfr}")
            print(f"  -> {final_nmg}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return alf_paths, fits_paths, final_tfr, final_nmg