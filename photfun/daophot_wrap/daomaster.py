from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from .docker_handler import run_proc, docker_run


def daomaster(in_mch, in_path_list, out_dir, out_prefix,
            new_id, out_mag, out_cor, out_raw, 
            out_mch, out_tfr, out_coo, out_mtr,
            min_min_fr="1,0,1", max_sig="99",
            deg_free="4", crit_rad="6", mult=1,
            verbose=True, use_docker=None, 
            working_dir=".", timeout=None) -> [".mag", ".cor", ".raw", 
                                                    ".mch", ".tfr", ".coo", ".mtr"]:
    """
    Ejecuta DAOMASTER con un archivo .mch de entrada y una lista de archivos de referencia
    (puede ser .als u otra extensión), suministrando todas las respuestas interactivas automáticamente:
      1. Lee el master .mch.
      2. Carga la lista de archivos de observación en el mismo orden.
      3. Configura parámetros de matching y transformación.
      4. Procesa prompts finales para generar archivos de salida.

    Args:
        in_mch (str): Ruta al archivo .mch maestro.
        in_path_list (list): Lista de rutas a archivos (.als u otros) en orden.
        out_prefix (str): Prefijo para los archivos de salida.
        verbose (bool): Si True, imprime logs de progreso.
        use_docker (str): Imagen Docker opcional.
        working_dir (str): Directorio base para trabajo temporal.
        timeout (int): Timeout en segundos para la ejecución.

    Returns:
        str: Ruta al archivo .mch final generado.
    """
    try:
        # Preparar carpeta temporal
        filename = os.path.splitext(os.path.basename(in_mch))[0]
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_DAOMASTER_0")))
        temp_mch = os.path.join(temp_dir, os.path.basename(in_mch))
        temp_list = [os.path.join(temp_dir, os.path.basename(p)) for p in in_path_list]
        log_file = os.path.join(temp_dir, "daomaster.log")
        final_log = os.path.join(out_dir, "daomaster.log")

        # Archivos posibles a copiar
        out_files = [
            ("no_ext", new_id),
            (".mag", out_mag),
            (".cor", out_cor),
            (".raw", out_raw),
            (".mch", out_mch),
            (".tfr", out_tfr),
            ("no_ext", out_coo),
            ("no_ext", out_mtr),
        ]


        # Copiar archivos a temp
        shutil.copy(in_mch, temp_mch)
        for src, dst in zip(in_path_list, temp_list):
            shutil.copy(src, dst)

        # Validar existencia
        check_file(temp_mch, "Master .mch no encontrado:")

        out_list = []
        for ext, use in out_files:
            if use:
                if ext=="no_ext":
                    out_list.extend(["y"])
                else:
                    out_list.extend(["y", f"temp_out{ext}"])
            else:
                out_list.append("n")

        cmd_list = ['daomaster << EOF >> daomaster.log',
                        filename, 
                        min_min_fr, 
                        f"{max_sig}", 
                        deg_free,
                        crit_rad,
                        *[""]*len(temp_list),
                        *[f"{i}" for i in range(10, 0, -1) 
                                    for _ in range(int(15*min(1,mult)) - i // 2)],
                        "0",
                        *out_list, 
                        "EOF"]
        cmd = '\n'.join(cmd_list)

        if verbose:
            print(f"daomaster: {filename}")
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | daomaster >> daomaster.log'"
            runner(cmd, os.path.relpath(temp_dir, start=working_dir), timeout)
        else:
            runner = run_proc
            cmd = cmd
            runner(cmd, temp_dir, timeout)

        move_file_noreplace(log_file, final_log)

        results = {}

        if out_mag:
            temp_out = os.path.join(temp_dir, "temp_out.mag")
            final_out = os.path.join(out_dir, f"{out_prefix}.mag")
            results['out_mag'] = move_file_noreplace(temp_out, final_out)

        if out_cor:
            temp_out = os.path.join(temp_dir, "temp_out.cor")
            final_out = os.path.join(out_dir, f"{out_prefix}.cor")
            results['out_cor'] = move_file_noreplace(temp_out, final_out)

        if out_raw:
            temp_out = os.path.join(temp_dir, "temp_out.raw")
            final_out = os.path.join(out_dir, f"{out_prefix}.raw")
            results['out_raw'] = move_file_noreplace(temp_out, final_out)

        if out_mch:
            temp_out = os.path.join(temp_dir, "temp_out.mch")
            final_out = os.path.join(out_dir, f"{out_prefix}.mch")
            results['out_mch'] = move_file_noreplace(temp_out, final_out)

        if out_tfr:
            temp_out = os.path.join(temp_dir, "temp_out.tfr")
            final_out = os.path.join(out_dir, f"{out_prefix}.tfr")
            results['out_tfr'] = move_file_noreplace(temp_out, final_out)

        if out_coo:
            coo_list = []
            for path in in_path_list:
                name = os.path.splitext(os.path.basename(path))[0]
                temp_coo = os.path.join(temp_dir, f"{name}.coo")
                final_coo = os.path.join(out_dir, f"{name}.coo")
                if os.path.exists(temp_coo):
                    coo_list.append(move_file_noreplace(temp_coo, final_coo))
            results['out_coo'] = coo_list

        if out_mtr:
            mtr_list = []
            for path in in_path_list:
                name = os.path.splitext(os.path.basename(path))[0]
                temp_mtr = os.path.join(temp_dir, f"{name}.mtr")
                final_mtr = os.path.join(out_dir, f"{name}.mtr")
                if os.path.exists(temp_mtr):
                    mtr_list.append(move_file_noreplace(temp_mtr, final_mtr))
            results['out_mtr'] = mtr_list

        # check_file(results.get("out_mch", ""), "Error: .mch no creado")

        if verbose:
            for key in ["out_mag", "out_cor", "out_raw", "out_mch", "out_tfr"]:
                if key in results:
                    print(f"-> {results[key]}")
    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass
    return results

