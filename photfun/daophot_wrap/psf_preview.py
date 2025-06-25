from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import re
import tempfile
import shutil
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm
from astropy.io import fits
from .docker_handler import run_proc, docker_run
module_dir = os.path.dirname(__file__)


def psf_preview(in_psf, in_daophot, out_png, verbose=True, 
                	use_docker=None, working_dir=".", timeout=None) -> [".png"]:
    try:
        tools_dir = os.path.join(module_dir, "tools")
        in_fits = os.path.abspath(os.path.join(tools_dir, "fake.fits"))
        in_lst = os.path.abspath(os.path.join(tools_dir, "fake.lst"))
        # Copiar archivos necesarios a la carpeta temporal
        filename = os.path.splitext(os.path.basename(in_fits))[0]
        # Crear carpeta temporal
        temp_dir = os.path.abspath(temp_mkdir(os.path.join(working_dir, f"{filename}_PSF_PREVIEW_0")))
        temp_fits = os.path.join(temp_dir, os.path.basename(in_fits))
        temp_psf = os.path.join(temp_dir, os.path.basename(in_psf))
        temp_lst = os.path.join(temp_dir, os.path.basename(in_lst))
        temp_daophot  = os.path.join(temp_dir, "daophot.opt")
        temp_png  = os.path.join(temp_dir, "out_png_file.png")
        temp_log  = os.path.join(temp_dir, "psf_preview.log")
        out_log = os.path.join(os.path.dirname(out_png), "psf_preview.log")
        temp_out_fits = os.path.join(temp_dir, "out_fits.fits")

        shutil.copy(in_fits, temp_fits)
        shutil.copy(in_psf, temp_psf)
        shutil.copy(in_lst, temp_lst)
        shutil.copy(in_daophot, temp_daophot)
        in_highgood = extract_hi(temp_daophot)
        
        out_png_filename = os.path.basename(temp_png)
        temp_psf_filename = os.path.basename(in_psf)
        temp_lst_filename = os.path.basename(in_lst)
        temp_out_fits_filename = os.path.basename(temp_out_fits)

        if verbose:
            print(f"daophot: preview_psf({filename})")
        check_file(temp_fits, "fits file input: ")
        check_file(temp_daophot, "opt file input: ")
        check_file(temp_psf, "psf file input: ")
        check_file(temp_lst, "positions file input: ")
        cmd_list = ['daophot << EOF >> psf_preview.log', f'at {filename}', 
                        'add', 
                        temp_psf_filename, "1", f"{in_highgood}",
                        temp_lst_filename, temp_out_fits_filename, "",
                        'exit', 'EOF']
        cmd = '\n'.join(cmd_list)

              
        # Ejecutar en la carpeta temporal
        if use_docker:
            runner = docker_run(use_docker)
            joined_cmds = '\n'.join(cmd_list[1:])
            cmd = "sh -c 'printf \"%s\\n\" \""+f"{joined_cmds}"+"\" | daophot >> add.log'"
            runner(cmd, os.path.relpath(temp_dir, start=working_dir), timeout)
        else:
            runner = run_proc
            cmd = cmd
            runner(cmd, temp_dir, timeout)

        # Recortar y convetir fits a png
        create_crop_png(temp_out_fits, temp_png)

        # Mover el archivo de salida a la ubicación final
        final_out_png = move_file_noreplace(temp_png, out_png)
        move_file_noreplace(temp_log, out_log)

        check_file(final_out_png, "preview not created: ")
        if verbose:
            print(f"  -> {final_out_png}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return final_out_png

def extract_hi(path):
    pattern = re.compile(r'^\s*hi\s*=\s*([-+]?[0-9]*\.?[0-9]+)')
    with open(path, 'r') as f:
        for line in f:
            m = pattern.match(line)
            if m:
                return float(m.group(1))
    raise KeyError("'hi' not found.")

def create_crop_png(input_fits: str,
                    output_png: str,
                    center_x: int = 75,
                    center_y: int = 75,
                    size: int = 30):
    """
    Carga un FITS, recorta un cuadrado de tamaño `size` píxeles
    centrado en (center_x, center_y) y guarda como PNG con dos paneles:
      - Izquierda: la imagen recortada con ticks.
      - Derecha: perfil horizontal a través de la fila central.
    """
    # 1. Leer FITS
    with fits.open(input_fits) as hdul:
        data = hdul[0].data

    # 2. Definir límites del recorte
    half = size // 2
    x_min = int(max(center_x - half, 0))
    x_max = int(min(center_x + half, data.shape[1]))
    y_min = int(max(center_y - half, 0))
    y_max = int(min(center_y + half, data.shape[0]))

    # 3. Extraer y limpiar
    cut = data[y_min:y_max, x_min:x_max]
    cut = np.nan_to_num(cut, nan=0.0)
    cut[cut <= 0] = 0.0001
    vmin, vmax = np.percentile(cut, [25, 90])

    # 4. Preparar figura de 1x2
    fig, (ax_img, ax_prof) = plt.subplots(
        nrows=1, ncols=2, figsize=(8, 4), dpi=150,
        gridspec_kw={'width_ratios': [1, 1], 'wspace': 0.3}
    )

    # 5. Panel de imagen
    extent = [x_min-75, x_max-75, y_min-75, y_max-75]
    ax_img.imshow(cut, origin='lower', cmap='gray', extent=extent
                    , norm=LogNorm())
    ax_img.set_xlim(x_min-75, x_max-75)
    ax_img.set_ylim(y_min-75, y_max-75)
    ax_img.set_xlabel('X (píxels)')
    ax_img.set_ylabel('Y (píxels)')

    # ticks cada 5 pixeles
    xticks = np.arange(x_min, x_max+1, 5)-75
    yticks = np.arange(y_min, y_max+1, 5)-75
    ax_img.set_xticks(xticks)
    ax_img.set_yticks(yticks)
    ax_img.tick_params(rotation=90)

    # Resaltar tick central
    if center_x in xticks:
        idx = list(xticks).index(center_x)
        ax_img.get_xticklabels()[idx].set_fontweight('bold')
    if center_y in yticks:
        idy = list(yticks).index(center_y)
        ax_img.get_yticklabels()[idy].set_fontweight('bold')

    # 6. Panel de perfil horizontal
    # fila central relativa
    row_idx = cut.shape[0] // 2
    profile = cut[row_idx, :]
    # eje x relativo al centro
    rel_x = np.arange(cut.shape[1]) - half

    ax_prof.plot(rel_x, np.log10(profile), lw=1)
    ax_prof.set_xlabel('pixels')
    ax_prof.set_ylim(0, max(np.log10(profile))*1.55)
    # ax_prof.set_ylabel('Intensidad')
    # ax_prof.set_title('Perfil horizontal')

    # marcar x=0
    # ax_prof.axvline(0, color='red', ls='--', lw=1)
    # ax_prof.grid(True, ls=':', lw=0.5)

    # plt.tight_layout()
    fig.savefig(output_png, dpi=150)
    plt.close(fig)


# # Ejemplo de uso:
# create_crop_png(
#     input_fits="/data/ciquezada/Projects/photfun/photfun/daophot_wrap/tools/fake.fits",
#     output_png="/data/ciquezada/Projects/photfun/photfun/daophot_wrap/tools/fake2.png",
#     center_x=75,
#     center_y=75,
#     size=30
# )
# print("PNG guardado en:", "/data/ciquezada/Projects/photfun/photfun/daophot_wrap/tools/fake2.png")


# # Ejemplo de uso:
# create_crop_png(
#     input_fits="fake.fits",
#     output_png="fake2.png",
#     center_x=75,
#     center_y=75,
#     size=30
# )
# print("PNG guardado en:", "fake2.png")
