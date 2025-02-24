import sys
sys.path.append("/data/ciquezada/Projects/py_photsuite")
### ERASE AFTER
import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm
from photfun.photfun_gui.gui_custom import input_local_file_ui, input_local_file_server

@module.ui
def nav_panel_IMAGE_ui():
    m = ui.page_fluid(
            ui.layout_column_wrap(
                input_local_file_ui("load_local_fits", "Load FITS"),
                ui.input_action_button("broadcast_fits", "Send FITS", 
                                        icon=icon_svg("tower-cell")),
            ),
            ui.h4("Fits preview"),
            ui.output_plot("plot_fits"),
        )
    return m

@module.server
def nav_panel_IMAGE_server(input, output, session, photfun_client, 
                            nav_table_sideview_update, fits_df):
    event_load_local_fits, input_load_local_fits  = input_local_file_server("load_local_fits", ".fits")

    # Evento al cargar archivos FITS
    @reactive.Effect
    @reactive.event(event_load_local_fits)
    def _():
        archivos = input_load_local_fits()
        if not archivos:
            return
        carpetas = [f for f in archivos if os.path.isdir(f)]
        archivos_fits = [f for f in archivos if os.path.isfile(f)]
        for carpeta in carpetas:
            photfun_client.add_fits(carpeta)
        if len(archivos_fits)>1:
            photfun_client.add_fits(archivos_fits)
        elif len(archivos_fits)==1:
            photfun_client.add_fits(archivos_fits[0])
        nav_table_sideview_update()

    # Obtener la ruta del FITS seleccionado en la tabla
    @reactive.Calc
    def selected_fits():
        selected_row = fits_df.data_view(selected=True)
        if selected_row.empty:
            return None  # No hay selección
        selected_id = selected_row.iloc[0]["FITS"]
        fits_obj = next((f for f in photfun_client.fits_files if f.id == selected_id), None)
        return fits_obj.image if fits_obj else None

    # Graficar el FITS seleccionado
    @render.plot()
    def plot_fits():
        fits_image = selected_fits()
        if not fits_image:
            return
        fig_main = plt.figure(figsize=(7.5, 7.5))
        image_data = np.array(fits_image.data)
        image_data[image_data < 0] = 1
        plt.imshow(image_data, cmap='gray', norm=LogNorm())
        plt.gca().invert_yaxis()
        fig_main.tight_layout()
        return fig_main

    return