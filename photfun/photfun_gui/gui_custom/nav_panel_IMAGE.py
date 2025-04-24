import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm
from pprint import pformat  # Import pprint para formateo de texto
from . import input_local_file_ui, input_local_file_server

@module.ui
def nav_panel_IMAGE_ui():
    return ui.page_fluid(
        ui.layout_column_wrap(
            input_local_file_ui("load_local_fits", "Load FITS"),
            ui.input_action_button("broadcast_fits", "Send FITS", 
                                    icon=icon_svg("tower-cell")),
        ),
        ui.layout_column_wrap(
            [
                ui.h4("Fits preview"),
                ui.output_plot("plot_fits"),
            ],
            [

                ui.input_select("select_fits_file", ui.h4("Select FITS file"), choices=[], size=10),
                ui.input_action_button("show_info", "File Info", icon=icon_svg("circle-info"), size="l"),

            ],
        ),
    )

@module.server
def nav_panel_IMAGE_server(input, output, session, photfun_client, samp_client, 
                            nav_table_sideview_update, fits_df):
    event_load_local_fits, input_load_local_fits  = input_local_file_server("load_local_fits", ".fits")

    # Evento al cargar archivos FITS
    @reactive.Effect
    @reactive.event(event_load_local_fits)
    def _():
        archivos = input_load_local_fits()
        if not archivos:
            return
        print("PhotFun: Load")
        carpetas = [f for f in archivos if os.path.isdir(f)]
        archivos_fits = [f for f in archivos if os.path.isfile(f)]
        for carpeta in carpetas:
            photfun_client.add_fits(carpeta)
        if len(archivos_fits) > 1:
            photfun_client.add_fits(archivos_fits)
        elif len(archivos_fits) == 1:
            photfun_client.add_fits(archivos_fits[0])
        nav_table_sideview_update(tables=False, psf=False)

    # Obtener la ruta del FITS seleccionado en la tabla
    @reactive.Calc
    def selected_fits():
        selected_row = fits_df.data_view(selected=True)
        if selected_row.empty:
            return None  # No hay selección
        selected_id = selected_row.iloc[0]["FITS"]
        fits_obj = next((f for f in photfun_client.fits_files if f.id == selected_id), None)
        return fits_obj if fits_obj else None

    # Actualizar opciones del input select cuando cambia el FITS seleccionado
    @reactive.Effect
    @reactive.event(selected_fits)
    def update_fits_selection():
        fits_obj = selected_fits()
        if not fits_obj or not fits_obj.path:
            ui.update_select("select_fits_file", choices={})  # Limpiar si no hay archivos
            return
        
        choices = {i:os.path.basename(p) for i, p in enumerate(fits_obj.path) if not os.path.basename(p).startswith("ERROR.")}
        ui.update_select("select_fits_file", choices=choices, selected=0)

    # Graficar el FITS seleccionado con la opción elegida en el select
    @render.plot()
    def plot_fits():
        fits_obj = selected_fits()
        if not fits_obj or not fits_obj.path:
            return
        
        selected_index = input.select_fits_file()
        if selected_index is None or selected_index == "":
            return
        
        selected_index = int(selected_index)  # Convertir el índice a entero
        if len(fits_obj.path)<=selected_index:
            return

        fig_main, ax = plt.subplots(figsize=(7.5, 7.5))
        image_data = np.array(fits_obj.image(selected_index).data)
        image_data = np.nan_to_num(image_data, nan=0)
        image_data[image_data <= 0] = 0.0001
        vmin, vmax = np.percentile(image_data, [25, 90])
        ax.imshow(image_data, cmap='gray', norm=LogNorm(vmin=vmin, vmax=vmax))
        plt.gca().invert_yaxis()
        fig_main.tight_layout()
        return fig_main

    @reactive.Effect
    @reactive.event(input.show_info)
    def show_fits_info():
        # Modal para mostrar la información del FITS
        
        ui_file_info =  ui.modal(                
                            # ui.div(
                                ui.output_text_verbatim("fits_header_info"),
                                title="FITS Info",
                                id="fits_info_modal",
                                easy_close=True,
                                size="l",
                                style=("height: 30vh; min-height: 150px; max-height: 400px;"
                                # "width: 300vh; min-width: 150px; max-width: 400px;"
                                "overflow-y: auto;"
                                "overflow-x: auto;"
                                "border: 1px solid #dee2e6;"
                                "border-radius: 5px;"
                                "padding: 15px;"
                                "margin-top: 20px;"
                                "background-color: #f8f9fa;")
                            # )
                        ),
        ui.modal_show(ui_file_info)

    @render.text
    def fits_header_info():
        fits_obj = selected_fits()
        if not fits_obj or not fits_obj.path:
            return "No FITS file selected."
        
        selected_index = input.select_fits_file()
        if selected_index is None or selected_index == "":
            return "No FITS file selected."
        
        selected_index = int(selected_index)
        info = fits_obj.file_info(selected_index)

        # Usar pformat para formatear mejor la salida
        return pformat(info, indent=2, width=80, sort_dicts=False)

    @reactive.Effect
    @reactive.event(input.broadcast_fits)
    def samp_broadcast_fits():
        fits_obj = selected_fits()
        if not fits_obj or not fits_obj.path:
            return "No FITS file selected."
        selected_index = input.select_fits_file()
        if selected_index is None or selected_index == "":
            return "No FITS file selected."
        print(f"PhotFun: broadcast({fits_obj.alias})")
        selected_index = int(selected_index)
        out_path = fits_obj.path[selected_index]
        samp_client.broadcast_fits(fits_obj.path[selected_index], fits_obj.alias[selected_index])
    return
