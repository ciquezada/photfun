import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm
from pprint import pformat  # Import pprint para formateo de texto
from . import input_local_file_ui, input_local_file_server
from . import output_save_location_ui, output_save_location_server
from ...misc_tools import cube_slicer, spectra_compile, daophot_pbar

@module.ui
def nav_panel_PHOTCUBE_ui():
    return ui.page_fluid(
        ui.layout_column_wrap(
            [
                ui.h4("Slice & Load Datacube"),
                ui.div(
                    input_local_file_ui("load_slice_datacube", "Search DataCube", width="auto"),
                ),
            ],
            [
                ui.h4("Spectra compiler"),
                ui.div(
                    ui.input_select("master_select", "Select MasterList", choices={}, width="auto"),
                    ui.input_select("table_select", "Select Spectra Photometry", choices={}, width="auto"),
                    output_save_location_ui("btn_export", "Compile Spectra"),
                    style="padding-left: 20px; border-left: 1px solid #dee2e6;"
                ),
            ],
        ),
        
    )

@module.server
def nav_panel_PHOTCUBE_server(input, output, session, photfun_client, samp_client, 
                            nav_table_sideview_update, input_tabs_main):
    event_load_local_fits, input_load_local_fits  = input_local_file_server("load_slice_datacube", ".fits")
    event_save_location, input_save_location  = output_save_location_server("btn_export")

    # Cargar opciones de FITS en el select_input
    @reactive.Effect
    @reactive.event(input_tabs_main)
    def _():
        if input_tabs_main()=="PHOTCUBE":
            objects = [table for table in photfun_client.tables]
            choices = {obj.id: f"[{obj.id}] {obj.alias}" for obj in objects}
            ui.update_select("master_select", choices=choices)
            objects = [table for table in photfun_client.tables]
            choices = {obj.id: f"[{obj.id}] {obj.alias}" for obj in objects}
            ui.update_select("table_select", choices=choices)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_master_table():
        selected_id = input.master_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_spectra_table():
        selected_id = input.table_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)


    # Evento al guardar archivos FITS
    @reactive.Effect
    @reactive.event(event_save_location)
    def _():
        out_path = input_save_location()
        master_obj = selected_master_table()
        spectra_obj = selected_spectra_table()
        print("PhotFun: Compile spectra")

        if master_obj and spectra_obj:
            try:
                with ui.Progress(min=0, max=1) as p1:
                    p1.set(message="Compiling spectra")
                    pbar_extract = daophot_pbar(p1, "Compiling spectra")
                    with ui.Progress(min=0, max=1) as p2:
                        p2.set(message="Saving spectra", detail="Waiting compilation...")
                        pbar_save = daophot_pbar(p2, "Saving spectra")
                        spectra_compile(spectra_obj.path, master_obj.path[0], out_path, 
                                            pbar_extract=pbar_extract, pbar_save=pbar_save)
                ui.notification_show(f"Spectra compilation complete")
                print(" -> Spectra compilation complete")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: Master or Photometry not selected.", type="warning")

    # Evento al cargar archivos FITS
    @reactive.Effect
    @reactive.event(event_load_local_fits)
    def _():
        archivos = input_load_local_fits()
        if not archivos:
            return
        print("PhotFun: Slice")
        carpetas = [f for f in archivos if os.path.isdir(f)]
        archivos_fits = [f for f in archivos if os.path.isfile(f)]
        if len(carpetas) or len(archivos)>1:
            ui.notification_show("Error: not single FITS selected.", type="warning")
            return 0
        else:
            try:
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Slicing Cube")
                    datacube_slices = cube_slicer(archivos_fits[0], pbar)
                out_fits_obj = photfun_client.add_fits(datacube_slices)
                ui.notification_show(f"Datacube slice complete\n -> [{out_fits_obj.id}] {out_fits_obj.alias}")
                nav_table_sideview_update(tables=False, psf=False)
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        # nav_table_sideview_update(tables=False, psf=False)

    return
