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
def nav_panel_TABLE_ui():
    m = ui.page_fluid(
            ui.layout_column_wrap(
                input_local_file_ui("load_local_table", "Load Table"),
                ui.input_action_button("broadcast_table", "Send Table", 
                                    icon=icon_svg("tower-cell")),
            ),
            ui.h4("Table Preview"),
            ui.output_data_frame("table_preview"),  # Muestra el DataFrame
        )
    return m

@module.server
def nav_panel_TABLE_server(input, output, session, photfun_client, 
                            nav_table_sideview_update, tables_df):
    event_load_local_table, input_load_local_table  = input_local_file_server("load_local_table", 
                                                                                [".csv", ".coo", ".als", 
                                                                                ".ap", ".lst"])

    # Evento al cargar archivos table
    @reactive.Effect
    @reactive.event(event_load_local_table)
    def _():
        archivos = input_load_local_table()
        if not archivos:
            return
        # carpetas = [f for f in archivos if os.path.isdir(f)]
        archivos_table = [f for f in archivos if os.path.isfile(f)]
        # for carpeta in carpetas:
        #     photfun_client.add_table([carpeta])
        for table in archivos_table:
            photfun_client.add_table(table)
        nav_table_sideview_update()

    # Obtener la ruta del table seleccionado en la tabla
    @reactive.Calc
    def selected_table():
        selected_row = tables_df.data_view(selected=True)
        if selected_row.empty:
            return None  # No hay selección
        selected_id = selected_row.iloc[0]["Table"]
        table_obj = next((f for f in photfun_client.tables if f.id == selected_id), None)
        return table_obj.df if table_obj else None

    # Mostrar el DataFrame de la tabla seleccionada
    @render.data_frame
    def table_preview():
        table_df = selected_table()
        return table_df  # Retorna el DataFrame para visualizarlo

    return