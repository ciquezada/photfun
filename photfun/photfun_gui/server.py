import sys
sys.path.append("/data/ciquezada/Projects/py_photsuite")
### ERASE AFTER
from shiny import reactive, render
from shiny.types import FileInfo
from shiny import App, ui
from pathlib import Path
import os
import pandas as pd
from photfun.photfun_classes import PhotFun
from photfun.photfun_gui.gui_custom import nav_table_sideview_server
from photfun.photfun_gui.gui_custom import nav_panel_IMAGE_server
from photfun.photfun_gui.gui_custom import nav_panel_TABLE_server
from photfun.photfun_gui.gui_custom import nav_panel_DAOPHOT_server


app_dir = Path(__file__).parent

def server(input, output, session):
    photfun_client = PhotFun()

    # Reactivos de modulos
    nav_table_sideview_update, fits_df, tables_df, psf_df = nav_table_sideview_server("nav_table_sideview", photfun_client)
    _ = nav_panel_IMAGE_server("nav_panel_IMAGE", photfun_client, nav_table_sideview_update, fits_df)
    _ = nav_panel_TABLE_server("nav_panel_TABLE", photfun_client, nav_table_sideview_update, tables_df)
    _ = nav_panel_DAOPHOT_server("nav_panel_DAOPHOT", photfun_client, nav_table_sideview_update, fits_df, tables_df, input.tabs_main)

    # photfun_client.add_fits("/data/ciquezada/Projects/py_photsuite/photfun/photfun_classes/input/12_white.fits")
    # photfun_client.find(0)
    # photfun_client.phot(0, 1)
    # photfun_client.pick(0, 2)
    # photfun_client.psf(0, 2, 3)
    # photfun_client.sub(0, 4, 5)
    # photfun_client.allstar(0, 4, 2)
    nav_table_sideview_update()

    @reactive.Effect
    @reactive.event(input.broadcast_fits)
    def broadcast_fits():
        daofun_client.broadcast_fits()

    @reactive.Effect
    @reactive.event(input.activate_samp)
    def receive_samp():
        if input.activate_samp():
            print(input.activate_samp())
            daofun_client.start_samp()
        else:
            daofun_client.stop_samp()

    @render.text
    @reactive.event(input.receive_samp)
    def message_samp():
        if daofun_client.samp_receiver.received:
            return str(daofun_client.samp_receiver.params)

    # Ejecutar cleanup cuando la sesión termine
    @session.on_ended
    def on_ended():
        photfun_client.clean_up()

