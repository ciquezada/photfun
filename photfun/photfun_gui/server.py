from shiny import reactive, render
from shiny.types import FileInfo
from shiny import App, ui
from pathlib import Path
import os
import time
import pandas as pd
import tempfile
from astropy.io.votable import from_table, writeto
from astropy.table import Table as astroTable
from urllib.parse import urlparse
from ..misc_tools import move_file_noreplace
from ..photfun_classes import PhotFun
from .gui_custom import (nav_table_sideview_server, nav_panel_IMAGE_server, 
                                            nav_panel_TABLE_server, nav_panel_DAOPHOT_server, 
                                            nav_panel_SELECTION_server, nav_panel_PHOTCUBE_server,
                                            nav_panel_EXPORT_server, nav_panel_LOGS_server, SAMPclient)

app_dir = Path(__file__).parent

def server(input, output, session):
    photfun_client = PhotFun()

    samp_client = SAMPclient()
    samp_message = reactive.Value({})

    # Reactivos de modulos
    nav_table_sideview_update, fits_df, tables_df, psf_df = nav_table_sideview_server("nav_table_sideview", photfun_client, samp_client)
    _ = nav_panel_IMAGE_server("nav_panel_IMAGE", photfun_client, samp_client, nav_table_sideview_update, fits_df)
    _ = nav_panel_TABLE_server("nav_panel_TABLE", photfun_client, samp_client, nav_table_sideview_update, tables_df)
    _ = nav_panel_DAOPHOT_server("nav_panel_DAOPHOT", photfun_client, nav_table_sideview_update, fits_df, tables_df, input.tabs_main)
    _ = nav_panel_SELECTION_server("nav_panel_SELECTION", photfun_client, nav_table_sideview_update, fits_df, tables_df)
    _ = nav_panel_PHOTCUBE_server("nav_panel_PHOTCUBE", photfun_client, samp_client, nav_table_sideview_update, input.tabs_main)
    _ = nav_panel_EXPORT_server("nav_panel_EXPORT", photfun_client, input.tabs_main)
    _ = nav_panel_LOGS_server("nav_panel_LOGS", photfun_client)

    nav_table_sideview_update()

    # Mantener una referencia al cliente para evitar garbage collection
    @reactive.Effect
    def init_samp():
        samp_client.start_samp()
        return samp_client

    # Verificador reactivo en el contexto principal
    @reactive.Effect
    def check_messages():
        reactive.invalidate_later(0.1)
        if samp_client.samp_receiver and samp_client.samp_receiver.new_message:
            new_params = samp_client.samp_receiver.params.copy()
            ui.notification_show(
                f"Received: {new_params.get('name', 'not named')}",
                duration=8,
            )
            samp_message.set(new_params)
            samp_client.samp_receiver.reset_flag()

    @reactive.Effect
    @reactive.event(samp_message)
    def samp_display():
        new_params = samp_message.get()
        # Parsear URL y obtener ruta local
        if new_params.get('mtype')!='table.load.votable' and new_params.get('mtype')!='image.load.fits':
            return
        
        url = urlparse(new_params['url'])
        with tempfile.NamedTemporaryFile(suffix=".vot", delete=False) as tmpfile:
            # Convertir DataFrame de pandas a Table de astropy
            astropy_table = astroTable.read(new_params['url'])
            votable = from_table(astropy_table)
            votable.description = new_params['name']
            
            # Escribir archivo temporal
            writeto(votable, tmpfile.name)   
            src_path = os.path.abspath(tmpfile.name) 
        
        # Construir ruta destino
        original_ext = os.path.splitext(src_path)[1]
        dest_path = os.path.join(photfun_client.working_dir, 
                        f"{os.path.basename(new_params['name'].replace(' ', '_'))}{original_ext}")
        
        # Copiar archivo
        final_dest = move_file_noreplace(src_path, dest_path)
        try:
            # Determinar tipo de archivo
            if new_params.get('mtype') == 'table.load.votable':
                print(f"PhotFun: import({new_params['name']})")
                photfun_client.add_table(final_dest)
                nav_table_sideview_update(fits=False, psf=False)
                
            elif new_params.get('mtype') == 'image.load.fits':
                print(f"PhotFun: import({new_params['name']})")
                photfun_client.add_fits(final_dest)
                nav_table_sideview_update(table=False, psf=False)
        except ValueError as e:
            ui.notification_show(str(e), type="error", duration=10)

    # Ejecutar cleanup cuando la sesión termine
    @session.on_ended
    def on_ended():
        photfun_client.clean_up()
        samp_client.stop_samp()
        pass

