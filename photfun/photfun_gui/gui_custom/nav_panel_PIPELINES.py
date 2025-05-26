import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm
# from astropy.visualization import ZScaleInterval
from .nav_panel_pipelines import (
    nav_panel_PIPELINE1_ui, nav_panel_PIPELINE1_server, PIPELINE1_description,
    nav_panel_PIPELINE2_ui, nav_panel_PIPELINE2_server, PIPELINE2_description,
    nav_panel_PIPELINE3_ui, nav_panel_PIPELINE3_server, PIPELINE3_description,
    nav_panel_PIPELINE4_ui, nav_panel_PIPELINE4_server, PIPELINE4_description,
    nav_panel_PIPELINE5_ui, nav_panel_PIPELINE5_server, PIPELINE5_description,
)
from .nav_panel_daophot_fn import (
    nav_panel_opt_ALLSTAR_ui, nav_panel_opt_ALLSTAR_server,
    nav_panel_opt_DAOPHOT_ui, nav_panel_opt_DAOPHOT_server,
    nav_panel_opt_PHOTO_ui, nav_panel_opt_PHOTO_server,
    nav_panel_opt_LOADOPT_ui, nav_panel_opt_LOADOPT_server,
)


@module.ui
def nav_panel_PIPELINES_ui():
    m = ui.page_fillable(
            ui.layout_columns(
                ui.page_fillable(
                    ui.div(
                        ui.h4("Pipeline Description", class_="d-inline"),
                        ui.div(
                            ui.div(
                                ui.input_action_button(
                                    "toggle_docker",
                                    "Connect Docker",  # Puedes actualizar dinámicamente el label desde el server
                                    icon=icon_svg("plug"),
                                    class_="btn-sm btn-outline-primary"
                                ),
                                class_="d-flex flex-column align-items-end"
                            ),
                            class_="float-end"
                        ),
                        ui.div(
                            ui.output_text_verbatim("pipeline_description"),
                           style=(
                                "flex: 1;"
                                "overflow-y: auto;"
                                "border: 1px solid #dee2e6;"
                                "border-radius: 5px;"
                                "padding: 15px;"
                                "margin-top: 20px;"
                                "background-color: #f8f9fa;"
                            )
                        )
                    )
                ),
                ui.page_fillable(
                    ui.navset_card_tab(  
                        ui.nav_panel("PIPELINE 1", nav_panel_PIPELINE1_ui("nav_panel_PIPELINE1"), value="PIPELINE1"),
                        ui.nav_panel("PIPELINE 2", nav_panel_PIPELINE2_ui("nav_panel_PIPELINE2"), value="PIPELINE2"),
                        ui.nav_panel("PIPELINE 3", nav_panel_PIPELINE3_ui("nav_panel_PIPELINE3"), value="PIPELINE3"),
                        ui.nav_panel("PIPELINE 4", nav_panel_PIPELINE4_ui("nav_panel_PIPELINE4"), value="PIPELINE4"),
                        ui.nav_panel("PIPELINE 5", nav_panel_PIPELINE5_ui("nav_panel_PIPELINE5"), value="PIPELINE5"),
                        ui.nav_menu(
                            "Settings",
                            ui.nav_panel("ALLSTAR", nav_panel_opt_ALLSTAR_ui("nav_panel_opt_ALLSTAR"), value="opt_ALLSTAR"),
                            ui.nav_panel("DAOPHOT", nav_panel_opt_DAOPHOT_ui("nav_panel_opt_DAOPHOT"), value="opt_DAOPHOT"),
                            ui.nav_panel("PHOTO", nav_panel_opt_PHOTO_ui("nav_panel_opt_PHOTO"), value="opt_PHOTO"),
                            ui.nav_panel("LOADOPT", nav_panel_opt_LOADOPT_ui("nav_panel_opt_LOADOPT"), value="opt_LOADOPT"),
                        ),
                        id="tabs_pipelines",  
                    ),
                ),  
                col_widths=(6, 6),
            ),
        )
    return m


@module.server
def nav_panel_PIPELINES_server(input, output, session, photfun_client, nav_table_sideview_update, fits_df, tables_df, input_tabs_main):
        
    descriptions = {
        "PIPELINE1": PIPELINE1_description,
        "PIPELINE2": PIPELINE2_description,
        "PIPELINE3": PIPELINE3_description,
        "PIPELINE4": PIPELINE4_description,
        "PIPELINE5": PIPELINE5_description,
        }

    @reactive.Effect
    @reactive.event(input_tabs_main)
    def _():
        if input_tabs_main()=="PIPELINES":
            docker_toggle_handler()
    
    def docker_toggle_handler():
        if photfun_client.docker_container[0]:
            ui.update_action_button("toggle_docker", 
                label="Disconnect Docker",
                icon=icon_svg("plug-circle-xmark"), 
            )
        else:
            ui.update_action_button("toggle_docker", 
                label="Connect Docker",
                icon=icon_svg("plug"),
            )

    # Obtener la imagen FITS seleccionada
    @reactive.Calc
    def selected_fits():
        selected_row = fits_df.data_view(selected=True)
        if selected_row.empty:
            return None
        selected_id = selected_row.iloc[0]["FITS"]
        fits_obj = next((f for f in photfun_client.fits_files if f.id == selected_id), None)
        return fits_obj

    # Obtener la tabla seleccionada
    @reactive.Calc
    def selected_table():
        selected_row = tables_df.data_view(selected=True)
        if selected_row.empty:
            return None
        selected_id = selected_row.iloc[0]["Table"]
        table_obj = next((f for f in photfun_client.tables if f.id == selected_id), None)
        return table_obj

    _ = nav_panel_PIPELINE1_server("nav_panel_PIPELINE1", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_pipelines, selected_fits)
    _ = nav_panel_PIPELINE2_server("nav_panel_PIPELINE2", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_pipelines, selected_fits)
    _ = nav_panel_PIPELINE3_server("nav_panel_PIPELINE3", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_pipelines, selected_fits)
    _ = nav_panel_PIPELINE4_server("nav_panel_PIPELINE4", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_pipelines, selected_fits)
    _ = nav_panel_PIPELINE5_server("nav_panel_PIPELINE5", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_pipelines, selected_fits)
    _ = nav_panel_opt_ALLSTAR_server("nav_panel_opt_ALLSTAR", photfun_client, input_tabs_main, input.tabs_pipelines)
    _ = nav_panel_opt_DAOPHOT_server("nav_panel_opt_DAOPHOT", photfun_client, input_tabs_main, input.tabs_pipelines)
    _ = nav_panel_opt_PHOTO_server("nav_panel_opt_PHOTO", photfun_client, input_tabs_main, input.tabs_pipelines)
    _ = nav_panel_opt_LOADOPT_server("nav_panel_opt_LOADOPT", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_pipelines)


    # Dynamic description display
    @output
    @render.text
    def pipeline_description():
        active = input.tabs_pipelines()
        desc = descriptions.get(active, None)
        if not desc:
            return "Select a Pipeline"
        # add other pipelines as needed
        return desc
    
    @reactive.Effect
    @reactive.event(input.toggle_docker)
    def toggle_docker():
        if photfun_client.docker_container[0]:
            # Mensaje pequeño y gris mientras desconecta
            ui.notification_show(
                ui.HTML("Disconnecting <strong>Docker</strong>…"),
                duration=None,
                close_button=False,
                id="disconnecting_docker"
            )
            photfun_client.disconnect_docker()
            ui.notification_remove("disconnecting_docker")
            # Mensaje conciso y verde tras desconexión
            ui.notification_show(
                ui.HTML("<strong>Disconnected</strong>"),
                type="message",
                duration=2,
                id="disconnected"
            )
            docker_toggle_handler()
        else:
            # Mensaje pequeño y gris mientras conecta
            ui.notification_show(
                ui.HTML("Connecting <strong>Docker</strong>…"),
                duration=None,
                close_button=False,
                id="connecting_docker"
            )
            photfun_client.reconnect_docker()
            ui.notification_remove("connecting_docker")
            if photfun_client.docker_container[0]:
                # Mensaje conciso y verde tras conexión
                ui.notification_show(
                    ui.HTML("<strong>Connected</strong>"),
                    type="message",
                    duration=2,
                    id="connected"
                )
            else:
                # Mensaje conciso y rojo si falla
                ui.notification_show(
                    ui.HTML("<strong>Failed</strong>"),
                    type="error",
                    duration=2,
                    id="failed"
                )
            docker_toggle_handler()


