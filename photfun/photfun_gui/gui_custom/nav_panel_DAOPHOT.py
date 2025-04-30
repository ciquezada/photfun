import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm
# from astropy.visualization import ZScaleInterval
from .nav_panel_daophot_fn import (
    nav_panel_FIND_ui, nav_panel_FIND_server,
    nav_panel_PHOT_ui, nav_panel_PHOT_server,
    nav_panel_PICK_ui, nav_panel_PICK_server,
    nav_panel_PSF_ui, nav_panel_PSF_server,
    nav_panel_SUB_ui, nav_panel_SUB_server,
    nav_panel_ALLSTAR_ui, nav_panel_ALLSTAR_server,
    nav_panel_DAOMATCH_ui, nav_panel_DAOMATCH_server,
    nav_panel_CREATE_MASTER_ui, nav_panel_CREATE_MASTER_server,
    nav_panel_opt_DAOPHOT_ui, nav_panel_opt_DAOPHOT_server,
    nav_panel_opt_PHOTO_ui, nav_panel_opt_PHOTO_server,
    nav_panel_opt_ALLSTAR_ui, nav_panel_opt_ALLSTAR_server,
)


@module.ui
def nav_panel_DAOPHOT_ui():
    m = ui.page_fillable(
            ui.layout_columns(
                ui.page_fillable(
                    ui.div(
                        ui.h4("Loaded Data", class_="d-inline"),
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
                        ui.output_plot("plot_fits"),
                    )
                ),
                ui.page_fillable(
                    ui.navset_card_tab(  
                        ui.nav_panel("FIND", nav_panel_FIND_ui("nav_panel_FIND"), value="FIND"),
                        ui.nav_panel("PHOT", nav_panel_PHOT_ui("nav_panel_PHOT"), value="PHOT"),
                        ui.nav_panel("PICK", nav_panel_PICK_ui("nav_panel_PICK"), value="PICK"),
                        ui.nav_panel("PSF", nav_panel_PSF_ui("nav_panel_PSF"), value="PSF"),
                        ui.nav_panel("SUBTRACT", nav_panel_SUB_ui("nav_panel_SUB"), value="SUB"),
                        ui.nav_panel("ALLSTAR", nav_panel_ALLSTAR_ui("nav_panel_ALLSTAR"), value="ALLSTAR"),
                        ui.nav_panel("DAOMATCH", nav_panel_DAOMATCH_ui("nav_panel_DAOMATCH"), value="DAOMATCH"),
                        ui.nav_panel("MASTER", nav_panel_CREATE_MASTER_ui("nav_panel_CREATE_MASTER"), value="CREATE_MASTER"),
                        ui.nav_menu(
                            "Settings",
                            ui.nav_panel("DAOPHOT", nav_panel_opt_DAOPHOT_ui("nav_panel_opt_DAOPHOT"), value="opt_DAOPHOT"),
                            ui.nav_panel("PHOTO", nav_panel_opt_PHOTO_ui("nav_panel_opt_PHOTO"), value="opt_PHOTO"),
                            ui.nav_panel("ALLSTAR", nav_panel_opt_ALLSTAR_ui("nav_panel_opt_ALLSTAR"), value="opt_ALLSTAR"),
                        ),
                        id="tabs_daophot",  
                    ),
                ),  
                col_widths=(6, 6),
            ),
        )
    return m


@module.server
def nav_panel_DAOPHOT_server(input, output, session, photfun_client, nav_table_sideview_update, fits_df, tables_df, input_tabs_main):
        
    @reactive.Effect
    @reactive.event(input_tabs_main)
    def _():
        if input_tabs_main()=="DAOPHOT":
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

    _ = nav_panel_FIND_server("nav_panel_FIND", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_daophot, selected_fits)
    _ = nav_panel_PHOT_server("nav_panel_PHOT", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_daophot, selected_fits)
    _ = nav_panel_PICK_server("nav_panel_PICK", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_daophot, selected_fits)
    _ = nav_panel_PSF_server("nav_panel_PSF", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_daophot, selected_fits)
    _ = nav_panel_SUB_server("nav_panel_SUB", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_daophot, selected_fits)
    _ = nav_panel_ALLSTAR_server("nav_panel_ALLSTAR", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_daophot, selected_fits)
    _ = nav_panel_DAOMATCH_server("nav_panel_DAOMATCH", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_daophot)
    _ = nav_panel_CREATE_MASTER_server("nav_panel_CREATE_MASTER", photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_daophot)
    _ = nav_panel_opt_DAOPHOT_server("nav_panel_opt_DAOPHOT", photfun_client, input_tabs_main, input.tabs_daophot)
    _ = nav_panel_opt_PHOTO_server("nav_panel_opt_PHOTO", photfun_client, input_tabs_main, input.tabs_daophot)
    _ = nav_panel_opt_ALLSTAR_server("nav_panel_opt_ALLSTAR", photfun_client, input_tabs_main, input.tabs_daophot)


    # Graficar la imagen FITS con posiciones de la tabla si está disponible
    @render.plot()
    def plot_fits():
        fits_obj = selected_fits()
        table_obj = selected_table()
        fits_image = fits_obj.image(0) if fits_obj else None
        table_df = table_obj.df(0) if table_obj else None
        
        if not fits_image:
            return
        
        fig, ax = plt.subplots(figsize=(7.5, 7.5))
        image_data = np.array(fits_image.data)
        image_data = np.nan_to_num(image_data, nan=0)
        image_data[image_data <= 0] = 0.0001
        # ax.imshow(image_data, cmap='gray', norm=LogNorm())
        vmin, vmax = np.percentile(image_data, [25, 90])
        ax.imshow(image_data, cmap='gray', norm=LogNorm(vmin=vmin, vmax=vmax))
        ax.invert_yaxis()
        
        if table_df is not None and "X" in table_df and "Y" in table_df:
            ax.scatter(table_df["X"], table_df["Y"], edgecolors='red', facecolors='none', s=30, alpha=0.3)

        
        fig.tight_layout()
        return fig
    
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


