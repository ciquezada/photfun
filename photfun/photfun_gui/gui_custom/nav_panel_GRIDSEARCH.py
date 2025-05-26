import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm
# from astropy.visualization import ZScaleInterval


@module.ui
def nav_panel_GRIDSEARCH_ui():
    m = ui.page_fillable(
            ui.layout_columns(
                ui.page_fillable(
                    ui.div(
                        ui.h4("Grid Search Description", class_="d-inline"),
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
                        #     ui.output_text_verbatim("pipeline_description"),
                        #    style=(
                        #         "flex: 1;"
                        #         "overflow-y: auto;"
                        #         "border: 1px solid #dee2e6;"
                        #         "border-radius: 5px;"
                        #         "padding: 15px;"
                        #         "margin-top: 20px;"
                        #         "background-color: #f8f9fa;"
                        #     )
                        )
                    )
                ),
                ui.page_fillable(
                    ui.layout_columns(
                        ui.div(
                            # Selectores (sin cambios)
                            ui.input_select("fits_select", "Photometry on FITS", choices={}),
                            ui.input_select("noncentered_table_select", "General Targets", choices={}, width="auto"),  # Lista Tables
                            ui.input_select("centered_table_select", "Final Master Targets", choices={}, width="auto"),  # Lista Tables
                            ui.input_select("table_lst_select", "Select coordinates", choices={}),
                            ui.input_action_button("phot_btn", "RUN GRIDSEARCH", icon=icon_svg("camera")),
                            style="padding-right: 20px; border-right: 1px solid #dee2e6;"
                        ),
                        ui.div(
                            # Nuevos controles de rango
                            ui.tooltip(
                                ui.input_text("grid_fw", "FWHM Range", value="2, 5"),
                                "Comma separated values for FWHM range (Ex: 2.5,3.5)"
                            ),
                            ui.tooltip(
                                ui.input_text("grid_fi", "Fitting Radius Range", value="1, 3"), 
                                "Range for radius of PSF fitting"
                            ),
                            ui.tooltip(
                                ui.input_text("grid_ps", "PSF Radius Range", value="4, 18 "),
                                "Range for PSF model radius"
                            ),
                            ui.tooltip(
                                ui.input_text("grid_is", "Inner Sky Range", value="1, 3"),
                                "Range for inner sky radius"
                            ),
                            ui.tooltip(
                                ui.input_text("grid_os", "Outer Sky Range", value="7, 14"),
                                "Range for outer sky radius"
                            ),
                            ui.tooltip(
                                ui.input_numeric("ncalls_gs", "N Iterations", 
                                            value=50,  # Valor por defecto de 'fi' en opt_daophot_dict
                                            step=1
                                            ),
                                "Sum of several individual exposures, how many?"
                            ),
                            ui.tooltip(
                                ui.input_switch("param_re", "Recentering", value=True),
                                "Automatic re-centering"
                            ),
                            style="padding-left: 20px;"
                        ),
                        col_widths=(8, 4)
                    )
                ),
                col_widths=(6, 6),
            ),
        )
    return m


@module.server
def nav_panel_GRIDSEARCH_server(input, output, session, photfun_client, nav_table_sideview_update, fits_df, input_tabs_main):

    # Obtener la imagen FITS seleccionada
    @reactive.Calc
    def navselected_fits():
        selected_row = fits_df.data_view(selected=True)
        if selected_row.empty:
            return None
        selected_id = selected_row.iloc[0]["FITS"]
        fits_obj = next((f for f in photfun_client.fits_files if f.id == selected_id), None)
        return fits_obj

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices, selected=str(navselected_fits().id) if navselected_fits() else None)
        table_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("noncentered_table_select", choices=table_choices)
        table_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("centered_table_select", choices=table_choices)
        table_lst_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("table_lst_select", choices=table_lst_choices)

    @reactive.Effect
    @reactive.event(input_tabs_main)
    def _():
        if input_tabs_main()=="GRIDSEARCH":
            docker_toggle_handler()
            update_select()
            update_settings()
    
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

    # Obtener el FITS seleccionado
    @reactive.Calc
    def selected_fits():
        selected_id = input.fits_select()
        return next((f for f in photfun_client.fits_files if str(f.id) == selected_id), None)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_noncentered_table():
        selected_id = input.noncentered_table_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_centered_table():
        selected_id = input.centered_table_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)
    
    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_lst_table():
        selected_id = input.table_lst_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

    @reactive.Effect
    @reactive.event(input.param_re)
    def _():
        photfun_client.allstar_recentering = input.param_re()

    # Modificamos los opt files
    def update_settings():
        # Sincronizar valores iniciales
        ui.update_switch("param_re", value=photfun_client.allstar_recentering)

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

    # Función de parseo (nueva)
    def parse_list(txt):
        try:
            return [float(x.strip()) for x in txt.split(",") if x.strip()]
        except ValueError:
            return []

    @reactive.Effect
    @reactive.event(input.phot_btn)
    def phot_action():
        fits_obj = selected_fits()
        find_table = selected_noncentered_table()
        master_table = selected_centered_table()
        coord_list = selected_lst_table()   # This is the coordinate list from the pipeline description
        
        # Parsear rangos
        ranges = {
            'fw': parse_list(input.grid_fw()),
            'fi': parse_list(input.grid_fi()),
            'ps': parse_list(input.grid_ps()),
            'is': parse_list(input.grid_is()),
            'os': parse_list(input.grid_os())
        }

        # Validar rangos
        for param, values in ranges.items():
            if len(values) < 2:
                ui.notification_show(f"The range {param} should have 2 entries", type="error")
                return

        if fits_obj and find_table and master_table and coord_list:
            try:
                # Ejecutar búsqueda grid
                with ui.Progress(min=0, max=1) as p:
                    best_params_obj = photfun_client.grid_search(
                        fits_id=fits_obj.id,
                        find_id=find_table.id,
                        master_id=master_table.id,
                        coords_id=coord_list.id,
                        fw_range=ranges['fw'],
                        fi_range=ranges['fi'],
                        ps_range=ranges['ps'],
                        is_range=ranges['is'],
                        os_range=ranges['os'],
                        n_calls=int(input.ncalls_gs()),
                        pbar=p
                    )
                ui.notification_show(
                    f"Best params created"
                    f" -> [{best_params_obj.id}] {best_params_obj.alias}"
                )
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: Please select FITS image, master list, and coordinate list.", type="warning")
        
        nav_table_sideview_update()
        update_select()

