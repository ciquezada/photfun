import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm
import itertools
import re
# from astropy.visualization import ZScaleInterval
from .nav_panel_daophot_fn import (
    nav_panel_FIND_ui, nav_panel_FIND_server,
    nav_panel_PHOT_ui, nav_panel_PHOT_server,
    nav_panel_PICK_ui, nav_panel_PICK_server,
    nav_panel_PSF_ui, nav_panel_PSF_server,
    nav_panel_SUB_ui, nav_panel_SUB_server,
    nav_panel_ALLSTAR_ui, nav_panel_ALLSTAR_server,
    nav_panel_DAOMATCH_ui, nav_panel_DAOMATCH_server,
    nav_panel_DAOMASTER_ui, nav_panel_DAOMASTER_server,
    nav_panel_ALLFRAME_ui, nav_panel_ALLFRAME_server,
    nav_panel_CREATE_MASTER_ui, nav_panel_CREATE_MASTER_server,
    nav_panel_opt_ALLSTAR_ui, nav_panel_opt_ALLSTAR_server,
    nav_panel_opt_DAOPHOT_ui, nav_panel_opt_DAOPHOT_server,
    nav_panel_opt_PHOTO_ui, nav_panel_opt_PHOTO_server,
    nav_panel_opt_LOADOPT_ui, nav_panel_opt_LOADOPT_server,
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
                        ui.nav_panel("DAOMASTER", nav_panel_DAOMASTER_ui("nav_panel_DAOMASTER"), value="DAOMASTER"),
                        ui.nav_panel("ALLFRAME", nav_panel_ALLFRAME_ui("nav_panel_ALLFRAME"), value="ALLFRAME"),
                        ui.nav_panel("MASTER", nav_panel_CREATE_MASTER_ui("nav_panel_CREATE_MASTER"), value="CREATE_MASTER"),
                        ui.nav_menu(
                            "Settings",
                            ui.nav_panel("ALLSTAR", nav_panel_opt_ALLSTAR_ui("nav_panel_opt_ALLSTAR"), value="opt_ALLSTAR"),
                            ui.nav_panel("DAOPHOT", nav_panel_opt_DAOPHOT_ui("nav_panel_opt_DAOPHOT"), value="opt_DAOPHOT"),
                            ui.nav_panel("PHOTO", nav_panel_opt_PHOTO_ui("nav_panel_opt_PHOTO"), value="opt_PHOTO"),
                            ui.nav_panel("LOAD OPT", nav_panel_opt_LOADOPT_ui("nav_panel_opt_LOADOPT"), value="opt_LOADOPT"),
                        ),
                        id="tabs_daophot",  
                    ),
                ),  
                col_widths=(6, 6),
            ),
            # Sección de Plots
            ui.layout_column_wrap(
                ui.input_switch("show_mapping", "(Experimental) Mapping mode", value=False),
                # ui.input_action_button("show_plots", "Show Plots", icon=icon_svg("magnifying-glass-chart")),
            ),
            ui.output_ui("mapping_ui"),
            ui.output_ui("preview_panel")
        )
    return m


@module.server
def nav_panel_DAOPHOT_server(input, output, session, photfun_client, nav_table_sideview_update, fits_df, tables_df, input_tabs_main):

    # Dentro de nav_panel_PSF_server
    plot_map_preview = reactive.Value({})

    panel_args = [photfun_client, nav_table_sideview_update, input_tabs_main, input.tabs_daophot]        
    panel_selection = {}
    panel_selection["FIND"] = nav_panel_FIND_server("nav_panel_FIND", *panel_args)
    panel_selection["PHOT"] = nav_panel_PHOT_server("nav_panel_PHOT", *panel_args)
    panel_selection["PICK"] = nav_panel_PICK_server("nav_panel_PICK", *panel_args)
    panel_selection["PSF"] = nav_panel_PSF_server("nav_panel_PSF", *panel_args)
    panel_selection["SUB"] = nav_panel_SUB_server("nav_panel_SUB", *panel_args)
    panel_selection["ALLSTAR"] = nav_panel_ALLSTAR_server("nav_panel_ALLSTAR", *panel_args)
    panel_selection["DAOMATCH"] = nav_panel_DAOMATCH_server("nav_panel_DAOMATCH", *panel_args)
    panel_selection["DAOMASTER"] = nav_panel_DAOMASTER_server("nav_panel_DAOMASTER", *panel_args)
    panel_selection["ALLFRAME"] = nav_panel_ALLFRAME_server("nav_panel_ALLFRAME", *panel_args)
    panel_selection["CREATE_MASTER"] = nav_panel_CREATE_MASTER_server("nav_panel_CREATE_MASTER", *panel_args)
    panel_selection["opt_ALLSTAR"] = nav_panel_opt_ALLSTAR_server("nav_panel_opt_ALLSTAR", photfun_client, input_tabs_main, input.tabs_daophot)
    panel_selection["opt_DAOPHOT"] = nav_panel_opt_DAOPHOT_server("nav_panel_opt_DAOPHOT", photfun_client, input_tabs_main, input.tabs_daophot)
    panel_selection["opt_PHOTO"] = nav_panel_opt_PHOTO_server("nav_panel_opt_PHOTO", photfun_client, input_tabs_main, input.tabs_daophot)
    panel_selection["opt_LOADOPT"] = nav_panel_opt_LOADOPT_server("nav_panel_opt_LOADOPT", *panel_args)


    def parse_triplet(txt, name):
        """Devuelve (min, max, step) o lanza ValueError."""
        parts = [p.strip() for p in txt.split(",")]
        if len(parts) != 3:
            raise ValueError(f"{name}: 3 comma separated values expected.")
        try:
            mn, mx, st = map(float, parts)
        except:
            raise ValueError(f"{name}: only numbers separated by comma.")
        if not (mn < mx and st > 0):
            raise ValueError(f"{name}: min < max and step > 0 required.")
        return mn, mx, st

    def build_grid_params(ranges_dict):
        """
        ranges_dict: { 'fw':(mn,mx,num_points), ... }
        Retorna lista de dicts con todas las combinaciones.
        """
        keys = list(ranges_dict.keys())
        arrays = [ np.arange(r[0], r[1], r[2]) for r in ranges_dict.values() ]
        grid = []
        for combo in itertools.product(*arrays):
            grid.append({ k: round(val, 2) for k, val in zip(keys, combo) })
        return grid

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
        selected_fits_obj = panel_selection[input.tabs_daophot()].get("selected_fits")
        return selected_fits_obj() if selected_fits_obj else None

    # Obtener la tabla seleccionada
    @reactive.Calc
    def selected_table():
        selected_table_obj = panel_selection[input.tabs_daophot()].get("selected_table")
        return selected_table_obj() if selected_table_obj else None


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

    mapping_ui_cache = reactive.Value(None)

    @render.ui
    def mapping_ui():
        def param_row(key, label, value):
            return ui.div(
                ui.input_checkbox(f"enable_{key}", label, value=False),
                ui.input_text(f"grid_{key}", "", value=value, 
                            placeholder="min, max, step", width="100%"),
                class_="mb-4 p-3 border rounded"
            )
            
        if not input.show_mapping():
            return ui.div(style="height:16px")
        cached = mapping_ui_cache.get()
        if cached is None:
            mapping_ui_cache.set(
                ui.div(
                    ui.layout_column_wrap(
                        param_row("fw", "FWHM Range",         "3, 5, 0.5"),
                        param_row("fi", "Fitting Radius",     "4, 6, 0.5"),
                        param_row("ps", "PSF Radius",         "6, 11, 0.5"),
                        param_row("is", "Inner Sky Radius",   "1, 6, 0.5"),
                        param_row("os", "Outer Sky Radius",   "6, 12, 0.5"),
                        # col_widths=[2,2,2,2,2],
                        style="padding-left: 20px;"
                    ),
                    ui.output_ui("preview_panel")
                )    
            )
        cached = mapping_ui_cache.get()
        panel = ui.div(
                    ui.h5("Parameters to map"),
                    ui.input_action_button("map_btn", f"Map {input.tabs_daophot()}", icon=icon_svg("searchengin"), width="auto"),
                    cached,
                )
        return panel
    
    @reactive.Effect
    @reactive.event(input.map_btn)
    def confirm_run_map_action():
        try: 
            rngs = {}
            for key in ("fw", "fi", "ps", "is", "os"):
                if getattr(input, f"enable_{key}")():
                    txt = getattr(input, f"grid_{key}")()
                    rngs[key] = parse_triplet(txt, key.upper())
            if not rngs:
                ui.notification_show("Activate at least one parameter to map.")
                return
            updates = build_grid_params(rngs)
            if not updates:
                ui.notification_show("Not parameters found.")
                return
            # Estimación de tiempo: 4 minutos por cada 1000 parámetros
            n_params = len(updates)
            estimated_minutes = round(50 * n_params / 6300, 1)

            # Lanza un modal de confirmación con IDs únicos
            ui.modal_show(
                ui.modal(
                    ui.p(f"You're about to map {n_params} grid points. "
                        f"Estimated time: {estimated_minutes} minutes.\n\n"
                        "Do you want to continue?"),
                    title="Confirm Mapping",
                    easy_close=True,
                    footer=ui.row(
                        ui.column(6, ui.input_action_button(f"map_confirm", "Confirm", class_="btn-success w-100")),
                        ui.column(6, ui.input_action_button(f"map_cancel", "Cancel", class_="btn-secondary w-100")),
                    ),
                )
            )
        except Exception as e:
            ui.notification_show(f"Error: {str(e)}", type="error")

    # 2. El efecto que se dispara SOLO cuando el usuario confirma
    @reactive.Effect
    @reactive.event(input.map_confirm)
    def on_map_confirm():
        ui.modal_remove()
        run_map_action()


    # 3. El efecto que se dispara SOLO cuando el usuario cancela
    @reactive.Effect
    @reactive.event(input.map_cancel)
    def on_map_cancel():
        ui.modal_remove()
        ui.notification_show("Mapping cancelled.")


    def run_map_action():
        try: 
            rngs = {}
            for key in ("fw", "fi", "ps", "is", "os"):
                if getattr(input, f"enable_{key}")():
                    txt = getattr(input, f"grid_{key}")()
                    rngs[key] = parse_triplet(txt, key.upper())
            if not rngs:
                ui.notification_show("Activate at least one parameter to map.")
                return
            updates = build_grid_params(rngs)
            if not updates:
                ui.notification_show("Not parameters found.")
                return
            if panel_selection[input.tabs_daophot()].get("map_action"):
                map_action = panel_selection[input.tabs_daophot()].get("map_action")
                plot_map_preview.set(map_action(updates))
            else:
                ui.notification_show("Not mapping configured for this Subroutine.", type="warning")
        except Exception as e:
            ui.notification_show(f"Error: {str(e)}", type="error")

    @render.ui
    def preview_panel():
        map_dict = plot_map_preview.get()
        if not input.show_mapping():
            return ui.div(style="height:16px")
        if not map_dict:
            return ui.div("No previews created", style="color: #666; height: 16px;")

        # 1) Extraer lista de parámetros de la primera key
        first_key = next(iter(map_dict))
        param_names = [kv.split("=")[0] for kv in first_key.split(";")]
        rngs = {}
        for key in ("fw", "fi", "ps", "is", "os"):
            if getattr(input, f"enable_{key}")():
                txt = getattr(input, f"grid_{key}")()
                rngs[key] = parse_triplet(txt, key.upper())

        # 3) Crear un slider por parámetro
        sliders = []
        for p in param_names:
            vs = rngs.get(p)
            if not vs:
                continue
            sliders.append(
                ui.input_slider(
                    f"preview_{p}",        # inputId dinámico
                    p.upper(),             # etiqueta
                    min=vs[0],
                    max=vs[1]-vs[2],
                    value=vs[0],
                    step=vs[2]
                )
            )

        # 4) Devolver todos los sliders + placeholder para la imagen
        return ui.layout_columns(
            ui.div(*sliders),
            ui.output_ui("plot_preview"),
            col_widths=(4, 8),
            style="padding: 10px; border: 1px solid #ddd; border-radius: 4px;"
        )

    @render.ui
    def plot_preview():
        map_dict = plot_map_preview.get()
        if not map_dict:
            return ui.div()  # nada que mostrar

        # Reconstruir la key usando los valores actuales de los sliders
        # => "fi=4.0;ps=20.0;fw=3.0" en el mismo orden que en preview_panel
        first_key = next(iter(map_dict))
        param_names = [kv.split("=")[0] for kv in first_key.split(";")]

        key_parts = []
        for p in param_names:
            val = getattr(input, f"preview_{p}")()
            # Aseguramos el mismo formateo en string que las keys originales
            key_parts.append(f"{p}={val:.2f}")
        key = ";".join(key_parts)

        img_b64 = map_dict.get(key)
        if not img_b64:
            return ui.div(f"No image for {key}", style="color: red;")
        sizes_dict = {  "FIND": "100%",
                        "PHOT": "100%",
                        "PICK": "100%",
                        "PSF": "100%",
                        "ALLSTAR": "100%",
                        }
        return ui.img(src=f"data:image/png;base64,{img_b64}", 
                        width=sizes_dict[input.tabs_daophot()])
