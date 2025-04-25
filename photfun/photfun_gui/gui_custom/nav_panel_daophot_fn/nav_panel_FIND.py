from ....misc_tools import daophot_pbar
from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

    

@module.ui
def nav_panel_FIND_ui():
    return ui.page_fillable(
        ui.layout_columns(
            # Columna izquierda - Controles principales
            ui.div(
                ui.input_select("fits_select", "Find targets on FITS", choices={}, width="auto", selected=None),
                ui.input_action_button("find_btn", "FIND", icon=icon_svg("magnifying-glass"), width="auto"),
                style="padding-right: 20px; border-right: 1px solid #dee2e6;"
            ),
            # Columna derecha - Parámetros DAOPHOT
            ui.div(
                ui.tooltip(
                    ui.input_numeric("param_fw", "FWHM (pixels)", value=3, step=0.1),
                    "Full Width at Half Maximum of stars in pixels"
                ),
                ui.tooltip(
                    ui.input_numeric("param_th", "Threshold (σ)", value=5, step=0.1),
                    "Sigma threshold for a detection"
                ),
                                ui.tooltip(
                ui.input_numeric("param_sum", "Summed", 
                                   value=1,  # Valor por defecto de 'fi' en opt_daophot_dict
                                   step=1
                                   ),
                    "Sum of several individual exposures, how many?"
                ),
                ui.tooltip(
                    ui.input_numeric("param_average", "Averaged", 
                                   value=1,  # Valor por defecto de 'ps' en opt_daophot_dict
                                   step=1,
                                   ),
                    "Average of several individual exposures, how many?"
                ),
                style="padding-left: 20px;"
            ),
            col_widths=(8, 4)
        )
    )

@module.server
def nav_panel_FIND_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot, navselected_fits):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices, selected=str(navselected_fits().id) if navselected_fits() else None)

    # Cargar opciones de FITS en el select_input
    @reactive.Effect
    @reactive.event(input_tabs_main)
    def _():
        if input_tabs_main()=="DAOPHOT":
            update_select()
            update_settings()
    
        # Cargar opciones de FITS en el select_input
    @reactive.Effect
    @reactive.event(input_tabs_daophot)
    def _():
        if input_tabs_daophot()=="FIND":
            update_select()
            update_settings()

    # Obtener el FITS seleccionado
    @reactive.Calc
    def selected_fits():
        selected_id = input.fits_select()
        return next((f for f in photfun_client.fits_files if str(f.id) == selected_id), None)
    
    # Modificamos los opt files
    def update_settings():
        # Sincronizar valores iniciales
        ui.update_numeric("param_fw", value=photfun_client.daophot_opt['fw'])
        ui.update_numeric("param_th", value=photfun_client.daophot_opt['th'])
        ui.update_numeric("param_sum", value=photfun_client.find_sum)
        ui.update_numeric("param_average", value=photfun_client.find_average)

    @reactive.Effect
    @reactive.event(input.param_fw)
    def _():
        photfun_client.daophot_opt['fw'] = float(input.param_fw())
        
    @reactive.Effect
    @reactive.event(input.param_th)
    def _():
        photfun_client.daophot_opt['th'] = float(input.param_th())

    @reactive.Effect
    @reactive.event(input.param_sum)
    def _():
        photfun_client.find_sum = int(input.param_sum())
        
    @reactive.Effect
    @reactive.event(input.param_average)
    def _():
        photfun_client.find_average = int(input.param_average())        

    # Ejecutar Find al presionar el botón
    @reactive.Effect
    @reactive.event(input.find_btn)
    def find_action():
        fits_obj = selected_fits()
        if fits_obj:
            try:
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Find")
                    out_table_obj = photfun_client.find(fits_obj.id, pbar=pbar)
                ui.notification_show(
                    f"Found {out_table_obj.df(0).shape[0]} sources"
                    f" -> [{out_table_obj.id}] {out_table_obj.alias}"
                )
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: FITS not selected.", type="warning")
        
        nav_table_sideview_update(fits=False, psf=False)
        update_select()

    return
