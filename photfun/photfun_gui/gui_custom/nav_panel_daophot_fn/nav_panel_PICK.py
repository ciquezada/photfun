from ....misc_tools import daophot_pbar
from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_PICK_ui():
    return ui.page_fillable(
        ui.layout_columns(
            # Columna izquierda - Controles principales
            ui.div(
                ui.input_select("fits_select", "Pick best targets on FITS", choices={}, width="auto"),
                ui.input_select("table_select", "Select Targets", choices={}, width="auto"),
                ui.input_action_button("pick_btn", "PICK", icon=icon_svg("ranking-star"), width="auto"),
                style="padding-right: 20px; border-right: 1px solid #dee2e6;"
            ),
            # Columna derecha - Parámetros DAOPHOT específicos de PICK
            ui.div(
                ui.tooltip(
                    ui.input_numeric("param_fi", "Fitting Radius (px)", 
                                   value=4,  # Valor por defecto de 'fi' en opt_daophot_dict
                                   step=0.1
                                   ),
                    "Same or less than FWHM if the field is crowded"
                ),
                ui.tooltip(
                    ui.input_numeric("param_ps", "PSF Radius (px)", 
                                   value=20,  # Valor por defecto de 'ps' en opt_daophot_dict
                                   step=0.1,
                                   ),
                    "3 or 4 times the FWHM"
                ),
                ui.tooltip(
                    ui.input_numeric("param_stars", "Max stars", 
                                   value=200,  # Valor por defecto de 'fi' en opt_daophot_dict
                                   step=1
                                   ),
                    "100 stars or more for a PSF modeling is correct"
                ),
                ui.tooltip(
                    ui.input_numeric("param_minmag", "Min magnitude", 
                                   value=20,  # Valor por defecto de 'ps' en opt_daophot_dict
                                   step=0.1,
                                   ),
                    "Often weak stars are worse"
                ),
                style="padding-left: 20px;"
            ),
            col_widths=(8, 4)
        )
    )

@module.server
def nav_panel_PICK_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot, navselected_fits):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices, selected=str(navselected_fits().id) if navselected_fits() else None)
        table_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("table_select", choices=table_choices)

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
        if input_tabs_daophot()=="PICK":
            update_select()
            update_settings()
            

    # Obtener el FITS seleccionado
    @reactive.Calc
    def selected_fits():
        selected_id = input.fits_select()
        return next((f for f in photfun_client.fits_files if str(f.id) == selected_id), None)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_table():
        selected_id = input.table_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

    # Modificamos los opt files
    def update_settings():
        # Sincronizar valores iniciales
        ui.update_numeric("param_fi", value=photfun_client.daophot_opt['fi'])
        ui.update_numeric("param_ps", value=photfun_client.daophot_opt['ps'])
        ui.update_numeric("param_stars", value=photfun_client.pick_max_stars)
        ui.update_numeric("param_minmag", value=photfun_client.pick_min_mag)

    @reactive.Effect
    @reactive.event(input.param_fi)
    def _():
        photfun_client.daophot_opt['fi'] = float(input.param_fi())
        
    @reactive.Effect
    @reactive.event(input.param_ps)
    def _():
        photfun_client.daophot_opt['ps'] = float(input.param_ps())

    @reactive.Effect
    @reactive.event(input.param_stars)
    def _():
        photfun_client.pick_max_stars = int(input.param_stars())
        
    @reactive.Effect
    @reactive.event(input.param_minmag)
    def _():
        photfun_client.pick_min_mag = int(input.param_minmag())

    # Ejecutar PICK al presionar el botón
    @reactive.Effect
    @reactive.event(input.pick_btn)
    def pick_action():
        fits_obj = selected_fits()
        table_obj = selected_table()
        if fits_obj and table_obj:
            try:
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Pick")
                    out_table_obj = photfun_client.pick(fits_obj.id, table_obj.id, pbar=pbar)
                ui.notification_show(f"Selected {out_table_obj.df(0).shape[0]} sources\n -> [{out_table_obj.id}] {out_table_obj.alias}")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: FITS not selected.", type="warning")
        
        nav_table_sideview_update(fits=False, psf=False)
        update_select()

    return

