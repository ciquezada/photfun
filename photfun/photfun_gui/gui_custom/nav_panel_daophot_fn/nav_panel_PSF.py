from ....misc_tools import daophot_pbar
from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_PSF_ui():
    return ui.page_fillable(
        ui.layout_columns(
            # Columna izquierda - Controles principales
            ui.div(
                ui.input_select("fits_select", "Model PSF on FITS", choices={}, width="auto"),
                ui.input_select("table_ap_select", "Select targets", choices={}, width="auto"),
                ui.input_select("table_lst_select", "Select best targets list", choices={}, width="auto"),
                ui.input_action_button("psf_btn", "PSF", icon=icon_svg("bullseye"), width="auto"),
                style="padding-right: 20px; border-right: 1px solid #dee2e6;"
            ),
            # Columna derecha - Parámetros DAOPHOT
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
                style="padding-left: 20px;"
            ),
            col_widths=(8, 4)
        )
    )

@module.server
def nav_panel_PSF_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot, navselected_fits):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices, selected=str(navselected_fits().id) if navselected_fits() else None)
        table_ap_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("table_ap_select", choices=table_ap_choices)
        table_lst_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("table_lst_select", choices=table_lst_choices)

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
        if input_tabs_daophot()=="PSF":
            update_select()
            update_settings()

    # Obtener el FITS seleccionado
    @reactive.Calc
    def selected_fits():
        selected_id = input.fits_select()
        return next((f for f in photfun_client.fits_files if str(f.id) == selected_id), None)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_ap_table():
        selected_id = input.table_ap_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)
    
    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_lst_table():
        selected_id = input.table_lst_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

    # Modificamos los opt files
    def update_settings():
        # Sincronizar valores iniciales
        ui.update_numeric("param_fi", value=photfun_client.daophot_opt['fi'])
        ui.update_numeric("param_ps", value=photfun_client.daophot_opt['ps'])

    @reactive.Effect
    @reactive.event(input.param_fi)
    def _():
        photfun_client.daophot_opt['fi'] = float(input.param_fi())
        
    @reactive.Effect
    @reactive.event(input.param_ps)
    def _():
        photfun_client.daophot_opt['ps'] = float(input.param_ps())

    # Ejecutar PSF al presionar el botón
    @reactive.Effect
    @reactive.event(input.psf_btn)
    def psf_action():
        fits_obj = selected_fits()
        ap_obj = selected_ap_table()
        lst_obj = selected_lst_table()
        if fits_obj and ap_obj and lst_obj:
            try:
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "PSF")
                    out_psf_obj, out_table_obj = photfun_client.psf(fits_obj.id, ap_obj.id, lst_obj.id, pbar=pbar)
                ui.notification_show(f"PSF created\n -> [{out_psf_obj.id}] {out_psf_obj.alias}\n (Neighbors: [{out_table_obj.id}] {out_table_obj.alias})")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: FITS not selected.", type="warning")
        
        nav_table_sideview_update(fits=False)
        update_select()

    return
