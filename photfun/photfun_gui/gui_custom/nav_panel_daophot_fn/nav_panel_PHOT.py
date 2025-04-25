from ....misc_tools import daophot_pbar
from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_PHOT_ui():
    return ui.page_fillable(
        ui.layout_columns(
            # Columna izquierda - Controles principales
            ui.div(
                ui.input_select("fits_select", "Photometry on FITS", choices={}, width="auto"),  # Lista FITS
                ui.input_select("table_select", "Select Targets", choices={}, width="auto"),  # Lista Tables
                ui.input_action_button("phot_btn", "PHOT", icon=icon_svg("camera"), width="auto"),  # Botón compacto
                style="padding-right: 20px; border-right: 1px solid #dee2e6;"
            ),
            # Columna derecha - Parámetros DAOPHOT
            ui.div(
                ui.tooltip(
                    ui.input_numeric("param_ap1", "Aperture 1 (px)", 
                                   value=3,  # Valor por defecto de 'fi' en opt_daophot_dict
                                   step=0.1
                                   ),
                    "Aperture radius 1"
                ),
                ui.tooltip(
                    ui.input_numeric("param_ap2", "Aperture 2 (px)", 
                                   value=6,  # Valor por defecto de 'ps' en opt_daophot_dict
                                   step=0.1,
                                   ),
                    "Aperture radius 2"
                ),
                ui.tooltip(
                    ui.input_numeric("param_ap3", "Aperture 3 (px)", 
                                   value=12,  # Valor por defecto de 'ps' en opt_daophot_dict
                                   step=0.1,
                                   ),
                    "Aperture radius 3"
                ),
                                ui.tooltip(
                    ui.input_numeric("param_is", "Inner Sky (px)", 
                                   value=12,  # Valor por defecto de 'ps' en opt_daophot_dict
                                   step=0.1,
                                   ),
                    "Inner Sky radius, larger than the bigger star"
                ),
                ui.tooltip(
                    ui.input_numeric("param_os", "Outer Sky (px)", 
                                   value=20,  # Valor por defecto de 'ps' en opt_daophot_dict
                                   step=0.1,
                                   ),
                    "Outer Sky radius, larger than the inner radius but less to be contaminated by neighbours"
                ),
                style="padding-left: 20px;"
            ),
            col_widths=(8, 4)
        )
    )

@module.server
def nav_panel_PHOT_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot, navselected_fits):

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
        if input_tabs_daophot()=="PHOT":
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
        ui.update_numeric("param_ap1", value=photfun_client.photo_opt['A1'])
        ui.update_numeric("param_ap2", value=photfun_client.photo_opt['A2'])
        ui.update_numeric("param_ap3", value=photfun_client.photo_opt['A3'])
        ui.update_numeric("param_is", value=photfun_client.photo_opt['IS'])
        ui.update_numeric("param_os", value=photfun_client.photo_opt['OS'])

    @reactive.Effect
    @reactive.event(input.param_ap1)
    def _():
        photfun_client.photo_opt['A1'] = float(input.param_ap1())
        
    @reactive.Effect
    @reactive.event(input.param_ap2)
    def _():
        photfun_client.photo_opt['A2'] = float(input.param_ap2())

    @reactive.Effect
    @reactive.event(input.param_ap3)
    def _():
        photfun_client.photo_opt['A3'] = float(input.param_ap3())
        
    @reactive.Effect
    @reactive.event(input.param_is)
    def _():
        photfun_client.photo_opt['IS'] = float(input.param_is())

    @reactive.Effect
    @reactive.event(input.param_os)
    def _():
        photfun_client.photo_opt['OS'] = float(input.param_os())
 

    # Ejecutar PHOT al presionar el botón
    @reactive.Effect
    @reactive.event(input.phot_btn)
    def phot_action():
        fits_obj = selected_fits()
        table_obj = selected_table()
        if fits_obj and table_obj:
            try:
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Phot")
                    out_table_obj = photfun_client.phot(fits_obj.id, table_obj.id, pbar=pbar)
                ui.notification_show(f"Aperture photometry\n -> [{out_table_obj.id}] {out_table_obj.alias}")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: FITS not selected.", type="warning")
        
        nav_table_sideview_update(fits=False, psf=False)
        update_select()

    return
