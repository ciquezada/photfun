from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_PSF_ui():
    return ui.page_fillable(
        ui.input_select("fits_select", "Model PSF on FITS", choices={}, width="auto"),  # Lista FITS
        ui.input_select("table_ap_select", "Select targets", choices={}, width="auto"),  # Lista Tables
        ui.input_select("table_lst_select", "Select best targets list", choices={}, width="auto"),  # Lista Tables
        ui.input_action_button("psf_btn", "PSF", icon=icon_svg("bullseye"), width="auto"),  # Botón compacto
    )

@module.server
def nav_panel_PSF_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices)
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
    
        # Cargar opciones de FITS en el select_input
    @reactive.Effect
    @reactive.event(input_tabs_daophot)
    def _():
        if input_tabs_daophot()=="PSF":
            update_select()

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

    # Ejecutar PSF al presionar el botón
    @reactive.Effect
    @reactive.event(input.psf_btn)
    def psf_action():
        fits_obj = selected_fits()
        ap_obj = selected_ap_table()
        lst_obj = selected_lst_table()
        if fits_obj and ap_obj and lst_obj:
            photfun_client.psf(fits_obj.id, ap_obj.id, lst_obj.id)
        nav_table_sideview_update()
        update_select()

    return
