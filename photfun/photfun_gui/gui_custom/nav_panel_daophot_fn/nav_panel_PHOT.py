from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_PHOT_ui():
    return ui.page_fillable(
        ui.input_select("fits_select", "Photometry on FITS", choices={}, width="auto"),  # Lista FITS
        ui.input_select("table_select", "Select Targets", choices={}, width="auto"),  # Lista Tables
        ui.input_action_button("phot_btn", "PHOT", icon=icon_svg("camera"), width="auto"),  # Botón compacto
    )

@module.server
def nav_panel_PHOT_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices)
        table_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("table_select", choices=table_choices)

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
        if input_tabs_daophot()=="PHOT":
            update_select()

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

    # Ejecutar PHOT al presionar el botón
    @reactive.Effect
    @reactive.event(input.phot_btn)
    def phot_action():
        fits_obj = selected_fits()
        table_obj = selected_table()
        if fits_obj and table_obj:
            photfun_client.phot(fits_obj.id, table_obj.id)
        nav_table_sideview_update()
        update_select()

    return
