from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_FIND_ui():
    return ui.page_fillable(
        ui.input_select("fits_select", "Find targets on FITS", choices={}, width="auto"),  # Lista desplegable de FITS
        ui.input_action_button("find_btn", "FIND", icon=icon_svg("magnifying-glass"), width="auto"),  # Botón compacto
    )

@module.server
def nav_panel_FIND_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices)

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
        if input_tabs_daophot()=="FIND":
            update_select()

    # Obtener el FITS seleccionado
    @reactive.Calc
    def selected_fits():
        selected_id = input.fits_select()
        return next((f for f in photfun_client.fits_files if str(f.id) == selected_id), None)

    # Ejecutar Find al presionar el botón
    @reactive.Effect
    @reactive.event(input.find_btn)
    def find_action():
        fits_obj = selected_fits()
        if fits_obj:
            photfun_client.find(fits_obj.id)
        nav_table_sideview_update()
        update_select()

    return
