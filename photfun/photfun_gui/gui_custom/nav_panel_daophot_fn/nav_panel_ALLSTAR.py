from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_ALLSTAR_ui():
    return ui.page_fillable(
        ui.input_select("fits_select", "Select FITS", choices={}, width="auto"),  # Lista FITS
        ui.input_select("table_psf_select", "Select PSF model", choices={}, width="auto"),  # Lista Tables
        ui.input_select("table_select", "Select Targets", choices={}, width="auto"),  # Lista Tables
        ui.input_action_button("allstar_btn", "ALLSTAR", icon=icon_svg("sun"), width="auto"),  # Botón compacto
    )

@module.server
def nav_panel_ALLSTAR_server(input, output, session, daofun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in daofun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices)
        table_psf_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in daofun_client.psf_files}
        ui.update_select("table_psf_select", choices=table_psf_choices)
        table_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in daofun_client.tables}
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
        if input_tabs_daophot()=="ALLSTAR":
            update_select()

    # Obtener el FITS seleccionado
    @reactive.Calc
    def selected_fits():
        selected_id = input.fits_select()
        return next((f for f in daofun_client.fits_files if str(f.id) == selected_id), None)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_psf_table():
        selected_id = input.table_psf_select()
        return next((t for t in daofun_client.psf_files if str(t.id) == selected_id), None)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_table():
        selected_id = input.table_select()
        return next((t for t in daofun_client.tables if str(t.id) == selected_id), None)

    # Ejecutar ALLSTAR al presionar el botón
    @reactive.Effect
    @reactive.event(input.allstar_btn)
    def allstar_action():
        fits_obj = selected_fits()
        psf_obj = selected_psf_table()
        table_obj = selected_table()
        if fits_obj and psf_obj and table_obj:
            try:
                out_table_obj, out_fits_obj = daofun_client.allstar(fits_obj.id, psf_obj.id, table_obj.id)
                ui.notification_show(f"ALLSTAR PSF photometry complete\n -> [{out_table_obj.id}] {out_table_obj.alias}\n (Substracted: [{out_fits_obj.id}] {out_fits_obj.alias})")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: FITS not selected.", type="warning")
        
        nav_table_sideview_update()
        update_select()

    return
