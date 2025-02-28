from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_PICK_ui():
    return ui.page_fillable(
        ui.input_select("fits_select", "Pick best targets on FITS", choices={}, width="auto"),  # Lista FITS
        ui.input_select("table_select", "Select Targets", choices={}, width="auto"),  # Lista Tables
        ui.input_action_button("pick_btn", "PICK", icon=icon_svg("ranking-star"), width="auto"),  # Botón compacto
    )

@module.server
def nav_panel_PICK_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot):

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
        if input_tabs_daophot()=="PICK":
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

    # Ejecutar PICK al presionar el botón
    @reactive.Effect
    @reactive.event(input.pick_btn)
    def pick_action():
        fits_obj = selected_fits()
        table_obj = selected_table()
        if fits_obj and table_obj:
            try:
                out_table_obj = photfun_client.pick(fits_obj.id, table_obj.id)
                ui.notification_show(f"Selected {out_table_obj.df.shape[0]} sources\n -> [{out_table_obj.id}] {out_table_obj.alias}")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: FITS not selected.", type="warning")
        
        nav_table_sideview_update(fits=False, psf=False)
        update_select()

    return

