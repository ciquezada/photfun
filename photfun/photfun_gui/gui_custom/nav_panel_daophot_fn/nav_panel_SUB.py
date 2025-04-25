from ....misc_tools import daophot_pbar
from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_SUB_ui():
    return ui.page_fillable(
        ui.div(
            ui.input_select("fits_select", "Substract targets on FITS", choices={}, width="auto"),  # Lista FITS
            ui.input_select("table_psf_select", "Select PSF model", choices={}, width="auto"),  # Lista Tables
            ui.input_select("table_nei_select", "Select target list", choices={}, width="auto"),  # Lista Tables
            ui.input_select("table_lst_select", "Select exception list", choices={}, width="auto"),  # Lista Tables
            ui.input_action_button("sub_btn", "SUBSTRACT", icon=icon_svg("square-minus"), width="auto"),  # Botón compacto
            style="padding-right: 20px; border-right: 1px solid #dee2e6;"
        ),
    )

@module.server
def nav_panel_SUB_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot, navselected_fits):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices, selected=str(navselected_fits().id) if navselected_fits() else None)
        table_psf_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.psf_files}
        ui.update_select("table_psf_select", choices=table_psf_choices)
        table_nei_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("table_nei_select", choices=table_nei_choices)
        table_lst_choices = {"":"No exception"}
        table_lst_choices.update({str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables})
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
        if input_tabs_daophot()=="SUB":
            update_select()

    # Obtener el FITS seleccionado
    @reactive.Calc
    def selected_fits():
        selected_id = input.fits_select()
        return next((f for f in photfun_client.fits_files if str(f.id) == selected_id), None)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_psf_table():
        selected_id = input.table_psf_select()
        return next((t for t in photfun_client.psf_files if str(t.id) == selected_id), None)

    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_nei_table():
        selected_id = input.table_nei_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

        # Obtener el Table seleccionado
    @reactive.Calc
    def selected_lst_table():
        selected_id = input.table_lst_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), False)

    # Ejecutar SUB al presionar el botón
    @reactive.Effect
    @reactive.event(input.sub_btn)
    def sub_action():
        fits_obj = selected_fits()
        psf_obj = selected_psf_table()
        nei_obj = selected_nei_table()
        lst_obj = selected_lst_table()
        if fits_obj and psf_obj and nei_obj:
            try:
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Subtract")
                    out_fits_obj = photfun_client.sub(fits_obj.id, psf_obj.id, nei_obj.id, 
                                            lst_obj.id if lst_obj else False, pbar=pbar)
                ui.notification_show(f"Substracted fits\n -> [{out_fits_obj.id}] {out_fits_obj.alias}")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: FITS not selected.", type="warning")
        
        nav_table_sideview_update(tables=False, psf=False)
        update_select()

    return
