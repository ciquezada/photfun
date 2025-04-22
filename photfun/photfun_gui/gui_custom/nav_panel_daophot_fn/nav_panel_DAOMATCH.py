from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_DAOMATCH_ui():
    return ui.page_fillable(
        ui.div(
            ui.input_select("table_master_select", "Select Master list", choices={}, width="auto"),  # Lista Tables
            ui.input_select("table_slave_select", "Select photometry sub targets", choices={}, width="auto"),  # Lista Tables
            ui.input_action_button("mch_btn", "DAOMATCH", icon=icon_svg("object-ungroup"), width="auto"),  # Botón compacto
        ),
    )

@module.server
def nav_panel_DAOMATCH_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot):

    def update_select():
        table_master_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" 
                            for obj in photfun_client.tables 
                            if obj.file_type in {".als", ".ap", ".alf"}
                            }
        ui.update_select("table_master_select", choices=table_master_choices)
        table_slave_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" 
                            for obj in photfun_client.tables 
                            if obj.file_type in {".als", ".ap", ".alf"}
                            }
        ui.update_select("table_slave_select", choices=table_slave_choices)

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
        if input_tabs_daophot()=="DAOMATCH":
            update_select()


    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_master_table():
        selected_id = input.table_master_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)
    
    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_slave_table():
        selected_id = input.table_slave_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

    # Ejecutar DAOMATCH al presionar el botón
    @reactive.Effect
    @reactive.event(input.mch_btn)
    def daomatch_action():
        master_obj = selected_master_table()
        slave_obj = selected_slave_table()
        if master_obj and slave_obj:
            try:
                out_mch_table = photfun_client.daomatch(master_obj.id, slave_obj.id)
                ui.notification_show(f"MATCH created\n -> [{out_mch_table.id}] {out_mch_table.alias}")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: TABLE not selected.", type="warning")
        
        nav_table_sideview_update(fits=False, psf=False)
        update_select()

    return
