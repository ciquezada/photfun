from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_CREATE_MASTER_ui():
    return ui.page_fillable(
        ui.div(
            ui.input_select("table_master_select", "Select Master list", choices={}, width="auto"),  # Lista Tables
            ui.input_select("table_mch_select", "Select match transform", choices={}, width="auto"),  # Lista Tables
            ui.input_action_button("master_btn", "CREATE MASTER", icon=icon_svg("folder-tree"), width="auto"),  # Botón compacto
        ),
    )

@module.server
def nav_panel_CREATE_MASTER_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot):

    def update_select():
        table_master_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" 
                            for obj in photfun_client.tables 
                            if obj.file_type not in {".mch"}
                            }
        ui.update_select("table_master_select", choices=table_master_choices)
        table_mch_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" 
                            for obj in photfun_client.tables 
                            if obj.file_type in {".mch"}
                            }
        ui.update_select("table_mch_select", choices=table_mch_choices)

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
        if input_tabs_daophot()=="CREATE_MASTER":
            update_select()


    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_master_table():
        selected_id = input.table_master_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)
    
    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_mch_table():
        selected_id = input.table_mch_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

    # Ejecutar CREATE MASTER al presionar el botón
    @reactive.Effect
    @reactive.event(input.master_btn)
    def create_master_action():
        master_obj = selected_master_table()
        mch_obj = selected_mch_table()
        if master_obj and mch_obj:
            try:
                out_table_list = photfun_client.create_master(master_obj.id, mch_obj.id)
                ui.notification_show(f"Masterlist transformation created\n -> [{out_table_list.id}] {out_table_list.alias}")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: TABLE not selected.", type="warning")
        
        nav_table_sideview_update(fits=False, psf=False)
        update_select()

    return
