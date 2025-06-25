from tqdm import tqdm
from ....misc_tools import daophot_pbar
from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones
import numpy as np


@module.ui
def nav_panel_DAOMASTER_ui():
    return ui.page_fillable(
        ui.layout_columns(
            # Columna izquierda - Controles principales
            ui.div(
                ui.input_select("master_select","Master Table", choices={}, width="auto"),
                ui.input_select("table_mch_select",   "Match Table", choices={}, width="auto"),
                ui.input_select("table_select",  "Frame list", choices={}, width="auto"),
                ui.input_action_button("dm_btn", "Run DAOMASTER", icon=icon_svg("plug"), width="auto"),
                style="padding-right:20px; border-right:1px solid #ddd"
            ),
            # Columna parámetros DAOMASTER
            ui.div(
                ui.input_numeric("param_minfram", "Min frames",       value=1, step=1),
                ui.input_numeric("param_minfrac", "Min fraction",       value=0, step=1),
                ui.input_numeric("param_maxfram", "Enough frames",       value=1, step=1),
                ui.input_numeric("param_maxsig", "Max sig match",       value=99, step=1),
                ui.input_numeric("param_deg",    "Deg freedom",       value=4,  step=1),
                ui.input_numeric("param_rad",    "Crit radius px",    value=6,  step=0.1),
                # Checkboxes en dos columnas
                ui.layout_columns(
                    ui.input_checkbox("chk_new_id", "New IDs",     value=False),
                    ui.input_checkbox("chk_out_mag", "Save .mag",  value=True),
                    ui.input_checkbox("chk_out_cor", "Save .cor",  value=False),
                    ui.input_checkbox("chk_out_raw", "Save .raw",  value=False),
                    ui.input_checkbox("chk_out_mch", "Save .mch",  value=False),
                    ui.input_checkbox("chk_out_tfr", "Save .tfr",  value=False),
                    ui.input_checkbox("chk_out_coo", "Save .coo",  value=False),
                    ui.input_checkbox("chk_out_mtr", "Save .mtr",  value=False),
                    col_widths=(6,6)
                ),
                style="padding-left:20px;"
            ),
            col_widths=(7, 5)
        ),
    )

@module.server
def nav_panel_DAOMASTER_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot):

    def update_select():
        master_choices = {"":"No Master"}
        master_choices.update({str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables})
        prev_selected_master = str(selected_master().id) if selected_master() else None
        ui.update_select("master_select", choices=master_choices, selected=prev_selected_master)
        table_mch_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables 
                                                                        if obj.file_type in {".mch"}}
        prev_selected_mch_table = str(selected_mch_table().id) if selected_mch_table() else None                             
        ui.update_select("table_mch_select", choices=table_mch_choices, selected=prev_selected_mch_table)
        table_als_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        prev_selected_als_table = str(selected_als_table().id) if selected_als_table() else None
        ui.update_select("table_select", choices=table_als_choices, selected=prev_selected_als_table)

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
        if input_tabs_daophot()=="DAOMASTER":
            update_select()
            update_settings()

    # Obtener objetos seleccionados
    @reactive.Calc
    def selected_mch_table():
        sid = input.table_mch_select()
        return next((t for t in photfun_client.tables if str(t.id)==sid), None)
    @reactive.Calc
    def selected_master():
        sid = input.master_select()
        return next((t for t in photfun_client.tables if str(t.id)==sid), None)
    @reactive.Calc
    def selected_als_table():
        sid = input.table_select()
        return next((t for t in photfun_client.tables if str(t.id)==sid), None)

    # Modificamos los opt files
    def update_settings():
        # Sincronizar valores numéricos
        ui.update_numeric("param_minfram", value=photfun_client.minimum_frames)
        ui.update_numeric("param_minfrac", value=photfun_client.minimum_fraction)
        ui.update_numeric("param_maxfram", value=photfun_client.enough_frames)
        ui.update_numeric("param_maxsig",  value=photfun_client.max_sig)
        ui.update_numeric("param_deg",     value=photfun_client.degrees_freedom)
        ui.update_numeric("param_rad",     value=photfun_client.critical_radius)

        # Sincronizar checkboxes DAOMASTER
        ui.update_checkbox("chk_new_id",  value=photfun_client.new_id)
        ui.update_checkbox("chk_out_mag", value=photfun_client.out_mag)
        ui.update_checkbox("chk_out_cor", value=photfun_client.out_cor)
        ui.update_checkbox("chk_out_raw", value=photfun_client.out_raw)
        ui.update_checkbox("chk_out_mch", value=photfun_client.out_mch)
        ui.update_checkbox("chk_out_tfr", value=photfun_client.out_tfr)
        ui.update_checkbox("chk_out_coo", value=photfun_client.out_coo)
        ui.update_checkbox("chk_out_mtr", value=photfun_client.out_mtr)

    # Reaccionar a cambios del usuario y actualizar atributos del cliente
    @reactive.Effect
    @reactive.event(input.param_minfram)
    def _():
        photfun_client.minimum_frames = int(input.param_minfram())

    @reactive.Effect
    @reactive.event(input.param_minfrac)
    def _():
        photfun_client.minimum_fraction = float(input.param_minfrac())

    @reactive.Effect
    @reactive.event(input.param_maxfram)
    def _():
        photfun_client.enough_frames = int(input.param_maxfram())

    @reactive.Effect
    @reactive.event(input.param_maxsig)
    def _():
        photfun_client.max_sig = float(input.param_maxsig())

    @reactive.Effect
    @reactive.event(input.param_deg)
    def _():
        photfun_client.degrees_freedom = int(input.param_deg())

    @reactive.Effect
    @reactive.event(input.param_rad)
    def _():
        photfun_client.critical_radius = float(input.param_rad())

    # Reaccionar a cambios de cada checkbox
    @reactive.Effect
    @reactive.event(input.chk_new_id)
    def _():
        photfun_client.new_id = bool(input.chk_new_id())

    @reactive.Effect
    @reactive.event(input.chk_out_mag)
    def _():
        photfun_client.out_mag = bool(input.chk_out_mag())

    @reactive.Effect
    @reactive.event(input.chk_out_cor)
    def _():
        photfun_client.out_cor = bool(input.chk_out_cor())

    @reactive.Effect
    @reactive.event(input.chk_out_raw)
    def _():
        photfun_client.out_raw = bool(input.chk_out_raw())

    @reactive.Effect
    @reactive.event(input.chk_out_mch)
    def _():
        photfun_client.out_mch = bool(input.chk_out_mch())

    @reactive.Effect
    @reactive.event(input.chk_out_tfr)
    def _():
        photfun_client.out_tfr = bool(input.chk_out_tfr())

    @reactive.Effect
    @reactive.event(input.chk_out_coo)
    def _():
        photfun_client.out_coo = bool(input.chk_out_coo())

    @reactive.Effect
    @reactive.event(input.chk_out_mtr)
    def _():
        photfun_client.out_mtr = bool(input.chk_out_mtr())

    # Ejecutar DAOMASTER al presionar el botón
    @reactive.Effect
    @reactive.event(input.dm_btn)
    def daomaster_action():
        mch_obj    = selected_mch_table()
        master_obj = selected_master()
        als_obj    = selected_als_table()
        if mch_obj and als_obj:
            try:
                ui.notification_show(
                    ui.HTML("Running DAOMASTER..."),
                    duration=None,
                    close_button=False,
                    id="noti_DAOMASTER"
                )
                # Ejecuta usando los atributos de photfun_client ya sincronizados
                out_tables = photfun_client.daomaster(
                    master_id     = master_obj.id if master_obj else None,
                    mch_id        = mch_obj.id,
                    id_table_list = als_obj.id
                )
                ui.notification_remove("noti_DAOMASTER")
                # Mostrar notificación con cada salida
                for t in out_tables:
                    ui.notification_show(f"DAOMASTER → [{t.id}] {t.alias}")
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: Master/.mch/.als not all selected.", type="warning")

        # Refrescar vista y selects
        nav_table_sideview_update(fits=False, psf=False)
        update_select()

    return {}