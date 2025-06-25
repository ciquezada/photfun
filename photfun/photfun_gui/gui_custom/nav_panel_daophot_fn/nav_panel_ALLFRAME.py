from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

@module.ui
def nav_panel_ALLFRAME_ui():
    return ui.page_fillable(
        ui.div(
            ui.input_select("fits_select", "FITS List",   choices={}, width="auto"),
            ui.input_select("psf_select",  "PSF List",    choices={}, width="auto"),
            ui.input_select("als_select",  "Frames List",    choices={}, width="auto"),
            ui.input_select("mch_select",  "MCH Table",   choices={}, width="auto"),
            ui.input_select("mag_select",  "Master Table",  choices={}, width="auto"),
            ui.input_action_button("af_btn", "Run ALLFRAME",
                                    icon=icon_svg("diagram-project"),
                                    width="auto"),
            style="padding-right:20px; border-right:1px solid #ddd"
        ),
    )

@module.server
def nav_panel_ALLFRAME_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        prev_selected_fits = str(selected_fits().id) if selected_fits() else None
        ui.update_select("fits_select", choices=fits_choices, selected=prev_selected_fits)
        table_psf_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.psf_files}
        prev_selected_psf = str(selected_psf().id) if selected_psf() else None
        ui.update_select("psf_select", choices=table_psf_choices, selected=prev_selected_psf)
        als_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        prev_selected_als = str(selected_als().id) if selected_als() else None
        ui.update_select("als_select", choices=als_choices, selected=prev_selected_als)
        mch_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables
                                                                    if obj.file_type in {".mch"}}
        prev_selected_mch = str(selected_mch().id) if selected_mch() else None
        ui.update_select("mch_select", choices=mch_choices, selected=prev_selected_mch)
        mag_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        prev_selected_mag = str(selected_mag().id) if selected_mag() else None
        ui.update_select("mag_select", choices=mag_choices, selected=prev_selected_mag)

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
        if input_tabs_daophot()=="ALLFRAME":
            update_select()


    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_fits():
        sid = input.fits_select()
        return next((f for f in photfun_client.fits_files if str(f.id) == sid), None)

    @reactive.Calc
    def selected_psf():
        sid = input.psf_select()
        return next((p for p in photfun_client.psf_files if str(p.id) == sid), None)

    @reactive.Calc
    def selected_als():
        sid = input.als_select()
        return next((t for t in photfun_client.tables if str(t.id) == sid), None)

    @reactive.Calc
    def selected_mch():
        sid = input.mch_select()
        return next((t for t in photfun_client.tables if str(t.id) == sid), None)

    @reactive.Calc
    def selected_mag():
        sid = input.mag_select()
        return next((t for t in photfun_client.tables if str(t.id) == sid), None)

    # Acción al presionar el botón ALLFRAME
    @reactive.Effect
    @reactive.event(input.af_btn)
    def allframe_action():
        fits_obj = selected_fits()
        psf_obj  = selected_psf()
        als_obj  = selected_als()
        mch_obj  = selected_mch()
        mag_obj  = selected_mag()

        if not all([fits_obj, psf_obj, als_obj, mch_obj, mag_obj]):
            ui.notification_show("Error: selecciona FITS, PSF, ALS, MCH y MAG", type="warning")
            return

        try:
            # Llamada al método de cliente
            ui.notification_show(
                ui.HTML("Running ALLFRAME…"),
                duration=None,
                close_button=False,
                id="noti_ALLFRAME"
            )
            out_alf, out_fits, out_tfr, out_nmg = photfun_client.allframe(
                fits_obj.id, psf_obj.id, als_obj.id, mch_obj.id, mag_obj.id
            )
            ui.notification_remove("noti_ALLFRAME")
            # Notificar cada salida
            ui.notification_show(f"ALLFRAME →  [{out_alf.id}] {out_alf.alias}")
            ui.notification_show(f"ALLFRAME →  [{out_fits.id}] {out_fits.alias}")
            ui.notification_show(f"ALLFRAME →  [{out_tfr.id}] {out_tfr.alias}")
            ui.notification_show(f"ALLFRAME →  [{out_nmg.id}] {out_nmg.alias}")
        except Exception as e:
            ui.notification_show(f"Error: {str(e)}", type="error")

        nav_table_sideview_update(psf=False)
        update_select()

    return {
        "selected_fits": selected_fits,
        "selected_psf":  selected_psf,
        "selected_als":  selected_als,
        "selected_mch":  selected_mch,
        "selected_table":  selected_mag,
    }

    return {}
