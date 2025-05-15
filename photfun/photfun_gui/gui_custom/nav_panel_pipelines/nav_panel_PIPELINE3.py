from ....misc_tools import daophot_pbar
from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

PIPELINE3_description = '''
Standard PSF Photometry Pipeline
    Including a Master and target list

Steps:
    1. phot(fits, table)
       Inputs:  FITS image and detected positions
       Output:  Photometry table with measured fluxes

    2. psf(fits, aperture_list, coordinate_list)
       Inputs:  FITS image, master list (from photometry) 
                    and coordinate list
       Output:  PSF model describing the point-spread function

    3. sub(fits, psf_model, aperture_list, coordinate_list)
       Inputs:  FITS image, PSF model, master list (from photometry) 
                            and coordinate list 
       Output:  Image with neighboring sources subtracted

    4. psf(fits, aperture_list, coordinate_list)
       Inputs:  Cleaned FITS image (from sub), master list (from photometry) 
                    and coordinate list 
       Output:  Refined PSF model after subtraction

    5. allstar(fits, psf_model, aperture_list)
       Inputs:  FITS image, refined PSF model and master list
       Output:  Final PSF-based photometry table with refined measurements
'''

@module.ui
def nav_panel_PIPELINE3_ui():
    return ui.page_fillable(
        ui.layout_columns(
            # Columna izquierda - Controles principales
            ui.div(
                ui.input_select("fits_select", "Photometry on FITS", choices={}, width="auto"),  # Lista FITS
                ui.input_select("table_select", "Select Targets", choices={}, width="auto"),  # Lista Tables
                ui.input_select("table_lst_select", "Select best targets list", choices={}, width="auto"),
                ui.input_action_button("phot_btn", "PHOT", icon=icon_svg("camera"), width="auto"),  # Botón compacto
                style="padding-right: 20px; border-right: 1px solid #dee2e6;"
            ),
            # Columna derecha - Parámetros ALLSTAR
            ui.div(
                ui.tooltip(
                    ui.input_numeric("param_fi", "Fitting Radius (px)", 
                                   value=4,  # Valor por defecto de 'fi' en opt_allstar_dict
                                   step=0.1,
                                   ),
                    "Fitting radius to fit the PSF model"
                ),
                ui.tooltip(
                    ui.input_numeric("param_is", "Inner Sky (px)", 
                                   value=5,  # Valor por defecto de 'is' en opt_allstar_dict
                                   step=0.5,
                                   ),
                    "Inner sky radius for the sky measure"
                ),
                ui.tooltip(
                    ui.input_numeric("param_os", "Outer Sky (px)", 
                                   value=25,  # Valor por defecto de 'is' en opt_allstar_dict
                                   step=0.5,
                                   ),
                    "Outer sky radius for the sky measure"
                ),
                ui.tooltip(
                    ui.input_switch("param_re", "Recentering", value=True),
                    "Automatic recentering on photometry"
                ),
                style="padding-left: 20px;"
            ),
            col_widths=(8, 4)
        )
    )

@module.server
def nav_panel_PIPELINE3_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot, navselected_fits):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices, selected=str(navselected_fits().id) if navselected_fits() else None)
        table_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("table_select", choices=table_choices)
        table_lst_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("table_lst_select", choices=table_lst_choices)

    # Cargar opciones de FITS en el select_input
    @reactive.Effect
    @reactive.event(input_tabs_main)
    def _():
        if input_tabs_main()=="PIPELINES":
            update_select()
            update_settings()
    
        # Cargar opciones de FITS en el select_input
    @reactive.Effect
    @reactive.event(input_tabs_daophot)
    def _():
        if input_tabs_daophot()=="PIPELINE3":
            update_select()
            update_settings()

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
    
    # Obtener el Table seleccionado
    @reactive.Calc
    def selected_lst_table():
        selected_id = input.table_lst_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

    # Modificamos los opt files
    def update_settings():
        # Sincronizar valores iniciales
        ui.update_numeric("param_fi", value=photfun_client.allstar_opt['fi'])
        ui.update_numeric("param_is", value=photfun_client.allstar_opt['is'])
        ui.update_numeric("param_os", value=photfun_client.allstar_opt['os'])
        ui.update_switch("param_re", value=photfun_client.allstar_recentering)

    @reactive.Effect
    @reactive.event(input.param_fi)
    def _():
        photfun_client.allstar_opt['fi'] = float(input.param_fi())
        
    @reactive.Effect
    @reactive.event(input.param_is)
    def _():
        photfun_client.allstar_opt['is'] = float(input.param_is())

    @reactive.Effect
    @reactive.event(input.param_os)
    def _():
        photfun_client.allstar_opt['os'] = float(input.param_os())

    @reactive.Effect
    @reactive.event(input.param_re)
    def _():
        photfun_client.allstar_recentering = input.param_re()
 

    # Ejecutar PHOT al presionar el botón
    @reactive.Effect
    @reactive.event(input.phot_btn)
    def phot_action():
        fits_obj = selected_fits()
        find_table = selected_table()
        pick_list = selected_lst_table()
        if fits_obj and find_table and pick_list:
            try:
                # 1. Phot
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Phot")
                    phot_table = photfun_client.phot(fits_obj.id, find_table.id, pbar=pbar)
                ui.notification_show(
                    f"Aperture photometry complete"
                    f" -> [{phot_table.id}] {phot_table.alias}"
                )
                # 2. PSF (first pass)
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "PSF")
                    psf_model, neighbor_list = photfun_client.psf(
                        fits_obj.id, phot_table.id, pick_list.id, pbar=pbar
                    )
                ui.notification_show(
                    f"PSF model created"
                    f" -> [{psf_model.id}] {psf_model.alias}"
                    f" (Neighbors: [{neighbor_list.id}] {neighbor_list.alias})"
                )
                # 3. Subtract
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Subtract")
                    subtracted_fits = photfun_client.sub(
                        fits_obj.id, psf_model.id, phot_table.id, pick_list.id, pbar=pbar
                    )
                ui.notification_show(
                    f"Neighbors subtracted"
                    f" -> [{subtracted_fits.id}] {subtracted_fits.alias}"
                )
                # 4. PSF (refined)
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "PSF (refined)")
                    refined_psf, _ = photfun_client.psf(
                        subtracted_fits.id, phot_table.id, pick_list.id, pbar=pbar
                    )
                ui.notification_show(
                    f"Refined PSF model created"
                    f" -> [{refined_psf.id}] {refined_psf.alias}"
                )
                # 5. Allstar
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Allstar")
                    allstar_table, allstar_subtracted = photfun_client.allstar(
                        fits_obj.id, refined_psf.id, phot_table.id, pbar=pbar
                    )
                ui.notification_show(
                    f"ALLSTAR complete"
                    f" -> [{allstar_table.id}] {allstar_table.alias}"
                    f" (Subtracted: [{allstar_subtracted.id}] {allstar_subtracted.alias})"
                )
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: FITS not selected.", type="warning")
        
        nav_table_sideview_update()
        update_select()


    return
