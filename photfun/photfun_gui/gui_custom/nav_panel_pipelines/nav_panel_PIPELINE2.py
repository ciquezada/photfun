from ....misc_tools import daophot_pbar
from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones

PIPELINE2_description = '''
Preliminary PSF:
    intended to prepare a list for a maunal selection

Steps:
    1. find(fits, table)
       Inputs:  FITS image and initial source list
       Output:  Detected source positions list

    2. phot(fits, table)
       Inputs:  FITS image and detected positions
       Output:  Photometry table with measured fluxes

    3. pick(fits, photometry)
       Inputs:  FITS image and photometry table
       Output:  Refined source list with precise coordinates

    4. psf(fits, aperture_list, coordinate_list)
       Inputs:  FITS image, aperture list (from photometry) 
                    and coordinate list (from pick)
       Output:  PSF model describing the point-spread function

    5. sub(fits, psf_model, aperture_list, coordinate_list)
       Inputs:  FITS image, PSF model, aperture list (from photometry) 
                            and coordinate list (from pick)
       Output:  Image with neighboring sources subtracted

'''

@module.ui
def nav_panel_PIPELINE2_ui():
    return ui.page_fillable(
        ui.h4("Standard PSF Photometry Pipeline", class_="d-inline"),
        ui.layout_columns(
            # Columna izquierda - Controles principales
            ui.div(
                ui.input_select("fits_select", "Find targets on FITS", choices={}, width="auto", selected=None),
                ui.input_action_button("find_btn", "Run Pipeline", icon=icon_svg("magnifying-glass"), width="auto"),
                style="padding-right: 20px; border-right: 1px solid #dee2e6;"
            ),
            # Columna derecha - Parámetros DAOPHOT
            ui.div(
                ui.tooltip(
                    ui.input_numeric("param_fw", "FWHM (pixels)", value=3, step=0.1),
                    "Full Width at Half Maximum of stars in pixels"
                ),
                ui.tooltip(
                    ui.input_numeric("param_th", "Threshold (σ)", value=5, step=0.1),
                    "Sigma threshold for a detection"
                ),
                ui.tooltip(
                    ui.input_numeric("param_sum", "Summed", 
                                   value=1,  # Valor por defecto de 'fi' en opt_daophot_dict
                                   step=1
                                   ),
                    "Sum of several individual exposures, how many?"
                ),
                ui.tooltip(
                    ui.input_numeric("param_average", "Averaged", 
                                   value=1,  # Valor por defecto de 'ps' en opt_daophot_dict
                                   step=1,
                                   ),
                    "Average of several individual exposures, how many?"
                ),
                style="padding-left: 20px;"
            ),
            col_widths=(8, 4)
        )
    )

@module.server
def nav_panel_PIPELINE2_server(input, output, session, photfun_client, nav_table_sideview_update, input_tabs_main, input_tabs_daophot, navselected_fits):

    def update_select():
        fits_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.fits_files}
        ui.update_select("fits_select", choices=fits_choices, selected=str(navselected_fits().id) if navselected_fits() else None)

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
        if input_tabs_daophot()=="PIPELINE2":
            update_select()
            update_settings()

    # Obtener el FITS seleccionado
    @reactive.Calc
    def selected_fits():
        selected_id = input.fits_select()
        return next((f for f in photfun_client.fits_files if str(f.id) == selected_id), None)
    
    # Modificamos los opt files
    def update_settings():
        # Sincronizar valores iniciales
        ui.update_numeric("param_fw", value=photfun_client.daophot_opt['fw'])
        ui.update_numeric("param_th", value=photfun_client.daophot_opt['th'])
        ui.update_numeric("param_sum", value=photfun_client.find_sum)
        ui.update_numeric("param_average", value=photfun_client.find_average)

    @reactive.Effect
    @reactive.event(input.param_fw)
    def _():
        photfun_client.daophot_opt['fw'] = float(input.param_fw())
        
    @reactive.Effect
    @reactive.event(input.param_th)
    def _():
        photfun_client.daophot_opt['th'] = float(input.param_th())

    @reactive.Effect
    @reactive.event(input.param_sum)
    def _():
        photfun_client.find_sum = int(input.param_sum())
        
    @reactive.Effect
    @reactive.event(input.param_average)
    def _():
        photfun_client.find_average = int(input.param_average())        

    # Ejecutar Find al presionar el botón
    @reactive.Effect
    @reactive.event(input.find_btn)
    def pipeline_action():
        fits_obj = selected_fits()
        if fits_obj:
            try:
                # 1. Find
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Find")
                    find_table = photfun_client.find(fits_obj.id, pbar=pbar)
                ui.notification_show(
                    f"Found {find_table.df(0).shape[0]} sources"
                    f" -> [{find_table.id}] {find_table.alias}"
                )
                # 2. Phot
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Phot")
                    phot_table = photfun_client.phot(fits_obj.id, find_table.id, pbar=pbar)
                ui.notification_show(
                    f"Aperture photometry complete"
                    f" -> [{phot_table.id}] {phot_table.alias}"
                )
                # 3. Pick
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Pick")
                    pick_list = photfun_client.pick(fits_obj.id, phot_table.id, pbar=pbar)
                ui.notification_show(
                    f"Selected {pick_list.df(0).shape[0]} sources"
                    f" -> [{pick_list.id}] {pick_list.alias}"
                )
                # 4. PSF (first pass)
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
                # 5. Subtract
                with ui.Progress(min=0, max=1) as p:
                    pbar = daophot_pbar(p, "Subtract")
                    subtracted_fits = photfun_client.sub(
                        fits_obj.id, psf_model.id, phot_table.id, pick_list.id, pbar=pbar
                    )
                ui.notification_show(
                    f"Neighbors subtracted"
                    f" -> [{subtracted_fits.id}] {subtracted_fits.alias}"
                )
            except Exception as e:
                ui.notification_show(f"Error: {str(e)}", type="error")
        else:
            ui.notification_show("Error: FITS not selected.", type="warning")
        
        nav_table_sideview_update()
        update_select()

    return
