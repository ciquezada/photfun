from ...misc_tools import daophot_pbar
import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import pandas as pd
from . import input_local_file_ui, input_local_file_server
import tempfile
from astropy.io.votable import from_table, writeto
from astropy.table import Table
from .plot_preview_tools import psf_preview, generate_psf_profile, psf_and_profile


@module.ui
def nav_panel_PSF_ui():
    return ui.page_fluid(
        ui.layout_column_wrap(
            input_local_file_ui("load_local_psf", "Load PSF"),
            # ui.input_action_button("broadcast_table", "Send PSF", 
            #                        icon=icon_svg("tower-cell")),
        ),
        ui.layout_column_wrap(
            [
                ui.h4("PSF Preview"),
                ui.output_ui("plot_psf_and_profile"),
                # ui.h4("PSF Radial Profile"),
                # ui.output_ui("plot_psf_profile"),
            ],
            [
                ui.input_select("select_psf_file", ui.h4("PSF Contents"), choices=[], size=10, multiple=True),
                ui.input_action_button("show_info", "PSF Info", icon=icon_svg("circle-info")),
            ],
        ),
        # Sección de Plots
        # ui.layout_column_wrap(
        #     ui.input_switch("show_plots", "Show Allstar Metrics", value=False),
        #     # ui.input_action_button("show_plots", "Show Plots", icon=icon_svg("magnifying-glass-chart")),
        # ),
        # ui.output_ui("plots_ui"),
    )

@module.server
def nav_panel_PSF_server(input, output, session, photfun_client,
                           nav_table_sideview_update, psf_df, input_tabs_main):
    event_load_local_psf, input_load_local_psf = input_local_file_server(
        "load_local_psf", [".psf"]
    )

    @reactive.Effect
    @reactive.event(input_tabs_main)
    def _():
        if input_tabs_main()=="PSF":
            update_psf_selection()

    # Evento al cargar archivos de tabla
    @reactive.Effect
    @reactive.event(event_load_local_psf)
    def _():
        archivos = input_load_local_psf()
        if not archivos:
            return
        print("PhotFun: Load")
        carpetas = [f for f in archivos if os.path.isdir(f)]
        archivos_table = [f for f in archivos if os.path.isfile(f)]
        for carpeta in carpetas:
            photfun_client.add_psf(carpeta)
        if len(archivos_table) > 1:
            photfun_client.add_psf(archivos_table)
        elif len(archivos_table) == 1:
            photfun_client.add_psf(archivos_table[0])
        nav_table_sideview_update(fits=False, tables=False)


    # Obtener la tabla seleccionada en la lista general
    @reactive.Calc
    def selected_psf():
        selected_row = psf_df.data_view(selected=True)
        if selected_row.empty:
            return None  # No hay selección
        selected_id = selected_row.iloc[0]["PSF"]
        psf_obj = next((f for f in photfun_client.psf_files if f.id == selected_id), None)
        return psf_obj if psf_obj else None

    @reactive.Effect
    @reactive.event(selected_psf)
    def _():
        update_psf_selection()

    # Actualizar opciones del input select cuando cambia la tabla seleccionada
    def update_psf_selection():
        psf_obj = selected_psf()
        if not psf_obj or not psf_obj.path:
            ui.update_select("select_psf_file", choices={})  # Limpiar si no hay archivos
            return
        
        choices = {i:os.path.basename(p) for i, p in enumerate(psf_obj.path) if not os.path.basename(p).startswith("ERROR.")}
        ui.update_select("select_psf_file", choices=choices, selected=0)

    @reactive.Effect
    @reactive.event(input.show_info)
    def show_file_info():
        # Modal para mostrar la información del File
        ui_file_info =  ui.modal(
                            ui.output_text_verbatim("file_info"),
                            title="PSF Info",
                            id="file_info_modal",
                            easy_close=True,
                            size="l",
                        ),
        ui.modal_show(ui_file_info)

    @render.text
    def file_info():
        psf_obj = selected_psf()
        if not psf_obj:
            return "No file selected."
        
        selected_index = next(iter(input.select_psf_file()), None)
        if selected_index is None or selected_index == "":
            return "Select a file from the list above."
        
        selected_index = int(selected_index)
        info = psf_obj.file_info(selected_index)

        # Generar texto formateado
        info_text = "\n".join([f"{key}: {value}" for key, value in info.items()])
        return info_text

    psf_preview_counter = reactive.Value(0)
    @render.ui
    # UI function to plot PSF and profile or show a button to generate preview
    def plot_psf_and_profile():
        _ = psf_preview_counter()

        psf_obj = selected_psf()
        if not psf_obj or not psf_obj.path:
            return ui.HTML("<em>No PSF loaded</em>")

        idx_str = next(iter(input.select_psf_file()), None)
        if idx_str is None or idx_str == "":
            return ui.HTML("<em>Select a PSF file</em>")

        idx = int(idx_str)
        # preview_plot returns a base64-encoded gif or None
        img_b64 = psf_obj.preview_plot(idx)

        if img_b64:
            # Display the preview image if available
            return ui.img(src=f"data:image/png;base64,{img_b64}", width="100%")
        else:
            # Show button to generate PSF preview
            return ui.div(
                ui.HTML("<em>No PSF preview available</em>"),
                ui.input_action_button("generate_psf_preview_btn", "Generate PSF Preview", class_="btn-primary")
            )

    # Effect to handle PSF preview generation
    @reactive.Effect
    @reactive.event(input.generate_psf_preview_btn)
    def generate_psf_preview_action():
        psf_obj = selected_psf()
        if not psf_obj:
            ui.notification_show("Error: No PSF selected.", type="warning")
            return
        try:
            with ui.Progress(min=0, max=1) as p:
                pbar = daophot_pbar(p, "PSF Preview")
                # Call the photfun client to generate PSF preview
                photfun_client.psf_preview(psf_obj.id, pbar=pbar)
            ui.notification_show("PSF preview generated successfully.")
            psf_preview_counter.set(psf_preview_counter() + 1)  # <- Forzar re-render
        except Exception as e:
            ui.notification_show(f"Error generating PSF preview: {e}", type="error")

    # # ** Preview de la PSF como GIF **
    # @render.ui
    # def plot_psf_preview():
    #     psf_obj = selected_psf()
    #     if not psf_obj or not psf_obj.path:
    #         return ui.HTML("<em>No PSF loaded</em>")
    #     idx_str = next(iter(input.select_psf_file()), None)
    #     if idx_str is None or idx_str == "":
    #         return ui.HTML("<em>Select a PSF file</em>")
    #     idx = int(idx_str)
    #     # psf_obj.model(idx) debe devolver la matriz de corrección
    #     psf_data = psf_obj.model(idx)
    #     # psf_preview devuelve un base64 GIF
    #     img_b64 = psf_preview(psf_data, photfun_client.n_jobs)
    #     return ui.img(src=f"data:image/gif;base64,{img_b64}", width="100%")

    # # ------------------------------------
    # # ** Perfil radial de la PSF como GIF **
    # @render.ui
    # def plot_psf_profile():
    #     psf_obj = selected_psf()
    #     if not psf_obj or not psf_obj.path:
    #         return ui.HTML("<em>No PSF loaded</em>")
    #     idx_str = next(iter(input.select_psf_file()), None)
    #     if idx_str is None or idx_str == "":
    #         return ui.HTML("<em>Select a PSF file</em>")
    #     idx = int(idx_str)
    #     psf_data = psf_obj.model(idx)
    #     # generate_psf_profile devuelve un base64 GIF
    #     img_b64 = generate_psf_profile(psf_data, photfun_client.n_jobs)
    #     return ui.img(src=f"data:image/gif;base64,{img_b64}", width="100%")

    return
