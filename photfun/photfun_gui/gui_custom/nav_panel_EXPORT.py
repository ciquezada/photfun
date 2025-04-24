# photfun/photfun_gui/gui_export.py
from shiny import module, reactive, render, ui
from faicons import icon_svg
import tempfile
import os
from . import output_save_location_ui, output_save_location_server

@module.ui
def nav_panel_EXPORT_ui():
    return ui.page_fluid(
        ui.layout_column_wrap(
            ui.input_select("export_type", "Data type", 
                           {"fits": "FITS", "tables": "Tables", "psf": "PSFs"}),
            ui.input_select("export_object", "Select table", choices={}),
            output_save_location_ui("btn_export", "Export file"),
            # ui.download_button("btn_export", "Exportar Selección", 
            #                   icon=icon_svg("download"), width="100%")
        ),
        ui.div(
            ui.output_text_verbatim("export_status"),
            style=("height: 30vh; min-height: 150px; max-height: 400px;"
                  "overflow-y: auto;"
                  "border: 1px solid #dee2e6;"
                  "border-radius: 5px;"
                  "padding: 15px;"
                  "margin-top: 20px;"
                  "background-color: #f8f9fa;")
        )
    )

@module.server
def nav_panel_EXPORT_server(input, output, session, photfun_client, input_tabs_main):
    event_save_location, input_save_location  = output_save_location_server("btn_export")

    @reactive.Effect
    def _():
        match input.export_type():
            case "fits":
                objects = photfun_client.fits_files
            case "tables":
                objects = photfun_client.tables
            case "psf":
                objects = photfun_client.psf_files
            case _:
                objects = []
        
        choices = {obj.id: f"[{obj.id}] {obj.alias}" for obj in objects}
        ui.update_select("export_object", choices=choices)

    # Evento al cargar archivos FITS
    @reactive.Effect
    @reactive.event(event_save_location)
    def _():
        out_path = input_save_location()
        obj_id = int(input.export_object())
        photfun_client.export_file(obj_id, out_path)
        

    @reactive.Calc
    def photfun_client_export_filename():
        obj_id = int(input.export_object())
        obj_type = input.export_type()
        
        match obj_type:
            case "fits":
                obj = next((f for f in photfun_client.fits_files if f.id == obj_id), None)
            case "tables":
                obj = next((t for t in photfun_client.tables if t.id == obj_id), None)
            case "psf":
                obj = next((p for p in photfun_client.psf_files if p.id == obj_id), None)
        
        return [obj_path for obj_path in obj.path 
                    if not os.path.basename(obj_path).startswith("ERROR.")] if obj else "export"

    @output
    @render.text
    def export_status():
        if not input.export_object():
            return "Seleccione un objeto para exportar"
        paths_names = '\n\t'.join([os.path.basename(obj_path) 
                        for obj_path in photfun_client_export_filename()])
        return f"Preparando exportación:\n\t{paths_names}"

    # Cargar opciones de FITS en el select_input
    @reactive.Effect
    @reactive.event(input_tabs_main)
    def updatechoices():
        if input_tabs_main()=="EXPORT":
            match input.export_type():
                case "fits":
                    objects = photfun_client.fits_files
                case "tables":
                    objects = photfun_client.tables
                case "psf":
                    objects = photfun_client.psf_files
                case _:
                    objects = []
            
            choices = {obj.id: f"[{obj.id}] {obj.alias}" for obj in objects}
            ui.update_select("export_object", choices=choices)