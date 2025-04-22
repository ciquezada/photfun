# photfun/photfun_gui/gui_logs.py
from shiny import module, reactive, render, ui
from faicons import icon_svg
import time

@module.ui
def nav_panel_LOGS_ui():
    return ui.page_fluid(
        ui.layout_column_wrap(
            ui.input_action_button("refresh_logs", "Refresh Logs", icon=icon_svg("arrows-rotate")),
            ui.output_ui("log_display", fillable=True)
        ),
        height="85vh"
    )

@module.server
def nav_panel_LOGS_server(input, output, session, photfun_client):

    @render.ui
    @reactive.event(input.refresh_logs)
    def log_display():
        logs = photfun_client.logs if hasattr(photfun_client, 'logs') else []
        
        log_entries = [
            ui.div(
                ui.div(
                    ui.div(f"[{entry[0]}]", style="color: #888; margin-right: 10px;"),
                    ui.div(entry[1]),
                    style="padding: 5px 10px; border-bottom: 1px solid #eee;"
                ),
                style="font-family: monospace; font-size: 0.9em;"
            ) 
            for entry in logs  # Mostrar últimos 200 logs
        ]
        
        return ui.div(
            *log_entries,
            style=("height: calc(100vh - 150px);"
                   "overflow-y: auto;"
                   "padding: 15px;"
                   "background-color: #f8f9fa;"
                   "border-radius: 5px;")
        )
    