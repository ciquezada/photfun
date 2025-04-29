# main.py (antes de todo lo demás)
import sys, asyncio
if sys.platform.startswith("win"):
    asyncio.set_event_loop_policy(
        asyncio.WindowsSelectorEventLoopPolicy()
    )

from pathlib import Path
from shiny import App, ui
from .server import server
from .gui_custom import (nav_table_sideview_ui, nav_panel_IMAGE_ui, 
                                            nav_panel_TABLE_ui, nav_panel_DAOPHOT_ui, 
                                            nav_panel_SELECTION_ui, nav_panel_PHOTCUBE_ui,
                                            nav_panel_EXPORT_ui, nav_panel_LOGS_ui)
import socket


def get_local_ipv4():
    # Crea un socket UDP temporal
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as s:
        try:
            # No necesita estar disponible, solo para resolver la IP local usada
            s.connect(("8.8.8.8", 80))
            ip = s.getsockname()[0]
        except Exception:
            ip = "127.0.0.1"  # fallback si falla
    return ip

app_dir = Path(__file__).parent

app_ui = ui.page_fillable(
    ui.tags.head(
        ui.tags.style("""
        	:root {
    			--bslib-sidebar-main-bg: #f8f8f8;
  			}
  
            .main-header {
                background: linear-gradient(rgba(0, 0, 0, 0.7), rgba(0, 0, 0, 0.7)),
                            url('https://images.unsplash.com/photo-1465101162946-4377e57745c3?auto=format&fit=crop&w=1920&q=80');
                background-size: cover;
                background-position: center 30%;
                padding: 2rem;
                border-bottom: 1px solid rgba(255, 255, 255, 0.1);
            }
            
            .glass-panel {
                background: rgba(255, 255, 255, 0.9) !important;
                backdrop-filter: blur(10px);
                border-radius: 8px !important;
                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
            }
            
            .sidebar-light {
                background-color: #ffffff !important;
                border-right: 1px solid #e0e0e0 !important;
                padding: 1rem;
            }
            
            .dataframe-container {
                background: white !important;
                color: #333 !important;
            }
        """)
    ),
    
    ui.div(
        ui.card(
            ui.card_header(
                ui.h1("PHOTFUN", class_="display-4 mb-0 text-light"),
                ui.h4("Astronomical Photometry Suite", class_="text-secondary mt-1 text-light"),
                class_="main-header"
            ),
            ui.page_sidebar(
                nav_table_sideview_ui("nav_table_sideview"),
                ui.div(
                    ui.navset_card_pill(
                        ui.nav_menu(
                            "Files",
                            ui.nav_panel("Image", nav_panel_IMAGE_ui("nav_panel_IMAGE"), value="IMAGE"),
                            ui.nav_panel("Table", nav_panel_TABLE_ui("nav_panel_TABLE"), value="TABLE"),
                        ),            
                        ui.nav_panel("DAOphot", nav_panel_DAOPHOT_ui("nav_panel_DAOPHOT"), value="DAOPHOT"),
                        ui.nav_panel("Target Selection", nav_panel_SELECTION_ui("nav_panel_SELECTION"), value="SELECTION"),
                        ui.nav_panel("PHOTcube", nav_panel_PHOTCUBE_ui("nav_panel_PHOTCUBE"), value="PHOTCUBE"),
                        ui.nav_panel("Export", nav_panel_EXPORT_ui("nav_panel_EXPORT"), value="EXPORT"),
                        ui.nav_panel("Logs", nav_panel_LOGS_ui("nav_panel_LOGS"), value="LOGS"),
                        id="tabs_main",
                    ),
                    class_="glass-panel"
                ),
                class_="mt-3",
                width="100%"
            ),
            class_="glass-panel border-0"
        ),
        style="padding: 2rem; width: 100%; height: 60vh;"
    ),
    fillable=True,
    fillable_mobile=True,
    fullscreen=True,
)
app = App(app_ui, server)

def run_photfun():
    # print("INFO:     Hosted in http://localhost:8000 (ignore http://0.0.0.0:8000)")
    app.run(host=get_local_ipv4(), port=8000)

if __name__ == "__main__":
    app.run()