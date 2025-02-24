import sys
sys.path.append("/data/ciquezada/Projects/py_photsuite")
### ERASE AFTER
import seaborn as sns
from faicons import icon_svg
from pathlib import Path
import os

# Import data from shared.py
from shiny import App, ui
from server import server
from photfun.photfun_gui.gui_custom import nav_table_sideview_ui
from photfun.photfun_gui.gui_custom import nav_panel_IMAGE_ui
from photfun.photfun_gui.gui_custom import nav_panel_TABLE_ui
from photfun.photfun_gui.gui_custom import nav_panel_DAOPHOT_ui

app_dir = Path(__file__).parent


app_ui = ui.page_fillable(
            ui.card(
                ui.card_header(
                    "PhotFun Shiny",
                    style="font-size: 2rem; text-align: left;"  # Aumenta el tamaño y centra el texto
                ),
                ui.page_sidebar(
                    nav_table_sideview_ui("nav_table_sideview"),
                    ui.navset_card_pill(
                        ui.nav_panel("IMAGE",   nav_panel_IMAGE_ui("nav_panel_IMAGE"), value="IMAGE"),
                        ui.nav_panel("TABLE",   nav_panel_TABLE_ui("nav_panel_TABLE"), value="TABLE"),            
                        ui.nav_panel("DAOPHOT", nav_panel_DAOPHOT_ui("nav_panel_DAOPHOT"), value="DAOPHOT"),
                        id="tabs_main",
                    ),
                ),
            ),
            ui.include_css(app_dir / "styles.css"),
            fillable=True,
            fillable_mobile=True,
            fullscreen=True,
        )
                


app = App(app_ui, server)

if __name__ == "__main__":
    app.run()
