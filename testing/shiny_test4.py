from shiny import App, ui, render
from shinywidgets import output_widget, render_widget
import ipywidgets as widgets

# Definir la UI
app_ui = ui.page_fluid(
    ui.tags.style("""
        .car-container { display: flex; justify-content: center; gap: 20px; }
        .car-card { border: 2px solid transparent; border-radius: 10px; padding: 10px; text-align: center; cursor: pointer; }
        .car-card img { width: 150px; height: 100px; border-radius: 10px; }
        .selected { border-color: gold; box-shadow: 0px 0px 10px gold; }
    """),
    
    ui.h3("Checkbox Selection"),
    ui.div(
        ui.div(
            ui.img(src="https://www.mazdausa.com/siteassets/vehicles/2023/mx-5/roadster/trims/club/2023-mazda-mx-5-club-arctic-white.png", width="150px"),
            ui.p("Mazda MX-5 Miata"),
            ui.input_checkbox("miata", "", False),
            class_="car-card", id="miata_card"
        ),
        ui.div(
            ui.img(src="https://www.toyota.com/imgix/responsive/images/mlp/colorizer/2022/supra/3P0.png", width="150px"),
            ui.p("Toyota Supra"),
            ui.input_checkbox("supra", "", False),
            class_="car-card", id="supra_card"
        ),
        class_="car-container"
    ),
)

# Definir la lógica del servidor
def server(input, output, session):
    @render
    def _():
        miata_selected = input.miata()
        supra_selected = input.supra()
        
        script = f"""
        document.getElementById('miata_card').classList.toggle('selected', {str(miata_selected).lower()});
        document.getElementById('supra_card').classList.toggle('selected', {str(supra_selected).lower()});
        """
        return ui.tags.script(script)

# Crear la aplicación
app = App(app_ui, server)
