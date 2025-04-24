from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones
from ....daophot_opt import opt_photo_labels, info_docstrings

def generate_inputs(photo_opt):
    inputs = []
    for key, value in photo_opt.items():
        label = opt_photo_labels.get(key, key)  # Usa el significado si está disponible
        info_text = info_docstrings.get(f"INFO_{key}", "No description available.")
        
        input_field = ui.input_numeric(f"opt_{key}", f"{label}", value=value)
        
        inputs.append(
            ui.div(
                ui.tooltip(input_field, info_text),
            )
        )
    
    return inputs

@module.ui
def nav_panel_opt_PHOTO_ui():
    return ui.page_fillable(
        ui.h4("PHOTO Options"),
        ui.output_ui("options_ui"),
    )   

@module.server
def nav_panel_opt_PHOTO_server(input, output, session, photfun_client, input_tabs_main, input_tabs_daophot):

    @render.ui
    @reactive.event(input_tabs_daophot)
    def options_ui():
        if input_tabs_daophot()=="opt_PHOTO":
            inputs = generate_inputs(photfun_client.photo_opt)
            col1 = inputs[::2]
            col2 = inputs[1::2]
            return ui.layout_columns(col1, col2, col_widths=(6, 6))
    
    # Actualizar valores inmediatamente cuando el usuario cambia un valor
    for key in photfun_client.photo_opt.keys():
        @reactive.Effect
        @reactive.event(input[f"opt_{key}"])
        def update_option(key=key):
            new_value = input[f"opt_{key}"]()
            if new_value is not None:
                photfun_client.photo_opt[key] = float(new_value)
    
    return
