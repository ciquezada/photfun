from shiny import App, ui, reactive, render

# Definir la interfaz de usuario
app_ui = ui.page_fluid(
    ui.h2("Shiny App con un Botón"),
    ui.input_action_button("my_button", "Haz clic aquí"),
    ui.output_text("output_1")
)

# Definir la lógica del servidor
def server(input, output, session):
    # Crear una señal reactiva para rastrear el estado del botón
    button_pressed = reactive.Value(False)

    @reactive.Effect
    @reactive.event(input.my_button)
    def on_button_click():
        print("El botón fue presionado")
        if button_pressed():
            button_pressed.set(False)
            return 1
        button_pressed.set(True)  # Actualizar el estado a True


    # Generar la salida basada en el estado del botón
    # @output
    @render.text
    def output_1():
        if button_pressed():
            return "¡El botón fue presionado!"
        return ""  # Salida vacía por defecto

# Crear y ejecutar la aplicación
app = App(app_ui, server)

if __name__ == "__main__":
    app.run()
