import os
from shiny import App, ui, reactive, render

def get_dir_contents(path):
    """ Devuelve dos listas: carpetas y archivos en el directorio dado. """
    items = os.listdir(path)
    folders = sorted([".", ".."] + [f for f in items if os.path.isdir(os.path.join(path, f))], key= lambda f: f.lower())
    files = sorted([f for f in items if os.path.isfile(os.path.join(path, f))], key= lambda f: f.lower())
    return folders, files

ROOT_PATH = os.getcwd()



app_ui = ui.page_fluid(
    ui.input_action_button("button_open", "Open Folder Selector"),
    # ui.output_text("current_path"),

)

def server(input, output, session):
    current_path = reactive.value(ROOT_PATH)
    
    @reactive.effect
    @reactive.event(input.button_open)
    def _():
        folders, files = get_dir_contents(current_path())
        FOLDER_BROWSER = ui.modal(
                            ui.panel_title("Select a folder"),
                            ui.output_text_verbatim("current_path_display"),
                            ui.layout_column_wrap(
                                [ui.input_select("in_folder", "Select folder", choices=folders, selected=".", size=10),
                                ui.input_action_button("button_select_folder", "Select Folder")],
                                [ui.input_select("in_file", "Select file", choices=files, selected=".", size=10, multiple=True),
                                ui.input_action_button("button_select_file", "Select File")],
                                ),
                            id="folder_modal"
                        )
        ui.modal_show(FOLDER_BROWSER)
    
    @reactive.effect
    @reactive.event(input.in_folder)
    def change_directory():
        selected = input.in_folder()
        new_path = os.path.abspath(os.path.join(current_path(), selected))
        if os.path.isdir(new_path):
            current_path.set(new_path)
            print(new_path)
            folders, files = get_dir_contents(current_path())
            ui.update_select("in_folder", choices=folders)
            ui.update_select("in_file", choices=files)
    
    @output
    @render.text
    def current_path_display():
        return current_path()

    @reactive.effect
    @reactive.event(input.button_select_folder)
    def select_path():
        print("Selected path", os.path.abspath(os.path.join(current_path(), input.in_folder())))
        
    @reactive.effect
    @reactive.event(input.button_select_file)
    def select_path():
        selected_path = input.in_file()
        # Verificar si hay algo seleccionado
        if selected_path:
            if len(selected_path) > 1:
                print("Selected paths:", [os.path.abspath(os.path.join(current_path(), p)) for p in selected_path])
            else:
                print("Selected path:", os.path.abspath(os.path.join(current_path(), selected_path[0])))  # Si es solo uno, lo imprime como string
        else:
            print("No path selected")


app = App(app_ui, server)

if __name__ == "__main__":
    app.run()
