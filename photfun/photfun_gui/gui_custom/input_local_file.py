import os
from shiny import module, reactive, render, ui

def get_dir_contents(path, ext_filter=None):
    items = os.listdir(path)
    folders = sorted([".", ".."] + [f for f in items if os.path.isdir(os.path.join(path, f))], key=lambda f: f.lower())
    files = [f for f in items if os.path.isfile(os.path.join(path, f))]
    if ext_filter:
        if isinstance(ext_filter, str):  
            ext_filter = [ext_filter]
        files = [f for f in files if os.path.splitext(f)[1].lower() in ext_filter]
    files = sorted(["."] + files, key=lambda f: f.lower())
    return folders, files


ROOT_PATH = os.getcwd()


@module.ui
def input_local_file_ui(label="Load File"):
    return  ui.page_fluid(
                ui.input_action_button("button_open", label, width="100%"),
            )

@module.server
def input_local_file_server(input, output, session, ext_filter):
    current_path = reactive.value(ROOT_PATH)
    selected_path_out = reactive.value(ROOT_PATH)
    
    @reactive.effect
    @reactive.event(input.button_open)
    def _():
        folders, files = get_dir_contents(current_path(), ext_filter)
        FOLDER_BROWSER = ui.modal(
                            ui.panel_title("Select a folder"),
                            ui.output_text_verbatim("current_path_display"),
                            ui.layout_column_wrap(
                                ui.input_select("in_folder", "Select folder", choices=folders, selected=".", size=10),
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
            folders, files = get_dir_contents(current_path(), ext_filter)
            ui.update_select("in_folder", choices=folders)
            ui.update_select("in_file", choices=files)
    
    @output
    @render.text
    def current_path_display():
        return current_path()
        
    @reactive.effect
    @reactive.event(input.button_select_file)
    def select_file():
        selected_path = input.in_file()
        selected_path_out.set([os.path.abspath(os.path.join(current_path(), p)) for p in selected_path])
        
        
    return input.button_select_file, selected_path_out

# app = App(app_ui, server)

# if __name__ == "__main__":
#     app.run()
