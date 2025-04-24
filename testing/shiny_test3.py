import os
from shiny import App, ui, reactive, render
import pandas as pd

def get_dir_contents(path):
    """ Devuelve dos listas: carpetas y archivos en el directorio dado. """
    items = os.listdir(path)
    folders = [".", ".."] + [f for f in items if os.path.isdir(os.path.join(path, f))]
    files = [f for f in items if os.path.isfile(os.path.join(path, f))]
    return folders, files

ROOT_PATH = os.getcwd()



app_ui = ui.page_fluid(
    ui.input_action_button("button_open", "Open Folder Selector"),
    # ui.output_text("current_path"),

)

def server(input, output, session):
    current_path = reactive.value(ROOT_PATH)
    folder_loading, file_loading = get_dir_contents(ROOT_PATH)
    folder_loading, file_loading = reactive.value(pd.DataFrame({"Folders": folder_loading})), reactive.value(pd.DataFrame({"Files": file_loading}))

    @reactive.effect
    @reactive.event(input.button_open)
    def _():
        FOLDER_BROWSER = ui.modal(
                            ui.output_text("current_path_display"),
                            ui.panel_title("Select a folder or file"),
                            ui.layout_column_wrap(
                                ui.div(
                                    ui.output_data_frame("folder_table"),
                                    ui.input_action_button("select_folder", "Select Folder")
                                ),
                                ui.div(
                                    ui.output_data_frame("file_table"),
                                    ui.input_action_button("select_file", "Select File")
                                ),
                            ),
                            id="folder_modal"
                        )   
        ui.modal_show(FOLDER_BROWSER)
    
    @reactive.effect
    def change_directory():
        if len(folder_table.data_view(selected=True).Folders.values):
            selected = folder_table.data_view(selected=True).Folders.values[0]
            print(selected)
            new_path = os.path.abspath(os.path.join(current_path(), selected))
            print(new_path)
            if os.path.isdir(new_path):
                current_path.set(new_path)
                folder, file = get_dir_contents(current_path())
                print(folder)
            #     folder_loading.set(pd.DataFrame({"Folders": folder}))
            #     file_loading.set(pd.DataFrame({"Files": file}))
    
    @output
    @render.text
    def current_path_display():
        return current_path()

    @output    
    @render.data_frame
    def folder_table():
        folders, _ = get_dir_contents(current_path())
        return render.DataGrid(folder_loading(), selection_mode="rows")

    @output
    @render.data_frame
    def file_table():
        _, files = get_dir_contents(current_path())
        return render.DataGrid(file_loading(), selection_mode="rows")

    @reactive.effect
    @reactive.event(input.button_select)
    def select_path():
        print("Selected path", os.path.abspath(os.path.join(current_path(), input.dir())))


app = App(app_ui, server)

if __name__ == "__main__":
    app.run()
