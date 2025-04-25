import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import tempfile

def get_dir_contents(path):
    items = os.listdir(path)
    folders = sorted([".", ".."] + [f for f in items if os.path.isdir(os.path.join(path, f))], 
                    key=lambda f: f.lower())
    return folders

ROOT_PATH = os.getcwd()

@module.ui
def output_save_location_ui(label="Select Save Location"):
    return ui.page_fluid(
        ui.input_action_button("button_open", label, width="auto")
    )

@module.server
def output_save_location_server(input, output, session):
    current_path = reactive.Value(ROOT_PATH)
    selected_path = reactive.Value("")
    
    def create_folder_modal():
        return ui.modal(
            ui.input_text("new_folder_name", "", placeholder="my_new_folder"),
            ui.div(
                ui.input_action_button("confirm_create", "Accept", class_="btn-primary"),
                ui.input_action_button("cancel_create", "Cancel"),
                style="margin-top: 20px; display: flex; gap: 10px; justify-content: flex-start;"
            ),
            # title="Crear nueva carpeta",
            easy_close=False
        )
    
    @reactive.Effect
    @reactive.event(input.button_open)
    def _():
        folders = get_dir_contents(current_path())
        SAVE_BROWSER = ui.modal(
            ui.panel_title(f"Exporting files"),
            ui.layout_column_wrap(
                ui.div(
                    ui.input_action_button("create_folder", "", icon=icon_svg("folder-plus")),
                    ui.output_text_verbatim("current_path_display"),
                    style="text-align: left; "
                ),
            ),
            ui.input_select("in_folder", "", 
                            choices=folders, selected=".", size=10),
            ui.div(
                ui.input_action_button("confirm_save", "Confirm", class_="btn-primary"),
                style="margin-top: 20px; text-align: left; display: flex; justify-content: flex-start;"
            ),
            size="l"
        )
        ui.modal_show(SAVE_BROWSER)
    
    @reactive.Effect
    @reactive.event(input.create_folder)
    def show_create_folder():
        ui.modal_show(create_folder_modal())
    
    @reactive.Effect
    @reactive.event(input.confirm_create)
    def create_new_folder():
        new_folder = input.new_folder_name()
        if new_folder:
            try:
                full_path = os.path.join(current_path(), new_folder)
                os.makedirs(full_path, exist_ok=False)
                update_directory_listing()
            except Exception as e:
                ui.notification_show(f"Error creando carpeta: {str(e)}", type="error")
    
    @reactive.Effect
    @reactive.event(input.in_folder)
    def change_directory():
        selected = input.in_folder()
        new_path = os.path.abspath(os.path.join(current_path(), selected))
        if os.path.isdir(new_path):
            current_path.set(new_path)
            update_directory_listing()
    
    def update_directory_listing():
        folders = get_dir_contents(current_path())
        ui.update_select("in_folder", choices=folders, selected=".")
    
    @output
    @render.text
    def current_path_display():
        return f"-> {current_path()}"
    
    @reactive.Effect
    @reactive.event(input.confirm_save)
    def finalize_selection():
        selected_path.set(current_path())
        ui.modal_remove()
    
    return input.confirm_save, selected_path