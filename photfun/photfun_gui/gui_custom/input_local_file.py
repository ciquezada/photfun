import os
import re
from shiny import module, reactive, render, ui

def get_dir_contents(path, ext_filter=None, filter_file=None):
    items = os.listdir(path)
    folders = sorted([".", ".."] + [f for f in items if os.path.isdir(os.path.join(path, f))], key=lambda f: f.lower())
    files = [f for f in items if os.path.isfile(os.path.join(path, f))]
    if ext_filter:
        if isinstance(ext_filter, str):  
            ext_filter = [ext_filter]
        files = [f for f in files if os.path.splitext(f)[1].lower() in ext_filter]
    if filter_file and filter_file != "":
        raw = filter_file
        # Si el usuario no puso ningún '*', envolvemos para que sea substring match
        if "*" not in raw:
            raw = f"*{raw}*"
        # Escapamos todos los caracteres especiales, salvo '*'
        escaped = re.escape(raw)
        replaced = escaped.replace(r"\*", ".*")
        # Reemplazamos los '\*' generados por re.escape() con '.*'
        pattern = f"^{replaced}$"
        regex = re.compile(pattern, re.IGNORECASE)
        # Filtramos usando fullmatch (coincidencia completa) o search (si sólo quieres buscar en cualquier parte)
        files = [f for f in files if regex.match(f)]
    files = sorted(["."] + files, key=lambda f: f.lower())
    return folders, files


def find_existing_parent(path):
    path = os.path.abspath(path)
    while not os.path.isdir(path):
        parent = os.path.dirname(path)
        if parent == path:
            # Se llegó a la raíz y no se encontró un directorio válido
            return path
        path = parent
    return path


ROOT_PATH = os.getcwd()


@module.ui
def input_local_file_ui(label="Load File", width="100%"):
    return ui.page_fluid(
        ui.input_action_button("button_open", label, width=width),
        # ui.tags.head(ui.tags.style("""
        #     .modal-dialog { 
        #         width: flex;
        #         height: flex;
        #     }
        #     .modal-content {
        #         height: flex;
        #     }
        #     #folder_modal .select-container {
        #         display: flex;
        #         gap: 15px;
        #     }
        #     #folder_modal .scrollable {
        #         height: 100%;
        #         overflow: auto;
        #     }
        # """))
    )

@module.server
def input_local_file_server(input, output, session, ext_filter):
    current_path = reactive.value(ROOT_PATH)
    selected_path_out = reactive.value(ROOT_PATH)
    
    @reactive.effect
    @reactive.event(input.button_open)
    def _():
        new_path = os.path.abspath(current_path())
        valid_path = find_existing_parent(new_path)
        if valid_path:
            current_path.set(valid_path)
            # Actualiza los selectores con el nuevo directorio válido
            folders, files = get_dir_contents(valid_path, ext_filter)
        else:
            session.send_message(f"‘{raw_path}’ invalid directory.", type="error")
            return 

        FOLDER_BROWSER = ui.modal(
            ui.div(
                ui.h4("Select a folder", class_="modal-title"),
                # ui.output_text_verbatim("current_path_display"),
                # — CAMBIO: input_text en lugar de output_text_verbatim —
                ui.input_text(
                    "current_path_input",
                    None,
                    value=current_path(),
                    placeholder="Type or edit path…",
                    width="100%",
                    update_on="blur"  # dispara al perder foco o Enter
                ),
                ui.layout_column_wrap(
                    ui.div(
                        ui.input_select("in_folder", "Folders", 
                                      choices=folders, 
                                      selected=".", 
                                      size=12,
                                      ),
                        # class_="col-6 p-0",
                        # style="display: flex; justify-content: flex-start;",
                        
                    ),
                    ui.div(
                        ui.input_select("in_file", "Files", 
                                      choices=files, 
                                      selected=".", 
                                      size=12,
                                      multiple=True,
                                      ),
                        ui.input_text("filter_file", None, placeholder="Type to filter..."), 
                        ui.input_action_button("button_select_file", 
                                              "Select File",
                                              class_="btn-primary"),
                        # class_="col-6 d-flex flex-column",
                		# style="display: flex; justify-content: flex-start;",
                        
                    ),
                    # class_="select-container",
                	# style="margin-top: 20px; display: flex; gap: 10px; justify-content: flex-start;"
                ),
                # class_="d-flex flex-column h-80",
            ),
            id="folder_modal",
            size="xl",
            easy_close=True,
            footer=None,
            # style="justify-content: width: 800px"
            # class_="modal-dialog-centered"
        )
        ui.modal_show(FOLDER_BROWSER)
    
    @reactive.effect
    @reactive.event(input.current_path_input)
    def on_path_edit():
        raw_path = input.current_path_input().strip()
        new_path = os.path.abspath(raw_path)
        valid_path = find_existing_parent(new_path)
        if valid_path:
            current_path.set(valid_path)
            # Actualiza los selectores con el nuevo directorio válido
            folders, files = get_dir_contents(valid_path, ext_filter, input.filter_file())
            ui.update_select("in_folder", choices=folders)
            ui.update_select("in_file", choices=files)
            # Opcional: actualizar el campo de texto con la ruta válida encontrada
            ui.update_text("current_path_input", value=valid_path)
        else:
            session.send_message(f"‘{raw_path}’ invalid directory.", type="error")

    @reactive.effect
    @reactive.event(input.in_folder)
    def change_directory():
        selected = input.in_folder()
        new_path = os.path.abspath(os.path.join(current_path(), selected))
        if os.path.isdir(new_path):
            current_path.set(new_path)
            folders, files = get_dir_contents(current_path(), ext_filter, input.filter_file())
            ui.update_select("in_folder", choices=folders)
            ui.update_select("in_file", choices=files)
            ui.update_text("current_path_input", value=current_path())
    
    @reactive.effect
    @reactive.event(input.filter_file)
    def filtering_files():
        folders, files = get_dir_contents(current_path(), ext_filter, input.filter_file())
        ui.update_select("in_folder", choices=folders)
        ui.update_select("in_file", choices=files)



    # @reactive.Calc
    # def current_path_display():
    #     return ui.update_text("current_path_input", current_path())
        
    @reactive.effect
    @reactive.event(input.button_select_file)
    def select_file():
        selected_path = input.in_file()
        selected_path_out.set([os.path.abspath(os.path.join(current_path(), p)) for p in selected_path])
        
        
    return input.button_select_file, selected_path_out

# app = App(app_ui, server)

# if __name__ == "__main__":
#     app.run()
