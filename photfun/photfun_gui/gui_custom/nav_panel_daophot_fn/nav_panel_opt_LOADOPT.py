from shiny import module, reactive, render, ui
from faicons import icon_svg  # Para iconos en botones
from ....daophot_opt import opt_photo_labels, info_docstrings
import os

@module.ui
def nav_panel_opt_LOADOPT_ui():
    return ui.page_fillable(
        ui.layout_columns(
            # Left column - Table selection
            ui.div(
                # Nueva sección para guardar parámetros
                ui.div(
                    ui.h4("Save current OPT", class_="mb-3"),
                    ui.input_action_button(
                        "save_btn",
                        "Export OPT files",
                        icon=icon_svg("floppy-disk"),
                        class_="btn-success"
                    ),
                    class_="mb-4 p-3 border rounded"
                ),
                
                # Sección existente de carga (modificada)
                ui.div(
                    ui.h4("Import OPT table", class_="mb-3"),
                    ui.input_select("params_table_select", "Parameter Table:", 
                                   choices={}, width="100%"),
                    ui.div(
                        ui.input_action_button("load_btn", "LOAD PARAMETERS", 
                                             icon=icon_svg("upload"), 
                                             class_="btn-primary"),
                        class_="text-center mt-3"
                    ),
                    class_="p-3 border rounded"
                ),
                # Nueva sección para Ajustar parametros PhotFun
                ui.div(
                    ui.h4("Other Configurations", class_="mb-3"),
                    ui.tooltip(
                        ui.input_numeric("daophot_timeout", "Daophot Timeout", 
                                    value=60,  # Valor por defecto de 'fi' en opt_daophot_dict
                                    step=1,
                                    min=0.001
                                    ),
                        "DAOPHOT subroutines timout (s) before killing. \
                        Prevents DAOPHOT subroutine stuck"
                    ),
                    ui.tooltip(
                        ui.input_numeric("n_jobs", "Parallel processing", 
                                    value=-1,  # Valor por defecto de 'fi' en opt_daophot_dict
                                    step=1,
                                    min=-1, max=os.cpu_count()
                                    ),
                        "Number of cores for parallel processing. \
                        -1 for all cores.  \
                        In case of Docker creates 1 container per process."
                    ),
                    class_="mb-4 p-3 border rounded"
                ),
                style=("padding-right: 20px; border-right: 1px solid #dee2e6;"
                      "display: flex; flex-direction: column; gap: 1.5rem;")
            ),
            
            # Right column - Parameter preview
            ui.div(
                ui.h5("Parameters in selected table:", class_="text-muted"),
                ui.output_ui("params_preview"),
                class_="p-3 border rounded",
                style="padding-left: 20px;"
            ),
            col_widths=(6, 6)
        )
    )

@module.server
def nav_panel_opt_LOADOPT_server(input, output, session, photfun_client, 
                               nav_table_sideview_update, input_tabs_main, input_sub_tab):
    
    # Update available tables list
    def update_tables():
        table_choices = {str(obj.id): f"[{obj.id}] {obj.alias}" for obj in photfun_client.tables}
        ui.update_select("params_table_select", choices=table_choices)
        ui.update_numeric("daophot_timeout", value=photfun_client.daophot_timeout)
        ui.update_numeric("n_jobs", value=photfun_client.n_jobs)

    @reactive.Effect
    @reactive.event(input.daophot_timeout)
    def _():
        photfun_client.daophot_timeout = float(input.daophot_timeout())

    @reactive.Effect
    @reactive.event(input.n_jobs)
    def _():
        photfun_client.n_jobs = int(input.n_jobs())

    # Parameter preview
    @output
    @render.ui
    def params_preview():
        table = selected_table()
        
        # Validation checks
        valid_table = False
        if table:
            valid_table = len(table.path) == 3 and all(
                {'alias', 'param', 'value'}.issubset(df.columns) 
                for df in [table.df(0), table.df(1), table.df(2)]
            )
        else:
            return ui.div(
                ui.h6("Select a parameter table", class_="text-muted"),
                ui.p("Select a table containing optimized parameters", 
                    class_="text-muted small"),
                class_="text-center"
            )

        if not valid_table:
            return ui.div(
                ui.h6("Invalid parameter table", class_="text-muted"),
                ui.p("Select a table containing optimized parameters", 
                    class_="text-muted small"),
                class_="text-center"
            )
        
        # Display parameters if valid
        return ui.div(
            ui.h6("Parameters preview:", class_="text-success"),
            ui.div(
                ui.div(
                    ui.span("ALLSTAR:", class_="fw-bold"),
                    ", ".join([f"{row['alias']}:{row['value']}" 
                            for _, row in table.df(0).iterrows()]),
                ),
                ui.div(
                    ui.span("DAOPHOT:", class_="fw-bold"),
                    ", ".join([f"{row['alias']}:{row['value']}" 
                            for _, row in table.df(1).iterrows()]),
                    class_="mb-2"
                ),
                ui.div(
                    ui.span("PHOTO:", class_="fw-bold"),
                    ", ".join([f"{row['alias']}:{row['value']}" 
                            for _, row in table.df(2).iterrows()]),
                    class_="mb-2"
                ),
                style="font-family: monospace; font-size: 0.9em;"
            ),
            class_="text-left"
        )

    # Selected table
    @reactive.Calc
    def selected_table():
        selected_id = input.params_table_select()
        return next((t for t in photfun_client.tables if str(t.id) == selected_id), None)

    # Load parameters handler
    @reactive.Effect
    @reactive.event(input.load_btn)
    def load_params():
        table = selected_table()
        if not table:
            ui.notification_show("Please select a table first!", type="error")
            return
        
        try:
            photfun_client.load_parameters(table.id)
            ui.notification_show(
                ui.div(
                    ui.h5("OPT tables imported"),
                ), 
                duration=5,
                type="message"
            )
        except Exception as e:
            ui.notification_show(
                f"Error importing OPT table: {str(e)}", 
                type="error",
                duration=8
            )

    # Nuevo handler para guardar parámetros
    @reactive.Effect
    @reactive.event(input.save_btn)
    def save_current_params():
        try:
            saved_table = photfun_client.save_current_parameters()
            ui.notification_show(
                f"OPTs exported to table [>{saved_table.id}] {saved_table.alias}",
                type="message",
                duration=5
            )
            update_tables()  # Actualizar lista de tablas
            nav_table_sideview_update()

        except Exception as e:
            ui.notification_show(
                f"Error exporting OPT tables: {str(e)}", 
                type="error",
                duration=8
            )

    # Update when entering panel
    @reactive.Effect
    @reactive.event(input_tabs_main)
    def _():
        if input_tabs_main() == "DAOPHOT" or input_tabs_main() == "PIPELINES":
            update_tables()

    @render.ui
    @reactive.event(input_sub_tab)
    def options_ui():
        if input_sub_tab()=="opt_LOADOPT":
            update_tables()


    return