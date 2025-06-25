import os
from shiny import module, reactive, render, ui
from faicons import icon_svg
import pandas as pd
from . import input_local_file_ui, input_local_file_server
import tempfile
from astropy.io.votable import from_table, writeto
from astropy.table import Table


@module.ui
def nav_panel_TABLE_ui():
    return ui.page_fluid(
        ui.layout_column_wrap(
            input_local_file_ui("load_local_table", "Load Table"),
            ui.input_action_button("broadcast_table", "Send Table", 
                                   icon=icon_svg("tower-cell")),
        ),
        ui.layout_column_wrap(
            [
                ui.h4("Table Preview"),
                ui.output_data_frame("table_preview"),  # Muestra el DataFrame
            ],
            [
                ui.input_select("select_table_file", ui.h4("Table Contents"), choices=[], size=10, multiple=True),
                ui.input_action_button("show_info", "File Info", icon=icon_svg("circle-info")),
            ],
        ),
        # Sección de Plots
        ui.layout_column_wrap(
            ui.input_switch("show_plots", "Show Allstar Metrics", value=False),
            # ui.input_action_button("show_plots", "Show Plots", icon=icon_svg("magnifying-glass-chart")),
        ),
        ui.output_ui("plots_ui"),
    )

@module.server
def nav_panel_TABLE_server(input, output, session, photfun_client, samp_client,
                           nav_table_sideview_update, tables_df, input_tabs_main):
    event_load_local_table, input_load_local_table = input_local_file_server(
        "load_local_table", [".csv", ".coo", ".als", ".ap", ".lst", ".mch"]
    )

    @reactive.Effect
    @reactive.event(input_tabs_main)
    def _():
        if input_tabs_main()=="TABLE":
            update_table_selection()

    # Evento al cargar archivos de tabla
    @reactive.Effect
    @reactive.event(event_load_local_table)
    def _():
        archivos = input_load_local_table()
        if not archivos:
            return
        print("PhotFun: Load")
        carpetas = [f for f in archivos if os.path.isdir(f)]
        archivos_table = [f for f in archivos if os.path.isfile(f)]
        for carpeta in carpetas:
            photfun_client.add_table(carpeta)
        if len(archivos_table) > 1:
            photfun_client.add_table(archivos_table)
        elif len(archivos_table) == 1:
            photfun_client.add_table(archivos_table[0])
        nav_table_sideview_update(fits=False, psf=False)


    # Obtener la tabla seleccionada en la lista general
    @reactive.Calc
    def selected_table():
        selected_row = tables_df.data_view(selected=True)
        if selected_row.empty:
            return None  # No hay selección
        selected_id = selected_row.iloc[0]["Table"]
        table_obj = next((f for f in photfun_client.tables if f.id == selected_id), None)
        return table_obj if table_obj else None

    # Actualizar opciones del input select cuando cambia la tabla seleccionada
    @reactive.Effect
    @reactive.event(selected_table)
    def _():
        update_table_selection()
        
    def update_table_selection():
        table_obj = selected_table()
        if not table_obj or not table_obj.path:
            ui.update_select("select_table_file", choices={})  # Limpiar si no hay archivos
            return
        
        choices = {i:os.path.basename(p) for i, p in enumerate(table_obj.path) if not os.path.basename(p).startswith("ERROR.")}
        ui.update_select("select_table_file", choices=choices, selected=0)

    # Mostrar el DataFrame de la tabla seleccionada
    @render.data_frame
    def table_preview():
        table_obj = selected_table()
        if not table_obj or not table_obj.path:
            return pd.DataFrame()  # Retorna un DataFrame vacío si no hay selección
        
        selected_index = next(iter(input.select_table_file()), None)
        if selected_index is None or selected_index == "":
            return pd.DataFrame()

        selected_index = int(selected_index)  # Convertir el índice a entero
        if len(table_obj.path)<=selected_index:
            return pd.DataFrame()

        return table_obj.df(selected_index)  # Retorna el DataFrame correcto

    @reactive.Effect
    @reactive.event(input.show_info)
    def show_file_info():
        # Modal para mostrar la información del File
        ui_file_info =  ui.modal(
                            ui.output_text_verbatim("file_info"),
                            title="Table Info",
                            id="file_info_modal",
                            easy_close=True,
                            size="l",
                        ),
        ui.modal_show(ui_file_info)

    @render.text
    def file_info():
        table_obj = selected_table()
        if not table_obj:
            return "No file selected."
        
        selected_index = next(iter(input.select_table_file()), None)
        if selected_index is None or selected_index == "":
            return "Select a file from the list above."
        
        selected_index = int(selected_index)
        info = table_obj.file_info(selected_index)

        # Generar texto formateado
        info_text = "\n".join([f"{key}: {value}" for key, value in info.items()])
        return info_text

    @reactive.Effect
    @reactive.event(input.broadcast_table)
    def samp_broadcast_table():
        table_obj = selected_table()
        if not table_obj or not table_obj.path:
            ui.notification_show("No se ha seleccionado ninguna tabla", type="error")
            return

        selected_indexes = input.select_table_file()
        if not selected_indexes:
            ui.notification_show("Índice de tabla no válido", type="error")
            return
        
        for selected_index in selected_indexes:
            try:
                selected_index = int(selected_index)
                df = table_obj.df(selected_index)
                alias = os.path.basename(table_obj.path[selected_index])
                print(f"PhotFun: broadcast({alias})")
                # Crear archivo temporal
                with tempfile.NamedTemporaryFile(suffix=f"{alias}.vot", delete=False) as tmpfile:
                    # Convertir DataFrame de pandas a Table de astropy
                    astropy_table = Table.from_pandas(df)
                    
                    # Crear VOTable
                    votable = from_table(astropy_table)
                    votable.description = alias
                    
                    # Escribir archivo temporal
                    writeto(votable, tmpfile.name)
                    out_path = os.path.abspath(tmpfile.name)
                    # Transmitir
                    samp_client.broadcast_table(out_path, alias)
                    ui.notification_show(
                        f"Broadcast {alias}",
                        type="message",
                        duration=5
                    )

            except Exception as e:
                ui.notification_show(
                    f"Broadcast error: {str(e)}",
                    type="error",
                    duration=10
                )

    # Función utilitaria de validación
    def _check_columns(df, cols):
        missing = [c for c in cols if c not in df.columns]
        if missing:
            ui.notification_show(f"Column(s) missing: {', '.join(missing)}", type="error")
            return False
        return True

    # Generar contenido de plots dinámicamente
    @render.ui
    def plots_ui():
        if not input.show_plots():
            # Espacio vacío cuando está desactivado
            return ui.div(style="height:16px")
        # Mostrar los tres plots en columnas
        return ui.layout_column_wrap(
            ui.output_plot("plot_merr"),
            ui.output_plot("plot_sharp"),
            ui.output_plot("plot_chi"),
        )

    # Render plots con chequeo de columnas
    @render.plot
    def plot_chi():
        if not input.show_plots():
            return
        tbl = selected_table()
        idx = next(iter(input.select_table_file()), None)
        if not tbl or idx is None or idx == "":
            return
        df = tbl.df(int(idx))
        if not _check_columns(df, ["MAG", "merr", "chi", "sharpness"]):
            return
        sel = df[(df.merr < 0.13) & (df.chi < 2) & (df.sharpness.between(-1, 1))]
        no_sel = df
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        ax.plot(no_sel.MAG, no_sel.chi, 'k+', alpha=0.4)
        ax.plot(sel.MAG, sel.chi, 'r+')
        ax.set_ylim(0, 7)#; ax.set_xlim(11, 22)
        ax.set_xlabel('MAG'); ax.set_ylabel('$\chi^2$')
        ax.grid()
        return fig

    @render.plot
    def plot_sharp():
        if not input.show_plots():
            return
        tbl = selected_table()
        idx = next(iter(input.select_table_file()), None)
        if not tbl or idx is None or idx == "":
            return
        df = tbl.df(int(idx))
        if not _check_columns(df, ["MAG", "merr", "chi", "sharpness"]):
            return
        sel = df[(df.merr < 0.13) & (df.chi < 2) & (df.sharpness.between(-1, 1))]
        no_sel = df
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        ax.plot(no_sel.MAG, no_sel.sharpness, 'k+', alpha=0.4)
        ax.plot(sel.MAG, sel.sharpness, 'r+')
        # ax.set_xlim(11, 22)
        ax.set_ylim(-4, 4)
        ax.set_xlabel('MAG'); ax.set_ylabel('Sharp')
        ax.grid()
        return fig

    @render.plot
    def plot_merr():
        if not input.show_plots():
            return
        tbl = selected_table()
        idx = next(iter(input.select_table_file()), None)
        if not tbl or idx is None or idx == "":
            return
        df = tbl.df(int(idx))
        if not _check_columns(df, ["MAG", "merr", "chi", "sharpness"]):
            return
        sel = df[(df.merr < 0.13) & (df.chi < 2) & (df.sharpness.between(-1, 1))]
        no_sel = df
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        ax.plot(no_sel.MAG, no_sel.merr, 'k+', alpha=0.4)
        ax.plot(sel.MAG, sel.merr, 'r+')
        # ax.set_xlim(11, 22)
        ax.set_ylim(0, 0.5)
        ax.set_xlabel('MAG'); ax.set_ylabel('$MAG_{err}$')
        ax.grid()
        return fig

    return
