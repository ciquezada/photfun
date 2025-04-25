import os
from shiny import module, reactive, render, ui
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm, Normalize
import time
from datetime import timedelta
from .plot_preview_tools import (source_preview, generate_prof, generate_prof_fast, 
                                                                generate_prof_animation, generate_rotation_animation)
from ...misc_tools import temp_mkdir, move_file_noreplace
import shutil


@module.ui
def nav_panel_SELECTION_ui():
    m = ui.page_fillable(
            ui.layout_columns(
                ui.page_fillable(
                    ui.h4("FITS Preview"),
                    ui.output_plot("plot_fits"),
                ),
                ui.page_fillable(
                    ui.output_data_frame("table_preview")
                ),  
                col_widths=(6, 6),
            ),
            ui.hr(),
            ui.layout_columns(
                ui.input_action_button("load_selection", "Load sources selection"),
                ui.input_action_button("export_selection", "Export Selection"),
                ui.input_select(
                    "selected_function",
                    "",
                    choices=[
                                "Optional advanced preview:",
                                "",
                                "Generate profile",
                                "Generate profile faster",
                                "Generate animated profile",
                                "Generate animated 3d profile",
                            ],  # Opciones en el dropdown
                    selected="Optional advanced preview:",  # Opción por defecto
                ),
                col_widths=(4,4,4)
            ),
            # ui.div(style="heigth: 50px; display: inline-block;"),
            ui.output_ui("cards_ui")
        )
    return m

@module.server
def nav_panel_SELECTION_server(input, output, session, photfun_client, nav_table_sideview_update, fits_df, tables_df):

    id_current_preview = reactive.Value(int()) 
    # Obtener la imagen FITS seleccionada
    @reactive.Calc
    def selected_fits():
        selected_row = fits_df.data_view(selected=True)
        if selected_row.empty:
            return None
        selected_id = selected_row.iloc[0]["FITS"]
        fits_obj = next((f for f in photfun_client.fits_files if f.id == selected_id), None)
        return fits_obj.image(0) if fits_obj else None

    # Obtener la tabla seleccionada
    @reactive.Calc
    def selected_table():
        selected_row = tables_df.data_view(selected=True)
        if selected_row.empty:
            return None
        selected_id = selected_row.iloc[0]["Table"]
        table_obj = next((f for f in photfun_client.tables if f.id == selected_id), None)
        return table_obj.df(0) if table_obj else None

    # Graficar la imagen FITS con posiciones de la tabla si está disponible
    @render.plot()
    def plot_fits():
        fits_image = selected_fits()
        table_df = selected_table()
        
        if not fits_image:
            return
        
        fig, ax = plt.subplots(figsize=(7.5, 7.5))
        image_data = np.array(fits_image.data)
        image_data = np.nan_to_num(image_data, nan=0)
        image_data[image_data < 0] = 0
        # ax.imshow(image_data, cmap='gray', norm=LogNorm())
        vmin, vmax = np.percentile(image_data, [25, 90])
        ax.imshow(image_data, cmap='gray', norm=Normalize(vmin=vmin, vmax=vmax))
        ax.invert_yaxis()
        
        if table_df is not None and "X" in table_df and "Y" in table_df:
            ax.scatter(table_df["X"], table_df["Y"], edgecolors='red', facecolors='none', s=30, alpha=0.3)

        
        fig.tight_layout()
        return fig

    # Mostrar el DataFrame de la tabla seleccionada
    @render.data_frame
    def table_preview():
        table_df = selected_table()
        return table_df  # Retorna el DataFrame para visualizarlo

    @render.ui
    @reactive.event(input.load_selection)
    def cards_ui():
        selected_row = tables_df.data_view(selected=True)
        if selected_row.empty:
            return ui.notification_show(f"Error: Select a Table", type="error")
        id_current_preview.set(selected_row.iloc[0]["Table"])

        preview_functions = {
                                "Optional advanced preview:": None,
                                "": None,
                                "Source preview": source_preview,
                                "Generate profile": generate_prof,
                                "Generate profile faster": generate_prof_fast,
                                "Generate animated profile": generate_prof_animation,
                                "Generate animated 3d profile": generate_rotation_animation
                            }
        table_df = selected_table()
        fits_image = selected_fits()

        mag_col = next((col for col in table_df.columns if 'mag' in col.lower()), None)
        if mag_col:
            table_df = table_df.sort_values(by=mag_col)

        start_time = time.time()
        cards = []
        with ui.Progress(min=0, max=1) as p:
            p.set(message="Loading previews", detail="This may take a while...")
            total = table_df.shape[0]
            for i, row in table_df.iterrows():
                elapsed_time = time.time() - start_time
                time_per_iter = elapsed_time / (i+1)
                remaining_time = time_per_iter * (total - i)
                remaining_time_str = remaining_time_str = str(timedelta(seconds=int(remaining_time))) if i > 1 else "Estimating..."

                p.inc(1/total, message="Loading previews", detail=f"{remaining_time_str}")
                ####################

                img_data = preview_functions["Source preview"](row, fits_image)
                adv_func = preview_functions[input.selected_function()]
                img_data_adv = adv_func(row, fits_image, photfun_client.n_jobs) if adv_func else None # Genera la animación

                # Obtener MAG si existe en la fila
                mag_value = next((row[col] for col in row.index if 'mag' in col.lower()), None)
                header = f"{row['ID']} - MAG: {mag_value}" if mag_value is not None else f"{row['ID']}"

                card = ui.card(
                            ui.card_header(header),
                            ui.layout_columns(
                                ui.img(src=f"data:image/gif;base64,{img_data}", width="100%"),
                                ui.img(src=f"data:image/gif;base64,{img_data_adv}", width="100%") if adv_func else None,
                                col_widths=(6, 6) if adv_func else (12),
                            ),
                        )
                cards.append((row.ID, card))

        # Dividir las tarjetas en tres subgrupos
        num_columns = 2
        split_cards = [cards[i::num_columns] for i in range(num_columns)]  # Divide en 3 sublistas

        # Crear el layout con tres columnas
        columns = []
        for card_group in split_cards:
            # Crear un grupo de checkboxes por columna
            checkbox_group = ui.input_checkbox_group(
                f"selected_cards_{split_cards.index(card_group)}", 
                "", 
                choices={i: card for (i, card) in card_group}, 
                selected=[i for (i, card) in card_group],  # Pre-seleccionar
                width="100%"
            )
            columns.append(checkbox_group)
        
        # Disponer las columnas
        return ui.layout_columns(*columns, col_widths=(6, 6))

    @reactive.Effect
    @reactive.event(input.export_selection)
    def _():
        selected_row = tables_df.data_view(selected=True)
        if selected_row.empty:
            return ui.notification_show(f"Error: Select a Table", type="error")
        selected_id = selected_row.iloc[0]["Table"]
        table_obj = next((f for f in photfun_client.tables if f.id == selected_id), None)
        if selected_id!=id_current_preview():
            return ui.notification_show(f"Error: Another table selected during selection, select the respectively", type="error")
        selected_ids = []
        for checkbox_group in range(2):  # Se asume que hay 2 grupos
            selected_ids += [i for i in input[f"selected_cards_{checkbox_group}"]()]
        output_dir = photfun_client.working_dir
        table_name = os.path.splitext(os.path.basename(table_obj.path[0]))[0]
        out_subtable = os.path.join(output_dir, f"{table_name}_sel{table_obj.file_type}")
        temp_dir = os.path.abspath(temp_mkdir(f"{table_name}_subselection_0"))
        temp_table = os.path.join(temp_dir, os.path.basename(table_obj.path[0]))
        table_obj.subtable(temp_table, selected_ids)
        final_out_subtable = move_file_noreplace(temp_table, out_subtable)
        new_table_obj = photfun_client.add_table(final_out_subtable)
        shutil.rmtree(temp_dir)
        nav_table_sideview_update()
        ui.notification_show(f"Selected {new_table_obj.df(0).shape[0]} sources\n -> [{new_table_obj.id}] {new_table_obj.alias}")


    # @render.ui
    # @reactive.event(input.load_selection)
    # def cards_ui():
    #     import concurrent.futures
    #     from functools import partial
        
    #     selected_row = tables_df.data_view(selected=True)
    #     if selected_row.empty:
    #         return ui.notification_show(f"Error: Select a Table", type="error")
    #     id_current_preview.set(selected_row.iloc[0]["Table"])

    #     preview_functions = {
    #         "Optional advanced preview:": None,
    #         "Source preview": source_preview,
    #         "Generate profile": generate_prof,
    #         "Generate profile faster": generate_prof_fast,
    #         "Generate animated profile": generate_prof_animation,
    #         "Generate animated 3d profile": generate_rotation_animation
    #     }

    #     table_df = selected_table()
    #     fits_image = selected_fits()

    #     mag_col = next((col for col in table_df.columns if 'mag' in col.lower()), None)
    #     if mag_col:
    #         table_df = table_df.sort_values(by=mag_col)

    #     start_time = time.time()
    #     cards = []
        
    #     # Función para procesamiento paralelo
    #     def process_row(row, selected_func, fits_img):
    #         try:
    #             img_data = source_preview(row, fits_img)
    #             adv_func = preview_functions.get(selected_func)
    #             img_data_adv = adv_func(row, fits_img) if adv_func else None
                
    #             mag_value = next((row[col] for col in row.index if 'mag' in col.lower()), None)
    #             header = f"{row['ID']} - MAG: {mag_value}" if mag_value else f"{row['ID']}"
                
    #             return (row['ID'], img_data, img_data_adv, header)
    #         except Exception as e:
    #             print(f"Error procesando fila {row['ID']}: {str(e)}")
    #             return None

    #     # Configurar paralelismo
    #     num_workers = os.cpu_count() - 1 or 1
    #     processed = 0
    #     total_rows = len(table_df)
        
    #     with ui.Progress(min=1, max=total_rows) as p, \
    #         concurrent.futures.ThreadPoolExecutor(max_workers=num_workers) as executor:
            
    #         p.set(message="Preparando procesamiento paralelo...")
            
    #         # Crear partial function con parámetros fijos
    #         process_func = partial(
    #             process_row,
    #             selected_func=input.selected_function(),
    #             fits_img=fits_image
    #         )
            
    #         # Enviar tareas al executor
    #         futures = [executor.submit(process_func, row) for _, row in table_df.iterrows()]
            
    #         # Procesar resultados conforme se completan
    #         for future in concurrent.futures.as_completed(futures, timeout=300):
    #             processed += 1
    #             elapsed = time.time() - start_time
    #             remaining = (elapsed / processed) * (total_rows - processed) if processed > 0 else 0
    #             remaining_str = str(timedelta(seconds=int(remaining))) if processed > 1 else "Estimando..."
                
    #             p.set(
    #                 processed,
    #                 message="Generando vistas previas",
    #                 detail=f"Tiempo restante: {remaining_str}"
    #             )
                
    #             result = future.result()
    #             if result:
    #                 row_id, img_data, img_data_adv, header = result
    #                 card = ui.card(
    #                     ui.card_header(header),
    #                     ui.layout_columns(
    #                         ui.img(src=f"data:image/gif;base64,{img_data}", width="100%"),
    #                         ui.img(src=f"data:image/gif;base64,{img_data_adv}", width="100%") if img_data_adv else None,
    #                         col_widths=(6, 6) if img_data_adv else (12,),
    #                     ),
    #                 )
    #                 cards.append((row_id, card))

    #     # Organizar columnas
    #     num_columns = 2
    #     split_cards = [cards[i::num_columns] for i in range(num_columns)]
        
    #     columns = []
    #     for idx, card_group in enumerate(split_cards):
    #         checkbox_group = ui.input_checkbox_group(
    #             f"selected_cards_{idx}", 
    #             "", 
    #             choices={i: card for (i, card) in card_group}, 
    #             selected=[i for (i, _) in card_group],
    #             width="100%"
    #         )
    #         columns.append(checkbox_group)
        
    #     return ui.layout_columns(*columns, col_widths=(6, 6))