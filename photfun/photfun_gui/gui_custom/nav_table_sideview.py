from shiny import module, reactive, render, ui
import pandas as pd
from faicons import icon_svg

@module.ui
def nav_table_sideview_ui():
    m = ui.sidebar(
            ui.div(
                ui.h4("Loaded Data", class_="d-inline"),
                ui.div(
                    ui.input_action_button(
                        "reconnect_samp",
                        "Reconnect SAMP",
                        icon=icon_svg("plug"),
                        class_="btn-sm btn-outline-primary float-end"
                    ),
                    class_="d-inline-block float-end"
                ),
                class_="d-flex justify-content-between align-items-center mb-3"
            ),
            # FITS Section
            ui.div(
                ui.output_data_frame("fits_df"),
                ui.input_action_button(
                    "delete_fits",
                    ui.span(icon_svg("trash-can"), " Delete Selected"),
                    class_="btn-sm btn-outline-danger mt-2"
                ),
                class_="mb-3"
            ),
            # Tables Section
            ui.div(
                ui.output_data_frame("tables_df"),
                ui.input_action_button(
                    "delete_tables",
                    ui.span(icon_svg("trash-can"), " Delete Selected"),
                    class_="btn-sm btn-outline-danger mt-2"
                ),
                class_="mb-3"
            ),
            # PSF Section
            ui.div(
                ui.output_data_frame("psf_df"),
                ui.input_action_button(
                    "delete_psf",
                    ui.span(icon_svg("trash-can"), " Delete Selected"),
                    class_="btn-sm btn-outline-danger mt-2"
                ),
                class_="mb-3"
            ),
            width="33%",
            height="100%",
            open={"desktop": "open", "mobile": "closed"},
        )
    return m

@module.server
def nav_table_sideview_server(input, output, session, photfun_client, samp_client):
    fits_data = reactive.Value(pd.DataFrame(columns=["FITS", "File"]))
    tables_data = reactive.Value(pd.DataFrame(columns=["Table", "File"]))
    psf_data = reactive.Value(pd.DataFrame(columns=["PSF", "File"]))
    
    def update_dataframes(fits=True, tables=True, psf=True):
        if fits:
            fits_data.set(pd.DataFrame([{"FITS": f.id, "File": f.alias} for f in photfun_client.fits_files]))
        if tables:    
            tables_data.set(pd.DataFrame([{"Table": t.id, "File": t.alias} for t in photfun_client.tables]))
        if psf:
            psf_data.set(pd.DataFrame([{"PSF": p.id, "File": p.alias} for p in photfun_client.psf_files]))

    # Render tables
    @render.data_frame
    def fits_df():
        return render.DataTable(fits_data.get(), height="200px", width="100%", selection_mode="row")

    @render.data_frame
    def tables_df():
        return render.DataTable(tables_data.get(), height="200px", width="100%", selection_mode="row")

    @render.data_frame
    def psf_df():
        return render.DataTable(psf_data.get(), height="200px", width="100%", selection_mode="row")

    # Delete handlers
    def create_delete_handler(button_id, list_ref, df_type):
        @reactive.Effect
        @reactive.event(button_id)
        def _():
            selected = input[f"{df_type}_df_selected_rows"]()
            if not selected:
                ui.notification_show(f"No {df_type} selected!", type="warning")
                return
                
            ui.modal_show(
                ui.modal(
                    ui.p(f"Are you sure you want to delete {len(selected)} {df_type} items?"),
                    ui.row(
                        ui.column(6, ui.input_action_button(f"confirm_delete_{df_type}", "Delete", class_="btn-danger w-100")),
                        ui.column(6, ui.input_action_button(f"cancel_delete_{df_type}", "Cancel", class_="btn-secondary w-100")),
                    ),
                    title="Confirm Deletion",
                    easy_close=True
                )
            )

        @reactive.Effect
        @reactive.event(getattr(input, f"confirm_delete_{df_type}"))
        def _():
            selected = input[f"{df_type}_df_selected_rows"]()
            if selected:
                for idx in sorted(selected, reverse=True):
                    if idx < len(list_ref):
                        del list_ref[idx]
                update_dataframes(**{df_type: True})
            ui.modal_remove()

        @reactive.Effect
        @reactive.event(getattr(input, f"cancel_delete_{df_type}"))
        def _():
            ui.modal_remove()

    # Create delete handlers for each type
    create_delete_handler(input.delete_fits, photfun_client.fits_files, "fits")
    create_delete_handler(input.delete_tables, photfun_client.tables, "tables")
    create_delete_handler(input.delete_psf, photfun_client.psf_files, "psf")

    # SAMP reconnect
    @reactive.Effect
    @reactive.event(input.reconnect_samp)
    def init_samp():
        samp_client.start_samp()
        return samp_client
        
    return update_dataframes, fits_df, tables_df, psf_df