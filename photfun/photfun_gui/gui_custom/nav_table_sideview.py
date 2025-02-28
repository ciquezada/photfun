from shiny import module, reactive, render, ui
import pandas as pd

@module.ui
def nav_table_sideview_ui():
    m = ui.sidebar(
            ui.h4("Loaded Data"),
            ui.output_data_frame("fits_df"),
            ui.output_data_frame("tables_df"),
            ui.output_data_frame("psf_df"),
            width="33%",
            height="100%",
            open={"desktop": "open", "mobile": "closed"},
        )
    return m

@module.server
def nav_table_sideview_server(input, output, session, photfun_client):
    # Inicializar DataFrames reactivos
    fits_data = reactive.Value(pd.DataFrame(columns=["FITS", "File"]))
    tables_data = reactive.Value(pd.DataFrame(columns=["Table", "File"]))
    psf_data = reactive.Value(pd.DataFrame(columns=["PSF", "File"]))
    
    # Función para actualizar los DataFrames reactivos
    def update_dataframes(fits=True, tables=True, psf=True):
        if fits:
            fits_data.set(pd.DataFrame([{"FITS": f.id, "File": f.alias} for f in photfun_client.fits_files]))
        if tables:    
            tables_data.set(pd.DataFrame([{"Table": t.id, "File": t.alias} for t in photfun_client.tables]))
        if psf:
            psf_data.set(pd.DataFrame([{"PSF": p.id, "File": p.alias} for p in photfun_client.psf_files]))

    # Renderizado de las tablas
    @render.data_frame
    def fits_df():
        return render.DataTable(
                    fits_data.get(),
                    height="200px",
                    width="100%",
                    selection_mode="row"
                )

    @render.data_frame
    def tables_df():
        return render.DataTable(
                    tables_data.get(),
                    height="200px",
                    width="100%",
                    selection_mode="row"
                )

    @render.data_frame
    def psf_df():
        return render.DataTable(
                    psf_data.get(),
                    height="200px",
                    width="100%",
                    selection_mode="row"
                )

    return update_dataframes, fits_df, tables_df, psf_df