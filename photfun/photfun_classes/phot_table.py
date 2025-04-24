from .phot_file import PhotFile
import pandas as pd
import os
from astropy.io import fits, ascii
from astropy.table import Table


class PhotTable(PhotFile):
    def __init__(self, path, *args, **kwargs):
        super().__init__(path, *args, **kwargs)  # Inicializa PhotFile y obtiene self.path y self.file_type
        self.header = None
        self.info = {}

    def df(self, indx=0):
        """Carga la tabla correspondiente cada vez que se accede a `df`."""
        return self._load_table(indx)

    def _load_table(self, indx):
        """Carga la tabla según la extensión del archivo."""
        if self.file_type == ".csv":
            return pd.read_csv(self.path[indx])
        elif self.file_type in [".fits", ".fit"]:
            with fits.open(self.path[indx]) as hdul:
                return pd.DataFrame(hdul[1].data) if len(hdul) > 1 else None
        elif self.file_type in [".vot", ".xml"]:
            return Table.read(self.path[indx], format="votable").to_pandas()
        elif self.file_type in [".coo", ".lst", ".nei"]:
            return self._load_coord(indx)
        elif self.file_type == ".ap":
            return self._load_ap(indx)
        elif self.file_type == ".als":
            return self._load_als(indx)
        elif self.file_type == ".mch":
            return self._load_mch(indx)
        else:
            return ascii.read(self.path[indx]).to_pandas()

    def _load_coord(self, indx):
        with open(self.path[indx], "r") as f:
            lines = f.readlines()
        
        # Extraer header
        header_keys = lines[0].split()
        header_values = lines[1].split()
        self.header = {key: val for key, val in zip(header_keys, header_values)}
        
        # Cargar datos
        col_names = ["ID", "X", "Y", "coo_MAG"]
        df = pd.read_csv(self.path[indx], sep='\s+', skiprows=3, names=col_names, usecols=range(4), dtype={"ID": str})
        df.iloc[:, 1:] = df.iloc[:, 1:].astype(float)  # Convertir todas las columnas excepto ID
        return df

    def _load_als(self, indx):
        with open(self.path[indx], "r") as f:
            lines = f.readlines()
        
        # Extraer header
        header_keys = lines[0].split()
        header_values = lines[1].split()
        self.header = {key: val for key, val in zip(header_keys, header_values)}
        
        # Cargar datos
        col_names = ['ID', 'X', 'Y', "MAG", "merr", "msky", "niter", "chi", "sharpness"]
        df = pd.read_csv(self.path[indx], sep='\s+', skiprows=3, names=col_names, usecols=range(9), dtype={"ID": int})
        df.iloc[:, 1:-1] = df.iloc[:, 1:-1].astype(float)  # Convertir todas las columnas excepto ID y niter
        return df

    def _load_ap(self, indx):
        with open(self.path[indx], "r") as f:
            lines = f.readlines()

        # Extraer header
        header_keys = lines[0].split()
        header_values = lines[1].split()
        self.header = {key: float(val) for key, val in zip(header_keys, header_values)}

        # Extraer datos en bloques de 3 líneas
        data = []
        col_names = ["ID", "X", "Y", "AP1", "AP2", "AP3", "SKY", "SKY_err", "SKY_skew", "AP1_err", "AP2_err", "AP3_err"]

        for i in range(4, len(lines), 3):
            line1 = lines[i].split()
            line2 = lines[i + 1].split()
            if len(line1) == 6 and len(line2) == 6:
                data.append(line1 + line2)

        df = pd.DataFrame(data, columns=col_names)
        df = df.astype({col: float for col in col_names if col != "ID"})  # Convertir todas menos ID a float
        return df

    def _load_mch(self, indx):
        table = ascii.read(self.path[indx], format='no_header', delimiter=' ', quotechar="'")
        columns = ['FILE', 'A', 'B', 'C', 'D', 'E', 'F', 'IDK1', 'IDK2']
        df_mch = table.to_pandas()
        df_mch.columns = columns
        return df_mch    

    def subtable(self, out_path, selected_ids):
        # Filtrar la tabla usando los IDs seleccionados
        sub_df = self.df(0)[self.df(0)["ID"].isin(selected_ids)].copy()

        if sub_df.empty:
            raise ValueError("No matching IDs found in the table.")

        # Guardar en el mismo formato que el archivo original
        if self.file_type == ".csv":
            sub_df.to_csv(out_path, index=False)
        elif self.file_type in [".fits", ".fit"]:
            hdu = fits.BinTableHDU(Table.from_pandas(sub_df))
            hdu.writeto(out_path, overwrite=True)
        elif self.file_type in [".vot", ".xml"]:
            table = Table.from_pandas(sub_df)
            table.write(out_path, format="votable", overwrite=True)
        else:  # Archivos separados por espacios o ASCII
            if self.header:  # Si hay header, guardarlo
                with open(out_path, "w") as f:
                    keys = list(self.header.keys())
                    values = list(self.header.values())

                    # Escribir la primera línea con los nombres, respetando los anchos de columna
                    f.write(f"{keys[0]:>3} {keys[1]:>5} {keys[2]:>5} " + 
                            " ".join(f"{k:>7}" for k in keys[3:]) + "\n")

                    # Escribir la segunda línea con los valores alineados
                    f.write(f"{values[0]:>3} {values[1]:>5} {values[2]:>5} " + 
                            " ".join(f"{v:>7}" for v in values[3:]) + "\n")

                    f.write("\n")  # Línea en blanco entre el header y los datos
                # sub_df.to_csv(out_path, sep=" ", index=False, header=False, mode="a")
                # Formatear los datos con alineación fija
                    for _, row in sub_df.iterrows():
                        f.write(f"{int(row['ID']):>7} {row['X']:>8.3f} {row['Y']:>8.3f} " + 
                                " ".join(f"{row[col]:>8.3f}" for col in sub_df.columns[3:]) + "\n")
            else:
                sub_df.to_csv(out_path, sep=" ", index=False)

    def file_info(self, indx=0):
        """Devuelve información básica del archivo en un diccionario."""
        info = {
            "Filename": os.path.basename(self.path[indx]),
            "File type": self.file_type,
            "File location": os.path.dirname(self.path[indx]),
            "Num Rows": 0,
            "Num Columns": 0,
            "Columns": []
        }

        try:
            df = self.df(indx)
            if df is not None:
                info["Num Rows"] = len(df)
                info["Num Columns"] = len(df.columns)
                info["Columns"] = list(df.columns)
        except Exception as e:
            info["Error"] = str(e)

        return info