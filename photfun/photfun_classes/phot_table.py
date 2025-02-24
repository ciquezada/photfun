import pandas as pd
import os
from astropy.io import fits, ascii
from astropy.table import Table


class PhotTable:
    def __init__(self, path):
        if isinstance(path, list):  # Si es una carpeta
            self.file_type = "list"
            self.path_list = sorted(path)
            indv_path = self.path_list[0] if self.path_list else None
            self.alias = f"[>] {os.path.basename(indv_path)}"
            self.path = os.path.abspath(indv_path)
            self.table_type = self._get_table_type(indv_path)
            
        elif os.path.isdir(path):  # Si es una lista de archivos
            self.file_type = "dir"
            self.path_list = sorted([
                                    os.path.join(path, f) for f in os.listdir(path)
                                    if os.path.isfile(os.path.join(path, f))
                                ])
            self.alias = f"[>] {os.path.basename(path)}"
            self.path = os.path.abspath(path)
            self.table_type = self._get_table_type(self.path_list[0]) if self.path_list else None
        elif os.path.isfile(path):  # Si es un archivo único
            self.file_type = "file"
            self.path_list = None
            self.alias = os.path.basename(path)
            self.path = os.path.abspath(path)
            self.table_type = self._get_table_type(path)
        else:  # Manejo de error si el path no es válido
            raise ValueError(f"Invalid path: {path}")
        # Verificar que todos los archivos tienen la misma extensión
        if self.path_list:
            extensions = {os.path.splitext(f)[1].lower() for f in self.path_list}
            if len(extensions) > 1:
                raise ValueError(f"Multiple file types detected: {extensions}. "
                                "All files must have the same extension.")
        elif self.file_type in ["list", "dir"]:
            raise ValueError(f"The directory '{path}' is empty.")
        self.header = None
        self.df = self._load_table()

    def _get_table_type(self, file_path):
        ext = os.path.splitext(file_path)[-1].lower()
        return ext.lstrip('.') if ext else 'unknown'
    
    def _load_table(self):
        if self.table_type in ['csv']:
            return pd.read_csv(self.path)
        elif self.table_type in ['fits', 'fit']:
            with fits.open(self.path) as hdul:
                return pd.DataFrame(hdul[1].data) if len(hdul) > 1 else None
        elif self.table_type in ['vot', 'xml']:
            table = Table.read(self.path, format='votable')
            return table.to_pandas()
        elif self.table_type in ['coo', 'lst', 'nei']:
            table = self._load_coord()
            return table
        elif self.table_type in ['ap']:
            table = self._load_ap()
            return table
        elif self.table_type in ['als']:
            table = self._load_als()
            return table
        else:
            table = ascii.read(self.path)
            return table.to_pandas()

    def _load_coord(self):
        with open(self.path, "r") as f:
            lines = f.readlines()
        
        # Extraer header
        header_keys = lines[0].split()
        header_values = lines[1].split()
        self.header = {key: val for key, val in zip(header_keys, header_values)}
        
        # Cargar datos
        col_names = ["ID", "X", "Y", "coo_MAG"]
        df = pd.read_csv(self.path, sep='\s+', skiprows=3, names=col_names, usecols=range(4), dtype={"ID": str})
        df.iloc[:, 1:] = df.iloc[:, 1:].astype(float)  # Convertir todas las columnas excepto ID
        return df

    def _load_als(self):
        with open(self.path, "r") as f:
            lines = f.readlines()
        
        # Extraer header
        header_keys = lines[0].split()
        header_values = lines[1].split()
        self.header = {key: val for key, val in zip(header_keys, header_values)}
        
        # Cargar datos
        col_names = ['ID', 'X', 'Y', "MAG", "merr", "msky", "niter", "chi", "sharpness"]
        df = pd.read_csv(self.path, sep='\s+', skiprows=3, names=col_names, usecols=range(9), dtype={"ID": str, "niter": str})
        df.iloc[:, 1:-1] = df.iloc[:, 1:-1].astype(float)  # Convertir todas las columnas excepto ID y niter
        return df

    def _load_ap(self):
        with open(self.path, "r") as f:
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