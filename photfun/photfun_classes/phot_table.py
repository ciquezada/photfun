import pandas as pd
import os
from astropy.io import fits, ascii
from astropy.table import Table


class PhotTable:
    _id_counter = 0
    
    def __init__(self, path):
        self.id = PhotTable._id_counter
        PhotTable._id_counter += 1

        if isinstance(path, list):
            self.file_type = "fits_list"
            self.path_list = sorted(path)
            self.alias = f"[dir] {os.path.basename(self.path_list[0])}"
            self.path = os.path.abspath(self.path_list[0])
            self.table_type = self._get_table_type(self.path_list[0])
        else:
            self.file_type = "fits"
            self.path_list = None
            self.alias = os.path.basename(path)
            self.path = os.path.abspath(path)
            self.table_type = self._get_table_type(path)

        self.header = None
        self.df = self._load_table()

    
    def _get_table_type(self, path):
        ext = os.path.splitext(path)[-1].lower()
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
        # header
        header_keys = lines[0].split()
        header_values = lines[1].split()
        self.header = {key: val for key, val in zip(header_keys, header_values)}
        col_names = ["ID", "X", "Y", "coo_MAG"]
        df = pd.read_csv(self.path, sep='\s+', skiprows=3, names=col_names, usecols=range(4))
        return df

    def _load_als(self):
        with open(self.path, "r") as f:
            lines = f.readlines()
        # header
        header_keys = lines[0].split()
        header_values = lines[1].split()
        self.header = {key: val for key, val in zip(header_keys, header_values)}
        col_names = ['ID', 'X', 'Y', "MAG", "merr", "msky", "niter", "chi", "sharpness"]
        df = pd.read_csv(self.path, sep='\s+', skiprows=3, names=col_names, usecols=range(9))
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
            line1 = list(map(lambda x: x, lines[i].split()))
            line2 = list(map(lambda x: x, lines[i + 1].split()))
            if len(line1) == 6 and len(line2) == 6:
                data.append(line1 + line2)

        return pd.DataFrame(data, columns=col_names)

    @classmethod
    def reset_id_counter(cls):
        """Resetea el contador de ID a cero."""
        cls._id_counter = 0