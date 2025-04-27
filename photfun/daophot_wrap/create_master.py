from ..misc_tools import check_file, temp_mkdir, move_file_noreplace
import os
import tempfile
import shutil
from astropy.io import ascii
import pandas as pd
import numpy as np


def create_master(in_master_als, in_mch, out_dir, verbose=True): 
    try:
        # Copiar archivos necesarios a la carpeta temporal
        master_name = os.path.splitext(os.path.basename(in_master_als))[0]
        master_ext = os.path.splitext(os.path.basename(in_master_als))[-1].lower().lstrip('.')
        # Crear carpeta temporal
        temp_dir = os.path.abspath(temp_mkdir(f"{master_name}_PHOTMASTER_0"))
        temp_master = os.path.join(temp_dir, os.path.basename(in_master_als))
        temp_mch = os.path.join(temp_dir, os.path.basename(in_mch))

        shutil.copy(in_master_als, temp_master)
        shutil.copy(in_mch, temp_mch)

        if verbose:
            print(f"photfun: create_master({master_name})")
        check_file(temp_mch, "mch file input: ")
        mch = load_mch(temp_mch).iloc[1:]
        check_file(temp_master, "master file input: ")
        master_df, master_header = load_table(temp_master)
        xv = master_df['X'].values
        yv = master_df['Y'].values
        temp_out_path_list = [os.path.join(temp_dir, file_name) 
                                for file_name in mch.FILE.values]
        out_path_list = [os.path.join(out_dir, 
        f"{master_name}_{os.path.splitext(os.path.basename(file_name))[0]}.{master_ext}") 
                                for file_name in mch.FILE.values]

        #name the constants and prepare the arrays
        final_out_path_list=[]
        for temp_out_path, out_path in zip(temp_out_path_list, out_path_list):
            mch_row = mch.loc[mch.FILE==os.path.basename(temp_out_path)]
            a, b, c, d, e, f = mch_row['A'].iloc[0], mch_row['B'].iloc[0], mch_row['C'].iloc[0], \
                                mch_row['D'].iloc[0], mch_row['E'].iloc[0], mch_row['F'].iloc[0]
            A = np.array([[c, e], [d, f]])
            b = np.array([xv - a, yv - b])
            x_new, y_new = np.linalg.solve(A, b)

            new_df = master_df.copy()
            new_df["X"] = x_new
            new_df["Y"] = y_new
            save_table(temp_out_path, new_df, master_header)
            final_out_path = move_file_noreplace(temp_out_path, out_path)
            final_out_path_list.append(final_out_path)
            if verbose:
                print(f"  -> {final_out_path}")

    finally:
        # Limpiar carpeta temporal
        shutil.rmtree(temp_dir)
        pass

    return final_out_path_list

def save_table(file_path, df, header):
    if header:  # Si hay header, guardarlo
        with open(file_path, "w") as f:
            # Escribir la primera línea con los nombres, respetando los anchos de columna
            f.write(header)

            # Formatear los datos con alineación fija
            ext = os.path.splitext(file_path)[-1].lower().lstrip('.')
            if ext=="ap":
                f.write("\n")
                for _, row in df.iterrows():
                    f.write(f"{int(row['ID']):>7} " + 
                        " ".join(f"{row[col]:>8.3f}" for col in ["X", "Y", "AP1", "AP2", "AP3"]) +
                             "\n")
                    f.write(f"{int(row['SKY']):>14.3f} " +
                        " ".join(f"{row[col]:>5.2f}" for col in ["SKY_err", "SKY_skew"]) +
                        " ".join(f"{row[col]:>8.4f}" for col in ["AP1_err", "AP2_err", "AP3_err"]) +
                             "\n")         
            else:
                for _, row in df.iterrows():
                    f.write(f"{int(row['ID']):>7} " + 
                            " ".join(f"{row[col]:>8.3f}" for col in df.columns[1:]) + "\n")
    else:
        df.to_csv(file_path, sep=" ", index=False)

def load_mch(file_path):
    table = ascii.read(file_path, format='no_header', delimiter=' ', quotechar="'")
    columns = ['FILE', 'A', 'B', 'C', 'D', 'E', 'F', 'IDK1', 'IDK2']
    df_mch = table.to_pandas()
    df_mch.columns = columns
    return df_mch

def load_als(file_path):
    """Carga un archivo .als en un DataFrame."""
    with open(file_path, "r") as f:
        lines = f.readlines()

    # Extraer header
    header_keys = lines[0].split()
    header_values = lines[1].split()
    header = f"{header_keys[0]:>3} {header_keys[1]:>5} {header_keys[2]:>5} " + \
                " ".join(f"{k:>7}" for k in header_keys[3:]) + "\n"
    header += f"{header_values[0]:>3} {header_values[1]:>5} {header_values[2]:>5} " + \
                " ".join(f"{v:>7}" for v in header_values[3:]) + "\n"
    header += "\n"
    
    col_names = ['ID', 'X', 'Y', "MAG", "merr", "msky", "niter", "chi", "sharpness"]
    df = pd.read_csv(file_path, sep='\s+', skiprows=3, names=col_names, usecols=range(9), dtype={"ID": int})
    df.iloc[:, 1:-1] = df.iloc[:, 1:-1].astype(float)
    return df, header

def load_coo(file_path):
    """Carga un archivo .coo o lst en un DataFrame."""
    with open(file_path, "r") as f:
        lines = f.readlines()

    # Extraer header
    header_keys = lines[0].split()
    header_values = lines[1].split()
    header = f"{header_keys[0]:>3} {header_keys[1]:>5} {header_keys[2]:>5} " + \
                " ".join(f"{k:>7}" for k in header_keys[3:]) + "\n"
    header += f"{header_values[0]:>3} {header_values[1]:>5} {header_values[2]:>5} " + \
                " ".join(f"{v:>7}" for v in header_values[3:]) + "\n"
    header += "\n"
    
    col_names = ["ID", "X", "Y", "coo_MAG"]
    df = pd.read_csv(file_path, sep='\s+', skiprows=3, names=col_names, usecols=range(4), dtype={"ID": str})
    df.iloc[:, 1:] = df.iloc[:, 1:].astype(float)
    return df, header

def load_ap(file_path):
    """Carga un archivo .ap o lst en un DataFrame."""
    with open(file_path, "r") as f:
        lines = f.readlines()

    # Extraer header
    header_keys = lines[0].split()
    header_values = lines[1].split()
    header = f"{header_keys[0]:>3} {header_keys[1]:>5} {header_keys[2]:>5} " + \
                " ".join(f"{k:>7}" for k in header_keys[3:]) + "\n"
    header += f"{header_values[0]:>3} {header_values[1]:>5} {header_values[2]:>5} " + \
                " ".join(f"{v:>7}" for v in header_values[3:]) + "\n"
    header += "\n"
    
    col_names = ["ID", "X", "Y", "AP1", "AP2", "AP3", "SKY", "SKY_err", "SKY_skew", "AP1_err", "AP2_err", "AP3_err"]
    data = []
    for i in range(4, len(lines), 3):
        line1 = lines[i].split()
        line2 = lines[i + 1].split()
        if len(line1) == 6 and len(line2) == 6:
            data.append(line1 + line2)
    
    df = pd.DataFrame(data, columns=col_names)
    df = df.astype({col: float for col in col_names if col != "ID"})
    return df, header

def load_table(file_path):
    """Carga un archivo en un DataFrame según su extensión."""
    ext = os.path.splitext(file_path)[-1].lower().lstrip('.')
    loaders = {
        'mch': load_mch,
        'als': load_als,
        'lst': load_coo,
        'coo': load_coo,
        'ap': load_ap,
    }
    if ext in loaders:
        return loaders[ext](file_path)
    else:
        raise ValueError(f"Formato de archivo no soportado: {ext}")