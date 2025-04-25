import os
import re
import warnings
import argparse
import numpy as np
import pandas as pd
from tqdm import tqdm
import nest_asyncio
import asyncio


def mag2flux(mag):
    return 10**(-2/5 * (mag-25))

def read_als(filepath):
    col_names = ['ID', 'X', 'Y', "MAG", "merr", "msky", "niter", "chi", "sharpness"]
    try:
        return pd.read_csv(filepath, sep='\s+', skiprows=3, names=col_names, usecols=range(9), dtype={"ID": int})
    except FileNotFoundError:
        return None

def read_master(filepath):
    col_names = ['ID']
    try:
        return pd.read_csv(filepath, sep='\s+', skiprows=3, names=col_names, usecols=[0], dtype={"ID": int})
    except FileNotFoundError:
        return None

def parse_wavelength_from_name(filename):
    match = re.search(r"slice_(\d+p\d+)\.als", filename)
    if match:
        return float(match.group(1).replace('p', '.'))
    else:
        raise ValueError(f"No valid wavelength found in filename: {filename}")

def spectra_compile(als_dir, master_list, output_dir, pbar_extract=tqdm, pbar_save=tqdm):
    nest_asyncio.apply()
    os.makedirs(output_dir, exist_ok=True)
    star_ids = read_master(master_list)["ID"].astype(int).tolist()
    
    # Crear lista de paths de archivos .als
    if isinstance(als_dir, list):
        als_files = [
            f
            for f in als_dir
            if re.match(r".*_slice_\d+p\d+\.als", f)
        ]
    elif os.path.isdir(als_dir):
        als_files = [
            os.path.join(als_dir, f)
            for f in os.listdir(als_dir)
            if re.match(r".*_slice_\d+p\d+\.als", f)
        ]
    else:
        raise ValueError("Selected path is not a dir or even a list of .als")
    
    if not als_files:
        raise ValueError("No valid .als files found")

    # Extraer longitudes de onda y ordenar
    slice_info = sorted(
        [(parse_wavelength_from_name(os.path.basename(path)), path) for path in als_files],
        key=lambda x: x[0]
    )

    wavelengths = [w for w, _ in slice_info]
    spectra_data = {star_id: [np.nan] * len(wavelengths) for star_id in star_ids}
    errors_data = {star_id: [np.nan] * len(wavelengths) for star_id in star_ids}

    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    try:
        for idx, (wavelength, als_file) in pbar_extract([*enumerate(slice_info)]):
            als_data = read_als(als_file)
            if als_data is not None:
                als_data = als_data.set_index("ID")
                for star_id in star_ids:
                    if star_id in als_data.index:
                        spectra_data[star_id][idx] = als_data.loc[star_id, "MAG"]
                        errors_data[star_id][idx] = als_data.loc[star_id, "merr"]
    finally:
        loop.close()

    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    try:
        for star_id in pbar_save(star_ids):
            spectra_flux = mag2flux(np.array(spectra_data[star_id]))
            spectra_flux_sig = mag2flux(np.array(spectra_data[star_id]) - np.array(errors_data[star_id])) - spectra_flux
            df = pd.DataFrame({
                "wavelength": wavelengths,
                "flux": spectra_flux,
                "flux_sig": spectra_flux_sig
            })
            df.to_csv(os.path.join(output_dir, f"id{star_id}.csv"), index=False)
    finally:
        loop.close()
    return 0
    
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Extract spectra from ALS files using encoded wavelengths in filenames")
    parser.add_argument("-a", "--als_dir", type=str, required=True, help="Directory containing .als files")
    parser.add_argument("-m", "--master_list", type=str, required=True, help="CSV file with star IDs")
    parser.add_argument("-o", "--output_dir", type=str, required=True, help="Output directory for spectra CSV files")

    args = parser.parse_args()
    spectra_compile(args.als_dir, args.master_list, args.output_dir)
