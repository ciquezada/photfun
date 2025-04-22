import os
import warnings
import argparse
import numpy as np
import pandas as pd
from tqdm import tqdm
from astropy.io import fits
from astropy.wcs import WCS

def find_valid_hdu(fits_path):
    """Finds the first HDU in the FITS file that contains 3D data."""
    with fits.open(fits_path) as hdul:
        for i, hdu in enumerate(hdul):
            if hasattr(hdu, 'data') and hdu.data is not None and hdu.data.ndim == 3:
                return i  # Return the first valid HDU index
    raise ValueError(f"No valid 3D data found in {fits_path}")

def mag2flux(mag):
    return 10**(-2/5 * (mag-25))

def read_als(filepath):
    col_names = ['ID', 'X', 'Y', "MAG", "merr", "msky", "niter", "chi", "sharpness"]
    try:
        return pd.read_csv(filepath, sep='\s+', skiprows=3, names=col_names, usecols=range(9), dtype={"ID": int})
    except FileNotFoundError:
        return None

def extract_spectra(als_dir, master_list, cube_fits, output_dir):
    os.makedirs(output_dir, exist_ok=True)
    star_ids = pd.read_csv(master_list)["ID"].astype(int).tolist()
    # Find the correct HDU with 3D data
    hdu_index = find_valid_hdu(fits_path)
    
    with fits.open(cube_fits) as hdul:
        header = hdul[hdu_index].header
        data_shape = hdul[hdu_index].data.shape
        wcs = WCS(header, naxis=1)
        wavelengths = wcs.all_pix2world(np.arange(data_shape[0]), 0)[0]
    
    first_slice, last_slice = 0, data_shape[0]-1
    
    spectra_data = {star_id: [np.nan] * (last_slice - first_slice + 1) for star_id in star_ids}
    errors_data = {star_id: [np.nan] * (last_slice - first_slice + 1) for star_id in star_ids}
    
    for slice_i in tqdm(range(first_slice, last_slice+1), desc='Extracting spectra'):
        als_file = os.path.join(als_dir, f"slice_{slice_i:04d}.als")
        als_data = read_als(als_file)
        
        if als_data is not None:
            als_data = als_data.set_index("ID")
            for star_id in star_ids:
                if star_id in als_data.index:
                    spectra_data[star_id][slice_i - first_slice] = als_data.loc[star_id, "MAG"]
                    errors_data[star_id][slice_i - first_slice] = als_data.loc[star_id, "merr"]
    
    for star_id in tqdm(star_ids, desc='Saving spectra'):
        spectra_flux = mag2flux(np.array(spectra_data[star_id]))
        spectra_flux_sig = mag2flux(np.array(spectra_data[star_id]) - np.array(errors_data[star_id])) - spectra_flux
        
        df = pd.DataFrame({"wavelength": wavelengths, "flux": spectra_flux, "flux_sig": spectra_flux_sig})
        df.to_csv(os.path.join(output_dir, f"id{star_id}.csv"), index=False)
    
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Extract spectra from ALS files and spectral cube")
    parser.add_argument("-a", "--als_dir", type=str, required=True, help="Directory containing .als files")
    parser.add_argument("-m", "--master_list", type=str, required=True, help="CSV file with star IDs")
    parser.add_argument("-c", "--cube_fits", type=str, required=True, help="Path to spectral cube FITS file")
    parser.add_argument("-o", "--output_dir", type=str, required=True, help="Output directory for spectra CSV files")
    
    args = parser.parse_args()
    extract_spectra(args.als_dir, args.master_list, args.cube_fits, args.output_dir)
