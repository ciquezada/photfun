#!/usr/bin/env python3

import os
import sys
import argparse
import warnings
import numpy as np
from tqdm import tqdm
from astropy.io import fits
from astropy.wcs import WCS
import nest_asyncio
import asyncio


def find_valid_hdu(fits_path):
    """Finds the first HDU in the FITS file that contains 3D data."""
    with fits.open(fits_path) as hdul:
        for i, hdu in enumerate(hdul):
            if hasattr(hdu, 'data') and hdu.data is not None and hdu.data.ndim == 3:
                return i  # Return the first valid HDU index
    raise ValueError(f"No valid 3D data found in {fits_path}")

def cube_slicer(fits_path, pbar=tqdm):
    nest_asyncio.apply()
    """Loads a 3D FITS cube, extracts each slice, and saves them as individual files."""

    # Check if the file exists
    if not os.path.exists(fits_path):
        print(f"Error: The file '{fits_path}' does not exist.")
        sys.exit(1)

    # Find the correct HDU with 3D data
    hdu_index = find_valid_hdu(fits_path)

    # Load the FITS cube
    with fits.open(fits_path) as hdul:
        data = hdul[hdu_index].data
        header = hdul[hdu_index].header
        # Get wavelength values from WCS
        wcs = WCS(header, naxis=0)
        num_slices = data.shape[0]
        coords = np.zeros((num_slices, 3))
        coords[:, 2] = np.arange(num_slices)
        wavelengths = wcs.all_pix2world(coords, 2)[:, 2]*1e10

    # Ensure the FITS file is a 3D cube
    if data.ndim != 3:
        print(f"Error: The file '{fits_path}' is not a 3D cube.")
        sys.exit(1)

    # Create output directory
    base_name = os.path.splitext(os.path.basename(fits_path))[0]
    output_dir = os.path.join(os.path.dirname(fits_path), f"{base_name}_slices")
    os.makedirs(output_dir, exist_ok=True)

    # num_slices = data.shape[0]  # Spectral axis is the first dimension

    # Save each slice as a separate FITS file using wavelength
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    try:
        for i in pbar(range(data.shape[0])):
            slice_data = data[i, :, :]
            slice_header = header.copy()
            slice_header["SLICE"] = i
            wavelength = wavelengths[i]
            wavelength_str = f"{wavelength:.5f}".replace('.', 'p')

            slice_filename = os.path.join(output_dir, f"{base_name}_slice_{wavelength_str}.fits")

            # Save the slice
            fits.writeto(slice_filename, slice_data, slice_header, overwrite=True)
    finally:
        loop.close()
    return output_dir

def main():
    parser = argparse.ArgumentParser(
        description="Splits a 3D FITS cube into individual slices without using SpectralCube."
    )
    parser.add_argument(
        "fits_path",
        type=str,
        help="Path to the FITS cube file."
    )

    args = parser.parse_args()
    cube_slicer(args.fits_path)

if __name__ == "__main__":
    main()
