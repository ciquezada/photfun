from .phot_file import PhotFile
import os
import re
import numpy as np
import base64
from io import BytesIO
from PIL import Image


class PhotPSF(PhotFile):
    def __init__(self, path, *args, **kwargs):
        super().__init__(path, *args, **kwargs)
        self.preview_path = None

    def preview_plot(self, indx):
        # 1. Obtén la ruta
        if not self.preview_path:
            return None
        png_path = self.preview_path[indx]


        # 2. Abre la imagen
        with Image.open(png_path) as img:
            # 3. Guarda en buffer como PNG
            buf = BytesIO()
            img.save(buf, format='PNG')
            buf.seek(0)

        # 4. Codifica en Base64 y retorna
        b64_str = base64.b64encode(buf.getvalue()).decode('ascii')
        return b64_str

    def model(self, indx=0):
        """Carga la tabla correspondiente cada vez que se accede a `df`."""
        return self._load_psf(indx)

    def _load_psf(self, indx):
        """Carga un archivo PSF de DAOPHOT y devuelve la tabla de corrección."""
        with open(self.path[indx], 'r') as f:
            lines = f.readlines()
        header_parts = lines[0].strip().split()
        table_size = int(header_parts[1])

        # Extraer sólo números científicos de las líneas de la tabla
        pattern = r'[+-]?\d+\.\d+E[+-]?\d+'
        vals = []
        for line in lines[2:]:
            vals.extend([float(x) for x in re.findall(pattern, line)])
        table = np.array(vals).reshape((table_size, table_size))

        return table

    def file_info(self, indx=0):
        """Devuelve información básica del PSF en un diccionario."""
        info = {
            "Filename":       os.path.basename(self.path[indx]),
            "File location":  os.path.dirname(self.path[indx]),
            "File type":      self.file_type,
        }

        # Leer las dos primeras líneas para extraer parámetros
        try:
            with open(self.path[indx], 'r') as f:
                lines = f.readlines()

            header_parts = lines[0].strip().split()
            shape_parts  = [float(v) for v in lines[1].strip().split()]

            psf_data = {
                'model':           header_parts[0],
                'table_size':      int(header_parts[1]),
                'n_shape_params':  int(header_parts[2]),
                'n_tables':        int(header_parts[3]),
                'frac_pixel_exp':  int(header_parts[4]),
                'inst_mag':        float(header_parts[5]),
                'central_height':  float(header_parts[6]),
                'x_center':        float(header_parts[7]),
                'y_center':        float(header_parts[8]),
                'shape_params (HWHM)':    shape_parts,
            }

            info.update(psf_data)

        except Exception as e:
            info["Error"] = f"Cannot parse PSF file: {e}"

        return info