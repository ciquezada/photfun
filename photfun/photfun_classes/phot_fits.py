import os
import glob
from astropy.io import fits
from .phot_file import PhotFile


class PhotFits(PhotFile):
    def __init__(self, path, *args, **kwargs):
        super().__init__(path, *args, **kwargs)  # Hereda la lógica de PhotFile
        self._image_cache = {}  # Diccionario para almacenar imágenes cargadas

    def image(self, index=0):
        """Carga y devuelve la imagen FITS del archivo en la posición `index`."""
        if index < 0 or index >= len(self.path):
            raise IndexError(f"Index {index} is out of range. Available files: 0 to {len(self.path) - 1}.")
        
        if index not in self._image_cache:  # Solo carga si no está en caché
            self._image_cache[index] = fits.open(self.path[index])[0]

        return self._image_cache[index]

    def file_info(self, indx=0):
        """Devuelve información básica del archivo en un diccionario."""
        info = {
            "Filename": os.path.basename(self.path[indx]),
            "File type": self.file_type,
            "File location": os.path.dirname(self.path[indx]),
        }

        try:
            with fits.open(self.path[indx]) as hdul:
                header = hdul[0].header  # Extrae el header de la extensión primaria
                info["Header"] = {key: header[key] for key in header.keys()}  # Convierte el header en diccionario
        except Exception as e:
            info["Error"] = str(e)

        return info


