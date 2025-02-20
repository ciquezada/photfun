import os
import glob
from astropy.io import fits


class PhotFits:
    _id_counter = 0
    
    def __init__(self, path):
        self.id = PhotFits._id_counter
        PhotFits._id_counter += 1
        
        if isinstance(path, list):
            self.file_type = "fits_list"
            self.path_list = sorted(path)
            self.alias = f"[dir] {os.path.basename(self.path_list[0])}"
            self.path = self.path_list[0]
        elif os.path.isdir(path):
            self.file_type = "fits_dir"
            self.path_list = sorted(glob.glob(os.path.join(path, "*.fits")))
            self.alias = f"[dir] {os.path.basename(path)}"
            self.path = self.path_list[0]
        else:
            self.file_type = "fits"
            self.path_list = None
            self.alias = os.path.basename(path)
            self.path = os.path.abspath(path)

        self._image = None
    
    @property
    def image(self):
        if self._image is None and self.path_list:
            self._image = fits.open(self.path_list[0])
        elif self._image is None:
            self._image = fits.open(self.path)
        return self._image
    
    @classmethod
    def reset_id_counter(cls):
        """Resetea el contador de ID a cero."""
        cls._id_counter = 0