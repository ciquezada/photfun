import os
from astropy.io import fits


class PhotFits:
    _id_counter = 0
    
    def __init__(self, path):
        self.id = PhotFits._id_counter
        PhotFits._id_counter += 1
        
        self.path = os.path.abspath(path)
        self.alias = os.path.basename(path)
        self._image = None  # Se inicializa como None

    @property
    def image(self):
        if self._image is None:
            self._image = fits.open(self.path)
        return self._image
