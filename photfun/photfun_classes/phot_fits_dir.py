import os
import glob


class PhotFitsDir:
    _id_counter = 0
    
    def __init__(self, path):
        self.id = PhotFitsDir._id_counter
        PhotFitsDir._id_counter += 1
        
        self.path = os.path.abspath(path)
        self.alias = os.path.basename(path)
        self.fits_files = self._load_fits_files()
    
    def _load_fits_files(self):
        return [PhotFits(f) for f in glob.glob(os.path.join(self.path, '*.fits'))]

    @classmethod
    def reset_id_counter(cls):
        """Resetea el contador de ID a cero."""
        cls._id_counter = 0