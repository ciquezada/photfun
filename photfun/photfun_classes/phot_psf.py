import os


class PhotPSF:
    _id_counter = 0
    
    def __init__(self, path):
        self.id = PhotPSF._id_counter
        PhotPSF._id_counter += 1
        
        if isinstance(path, list):
            self.file_type = "psf_list"
            self.path_list = sorted(path)
            self.alias = f"[dir] {os.path.basename(self.path_list[0])}"
            self.path = os.path.abspath(self.path_list[0])
        else:
            self.file_type = "psf"
            self.path_list = None
            self.alias = os.path.basename(path)
            self.path = os.path.abspath(path)

    @classmethod
    def reset_id_counter(cls):
        """Resetea el contador de ID a cero."""
        cls._id_counter = 0