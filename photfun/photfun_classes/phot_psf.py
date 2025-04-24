from .phot_file import PhotFile
import os


class PhotPSF(PhotFile):
    def __init__(self, path, *args, **kwargs):
        super().__init__(path, *args, **kwargs)