import os
from urllib.parse import urlparse

def is_votable_url(url):
    parsed = urlparse(url)
    if not all([parsed.scheme in ['http', 'https'], parsed.netloc]):
        return False
    return parsed.path.lower()

class PhotFile:
    def __init__(self, path, alias=None, *args, **kwargs):
        self.path = None
        if isinstance(path, list):  # Lista explícita de archivos
            self.path = path
        elif os.path.isdir(path):  # Es un directorio, obtenemos todos los archivos
            self.path = sorted([
                os.path.join(path, f) for f in os.listdir(path)
                if os.path.isfile(os.path.join(path, f))
            ], key=lambda f: f.lower())
        elif os.path.isfile(path):  # Es un único archivo, lo convertimos en una lista
            self.path = [path]
        elif is_votable_url(path).endswith('.vot') or is_votable_url(path).endswith('.fits'): 
                self.path = [path]       
        else:
            raise ValueError(f"Invalid path: {path}")

        if not self.path or not len(self.path):
            raise ValueError(f"The directory '{path}' is empty or contains no valid files.")

        # Alias: Si hay varios archivos, usa el primero; si es un archivo único, usa su nombre.
        self.alias = f"[>] {os.path.basename(self.path[0])}" if len(self.path) > 1 else os.path.basename(self.path[0])
        if alias:
            self.alias = alias

        # Validación de extensiones
        extensions = {os.path.splitext(f)[1].lower() for f in self.path}
        if len(extensions) > 1:
            raise ValueError(f"Multiple file types detected: {extensions}. All files must have the same extension.")

        self.file_type = list(extensions)[0]

        self.header = None

