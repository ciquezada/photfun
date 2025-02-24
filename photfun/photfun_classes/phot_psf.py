import os


class PhotPSF:
    def __init__(self, path):
        if os.path.isdir(path):  # Si es una carpeta
            self.file_type = "dir"
            self.path_list = sorted([
                                    os.path.join(path, f) for f in os.listdir(path)
                                    if os.path.isfile(os.path.join(path, f))
                                ])
            self.alias = f"[>] {os.path.basename(path)}"
            self.path = os.path.abspath(path)
        elif isinstance(path, list):  # Si es una lista de archivos
            self.file_type = "list"
            self.path_list = sorted(path)
            indv_path = self.path_list[0] if self.path_list else None
            self.alias = f"[>] {os.path.basename(indv_path)}"
            self.path = os.path.abspath(indv_path)
        elif os.path.isfile(path):  # Si es un archivo único
            self.file_type = "file"
            self.path_list = None
            self.alias = os.path.basename(path)
            self.path = os.path.abspath(path)
        else:  # Manejo de error si el path no es válido
            raise ValueError(f"Invalid path: {path}")
        # Verificar que todos los archivos tienen la misma extensión
        if self.path_list:
            extensions = {os.path.splitext(f)[1].lower() for f in self.path_list}
            if len(extensions) > 1:
                raise ValueError(f"Multiple file types detected: {extensions}. "
                                "All files must have the same extension.")
        elif self.file_type in ["list", "dir"]:
            raise ValueError(f"The directory '{path}' is empty.")
