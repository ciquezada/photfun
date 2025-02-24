import os
import shutil

def copy_file_noreplace(temp_log, out_log):
    # Obtener el directorio y el nombre base del archivo
    out_dir = os.path.dirname(out_log)
    filename, ext = os.path.splitext(os.path.basename(out_log))
    
    # Si el archivo ya existe, agregar un número consecutivo antes de la extensión
    counter = 1
    new_out_log = out_log
    while os.path.exists(new_out_log):
        new_out_log = os.path.join(out_dir, f"{filename}_{counter}{ext}")
        counter += 1

    # Mover el archivo sin sobrescribir
    shutil.copy(temp_log, new_out_log)
    
    return new_out_log