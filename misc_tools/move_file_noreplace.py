import os
import shutil


def move_file_noreplace(temp_log, out_log):
    # Obtener el directorio de salida
    out_dir = os.path.dirname(out_log)
    
    # Definir nombre base del log
    base_name = os.path.basename(out_log)
    
    # Si el archivo ya existe, agregar un número consecutivo
    counter = 1
    while os.path.exists(out_log):
        out_log = os.path.join(out_dir, f"{base_name}_{counter}")
        counter += 1

    # Mover el archivo sin sobrescribir
    shutil.move(temp_log, out_log)
    
    return out_log