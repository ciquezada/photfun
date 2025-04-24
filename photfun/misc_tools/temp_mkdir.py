import os


def temp_mkdir(folder_name):
    # making needed directories
    gen = (x for x in range(1,99999))
    aux_folder_name = folder_name[:-1]
    while True:
        try:
            os.mkdir(folder_name)
            break
        except OSError as error:
            folder_name = f"{aux_folder_name}{next(gen)}"
    return folder_name