# install.py
import docker
import subprocess
import sys
import os

def download_and_save_docker_image():
    # Inicializa el cliente Docker
    client = docker.from_env()

    try:
        # Nombre de la imagen en Docker Hub (reemplaza 'mi_usuario' y 'mi_imagen_docker' con tus valores)
        image_name = "ciquezada/photfun-daophot_wrapper:latest"
        
        # Descargar la imagen desde Docker Hub
        print(f"Downloading image {image_name} from Docker Hub...")
        client.images.pull(image_name)
        
        print(f"Image {image_name} downloaded.")

        # Ruta donde se guardará el archivo .tar
        tar_file_path = "./daophot_wrap/photfun-daophot_wrapper.tar"
        
        # Guardar la imagen como un archivo .tar
        print(f"Saving Docker image in  {tar_file_path}...")
        with open(tar_file_path, "wb") as tar_file:
            for chunk in client.images.get(image_name).save():
                tar_file.write(chunk)

        print(f"Docker image saved in {tar_file_path}")
    except docker.errors.ImageNotFound as e:
        print(f"Error downloading Docker image: {e}")
        sys.exit(1)

if __name__ == "__main__":
    download_and_save_docker_image()
