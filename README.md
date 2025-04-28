# PHOTfun - PSF Photometry and IFU Spectral Extraction Toolkit

## Description
**PHOTfun** is a Python package designed to simplify PSF photometry workflows using the DAOPHOT-II and ALLSTAR suite. It provides an intuitive graphical interface for executing essential photometric tasks and includes a dedicated extension, **PHOTcube**, for extracting stellar spectra from IFU datacubes. The GUI is built using the Shiny web framework for Python, allowing users to interactively manage every step of the process, from source detection to photometric analysis.

In crowded stellar fields, PHOTcube enables efficient and accurate spectral extraction via monochromatic slicing and PSF photometry, reconstructing high-fidelity stellar spectra.

## Key Features
- Shiny-based graphical interface for running DAOPHOT-II routines interactively.
- Executes FIND, PICK, PHOT, PSF, SUBTRACT, and DAOMATCH for full PSF photometry workflows.
- **PHOTcube** extension for IFU datacube slicing and spectral extraction.
- Visual inspection and rejection of PSF stars via GUI.
- Interoperability with external tools like TOPCAT and DS9 through SAMP.
- Available as a standalone Docker container for easy setup.

## Installation

### Option 1: Native Installation (Requires DAOPHOT Installed Separately)

PHOTfun can be installed directly from PyPI:

```bash
pip install photfun
```

**Note:** You must have DAOPHOT-II, and their dependencies installed and available on your system path for full functionality.

### Option 2: Using Docker (Recommended for Standalone Usage)

We provide a pre-built Docker image that includes PHOTfun, DAOPHOT-II, and all necessary dependencies:
- Docker Image: `ciquezada/photfun-daophot_wrapper`

Only Docker installation is required on your system. Once Docker is installed, the container will automatically handle everything else.

**Quick Start (after installing Docker):**

```bash
photfun
```

Then open your browser and navigate to `http://localhost:8000` to start using PHOTfun.

#### Docker Installation Instructions by OS

**Ubuntu / Debian:**

```bash
sudo apt update
sudo apt install -y docker.io
sudo systemctl start docker
sudo systemctl enable docker
```

**Fedora:**

```bash
sudo dnf install -y docker
sudo systemctl start docker
sudo systemctl enable docker
```

**macOS (using Homebrew):**

```bash
brew install --cask docker
```
Then open Docker.app from your Applications.

**Windows:**
- Download Docker Desktop from [https://www.docker.com/products/docker-desktop/](https://www.docker.com/products/docker-desktop/) and install it following the installer prompts.

---

## Usage Instructions

### PHOTfun GUI (Photometry)
1. Run `photfun` from the command line.
2. Select a `.fits` file or set of images to process.
3. Use the interface to execute FIND, PICK, PHOT, PSF modeling, and photometry steps.
4. Interactively inspect PSF stars and reject outliers.

### PHOTcube (IFU Spectra Extraction)
1. Load a datacube in PHOTfun.
2. Automatically slice the datacube into monochromatic images.
3. Apply PSF photometry on each slice using previously defined source lists.
4. Extract and concatenate monochromatic fluxes into 1D spectra for each target.

---

## Dependencies

If installed PHOTfun depends on:
- `astropy==7.0.1`
- `faicons==0.2.2`
- `imageio==2.37.0`
- `joblib==1.4.2`
- `matplotlib==3.10.1`
- `nest_asyncio==1.6.0`
- `numpy==2.2.5`
- `pandas==2.2.3`
- `Pillow==11.2.1`
- `scipy==1.15.2`
- `shiny==1.4.0`
- `tqdm==4.67.1`
- `docker`


---

### Using DAOPHOT manually inside the Docker container

To run DAOPHOT interactively inside a Docker container with access to your local files, mount your working directory using the `-v` flag:

```bash
docker run -it -v /path/to/your/data:/data ciquezada/photfun-daophot_wrapper /bin/bash
```


**Explanation:**

- `-v /path/to/your/data:/data` mounts your local directory into the container at `/data`.
- `-it` starts an interactive terminal session.
- `/bin/bash` launches a bash shell inside the container. ([what is docker run -it flag? - Stack Overflow](https://stackoverflow.com/questions/48368411/what-is-docker-run-it-flag?utm_source=chatgpt.com))

Once inside the container, navigate to `/data` to access your files:

```bash
cd /data
```

you can run `daophot`, `allstar`, and other tools directly:

```bash
daophot
```

This allows you to use DAOPHOT independently from the GUI if needed.

---

## Credits
- **Developer:** Carlos Quezada
- Inspired by the work of Alvaro Valenzuela
- Built upon DAOPHOT-II by Peter Stetson

---

## License
This project is licensed under the MIT License. See the `LICENSE` file for details.

---

# (SPANISH) PHOTfun - Fotometría PSF y Extracción Espectral desde Cubos IFU

## Descripción
**PHOTfun** es un paquete en Python que facilita la realización de fotometría PSF usando DAOPHOT-II y ALLSTAR, con una interfaz gráfica intuitiva desarrollada con Shiny. Incluye una extensión llamada **PHOTcube**, especialmente diseñada para la extracción espectral desde cubos de datos IFU.

PHOTcube permite realizar una fotometría por PSF sobre imágenes monocromáticas obtenidas a partir de un cubo IFU, y luego reconstruir los espectros para cada fuente detectada, optimizando la separación de objetos en campos estelares densos.

## Características principales
- Interfaz gráfica basada en Shiny para ejecutar comandos de DAOPHOT-II.
- Incluye rutinas FIND, PICK, PHOT, PSF, SUBTRACT y DAOMATCH.
- Herramienta visual para inspección y rechazo de estrellas PSF.
- Soporte SAMP para interoperabilidad con herramientas como TOPCAT y DS9.
- **PHOTcube** para corte del cubo IFU y extracción espectral automatizada.
- Opción de ejecución standalone vía Docker.

## Instalación

### Opción 1: Instalación Nativa (requiere DAOPHOT instalado previamente)

Instala directamente desde PyPI:

```bash
pip install photfun
```

**Nota:** Necesitas tener DAOPHOT-II, ALLSTAR y sus dependencias ya instaladas en tu sistema.

### Opción 2: Uso de Docker (Recomendado para facilitar la instalación)

Usa el contenedor Docker `ciquezada/photfun-daophot_wrapper`, que incluye PHOTfun, DAOPHOT-II y todas las dependencias necesarias.

**Inicio rápido (tras instalar Docker):**

```bash
photfun
```

Luego abre tu navegador en `http://localhost:8000`.

#### Instrucciones para instalar Docker según el sistema operativo

**Ubuntu / Debian:**

```bash
sudo apt update
sudo apt install -y docker.io
sudo systemctl start docker
sudo systemctl enable docker
```

**Fedora:**

```bash
sudo dnf install -y docker
sudo systemctl start docker
sudo systemctl enable docker
```

**macOS (Homebrew):**

```bash
brew install --cask docker
```
Luego ejecuta Docker.app desde Aplicaciones.

**Windows:**
- Descarga Docker Desktop desde [https://www.docker.com/products/docker-desktop/](https://www.docker.com/products/docker-desktop/) y sigue las instrucciones.

---

## Instrucciones de uso

### Interfaz PHOTfun (Fotometría)
1. Ejecuta `photfun` desde la terminal.
2. Selecciona archivos `.fits` o conjuntos de imágenes para procesar.
3. Ejecuta FIND, PICK, PHOT, PSF y otros pasos desde la interfaz.
4. Revisa visualmente las estrellas PSF y descarta las inadecuadas.

### PHOTcube (Extracción Espectral desde Cubos IFU)
1. Carga un cubo en la interfaz PHOTfun.
2. El cubo será dividido automáticamente en imágenes monocromáticas.
3. Aplica fotometría PSF usando listas maestras de fuentes.
4. Los flujos monocromáticos se concatenan para formar los espectros de cada estrella.

---

### Uso de DAOPHOT manualmente dentro del contenedor Docker

Para ejecutar DAOPHOT interactivamente dentro de un contenedor Docker y acceder a tus archivos locales, monta tu directorio de trabajo utilizando la opción `-v`:

```bash
docker run -it -v /ruta/a/tu/directorio:/data ciquezada/photfun-daophot_wrapper /bin/bash
```

**Explicación:**

- `-v /ruta/a/tu/directorio:/data` monta tu directorio local en el contenedor en la ruta `/data`.
- `-it` inicia una sesión interactiva en la terminal.
- `/bin/bash` lanza una shell bash dentro del contenedor.

Una vez dentro del contenedor, navega al directorio `/data` para acceder a tus archivos y ejecutar DAOPHOT:

```bash
cd /data

```

Este enfoque te permite trabajar directamente con tus archivos locales dentro del entorno del contenedor. 
Una vez dentro, puedes ejecutar `daophot`, `allstar` y otras herramientas directamente:

```bash
daophot
```

---

## Créditos
- **Desarrollador:** Carlos Quezada
- Inspirado en el trabajo de Alvaro Valenzuela
- Basado en DAOPHOT-II y ALLSTAR, software de Peter Stetson

---


## Licencia
Este proyecto está bajo la Licencia MIT. Consulta el archivo `LICENSE` para más detalles.
