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

## Credits
- **Developer:** Carlos Quezada
- Inspired by the work of Alvaro Valenzuela
- Built upon DAOPHOT-II and ALLSTAR by Peter Stetson

## Installation
1. Clone this repository.
2. Make sure the dependencies listed in `setup.py` are installed.
3. Install the package:
   ```bash
   pip install .
   ```
4. Run the GUI using the command:
   ```bash
   photfun
   ```

## Dependencies
The package depends on the following libraries:
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

## Contributions
Contributions are welcome! To contribute:
1. Fork the repository.
2. Create a new branch:
   ```bash
   git checkout -b feature/my-feature
   ```
3. Make your changes and commit them:
   ```bash
   git commit -am 'Add my feature'
   ```
4. Push to your branch:
   ```bash
   git push origin feature/my-feature
   ```
5. Open a pull request.

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

## Créditos
- **Desarrollador:** Carlos Quezada
- Inspirado en el trabajo de Alvaro Valenzuela
- Basado en DAOPHOT-II y ALLSTAR, software de Peter Stetson

## Instalación
1. Clona este repositorio.
2. Asegúrate de tener las dependencias de `setup.py` instaladas.
3. Instala el paquete con:
   ```bash
   pip install .
   ```
4. Ejecuta la interfaz con:
   ```bash
   photfun
   ```

## Dependencias
El paquete requiere las siguientes librerías:
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

## Contribuciones
¡Las contribuciones son bienvenidas! Para contribuir:
1. Haz un fork del repositorio.
2. Crea una nueva rama:
   ```bash
   git checkout -b feature/nueva-funcionalidad
   ```
3. Realiza tus cambios y haz commit:
   ```bash
   git commit -am 'Agrega nueva funcionalidad'
   ```
4. Haz push de tu rama:
   ```bash
   git push origin feature/nueva-funcionalidad
   ```
5. Abre un pull request.

## Licencia
Este proyecto está bajo la Licencia MIT. Consulta el archivo `LICENSE` para más detalles.

