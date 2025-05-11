# PHOTfun - PSF Photometry and IFU Spectral Extraction Toolkit

## Description
**PHOTfun** is a Python package designed to streamline PSF photometry workflows using the **DAOPHOT-II** suite. It provides a modern graphical interface powered by the **Shiny** web framework, along with the **PHOTcube** extension for extracting stellar spectra from IFU datacubes.

Designed with professional astronomers in mind, **PHOTfun** enables efficient PSF-based analysis and visualization even in highly crowded fields.

---

## Installation 

Before using PHOTfun, install the package from PyPI:

```bash
pip install photfun
```

> **Important:** PHOTfun depends on the DAOPHOT-II suite (written in Fortran). If DAOPHOT is not already installed on your machine, **you must use Docker** to run the pre-packaged environment.

---

## Optional: Use Docker (Recommended if DAOPHOT is Not Installed)

We provide a Docker image with **PHOTfun**, **DAOPHOT-II**, and all dependencies pre-installed:
- Docker Image: `ciquezada/photfun-daophot_wrapper`

### How to Install Docker (If DAOPHOT is Not Installed)

#### Ubuntu (Recommended: install Docker Desktop or follow official Docker guide)

1. Set up Docker’s apt repository:

```bash
sudo apt-get update
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc
```

2. Add the Docker repository:

```bash
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "${UBUNTU_CODENAME:-$VERSION_CODENAME}") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```

3. Install Docker:

```bash
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
```

4. Start the Docker service:

```bash
sudo service docker start
```

5. (Recommended) Add your user to the `docker` group to avoid using `sudo` every time:

```bash
sudo groupadd docker   # (if group doesn't exist)
sudo usermod -aG docker $USER
```

Log out and back in (or reboot) for group changes to take effect.

#### macOS

Use Homebrew:

```bash
brew install --cask docker
```

Then open the **Docker.app** from your Applications folder.

#### Windows

Download and install **Docker Desktop** from:

[https://www.docker.com/products/docker-desktop/](https://www.docker.com/products/docker-desktop/)

---


## Quick Start

If DAOPHOT-II or Docker are installed and available in your system’s `$PATH`, you can run PHOTfun directly after the `pip install`:

```bash
photfun
```

A local Shiny web application will launch, and a message similar to this will appear in your terminal:

```
INFO:     Started server process [3396]
INFO:     Waiting for application startup.
INFO:     Application startup complete.
INFO:     Uvicorn running on http://0.0.0.0:41693 (Press CTRL+C to quit)
```

Open the browser and visit the address that appears (e.g., `http://0.0.0.0:41693`) to begin using the interface.
(if the IP didn't work, try the other IPs suggested during the running dialog in the terminal)

---


## How to Use

### PHOTfun GUI (PSF Photometry)

1. Launch the app with `photfun`
2. Upload your `.fits` images.
3. Use the GUI to execute:
   - `FIND`, `PICK`, `PHOT`, `PSF`, `ALLSTAR`, etc.
4. Visually inspect PSF stars and refine the sample interactively.

### PHOTcube (IFU Spectra Extraction)

1. Load a datacube in PHOTfun.
2. The cube will be sliced into monochromatic images.
3. PSF photometry is applied across the cube.
4. Extracted fluxes are concatenated to form 1D stellar spectra.

---

## Manual Use of DAOPHOT Inside Docker (Advanced)

You can open a shell inside the Docker container and run DAOPHOT directly:

```bash
docker run -it -v /path/to/your/data:/data ciquezada/photfun-daophot_wrapper /bin/bash
```

Then:

```bash
cd /data
daophot
```

This allows you to use DAOPHOT independently of the GUI, while keeping all dependencies encapsulated.

---

## Dependencies

PHOTfun installs the following dependencies via `pip`:

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

## Credits

- **Developer:** Carlos Quezada  
- Inspired by the work of Álvaro Valenzuela  
- Built on top of DAOPHOT-II by Peter Stetson  

---

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

---
