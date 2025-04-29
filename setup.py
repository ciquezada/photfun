from setuptools import setup, find_packages


with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name='photfun',
    version='0.1.8',
    packages=find_packages(),
    install_requires=[
        "astropy==7.0.1",
        "faicons==0.2.2",
        "imageio==2.37.0",
        "joblib==1.4.2",
        "matplotlib==3.10.1",
        "nest_asyncio==1.6.0",
        "numpy==2.2.5",
        "pandas==2.2.3",
        "Pillow==11.2.1",
        "scipy==1.15.2",
        "shiny==1.4.0",
        "tqdm==4.67.1",
        "docker",
        "psutil==7.0.0"
    ],
    
    python_requires='>=3.11',
    entry_points={
        'console_scripts': [
            'photfun = photfun.photfun_gui.app:run_photfun',
        ]
    },
    # Otros metadatos como autor, descripción, etc.
    author='Carlos Quezada',
    description="""
PHOTfun is an interactive adaptation of the DAOPHOT 
astronomical image processing software. It provides 
a user-friendly interface through button-based interactions, 
enabling astronomers to perform various tasks seamlessly. 
The software operates by executing DAOPHOT commands in the 
background, facilitating astronomical data analysis, such 
as finding, photometry, and point spread function (PSF) 
in images. PHOTfun simplifies the utilization of DAOPHOT's capabilities 
by integrating them into an accessible and intuitive graphical user interface.


Credits: by Carlos Quezada
            inspired in the work of Alvaro Valenzuela
            thanks to DAOPHOT by Peter Stetson""",
    long_description=long_description,
    long_description_content_type="text/markdown",
    license='MIT',
    keywords=['daophot', 'astronomical', 'python'],
    url='https://github.com/ciquezada/photfun'
)
