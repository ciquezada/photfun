import unittest
import os
import shutil
from photfun.daophot_wrap import find, phot, pick, create_psf, sub_fits, allstar


# Cambiar al directorio donde está el script
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
os.chdir(SCRIPT_DIR)

# Definir directorios
TEST_DIR = os.path.join(SCRIPT_DIR, "data/")
INPUT_DIR = os.path.join(TEST_DIR, "input/")
OUTPUT_DIR = os.path.join(TEST_DIR, "output/")
EXPECTED_DIR = os.path.join(TEST_DIR, "expected_output/")

class TestDaophotPipeline(unittest.TestCase):
    
    @classmethod
    def setUpClass(cls):
        """ Prepara un directorio de salida limpio. """
        if os.path.exists(OUTPUT_DIR):
            shutil.rmtree(OUTPUT_DIR)
        os.makedirs(OUTPUT_DIR)

    def compare_files(self, output_file, expected_file):
        """ Compara dos archivos binarios o de texto. """
        with open(output_file, 'rb') as f1, open(expected_file, 'rb') as f2:
            self.assertEqual(f1.read(), f2.read(), f"Mismatch: {output_file} != {expected_file}")

    def test_find(self):
        find(os.path.join(INPUT_DIR, "12_white.fits"), 
            os.path.join(INPUT_DIR, "daophot.opt"), 
            os.path.join(OUTPUT_DIR, "12_white.coo"))
        self.compare_files(os.path.join(OUTPUT_DIR, "12_white.coo"),
                             os.path.join(EXPECTED_DIR, "12_white.coo"))

    def test_phot(self):
        phot(os.path.join(INPUT_DIR, "12_white.fits"), 
                os.path.join(EXPECTED_DIR, "12_white.coo"), 
                os.path.join(INPUT_DIR, "daophot.opt"), 
                os.path.join(INPUT_DIR, "photo.opt"), 
                os.path.join(OUTPUT_DIR, "12_white.ap"))
        self.compare_files(os.path.join(OUTPUT_DIR, "12_white.ap"), 
                            os.path.join(EXPECTED_DIR, "12_white.ap"))

    def test_pick(self):
        pick(os.path.join(INPUT_DIR, "12_white.fits"), 
            os.path.join(EXPECTED_DIR, "12_white.ap"), 
            os.path.join(INPUT_DIR, "daophot.opt"), 
            os.path.join(OUTPUT_DIR, "12_white.lst"))
        self.compare_files(os.path.join(OUTPUT_DIR, "12_white.lst"), 
                            os.path.join(EXPECTED_DIR, "12_white.lst"))

    def test_create_psf(self):
        create_psf(os.path.join(INPUT_DIR, "12_white.fits"), 
                    os.path.join(EXPECTED_DIR, "12_white.ap"), 
                    os.path.join(EXPECTED_DIR, "12_white.lst"), 
                    os.path.join(INPUT_DIR, "daophot.opt"), 
                    os.path.join(OUTPUT_DIR, "12_white.psf"), 
                    os.path.join(OUTPUT_DIR, "12_white.nei"))
        self.compare_files(os.path.join(OUTPUT_DIR, "12_white.psf"), 
                            os.path.join(EXPECTED_DIR, "12_white.psf"))
        self.compare_files(os.path.join(OUTPUT_DIR, "12_white.nei"), 
                            os.path.join(EXPECTED_DIR, "12_white.nei"))

    def test_sub_fits(self):
        sub_fits(os.path.join(INPUT_DIR, "12_white.fits"), 
                    os.path.join(EXPECTED_DIR, "12_white.psf"), 
                    os.path.join(EXPECTED_DIR, "12_white.nei"), 
                    os.path.join(INPUT_DIR, "daophot.opt"), 
                    os.path.join(OUTPUT_DIR, "12_white_sub.fits"))
        self.compare_files(os.path.join(OUTPUT_DIR, "12_white_sub.fits"), 
                            os.path.join(EXPECTED_DIR, "12_white_sub.fits"))

    def test_allstar(self):
        allstar(os.path.join(INPUT_DIR, "12_white.fits"), 
                os.path.join(EXPECTED_DIR, "12_white.psf"), 
                os.path.join(EXPECTED_DIR, "12_white.ap"), 
                os.path.join(INPUT_DIR, "daophot.opt"), 
                os.path.join(INPUT_DIR, "allstar.opt"), 
                os.path.join(OUTPUT_DIR, "12_white.als"), 
                os.path.join(OUTPUT_DIR, "12_white_sub_als.fits"))
        self.compare_files(os.path.join(OUTPUT_DIR, "12_white.als"), 
                            os.path.join(EXPECTED_DIR, "12_white.als"))
        self.compare_files(os.path.join(OUTPUT_DIR, "12_white_sub_als.fits"), 
                            os.path.join(EXPECTED_DIR, "12_white_sub_als.fits"))
    
    @classmethod
    def tearDownClass(cls):
        """ Limpia el directorio de salida después de las pruebas. """
        if os.path.exists(OUTPUT_DIR):
            shutil.rmtree(OUTPUT_DIR)

if __name__ == '__main__':
    unittest.main()
