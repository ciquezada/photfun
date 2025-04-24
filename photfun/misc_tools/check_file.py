import os


def check_file(file, msg=""):
	if not os.path.isfile(file):
		raise FileNotFoundError(f"{msg}{file}")