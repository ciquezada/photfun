import sys
sys.path.append("/data/ciquezada/Projects/py_photsuite")
### ERASE AFTER
from misc_tools.check_file import check_file
import os


def find(in_fits, out_coo, sum_aver="1,1", verbose=True):
    filename = os.path.splitext(os.path.basename(in_fits))[0]
    if verbose:
        print(f"daophot: find({filename})")
    check_file("daophot.opt", "opt file input: ")
    check_file(f"{in_fits}", "fits file input: ")
    overwrite = [""] if os.path.isfile(f"{out_coo}") else []
    cmd_list = ['daophot << EOF >> pipe.log', f'at {filename}', 
                    'find', f'{sum_aver}', f"{out_coo}",
                    *overwrite,
                    'y', 
                    'exit', 'EOF']
    cmd = '\n'.join(cmd_list)
    os.system(cmd)
    check_file(f"{out_coo}", "coo not created: ")
    if verbose:
        print(f"  -> {out_coo}")
    return(f"{out_coo}")