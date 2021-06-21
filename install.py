from os.path import join as pjoin

import os
import sys
import json
import shutil
import pathlib

here = os.path.dirname(os.path.realpath(__file__))

KERNEL_DIR = pjoin(
    sys.prefix,
    "share",
    "jupyter",
    "kernels",
    "ielisp"
)
KERNEL_FILE = "ielisp.el"
 
def find_emacs():
    # find emacs executable
    return (
        os.getenv("EMACS", None)
        or shutil.which("emacs")
        or "emacs"
    )

def copy_kernel():
    shutil.copyfile(pjoin(here, KERNEL_FILE), pjoin(KERNEL_DIR, KERNEL_FILE))

def write_kernelspec():
    contents = {
        "argv": [find_emacs(), "--script", pjoin(KERNEL_DIR, KERNEL_FILE), "{connection_file}"],
        "display_name": "Emacs Lisp",
        "language": "elisp"
    }
    with open(pjoin(KERNEL_DIR, "kernel.json"), "w") as f:
        json.dump(contents, f)

if __name__ == "__main__":
    if not os.path.exists(KERNEL_DIR):
        os.makedirs(KERNEL_DIR, exist_ok=True)
    copy_kernel()
    write_kernelspec()
