#!/usr/bin/env python

import os
import subprocess

BLACK = "\u001b[30;1m"
RED = "\u001b[31;1m"
GREEN = "\u001b[32;1m"
YELLOW = "\u001b[33;1m"
BLUE = "\u001b[34;1m"
MAGENTA = "\u001b[35;1m"
CYAN = "\u001b[36;1m"
WHITE = "\u001b[37;1m"
RESET = "\u001b[0m"

def info(s, **args):
    print(f"{BLUE}{s}{RESET}", **args)

def success(s, **args):
    print(f"{GREEN}{s}{RESET}", **args)

def error(s, **args):
    print(f"{RED}{s}{RESET}", **args)

def parent(path: str, n=1):
    return "/".join(list(filter(lambda x: x != ".", path.split('/')))[:-n])


# ================================= MAIN PROGRAM =================================

GIT_DIR = parent(__file__, 2)
PYTHON_PACKAGE_DIR = GIT_DIR + "/chef-python"
EXAMPLES_DIR = PYTHON_PACKAGE_DIR + "/examples"
CHEF_RUST_DIR = GIT_DIR + "/chef-lang"
CHEF_EXE = CHEF_RUST_DIR + "/target/debug/chef"

info("Compiling Chef...")
subprocess.run(
    ["cargo", "build", "--manifest-path", CHEF_RUST_DIR + "/Cargo.toml"],
    capture_output=False
               )
print()

info("Compiling examples:")
for example in os.listdir(EXAMPLES_DIR):
    print(f"    {example} ... ", end="")
    out = subprocess.run(
        [CHEF_EXE, "--python", "/home/balder/.nix-profile/bin/python", "cook", f"{EXAMPLES_DIR}/{example}"],
        capture_output=True,
        env={'PYTHONPATH': f"{PYTHON_PACKAGE_DIR}/src"}
        )
    if out.returncode == 0:
        success("OK")
    else:
        error("FAILED")
        print(out.stdout.decode("utf-8"))
        print(out.stderr.decode("utf-8"))
        exit(1)
