from chef import *

with Block("main", All("input"), "int(raw-fish)"):
    statement("input[pump] * 30 - 10")
