from chef import *

with Block("main", Many("input"), Int("out", "raw-fish")):
    statement("out <<- input[pump] * 30 - 10")
