from chef import *

with Block("main", [], Int("out")):
    statement("c: counter(rail : 300)")
    statement("out <- c")
