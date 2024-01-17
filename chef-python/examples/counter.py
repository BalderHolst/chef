from chef import *

with Block("main", [], "int"):
    statement("c: counter(rail : 300);")
    statement("c")
