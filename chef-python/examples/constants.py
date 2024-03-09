from chef import *

var = Int("b")

const(var, 1000)
print("const A = 100")

with Block("main", "", Int("out")):
    statement(f"out <- {var} + A")
