# argv: "block_name" "a: int(pump); picks: ['signal-A', 'signal-B']" "out: many"

# This defines `INPUTS`
from chef import *

# Get parsed `picks` literal
pick_signals: list[str] = INPUTS["picks"]


with DynBlock():
    for signal in pick_signals:
        statement(f"out <- input[{signal}]")
