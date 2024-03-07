# Ideas

## Compiling
Cook your recopies (".rcp" files) with the following command:
```bash
chef cook <file>
```
This will copy the blueprint to your clipboard, and show a rough sketch of the layout in the terminal.

## Simulatoin
Simulate a chef program.
```bash
chef simulate <file>
```

## Variables
Variables are immutable as they simply represent outputs of an expression.

## Blueprint Interface
Defined areas where combinators are allowed to be placed.

Add combinators to preexisting blueprints.

## Mod Support
The ability to import mods, and use the added items in blueprints

## Fun
Random greeting after successful compilation.

Tells you useful cooking tips when compilation fails.

## Code generation
Import python files directly in your code, just like a chef file. The python file is then run, and its stdout is interpreted as chef code.

## Dynamic Blocks
Some blocks implementation may need to be generated at compile time. Dynamic blocks are blocks that have a defined interface in chef, but at handed over to a python script to be generated at compile time. This is useful for cases like shift registers, as depending on their size, their structure changes.

```
dyn block shift_register(shift: bool, input: all, size: lit) <script.py> 
```

Outputs should not need to be specified as they could depend on the inputs.

It should also be write the python macro withing the chef file like this:

```
dyn block shift_register(shift: bool, input: all, size: lit) {
    from chef import *

    # Write your python code here
}
```
## Pipeline
It would be nice to be able to route blocks into each other like this
```
out <- block1 <- block2 <- block3(input)
```
