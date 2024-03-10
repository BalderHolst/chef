import chef
import sys

chef.current_block = None
chef.compound_statement_stack = []
chef.indentation = 0

class ChefMacroError(Exception): pass

class Variable:
    def __init__(self, name: str, type: str) -> None:
        self.name = name
        self.type = type

    @classmethod
    def from_string(cls, s: str):
        name, type = s.split(":")
        name = name.strip()
        type = type.strip()

        if type == "int": return Int(name)
        elif type == "bool": return Bool(name)
        elif type == "many": return Many(name)
        elif type.startswith("counter"):
            type = type.split("(")[1].split(")")[0]
            type, limit = type.split(":")
            return Counter(name, type, int(limit))
        elif type.startswith("int"):
            item = type.split("(")[1].split(")")[0]
            return Int(name, item)
        elif type.startswith("bool"):
            item = type.split("(")[1].split(")")[0]
            return Bool(name, item)

        return None

    @property
    def definition(self) -> str:

        return f"{self.name}: {self.type}"

    def __repr__(self) -> str:
        return self.name

class Int(Variable):
    def __init__(self, name: str, item: str | None = None) -> None:
        if item is None: super().__init__(name, f"int")
        else: super().__init__(name, f"int({item})")

class Bool(Variable):
    def __init__(self, name: str, item: str | None = None) -> None:
        if item is None: super().__init__(name, f"int")
        else: super().__init__(name, f"int({item})")

class Many(Variable):
    def __init__(self, name: str) -> None:
        super().__init__(name, "many")

class Counter(Variable):
    def __init__(self, name: str, type: str, limit: int) -> None:
        super().__init__(name, f"counter({type} : {limit})")

class Block:
    def __init__(self, name: str, inputs: None | list[Variable] | Variable = None, outputs: None | list[Variable] | Variable = None) -> None:
        """Start a chef `block` with a name, arguments and output type."""
        self.name = name

        if inputs is None:
            self.inputs = []
        elif isinstance(inputs, Variable):
            self.inputs = [inputs]
        else:
            self.inputs = list(inputs)

        if outputs is None:
            self.outputs = []
        elif isinstance(outputs, Variable):
            self.outputs = [outputs]
        else:
            self.outputs = list(outputs)

        self.statements = []

    def __enter__(self):
        chef.current_block

        if not chef.current_block is None:
            raise Exception("Blocks cannot be defined within blocks.")

        chef.current_block = self

    def __exit__(self, *_):
        self = chef.current_block
        chef.current_block = None
        
        print(self)

    def __repr__(self) -> str:
        inputs = ", ".join(map(lambda a: a.definition, self.inputs))
        outputs = ", ".join(map(lambda a: a.definition, self.outputs))
        s = f"block {self.name}({inputs}) => ({outputs})" 
        s += " {\n"
        chef.indentation += 1
        indent = "\t" * chef.indentation
        for statement in self.statements:
            s += f"{indent}{statement}\n"
        s += "}"
        chef.indentation -= 1
        return s

class DynBlock(Block):
    def __init__(self) -> None:

        input_vars = []
        for var in INPUTS.values():
            if isinstance(var, Variable):
                input_vars.append(var)

        output_vars = []
        for var in OUTPUTS.values():
            if isinstance(var, Variable):
                output_vars.append(var)

        if not NAME:
            raise ChefMacroError("Name for dynanic block mut be provided through command line arguments.")

        super().__init__(NAME, input_vars, output_vars)

class CompoundStatement:
    """Generic class inherited by compound statements."""
    def __init__(self) -> None:
        self.statements = []
        self.is_inside = False

    def add_statement(self, statement: str):
        self.statements.append(statement)

    def __enter__(self):
        self.is_inside = True

    def __exit__(self, *_):
        self.is_inside = False

class When(CompoundStatement):
    """Start a `when` block with a condition."""

    def __init__(self, condition: str) -> None:
        super().__init__()
        self.condition = condition
        self.indentation = 0

    def __enter__(self):
        super().__enter__()
        chef.compound_statement_stack.append(self)
        chef.indentation += 1
        self.indentation = chef.indentation

    def __exit__(self, *_):
        super().__exit__()
        chef.compound_statement_stack.pop()
        if len(chef.compound_statement_stack) > 0:
            chef.compound_statement_stack[-1].add_statement(str(self))
        else:
            statement(str(self))
        chef.indentation -= 1

    def __repr__(self) -> str:
        s = f"when {self.condition}" + " {\n"
        chef.indentation += 1
        indent = "\t" * chef.indentation
        for statement in self.statements:
            s += f"{indent}{statement}\n"
        chef.indentation -= 1
        indent = "\t" * chef.indentation
        s += indent + "}"
        return s

def const(name: str, const_expr: str):
    """Create a chef constant."""
    print(f"const {name} = {const_expr}")
        
def statement(s, semicolon: bool = True):
    """Add a statement withing a block."""

    if semicolon:
        s += ";"

    if len(chef.compound_statement_stack) == 0:
        if not chef.current_block is None:
            chef.current_block.statements.append(s)
        else:
            raise Exception("Statements must be within blocks.")
    elif chef.compound_statement_stack[-1].is_inside:
        chef.compound_statement_stack[-1].add_statement(s)
    else:
        chef.compound_statement_stack.pop()
        chef.current_block.statements.append(s)

def parse_string_args(s: str) -> dict:
    vars = {}
    for arg in s.split(";"):
        var_name, var_type = arg.split(":")

        var_name = var_name.strip()
        var_type = var_type.strip()

        var = Variable.from_string(arg)

        if not var:
            var = eval(var_type)

        vars[var_name] = var

    return vars

def get_input_output() -> tuple[str, dict, dict]:
    """Get inputs from the command line."""

    if len(sys.argv) <= 1: return (None, {}, {})

    name = sys.argv[1].strip()

    if len(sys.argv) <= 2: return (name, {}, {})
    input_string = sys.argv[2]

    inputs = parse_string_args(input_string)

    if len(sys.argv) <= 3: return (name, inputs, {})

    output_string = sys.argv[3]
    outputs = parse_string_args(output_string)

    return (name, inputs, outputs)


NAME, INPUTS, OUTPUTS = get_input_output()

if __name__ == "__main__":
    pass

