import chef

chef.current_block = None
chef.compound_statement_stack = []
chef.indentation = 0

class Variable:
    def __init__(self, name: str, type: str) -> None:
        self.name = name
        self.type = type

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

if __name__ == "__main__":
    pass
