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

class All(Variable):
    def __init__(self, name: str) -> None:
        super().__init__(name, "all")

class Counter(Variable):
    def __init__(self, name: str, type: str, limit: int) -> None:
        super().__init__(name, f"counter({type} : {limit})")

class Block:
    def __init__(self, name: str, args: None | list[Variable] | Variable = None, out: str | None = None) -> None:
        """Start a chef `block` with a name, arguments and output type."""
        self.name = name

        if args is None:
            self.args = []
        elif isinstance(args, Variable):
            self.args = [args]
        else:
            self.args = list(args)

        self.out = out
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
        args = ", ".join(map(lambda a: a.definition, self.args))
        s = f"block {self.name}({args})" 
        if not self.out is None: s += f" -> {self.out}"
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
        
def statement(s):
    """Add a statement withing a block."""
    if len(chef.compound_statement_stack) == 0:
        if not chef.current_block is None:
            chef.current_block.statements.append(str(s))
        else:
            raise Exception("Statements must be within blocks.")
    elif chef.compound_statement_stack[-1].is_inside:
        chef.compound_statement_stack[-1].add_statement(s)
    else:
        chef.compound_statement_stack.pop()
        chef.current_block.statements.append(str(s))

if __name__ == "__main__":
    pass
