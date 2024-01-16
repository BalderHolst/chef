import chef

chef.current_block = None

class Variable:
    def __init__(self, name: str, type: str) -> None:
        self.name = name
        self.type = type

    def definition(self) -> str:
        return f"{self.name}: {self.type}"

    def __repr__(self) -> str:
        return self.name

    def __add__(self, other) -> str:
        return f"{self} + {str(other)}"

    def __radd__(self, other) -> str:
        return self.__add__(other)

class Block:
    def __init__(self, name: str, args) -> None:
        self.name = name
        self.args = args
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
        args = ", ".join(map(str, self.args))
        s = f"block {self.name}({args})" + " {\n"
        for statement in self.statements:
            s += f"\t{statement}\n"
        s += "}"
        return s

def statement(s):
    chef.current_block.statements.append(str(s))
        
if __name__ == "__main__":
    a = Variable("a", "int")
    b = Variable("b", "int")
    c = Variable("c", "int")

    with Block("test_block", (a, b ,c)):
        statement(f"a + b + 10;")
        statement(f"a + b + 10;")
        statement(f"a + b + 10;")
        statement(f"a + b + 10;")
        
