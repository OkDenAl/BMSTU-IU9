from dataclasses import dataclass


ALLOCA, LOAD, STORE, BR, \
CONDBR, ICMP, MUL, ADD, SUB, \
RET, PHI = 'alloca load store br condbr icmp mul add sub ret phi'.split()


class Value:
    pass


@dataclass
class Variable(Value):
    def __init__(self, name, version):
        self.name = name
        self.version = version
        self.is_temp = False

    def __repr__(self):
        return str(self)

    def __str__(self):
        return f'{self.name}{"("+str(self.version)+")" if not self.is_temp else ""}'

    def __hash__(self):
        return len(self.name)

    def __eq__(self, o):
        return type(self) == type(o) and self.name == o.name or type(o) == str and 0 == self.name


@dataclass
class IntConst(Value):
    value: int

    def __repr__(self):
        return str(self)

    def __str__(self):
        return str(self.value)
    
@dataclass
class Instruction:
    typ: str
    args: dict

    def __repr__(self):
        return str(self)

    def __str__(self):
        if self.typ == STORE:
            return f'{self.args["to"]} <- {self.args["from"]}'
        if self.typ == LOAD:
            return f'{self.args["to"]} <- {self.args["from"]}'
        if self.typ in [SUB, ADD, MUL]:
            return f'{self.args["to"]} <- {self.args["oper1"]} {self.typ} {self.args["oper2"]}'
        if self.typ == BR:
            return f'go to BLOCK{self.args["dest"]}'
        if self.typ == CONDBR:
            return f'if (!{self.args["cond"]}) go to BLOCK{self.args["dest1"]} else go to BLOCK{self.args["dest2"]}'
        if self.typ == ICMP:
            return f'{self.args["to"]} <- {self.args["arg1"]} > {self.args["arg2"]}'
        if self.typ == PHI:
            return f'{self.args["to"]} = phi({", ".join(map(str, self.args["from"]))})'
        if self.typ == ALLOCA:
            return f'new variable {self.args["name"]}'
        
        ret = f'    {self.typ}: '
        for k, v in self.args.items():
            ret += f'{k} {v} '
        return ret
    

@dataclass
class BB:
    def __init__(self):
        self.block_num = 0
        self.instructions = []
        self.returned = False
        self.variables = {}
        self.varcounter = 0
    
    def __hash__(self):
        return self.block_num

    def __str__(self):
        return f'BLOCK {self.block_num}'+ '{\n' + \
            '\n'.join(map(str, self.instructions)) + \
            "\n}\n"
    
    def __repr__(self):
        return f'BB({self.block_num})'

    # ФУНКЦИОНАЛ IR БЛОКА

    def add_instr(self, instr):
        if self.returned:
            return
        self.instructions.append(instr)
    
    def alloca_variable(self, name):
        newvar = Variable(name, 0)
        self.variables[name] = newvar
        instr = Instruction(ALLOCA, {'name':name})
        self.add_instr(instr)
        return newvar

    def create_tmp_var(self):
        self.varcounter += 1
        var =  Variable(f'tmp_{self.block_num}_{self.varcounter-1}', 0)
        var.is_temp = True
        return var

    def new_break(self, dest):
        self.add_instr(
            Instruction(
                BR, {'dest':dest.block_num})
        )

    def new_ret(self, val : Value):
        if isinstance(val, IntConst):
            self.add_instr(Instruction(RET, {'value': val}))

        elif isinstance(val, Variable):
            tmpvar = self.create_tmp_var()
            self.add_instr(Instruction(LOAD, {"from":val, "to":tmpvar}))
            self.add_instr(Instruction(RET, {'value':tmpvar}))

    def new_cond_break(self, cond: Variable, dest1, dest2):
        self.add_instr(Instruction(CONDBR, {'cond':cond, 'dest1':dest1.block_num, 'dest2':dest2.block_num}))

    def new_compare(self, arg1, arg2):
        tmp = self.create_tmp_var()
        self.add_instr(Instruction(ICMP, {"arg1":arg1, "arg2":arg2, "to":tmp}))
        return tmp

    # ФУНКЦИОНАЛ СТРОИТЕЛЯ

    def is_variable_in(self, name:str):
        return name in self.variables
    
    def set_variable(self, name:str, val:Value):
        if isinstance(val, Variable):
            tmpvar = self.create_tmp_var()
            self.add_instr(Instruction(LOAD, {"from":val, "to":tmpvar}))
            self.add_instr(Instruction(STORE, {"from":tmpvar, "to":self.variables[name]}))
        elif isinstance(val, IntConst):
            self.add_instr(Instruction(STORE, {"from":val, "to":self.variables[name]}))

    def set_map(self, parent):
        self.variables.update(parent.variables)

    # LAB 4

    def get_edges(self):
        last = self.instructions[-1]
        if last.typ == BR:
            return {(self.block_num, last.args["dest"])}
        elif last.typ == CONDBR:
            return {(self.block_num, last.args["dest1"]), (self.block_num, last.args["dest2"])}
        return set()

    def build_changing_variables(self):
        s = set()
        for i in self.instructions:
            if i.typ == STORE:
                s.add(i.args['to'])

        self.changing_variables = s
        self.phi_var_blocks = dict()

