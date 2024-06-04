from BB import *


# a = 1
# b = 2
# if a > b then
#   a = a - 1
# else
#   b = b - 1
# c = -1
# if c > 0 then
#   c = c - 1
#   return c
# else
#   c = 1
# a = 3
# return a
def example():
    b0 = BB()
    b0.block_num = 0
    tmp = b0.create_tmp_var()
    b0.variables = {'a': Variable('a', 0), 'b': Variable('b', 0)}
    b0.add_instr(Instruction("store", {'from': 1, 'to': Variable('a', 0)}))
    b0.add_instr(Instruction("store", {'from': 2, 'to': Variable('b', 0)}))
    b0.add_instr(Instruction('icmp', {'arg1': Variable('a', 0), 'arg2': Variable('b', 0), 'to': tmp}))
    b0.add_instr(Instruction('condbr', {'cond': tmp, 'dest1': 1, 'dest2': 2}))
    b0.returned = False

    print(b0)

    b1 = BB()
    b1.block_num = 1
    tmp = b1.create_tmp_var()
    b1.variables = {'a': Variable('a', 0), 'b': Variable('b', 0)}
    b1.add_instr(Instruction("sub", {'oper1': Variable('a', 0), 'oper2': '1', 'to': tmp}))
    b1.add_instr(Instruction("store", {'from': tmp, 'to': Variable('a', 0)}))
    b1.add_instr(Instruction('br', {'dest': 3}))
    b1.returned = False

    print(b1)

    b2 = BB()
    b2.block_num = 2
    tmp = b2.create_tmp_var()
    b2.variables = {'a': Variable('a', 0), 'b': Variable('b', 0)}
    b2.add_instr(Instruction("sub", {'oper1': Variable('b', 0), 'oper2': '1', 'to': tmp}))
    b2.add_instr(Instruction("store", {'from': tmp, 'to': Variable('b', 0)}))
    b2.add_instr(Instruction('br', {'dest': 3}))
    b2.returned = False

    print(b2)

    b3 = BB()
    b3.block_num = 3
    tmp = b3.create_tmp_var()
    b2.variables = {'a': Variable('a', 0), 'b': Variable('b', 0), 'c': Variable('c', 0)}
    b3.add_instr(Instruction("store", {'from': -1, 'to': Variable('c', 0)}))
    b3.add_instr(Instruction('icmp', {'arg1': Variable('c', 0), 'arg2': 0, 'to': tmp}))
    b3.add_instr(Instruction('condbr', {'cond': tmp, 'dest1': 4, 'dest2': 5}))
    b3.returned = False

    print(b3)

    b4 = BB()
    b4.block_num = 4
    tmp = b4.create_tmp_var()
    b4.variables = {'a': Variable('a', 0), 'b': Variable('b', 0), 'c': Variable('c', 0)}
    b4.add_instr(Instruction("sub", {'oper1': Variable('c', 0), 'oper2': '1', 'to': tmp}))
    b4.add_instr(Instruction("store", {'from': tmp, 'to': Variable('c', 0)}))
    # b4.add_instr(Instruction("ret", {'value': Variable('c',0)}))
    b4.add_instr(Instruction('br', {'dest': 6}))
    b4.returned = False

    print(b4)

    b5 = BB()
    b5.block_num = 5
    b5.varcounter = 0
    b5.variables = {'a': Variable('a', 0), 'b': Variable('b', 0), 'c': Variable('c', 0)}
    b5.add_instr(Instruction("store", {'from': 1, 'to': Variable('a', 0)}))
    b5.add_instr(Instruction('br', {'dest': 6}))
    b5.returned = False

    print(b5)

    b6 = BB()
    b6.block_num = 6
    b6.varcounter = 0
    b6.variables = {'a': Variable('a', 0), 'b': Variable('b', 0), 'c': Variable('c', 0)}
    b6.add_instr(Instruction("store", {'from': 3, 'to': Variable('a', 0)}))
    b6.add_instr(Instruction("ret", {'value': Variable('a', 0)}))
    b6.returned = True

    print(b6)

    return [b0, b1, b2, b3, b4, b5, b6]


# a = 1
# b = 2
# a = a - 1
# b = b - a
# return a
def example1():
    b0 = BB()
    b0.block_num = 0
    b0.varcounter = 2
    b0.variables = {'a': Variable('a', 0), 'b': Variable('b', 0)}
    b0.add_instr(Instruction("store", {'from': 1, 'to': Variable('a', 0)}))
    b0.add_instr(Instruction("store", {'from': 2, 'to': Variable('b', 0)}))
    b0.add_instr(Instruction("sub", {'oper1': Variable('a', 0), 'oper2': '1', 'to': 'tmp_1_0'}))
    b0.add_instr(Instruction("store", {'from': 'tmp_1_0', 'to': Variable('a', 0)}))
    b0.add_instr(Instruction("sub", {'oper1': Variable('a', 0), 'oper2': Variable('b', 0), 'to': 'tmp_1_0'}))
    b0.add_instr(Instruction("store", {'from': 'tmp_1_1', 'to': Variable('b', 0)}))
    b0.add_instr(Instruction("ret", {'value': Variable('a', 0)}))
    b0.returned = True

    print(b0)

    return [b0]
