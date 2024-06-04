import networkx as nx
from copy import deepcopy
from BB import *


class SsaBuilder:
    def __init__(self, blocks):
        self.blocks = blocks
        self.build_dom()
        self.build_df()
        self.build_changed_variables()

    # КОНСТРУКТОРЫ

    def build_dom(self):
        edges = set.union(*[x.get_edges() for x in self.blocks])
        CFG = nx.DiGraph(edges)
        for x in self.blocks_to_nums(self.blocks):
            CFG.add_node(x)
        cc = nx.node_connected_component(CFG.to_undirected(), 0)
        self.CFG = nx.DiGraph(CFG.subgraph(cc))
        self.blocks = set(filter(lambda x: x.block_num in self.CFG, self.blocks))

        imm_dom = nx.immediate_dominators(self.CFG, 0)
        self.dom_of = dict([(x, {imm_dom[x]}) for x in self.CFG])

        self.children = dict([(x, set()) for x in self.CFG])
        for x, ys in self.dom_of.items():
            for y in ys:
                if y != x:
                    self.children[y].add(x)

    def build_df(self):
        d = {}
        self.df = dict([(bb, set()) for bb in self.CFG])
        old = dict()
        while old != self.df:
            old = deepcopy(self.df)
            for x in self.blocks_to_nums(self.blocks):
                for y in self.get_succ(x):
                    if x not in self.dom_of[y]:
                        self.df[x].add(y)
                children = self.children[x]
                if str(x) + ' ->' not in d:
                    d[str(x) + ' ->'] = children
                for z in children:
                    for y in self.df[z]:
                        if y not in self.children[x]:
                            self.df[x].add(y)

        assert self.df == nx.dominance_frontiers(self.CFG, 0)  # совпадает ли с библиотечной

    def build_changed_variables(self):
        for x in self.blocks:
            x.build_changing_variables()

    # UTILS

    def print_blocks(self):
        for bb in self.blocks:
            print(bb)

    def to_graph(self):
        ret = "digraph G{\nnode [shape=box nojustify=false]\n"
        for x in self.blocks:
            s = str(x).replace('    ', '').replace('{', '').replace('}', '').replace("\n", "\\l    ").strip()
            while s[-2:] == '\\l':
                s = s[:-2].strip()
            ret += f'{x.block_num} [label=\"{s}\"]\n'
            last = x.instructions[-1]
            if last.typ == BR:
                ret += f'{x.block_num} -> {last.args["dest"]}\n'
            elif last.typ == CONDBR:
                ret += f'{x.block_num} -> {last.args["dest1"]} [label=true]\n'
                ret += f'{x.block_num} -> {last.args["dest2"]} [label=false]\n'
        ret += "}\n"
        return ret

    def get_block(self, n):
        return list(filter(lambda x: x.block_num == n, self.blocks))[0]

    def blocks_to_nums(self, s):
        return set(map(lambda bb: bb.block_num, s))

    def nums_to_bloks(self, nums):
        return set(filter(lambda bb: bb.block_num in nums, self.blocks))

    def get_all_vars_names(self):
        return set.union(*[set(bb.variables.keys()) for bb in self.blocks])

    def get_preds(self, node):
        if isinstance(node, BB):
            node = node.block_num
        return set(self.CFG.predecessors(node))

    def get_succ(self, node):
        if isinstance(node, BB):
            node = node.block_num
        return set(self.CFG.successors(node))

    def find_blocks_that_redefine_var(self, varname):
        s = set()
        for bb in self.blocks:
            for var in bb.changing_variables:
                if var.name == varname:
                    s.add(bb)
                    break
        return s

    # РАЗМЕЩЕНИЕ PHI-ФУНКЦИЙ

    def find_df(self, s):
        ret = set()
        for x in s:
            ret.update(self.df[x])
        return ret

    def find_df_post_order(self, s):
        old = set()
        new = self.find_df(s)
        while True:
            old |= new
            new = self.find_df(new)
            if new.difference(old) == set():
                return old

    def find_post_order(self, s):
        return self.find_df_post_order(s)

    def insert_phi(self, varname):
        stored_in_blocks = self.find_blocks_that_redefine_var(varname)
        stored_in_blocks_num = self.blocks_to_nums(stored_in_blocks)

        post_order_blocks_num = self.find_post_order(stored_in_blocks_num)
        post_order_blocks = self.nums_to_bloks(post_order_blocks_num)

        for bb in post_order_blocks:
            bb.phi_var_blocks[varname] = set()
            preds = self.CFG.predecessors(bb.block_num)
            for pred in preds:
                bb.phi_var_blocks[varname].add(pred)
            # JS = post_order_blocks | stored_in_blocks
            # JS_nums = self.blocks_to_nums(JS)
            # for phi_parent in JS:
            #     G = deepcopy(self.CFG)
            #     for rnode in JS_nums.difference({bb.block_num, phi_parent.block_num}):
            #         G.remove_node(rnode)
            #     if (phi_parent.block_num != bb.block_num) and nx.has_path(G, phi_parent.block_num, bb.block_num):
            #         bb.phi_var_blocks[varname].add(phi_parent.block_num)

    def insert_all_phi(self):
        vars = self.get_all_vars_names()
        vars_ = []
        for v in vars:
            vars_.append(v)
        vars_.sort()
        for varname in vars_:
            self.insert_phi(varname)

        for bb in self.blocks:
            for varname, phiblocks in bb.phi_var_blocks.items():
                instr = Instruction(PHI, {'to': Variable(varname, 0), 'from': list(phiblocks)})
                bb.instructions.insert(0, instr)

    # ОБНОВЛЕНИЕ ВЕРСИЙ ПЕРЕМЕННЫХ

    def update_variable_versions(self):
        self.traverse()

    def traverse(self):
        vars = self.get_all_vars_names()
        vars_ = []
        for v in vars:
            vars_.append(v)
        vars_.sort()

        for target_var in vars_:
            self.stack = []
            self.counter = 0
            self.traverse_rec(0, target_var)

    def which_pred(self, v, v1):
        preds = list(self.get_preds(v1))
        preds.sort()
        return preds.index(v)

    def traverse_rec(self, bb, target_var):
        print("->>> IN BLOCK", bb)

        for i, instr in enumerate(self.get_block(bb).instructions):
            for key, val in instr.args.items():
                if not isinstance(val, Variable) or val.is_temp or val.name != target_var:
                    continue
                name = val.name
                if instr.typ == STORE:
                    new_ver = self.counter
                    self.stack.append(self.counter)
                    self.counter += 1
                    self.get_block(bb).instructions[i].args['to'] = Variable(name, new_ver)
                if instr.typ == PHI:
                    new_ver = self.counter
                    self.stack.append(self.counter)
                    self.counter += 1
                    self.get_block(bb).instructions[i].args['to'] = Variable(name, new_ver)
                if instr.typ != PHI:
                    self.get_block(bb).instructions[i].args[key] = Variable(name, self.stack[-1])

        successors = self.get_succ(bb)
        for v1 in successors:
            j = self.which_pred(bb, v1)
            for instr in self.get_block(v1).instructions:
                if instr.typ != PHI or instr.args['to'].name != target_var:
                    continue
                instr.args['from'][j] = (Variable(target_var, self.stack[-1]))

        children = self.children[bb]
        for v1 in children:
            self.traverse_rec(v1, target_var)

        for instr in self.get_block(bb).instructions:
            if instr.typ == STORE and instr.args['to'].name == target_var:
                self.stack.pop()
