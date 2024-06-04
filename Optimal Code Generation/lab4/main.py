from ssa import SsaBuilder
from IR import *

if __name__ == '__main__':
    blocks = example()

    ssab = SsaBuilder(blocks)
    ssab.insert_all_phi()
    ssab.update_variable_versions()
    ssab.print_blocks()

    graphname = f'results/prog1.dot'
    with open(graphname, 'w') as f:
        f.write(ssab.to_graph())