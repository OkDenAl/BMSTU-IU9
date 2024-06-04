int main() {
    int a = 5;
    int b = 1;

    while (a < 10) {
        a += b;
    }

    return 0;
}

// gcc -fdump-tree-ssa-graph test/test.c
// dot -Tpng a-test.c.021t.ssa.dot > output.png