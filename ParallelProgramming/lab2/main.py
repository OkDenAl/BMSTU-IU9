import time
from mpi4py import MPI
import numpy as np
from numpy.linalg import norm, det
import sys

np.random.seed(42)

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

MATRIX_SIZE = 2 ** 13
MATRIX_SPLIT = int(sys.argv[1])

a = np.zeros((MATRIX_SIZE, MATRIX_SIZE), dtype=np.double)
for i in range(MATRIX_SIZE):
    for j in range(MATRIX_SIZE):
        if i == j:
            a[i, j] = 2
        else:
            a[i, j] = 1

# test 1
b = np.ones(MATRIX_SIZE, dtype=np.double) * (2 ** 13 + 1)
x = np.zeros(MATRIX_SIZE, dtype=np.double)

# test 2
# u = np.zeros(MATRIX_SIZE, dtype=np.double)
# for i in range(MATRIX_SIZE):
#     u[i] = np.random.random()
# b = np.matmul(a, u[:, None]).T[0]
# x = np.zeros(MATRIX_SIZE, dtype=np.double)

epsilon = 0.00001


def mult_matrix_by_vector(m, v):
    v = v[:, None]
    part_a = np.empty(shape=(MATRIX_SIZE // MATRIX_SPLIT,
                             MATRIX_SIZE), dtype=np.double)
    # передача частей на каждый процесс
    comm.Scatter(m, part_a, root=0)
    part_a = part_a @ v
    res = None
    if rank == 0:
        res = np.empty(shape=(MATRIX_SIZE, 1), dtype=np.double)
    # сбор частей
    comm.Gather(part_a, res, root=0)

    return comm.bcast(res, root=0).T[0]


def main():
    global x

    old_crit = 0
    i = 0
    while True:
        i += 1
        y = mult_matrix_by_vector(a, x) - b
        ay = mult_matrix_by_vector(a, y)
        flag = False
        if rank == 0:
            crit = norm(y) / norm(b)
            if crit < epsilon or crit == old_crit:
                flag = True
            else:
                old_crit = crit
                tao = (y.dot(ay)) / (ay.dot(ay))
                x = x - tao * y

        if comm.bcast(flag, root=0):
            break
        x = comm.bcast(x, root=0)


if __name__ == '__main__':
    t = time.time()
    main()
    if rank == 0:
        print(MATRIX_SPLIT, time.time() - t)
