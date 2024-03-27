import math

import numpy as np
from matplotlib import pyplot as plt


def find_lambd(A, b):
    return (np.linalg.inv(A).dot(b.T)).T


def find_A(xs, n, m):
    return np.array([[sum(xs[k] ** (i + j) for k in range(0, n + 1)) for j in range(0, m)] for i in range(0, m)])


def find_b(xs, ys, n, m):
    return np.array([sum(ys[k] * (xs[k] ** i) for k in range(0, n + 1)) for i in range(0, m)])


def z(lambd, x, m):
    return sum([lambd[i] * x ** i for i in range(m)])


def count_a_g_h(arr):
    a = (arr[0] + arr[len(arr) - 1]) / 2
    g = math.sqrt(arr[0] * arr[len(arr) - 1])
    h = 2 / ((1 / arr[0]) + (1 / arr[len(arr) - 1]))
    return a, g, h


def find_z_num(z):
    nums = [
        [1, 3, 6],
        [4, 2, 9],
        [5, 8, 7]
    ]

    x_a, x_g, x_h = count_a_g_h(x)
    y_a, y_g, y_h = count_a_g_h(y)

    print(f'x_a = {x_a}, x_g = {x_g}, x_h = {x_h}')
    print(f'y_a = {y_a}, y_g = {y_g}, y_h = {y_h}')
    print(f'z_x_a = {z(x_a)}, z_x_g = {z(x_g)}, z_x_h = {z(x_h)}')

    x_ = [x_a, x_g, x_h]
    y_ = [y_a, y_g, y_h]
    min_ = 10000
    min_idx = (0, 0)
    for i in range(len(x_)):
        for j in range(len(y_)):
            if abs(z(x_[i]) - y_[j]) < min_:
                min_idx = (i, j)
                min_ = abs(z(x_[i]) - y_[j])
    return nums[min_idx[0]][min_idx[1]]


def show_z(z, X, Y):
    plt.scatter(X, Y)
    x_ = np.linspace(1, 5, 10)
    y_ = list(map(z, x_))
    plt.plot(x_, y_)


def new_z(x, y, n, m):
    x_ln = list(map(np.log, x))
    y_swap = list(map(lambda x_var: 1 / x_var, y))

    A = find_A(x_ln, n, m)
    b = find_b(x_ln, y_swap, n, m)

    lambd = find_lambd(A, b)
    return lambda x_var: 1 / (lambd[0] + lambd[1] * math.log(x_var, math.e))


if __name__ == "__main__":
    x = [1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5]
    y = [1.24, 1.74, 1.61, 2.16, 3.06, 2.88, 4.53, 5.40, 7.07]

    m = 4
    n = len(x) - 1

    A = find_A(x, n, m)
    b = find_b(x, y, n, m)
    lambd = find_lambd(A, b)
    z_ = lambda p: z(lambd, p, m)

    D = sum([(y[k] - z_(x[k])) for k in range(m + 1)]) ** 2
    D = np.sqrt(D) / np.sqrt(n)
    print("\nСКО z:", D)

    show_z(z_, x, y)
    z_num = find_z_num(z_)
    print(f'k = {z_num}')
    z_new = new_z(x, y, n, m)

    D = sum([(y[k] - z_new(x[k])) for k in range(m + 1)]) ** 2
    D = np.sqrt(D) / np.sqrt(n)
    print("\nСКО z_new:", D)

    show_z(z_new, x, y)
    plt.show()
