import math

import numpy as np

eps = 0.01


def jacobian_matrix(f, x):
    n = len(x)
    J = np.zeros((n, n))
    h = 1e-6

    for i in range(n):
        x_h = x.copy()
        x_h[i] += h
        J[:, i] = (f(x_h) - f(x)) / h

    return J


def newton_method(f, x0, max_iter=1000):
    x = x0
    for _ in range(max_iter):
        J = jacobian_matrix(f, x)
        dx = np.linalg.solve(J, -f(x))
        x += dx
        if np.linalg.norm(dx) < eps:
            return x
    return x


def equations(vars):
    x, y = vars
    eq1 = math.cos(y - 1) + x - 0.5
    eq2 = y - math.cos(x) - 3
    return np.array([eq1, eq2])


analit = np.array([1.207, 3.356])
initial_guess = np.array([1.0, 3.0])
sol = newton_method(equations, initial_guess)

x_sol, y_sol = sol

print(f"Аналитическое решение: x = {analit[0]}, y = {analit[1]}")
print(f"Метод Ньютона: x = {x_sol}, y = {y_sol}")
print(f"Погрешность: x = {abs(analit[0]-x_sol)}, y = {abs(analit[1]-y_sol)}")
