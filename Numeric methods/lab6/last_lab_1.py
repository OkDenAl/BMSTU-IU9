import numpy as np
from matplotlib import pyplot as plt


def f(x):
    return 3 * x ** 4 - 10 * x ** 3 + 21 * x ** 2 + 12 * x


def bisection_method(a, b, eps,delta):
    while abs(b - a) > eps:
        c = (a + b) / 2
        if f(c-delta) <= f(c+delta):
            b = c
        else:
            a = c
    return (a + b) / 2


def show_func():
    x = np.linspace(-0.5, 1, 100)  # генерируем значения x от -3 до 3
    y = f(x)  # вычисляем соответствующие значения функции

    plt.plot(x, y, label='f(x) = 3*x^4 - 10*x^3 + 21*x^2 + 12*x')
    plt.axhline(y=0, color='k', linestyle='--')  # добавляем горизонтальную линию y=0 для наглядности
    plt.xlabel('x')
    plt.ylabel('f(x)')
    plt.title('График функции f(x) = 3*x^4 - 10*x^3 + 21*x^2 + 12*x')
    plt.grid(True)
    plt.legend()
    plt.show()


show_func()
a = 0
b = 0.5
eps = 0.01

minimum_point = bisection_method(a, b, eps,0.001)

print("Minimum point by method: x =", minimum_point, ", y =", f(minimum_point))
print("Minimum point by analyst: x =", 0,  ", y =", f(0))
print("Difference dx=",minimum_point-0, ", dy =", f(minimum_point)-f(0))
