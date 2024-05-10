import numpy as np
import matplotlib.pyplot as plt


def func(x):
    return x ** 3 - 5 * x - 1


def bisect_root(a, b, tolerance=1e-6):
    assert func(a) * func(b) < 0, "The function must change sign between a and b"

    while abs(b - a) > tolerance:
        mid = (a + b) / 2
        if func(mid) == 0:
            return mid
        elif func(a) * func(mid) < 0:
            b = mid
        else:
            a = mid
    return (a + b) / 2


def derivative(x):
    return 3 * x ** 2 - 5


def newton_root(initial_guess, tolerance=1e-6):
    x = initial_guess

    while abs(func(x)) > tolerance:
        x = x - func(x) / derivative(x)

    return x

def show_func():
    x = np.linspace(-3, 3, 100)  # генерируем значения x от -3 до 3
    y = func(x)  # вычисляем соответствующие значения функции

    plt.plot(x, y, label='f(x) = x^3 - 5x - 1')
    plt.axhline(y=0, color='k', linestyle='--')  # добавляем горизонтальную линию y=0 для наглядности
    plt.xlabel('x')
    plt.ylabel('f(x)')
    plt.title('График функции f(x) = x^3 - 5x - 1')
    plt.grid(True)
    plt.legend()
    plt.show()


def main():
    show_func()
    xs = [-3, -2, -1, 1, 2, 3]
    res_bisec = []
    i = 0
    print("Метод деления пополам xs:")
    while i < len(xs):
        res_bisec.append(bisect_root(xs[i],xs[i+1]))
        i += 2
    print(res_bisec)


    xs = [-2.12841906,-0.20163967,2.33]
    print("Аналитические точки xs:")
    print(xs)

    print("Разность xs:")
    print([abs(xs[0]-res_bisec[0]),abs(xs[1]-res_bisec[1]),abs(xs[2]-res_bisec[2])])


    # print()
    #
    # print("Метод Ньютона:")
    # roots = [-2,0,2]
    # res_newton = []
    # for root in roots:
    #     res_newton.append(newton_root(root))
    # print(res_newton)
    #
    # print()
    # print("Разность:")
    # razn=[]
    # for i in range(len(res_newton)):
    #     razn.append(abs(res_newton[i]-res_bisec[i]))
    # print(razn)

if __name__ == '__main__':
    main()
