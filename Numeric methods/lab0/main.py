import numpy as np
from decimal import Decimal, getcontext

getcontext().prec = 25


def forward(a, b, c, d):
    if b[0] == 0 or b[len(b) - 1] == 0:
        raise Exception("invalid b data, cant calculate forward")
    if abs(c[0]) / abs(b[0]) > 1 or abs(a[len(b) - 1 - 1]) / abs(b[len(b) - 1]) > 1:
        raise Exception("invalid matrix data, cant calculate forward")

    y = [Decimal(0.0)] * len(b)
    alpha = [Decimal(0.0)] * len(b)
    beta = [Decimal(0.0)] * len(b)
    y[0] = b[0]
    alpha[0] = -c[0] / b[0]
    beta[0] = d[0] / b[0]

    n = len(b) - 1
    for i in range(len(b)):
        if i > 1 and i < n and abs(b[i]) < abs(a[i - 1]) + abs(c[i]):
            raise Exception("invalid matrix data, cant calculate forward")

        if i == 0:
            continue
        elif i == n:
            y[n] = b[n] + a[n - 1] * alpha[n - 1]
            beta[n] = (d[n] - a[n - 1] * beta[n - 1]) / y[n]
        else:
            y[i] = b[i] + alpha[i - 1] * a[i - 1]
            alpha[i] = -c[i] / y[i]
            beta[i] = (d[i] - a[i - 1] * beta[i - 1]) / y[i]

    return alpha, beta


def backward(alpha, beta):
    x = [0] * len(beta)
    n = len(beta) - 1
    x[n] = beta[n]
    for i in range(n - 1, -1, -1):
        x[i] = alpha[i] * x[i + 1] + beta[i]

    return x


def main():
    D = [Decimal(5.0 / 9.0), Decimal(6.0 / 9.0), Decimal(6.0 / 9.0), Decimal(5.0 / 9.0)]
    B = [Decimal(4.0 / 9.0), Decimal(4.0 / 9.0), Decimal(4.0 / 9.0), Decimal(4.0 / 9.0)]
    C = [Decimal(1.0 / 9.0), Decimal(1.0 / 9.0), Decimal(1.0 / 9.0)]
    A = [Decimal(1.0 / 9.0), Decimal(1.0 / 9.0), Decimal(1.0 / 9.0)]

    # D = [5.0 / 9.0, 6.0 / 9.0, 6.0 / 9.0, 5.0 / 9.0]
    # B = [4.0 / 9.0, 4.0 / 9.0, 4.0 / 9.0, 4.0 / 9.0]
    # C = [1.0 / 9.0, 1.0 / 9.0, 1.0 / 9.0]
    # A = [1.0 / 9.0, 1.0 / 9.0, 1.0 / 9.0]

    alpha, beta = forward(A, B, C, D)
    x_ = backward(alpha, beta)
    print("ответ:", x_)


if __name__ == "__main__":
    main()
