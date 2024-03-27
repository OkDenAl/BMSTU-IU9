import math
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
        if 1 < i < n and abs(b[i]) < abs(a[i - 1]) + abs(c[i]):
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


def f():
    return 3.0


# через вольфрам альфа
def analytical(x):
    return 5 * (math.e ** x) - 3 * x - 5


p = -1.0
q = 0.0
a = analytical(0)
b = analytical(1)


def solve(n, h, a, p):
    as_ = []
    bs = []
    cs = []
    ds = []

    for i in range(1, n - 1):
        as_.append(1 - h / 2 * p)
        cs.append(1 + h / 2 * p)

    for i in range(1, n):
        bs.append(h * h * q - 2)

    ds.append(h * h * f() - a * (1 - h / 2 * p))
    for i in range(2, n):
        ds.append(h * h * f())
    ds[-1] = h * h * f() - b * (1 + h / 2 * p)

    alpha, beta = forward(as_, bs, cs, ds)
    ys = [a]
    ys.extend(backward(alpha, beta))
    ys.append(b)

    return ys


def main():
    print(f"y'' + {p}y' + {q}y = 5 * e^x - 3x - 5")
    print(f"y(0) = {a}\ny(1) = {b}")

    n = 40
    h = 1.0 / float(n)

    print(a,b)

    xs = []
    for i in range(n + 1):
        xs.append(float(i) * h)

    ys = solve(n, h, a, p)

    # for i in range(len(ys)):
    #     print(f"x={float(i) * h:.2f}, y={analytical(xs[i]):.6f},"
    #           f" y*={ys[i]:.6f}, |y-y*|={abs(ys[i] - analytical(xs[i])):.6f}")

    maxInaccuracy = 0.0
    for i in range(0, len(ys), 4):
        print(f"x={float(i) * h:.2f}, y={analytical(xs[i]):.6f},"
              f" y*={ys[i]:.6f}, |y-y*|={abs(ys[i] - analytical(xs[i])):.6f}")
        if abs(ys[i] - analytical(xs[i])) > maxInaccuracy:
            maxInaccuracy = abs(ys[i] - analytical(xs[i]))
    print(f"y-y={maxInaccuracy:.6f}")

if __name__ == "__main__":
    main()
