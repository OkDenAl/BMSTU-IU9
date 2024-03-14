import math

SIZE = 8


def f(x):
    return math.log(2 * x, math.e)


def forward(a, b, c, d):
    if b[0] == 0 or b[len(b) - 1] == 0:
        raise Exception("invalid b data, cant calculate forward")
    if abs(c[0]) / abs(b[0]) > 1 or abs(a[len(b) - 1 - 1]) / abs(b[len(b) - 1]) > 1:
        raise Exception("invalid matrix data, cant calculate forward")

    y = [0.0] * len(b)
    alpha = [0.0] * len(b)
    beta = [0.0] * len(b)
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
    l, r = 0.5, 2 * math.e
    h = (r - l) / SIZE
    xs = []
    ys = []
    i = l
    while i < r:
        xs.append(i)
        ys.append(f(i))
        i += h

    if len(xs)<SIZE + 1:
        xs.append(r)
        ys.append(f(r))

    for i in range(SIZE + 1):
        print(xs[i], ";", ys[i])

    print()

    d = []
    for i in range(1, SIZE):
        d.append(3 * (ys[i + 1] - 2 * ys[i] + ys[i - 1]) / (h * h))

    b = [4] * (SIZE - 1)
    a = [1] * (SIZE - 2)
    c = [1] * (SIZE - 2)
    alpha, beta = forward(a, b, c, d)
    coefC = backward(alpha, beta)
    coefC.insert(0, 0)
    coefC.append(0)

    coefA = []
    for i in range(SIZE):
        coefA.append(ys[i])

    coefB = []
    for i in range(1, SIZE + 1):
        coefB.append((ys[i] - ys[i - 1]) / h - (h / 3) * (coefC[i] + 2 * coefC[i - 1]))

    coefD = []
    for i in range(1, SIZE + 1):
        coefD.append((coefC[i] - coefC[i - 1]) / (3 * h))

    print(f'a: {coefA}\nb: {coefB}\nc: {coefC}\nd: {coefD}\n')

    for i in range(SIZE):
        var_x = l + i * h
        var_y = ys[i]
        s = coefA[i] + coefB[i] * (var_x - xs[i]) + coefC[i] * ((var_x - xs[i]) ** 2) + coefD[i] * (
                    (var_x - xs[i]) ** 3)
        print(f'x: {var_x}, y: {var_y}, y*: {s}, |y-y*|: {math.fabs(var_y - s)}')

    print()
    for i in range(SIZE):
        var_x = l + (i + 0.5) * h
        var_y = f(var_x)
        s = coefA[i] + coefB[i] * (var_x - xs[i]) + coefC[i] * ((var_x - xs[i]) ** 2) + coefD[i] * (
                    (var_x - xs[i]) ** 3)
        print(f'x: {var_x}, y: {var_y} y*: {s},|y-y*|: {math.fabs(var_y - s)}')


if __name__ == '__main__':
    main()
