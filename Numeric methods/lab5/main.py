import math
eps = 0.001
def f(x, y):
    return (2 * x ** 2) + (3 * y ** 2) - 2 * math.sin((x - y) / 2) + y
def analytical_min():
    return 0.24003045, -0.326686970012716
def f_x(x, y):
    return 4 * x - math.cos(x / 2 - y / 2)
def f_y(x, y):
    return 6 * y - math.cos(x / 2 - y / 2) + 1
def f_x_x(x, y):
    return math.sin(x / 2 - y / 2) / 2 + 4
def f_x_y(x, y):
    return math.sin(x / 2 - y / 2) / 2

def f_y_y(x, y):
    return math.sin(x / 2 - y / 2) / 2 + 6
k=0
xk, yk = 0.0, 0.0
while max(abs(f_x(xk, yk)), abs(f_y(xk, yk))) >= eps:
    phi1 = - (f_x(xk, yk)) ** 2 - (f_y(xk, yk)) ** 2
phi2 = (f_x_x(xk, yk) * (f_x(xk, yk)) ** 2 + 2 * f_x_y(xk, yk) * f_x(xk, yk) * f_y(xk, yk) + f_y_y(xk, yk) * (
    f_y(xk, yk)) ** 2)
t_star = - phi1 / phi2
xk = xk - t_star * f_x(xk, yk) yk = yk - t_star * f_y(xk, yk)
print(f'methods min {xk, yk}')
print(f'analytical min: {analytical_min()}')
print(f'difference: {math.fabs(xk - analytical_min()[0]), math.fabs(yk - analytical_min()[1])}')