import math
from scipy.integrate import quad

EPS = 0.001
a, b = 0, 1


def func(x):
    return math.e ** (x)
    # return math.log(2 * x, math.e)


def richardson_formula(I_h, I_h2, k):
    return (I_h - I_h2) / (2 ** k - 1)


def trapezoid_method(f, n):
    h = (b - a) / n
    s = 0
    for i in range(1, n):
        s += f(a + i * h)
    result = h * ((f(a) + f(b)) / 2 + s)
    return result


def rectangle_method(f, n):
    h = (b - a) / n
    s = 0
    for i in range(1, n + 1):
        s += f(a + (i - 0.5) * h)
    result = h * s
    return result


def simpson_method(f, n):
    h = (b - a) / n
    s1, s2, s3 = 0, 0, 0
    for i in range(1, n + 2):
        if i == n:
            s2 += f(a + (i - 0.5) * h)
            s3 += f(a + (i - 1) * h)
        elif i == n + 1:
            s3 += f(a + (i - 1) * h)
        else:
            s1 += f(a + i * h)
            s2 += f(a + (i - 0.5) * h)
            s3 += f(a + (i - 1) * h)
    result = h / 6 * (s1 + 4 * s2 + s3)
    return result


def res(method, k):
    n_arr, I_h_arr, R_arr = [], [], []
    n = 1
    R = 1
    I_h = 0
    while not (abs(R) < EPS):
        n *= 2
        I_h2 = I_h
        I_h = method(func, n)
        R = richardson_formula(I_h, I_h2, k)
        n_arr.append(n)
        I_h_arr.append(I_h)
        R_arr.append(R)
    return n_arr, I_h_arr, R_arr


def main():
    (result, error) = quad(func, a, b)  # 8.03667754145488
    n_rec, I_h_rec, R_rec = res(rectangle_method, 2)
    n_trap, I_h_trap, R_trap = res(trapezoid_method, 2)
    n_sim, I_h_sim, R_sim = res(simpson_method, 4)
    print(f"EPS= {EPS} \t I = {result}")
    print("\tCentral rectangles \t  Trapezoids method \t Simpson`s")
    print(f"n \t\t{n_rec[len(n_rec)-1]}\t\t\t\t\t{n_trap[len(n_trap)-1]}\t\t\t\t\t\t\t{n_sim[len(n_sim)-1]}")
    print(f"I*\t{I_h_rec[len(I_h_rec)-1]}\t{I_h_trap[len(I_h_trap)-1]}\t\t\t{I_h_sim[len(n_sim)-1]}")
    print(f"R \t{R_rec[len(R_rec)-1]}\t{R_trap[len(R_trap)-1]}\t\t{R_sim[len(n_sim)-1]}")
    print(f"I*+R \t{I_h_rec[len(I_h_rec)-1]+R_rec[len(R_rec) - 1]}\t{I_h_trap[len(I_h_trap)-1]+R_trap[len(R_trap) - 1]}\t\t{I_h_sim[len(n_sim)-1]+R_sim[len(n_sim) - 1]}\n")
    print()
    # for i in range(len(n_rec)):
    #     print(f"EPS = {EPS} \t I = {result}")
    #     if i >= len(n_sim):
    #         print("\tCentral rectangles \t  Trapezoids method \t Simpson`s")
    #         print(f"n \t\t{n_rec[i]}\t\t\t\t\t{n_trap[i]}\t\t\t\t\t\t\t-")
    #         print(f"I*\t{I_h_rec[i]}\t{I_h_trap[i]}\t\t\t-")
    #         print(f"R \t{R_rec[i]}\t{R_trap[i]}\t\t-\n")
    #     else:
    #         print("\tCentral rectangles \t  Trapezoids method \t Simpson`s")
    #         print(f"n \t\t{n_rec[i]}\t\t\t\t\t{n_trap[i]}\t\t\t\t\t\t\t{n_sim[i]}")
    #         print(f"I*\t{I_h_rec[i]}\t{I_h_trap[i]}\t{I_h_sim[i]}")
    #         print(f"R \t{R_rec[i]}\t{R_trap[i]}\t{R_sim[i]}\n")


if __name__ == "__main__":
    main()
