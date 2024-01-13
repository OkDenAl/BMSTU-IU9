from OpenGL.GL import *
import glfw
import numpy as np
import random

delta = 0
angle1, angle2, angle3 = 10, 10, 10
window = None
display = None
msh = 0.2

ta, tb = 0, 1

surfaces = (
    (0, 1, 2, 3),
    (3, 2, 7, 6),
    (6, 7, 5, 4),
    (4, 5, 1, 0),
    (1, 5, 7, 2),
    (4, 0, 3, 6)
)

verticies = np.array([
    [0.5, -0.5, -0.5],
    [0.5, 0.5, -0.5],
    [-0.5, 0.5, -0.5],
    [-0.5, -0.5, -0.5],
    [0.5, -0.5, 0.5],
    [0.5, 0.5, 0.5],
    [-0.5, -0.5, 0.5],
    [-0.5, 0.5, 0.5]
])

edges = (
    (0, 1),
    (0, 3),
    (0, 4),
    (2, 1),
    (2, 3),
    (2, 7),
    (6, 3),
    (6, 4),
    (6, 7),
    (5, 1),
    (5, 4),
    (5, 7)
)

to_cut = np.array([
    [1, 0.2, 1],
    [-0.1, 0, -1]
])

to_cut1 = np.array([
    [1, 0.2, 1],
    [-0.1, 0, -1]
])

def main():
    global window

    if not glfw.init():
        return

    window = glfw.create_window(1000, 1000, "Lab2", None, None)
    if not window:
        glfw.terminate()
        return

    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)
    while not glfw.window_should_close(window):
        display()

    glfw.destroy_window(window)
    glfw.terminate()


def key_callback(window, key, scancode, action, mods):
    global angle1, angle2, angle3, to_cut
    if action == glfw.PRESS:
        if key == glfw.KEY_A:
            angle1 -= 10
        elif key == glfw.KEY_D:
            angle1 += 10
        elif key == glfw.KEY_W:
            angle2 -= 10
        elif key == glfw.KEY_S:
            angle2 += 10
        elif key == glfw.KEY_Q:
            angle3 -= 10
        elif key == glfw.KEY_E:
            angle3 += 10
        elif key == glfw.KEY_1:
            to_cut = np.array([
                [-1, 0, 0.7],
                [1, 0, -0.7]
            ])
        elif key == glfw.KEY_2:
            to_cut = np.array([
                [0, 0, 0],
                [1, 0, -0.7]
            ])
        elif key == glfw.KEY_3:
            to_cut = np.array([
                [1, 0, -0.7],
                [0, 0, 0]
            ])
        elif key == glfw.KEY_4:
            to_cut = np.array([
                [0.5, 0.5, 1],
                [0.5, 0.5, -1]
            ])
        elif key == glfw.KEY_5:
            to_cut = np.array([
                [0, 0.7, 0],
                [0.7, 0, 0]
            ])


def cube():
    glPushMatrix()

    glScalef(msh, msh, msh)
    glRotatef(angle1, 0, 0, 1)
    glRotatef(angle2, 0, 1, 0)
    glRotatef(angle3, 1, 0, 0)

    glColor3fv((1.0, 0.0, 0.0))
    glBegin(GL_LINES)
    for edge in edges:
        for vertex in edge:
            glVertex3fv(verticies[vertex])
    glEnd()

    glColor3fv((0.0, 1.0, 0.0))
    glBegin(GL_LINES)
    for point in to_cut:
        glVertex3fv(point)
    glEnd()

    glColor3fv((0.0, 0.0, 1.0))
    glBegin(GL_LINES)
    glVertex3fv(to_cut[0])
    glVertex3fv(to_cut1[0])
    glVertex3fv(to_cut[1])
    glVertex3fv(to_cut1[1])
    glEnd()

    glPopMatrix()


def display():
    glClear(GL_COLOR_BUFFER_BIT)
    glLoadIdentity()
    glClearColor(0.0, 0.0, 0.0, 0.0)

    find_intersection()
    cube()

    glfw.swap_buffers(window)
    glfw.poll_events()

INSIDE = 0
BOTTOM = 1   #00 000001
LEFT = 2   #00 000010
TOP = 4   #00 000100
RIGHT = 8   #00 001000
BACK = 16   #00 010000
FRONT = 32   #00 100000

x_max = 0.5
y_max = 0.5
z_max= 0.5
x_min = -0.5
y_min = -0.5
z_min= -0.5


def computeCode(x, y,z):
    code = INSIDE
    if y > y_max:  # to the left of rectangle
        code |= TOP
    elif y < y_min:  # to the right of rectangle
        code |= BOTTOM
    if x > x_max:  # below the rectangle
        code |= RIGHT
    elif x < x_min:  # above the rectangle
        code |= LEFT
    if z > z_max:  # below the rectangle
        code |= FRONT
    elif z < z_min:  # above the rectangle
        code |= BACK
    return code

def find_intersection():
    global to_cut1
    global ta,tb
    a = to_cut[0]
    b = to_cut[1]
    x1=a[0]
    x2=b[0]
    y1=a[1]
    y2=b[1]
    z1=a[2]
    z2=b[2]
    code1 = computeCode(a[0],a[1],a[2])
    code2 = computeCode(b[0],b[1],b[2])
    accept = False
    while True:
        if code1 == 0 and code2 == 0:
            accept = True
            break
        elif (code1 & code2) != 0:
            break
        else:
            x = 1.0
            y = 1.0
            z = 1.0
            if code1 != 0:
                code_out = code1
            else:
                code_out = code2
            if code_out & TOP:
                x = x1 + (x2 - x1) * (y_max - y1) / (y2 - y1)
                z = z1 + (z2 - z1) * (y_max - y1) / (y2 - y1)
                y = y_max

            elif code_out & BOTTOM:
                x = x1 + (x2 - x1) * (y_min - y1) / (y2 - y1)
                z = z1 + (z2 - z1) * (y_min - y1) / (y2 - y1)
                y = y_min

            elif code_out & RIGHT:
                y = y1 + (y2 - y1) * (x_max - x1) / (x2 - x1)
                z = z1 + (z2 - z1) * (x_max - x1) / (x2 - x1)
                x = x_max

            elif code_out & LEFT:
                y = y1 + (y2 - y1) * (x_min - x1) / (x2 - x1)
                z = z1 + (z2 - z1) * (x_min - x1) / (x2 - x1)
                x = x_min

            elif code_out & FRONT:
                x = x1 + (x2 - x1) * (z_max - z1) / (z2 - z1)
                y = y1 + (y2 - y1) * (z_max - z1) / (z2 - z1)
                z=z_max
            elif code_out & BACK:
                x = x1 + (x2 - x1) * (z_min - z1) / (z2 - z1)
                y = y1 + (y2 - y1) * (z_min - z1) / (z2 - z1)
                z=z_min
            if code_out == code1:
                x1 = x
                y1 = y
                z1= z
                code1 = computeCode(x1, y1,z1)
            else:
                x2 = x
                y2 = y
                z2=z
                code2 = computeCode(x2, y2,z2)
    if accept:
        to_cut1[0][0] = x1
        to_cut1[0][1] = y1
        to_cut1[0][2]= z1
        to_cut1[1][0] = x2
        to_cut1[1][1] = y2
        to_cut1[1][2] = z2

main()