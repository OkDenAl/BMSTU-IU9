import math

from OpenGL.GL import *
from OpenGL.GLU import *
import glfw


mode=0
size=0.5
fi=0
tetha=0

def drawSphere(r, lats, longs) :
    i=0
    j=0
    for i in range(lats+1):
        lat0 = math.pi*(-0.5 + (i - 1) / lats)
        z0 = math.sin(lat0)
        zr0 = math.cos(lat0)

        lat1 = math.pi * (-0.5 + i / lats)
        z1 = math.sin(lat1)
        zr1 = math.cos(lat1)

        glBegin(GL_QUAD_STRIP)
        for j in range(longs+1):
            lng = 2 * math.pi *  (j - 1) / longs
            x = math.cos(lng)
            y = math.sin(lng)

            glColor3f(0.0, 0.0, 1)
            glVertex3f(r * x * zr0, r * y * zr0, r * z0*1.2);
            glColor3f(0.0, 0, 1)
            glVertex3f(r * x * zr1, r * y * zr1, r * z1*1.2)
        glEnd()


def display(window):
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    if mode == 1:
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    else:
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)

    rotate1 = [math.cos(fi), math.sin(fi) * math.sin(tetha), math.sin(fi) * math.cos(tetha), 0,
               0, math.cos(tetha), -math.sin(tetha), 0,
               math.sin(fi), -math.cos(fi) * math.sin(tetha), -math.cos(fi) * math.cos(tetha), 0,
               0, 0, 0, 1]
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glMultMatrixf(rotate1)
    drawSphere(0.8,30,30)

    glfw.swap_buffers(window)
    glfw.poll_events()


def key_callback(window, key, scancode, action,mods):
    global fi,tetha,mode
    if action == glfw.PRESS or action==glfw.REPEAT:
        if key == glfw.KEY_LEFT:
            fi -= 0.1
        if key == glfw.KEY_RIGHT:
            fi += 0.1
        if key == glfw.KEY_UP:
            tetha -= 0.1
        if key == glfw.KEY_DOWN:
            tetha += 0.1
        if key == glfw.KEY_SPACE:
            mode = 1
        if key == glfw.KEY_ESCAPE:
            mode = 0


def main2():
    if not glfw.init():
        return
    window = glfw.create_window(640, 640, "Lab2", None, None)
    if not window:
        glfw.terminate()
        return
    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)
    while not glfw.window_should_close(window):
        display(window)
    glfw.destroy_window(window)
    glfw.terminate()

if __name__ == "__main__":
    main2()