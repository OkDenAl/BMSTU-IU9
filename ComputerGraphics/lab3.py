import math
import os

import pyglet
from OpenGL.GL import *
from OpenGL.GLU import *
import glfw


mode=0
size=0.5
fi=0
tetha=0

def DrawCube(size):
    glBegin(GL_QUADS)

    glColor3f(0.0, size, 0.0)
    glVertex3f(size, size, -size)
    glVertex3f(-size, size, -size)
    glVertex3f(-size, size, size)
    glVertex3f(size, size, size)

    glColor3f(size, 0.0, 0.0)
    glVertex3f(size, -size, size)
    glVertex3f(-size, -size, size)
    glVertex3f(-size, -size, -size)
    glVertex3f(size, -size, -size)

    glColor3f(0.0, size, size)
    glVertex3f(size, size, size)
    glVertex3f(-size, size, size)
    glVertex3f(-size, -size, size)
    glVertex3f(size, -size, size)

    glColor3f(size, size, 0.0)
    glVertex3f(size, -size, -size)
    glVertex3f(-size, -size, -size)
    glVertex3f(-size, size, -size)
    glVertex3f(size, size, -size)

    glColor3f(0.0, 0.0, size)
    glVertex3f(-size, size, size)
    glVertex3f(-size, size, -size)
    glVertex3f(-size, -size, -size)
    glVertex3f(-size, -size, size)

    glColor3f(size, 0.0, size)
    glVertex3f(size, size, -size)
    glVertex3f(size, size, size)
    glVertex3f(size, -size, size)
    glVertex3f(size, -size, -size)

    glEnd()


def display(window):
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    if mode==1:
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    else:
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    translate1 = [0.87, -0.09, 0.98, 0.49, 0, 0.98, 0.35, 0.17, 0.5, 0.15, -1.7, -0.85, 0, 0, 1, 2]
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glMultMatrixf(translate1)

    translate=[0.4,0,0,0,0,0.4,0,0,0,0,0.4,0,1,1,1,1]
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glMultMatrixf(translate)
    DrawCube(size)

    rotate1=[math.cos(fi),math.sin(fi)*math.sin(tetha),math.sin(fi)*math.cos(tetha),0,
             0,math.cos(tetha),-math.sin(tetha),0,
             math.sin(fi),-math.cos(fi)*math.sin(tetha),-math.cos(fi)*math.cos(tetha),0,
             0,0,0,1]
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glMultMatrixf(rotate1)
    DrawCube(size)

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

image = pyglet.image.load(os.path.join(os.getcwd(), "./media/pic.bmp"))
tex = image.get_texture()
glBindTexture(tex.target, tex.id)

glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, image.width, image.height, 0,
             GL_RGBA, GL_UNSIGNED_BYTE, image.get_image_data().get_data("RGBA", image.width * 4))

glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE)

def main1():
    if not glfw.init():
        return
    window = glfw.create_window(640, 640, "Lab3", None, None)
    if not window:
        glfw.terminate()
        return
    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)
    while not glfw.window_should_close(window):
        display(window)
    glfw.destroy_window(window)
    glfw.terminate()

main1()