import math
import time

import glfw
from OpenGL.GL import *
from OpenGL.GLUT import *
import pygame

window_width = 800
window_height = 800
scale = 0.325

animation_mode = False
texture_sides = None

fi=0
tetha=0

flying_speed = 0
V = 0.0009*10
acl = 0.00006*5

light_mode = False
texture_mode = 1
filling_mode = True



def main():
    if not glfw.init():
        return
    window = glfw.create_window(window_width, window_height, "Lab6", None, None)
    if not window:
        glfw.terminate()
        return
    glfw.make_context_current(window)

    glfw.set_key_callback(window, key_callback)
    glfw.set_mouse_button_callback(window, mouse_callback)
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    generate_texture()
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    light()
    for _ in range(300):
        display(window)
    glfw.destroy_window(window)
    glfw.terminate()



def renderSphere(cx,cy, cz, r,p):
    glColor3f(1.0, 1.0, 1.0)
    TWOPI  = 2*math.pi
    PIDIV2 = math.pi/2
    for i in range (p//2+1):
        theta1 = i * TWOPI / p - PIDIV2
        theta2 = (i + 1) * TWOPI / p - PIDIV2

        glBegin( GL_TRIANGLE_STRIP )
        for j in range(p+1):
            theta3 = j * TWOPI / p

            ex = math.cos(theta2) * math.cos(theta3)
            ey = math.sin(theta2)
            ez = math.cos(theta2) * math.sin(theta3)
            px = cx + r * ex
            py = cy + r * ey
            pz = cz + r * ez

            glNormal3f( ex, ey, ez )
            glTexCoord2f( -(j/p) , 2*(i+1)/p )
            glVertex3f( px, py, pz*1.2 )

            ex = math.cos(theta1) * math.cos(theta3)
            ey = math.sin(theta1)
            ez = math.cos(theta1) * math.sin(theta3)
            px = cx + r * ex
            py = cy + r * ey
            pz = cz + r * ez

            glNormal3f( ex, ey, ez )
            glTexCoord2f( -(j/p), 2*i/p )
            glVertex3f( px, py, pz*1.2 )
        glEnd()



def display(window):
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    glClearColor(0.0, 0.0, 0.0, 0.0)
    glLoadIdentity()
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    if animation_mode:
        move_object()

    glScale(scale, scale, scale)

    glTranslatef(0, flying_speed, 0)
    glRotatef(fi, 1, 0, 0)
    glRotatef(tetha, 0, 1, 0)
    renderSphere(0, 0, 0, 0.8, 40)
    # drawSphere(0.8,30,30)
    glfw.swap_buffers(window)
    glfw.poll_events()


def key_callback(window, key, scancode, action, mods):
    global x_angle, y_angle, scale, animation_mode, fi , tetha
    if action == glfw.PRESS and key == glfw.KEY_ENTER:
        mode = glGetIntegerv(GL_POLYGON_MODE)
        if mode[1] == GL_LINE:
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        else:
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    if action == glfw.PRESS or action == glfw.REPEAT:
        if key == glfw.KEY_A:
            fi -= 2
        if key == glfw.KEY_D:
            fi += 2
        if key == glfw.KEY_W:
            tetha -= 2
        if key == glfw.KEY_S:
            tetha += 2
        if key == glfw.KEY_UP:
            scale += 0.05
        if key == glfw.KEY_DOWN:
            scale -= 0.05

        global light_mode
        if key == glfw.KEY_L:
            if glIsEnabled(GL_LIGHTING):
                glDisable(GL_LIGHTING)
            else:
                glEnable(GL_LIGHTING)
            return
        if key == glfw.KEY_M:
            animation_mode = not animation_mode
            return


def mouse_callback(window, button, action, mods):
    global filling_mode, texture_mode
    if action == glfw.PRESS:
        if button == glfw.MOUSE_BUTTON_LEFT:
            filling_mode = not filling_mode
            if filling_mode:
                glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
            else:
                glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        elif button == glfw.MOUSE_BUTTON_RIGHT:
            texture_mode = not texture_mode
            if texture_mode:
                glBindTexture(GL_TEXTURE_2D, texture_sides)
            else:
                glBindTexture(GL_TEXTURE_2D, 0)


def move_object():
    global V, flying_speed, acl
    flying_speed -= V
    V += acl
    if flying_speed < -2.2 or flying_speed > 2.2:
        V = -V


def generate_texture():
    textureSurface = pygame.image.load('eath.jpg')
    textureData = pygame.image.tostring(textureSurface, "RGBA", 1)
    width = textureSurface.get_width()
    height = textureSurface.get_height()

    glEnable(GL_TEXTURE_2D)
    texid = glGenTextures(1)

    glBindTexture(GL_TEXTURE_2D, texid)

    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height,
                 0, GL_RGBA, GL_UNSIGNED_BYTE, textureData)

def light():
    glEnable(GL_LIGHTING)
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, [[0.2, 0.2, 0.2, 1]])
    glLightfv(GL_LIGHT0, GL_AMBIENT, [0, 0, 0, 1])  # фоновое излучение
    glLightfv(GL_LIGHT0, GL_DIFFUSE, [0.4, 0.4, 0.4, 1])  # рассеяное излучение
    glLightfv(GL_LIGHT0, GL_SPECULAR, [1, 1, 1, 1])  # зеркальное излучение
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, [1, 1, 1, 1])
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, 50.0)
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, [0, 0, 0, 1])

    glLightfv(GL_LIGHT0, GL_POSITION, [[1, 2, -2, 0]])
    # glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 180)
    # glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 20)

    glEnable(GL_LIGHT0)
    glEnable(GL_LIGHT1)
    glEnable(GL_LIGHT2)
    glEnable(GL_COLOR_MATERIAL)
    glLightf(GL_LIGHT2, GL_CONSTANT_ATTENUATION, 0.0)
    glLightf(GL_LIGHT2, GL_LINEAR_ATTENUATION, 0.2)
    glLightf(GL_LIGHT2, GL_QUADRATIC_ATTENUATION, 0.4)


start = time.monotonic()
main()
stop = time.monotonic()
print('slow:', stop-start)