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
V = 0.0009*0.02
acl = 0.00006*0.02

light_mode = False
texture_mode = 1
filling_mode = True

array_normal=[]
array_side = []
array_texture = []
array_color = []

def create_shader(shader_type, source):
    shader = glCreateShader(shader_type)
    glShaderSource(shader, source)
    glCompileShader(shader)
    return shader

def main():
    if not glfw.init():
        return
    window = glfw.create_window(800, 800, "Lab8", None, None)
    if not window:
        glfw.terminate()
        return
    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)
    glfw.set_mouse_button_callback(window, mouse_callback)

    vertex = create_shader(GL_VERTEX_SHADER, """
            attribute vec3 aVert;
            varying vec3 n;
            varying vec3 v;
            varying vec2 uv;
            //varying vec4 vertexColor;
            void main()
            {   
                uv = gl_MultiTexCoord0.xy;
                v = vec3(gl_ModelViewMatrix * gl_Vertex);
                n = normalize(gl_NormalMatrix * gl_Normal);
                gl_TexCoord[0] = gl_TextureMatrix[0]  * gl_MultiTexCoord0;
                gl_Position = gl_ModelViewProjectionMatrix * vec4(gl_Vertex.x, gl_Vertex.y, gl_Vertex.z, 1);
                //vec4 vertexColor = vec4(0.5f, 0.0f, 0.0f, 1.0f);
            }
            """)

    fragment = create_shader(GL_FRAGMENT_SHADER, """
            varying vec3 n;
            varying vec3 v; 
            //varying vec4 vertexColor; // Входная переменная из вершинного шейдера (то же название и тот же тип)

            uniform sampler2D tex;
            void main ()  
            {  
                vec3 L = normalize(gl_LightSource[0].position.xyz - v);   
                vec3 E = normalize(-v);
                vec3 R = normalize(-reflect(L,n));  

                //calculate Ambient Term:  
                vec4 Iamb = gl_FrontLightProduct[0].ambient;    

                //calculate Diffuse Term:  
                vec4 Idiff = gl_FrontLightProduct[0].diffuse * max(dot(n,L), 1.0);
                Idiff = clamp(Idiff, 2.0, 0.6);     

                // calculate Specular Term:
                vec4 Ispec = gl_LightSource[0].specular 
                                * pow(max(dot(R,E),0.0),0.7);
                Ispec = clamp(Ispec, 0.0, 1.0); 

                vec4 texColor = texture2D(tex, gl_TexCoord[0].st);
                gl_FragColor = (Idiff + Iamb + Ispec) * texColor;
            }
            """)
    generate_texture()
    program = glCreateProgram()
    glAttachShader(program, vertex)
    glAttachShader(program, fragment)
    glLinkProgram(program)
    vertIndex = glGetAttribLocation(program, "aVert")
    glEnableClientState(GL_VERTEX_ARRAY)
    glEnableClientState(GL_NORMAL_ARRAY)
    glEnableClientState(GL_COLOR_ARRAY)

    if texture_mode > 0:
        glEnableClientState(GL_TEXTURE_COORD_ARRAY)
    make_vertices(0, 0, 0, 0.8, 40)
    init_second_display_list()
    init_first_display_list()
    glCallList(2)

    glUseProgram(program)
    while not glfw.window_should_close(window):
        display(window)
    glfw.destroy_window(window)
    glfw.terminate()


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
    # glRotatef(fi, 1, 0, 0)
    # glRotatef(tetha, 0, 1, 0)

    glCallList(1)
    glShadeModel(GL_FLAT)

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

def init_first_display_list():
    glNewList(2, GL_COMPILE)
    glEnable(GL_LIGHTING)
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, [[0.1, 0.1, 0.1, 1]])
    glLightfv(GL_LIGHT0, GL_AMBIENT, [0, 0, 0, 1])  # фоновое излучение
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, [1, 1, 1, 1])
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, 50.0)
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, [0, 0, 0, 1])

    glLightfv(GL_LIGHT0, GL_POSITION, [[1, 1, 0, 0]])
    glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 30)
    glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 20)

    glEnable(GL_LIGHT0)
    glEnable(GL_LIGHT1)
    glEnable(GL_LIGHT2)
    glEnable(GL_COLOR_MATERIAL)
    glLightf(GL_LIGHT2, GL_CONSTANT_ATTENUATION, 0.0)
    glLightf(GL_LIGHT2, GL_LINEAR_ATTENUATION, 0.2)
    glLightf(GL_LIGHT2, GL_QUADRATIC_ATTENUATION, 0.4)
    glEndList()


def init_second_display_list():
    glNewList(1, GL_COMPILE)
    glColorPointer(3, GL_FLOAT, 0, array_color)
    glNormalPointer(GL_FLOAT, 0, array_normal)
    glTexCoordPointer(2, GL_FLOAT, 0, array_texture)
    glVertexPointer(3, GL_FLOAT, 0, array_side)

    glDrawArrays(GL_TRIANGLE_STRIP, 0,1722)
    glEndList()

def make_vertices(cx,cy, cz, r,p):
    global array_side, array_texture, array_color,array_normal
    color = [1.0, 1.0, 1.0]
    array_color.append(color)
    TWOPI = 2 * math.pi
    PIDIV2 = math.pi / 2
    for i in range(p // 2 + 1):
        theta1 = i * TWOPI / p - PIDIV2
        theta2 = (i + 1) * TWOPI / p - PIDIV2
        for j in range(p + 1):
            theta3 = j * TWOPI / p

            ex = math.cos(theta2) * math.cos(theta3)
            ey = math.sin(theta2)
            ez = math.cos(theta2) * math.sin(theta3)
            px = cx + r * ex
            py = cy + r * ey
            pz = cz + r * ez

            array_normal.append([ex, ey, ez])
            array_texture.append([-(j / p), 2 * (i + 1) / p])
            array_side.append([px, py, pz * 1.2])
            array_color.append(color)

            ex = math.cos(theta1) * math.cos(theta3)
            ey = math.sin(theta1)
            ez = math.cos(theta1) * math.sin(theta3)
            px = cx + r * ex
            py = cy + r * ey
            pz = cz + r * ez

            array_normal.append([ex, ey, ez])
            array_texture.append([-(j / p), 2 * i / p])
            array_side.append([px, py, pz * 1.2])
            array_color.append(color)



main()