from OpenGL.GL import *
from OpenGL.GLU import *
import glfw


screen_width = 640
screen_height = 640

points_list = []
frame_buffer = []
edges=[]

for i in range(screen_width * screen_height*3):
    frame_buffer.append(0.0)

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

maxPointY=Point(0,-10000)
minPointY=Point(0,100000)

maxPointX=Point(-10000,0)
minPointX=Point(100000,0)


def set_pixel(x, y, r, g, b):
    global maxPointY, minPointY, maxPointX, minPointX
    if maxPointY.y<y:
        maxPointY=Point(x,y)
    if minPointY.y>y:
        minPointY=Point(x,y)
    if maxPointX.x<x:
        maxPointX=Point(x,y)
    if minPointX.x>x:
        minPointX=Point(x,y)

    index = 3*x + 3*(-y) * screen_width
    frame_buffer[index] = r
    frame_buffer[index + 1] = g
    frame_buffer[index + 2] = b

def clear_buffer():
    frame_buffer.clear()
    for i in range(screen_width * screen_height * 3):
        frame_buffer.append(0.0)


def sign(a):
    if a > 0:
        return 1
    if a < 0:
        return -1
    else:
        return 0

def bresenham(x1, y1, x2, y2):
    global edges,dict
    dx = abs(x2 - x1)
    dy = abs(y2 - y1)

    sign_x = sign(x2 - x1)
    sign_y = sign(y2 - y1)

    swap = False
    if dy > dx:
        dx, dy = dy, dx
        swap = True

    e = 2 * dy - dx
    x = x1
    y = y1

    for i in range(0, int(dx + 1)):
        edges.append(Point(x,y))
        set_pixel(x, y, 1.0, 1.0, 1.0)
        if e >= 0:
            if swap:
                x += sign_x
            else:
                y += sign_y
            e -= 2 * dx
        if swap:
            y += sign_y
        else:
            x += sign_x
        e += 2 * dy


def filling():
    lastX=[]
    counts = []
    for y in range (minPointY.y,maxPointY.y):
        cur=[]
        c=0
        for x in range(minPointX.x, maxPointX.x+1):
            index =3 * x + 3 * screen_width * (-y)
            r = frame_buffer[index]
            g = frame_buffer[index + 1]
            b = frame_buffer[index + 2]
            if r > 0 and g > 0 and b > 0:
                cur.append(x)
            if r > 0 and g > 0 and b > 0 and frame_buffer[3 * (x + 1) + 3 * screen_width * (-y)] == 0:
                c+=1
        lastX.append(cur[len(cur) - 1])
        counts.append(c)


    for y in range (minPointY.y,maxPointY.y):
        flag = False
        # с=counts[y-minPointY.y]
        for x in range(minPointX.x-3, maxPointX.x):
            index =3 * x + 3 * screen_width * (-y)
            r = frame_buffer[index]
            g = frame_buffer[index + 1]
            b = frame_buffer[index + 2]
            if flag and r == 0:
                r1 = 0.5
                g1 = 0
                b1 = 0
                set_pixel(x, y, r1, g1, b1)
            f=False
            if r >0 and g>0 and b>0 and frame_buffer[3 * (x+1) + 3 * screen_width * (-y)]==0:
                counts[y-minPointY.y]-=1
                f=True
                flag = not flag
            if counts[y-minPointY.y]%2!=0 and f and not flag:
                flag = True
            if x==lastX[y-minPointY.y] and flag:
                flag=not flag
    print("vse")

# def isEdgesOnOneSide(y,x):
#     ind=-1
#     for i in range(len(edges)):
#         if edges[i].y==y and edges[i].x==x:
#             ind=i
#             break
#     if ind==-1:
#         return False
#     for p in points_list:
#         if p.y==edges[ind].y and p.x==edges[ind].x:
#             print("da",x,y)
#     if ((edges[ind - 1].y < y) and (edges[(ind + 1)%len(edges)].y > y)) or ((edges[ind - 1].y > y) and (edges[(ind + 1)%len(edges)].y < y)) :
#         return True
#     return False

def smooth_post_filtration():
    new_width = 2 * screen_width + 1
    new_height = 2 * screen_height + 1
    new_frame_buffer = []
    for i in range(new_width * new_height):
        new_frame_buffer.append(0.0)

    for i in range(screen_height):
        for j in range(screen_width):
            index = 3 * i * screen_width + 3 * j
            r = frame_buffer[index]
            g = frame_buffer[index + 1]
            b = frame_buffer[index + 2]
            if r == 1 and g == 1 and b == 1:
                new_index = (2 * i + 1) * new_width + (2 * j + 1)

                ni = new_index
                new_frame_buffer[ni] = 1.0

                ni = new_index - new_width - 1
                new_frame_buffer[ni] = 1.0
                ni = new_index - new_width
                new_frame_buffer[ni] = 1.0
                ni = new_index - new_width + 1
                new_frame_buffer[ni] = 1.0

                ni = new_index + new_width - 1
                new_frame_buffer[ni] = 1.0
                ni = new_index + new_width
                new_frame_buffer[ni] = 1.0
                ni = new_index + new_width + 1
                new_frame_buffer[ni] = 1.0

                ni = new_index - 1
                new_frame_buffer[ni] = 1.0

                ni = new_index + 1
                new_frame_buffer[ni] = 1.0

    for i in range(screen_height):
        for j in range(screen_width):
            index = 3 * i * screen_width + 3 * j
            intens = 0

            new_index = (2 * i + 1) * new_width + (2 * j + 1)

            ni = new_index
            intens += 4*new_frame_buffer[ni]

            ni = new_index - new_width - 1
            intens += new_frame_buffer[ni]
            ni = new_index - new_width
            intens += 2*new_frame_buffer[ni]
            ni = new_index - new_width + 1
            intens += new_frame_buffer[ni]

            ni = new_index + new_width - 1
            intens += new_frame_buffer[ni]
            ni = new_index + new_width
            intens += 2*new_frame_buffer[ni]
            ni = new_index + new_width + 1
            intens += new_frame_buffer[ni]

            ni = new_index - 1
            intens += 2*new_frame_buffer[ni]

            ni = new_index + 1
            intens += 2*new_frame_buffer[ni]

            frame_buffer[index] = intens / 16
            frame_buffer[index + 1] = intens / 16
            frame_buffer[index + 2] = intens / 16


def display(window):
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    glDrawPixels(screen_width, screen_height, GL_RGB, GL_FLOAT, (GLfloat * len(frame_buffer))(*frame_buffer))

    glfw.swap_buffers(window)
    glfw.poll_events()

def mouse_callback(window, button, action, mods):
    global points_list, filling_mode
    if button == glfw.MOUSE_BUTTON_LEFT and action == glfw.PRESS:
        coords = glfw.get_cursor_pos(window)
        clear_buffer()
        x = int(coords[0])
        y = int(coords[1])
        print(x, y)
        points_list.append(Point(x, y))
        set_pixel(x, y, 1.0, 1.0, 1.0)
        draw_lines()

def key_callback(window, key, scancode, action,mods):
    global points_list, filling_mode, screen_width, screen_height
    if action == glfw.PRESS or action==glfw.REPEAT:
        if key == glfw.KEY_UP:
            if len(points_list) > 2:
                filling()
                filling_mode = True
        if key == glfw.KEY_DELETE:
            points_list.clear()
            filling_mode = False
            clear_buffer()


def draw_lines():
    if len(points_list) == 2:
        bresenham(points_list[0].x, points_list[0].y, points_list[1].x, points_list[1].y)
    elif len(points_list) > 2:
        for i in range(len(points_list) - 1):
            bresenham(points_list[i].x, points_list[i].y, points_list[i + 1].x, points_list[i + 1].y)
        bresenham(points_list[len(points_list) - 1].x, points_list[len(points_list) - 1].y,
                  points_list[0].x, points_list[0].y)
        smooth_post_filtration()

def main():
    if not glfw.init():
        return
    window = glfw.create_window(640, 640, "Lab4", None, None)
    if not window:
        glfw.terminate()
        return
    glfw.make_context_current(window)
    glfw.set_key_callback(window,key_callback)
    glfw.set_mouse_button_callback(window, mouse_callback)

    while not glfw.window_should_close(window):
        display(window)
    glfw.destroy_window(window)
    glfw.terminate()

if __name__ == "__main__":
    main()