import pygame
import textwrap
pygame.init()
pygame.mixer.init()


def KeyPress(target):
    exit=0
    while exit == 0:
        for event in pygame.event.get():
            if event.type == 2 and event.key == 113:
                pygame.quit()
            if event.type == 2 and event.key == target:
                exit=1
                print "exit"
                

def TextDisplay(file,x_pos,y_pos,width,size):
    """reads in a text file and displays it on the screen"""
    string = filter(None,[str.replace("\n",'') for str in open(file,'r').readlines()])
    wrappedstring=[]
    for str in string:
        new=textwrap.wrap(str,width)
        for st in new:
            wrappedstring.append(st)
        wrappedstring.append('')

    shift=0
    for str in wrappedstring:        
        font = pygame.font.Font(None, size)
        text = font.render(str.decode('utf-8'),1, (10, 10, 10))
        textpos = text.get_rect()
        textpos.topleft = (x_pos,y_pos+shift)
        screen.blit(text, textpos)
        shift+=size
    pygame.display.flip()


screen = pygame.display.set_mode([800,600])
screen.fill([255,255,255])
TextDisplay('instr0.txt',100,100,40,30,screen)
KeyPress(13)


