from psychopy import visual, event, core, gui
from math import sin, cos
import numpy, time, math, random, pygame, datetime, os, os.path, codecs, glob
from random import shuffle
import textwrap
pygame.init()

request = gui.Dlg()
request.addField("Probandennummer:")
request.addField("Alter:")
request.addField("Geschlecht:")
request.show()
subj_id = request.data[0]
age = request.data[1]
Geschlecht = request.data[2]


def TextDisplay(file,x_pos,y_pos,width,size,screen):
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

def KeyPress(target):
    exit=0
    pygame.event.clear()
    while exit == 0:
        for event in pygame.event.get():
            if event.type == 2 and event.key == target:
                exit=1
            if event.type == 2 and event.key == 113:
                pygame.quit()

def ClickResponse(c,screen,image):
    exit=0
    resp=0
    pygame.event.clear()
    while exit == 0:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
            if event.type==pygame.KEYDOWN and event.key==27:
                pygame.quit()
            if event.type == pygame.MOUSEBUTTONDOWN:
                StimDisplay(image,screen)
                position=event.pos
                if position[1]>c[1] and position[1]<c[1]+80:
                    if position[0]>c[0]-90 and position[0]<c[0]+10:
                        pygame.draw.lines(screen, (50,50,255),True, [[c[0]-90,c[1]],[c[0]+10,c[1]],[c[0]+10,c[1]+80],[c[0]-90,c[1]+80]],5)
                        pygame.display.flip()
                        resp=1
                    elif position[0]>c[0]+10 and position[0]<c[0]+110:
                        pygame.draw.lines(screen, (50,50,255),True, [[c[0]+10,c[1]],[c[0]+110,c[1]],[c[0]+110,c[1]+80],[c[0]+10,c[1]+80]],5)
                        pygame.display.flip()
                        resp=2
                    elif position[0]>c[0]+110 and position[0]<c[0]+205:
                        pygame.draw.lines(screen, (50,50,255),True, [[c[0]+110,c[1]],[c[0]+205,c[1]],[c[0]+205,c[1]+80],[c[0]+110,c[1]+80]],5)
                        pygame.display.flip()
                        resp=3
                    elif position[0]>c[0]+205 and position[0]<c[0]+305:
                        pygame.draw.lines(screen, (50,50,255),True, [[c[0]+205,c[1]],[c[0]+305,c[1]],[c[0]+305,c[1]+80],[c[0]+205,c[1]+80]],5)
                        pygame.display.flip()
                        resp=4
                    elif position[0]>c[0]+305 and position[0]<c[0]+405:
                        pygame.draw.lines(screen, (50,50,255),True, [[c[0]+305,c[1]],[c[0]+405,c[1]],[c[0]+405,c[1]+80],[c[0]+305,c[1]+80]],5)
                        pygame.display.flip()
                        resp=5
                    else:
                        resp=0
                else:
                    resp=0
            elif event.type==pygame.KEYDOWN and event.key==13:
                if resp>0:
                    return resp
            if resp>0:
                CenterText(700,screen,"Eingabtaste dr"+u"\u00FC"+"cken, um fortzufahren")
                pygame.display.flip()

def CenterText(y_pos,screen,string):
    font = pygame.font.Font(None, 36)
    text = font.render(string,1, (10, 10, 10))
    textpos = text.get_rect()
    textpos.centerx = screen.get_rect().centerx
    textpos.centery = y_pos
    screen.blit(text, textpos)

def StimDisplay(image,screen,show=1):
    img = pygame.image.load(image)
    screen.fill([255,255,255])
    impos=img.get_rect()
    impos.centerx=screen.get_rect().centerx
    impos.centery=screen.get_rect().centery
    if show > 0:
        CenterText(100,screen,"Klicke auf das K"+u"\u00E4"+"stchen, das den gr"+u"\u00F6"+u"\u00DF"+"eren Kasten vervollst"+u"\u00E4"+"ndigt")
    screen.blit(img,impos)
    pygame.display.flip()

def WriteLog(p_ID,responses,responsetimes):
    now = datetime.datetime.now()
    file = open('logs/CFT_logfile_'+p_ID+'_'+str(now.day)+'_'+str(now.month)+'_'+str(now.year)+'.txt', 'w')
    file.write("Response\t")
    for response in responses:
        file.write("%s\t" % response)
    file.write("\n")
    file.write("Response Time\t")
    for responsetime in responsetimes:
        file.write("%s\t" % responsetime)
    file.write("\n")
    file.write("Score\t")
    answers=[2,3,2,4,2,1,5,4,3,1,5,3,4,5,1]
    score = sum([1 for i in zip(responses,answers) if i[0]==i[1]])
    file.write("%s\t" % str(score))
    file.close()

def instructions(screen,section):
    wait=0
    pics=['test/cft_001.jpg','test/cft_001.jpg','test/cft_004.jpg']
    img = pygame.image.load(pics[section])
    impos=img.get_rect()
    impos.centerx=screen.get_rect().centerx
    impos.centery=550

    screen.fill([255,255,255])
    screen.blit(img,impos)
    TextDisplay('instr/instr'+str(section)+'.txt',screen.get_rect().centerx-300,50,60,30,screen)
    CenterText(700,screen,"Eingabtaste dr"+u"\u00FC"+"cken, um fortzufahren")
    pygame.display.flip()
    KeyPress(13)


def practice(screen):
    imagefiles=glob.glob("test/*.jpg")
    correctlist=[3,1,4]
    correcttext=["c","a","d"]
    for image,correct,text in zip(imagefiles,correctlist,correcttext):
        StimDisplay(image,screen)
        response = ClickResponse([screen.get_rect().centerx,screen.get_rect().centery],screen,image)
        StimDisplay(image,screen,0)
        if response==correct:
            CenterText(100,screen,"Richtig!")
        else:
            CenterText(100,screen,"Falsch!")
            CenterText(200,screen,"'"+text+"' w"+u"\u00E4"+"re die richtige Antwort gewesen!")
        CenterText(700,screen,"Eingabtaste dr"+u"\u00FC"+"cken, um fortzufahren")
        pygame.display.flip()
        KeyPress(13)
    screen.fill([255,255,255])
    CenterText(200,screen,"Das Experiment wird jetzt anfangen...")
    CenterText(700,screen,"Eingabtaste dr"+u"\u00FC"+"cken, um fortzufahren")

def test(p_ID,screen,section):
    imagefiles=glob.glob("images/"+str(section)+"/*.jpg")
    responses=[]
    responsetimes=[]
    for image in imagefiles:
        StimDisplay(image,screen)
        starttime=time.time()
        response = ClickResponse([screen.get_rect().centerx,screen.get_rect().centery],screen,image)
        responsetime=time.time()-starttime
        responses.append(response)
        responsetimes.append(responsetime)
    return responses, responsetimes

def main(p):
    p_ID = str(p.participant["Participant ID"])
    if p.ownscreen==1:
        winrect = p.rect
        screen = pygame.display.set_mode(winrect, pygame.FULLSCREEN)
    else:
        screen = p.screen
    instructions(screen,0)
    practice(screen)
    instructions(screen,1)
    responses, responsetimes = test(p_ID,screen,1)
    instructions(screen,2)
    responses2, responsetimes2 = test(p_ID,screen,2)
    
    screen.fill([255,255,255])
    CenterText(400,screen,"Eingabetaste dr"+u"\u00FC"+"cken, um fortzufahren")
    pygame.display.flip()
    KeyPress(13)
    if p.ownscreen==1:
        pygame.display.quit()
    
    responses.extend(responses2)
    responsetimes.extend(responsetimes2)
    WriteLog(p_ID,responses,responsetimes)

if __name__ == '__main__':
    class para(object):
        def __init__(self,rect,fullscr):
            self.participant= {'Participant ID': subj_id, 'Gender': Geschlecht, 'Age': age}
            self.now = datetime.datetime.now()                        # get current date and time for log files
            s = pygame.display.Info()
            winrect = [s.current_w,s.current_h]
            self.rect = winrect
            if fullscr== 0:                                           # screen is initialised, either fullcreen or not.
                self.screen = pygame.display.set_mode(self.rect)
            else:
                self.screen = pygame.display.set_mode(self.rect,pygame.FULLSCREEN)
            self.instrwait = 0 
            self.ownscreen = 1
    p = para([1200,800],1)
    main(p)

