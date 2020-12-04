from psychopy import visual, event, core, gui
from math import sin, cos
import numpy, time, math, random, pygame, datetime, os, os.path, codecs, glob
from random import shuffle
import textwrap
pygame.init()
pygame.mixer.init()


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

def WriteLog(p_ID,resp1,correct1,resp2,correct2):
    now = datetime.datetime.now()
    file = open('logs/NUMBERS_logfile_'+p_ID+'_'+str(now.day)+'_'+str(now.month)+'_'+str(now.year)+'.txt', 'w')
    file.write("Response forwards\t")
    for response in resp1:
        file.write("%s\t" % response)
    file.write("\n")
    file.write("Correct\t")
    for correct in correct1:
        file.write("%s\t" % correct)
    file.write("\n")
    
    file.write("Response backwards\t")
    for response in resp2:
        file.write("%s\t" % response)
    file.write("\n")
    file.write("Correct backwards\t")
    
    for correct in correct2:
        file.write("%s\t" % correct)
    file.write("\n")
    file.write("Score Forwards\t")
    file.write("%s\t" % str(sum(correct1)+1))
    
    file.write("\n")
    file.write("Score Backwards\t")
    file.write("%s\t" % str(sum(correct2)+2))
    
    file.close()

def KeyPress(target):
    pygame.event.clear()
    exit=0
    while exit == 0:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
            if event.type==pygame.KEYDOWN and event.key==27:
                pygame.quit()
            if event.type == 2 and event.key == 113:
                pygame.quit()
            if event.type == 2 and event.key == target:
                exit=1
                print "exit"

def PygletKeyPress(target):
    event.waitKeys(keyList=target)


def ClickResponse(screen,rect):
    pad = pygame.image.load("NUMPAD/numpad.png")
    screen.fill([255,255,255])
    CenterText(100,screen,"Zahlensequenz jetzt eingeben")
    impos=pad.get_rect()
    impos.centerx=screen.get_rect().centerx
    centerx = screen.get_rect().centerx
    impos.centery=screen.get_rect().centery
    centery = screen.get_rect().centery
    screen.blit(pad,impos)
    pygame.display.flip()
    exit=0
    ans=-1
    response = []
    pygame.event.clear()
    while exit == 0:
        if ans>-1:
            CenterText(rect[1]-100,screen,"Eingabetaste dr"+u"\u00FC"+"cken, um fortzufahren")
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
            if event.type==pygame.KEYDOWN and event.key==27:
                pygame.quit()
            if event.type == pygame.MOUSEBUTTONDOWN:
                position=event.pos
                if position[0]> centerx-150 and position[0]<centerx-50:
                    if position[1]> centery-200 and position[1]<centery-100:
                        ans=1
                        response.append(ans)
                    elif position[1]> centery-100 and position[1]<centery:
                        ans=4
                        response.append(ans)
                    elif position[1]> centery and position[1]<centery+100:
                        ans=7
                        response.append(ans)
                elif position[0]> centerx-50 and position[0]<centerx+50:
                    if position[1]> centery-200 and position[1]<centery-100:
                        ans=2
                        response.append(ans)
                    elif position[1]> centery-100 and position[1]<centery:
                        ans=5
                        response.append(ans)
                    elif position[1]> centery and position[1]<centery+100:
                        ans=8
                        response.append(ans)
                    elif position[1]> centery+100 and position[1]<centery+200:
                        ans=10
                        response.append(ans)
                elif position[0]> centerx+50 and position[0]<centerx+150:
                    if position[1]> centery-200 and position[1]<centery-100:
                        ans=3
                        response.append(ans)
                    elif position[1]> centery-100 and position[1]<centery:
                        ans=6
                        response.append(ans)
                    elif position[1]> centery and position[1]<centery+100:
                        ans=9
                        response.append(ans)
                if ans>-1:
                    CenterText(100,screen,"Zahlensequenz jetzt eingeben")
                    pad = pygame.image.load("NUMPAD/numpad_"+str(ans)+".png")
                    screen.blit(pad,impos)
                    pygame.display.flip()
            if event.type == pygame.MOUSEBUTTONUP:
                CenterText(100,screen,"Zahlensequenz jetzt eingeben")
                pad = pygame.image.load("NUMPAD/numpad.png")
                screen.blit(pad,impos)
                pygame.display.flip()
            if event.type == 2 and event.key == 13:
                if ans>-1:
                    return response
            if event.type == 2 and event.key == 113:
                pygame.quit()
    
def CenterText(y_pos,screen,string):
    font = pygame.font.Font(None, 36)
    text = font.render(string,1, (10, 10, 10))
    textpos = text.get_rect()
    textpos.centerx = screen.get_rect().centerx
    textpos.centery = y_pos
    screen.blit(text, textpos)

def DisplayInstructions(screen,section):
    wait=0 #waiting time between instruction slides
    slidefiles=glob.glob("instr/"+str(section)+"/*.txt")
    for slide in slidefiles:
        screen.fill([255,255,255])
        TextDisplay(slide,screen.get_rect().centerx-300,150,60,34,screen)
        pygame.display.flip()
    KeyPress(13)

def Practice(screen,section,rect):
    section-=1
    soundfiles=[["audio/practice/sound01.wav"],["audio/practice/p2_sound01.wav","audio/practice/p2_sound02.wav"]]
    responses = [[[2,9],[0,0]],[[2,8],[6,5]]]
    count=0
    for file in soundfiles[section]:
        screen.fill([255,255,255])
        CenterText(400,screen,"Beispiel")
        pygame.display.flip()

        speech = pygame.mixer.Sound(file)
        speech.play()
        time.sleep(speech.get_length())
        response = ClickResponse(screen,rect)

        if response == responses[section][count]:
            screen.fill([255,255,255])
            CenterText(100,screen,"Richtig!")
            CenterText(300,screen,"Die Antwort war")
            CenterText(400,screen,str(responses[section][count]))
            CenterText(rect[1]-100,screen,"Eingabetaste dr"+u"\u00FC"+"cken, um fortzufahren")
            pygame.display.flip()
        else:
            screen.fill([255,255,255])
            CenterText(100,screen,"Falsch!")
            CenterText(300,screen,"Die richtige Antwort war")
            CenterText(400,screen,str(responses[section][count]))
            CenterText(rect[1]-100,screen,"Eingabetaste dr"+u"\u00FC"+"cken, um fortzufahren")
            pygame.display.flip()
        count+=1
        KeyPress(13)

def NumbersTest(screen,section,rect):
    secflip=[0,2,1]
    soundfiles=glob.glob("audio/"+str(section)+"/*.wav")
    count=0
    responses=[]
    correct_list=[]
    trialnum=["a)","b)","c)","d)","e)","f)","g)","h)","i)","j)","k)","l)","m)","n)","o)","p)","q)","r)"]
    answers=[[[4,6],[3,8,6],[6,1,2],[3,4,1,7],[6,1,5,8],[8,4,2,3,9],[5,2,1,8,6],[3,8,9,1,7,4],[7,9,6,4,8,3],[5,1,7,4,2,3,8],[9,8,5,2,1,6,3],[1,8,4,5,9,7,6,3],[2,9,7,6,3,1,5,4],[5,3,8,7,1,2,4,6,9],[4,2,6,9,1,7,8,3,5]],[[1, 2],[3, 1],[5, 3],[4, 6],[4, 7, 5],[9, 5, 2],[6, 9, 2, 7],[3, 9, 4, 8],[7, 5, 3, 1, 4],[2, 5, 8, 7, 9],[8, 9, 2, 5, 6, 1],[4, 9, 1, 7, 6, 3],[6, 4, 3, 2, 9, 5, 8],[1, 8, 2, 9, 7, 5, 4],[8, 5, 2, 3, 7, 1, 9, 6],[2, 8, 4, 5, 9, 7, 1, 3]]]
    for file,trial in zip(soundfiles,trialnum):
        screen.fill([255,255,255])
        CenterText(400,screen,trial)
        pygame.display.flip()
        
        speech = pygame.mixer.Sound(file)
        speech.play()
        time.sleep(speech.get_length())
        
        response = ClickResponse(screen,rect)
        
        if response == answers[section-1][count]:
            correct = 1
        else:
            correct = 0
        correct_list.append(correct)
        responses.append(response)
        print correct_list
        
        if count>0 and (count % secflip[section]) == 0:
            if correct_list[count]+correct_list[count-1]==0:
                return responses,correct_list
        count += 1
    return responses, correct_list


def main(p):

    
    P_ID = str(p.participant["Participant ID"])
    if p.ownscreen==1:
        winrect = p.rect
        screen = pygame.display.set_mode(winrect, pygame.FULLSCREEN)
    else:
        screen = p.screen
    section=1
    
    DisplayInstructions(screen,section)

    Practice(screen,section,p.rect)

    screen.fill([255,255,255])
    CenterText(100,screen,"Die Aufgaben fangen jetzt an...")
    CenterText(200,screen,"Wenn Du noch Fragen hast, z"+u"\u00F6"+"gere nicht, uns zu Fragen!")
    CenterText(700,screen,"Eingabetaste dr"+u"\u00FC"+"cken, um fortzufahren")
    pygame.display.flip()
    KeyPress(13)

    resp1,list1 = NumbersTest(screen,section,p.rect)

    section=2

    DisplayInstructions(screen,section)

    Practice(screen,section,p.rect)

    screen.fill([255,255,255])
    CenterText(100,screen,"Die Aufgaben fangen jetzt an...")
    CenterText(200,screen,"Wenn Du noch Fragen hast, z"+u"\u00F6"+"gere nicht, uns zu Fragen!")
    CenterText(700,screen,"Eingabetaste dr"+u"\u00FC"+"cken, um fortzufahren")
    pygame.display.flip()
    KeyPress(13)

    resp2,list2 = NumbersTest(screen,section,p.rect)
    
    WriteLog(P_ID,resp1,list1,resp2,list2)
    
    screen.fill([255,255,255])
    CenterText(400,screen,"Eingabetaste dr"+u"\u00FC"+"cken, um fortzufahren")
    pygame.display.flip()
    KeyPress(13)
    if p.ownscreen==1:
        pygame.display.quit()

if __name__ == '__main__':
    pygame.init()
    class para(object):
        def __init__(self,rect,fullscr):
            self.participant= {'Participant ID': subj_id, 'Gender': Geschlecht, 'Age': age}
            self.now = datetime.datetime.now()                        # get current date and time for log files
            s = pygame.display.Info()
            winrect = [s.current_w,s.current_h]
            self.rect = winrect                                          # screen dimensions
            if fullscr== 0:                                           # screen is initialised, either fullcreen or not.
                self.screen = pygame.display.set_mode(self.rect)
            else:
                self.screen = pygame.display.set_mode(self.rect,pygame.FULLSCREEN)
            self.instrwait = 0
            self.ownscreen=0
    p = para([1200,800],1)
    total = main(p)
    print total