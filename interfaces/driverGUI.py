#!/usr/bin/env python
import sys, os, copy
import pygame

state = [
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 1, 0, 0, 0 ]]

cleanState = copy.deepcopy(state)

def drop(column, number):
    if state[0][column] != 0:
	return
    elif 0<=column<=6:
	for row in xrange(5,-1,-1):
	    if row == 5:
		if state[5][column] == 0:
		    state[5][column] = number
		    break
	    elif state[row+1][column] != 0 and state[row][column] == 0:
		state[row][column] = number
		break
	else:
	    print "Invalid move"
	    raise Exception("")

def SetTitle(s):
    pygame.display.set_caption(s)
    pygame.display.update()

steps = [
   [ [0,0], [-1,1],  [-2,2],  [-3,3]  ],  # diagonal, up-right
   [ [0,0], [0,1],   [0,2],   [0,3]   ],  # horizontal,right
   [ [0,0], [1,1],   [2,2],   [3,3]   ],  # diagonal, down-right
   [ [0,0], [1,0],   [2,0],   [3,0]   ]   # vertical, down
]

def inside(y,x):
    return y>=0 and y<=5 and x>=0 and x<=6

def FlashGameOver():
    for i in xrange(0,5):
	SetTitle("Thanassis' Score4")
	pygame.time.wait(500)
	SetTitle("Thanassis' Score4 - GAME OVER (Press SPACE to start again)")
	pygame.time.wait(500)

def PrintAndCheckState(window):
    gameOver = False
    for y in xrange(0,6):
	for x in xrange(0,7):
	    for direction in xrange(0,4):
		score = 0
		for idx in xrange(0,4):
		    yofs = steps[direction][idx][0]
		    xofs = steps[direction][idx][1]
		    if inside(y+yofs,x+xofs):
			value = state[y+yofs][x+xofs]
			if value in [1,-1]:
			    score += value
		if score in [4,-4]:
		    for idx in xrange(0,4):
			yofs = steps[direction][idx][0]
			xofs = steps[direction][idx][1]
			if inside(y+yofs,x+xofs):
			    state[y+yofs][x+xofs] = score
		    gameOver = True

    # print "\n".join(str(x).replace('0',' ').replace(',',' ').replace('1','X').replace('2','O') for x in state)
    canvas = pygame.Surface(window.get_size())
    canvas = canvas.convert()
    canvas.fill((255, 255, 255))
    for y in xrange(0,7):
	pygame.draw.line(canvas, (0, 0, 0), (0, y*81), (7*91, y*81))
    for x in xrange(0,8):
	pygame.draw.line(canvas, (0, 0, 0), (x*81, 0), (x*81, 6*81+1))
    for y in xrange(0,6):
	for x in xrange(0,7):
	    cell = state[y][x]
	    if   cell == 1: color = (255,0,0)
	    elif cell ==-1: color = (0,255,0)
	    elif cell ==-4: color = (0,128,0)
	    elif cell == 4: color = (128,0,0)
	    center = (x*81+40, y*81+40)
	    if cell != 0:
		pygame.draw.circle(canvas, color,   center, 35, 0)
		pygame.draw.circle(canvas, (0,0,0), center, 35, 1)

    window.blit(canvas, (0, 0))
    #draw it to the screen
    pygame.display.flip() 
    return gameOver

level = 7
if len(sys.argv)>1:
    level = int(sys.argv[1])
    if not 1<level<9:
	print "Invalid level selected - using 7"
	level = 7
pygame.init() 
window = pygame.display.set_mode((7*81+1, 6*81+1)) 
SetTitle("Thanassis' Score4")
PrintAndCheckState(window)

gameOver = False
while True: 
    for event in pygame.event.get(): 
	if event.type == pygame.QUIT: 
	    sys.exit(0) 
	else:
	    if event.type == pygame.KEYUP:
		if event.key == pygame.K_SPACE:
		    state = copy.deepcopy(cleanState)
		    gameOver = False
		    SetTitle("Thanassis' Score4")
		    PrintAndCheckState(window)
		elif event.key == pygame.K_ESCAPE:
		    sys.exit(0)

	    elif not gameOver and event.type == pygame.MOUSEBUTTONUP:
		column = event.pos[0]/81
		if not 0<=column<=6:
		    continue
		drop(column, -1)
		gameOver = PrintAndCheckState(window)
		if gameOver:
		    FlashGameOver()
		    continue
		s = ""
		for y in xrange(0,6):
		    for x in xrange(0,7):
			if state[y][x] in [1,-1]:
			    s+= " " + {1:"o",-1:"y"}[state[y][x]] + str(y) + str(x)
		pygame.mouse.set_visible(False)
		pipe = os.popen(os.path.dirname(os.path.abspath(sys.argv[0])) + os.sep + "engine.exe %s -level %s" % (s,level))
		resp = None
		try:
		    resp = pipe.readlines()[0]
		    resp = int(resp)
		except:
		    pass
		pygame.mouse.set_visible(True)
		exitCode = None
		try:
		    exitCode = pipe.close()
		except:
		    exitCode = -1
		if isinstance(resp, int):
		    drop(resp, 1)
		else:
		    print "Cmd string was:"
		    print "engine.exe %s -level %s" % (s,level)
		gameOver = PrintAndCheckState(window)
		if gameOver:
		    FlashGameOver()
