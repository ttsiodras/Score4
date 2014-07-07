#!/usr/bin/env python
import os, sys

state = [
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 0, 0, 0, 0 ],
    [ 0, 0, 0, 1, 0, 0, 0 ]]

def drop(column, number):
    if state[0][column] != 0:
        print "It is full!"
        raise Exception("")
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

def PrintState():
    print "\n".join(str(x).replace('0',' ').replace(',',' ').replace('1','X').replace('2','O') for x in state)
    print "--------------------"
    print " 0  1  2  3  4  5  6\n"

while True:
    column = -1
    resp = ""
    while True:
        while True:
            allOK = False
            PrintState()
            try:
                ans = raw_input('"q" to quit, or column number (0-6): --> ')
                if ans == "q":
                    sys.exit(0)
                column = int(ans)
                drop(column, 2)
                allOK = True
            except SystemExit:
                raise;
            except:
                pass
            if allOK:
                break

        PrintState()
        s = ""
        for y in xrange(0,6):
            for x in xrange(0,7):
                if state[y][x] != 0:
                    s+= " " + {1:"o",2:"y"}[state[y][x]] + str(y) + str(x)
        #print "./C++/score4 %s -level 7" % s
        pipe = os.popen("./interfaces/engine.exe %s -level 7" % s)
        try:
            resp = pipe.readlines()[0]
            resp = int(resp)
        except:
            if resp.split()[1] == "win" or resp.split()[0] == "No":
                print resp
                print "GAME OVER"
                sys.exit(0)
        exitCode = pipe.close()
        if None != exitCode:
            drop(resp, 1)
            PrintState()
            print "Game finished."
            sys.exit(0)
        print "Computer chose", resp, "\n"
        drop(resp, 1)
