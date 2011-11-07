#!/usr/bin/env python
import math
total=totalSq=n=0
allOfThem = []
while True:
    try:
        a=float(raw_input())
    except:
        break
    total += a
    totalSq += a*a
    n += 1
    allOfThem.append(a)

print "Average value:", total/n
variance = (totalSq - total*total/n)/n
try:
    v = math.sqrt(variance)
    print "Std deviation:", v
except:
    print "Std deviation:", 0.0
srted = sorted(allOfThem)
print "Median:", srted[-1 + len(allOfThem)/2]
print "Min %4.3f sec" % srted[0]
print "Max:", srted[-1]
