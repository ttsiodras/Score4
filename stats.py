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
print "Std deviation:", math.sqrt(variance)
srted = sorted(allOfThem)
print "Median:", srted[-1 + len(allOfThem)/2]
print "Min:", srted[0]
print "Max:", srted[-1]
