#!/usr/bin/env python
import math
total=totalSq=n=0
allOfThem = []
while True:
    try:
        a=float(raw_input())
    except:
        try:
            a=float(input())
        except:
            break
    total += a
    totalSq += a*a
    n += 1
    allOfThem.append(a)

print("Average value: %f" % (total/n))
variance = (totalSq - total*total/n)/n
try:
    v = math.sqrt(variance)
    print("Std deviation: %f" % v)
except:
    print("Std deviation: 0.0")
srted = sorted(allOfThem)
print("Median: %f" % srted[-1 + int(len(allOfThem)/2)])
print("Min: %f" % srted[0])
print("Max: %f" % srted[-1])
