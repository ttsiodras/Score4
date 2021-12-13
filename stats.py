#!/usr/bin/env python3
import math
import sys
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
l = len(allOfThem)
if not l:
    print("[x] Not enough data!")
    sys.exit(1)
m = int(l/2)
if l % 2:
    print("Median: %f" % srted[m])
else:
    print("Median: %f" % (0.5*(srted[m-1]+srted[m])))
print("Min: %f" % srted[0])
print("Max: %f" % srted[-1])
