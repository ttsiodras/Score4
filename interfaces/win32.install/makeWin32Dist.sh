#!/bin/bash
echo Make sure you are using ActivePython 2.6, py2exe for 2.6 and pygame for 2.6
echo Press ENTER to build...
read ANS
rm -rf build dist
python setup.py py2exe -b 2 --excludes email,multiprocessing,distutils,xml
rm -rf build
# g++ -o engine.exe -O3 score4.cpp
cp engine.exe dist/
mv dist/driverGUI.exe dist/score4.exe
rm -rf Score4
mv dist Score4
echo All done. Look in folder \"Score4\"
