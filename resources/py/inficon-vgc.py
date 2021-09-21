## needs pyserial to be installed:
## pip3 install pyserial
##

## call:
## python3 vgc.py 2 10 1
## where:
## * 2 is the channel
## * 10 number of measurements
## * 1 waittime between measurements in sec

import serial
import sys
from time import sleep
import json
ch = sys.argv[1]
n = int(sys.argv[2])
w = float(sys.argv[3])
ser = serial.Serial('/dev/ttyUSB1', timeout=1, baudrate = 9600)
sleep(0.1)
ser.write(bytearray("PR{}\r\n".format(ch).encode()))
sleep(0.1)
ser.readline()
res = []
for i in range(n):
    sleep(w)
    ser.write(bytearray.fromhex("05"))
    sleep(0.1)
    res.append(ser.readline())

ser.close()
print(json.dumps([float(i.decode().split(",")[1]) for i in res]))
