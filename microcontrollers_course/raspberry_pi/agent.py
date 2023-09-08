import socket
import time
import RPi.GPIO as GP
addr1=("192.168.1.206",2021)# Level 3 (lower) device
addr2=("192.168.1.63",8205)# Level 1 (upper) device
sost1=[0,0,0]
kk=0
cc=0
GP.setmode(GP.BCM)
GP.setup(12,GP.OUT)


try:
	while True:
		# initial setup
		print("begin")
		sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM) # UDP
		sock.bind(("192.168.1.211",2018))
		data, addr = sock.recvfrom(1024)
			
		print("received message: %s" % data)
		time.sleep(1.5)
		# send a request to turn on a specific diode
		sock = socket.socket ( socket.AF_INET, socket.SOCK_DGRAM,socket.IPPROTO_UDP )
		sock.bind(("192.168.1.211",2018))
		sock.sendto(data, addr1)
		# recieve diode condition 
		sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM) # UDP
		sock.bind(("192.168.1.211",2018))
		sost, addr = sock.recvfrom(1024)
		print("condition",sost)
		sost1[int(data)-1] = int(sost)
		print('type',type(sost1[int(data)-1]))
		time.sleep(0.75)
		sock = socket.socket ( socket.AF_INET, socket.SOCK_DGRAM,socket.IPPROTO_UDP )
		sock.bind(("192.168.1.211",2018))
		# turn on the diode if it does not light up
		if(sost1[int(data)-1]==0):
			print("OK")
			sock.sendto("1", addr1)
			time.sleep(0.75)
			sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM) # UDP
			sock.bind(("192.168.1.211",2018))
			case, addr = sock.recvfrom(1024)
			time.sleep(0.75)
			sock = socket.socket ( socket.AF_INET, socket.SOCK_DGRAM,socket.IPPROTO_UDP )
			sock.bind(("192.168.1.211",2018))
			sock.sendto(case, addr2)
		# if it is already enabled, then we signal an error and send a negative response
		else:
			print("ERROR")
			GP.output(12,GP.HIGH)
			sock.sendto("2", addr1)
			sock.sendto("Fail", addr2)
		time.sleep(3)
		GP.output(12,0)	
		
		
	
# Allows to close sockets using the keyboard shortcut CTRL+C (keyboard interrupt command)	
except KeyboardInterrupt:
	sock.close()
	print("Sockets are closed!\n")
	input()
