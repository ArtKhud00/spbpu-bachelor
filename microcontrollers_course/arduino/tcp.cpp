#include <SoftwareSerial.h>
SoftwareSerial instanceName(10, 11);

String sendToWifi(String command, const int timeout, boolean debug){
  String response = "";
  instanceName.println(command); // send the read character to the esp8266
  long int time = millis();
  while( (time+timeout) > millis())
  {
    if(instanceName.available()>0)
    {
    // The esp has data so display its output to the serial window 
    response = instanceName.readString();
    }  
  }
  if(debug)
  {
    Serial.println(response);
  }
  return response;
}

void setup() 
{
	// put your setup code here, to run once:
	Serial.begin(9600);
	instanceName.begin(9600);
	pinMode(13,OUTPUT);
	digitalWrite(13,LOW);
	sendToWifi("AT+CWMODE=1",rtime,DEB);
	delay(1500);
	sendToWifi("AT+CIFSR",rtime,DEB);
	delay(1500);
	sendToWifi("AT+CIPMUX=1",rtime,DEB);
	delay(1500);
	sendToWifi("AT+CIPSERVER=0",rtime,DEB);
	delay(1500);
	sendToWifi("AT+CWQAP",rtime,DEB);
	delay(3000);
	sendToWifi("AT+CWJAP=\"aboba\",\"\"",rtime,DEB);
	delay(4000);
	sendToWifi("AT+CIPSTART=1,\"TCP\",\"192.168.4.1\",2020",rtime,DEB);
}

void loop() {
	// put your main code here, to run repeatedly:
	sendToWifi("AT+CIPSEND=1,3",rtime,DEB);
    delay(100);
    sendToWifi("OFF",rtime,DEB);
    delay(2900);
}
