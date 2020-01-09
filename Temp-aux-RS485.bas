'--------------------------------------------------------------
'                   Thomas Jensen | stdout.no
'--------------------------------------------------------------
'  file: AVR_TEMP_AUX_v.1.0
'  date: 05/11/2011
'  prot: 2.10
'  sn# : 156
'--------------------------------------------------------------
$regfile = "m8def.dat"
$crystal = 8000000
$baud = 38400
Config Portb.0 = Output
Config Portb.1 = Output
Config Watchdog = 128

Config Print0 = Portd.2 , Mode = Set                        'RS-485
Config Pind.2 = Output                                      'set the direction

$version 1 , 0 , 7

'inn
'PC0: In 1 Temp
'PC1: In 2 Humid
'PC2: In 3 Aux

'ut
'PB0: Lifesignal
'PB1: Link activity

'serial
'PD0: Rx
'PD1: Tx
'PD2: Mode

Dim Send As String * 30 , Stored_id As Eram Byte
Dim Serialcharwaiting As Byte , Serialchar As Byte
Dim Comminput As String * 9 , Com_value As Word
Dim Com_com As String * 1 , Com_nr As String * 1
Dim Led As Byte , Com_stat As String * 4 , Status As Byte
Dim Value As Word , Values As String * 4 , Id As Byte , Ids As String * 2

Dim Crc As Byte
Dim Verinfo As String * 20

Config Adc = Single , Prescaler = Auto , Reference = Avcc
Start Adc

Const Min_id = 32
Const Max_id = 125
Const Pwm_max = 255
Const Out_max = 0
Const Stat_max = 7

Led_life Alias Portb.0
Led_act Alias Portb.1

If Stored_id >= Min_id And Stored_id <= Max_id Then Id = Stored_id Else Id = Min_id

Ids = Hex(id)                                               'module id number
Const Status_serial = "156"                                 'serial number
Const Status_name = "OMU3"                                  'unit name
Const Status_verboot = "1.0.0"                              'status version bootloader
Const Status_verprot = "2.1.1"                              'status version protocol
Const Status_dio = "0000"                                   'digital inputs, outputs
Const Status_ai = "030A"                                    'analog inputs, bits
Const Status_ao = "0000"                                    'analog outputs, bits

Start Watchdog                                              'startup parameters
Set Status.0
If Id = Min_id Then Set Status.1

Main:
Serialcharwaiting = Ischarwaiting()

If Serialcharwaiting = 1 Then                               'check if serial received
   Serialchar = Inkey()
   If Serialchar = Id Or Serialchar = 126 Then              'look for address or broadcast
      Led = 203
      Goto Set_value
      End If
   End If

If Led > 0 Then Decr Led                                    'activity LED timer
If Led = 200 Then Led_act = 1
If Led = 0 Then Led_act = 0

If Status = 0 Then                                          'life led & statusbyte set
   Led_life = 1
   Else
   Led_life = 0
   Led_act = 1
   End If

Reset Watchdog
Waitus 50
Goto Main
End

Set_value:                                                  'serial receive
Input Comminput Noecho                                      'read serialport

Com_com = Mid(comminput , 2 , 1)                            'command check
Com_nr = Mid(comminput , 4 , 1)                             'output nr check
Com_stat = Mid(comminput , 6 , 4)                           'output full check
Com_value = Hexval(com_stat)

If Com_com = "o" Then                                       'output
Select Case Com_nr

Case "0"                                                    'set digital output status
   Value = 0                                                'get digital output status
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",o,0:" + Values
   Gosub Serialsend
   'Goto Main

End Select
Goto Main
End If

If Com_com = "i" Then                                       'input
Select Case Com_nr

Case "0"                                                    'get digital input status
   Value = 0
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,0:" + Values
   Gosub Serialsend
   'Goto Main

Case "1"                                                    'analog input 1
   Value = Getadc(0)
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,1:" + Values
   Gosub Serialsend
   'Goto Main

Case "2"                                                    'analog input 2
   Value = Getadc(1)
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,2:" + Values
   Gosub Serialsend
   'Goto Main

Case "3"                                                    'analog input 3
   Value = Getadc(2)
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,3:" + Values
   Gosub Serialsend
   'Goto Main

End Select
Goto Main
End If

If Com_com = "s" Then                                       'status
Select Case Com_nr

Case "0"                                                    'status byte
   If Com_stat <> "" Then
      If Com_value > Stat_max Then Com_value = Stat_max     'max binary value
      If Com_value.0 = 1 Then Reset Status.0                'bootflag
      If Com_value.1 = 1 Then Reset Status.1                'default address
      If Com_value.2 = 1 Then Toggle Status.2               'manual fail
      End If
   Values = Hex(status)
   'Values = Format(values , "0000")
   Send = Ids + ",s,0:" + Values
   Gosub Serialsend

Case "1"                                                    'serial number
   Send = Ids + ",s,1:" + Status_serial
   Gosub Serialsend
Case "2"                                                    'unit name
   Send = Ids + ",s,2:" + Status_name
   Gosub Serialsend
Case "3"                                                    'firmware version
   Verinfo = Version(2)
   Send = Ids + ",s,3:" + Verinfo
   Gosub Serialsend
Case "4"                                                    'compiled date
   Verinfo = Version()
   Send = Ids + ",s,4:" + Verinfo
   Gosub Serialsend
Case "5"                                                    'bootloader version
   Send = Ids + ",s,5:" + Status_verboot
   Gosub Serialsend
Case "6"                                                    'protocol version
   Send = Ids + ",s,6:" + Status_verprot
   Gosub Serialsend
Case "7"                                                    'digital I/Os
   Send = Ids + ",s,7:" + Status_dio
   Gosub Serialsend
Case "8"                                                    'analog inputs & bits
   Send = Ids + ",s,8:" + Status_ai
   Gosub Serialsend
Case "9"                                                    'analog outputs & bits
   Send = Ids + ",s,9:" + Status_ao
   Gosub Serialsend

End Select
Goto Main
End If

If Com_com = "u" Then                                       'setup
Select Case Com_nr

Case "0"                                                    'reboot
   Send = Ids + ",u,0:0001"
   Gosub Serialsend
   Wait 1

Case "1"                                                    'address
   If Com_value >= Min_id And Com_value <= Max_id Then      'store address
      Stored_id = Com_value
      Id = Stored_id
      End If
   Send = Ids + ",u,1:00" + Hex(id)
   Gosub Serialsend
   If Ids <> Hex(id) Then Wait 1                            'reboot if address change

End Select
Goto Main
End If

Goto Main
End

Serialsend:
   Crc = Checksum(send)
   Print Send + "#" + Str(crc)
   Return
End
