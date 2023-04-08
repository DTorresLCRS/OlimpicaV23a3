%ENVIRON T
!
!---------------------------------------------------------------------------
! Variables para almacenar y repetir en un momento dado la última secuencia
! de operaciones contra el datáfono
!---------------------------------------------------------------------------
Integer*1 repeatLastSequence%, simulateSerialDev%
String lastSequence$(1), lastSequence0$(1)
Integer*2 lastSequenceCounter%, lastSequenceCounter0%
!---------------------------------------------------------------------------
!
Integer*4 Global \
	EP.SHORT.WAIT%
Integer*2 Global \
	EP.MAX.TRIES%,	\
	EP.IOPARM%
Integer*1 Global \
	EP.NO.TRACE%
!
String Global SERIAL.REQUEST$
!
Sub printDebug(pMsg$) External
	String pMsg$
End Sub
!
Sub JavaCall.Initialize.Request(ClassName,MethodName,TheRequest) External
	String ClassName, MethodName, TheRequest
End Sub
!
Sub JavaCall.AddParameter.String(TheRequest,TheParameter) External
	String TheRequest, TheParameter
End Sub
!
Sub JavaCall.AddParameter.Integer4(TheRequest, TheParameter) External
	String TheRequest
	Integer*4 TheParameter
End Sub
!
Sub JavaCall.InvokeMethod.ReturnVoid(TheRequest,Exception) External
	String TheRequest, Exception
End Sub
!
Sub JavaCall.InvokeMethod.ReturnI4(TheRequest, ReturnValue, Exception) External
	String TheRequest, Exception
	Integer*1 ReturnValue
End Sub
!
Sub JavaCall.InvokeMethod.ReturnString(TheRequest,ReturnValue, Exception) External
	String TheRequest, ReturnValue, Exception
End Sub
!
Sub APPMGR.COMM.INVOKEVOID(pMethod$)
	String pMethod$
	String tmpClassName$, tmpException$
	!
	tmpClassName$ = "com.lacadena.comm.SerialCommManager"
	tmpException$ = ""
	!
	Call Javacall.Initialize.Request(tmpClassName$, pMethod$, SERIAL.REQUEST$)
	!
	Call JavaCall.InvokeMethod.ReturnVoid(SERIAL.REQUEST$, tmpException$)
End Sub
!
Function simulateOpenPort
	Integer*1 simulateOpenPort, tmpAnswer%
	!
	lastSequenceCounter% = 0
	If repeatLastSequence% Then Begin
		tmpAnswer% = 0
	Endif Else Begin
		Dim lastSequence$(10)
		!
		tmpAnswer% = 0
		Call printDebug("Abriendo r::c:/tmp/jserial.sml")
		On Error Goto openSerial.err
		Open "r::c:/tmp/jserial.sml" As 98 Nowrite Nodel
		Call printDebug("Archivo abierto ok")
		Goto openSerial.end
		!
		openSerial.err:
		Call printDebug("Error abriendo r::c:/tmp/jserial.sml")
		tmpAnswer% = 1
		Resume openSerial.end
		!
	Endif
	!
	openSerial.end:
		simulateOpenPort = tmpAnswer%
End Function
!
Sub simulateClosePort
	Call printDebug("Cerrando r::c:/tmp/jserial.sml")
	On Error Goto closeSerial.err
	Close 98
	Call printDebug("Cerrado ok")
	Goto closeSerial.end
	!
	closeSerial.err:
	Call printDebug("Error cerrando")
	Resume closeSerial.end
	!
	closeSerial.end:
End Sub
!
Sub simulateReadSerialPort(pReturn$, pException$)
	String pReturn$, pException$
	!
	pReturn$ = ""
	pException$ = ""
	Call printDebug("Leyendo r::c:/tmp/jserial.sml")
	On Error Goto readSerial.err
	Read #98 ; Line pReturn$
	Call printDebug("Lectura ok")
	!
	If lastSequenceCounter% <= 10 Then Begin
		lastSequence$( lastSequenceCounter% ) = pReturn$
		lastSequenceCounter% = lastSequenceCounter% + 1
	Endif
	!
	Goto readSerial.end
	!
	readSerial.err:
	pException$ = "Error leyendo archivo jserial.sml"
	Resume readSerial.end
	!
	readSerial.end:
End Sub
!
Function APPMGR.COMM.OPENSERIAL%(PORT%,SPEED%,PARITY$,DATABITS%,STOPBITS%,SESSION%,BUFFERSIZE%,MISC$) Public
	Integer*2 PORT%,SPEED%,DATABITS%,STOPBITS%,SESSION%,BUFFERSIZE%
	Integer*1 APPMGR.COMM.OPENSERIAL%, tmpAnswer%
	String PARITY$, MISC$
	String tmpClassName$, tmpMethodName$, tmpException$, tmpReturn$
	!
	simulateSerialDev% = 0
	!
	lastSequenceCounter% = 0
	If repeatLastSequence% Then Begin
		tmpAnswer% = 0
	Endif Else If simulateSerialDev% Then Begin
		tmpAnswer% = simulateOpenPort
	Endif Else Begin
		Dim lastSequence$(10)
		!
		tmpClassName$ = "com.lacadena.comm.SerialCommManager"
		tmpMethodName$ = "openPort"
		tmpException$ = ""
		!
		Call Javacall.Initialize.Request(tmpClassName$, tmpMethodName$, SERIAL.REQUEST$)
		!
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, Str$(PORT%))
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, Str$(SPEED%))
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, PARITY$)
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, Str$(DATABITS%))
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, Str$(STOPBITS%))
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, Str$(BUFFERSIZE%))
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, MISC$)
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, Str$(EP.SHORT.WAIT%))
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, Str$(EP.MAX.TRIES%))
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, Str$(EP.NO.TRACE%))
		!
		Call JavaCall.InvokeMethod.ReturnString(SERIAL.REQUEST$, tmpReturn$, tmpException$)
		!
		If tmpException$ = "" Then \
			tmpAnswer% = Int%(Val(tmpReturn$)) \
		Else Begin
			Call printDebug("Err:" + tmpException$)
			tmpAnswer% = 1
		Endif
	Endif
	!
	APPMGR.COMM.OPENSERIAL% = tmpAnswer%
End Function
!
Sub APPMGR.COMM.CLOSESERIAL(SESSION%) Public
	Integer*2 SESSION%
	!
	String tmpMethodName$
	!
	If repeatLastSequence% Then Begin
		repeatLastSequence% = 0
	Endif Else If simulateSerialDev% Then Begin
		Call simulateClosePort
	Endif Else Begin
		tmpMethodName$ = "closePort"
		!
		Call APPMGR.COMM.INVOKEVOID(tmpMethodName$)
	Endif
End Sub
!
Function APPMGR.COMM.READSERIALACK$(SESSION%,TIMEOUT%,SENDACK%) Public
	Integer*2 SESSION%,SENDACK%
	Integer*4 TIMEOUT%
	String APPMGR.COMM.READSERIALACK$, tmpSend$
	!
	String tmpClassName$, tmpMethodName$, tmpException$
	String tmpReturn$
	!
	tmpException$ = ""
	If repeatLastSequence% Then Begin
		If lastSequenceCounter% > 10 Then Begin
			tmpException$ = "SU0000007E"
		Endif Else Begin
			tmpReturn$ = lastSequence$( lastSequenceCounter% )
			lastSequenceCounter% = lastSequenceCounter% + 1
		Endif
	Endif Else If simulateSerialDev% Then Begin
		Call simulateReadSerialPort(tmpReturn$, tmpException$)
	Endif Else Begin
		tmpClassName$ = "com.lacadena.comm.SerialCommManager"
		tmpMethodName$ = "read"
		If SENDACK% Then \
			tmpSend$ = "1" \
		Else \
			tmpSend$ = "0"
		!
		Call Javacall.Initialize.Request(tmpClassName$, tmpMethodName$, SERIAL.REQUEST$)
		!
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, Str$(TIMEOUT%))
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, tmpSend$)
		!
		Call JavaCall.InvokeMethod.ReturnString(SERIAL.REQUEST$, tmpReturn$, tmpException$)
		!
		If lastSequenceCounter% <= 10 Then Begin
			lastSequence$( lastSequenceCounter% ) = tmpReturn$
			lastSequenceCounter% = lastSequenceCounter% + 1
		Endif
	Endif
	!
	If tmpException$ = "" Then \
		APPMGR.COMM.READSERIALACK$ = tmpReturn$ \
	Else Begin
		Call printDebug("Err:" + tmpException$)
		APPMGR.COMM.READSERIALACK$ = ""
	Endif
End Function
!
Function APPMGR.COMM.WRITESERIAL%(SESSION%,MESSAGE$,WAITFORACK%) Public
	Integer*2 SESSION%
	Integer*4 WAITFORACK%
	Integer*1 APPMGR.COMM.WRITESERIAL%
	String MESSAGE$
	!
	String tmpClassName$, tmpMethodName$, tmpException$, tmpReturn$
	!
	If repeatLastSequence% <> 0 Or simulateSerialDev% <> 0  Then Begin
		APPMGR.COMM.WRITESERIAL% = 0
	Endif Else Begin
		tmpClassName$ = "com.lacadena.comm.SerialCommManager"
		tmpMethodName$ = "write"
		tmpException$ = ""
		!
		Call Javacall.Initialize.Request(tmpClassName$, tmpMethodName$, SERIAL.REQUEST$)
		!
		Call JavaCall.AddParameter.String(SERIAL.REQUEST$, MESSAGE$)
		!
		Call JavaCall.InvokeMethod.ReturnString(SERIAL.REQUEST$, tmpReturn$, tmpException$)
		!
		If tmpException$ = "" And tmpReturn$ <> "" Then \
			APPMGR.COMM.WRITESERIAL% = Int%(Val(tmpReturn$)) \
		Else Begin
			Call printDebug("Err:" + tmpException$)
			APPMGR.COMM.WRITESERIAL% = 1
		Endif
	Endif
End Function
!
Sub APPMGR.COMM.ACCEPT Public
	!
	String tmpMethodName$
	!
	If repeatLastSequence% = 0 And simulateSerialDev% = 0 Then Begin
		tmpMethodName$ = "acceptsMessage"
		!
		Call APPMGR.COMM.INVOKEVOID(tmpMethodName$)
	Endif
End Sub
!
Sub APPMGR.COMM.REFUSE Public
	!
	String tmpMethodName$
	!
	If repeatLastSequence% = 0 And simulateSerialDev% = 0 Then Begin
		tmpMethodName$ = "refusesMessage"
		!
		Call APPMGR.COMM.INVOKEVOID(tmpMethodName$)
	Endif
End Sub
!
Sub setRepeatLastSequence(pValue%) Public
	Integer*1 pValue%
	!
	lastSequenceCounter% = 0
	If pValue% <> 0 Then Begin
		repeatLastSequence% = -1
	Endif Else Begin
		Dim lastSequence$(10)
		Dim lastSequence0$(10)
		repeatLastSequence% = 0
	Endif
End Sub
!
Sub saveLastSequence Public
	Integer*2 tmpCounter%
	!
	Dim lastSequence0$(10)
	lastSequenceCounter0% = lastSequenceCounter%
	For tmpCounter% = 0 To lastSequenceCounter% - 1
		lastSequence0$(tmpCounter%) = lastSequence$(tmpCounter%)
	Next tmpCounter%
End Sub
!
Sub restoreLastSequence Public
	Integer*2 tmpCounter%
	!
	Dim lastSequence$(10)
	lastSequenceCounter% = lastSequenceCounter0%
	For tmpCounter% = 0 To lastSequenceCounter% - 1
		lastSequence$(tmpCounter%) = lastSequence0$(tmpCounter%)
	Next tmpCounter%
End Sub
!
Function getLastSequence Public
	String getLastSequence
	!
	If lastSequenceCounter% > 0 Then Begin
		getLastSequence = lastSequence$( lastSequenceCounter% - 1 )
	Endif Else Begin
		getLastSequence = ""
	Endif
End Function
!
Sub addLastSequence(pSequence$) Public
	String pSequence$
	!
	lastSequence$( lastSequenceCounter% ) = pSequence$
	lastSequenceCounter% = lastSequenceCounter% + 1
End Sub
!
