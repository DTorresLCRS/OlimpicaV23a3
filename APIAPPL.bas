!*********************************************************************
!                         (APIAPPL.BAS)
!                           (Ver 2.00)
!
%ENVIRON T
!
! Supermarket application global data
!
%INCLUDE C:\ADX_UPGM\EAMTSWKG.J86

%INCLUDE C:\ADX_UPGM\EAMTOPTS.J86

%INCLUDE C:\ADX_UPGM\EAMTRANS.J86

%INCLUDE C:\APPL\APIVARI.J86

! Indice del medio de pago cuyo tipo de verificación se ha modificado temporalmente
Integer*2 EP.VERIF.TV.IDX%
!
!----------------------------------------------------------------------------------
! 2021-10-07 jsv
! Variables para despliegue de mensajes
!----------------------------------------------------------------------------------
Integer*1 appl.displayMsg%
String appl.displayMsg1$, appl.displayMsg2$
!----------------------------------------------------------------------------------
! 2021-09-06 jsv
! Información complementaria para reversos de secuencia TCRO
!----------------------------------------------------------------------------------
String Global EP.REV.ACCOUNT$
Integer*2 Global EP.REV.VOUCHER.START%, EP.REV.VOUCHER.END%
!----------------------------------------------------------------------------------

! Campos opcionales para identificar una operación
String Global EP.OPT.CONVENIO$, EP.OPT.BOLSILLO$

! Variables para control de teclas que requieren llave
! en forma dinamica
Integer*1 EP.DYKEY.ACT%
Integer*1 EP.DYKEY.PRC%
String EP.DYKEY.KEY$, EP.DYKEY.MSG1$, EP.DYKEY.MSG2$
                                          
  string global               \
    gv.tarjetaCliente$       ,\
    gv.tipoVarTarjPriv$      ,\
    gv.idComercio$           ,\
    gv.nombrecajero$         ,\
    gv.idCliente$            ,\
    gv.segmentoCliente$      ,\
    gv.nombreCliente$        ,\
    gv.tipoCuenta$           ,\
    gv.inicioCampoVar$       ,\ ! Variables con alineación a la izquierda
    gv.inicioCampoVarR$      ,\ ! Variables con alineación a la derecha
    gv.inicioCampoVarM$      ,\ ! Variables con alineación al centro
    FISSUC$
    
 
 integer*4 global             \
  gv.I4UserField1%           ,\
  gv.I4UserField2%           ,\
  gv.I4UserField3%           ,\
  gv.I4UserField4%           ,\
  gv.acumVentasTrx%          ,\
  gv.acumComprasTotal%       ,\
  gv.acumPuntosTotal%        ,\
  gv.acumPuntosTrx%          ,\
  gv.bonifPuntosTrx%         ,\
  gv.acumComprasPeriodo%     ,\
  gv.acumPuntosPeriodo%      ,\
  gv.acumPuntosRedimTotal%   ,\
  gv.acumPuntosRedimPeriodo% ,\
  gv.acumPuntosRedimTrx%
  
  
! Manejo de nvram desde java
	Integer*4 EP.nvramPointer%
	String EP.nvramFormat$,EP.nvramMessage$
!
! user data del cliente
!

 integer*4 global             \       
  NFF.EXCLU%                 ,\
  IVA.tdes%                  ,\
  totalDesctoPago%           ,\
  MEX.VAT.TOTAL(1)           ,\
  IC% 
!
Integer*1 Global EP.trxRecaudoT%,EP.ENVIO.RECAUDO.TCRO%
!
String EP.DATA.REGULAR$
!
! Flag que indica que hay una transacción applmanager en curso
Integer*1 EP.applMgr.running%
!
 string   global              \
   ue.clf.cliente$           ,\
   N.TRX$
 
 ! Variables para manejar las franquicias cuyas operaciones
 ! no requieren la impresion de un voucher formal en TCRO
 ! En estos casos, se imprime un resumen de las transacciones
 ! realizadas para cada una de las franquicias
 Integer*2 franqResLimit%		! Limite de franquicias resumen (para dimensionar arreglo)
 Integer*2 franqResCount%		! Número de franquicias resumen
 Integer*2 global franqDetLimit% ! Limite de transacciones TEF para las franquicias que NO son resumen
 Integer*2 global franqDetCount% ! Contador de transacciones TEF para las franquicias que NO son resumen
 Integer*2 global franqResTrxLimit% ! Limite de transacciones TEF para las franquicias que SI son resumen
 Integer*2 global franqResTrxCount% ! Contador de transacciones TEF para las franquicias que SI son resumen
 String franqResDes$(1)			! Descriptores de las franquicias resumen
 String franqResMov$(2)			! Movimientos de las franquicias resumen
 Integer*1 franqResFlag%		! Flag que indica si la franquicia actual es una franquicia resumen
 Integer*1 global warningSMA%
 Integer*1 warningGeneral%
 !
 String EP.VALOR.SALDO$
 !
!   fin user data del cliente
!

!
! end of data definitions
!------------------------------------------------------------------------
!
!  Standard supermarket routines
!
SUB TSPREC01 EXTERNAL
END SUB
!
SUB TSDSEC01 EXTERNAL
END SUB
!
SUB TSTPEC01 EXTERNAL
END SUB
!
SUB TSCSEC08 EXTERNAL
END SUB
!
SUB TSCSECRK EXTERNAL
END SUB
!
SUB TSTDEC01 EXTERNAL
END SUB
!
FUNCTION FORMAT.AMOUNT(EP.INT4) EXTERNAL 
    INTEGER*1 FORMAT.AMOUNT
    INTEGER*4 EP.INT4 
END FUNCTION        
!---------------------------------------------------------------------------
!  System routines
SUB ADXSERVE (RET,FUNC,PARM1,PARM2) EXTERNAL
  INTEGER*4 RET
  INTEGER*2 FUNC,PARM1
  STRING PARM2
END SUB
!---------------------------------------------------------------------------
!  User defined routines
!
!
Sub printDebug(pMessage$) External
End Sub
!
Function RESET14.isTef External
	Integer*1 RESET14.isTef
End Function
!
Sub printMessage(message$)
    String message$
    TS.LINETYPE = 29
    TS.SAVPRT$ = message$
    TS.SAVPRT.OPT = 4100H
    CALL TSPREC01
End Sub
!
!
!
SUB INT4.TO.HEXA(ERRVALUE%,ERRVALUE$) PUBLIC
  INTEGER*4 ERRVALUE%
  STRING ERRVALUE$
  INTEGER*4 HX%,SX%,THE.SUM%,S%
  STRING ERRFX$,Z$
  ERRVALUE$ = ""
  HX%=ERRVALUE%
  ERRFX$=""
  FOR S%=28 TO 0 STEP -4
  SX%=SHIFT(HX%,S%)
  THE.SUM%=SX%AND 000FH
  IF THE.SUM%>9 THEN THE.SUM%=THE.SUM%+55 \
  ELSE THE.SUM%=THE.SUM%+48
  Z$=CHR$(THE.SUM%)
  ERRFX$=ERRFX$+Z$
  NEXT S%
  ERRVALUE$ =ERRFX$
END SUB
!
!   *** ASYNCRONOUS ERROR HANDLING  *********
!
SUB ASYNC.ERR(RFLAG,OVER) PUBLIC
  INTEGER*2 RFLAG
  STRING OVER
  STRING ERRFX$
  RFLAG = 0
  OVER = ""
  EP.ERRNCODE% = 1
  EP.ASYNC.ERROR% = -1
  EP.STATE% = 0
  IF ERRF% = EP.EIAP% THEN EP.DEVICE.STATE% = 98
  CALL INT4.TO.HEXA(ERRN,ERRFX$)
  TS.LINETYPE = 29
  TS.SAVPRT$ = "an async error has occurred"
  TS.SAVPRT.OPT = 4100H
  CALL TSPREC01
  TS.LINETYPE = 29
  TS.SAVPRT$ = "ERR ="+ERR+" ERRL ="+STR$(ERRL)
  TS.SAVPRT.OPT = 4100H
  CALL TSPREC01
  TS.LINETYPE = 29
  TS.SAVPRT$ = "ERRF ="+STR$(ERRF%)+" ERRN ="+ERRFX$
  TS.SAVPRT.OPT = 4100H
  CALL TSPREC01
  WAIT ;15000
  RESUME
END SUB
!
!
SUB ERRORTRAP  PUBLIC
  !*ERROR ASSEMBLY ROUTINE *!
  EP.HX%=ERRN
  EP.ERRNCODE% = 1
  EP.ASYNC.ERROR% = 0
  IF ERRF% = EP.EIAP% THEN EP.DEVICE.STATE% = 98
  CALL INT4.TO.HEXA(EP.HX%,EP.ERRFX$)
  TS.LINETYPE = 29
  TS.SAVPRT$ = "a runtime error has occurred"
  TS.SAVPRT.OPT = 4100H
  CALL TSPREC01
  TS.LINETYPE = 29
  TS.SAVPRT$ = "ERR ="+ERR+" ERRN% ="+STR$(EP.HX%)
  TS.SAVPRT.OPT = 4100H
  CALL TSPREC01
  TS.LINETYPE = 29
  TS.SAVPRT$ = "ERRF ="+STR$(ERRF%)+"ERRN ="+EP.ERRFX$
  TS.SAVPRT.OPT = 4100H
  CALL TSPREC01
  RESUME
!
END SUB
!
SUB EP.GET.CURRENT.TIME
  EP.TIME$ = TIME$
  EP.CURRENT.TIME% = VAL(LEFT$(EP.TIME$,2))*3600 + \ !! initial time seconds
    VAL(MID$(EP.TIME$,3,2))* 60 + VAL(RIGHT$(EP.TIME$,2))
END SUB
!
SUB EP.GETUNPK PUBLIC                                  !
    EP.K% = MATCH(":",EP.B$,EP.J%)                     ! Busca separador Campo
    IF EP.K% > EP.J% THEN \
      EP.A$ = UNPACK$(MID$(EP.B$,EP.J%,EP.K%-EP.J%))  \   ! y lo desempaqueta
    ELSE \
      EP.A$ = ""
    EP.J% = EP.K% + 1                                  ! Marca inicio del 
END SUB                                                ! siguiente campo 
!
SUB EP.SAVE.PRINT PUBLIC
      DIM PRT.USER.SAVE(12)
      PRT.USER.SAVE(1) = TS.LINETYPE
      PRT.USER.SAVE(2) = TS.LINEDATA
      PRT.USER.SAVE(3) = TS.LINEDATA2
      PRT.USER.SAVE(4) = TS.LINEDATA3
      PRT.USER.SAVE(5) = TS.PRT.PARM
      PRT.USER.SAVE(6) = TS.PRT.OPT
      PRT.USER.SAVE(7) = TS.PRT.SJDI
      PRT.USER.SAVE(8) = TS.SAVPRT.OPT
      PRT.USER.SAVE(9) = TS.XXMOD
      PRT.USER.SAVE(10) = TS.YYMOD
      PRT.USER.SAVE(11) = TS.ZMOD
      PRT.USER.SAVE(12) = TS.PRINTPRM
      DIM PRT.USER.SAVE$(5)
      PRT.USER.SAVE$(1) = TS.PRTBUF$
      PRT.USER.SAVE$(2) = TS.PRDATA$
      PRT.USER.SAVE$(3) = TS.FORMCR$
      PRT.USER.SAVE$(4) = TS.SJDATA$
      PRT.USER.SAVE$(5) = TS.SAVPRT$
END SUB
!
SUB EP.RESTORE.PRINT  PUBLIC
      TS.LINETYPE   = PRT.USER.SAVE(1)
      TS.LINEDATA   = PRT.USER.SAVE(2)
      TS.LINEDATA2  = PRT.USER.SAVE(3)
      TS.LINEDATA3  = PRT.USER.SAVE(4)
      TS.PRT.PARM   = PRT.USER.SAVE(5)
      TS.PRT.OPT    = PRT.USER.SAVE(6)
      TS.PRT.SJDI   = PRT.USER.SAVE(7)
      TS.SAVPRT.OPT = PRT.USER.SAVE(8)
      TS.XXMOD      = PRT.USER.SAVE(9)
      TS.YYMOD      = PRT.USER.SAVE(10)
      TS.ZMOD       = PRT.USER.SAVE(11)
      TS.PRINTPRM   = PRT.USER.SAVE(12)
      TS.PRTBUF$    = PRT.USER.SAVE$(1)
      TS.PRDATA$    = PRT.USER.SAVE$(2)
      TS.FORMCR$    = PRT.USER.SAVE$(3)
      TS.SJDATA$    = PRT.USER.SAVE$(4)
      TS.SAVPRT$    = PRT.USER.SAVE$(5)
END SUB
!
SUB EP.LINE.PRINT(LINEA$,ESTACION%) PUBLIC
    STRING LINEA$
    INTEGER*2 ESTACION%
    CALL EP.SAVE.PRINT  
    TS.LINETYPE = 29              
    IF LEN(LINEA$) > 38 THEN \
      TS.SAVPRT$ = LEFT$(LINEA$,38) \
    ELSE \
      TS.SAVPRT$ = LINEA$ 
    TS.SAVPRT.OPT = ESTACION%         
    CALL TSPREC01                       
    CALL EP.RESTORE.PRINT
END SUB
!
Sub printDebugGeneral(pMsg$) Public
	String pMsg$
	Integer*1 tmpUE20%,tmpUE60%
	!
	If EP.NO.TRACE% = 0 Then \
	Begin
		tmpUE20% = TO.USEREXIT(20)
		tmpUE60% = TO.USEREXIT(60)
		TO.USEREXIT(20) = 0
		TO.USEREXIT(60) = 0
		CALL EP.LINE.PRINT(pMsg$,4100h)
		TO.USEREXIT(20) = tmpUE20%
		TO.USEREXIT(60) = tmpUE60%
	Endif
End Sub
!
SUB EP.SAVE.DISPLAY PUBLIC
  DIM DIS.USER.SAVE(7)
  DIS.USER.SAVE(1) = TS.LINETYPE
  DIS.USER.SAVE(2) = TS.LINEDATA
  DIS.USER.SAVE(3) = TS.LINEDATA2
  DIS.USER.SAVE(4) = TS.LINEDATA3
  DIS.USER.SAVE(5) = TS.XXMOD
  DIS.USER.SAVE(6) = TS.YYMOD
  DIS.USER.SAVE(7) = TS.DS.NOSAVE
  DIM DIS.USER.SAVE$(2)
  DIS.USER.SAVE$(1) = TS.SAVDISP1$
  DIS.USER.SAVE$(2) = TS.SAVDISP2$ 
END SUB
!
SUB EP.RESTORE.DISPLAY PUBLIC
  TS.LINETYPE = DIS.USER.SAVE(1)
  TS.LINEDATA = DIS.USER.SAVE(2)
  TS.LINEDATA2 = DIS.USER.SAVE(3)
  TS.LINEDATA3 = DIS.USER.SAVE(4) 
  TS.XXMOD = DIS.USER.SAVE(5)
  TS.YYMOD = DIS.USER.SAVE(6)
  TS.DS.NOSAVE = DIS.USER.SAVE(7)
  TS.SAVDISP1$ = DIS.USER.SAVE$(1)
  TS.SAVDISP2$ = DIS.USER.SAVE$(2)
END SUB
!
SUB EP.DISPLAY.A.MESSAGE(UE.DISP.MESSAGE$) PUBLIC 
  STRING UE.DISP.MESSAGE$, UE.WORK$
  CALL EP.SAVE.DISPLAY
  UE.WORK$ = UE.DISP.MESSAGE$ + STRING$(40," ")
  TS.TEMP1$ = LEFT$(UE.WORK$,20)
  TS.TEMP2$ = MID$(UE.WORK$,21,20)
  TS.LINETYPE = 12
  CALL TSDSEC01
  CALL EP.RESTORE.DISPLAY
  UE.WORK$ = ""
END SUB
!
SUB EP.DISPLAY.AN.ERROR(UE.DISP.MESSAGE$) PUBLIC 
  STRING UE.DISP.MESSAGE$, UE.WORK$
  CALL EP.SAVE.DISPLAY
  UE.WORK$ =  LEFT$(UE.DISP.MESSAGE$+STRING$(40," "),40)
  TS.TEMP1$ = LEFT$(UE.WORK$,20)
  TS.TEMP2$ = MID$(UE.WORK$,21,20) 
  TS.LINETYPE = 12
  CALL TSCSEC08
  CALL EP.RESTORE.DISPLAY
  UE.WORK$ = ""
END SUB
!
SUB EP.SAVE.KEYS PUBLIC
  INTEGER*2 I.2% 
  DIM KBD.USER.SAVE$(10)
  DIM KBD.USER.SAVE(11)
  FOR I.2% = 1 TO 10 
    KBD.USER.SAVE$(I.2%) = TS.IO.DATA$(I.2%)
    KBD.USER.SAVE(I.2%)  = TS.IO.KEYS(I.2%)
  NEXT I.2%  
  KBD.USER.SAVE(11) = TS.IO.MOTORKEY 
END SUB
!
SUB EP.RESTORE.KEYS PUBLIC
  INTEGER*2 I.2% 
  FOR I.2% = 1 TO 10 
    TS.IO.DATA$(I.2%) = KBD.USER.SAVE$(I.2%)
    TS.IO.KEYS(I.2%)  = KBD.USER.SAVE(I.2%)
  NEXT I.2%  
  TS.IO.MOTORKEY = KBD.USER.SAVE(11)
END SUB
!--------------------------------------------------------------------------
!   Java API routines
SUB JavaCall.Initialize.Request(ClassName,MethodName,TheRequest) EXTERNAL
  STRING ClassName
  STRING MethodName
  STRING TheRequest
END SUB

SUB JavaCall.AddParameter.String(TheRequest,TheParameter) EXTERNAL
  STRING TheRequest
  STRING TheParameter
END SUB

SUB JavaCall.InvokeMethod.ReturnString(TheRequest,ReturnValue, Exception) EXTERNAL
  STRING TheRequest
  STRING ReturnValue
  STRING Exception
END SUB
!
SUB getOperatorName$ public
  string operName$ , filler1$, filler2$ 
! 
  ts.er.return = -1  
!  open "R::EAMOPERA" keyed recl 72 as ep.ioparm% nowrite nodel    ! Read operator file
  if ts.er.return then  \
  begin 
    if end #ep.ioparm%  then  NO.OPERATOR
    read  form "C32 C20 C20";#44 key ts.oper$;    \!
    filler1$, operName$, filler2$       
!
    gv.nombrecajero$ = operName$
!
  NO.OPERATOR:
!
!    close ep.ioparm%                                  !
  endif  \ 
  else \
    gv.nombrecajero$ = "no hay acceso operad"
!
END SUB  

!
!--------------------------------------------------------------------------
!   Application manager routines
!---------------------------------------------------------
!
Function rRemove$(line$,charCode%) public
    String rRemove$,line$
    Integer*1 i%,continue%,actualChar%,charCode%
    Integer*2 lineLen%,newLen%
    lineLen% = Len(line$)
    newLen% = lineLen%
    continue% = -1
    i% = lineLen%
    while i% >= 1 and continue%
        actualChar% = Asc(mid$(line$,i%,1))
        if actualChar% = charCode% then \
            newLen% = newLen% - 1 \
        else \
            continue% = 0
        i% = i% - 1
    Wend
    rRemove$ = left$(line$,newLen%)
End Function
!
Function rTrim$(line$) public
    String rTrim$,line$
    rTrim$ = rRemove$(line$,32)
End Function
!
Function lRemove$(line$,charCode%) public
    String lRemove$,line$
    Integer*1 i%,continue%,actualChar%,charCode%
    Integer*2 lineLen%,newLen%
    lineLen% = Len(line$)
    newLen% = lineLen%
    continue% = -1
    i% = 1
    while i% <= lineLen% and continue%
        actualChar% = Asc(mid$(line$,i%,1))
        if actualChar% = charCode% then \
            newLen% = newLen% - 1 \
        else \
            continue% = 0
        i% = i% + 1
    Wend
    lRemove$ = Right$(line$,newLen%)
End Function
!
Function lTrim$(line$) public
    String lTrim$,line$
    lTrim$ = lRemove$(line$,32)
End Function
!
Function trim$(line$) public
    String trim$,line$
    trim$ = lTrim$(rTrim$(line$))
End Function
!
Function align$(strToAlign$,fillChar$,lineLen%,alignMode%) public
    String align$,strToAlign$,fillChar$
    Integer*1 alignMode%,i%
    Integer*2 lineLen%,realLen%,lenToAdd%,lenToAdd1%
    strToAlign$ = trim$(strToAlign$)
    realLen% = Len(strToAlign$)
    lenToAdd% = lineLen% - realLen%
    if alignMode% = 0 then \        ! Alineación a la izquierda
    begin
        if lenToAdd% > 0 then \
            strToAlign$ = strToAlign$ + left$(String$(lenToAdd%,fillChar$),lenToAdd%) \
        else \
            strToAlign$ = left$(strToAlign$,lineLen%)
    endif \
    else if alignMode% = 1 then \   ! Alineación a la derecha
    begin
        if lenToAdd% > 0 then \
            strToAlign$ = left$(String$(lenToAdd%,fillChar$),lenToAdd%) + strToAlign$ \
        else \
            strToAlign$ = Right$(strToAlign$,lineLen%)
    endif \
    else \                          ! Alineación al centro
    begin
        if lenToAdd% > 0 then \
        begin
            lenToAdd1% = Int(lenToAdd% / 2)
            lenToAdd% = lenToAdd% - lenToAdd1%
            strToAlign$ = \
                left$(String$(lenToAdd%,fillChar$),lenToAdd%) + \
                strToAlign$ + \
                left$(String$(lenToAdd1%,fillChar$),lenToAdd1%)
        endif \
        else \
            strToAlign$ = left$(strToAlign$,lineLen%)
    endif
    align$ = strToAlign$
End Function
!
!
!
!
SUB EP.WRITE.TRANSNUM(UE.EPAY.TRANSNUM$)  PUBLIC
  STRING UE.WORK.TRANSNUM$, UE.EPAY.TRANSNUM$
!
  UE.WORK.TRANSNUM$ = PACK$(RIGHT$(STRING$(6,"0")+UE.EPAY.TRANSNUM$,6))
  UE.WORK.TRANSNUM$ = PACK$(UNPACK$(UE.WORK.TRANSNUM$)) ! garantiza un entero
!
  TS.ER.RETURN = -1
  WRITE FORM "C3"; #45, 15361 ; UE.WORK.TRANSNUM$ 
  IF NOT TS.ER.RETURN  THEN \   ! si no abre
  BEGIN
    CALL EP.DISPLAY.AN.ERROR("NO ESCRIBE RECIBO HT")
  ENDIF
!  CALL EP.DISPLAY.AN.ERROR("WRITE="+UNPACK$(UE.WORK.TRANSNUM$))
!
END SUB 
!
FUNCTION EP.READ.TRANSNUM  PUBLIC
  STRING EP.READ.TRANSNUM, UE.WORK.TRANSNUM$
!
  TS.ER.RETURN = -1
  READ FORM "C3" ;#45, 15361 ; UE.WORK.TRANSNUM$ 
  IF NOT TS.ER.RETURN  THEN \   ! si no abre
  BEGIN
    CALL EP.DISPLAY.AN.ERROR("SIN NRO RECIBO EN HT")
    UE.WORK.TRANSNUM$ = PACK$("000000")
    CALL EP.WRITE.TRANSNUM(UE.WORK.TRANSNUM$)
  ENDIF
  UE.WORK.TRANSNUM$ = PACK$(UNPACK$(UE.WORK.TRANSNUM$)) ! garantiza un entero
!  CALL EP.DISPLAY.AN.ERROR("LEI="+UNPACK$(UE.WORK.TRANSNUM$))
  EP.READ.TRANSNUM = UNPACK$(UE.WORK.TRANSNUM$)
!  
END FUNCTION 
!
FUNCTION EP.NEW.TRANSNUM  PUBLIC
  STRING EP.NEW.TRANSNUM, UE.WORK.TRANSNUM$
!
  UE.WORK.TRANSNUM$ = EP.READ.TRANSNUM
  UE.WORK.TRANSNUM$ = RIGHT$(STRING$(6,"0")+STR$(VAL(UE.WORK.TRANSNUM$) + 1),6)
!
  UE.WORK.TRANSNUM$ = PACK$(UE.WORK.TRANSNUM$)
  TS.ER.RETURN = -1
  WRITE FORM "C3"; #45, 15361 ; UE.WORK.TRANSNUM$ 
  IF NOT TS.ER.RETURN  THEN \   ! si no abre
  BEGIN
    CALL EP.DISPLAY.AN.ERROR("NO NUEVO RECIB EN HT")
  ENDIF
!
!  CALL EP.DISPLAY.AN.ERROR("NEW="+UNPACK$(UE.WORK.TRANSNUM$))
  EP.NEW.TRANSNUM = UNPACK$(UE.WORK.TRANSNUM$)
!
END FUNCTION 
!
Sub EP.INCREM.TRANSNUM(pIncrem%) Public
	Integer*2 pIncrem%
	String tmpCurrent$
	!
	tmpCurrent$ = EP.READ.TRANSNUM
	tmpCurrent$ = Pack$(Right$(String$(6, "0") + Str$(Val(tmpCurrent$) + pIncrem%), 6))
	Write Form "C3"; #45, 15361 ; tmpCurrent$ 
End Sub
!
SUB EP.WRITE.NVRAM(msgToWrite$,pointerToWrite%,formatToWrite$) PUBLIC
	String msgToWrite$,formatToWrite$
	Integer*4 pointerToWrite%
	TS.ER.RETURN = -1
	IF LEFT$(formatToWrite$,1) = "P" THEN \
	BEGIN
		formatToWrite$ = "C" + RIGHT$(formatToWrite$,LEN(formatToWrite$) - 1)
		WRITE FORM formatToWrite$; #45, pointerToWrite% ; PACK$(msgToWrite$)
	ENDIF ELSE \
		WRITE FORM formatToWrite$; #45, pointerToWrite% ; msgToWrite$
	IF NOT TS.ER.RETURN  THEN \   ! si no abre
    BEGIN
		CALL EP.DISPLAY.AN.ERROR("ERROR AL ESCRIBIR NVRAM")
	ENDIF	
END SUB
!
FUNCTION EP.READ.NVRAM(pointerToRead%,formatToRead$) PUBLIC
	String EP.READ.NVRAM,msgToRead$,formatToRead$
	Integer*4 pointerToRead%
	Integer*1 packed%
	IF LEFT$(formatToRead$,1) = "P" THEN \
		packed% = -1 \
	ELSE \
		packed% = 0
	formatToRead$ = "C" + RIGHT$(formatToRead$,LEN(formatToRead$) - 1)
	TS.ER.RETURN = -1
	READ FORM formatToRead$ ;#45, pointerToRead% ; msgToRead$
	IF NOT TS.ER.RETURN  THEN \   ! si no abre
	BEGIN
		CALL EP.DISPLAY.AN.ERROR("ERROR AL LEER NVRAM")
		msgToRead$ = ""
	ENDIF
	IF packed% THEN \
		EP.READ.NVRAM = UNPACK$(msgToRead$) \
	ELSE \
		EP.READ.NVRAM = msgToRead$
END FUNCTION
!
SUB EP.ADD.DATA.ENTRY(UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6) PUBLIC
  STRING UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6
  CALL EP.SAVE.KEYS                        ! SAVE KEYED DATA
  DIM TS.IO.KEYS(10)                       ! CLEAR KEYED DATA
  DIM TS.IO.DATA$(10)                      ! CLEAR KEYED DATA
  TS.IO.KEYS(10) = 63                      ! Data entry key
  TS.IO.KEYS(4)  = 100                     ! No-sale key
  TS.IO.DATA$(2) = UE.DATA1                ! Campo 1
  TS.IO.DATA$(3) = UE.DATA2		           ! Campo 2
  TS.IO.DATA$(4) = UE.DATA3	               ! Campo 3
  TS.IO.DATA$(5) = UE.DATA4		           ! Campo 4
  TS.IO.DATA$(6) = UE.DATA5	               ! Campo 5
  TS.IO.DATA$(7) = UE.DATA6		           ! Campo 6
  TS.TEMP1I1 = 11                          ! Data entry
  CALL TSTPEC01                            ! Process data entry
  CALL EP.RESTORE.KEYS
END SUB
!
SUB EP.ADD.DATA.ENTRY99(UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6) PUBLIC
  STRING UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6
!  
  TS.USERDATA$ = PACK$(UE.DATA1) + ":" + PACK$(UE.DATA2) + ":" + PACK$(UE.DATA3) \
    + ":" + PACK$(UE.DATA4) + ":" + PACK$(UE.DATA5) + ":" + PACK$(UE.DATA6)
     
  TS.TEMP1I1 = 99                  
  CALL TSTPEC01
!
END SUB
!

SUB EP.ADD.DATA.DENTRY99(UE.DATA1,UE.DATA2) PUBLIC
  STRING UE.DATA1,UE.DATA2
!
  TS.USERDATA$ = PACK$(UE.DATA1) + ":" + UE.DATA2
  TS.TEMP1I1 = 99                  
  CALL TSTPEC01
!
END SUB
!
function translateRange(range$) public
	String range$,range1$,currentCaracter$,previousCaracter$,translateRange
	Integer*2 counter1%,counter2%,repeat%
	!
	range$ = rTrim$(range$)
	if MATCH("x",range$,1) > 0 Or MATCH("X",range$,1) > 0 Then \
	Begin
		range1$ = ""
		for counter1% = 1 to len(range$)
			currentCaracter$ = mid$(range$,counter1%,1)
			If currentCaracter$ = "X" or currentCaracter$ = "x" Then \
			begin
				If counter1% > 1 And len(range1$) > 0 And len(range$) >= counter1% + 2 Then \
				Begin
					repeat% = int%(val(mid$(range$,counter1% + 1,2)))
					for counter2% = 1 to repeat% -1
						range1$ = range1$ + previousCaracter$
					next counter2%
					counter1% = counter1% + 2
				Endif
			endif \
			else \
			begin
				range1$ = range1$ + currentCaracter$
				previousCaracter$ = currentCaracter$
			endif
		next counter1%
	Endif \
	else \
		range1$ = range$
	!translateRange = Str$(Int%(Val(range1$)))
	range1$ = lRemove$(range1$,48)
	if range1$ = "" then range1$ = "0"
	translateRange = range1$
	!
end function
!
SUB EP.GET.KBDATA(UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, \
    UE.KEYB.DATA$) PUBLIC
  STRING UE.REQ.MESSAGE$, UE.KEYB.DATA$, UE.INI.RANGE$, UE.END.RANGE$,\
         UE.WORK$, UE.SAVDISP$ 
  Integer*1 ue14%
  !
  ue14% = TO.USEREXIT(14)
  TO.USEREXIT(14) = 0
  !
  UE.KEYB.DATA$ = ""
  UE.INI.RANGE$ = translateRange(UE.INI.RANGE$)
  UE.END.RANGE$ = translateRange(UE.END.RANGE$)
  UE.SAVDISP$ = TS.SAVDISP1$ + TS.SAVDISP2$
  UE.WORK$ = UE.REQ.MESSAGE$ + STRING$(40," ")
  TS.SAVDISP1$ = LEFT$(UE.WORK$,20) ! Split msg for display
  TS.SAVDISP2$ = MID$(UE.WORK$,21,20)
  TS.IO.MOTORKEY = 0                    ! Set for no input yet
  TS.IO.STATE = 11                      ! Enter/Data state
  WHILE TS.IO.MOTORKEY = 0              ! While still no input
    TS.LINETYPE = 9                     ! Redisplay prompt
    CALL TSDSEC01                       ! Prompt for Date
    UNLOCKDEV 32, TS.IO.STATE, PRIORITY ! Keyed expiration date
    WAIT  32; 100                       ! Wait for input
    WHILE NOT EVENT%                    ! While no input
      WAIT  32; 100                     ! Wait for input
    WEND
    TS11.OVRFLAG = 33                   ! Mark for exit 14
    CALL TSCSECRK                       ! Parse input / exit 14
    IF TS.IO.MOTORKEY = 73 THEN \       ! Clear key
    BEGIN 
       IF LEN(TS.IO.DATA$(1)) > 0 OR    \ Data Entered
          TS.TEMP1I2 > 1 THEN           \ More than 1 key
         TS.IO.MOTORKEY = 0             ! Try again
    ENDIF \
    ELSE \
    BEGIN                               ! Not clear key
      IF TS.IO.KEYS(10) = 80 THEN \     ! Keyed data
      BEGIN
        IF TS.IO.DATA$(10) = "" THEN TS.IO.DATA$(10) = "0"
        IF LEN(TS.IO.DATA$(10)) < LEN(UE.INI.RANGE$) OR \
           LEN(TS.IO.DATA$(10)) > LEN(UE.END.RANGE$) THEN \
        BEGIN 
          TS.LINETYPE = 8
          TS.LINEDATA = 6
          CALL TSCSEC08
          TS.IO.MOTORKEY = 0
        ENDIF \
        ELSE \
        BEGIN 
          UE.KEYB.DATA$ = TS.IO.DATA$(10) ! 
        ENDIF
      ENDIF                                ! Keyed data
    ENDIF                                  ! Not clear key
  WEND                                     ! While still no input
  UE.WORK$ = ""
  TS11.OVRFLAG = 0                         ! Clear exit marker
  TS.SAVDISP1$ = LEFT$(UE.SAVDISP$,20)     ! Restore prior display
  TS.SAVDISP2$ = MID$(UE.SAVDISP$,21,20)
  UE.SAVDISP$ = ""
  TS.IO.NEXTSTATE = 10                     ! Restore state = MAIN
  TS.IO.STATE = 10                         ! Restore state = MAIN
  UNLOCKDEV 32, TS.IO.STATE                ! Restore state = MAIN
  TO.USEREXIT(14) = ue14%
END SUB

SUB EP.GET.AUTH(UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, \
    UE.KEYB.DATA$) PUBLIC
  STRING UE.REQ.MESSAGE$, UE.KEYB.DATA$, UE.INI.RANGE$, UE.END.RANGE$,\
         UE.WORK$, UE.SAVDISP$ 
  UE.SAVDISP$ = TS.SAVDISP1$ + TS.SAVDISP2$
  UE.WORK$ = UE.REQ.MESSAGE$ + STRING$(40," ")
  TS.SAVDISP1$ = LEFT$(UE.WORK$,20) ! Split msg for display
  TS.SAVDISP2$ = MID$(UE.WORK$,21,20)
  TS.IO.MOTORKEY = 0                    ! Set for no input yet
  TS.IO.STATE = 6                      ! Enter/Data state
  WHILE TS.IO.MOTORKEY = 0              ! While still no input
    TS.LINETYPE = 9                     ! Redisplay prompt
    CALL TSDSEC01                       ! Prompt for Date
    UNLOCKDEV 32, TS.IO.STATE, PRIORITY ! Keyed expiration date
    WAIT  32; 100                       ! Wait for input
    WHILE NOT EVENT%                    ! While no input
      WAIT  32; 100                     ! Wait for input
    WEND
    TS11.OVRFLAG = 33                   ! Mark for exit 14
    CALL TSCSECRK                       ! Parse input / exit 14
    IF TS.IO.MOTORKEY = 73 THEN \       ! Clear key
    BEGIN 
       IF LEN(TS.IO.DATA$(0)) > 0 OR    \ Data Entered
          TS.TEMP1I2 > 1 THEN           \ More than 1 key
         TS.IO.MOTORKEY = 0             ! Try again
    ENDIF \
    ELSE \
    BEGIN                               ! Not clear key
      IF TS.IO.KEYS(10) = 80 THEN \     ! Keyed data
      BEGIN
        IF VAL(TS.IO.DATA$(10)) < VAL(UE.INI.RANGE$) OR \
           VAL(TS.IO.DATA$(10)) > VAL(UE.END.RANGE$) THEN \
        BEGIN 
          TS.LINETYPE = 8
          TS.LINEDATA = 6
          CALL TSCSEC08
          TS.IO.MOTORKEY = 0
        ENDIF \
        ELSE \
        BEGIN 
          UE.KEYB.DATA$ = TS.IO.DATA$(10) ! 
        ENDIF
      ENDIF                                ! Keyed data
    ENDIF                                  ! Not clear key
  WEND                                     ! While still no input
  UE.WORK$ = ""
  TS11.OVRFLAG = 0                         ! Clear exit marker
  TS.SAVDISP1$ = LEFT$(UE.SAVDISP$,20)     ! Restore prior display
  TS.SAVDISP2$ = MID$(UE.SAVDISP$,21,20)
  UE.SAVDISP$ = ""
  TS.IO.NEXTSTATE = 10                     ! Restore state = MAIN
  TS.IO.STATE = 10                         ! Restore state = MAIN
  UNLOCKDEV 32, TS.IO.STATE                ! Restore state = MAIN
END SUB
!
Sub resetFranqResMov
	Integer*2 tmpCounter%
	!
	warningSMA% = 0
	warningGeneral% = 0
	!
	franqResFlag% = 0
	!
	franqDetCount% = 0
	franqResTrxCount% = 0
	!
	! Soporta hasta 50 movimientos para cada una de estas franquicias
	! en una transaccion
	Dim franqResMov$(franqResCount%,franqResTrxLimit%)
	!
	For tmpCounter% = 0 To franqResCount%
		franqResMov$(tmpCounter%,0) = "0"	! En la posición 0 se almacenará el número de movimientos de cada franquicia
	Next tmpCounter%
End Sub
!
Sub redefineArray3
	String tmpData$(1)
	Integer*2 newLimit%,counter1%
	!
	newLimit% = 2 * franqResLimit%
	Dim tmpData$(franqResLimit%)
	!
	For counter1% = 1 To franqResLimit%
		tmpData$(counter1%) = franqResDes$(counter1%)
	Next counter1%
	!
	Dim franqResDes$(newLimit%)
	For counter1% = 1 To franqResLimit%
		franqResDes$(counter1%) = tmpData$(counter1%)
	Next counter1%
	franqResLimit% = newLimit%
End Sub
!
Sub readTefPar3(fileSession%)
	Integer*2 fileSession%
	String franq$,dummy$,param$
	!
	CALL EP.DISPLAY.A.MESSAGE("Leyendo franquicias")
	!
	franqResLimit% = 10	! Dimensionamiento inicial del arreglo
	franqResCount% = 0
	Dim franqResDes$(franqResLimit%)
	!
	TS.ER.RETURN = -1
	OPEN "r::adx_idt1:tefpar3.dat" AS fileSession% NOWRITE NODEL
	If Not TS.ER.RETURN Then \
		Exit Sub
	If End #fileSession% Then End.readTefpar3
	!
	While -1
		Read #fileSession% ; dummy$,dummy$,dummy$,dummy$,franq$,\
			dummy$,param$,dummy$,dummy$,dummy$,dummy$,dummy$,dummy$,dummy$,dummy$,dummy$,dummy$
		call ep.display.a.message("Franquicia " + franq$)
		param$ = ltrim$(param$)
		If Left$(param$,1) = "1" Then \
		Begin
			If franqResCount% >= franqResLimit% Then Call redefineArray3
			franqResCount% = franqResCount% + 1
			franqResDes$(franqResCount%) = franq$
		Endif
	Wend
	!
	End.readTefPar3:
		Close fileSession%
	!
	Call resetFranqResMov
	CALL EP.DISPLAY.A.MESSAGE("Franquicias cargadasOK")
End Sub
!
Function getFranqResIndex(franquicia$)
	Integer*2 getFranqResIndex, tmpCounter%
	String franquicia$
	Integer*1 franqFound%
	!
	franqFound% = 0
	tmpCounter% = 1
	While franqFound% = 0 And tmpCounter% <= franqResCount%
		If franquicia$ = franqResDes$(tmpCounter%) Then \
			franqFound% = -1 \
		Else \
			tmpCounter% = tmpCounter% + 1
	Wend
	If franqFound% = -1 Then \
		getFranqResIndex = tmpCounter% \
	Else \
		getFranqResIndex = 0
End Function
!
Function addFranqResRecord(pFranquicia$, pAccount$, pValue%, pAuth$)
	Integer*1 addFranqResRecord
	String pFranquicia$, pAccount$, pAuth$, strValue$
	Integer*4 pValue%
	Integer*2 index%, fIndex%
	!
	index% = getFranqResIndex(pFranquicia$)
	If index% > 0 And Val(franqResMov$(index%,0)) < franqResTrxLimit% Then \
	Begin
		Call FORMAT.AMOUNT(pValue%)
		strValue$ = TS.TEMP1$
		fIndex% = Val(franqResMov$(index%,0)) + 1
		franqResMov$(index%,fIndex%) = \
			Left$(pAccount$ + String$(16," "),16) + " " + \
			Right$(String$(10," ") + strValue$,10) + " " + \
			pAuth$
		franqResMov$(index%,0) = Str$(fIndex%)
		fIndex% = Val(franqResMov$(0,0)) + 1
		franqResMov$(0,0) = Str$(fIndex%)
		addFranqResRecord = -1
		franqResTrxCount% = franqResTrxCount% + 1
	Endif Else \
	Begin
		addFranqResRecord = 0
		franqDetCount% = franqDetCount% + 1
	Endif
End Function
!
Sub printFranqRes
	Integer*2 counter1%,counter2%,tmpCount%
	!
	If Val(franqResMov$(0,0)) > 0 Then \
	Begin
		! Apaga user exits de impresion
		TO.USEREXIT(20) = 0
		TO.USEREXIT(60) = 0
		!
		! Guarda estado actual de impresion
		CALL EP.SAVE.PRINT
		!
		! Imprime Encabezado
		TO.HEADERLINE1$  = "         OTROS PAGOS EN LINEA         "
		TO.HEADERLINE2$  = STRING$(38," ")
		TS.LINETYPE = 18 
		TS.LINEDATA = 99
		CALL TSPREC01
		CALL EP.LINE.PRINT("     Cuenta        Valor    Autori",6100H)
		CALL EP.LINE.PRINT("---------------- ---------- ------",6100H)
		!
		! Recorre e imprime las franquicias que tengan
		! movimiento
		For counter1% = 1 To franqResCount%
			tmpCount% = Val(franqResMov$(counter1%,0))
			If tmpCount% > 0 Then \
			Begin
				Call EP.LINE.PRINT("Franquicia: " + franqResDes$(counter1%),6100H)
				For counter2% = 1 To tmpCount%
					Call EP.LINE.PRINT(franqResMov$(counter1%,counter2%),6100H)
				Next counter2%
				Call EP.LINE.PRINT(" ",6100H)
			Endif
		Next counter1%
		!
		! Corte de papel
		TO.HEADERLINE1$ = EP.SAV.HD1$
		TO.HEADERLINE2$ = EP.SAV.HD2$
		TS.LINETYPE = 18 
		TS.LINEDATA = 0
		CALL TSPREC01		
		!
		! Restaura estado previo de impresion		
		CALL EP.RESTORE.PRINT
		!
		! Enciende user exits de impresion
		TO.USEREXIT(20) = -1
		TO.USEREXIT(60) = -1
		!
		! Reinicia arreglos
		Call resetFranqResMov
	Endif
End Sub
!
!
!  Print vouchers
!
SUB EP.STORE.EFTLINE(UE.PRINT.LINE$) PUBLIC
  STRING UE.PRINT.LINE$
!
  If EP.POINTER2% < 100 Then \
  Begin
	  EP.POINTER2% = EP.POINTER2% + 1
	  IF LEN(UE.PRINT.LINE$) <= 38 THEN \
	    EP.EFT.LINE$(EP.POINTER2%) = UE.PRINT.LINE$ \
	  ELSE \
	    EP.EFT.LINE$(EP.POINTER2%) = LEFT$(UE.PRINT.LINE$,38)
  EndIf Else \
  	Call EP.DISPLAY.AN.ERROR("Se ha excedido el # de lineas de tirilla")
!
END SUB
!
Sub EP.REMOVE.VOUCHER(pPointer1%, pPointer2%) Public
	Integer*2 pPointer1%, pPointer2%, tmpCounter%, tmpDiff%
	!
	Call printDebug("EP.REMOVE.VOUCHER("+Str$(pPointer1%)+", "+Str$(pPointer2%)+") EP.POINTER1%="+str$(EP.POINTER1%))
	On Error Goto removeVoucher.err
	tmpDiff% = pPointer2% - pPointer1%
	If tmpDiff% > 0 Then Begin
		For tmpCounter% = pPointer1% + 1 To EP.POINTER1% - tmpDiff%
			EP.VOUCHER$( tmpCounter% ) = EP.VOUCHER$( tmpCounter% + tmpDiff% )
		Next tmpCounter%
		For tmpCounter% = EP.POINTER1% - tmpDiff% + 1 To EP.POINTER1%
			EP.VOUCHER$( tmpCounter% ) = ""
		Next tmpCounter%
		EP.POINTER1% = EP.POINTER1% - tmpDiff%
	Endif
	Goto removeVoucher.end
	!
	removeVoucher.err:
		Resume removeVoucher.end
	!
	removeVoucher.end:
End Sub
!
SUB EP.STORE.VOUCHER(UE.PRINT.LINE$,UE.PRT.CUT$) PUBLIC 
  STRING UE.PRINT.LINE$, UE.PRT.CUT$
  ! Si la franquicia está registrada dentro de la lista
  ! de franquicias que soportan resumen de movimientos,
  ! se ignora la orden de almacenamiento del voucher,
  ! ya que éste no aplica
  If franqResFlag% = 0 Then \
  Begin
	  If EP.POINTER1% < 300 Then \
	  Begin
		  EP.POINTER1% = EP.POINTER1% + 1
		  IF LEN(UE.PRINT.LINE$) <= 38 THEN \
		    EP.VOUCHER$(EP.POINTER1%) = UE.PRINT.LINE$ \
		  ELSE \
		    EP.VOUCHER$(EP.POINTER1%) = LEFT$(UE.PRINT.LINE$,38)
		  EP.CUT.VOUCHER$(EP.POINTER1%) = UE.PRT.CUT$
	  EndIf Else \
	  	Call EP.DISPLAY.AN.ERROR("Se ha excedido el # de lineas de voucher")
  Endif 
END SUB
!
!
SUB EPAY.VOUCHER PUBLIC
  INTEGER*2 EP.COUNT%
  IF EP.REPRINT% THEN \
  BEGIN
    TO.TRAILERLINE1$ = LEFT$(EP.GUIDE.MESSAGE$(20),38)
    EP.REPRINT% = 0     
  ENDIF \
  ELSE  \
  IF EP.INTERCHG% THEN \
  BEGIN
    TO.TRAILERLINE1$ = LEFT$(EP.GUIDE.MESSAGE$(21),38)
    EP.INTERCHG% = 0     
  ENDIF \
  ELSE  \
  BEGIN 
    TO.TRAILERLINE1$ = LEFT$(EP.GUIDE.MESSAGE$(19),38)
  ENDIF
  TO.TRAILERLINE2$ = STRING$(38,"_")
  IF EP.POINTER1% >= 2 THEN \
  BEGIN
    CALL EP.SAVE.PRINT
    TO.USEREXIT(20) = 0
    FOR EP.COUNT% = 2 TO EP.POINTER1%
      IF EP.CUT.VOUCHER$(EP.COUNT%) = "1" THEN \
      BEGIN 
        CALL EP.LINE.PRINT(EP.SAVE.GLINE$,6100H) 
        TS.LINETYPE = 18 
        TS.LINEDATA = 0
        CALL TSPREC01
      ENDIF  \
      ELSE \
        CALL EP.LINE.PRINT(EP.VOUCHER$(EP.COUNT%),6100H) 
    NEXT EP.COUNT% 
  ENDIF
  CALL EP.LINE.PRINT(EP.SAVE.GLINE$,6100H) 
  TO.HEADERLINE1$ = EP.SAV.HD1$
  TO.HEADERLINE2$ = EP.SAV.HD2$
  TS.LINETYPE = 18 
  TS.LINEDATA = 0
  CALL TSPREC01
  TO.USEREXIT(20) = -1
  CALL EP.RESTORE.PRINT
  TO.TRAILERLINE1$ = EP.SAV.TR1$
  TO.TRAILERLINE2$ = EP.SAV.TR2$
END SUB

SUB EPAY.RESET.VOUCHER PUBLIC
  DIM EP.VOUCHER$(300)
  DIM EP.CUT.VOUCHER$(300)
  EP.POINTER1%  = 0
  EP.LINEA.IMP% = 1
  EP.POINTER2%  = 0
  EP.REPRINT% = 0
  Call resetFranqResMov
END SUB
!
!
SUB PRINT.EFT.HEADER PUBLIC
  INTEGER*2 EP.COUNT%
!
  IF EP.POINTER1% >= 2 THEN \
  BEGIN
    TO.USEREXIT(20) = 0
    TO.USEREXIT(60) = 0
    CALL EP.SAVE.PRINT
    TS.LINETYPE = 29
    TS.SAVPRT$ = STRING$(38," ")
    TS.SAVPRT.OPT = 4100H
    CALL TSPREC01
!    TO.HEADERLINE1$  = EP.ESC$+CHR$(58)+LEFT$(EP.EFT.LINE$(1),30)
    TO.HEADERLINE1$  = LEFT$(EP.EFT.LINE$(1),38)
    TO.HEADERLINE2$  = STRING$(38," ")
    TS.LINETYPE = 18 
    TS.LINEDATA = 99
    CALL TSPREC01
    CALL EP.RESTORE.PRINT
    TO.USEREXIT(20) = -1
    TO.USEREXIT(60) = -1
  ENDIF
END SUB
!
SUB PRINT.EFT.HEADER.CUT PUBLIC
  INTEGER*2 EP.COUNT%
  TO.USEREXIT(20) = 0
  TO.USEREXIT(60) = 0
  CALL EP.SAVE.PRINT
!  TO.HEADERLINE1$  = EP.ESC$+CHR$(58)+LEFT$(EP.EFT.LINE$(1),30)
  TO.HEADERLINE1$  = LEFT$(EP.EFT.LINE$(1),38)
  TO.HEADERLINE2$  = STRING$(38," ")
  TS.LINETYPE = 18 
  TS.LINEDATA = 0
  CALL TSPREC01
  CALL EP.RESTORE.PRINT
  TO.USEREXIT(20) = -1
  TO.USEREXIT(60) = -1
END SUB
!
!
SUB PRINT.IVA.DETAIL(UE.TAX.EPAY$) PUBLIC
  INTEGER*4 UE.INT4%
  INTEGER*1 UE.INT1%
  STRING    UE.TAX.EPAY$
! 
  IF EP.FUNCTION$ = "11" OR \
     EP.FUNCTION$ = "12" OR \
     EP.FUNCTION$ = "14" OR \
     EP.FUNCTION$ = "19" THEN \
  BEGIN
    UE.INT4% = VAL(UE.TAX.EPAY$)
    UE.INT1% = FORMAT.AMOUNT(UE.INT4%)
    CALL EP.LINE.PRINT(LEFT$(EP.GUIDE.MESSAGE$(08), 15) + \
      RIGHT$(STRING$(12," ") + TS.TEMP1$, 12), 6200H)
  ENDIF
END SUB

SUB PRINT.EFT.DETAIL(UE.VALOR.EPAY$, UE.TAX.EPAY$) PUBLIC
  INTEGER*4 UE.INT4%
  INTEGER*2 UE.COUNT%
  INTEGER*1 UE.INT1%
  STRING    UE.VALOR.EPAY$, UE.TAX.EPAY$
  
  IF TS.PROCEDURE < 1 THEN \
  BEGIN 
    FOR UE.COUNT% = 1 TO EP.POINTER2%
      CALL EP.LINE.PRINT(EP.EFT.LINE$(UE.COUNT%),6100H) 
    NEXT UE.COUNT% 
    CALL EP.LINE.PRINT(STRING$(38," "),6100H) 
    EP.POINTER2%   = 0
    EP.EFT.DETAIL% = 0
  ENDIF \
  ELSE \
  BEGIN 
    EP.M% = 0 
    WHILE EP.LINEA.IMP% <= EP.POINTER2% AND \
          EP.M% = 0 
      CALL EP.LINE.PRINT(EP.EFT.LINE$(EP.LINEA.IMP%),6100H)
      EP.LINEA.IMP% = EP.LINEA.IMP% + 1 
      EP.M% = MATCH(EP.EFT.LINE$(1), EP.EFT.LINE$(EP.LINEA.IMP%),1)
    WEND
    CALL EP.LINE.PRINT(STRING$(38," "),6100H) 
    IF EP.LINEA.IMP% > EP.POINTER2% THEN EP.EFT.DETAIL% = 0
  ENDIF
END SUB
!
!
SUB INTERCHG.VOUCHER PUBLIC
!
  EP.INTERCHG%  = -1
!
  CALL PRINT.EFT.HEADER.CUT
  CALL EPAY.VOUCHER
!
END SUB
!
!
FUNCTION EP.CHECK.DIGIT(UE.ACCOUNT.NBR$) PUBLIC 
  STRING UE.ACCOUNT.NBR$, EP.CHECK.DIGIT, UE.ADD.QTY$
  INTEGER*2 UE.POS%, UE.LEN%, UE.TOTAL%, UE.10%, UE.ADD.QTY%
  UE.LEN% = LEN(UE.ACCOUNT.NBR$)
  UE.TOTAL% = 0
  IF UE.LEN% >= 2 THEN \
  BEGIN 
    FOR UE.POS% = 1 TO UE.LEN% STEP 2
      UE.ADD.QTY% = VAL(MID$(UE.ACCOUNT.NBR$,UE.POS%,1)) * 2
      IF UE.ADD.QTY% > 9 THEN \
      BEGIN
        UE.ADD.QTY$ = STR$(UE.ADD.QTY%)
        UE.ADD.QTY% = VAL(MID$(UE.ADD.QTY$,1,1)) + VAL(MID$(UE.ADD.QTY$,2,1))
      ENDIF 
      UE.TOTAL% = UE.TOTAL% + UE.ADD.QTY% 
    NEXT UE.POS%
    FOR UE.POS% = 2 TO UE.LEN% STEP 2
      UE.TOTAL% = UE.TOTAL% + VAL(MID$(UE.ACCOUNT.NBR$,UE.POS%,1)) 
    NEXT UE.POS%
    IF MOD(UE.TOTAL%,10) <> 0 THEN \
      EP.CHECK.DIGIT = STR$(10 - MOD(UE.TOTAL%,10)) \
    ELSE \
      EP.CHECK.DIGIT = "0"
  ENDIF
END FUNCTION
!
Function isApplMgrRunning Public
	Integer*1 isApplMgrRunning
	!
	isApplMgrRunning = EP.applMgr.running%
End Function
!
SUB EPAY.INITIALIZATION PUBLIC
  STRING UE.VAL1$, UE.VAL2$, EP.LAST$
  INTEGER*2 UE.VAL2%, EP.I%, EP.PARM%, UE.Q%
!
  DIM EP.TRX.REV.PEND$(99,1)
!
  !
  EP.applMgr.running% = 0
  !
  EP.IOPARM%  = 95           !! session number for parameters
  EP.SW.VERSION$ = "21"
  CALL EP.DISPLAY.A.MESSAGE("API APPL MNGR V:"+LEFT$(EP.SW.VERSION$,1)+"."+\
    RIGHT$(EP.SW.VERSION$,1))
  WAIT ; 3000
  EP.SAV.HD1$ = TO.HEADERLINE1$
  EP.SAV.HD2$ = TO.HEADERLINE2$
  EP.SAV.TR1$ = TO.TRAILERLINE1$
  EP.SAV.TR2$ = TO.TRAILERLINE2$
  EP.STX$ = CHR$(2)
  EP.ETX$ = CHR$(3)
  EP.EOT$ = CHR$(4)
  EP.ENQ$ = CHR$(5)
  EP.ACK$ = CHR$(6)
  EP.LF$  = CHR$(10)
  EP.CR$  = CHR$(13)
  EP.NACK$= CHR$(21)
  EP.ETB$ = CHR$(23)
  EP.ESC$ = CHR$(27)
  EP.SIZE.ISO%   = 120
  EP.SIZE.APPL%  = 50
  EP.SIZE.COMM%  = 50
  EP.SIZE.GUIDE% = 50
  EP.SIZE.TV%    = 50
  EP.MAX.TRX%    = 20
  EP.STORE$      = RIGHT$(STRING$(4,"0") + TS.STORE$  ,4)
  EP.NO.TRANSMITION$ = "Xx"
!
! nombre Logico: APIPARM = ADX_IDT1:APIPARMR.DAT
!
!---------------------------------------------------------
  EP.LINEA% = 0
  DIM EP.VOUCHER$(300)
  DIM EP.CUT.VOUCHER$(300)
  DIM EP.EFT.LINE$(100)
  DIM EP.TRX.APPL$(EP.MAX.TRX%)
  DIM EP.TRX.FUNCTION$(EP.MAX.TRX%)
  DIM EP.TRX.TRANSNUM$(EP.MAX.TRX%)
  DIM EP.TRX.RESP%(EP.MAX.TRX%)
  DIM EP.TRX.APPROV%(EP.MAX.TRX%)
  DIM EP.TRX.VOUCHER%(EP.MAX.TRX%)
  DIM EP.TRX.TOHOST1%(EP.MAX.TRX%) 
  DIM EP.TRX.TOHOST2%(EP.MAX.TRX%)
  DIM EP.TRX.ANUL%(EP.MAX.TRX%)
  DIM EP.TRX.REVERSO%(EP.MAX.TRX%)
  DIM EP.TRX.PEND%(EP.MAX.TRX%)   
  DIM EP.TRX.FECHA$(EP.MAX.TRX%)   
  DIM EP.PROMO.ID$(10)
  DIM EP.PROMO.STATUS$(10)
  DIM EP.PROMO.POINTS$(10) 
  DIM EP.PROMO.REDEEM$(10)
  DIM EP.PROMO.AMTPUR$(10)
  DIM EP.PROMO.QTYPUR$(10)     
!
  TS.ER.RETURN = -1
  TS.ERRN = 0
  TS.TS11WERR$ = ""
!
  OPEN "MSR:" AS 41
!
    DIM EP.ISO.CODE$(EP.SIZE.ISO%)	      ! Descripcion de los mensajes iso 8583
    DIM EP.ISO.RESPONSE$(EP.SIZE.ISO%)          !
    DIM EP.APPL.CODE$(EP.SIZE.APPL%)	      ! Descripcion de los mensajes APPL MNGR
    DIM EP.APPL.RESPONSE$(EP.SIZE.APPL%)        !
    DIM EP.COMM.CODE$(EP.SIZE.COMM%)	      ! Descripcion de los mensajes COMUNICACIONES
    DIM EP.COMM.RESPONSE$(EP.SIZE.COMM%)        !
    DIM EP.GUIDE.CODE$(EP.SIZE.GUIDE%)	      ! Descripcion de los mensajes COMUNICACIONES
    DIM EP.GUIDE.MESSAGE$(EP.SIZE.GUIDE%)       !
    DIM EP.TV.CODE$(EP.SIZE.TV%)	      ! Descripcion de los grupo de tipo variedad especial
    DIM EP.TV.GROUP$(EP.SIZE.TV%)       !
    TS.ER.RETURN = -1
    OPEN "R::ADX_IDT1:APIPARM.DAT" AS EP.IOPARM% NOWRITE NODEL
    IF NOT TS.ER.RETURN  THEN \
    BEGIN
      CALL EP.DISPLAY.AN.ERROR("Imposible abrir APIPARM")
      EP.DEVICE.STATE% = 99
      GOTO NOMORE.MESSAGES
    ENDIF
    IF END # EP.IOPARM% THEN  CLOSE.MESSAGES
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.EFT.ACTIVO% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.POS.PTOS$ = UE.VAL2$
    EP.POS% = MATCH(STRING$(4,"9")+":",EP.POS.PTOS$,1)
    IF EP.POS% = 0 THEN  \
      EP.POS% = MATCH(RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4)+":",EP.POS.PTOS$,1)
    IF EP.POS% <> 0 THEN  \
      EP.PTOS.ACTIVO% = -1 \
    ELSE \
      EP.PTOS.ACTIVO% = 0
!
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.POS.RED$ = UE.VAL2$
    EP.POS% = MATCH(STRING$(4,"9")+":",EP.POS.RED$,1)
    IF EP.POS% = 0 THEN  \
      EP.POS% = MATCH(RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4)+":",EP.POS.RED$,1)
    IF EP.POS% <> 0 THEN  \
      EP.RED.ACTIVO% = -1 \
    ELSE \
      EP.RED.ACTIVO% = 0
!
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.POS.SALDO$ = UE.VAL2$
    EP.POS% = MATCH(STRING$(4,"9")+":",EP.POS.SALDO$,1)
    IF EP.POS% = 0 THEN  \
      EP.POS% = MATCH(RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4)+":",EP.POS.SALDO$,1)
    IF EP.POS% <> 0 THEN  \
      EP.SALDO.ACTIVO% = -1 \
    ELSE \
      EP.SALDO.ACTIVO% = 0
!
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.POS.CASHB$ = UE.VAL2$
    EP.POS% = MATCH(STRING$(4,"9")+":",EP.POS.CASHB$,1)
    IF EP.POS% = 0 THEN \
    BEGIN
      EP.POS% = MATCH(RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4)+":",EP.POS.CASHB$,1)
      IF EP.POS% = 0 THEN \
      BEGIN
        EP.CB.MAXIMP% = 0
        EP.CB.PORC%   = 0
      ENDIF
    ENDIF
!   
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.EFT.CLAVE$ = UE.VAL2$
    EP.LEN.CLAVE% = LEN(EP.EFT.CLAVE$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.CHAIN$ = RIGHT$(STRING$(4,"0") + UE.VAL2$,4)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    !
    If Right$(UE.VAL1$, 13) = "EP.DYKEY.ACT%" Then Begin
    	EP.DYKEY.ACT% = Int%(Val(UE.VAL2$))
    	Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
    Endif Else Begin
    	EP.DYKEY.ACT% = 0
    Endif    
    !
    If Right$(UE.VAL1$, 13) = "EP.DYKEY.PRC%" Then Begin
    	EP.DYKEY.PRC% = Int%(Val(UE.VAL2$))
    	Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
    Endif Else Begin
    	EP.DYKEY.PRC% = 0
    Endif
    !
    If Right$(UE.VAL1$, 13) = "EP.DYKEY.KEY$" Then Begin
    	EP.DYKEY.KEY$ = ":" + UE.VAL2$ + ":"
    	Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
    Endif Else Begin
    	EP.DYKEY.KEY$ = ""
    Endif
    !
    If Right$(UE.VAL1$, 14) = "EP.DYKEY.MSG1$" Then Begin
    	EP.DYKEY.MSG1$ = UE.VAL2$
    	Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
    Endif Else Begin
    	EP.DYKEY.MSG1$ = "DYNAMIC KEY ACTIVE"
    Endif
    !
    If Right$(UE.VAL1$, 14) = "EP.DYKEY.MSG2$" Then Begin
    	EP.DYKEY.MSG2$ = UE.VAL2$
    	Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
    Endif Else Begin
    	EP.DYKEY.MSG2$ = "DYNAMIC KEY INACTIVE"
    Endif
    !
    EP.TV.CHEQ$ = RIGHT$(STRING$(2,"0") + UE.VAL2$,2)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.TV.TJDB$ = RIGHT$(STRING$(2,"0") + UE.VAL2$,2)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.TV.TJCR$ = RIGHT$(STRING$(2,"0") + UE.VAL2$,2)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.IVADEV$ = UE.VAL2$
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.CB.PORC% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.CB.MAXIMP% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    IF LEFT$(UE.VAL1$,17) = "Max VALOR RECAUDO" THEN \
    BEGIN
    	EP.RC.MAXIMP% = VAL(UE.VAL2$)
    	READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    ENDIF ELSE \
    	EP.RC.MAXIMP% = 999999
    EP.TEF.VOU% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.DET.FIN% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    If Left$(UE.VAL1$,28) = "Limite de movtos TEF por trx" Then \
    Begin
    	franqDetLimit% = Val(UE.VAL2$)
    	READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Endif Else \
    	franqDetLimit% = 10
    If Left$(UE.VAL1$,30) = "Limite de movtos BONOS por trx" Then \
    Begin
    	franqResTrxLimit% = Val(UE.VAL2$)
    	READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EndIf Else \
    	franqResTrxLimit% = 30
    EP.SPIPE% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.RPIPE% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.EIAP% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.NO.TRACE% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.COM% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.SPEED% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.PARITY$ = UE.VAL2$
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.DATA.BITS% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.STOP.BITS% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.MAX.TRIES% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.CHARS.WAIT% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.SHORT.WAIT% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.LONG.WAIT% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.MESSAGE.WAIT% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.MESSAGE.TIMEOUT% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.APPROVAL.TIMEOUT% = VAL(UE.VAL2$)
    !
    !EP.PARM.LOADED%  = -1
    !
  !
    EP.PARM% = 0
    WHILE EP.PARM% <= 6000
      READ # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
      IF UE.VAL2$ = "1000" OR \
         UE.VAL2$ = "2000" OR \
         UE.VAL2$ = "3000" OR \
         UE.VAL2$ = "4000" OR \
         UE.VAL2$ = "5000" OR \
         UE.VAL2$ = "6000" THEN \
      BEGIN
        EP.PARM% = VAL(UE.VAL2$)
        IF EP.PARM% = 2000 THEN  \
        BEGIN 
          EP.ISO.CODE$(EP.SIZE.ISO%) = EP.ISO.CODE$(EP.I% - 1)
          EP.ISO.RESPONSE$(EP.SIZE.ISO%) = EP.ISO.RESPONSE$(EP.I% - 1)
        ENDIF
        IF EP.PARM% = 3000 THEN  \
        BEGIN 
          EP.APPL.CODE$(EP.SIZE.APPL%) = EP.APPL.CODE$(EP.I% - 1)
          EP.APPL.RESPONSE$(EP.SIZE.APPL%) = EP.APPL.RESPONSE$(EP.I% - 1)
        ENDIF
        IF EP.PARM% = 4000 THEN  \
        BEGIN 
          EP.COMM.CODE$(EP.SIZE.COMM%) = EP.COMM.CODE$(EP.I% - 1)
          EP.COMM.RESPONSE$(EP.SIZE.COMM%) = EP.COMM.RESPONSE$(EP.I% - 1)
        ENDIF
        IF EP.PARM% = 5000 THEN  \
        BEGIN 
          EP.GUIDE.CODE$(EP.SIZE.GUIDE%) = EP.GUIDE.CODE$(EP.I% - 1)
          EP.GUIDE.MESSAGE$(EP.SIZE.GUIDE%) = EP.GUIDE.MESSAGE$(EP.I% - 1)
        ENDIF
        IF EP.PARM% = 6000 THEN  \
        BEGIN 
          EP.TV.CODE$(EP.SIZE.TV%) = EP.TV.CODE$(EP.I% - 1)
          EP.TV.GROUP$(EP.SIZE.TV%) = EP.TV.GROUP$(EP.I% - 1)
          EP.PARM% = 7000
        ENDIF
        EP.I% = 1
      ENDIF \
      ELSE \
      BEGIN
        IF EP.PARM% = 1000 THEN  \
        BEGIN
          EP.ISO.RESPONSE$(EP.I%) = UE.VAL1$
          EP.ISO.CODE$(EP.I%) = UE.VAL2$
        ENDIF
        IF EP.PARM% = 2000 THEN  \
        BEGIN
          EP.APPL.RESPONSE$(EP.I%) = UE.VAL1$
          EP.APPL.CODE$(EP.I%) = UE.VAL2$
        ENDIF
        IF EP.PARM% = 3000 THEN  \
        BEGIN
          EP.COMM.RESPONSE$(EP.I%) = UE.VAL1$
          EP.COMM.CODE$(EP.I%) = UE.VAL2$
        ENDIF
        IF EP.PARM% = 4000 THEN  \
        BEGIN
          EP.GUIDE.MESSAGE$(EP.I%) = UE.VAL1$
          EP.GUIDE.CODE$(EP.I%) = UE.VAL2$
        ENDIF
        IF EP.PARM% = 5000 THEN  \
        BEGIN
          UE.Q% = MATCH(" ",UE.VAL1$,1)
          IF UE.Q% >= 2 THEN \
            UE.VAL1$ = LEFT$(UE.VAL1$, UE.Q%-1)
          EP.TV.GROUP$(EP.I%) = UE.VAL1$
          EP.TV.CODE$(EP.I%)  = UE.VAL2$
        ENDIF
        EP.I% = EP.I% + 1
      ENDIF        !
    WEND
CLOSE.MESSAGES:    
    CLOSE EP.IOPARM%
    !
NOMORE.MESSAGES:

!
  IF NOT EP.PARM.LOADED% THEN \
  BEGIN 
  EP.PARM.LOADED%  = -1
	CALL EP.DISPLAY.A.MESSAGE("Inicializando Kernel")
    EP.METHOD$      = "runOther"
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.REQUEST$     ="C$"
    EP.RUNMETH$     ="com.appl.ApplKernel.initializeKernel"
    EP.EXCEPTION$=""
    EP.TERMINAL$= RIGHT$(STRING$(4,"0")+EP.CHAIN$,4) + \
                  RIGHT$(STRING$(4,"0")+TS.STORE$,4) + \
                  RIGHT$(STRING$(6,"0")+TS.TERMINAL$,6)

    CALL Javacall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    CALL Javacall.AddParameter.String(EP.REQUEST$,EP.TERMINAL$)
    CALL Javacall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
    EP.EXCEPTION$=""
    CALL Javacall.InvokeMethod.ReturnString(EP.REQUEST$,EP.RETURNVALUE$,EP.EXCEPTION$)


    IF LEN(EP.EXCEPTION$) > 0 THEN \
    BEGIN
      CALL EP.DISPLAY.AN.ERROR("Err en:"+EP.METHOD$)
      EXIT SUB
    ENDIF
!----------------------------------------------------------------------------------    
!   ojo  restaurar para version pre SCAP en Olimpica
!
!    CALL EP.DISPLAY.A.MESSAGE(EP.RETURNVALUE$)
!   
	EP.RUNMETH$     ="com.appl.ApplKernel.getIAN"
	CALL Javacall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
	CALL Javacall.AddParameter.String(EP.REQUEST$,EP.TERMINAL$)
	CALL Javacall.AddParameter.String(EP.REQUEST$,EP.RUNMETH$)
	EP.EXCEPTION$=""
	EP.RETURNVALUE$ = ""
	WHILE EP.RETURNVALUE$ <> "OK" AND EP.EXCEPTION$=""
		CALL Javacall.InvokeMethod.ReturnString(EP.REQUEST$,EP.RETURNVALUE$,EP.EXCEPTION$)
		IF EP.EXCEPTION$ <> "" THEN CALL EP.DISPLAY.AN.ERROR(EP.EXCEPTION$) ELSE \
		BEGIN
		CALL EP.DISPLAY.A.MESSAGE(EP.RETURNVALUE$)
		WAIT ; 500
		ENDIF
	WEND
!----------------------------------------------------------------------------------    
    
    !
  ENDIF
!
  Call readTefPar3(EP.IOPARM%)
  !
  IF EP.EFT.ACTIVO% THEN \
  BEGIN  
!---------------------------------------------------------
!  CALL EP.DISPLAY.A.MESSAGE("userht="+ UNPACK$(TE.TR.USERHT$) + "|")
!---------------------------------------------------------
!
    EP.EPAY.TRANSNUM$ = EP.READ.TRANSNUM
    IF VAL(EP.EPAY.TRANSNUM$) = 0 THEN \
    BEGIN 
      TS.ER.RETURN = -1
      OPEN "R::EFTTRX" KEYED RECL 158 AS EP.IOPARM% NOWRITE NODEL
      IF NOT TS.ER.RETURN  THEN \   ! si no abre
      BEGIN 
        EP.EPAY.TRANSNUM$ = STRING$(6,"0")
!---------------------------------------------------------
        CALL EP.DISPLAY.AN.ERROR("no abre EFTTRX")
!---------------------------------------------------------
        CALL EP.WRITE.TRANSNUM(EP.EPAY.TRANSNUM$)
      ENDIF \
      ELSE \
      BEGIN 
        EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) +   \
           STRING$(12,"9")
        TS.ER.RETURN = -1 
        READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%         \
          KEY EP.KEY$ ;                                     \
          EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,  \
          EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$,         \
          EP.A$, EP.A$
        IF NOT TS.ER.RETURN THEN \  ! si no existe
        BEGIN 
          EP.EPAY.TRANSNUM$ = STRING$(6,"0")
          CALL EP.WRITE.TRANSNUM(EP.EPAY.TRANSNUM$)
!---------------------------------------------------------
          CALL EP.DISPLAY.AN.ERROR("REINICIO CONSECUTIVO"+ EP.EPAY.TRANSNUM$ + ":")
!---------------------------------------------------------
        ENDIF \
        ELSE \
        BEGIN 
          EP.EPAY.TRANSNUM$ = LEFT$(EP.DE2.DATA$,6)
          CALL EP.WRITE.TRANSNUM(EP.EPAY.TRANSNUM$)
!---------------------------------------------------------
          CALL EP.DISPLAY.AN.ERROR("Se carga="+ EP.EPAY.TRANSNUM$ + ":")
!---------------------------------------------------------
        ENDIF
        CLOSE EP.IOPARM%
      ENDIF
    ENDIF
!
  ENDIF  
!

END SUB
!
SUB EP.SEND.TO.THREADER PUBLIC
!
!   call Java appl manager routine
!
    EP.CLASS.GRAL$  = "com.appl.ApplKernel"
    EP.METHOD$      = "threader"
    EP.REQUEST$     = "C$"
    EP.EXCEPTION$   = ""
    EP.RETURNVALUE$ = ""
    EP.AMJ.MESSAGE$ = ""
!
    CALL Javacall.Initialize.Request(EP.CLASS.GRAL$,EP.METHOD$,EP.REQUEST$)
    CALL Javacall.AddParameter.String(EP.REQUEST$,EP.MESSAGE$)
    EP.EXCEPTION$=""
    CALL Javacall.InvokeMethod.ReturnString(EP.REQUEST$,EP.RETURNVALUE$,\
        EP.EXCEPTION$)
!
    IF LEN(EP.EXCEPTION$) > 0 THEN \
    BEGIN
!      CALL EP.LINE.PRINT("Error en el envio",6100H)
!      CALL EP.LINE.PRINT(LEFT$(EP.EXCEPTION$,38),6100H)
      EP.ERRNCODE% = 1
      EP.GOOD.END% = 0
    ENDIF \
    ELSE \
    BEGIN
      EP.ERRNCODE% = 0
      EP.GOOD.END% = -1
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
    ENDIF
END SUB
!
SUB EP.PARSE.THREADER.RESPONSE PUBLIC
!
  ! Campos opcionales para identificar una operación
  EP.OPT.CONVENIO$ = ""
  EP.OPT.BOLSILLO$ = ""
  !
  EP.APPL.STATUS$   = ""
  EP.H.AUTH.NUMBER$ = ""
  EP.H.APPROV.CODE$ = ""
  EP.H.APPROV.DESC$ = ""
  EP.COMERCIO$      = ""
  EP.EPAY.TERMINAL$ = ""
  EP.FECHA.POSTEO$  = ""
  EP.FECHA.PROC$    = ""
  EP.COD.PROC$      = ""
  EP.RRN$           = ""
  EP.FRANQUICIA$    = ""
  EP.BANCO$         = ""
  EP.PRODUCTO$      = ""
  EP.FECHA.VENC$    = ""
  EP.CARD.BIN$      = ""
  EP.H.CARD.NUMBER$ = ""
  EP.H.USER.DATA$   = ""
  EP.APPROV.CODE$   = ""
  EP.APPROV.DESC$   = ""
  EP.ISO.TRX$       = ""   ! FALTA DEFINIR EL SITIO
  EP.VALOR.SALDO$   = ""
  franqResFlag% = 0 
!
    IF EP.M.LEN% >= 10 THEN \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    IF EP.M.LEN% >= 12 THEN  \
      EP.H.AUTH.NUMBER$ = MID$(EP.AMJ.MESSAGE$,12,6)
    IF EP.M.LEN% >= 18 THEN  \
      EP.H.APPROV.CODE$ = MID$(EP.AMJ.MESSAGE$,18,2)
    IF EP.M.LEN% >= 20 THEN  \
      EP.H.APPROV.DESC$ = MID$(EP.AMJ.MESSAGE$,20,20)
    IF EP.M.LEN% >= 40 THEN  \
      EP.COMERCIO$ = MID$(EP.AMJ.MESSAGE$,40,10)
    IF EP.M.LEN% >= 50 THEN  \
      EP.EPAY.TERMINAL$ = MID$(EP.AMJ.MESSAGE$,50,8)
    IF EP.M.LEN% >= 58 THEN  \
      EP.FECHA.POSTEO$ = MID$(EP.AMJ.MESSAGE$,58,4)
    IF EP.M.LEN% >= 62 THEN  \
      EP.FECHA.PROC$ = MID$(EP.AMJ.MESSAGE$,62,12)
    IF EP.M.LEN% >= 74 THEN  \
      EP.COD.PROC$ = MID$(EP.AMJ.MESSAGE$,74,6)
    IF EP.M.LEN% >= 80 THEN  \
      EP.RRN$ = MID$(EP.AMJ.MESSAGE$,80,12)
    IF EP.M.LEN% >= 92 THEN  \
      EP.FRANQUICIA$ = MID$(EP.AMJ.MESSAGE$,92,10)
    IF EP.M.LEN% >= 102 THEN  \
      EP.BANCO$  = MID$(EP.AMJ.MESSAGE$,102,10)
    IF EP.M.LEN% >= 112 THEN \
      EP.PRODUCTO$ = MID$(EP.AMJ.MESSAGE$,112,10)
    IF EP.M.LEN% >= 122 THEN \
      EP.FECHA.VENC$ = MID$(EP.AMJ.MESSAGE$,122,4)
    IF EP.M.LEN% >= 126 THEN \
      EP.CARD.BIN$ = MID$(EP.AMJ.MESSAGE$,126,6)    
    IF EP.M.LEN% >= 132 THEN  \
      EP.H.CARD.NUMBER$ = MID$(EP.AMJ.MESSAGE$,132,4)
    IF EP.M.LEN% >= 136 THEN  \
       EP.ISO.TRX$ = MID$(EP.AMJ.MESSAGE$,136,4) 
!      EP.H.USER.DATA$ = MID$(EP.AMJ.MESSAGE$,136,10)
		If EP.M.LEN% >= 146 Then \
			EP.VALOR.SALDO$ = Mid$(EP.AMJ.MESSAGE$,146,10)
	If EP.H.APPROV.CODE$ = "00" Then \
	Begin
		If EP.FUNCTION$ = "05" Then \
			franqResFlag% = addFranqResRecord(EP.FRANQUICIA$, EP.ALL.CARD$, -VAL(EP.VALOR.EPAY$), EP.H.AUTH.NUMBER$) \
		Else \
			franqResFlag% = addFranqResRecord(EP.FRANQUICIA$, EP.ALL.CARD$, VAL(EP.VALOR.EPAY$), EP.H.AUTH.NUMBER$)
	Endif
	!
	If EP.M.LEN% > 156 Then Begin
		EP.OPT.CONVENIO$ = trim$( Mid$(EP.AMJ.MESSAGE$, 156, 20) )
  	EP.OPT.BOLSILLO$ = trim$( EP.CARD.BIN$ + EP.H.CARD.NUMBER$ )
  Endif
END SUB

!
!
!
SUB TRANSLATE.COMM.CODE(UE.CODE$,UE.CODE.DESC$) PUBLIC
  STRING UE.CODE$,UE.CODE.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
!
  EP.FOUND% = 0
  EP.I% = 1
  WHILE NOT EP.FOUND% AND EP.I% < EP.SIZE.COMM%
    IF UE.CODE$ = EP.COMM.CODE$(EP.I%) THEN \
    BEGIN
      EP.FOUND% = -1
      UE.CODE.DESC$ = EP.COMM.RESPONSE$(EP.I%)
    ENDIF
    EP.I% = EP.I% + 1
  WEND
  IF NOT EP.FOUND% THEN \
    UE.CODE.DESC$ = EP.COMM.RESPONSE$(EP.SIZE.COMM%)
END SUB
!
!
SUB TRANSLATE.APPL.CODE(UE.CODE$,UE.CODE.DESC$) PUBLIC
  STRING UE.CODE$,UE.CODE.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
  EP.FOUND% = 0
  EP.I% = 1
  WHILE NOT EP.FOUND% AND EP.I% <= EP.SIZE.APPL%
    IF UE.CODE$ = EP.APPL.CODE$(EP.I%) THEN \
    BEGIN
      EP.FOUND% = -1
      UE.CODE.DESC$ = EP.APPL.RESPONSE$(EP.I%)
    ENDIF
    EP.I% = EP.I% + 1
  WEND
  IF NOT EP.FOUND% THEN \
    UE.CODE.DESC$ = EP.APPL.RESPONSE$(EP.SIZE.APPL%)
END SUB
!
!
SUB TRANSLATE.ISO.CODE(UE.CODE$,UE.CODE.DESC$) PUBLIC
  STRING UE.CODE$,UE.CODE.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
  EP.FOUND% = 0
  EP.I% = 1
  WHILE NOT EP.FOUND% AND EP.I% <= EP.SIZE.ISO%
    IF UE.CODE$ = EP.ISO.CODE$(EP.I%) THEN \
    BEGIN
      EP.FOUND% = -1
      UE.CODE.DESC$ = EP.ISO.RESPONSE$(EP.I%)
    ENDIF
    EP.I% = EP.I% + 1
  WEND
  IF NOT EP.FOUND% THEN \
    UE.CODE.DESC$ = EP.ISO.RESPONSE$(EP.SIZE.ISO%)
END SUB
!
!
SUB TRANSLATE.GUIDE.CODE(UE.CODE$,UE.CODE.DESC$) PUBLIC
  STRING UE.CODE$,UE.CODE.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
  EP.FOUND% = 0
  EP.I% = 1
  WHILE NOT EP.FOUND% AND EP.I% <= EP.SIZE.GUIDE%
    IF UE.CODE$ = EP.GUIDE.CODE$(EP.I%) THEN \
    BEGIN
      EP.FOUND% = -1
      UE.CODE.DESC$ = EP.GUIDE.MESSAGE$(EP.I%)
    ENDIF
    EP.I% = EP.I% + 1
  WEND
  IF NOT EP.FOUND% THEN \
    UE.CODE.DESC$ = EP.GUIDE.MESSAGE$(EP.SIZE.GUIDE%)
END SUB
!
SUB TRANSLATE.TV.CODE(UE.GROUP.DESC$,UE.CODE$) PUBLIC
  STRING UE.CODE$,UE.GROUP.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
  EP.FOUND% = 0
  EP.I% = 1                              
  UE.CODE$ = ""
  WHILE NOT EP.FOUND% AND EP.I% <= EP.SIZE.TV%
    EP.POS% = MATCH(UE.GROUP.DESC$,EP.TV.GROUP$(EP.I%),1)
    IF EP.POS% <> 0 THEN \
    BEGIN
      EP.FOUND% = -1
      UE.CODE$ = EP.TV.CODE$(EP.I%)
    ENDIF
    EP.I% = EP.I% + 1
  WEND
  IF NOT EP.FOUND% THEN \
    UE.CODE$ = "00"
END SUB
!
!
SUB CAL.TAX.TOTALS(EP.TAX.VAL%) PUBLIC
  INTEGER*4 EP.TAX.VAL%
  INTEGER*2 IFOR%
!  
  EP.TAX.VAL% = 0
  FOR IFOR% = 1 TO 8

    EP.TAX.VAL%= EP.TAX.VAL% + ROUND(FLOAT(UE.IVAC.TOTALS(IFOR%))/\
              (1.+FLOAT(UE.IVAC.TABLAIVA(IFOR%))/100.)*FLOAT(UE.IVAC.TABLAIVA(IFOR%))/100.,0,0)

!---------------------------------------------------------
!
!  CALL EP.LINE.PRINT("IVA= "+STR$(EP.TAX.VAL%)+" TOTIVA="+STR$(UE.IVAC.TOTALS(IFOR%))+\
!    " TABLAIVA="+STR$(UE.IVAC.TABLAIVA(IFOR%)), 4100H)
!-----------------------------------------------------

  NEXT IFOR%
  
END SUB
!
!
SUB CAL.TAX.BASE(TAX.BASE%) PUBLIC
  INTEGER*4 TAX.BASE%
  INTEGER*2 IFOR%, POS%
  TAX.BASE% = 0
  FOR IFOR% = 1 TO 8
    POS% = MATCH(STR$(UE.IVAC.TABLAIVA(IFOR%))+"%",EP.IVADEV$,1)
    IF POS% <> 0 THEN \
    BEGIN  
      TAX.BASE%= TAX.BASE% + ROUND(FLOAT(UE.IVAC.TOTALS(IFOR%))/\
              (1.+FLOAT(UE.IVAC.TABLAIVA(IFOR%))/100.),0,0)
    ENDIF 
!---------------------------------------------------------
!  CALL EP.LINE.PRINT("BASEiva= "+STR$(TAX.BASE%)+" TOTIVA="+STR$(UE.IVAC.TOTALS(IFOR%))+\
!    " TABLAIVA="+STR$(UE.IVAC.TABLAIVA(IFOR%)), 4100H)
!-----------------------------------------------------
!---------------------------------------------------------
!  TS.LINETYPE = 29
!  TS.SAVPRT$ = "BASEiva= "+STR$(TAX.BASE%)+" TOTBASE="+STR$(UE.IVAC.TOTALS(IFOR%))+\
!    " TABLAIVA="+STR$(UE.IVAC.TABLAIVA(IFOR%))
!  TS.SAVPRT.OPT = 4100H
!  CALL TSPREC01
!-----------------------------------------------------
  NEXT IFOR%
END SUB
!
Sub saveVerifType(pIdx%)
	Integer*2 pIdx%
	!
	EP.VERIF.TV.SAV% = TO.TENDOPTS(pIdx%, 7)
	EP.VERIF.TV.IDX% = pIdx%
	TO.TENDOPTS(pIdx%, 7) = 2
End Sub
!
Sub restoreVerifType
	If EP.VERIF.TV.IDX% > -1 Then Begin
		TO.TENDOPTS(EP.VERIF.TV.IDX%, 7) = EP.VERIF.TV.SAV%
		EP.VERIF.TV.IDX% = -1
	Endif
End Sub
!
SUB EP.BUSCA.LIMITE.TV(UE.TIPO.VAR$) PUBLIC
  INTEGER*2 UE.I%
  INTEGER*1 UE.FOUND%
  STRING UE.TIPO.VAR$

  Call restoreVerifType
  !
  UE.I% = 1
  UE.FOUND% = 0 
  WHILE UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    IF TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) THEN \
    BEGIN 
      EP.MAX.TV% = TO.TENDLIMITS(UE.I%,0)
      IF TO.TENDLIMITS(UE.I%,1) < 0 THEN \
        EP.CHG.TV% = ABS(TO.TENDLIMITS(UE.I%,1)+1) \
      ELSE \
        EP.CHG.TV% = TO.TENDLIMITS(UE.I%,1) 
      IF TO.TENDOPTS(UE.I%,7) <> 2 THEN \
      BEGIN 
        Call saveVerifType( UE.I% )
      ENDIF \
      ELSE  \
      BEGIN
        EP.VERIF.TV.SAV% = 99
      ENDIF
      UE.FOUND%  = -1
      EP.TV.POS% = UE.I%
    ENDIF \
    ELSE  \
    UE.I% = UE.I% + 1
!    CALL EP.LINE.PRINT("TV="+UE.TIPO.VAR$,4100H)
!    CALL EP.LINE.PRINT(STR$(UE.I%)+"T="+STR$(TO.TENDOPTS(UE.I%,0))+"V="+STR$(TO.TENDOPTS(UE.I%,1)),4100H)
!    CALL EP.LINE.PRINT(STR$(TO.TENDLIMITS(UE.I%,0))+"CH="+STR$(TO.TENDLIMITS(UE.I%,1)),4100H)
!    CALL EP.LINE.PRINT("VERIF="+STR$(TO.TENDOPTS(UE.I%,7)),4100H)
  WEND    


END SUB
!
FUNCTION EP.BUSCA.INDICE.TV(UE.TIPO.VAR$) PUBLIC
  INTEGER*2 UE.I%,EP.BUSCA.INDICE.TV
  INTEGER*1 UE.FOUND%
  STRING UE.TIPO.VAR$

  UE.I% = 1
  UE.FOUND% = 0 
  WHILE UE.I% <= TO.NUMTNDR AND NOT UE.FOUND%
    IF TO.TENDOPTS(UE.I%,0) = VAL(LEFT$(UE.TIPO.VAR$,1)) AND  \
       TO.TENDOPTS(UE.I%,1) = VAL(RIGHT$(UE.TIPO.VAR$,1)) THEN \
      UE.FOUND%  = -1 \
    ELSE  \
      UE.I% = UE.I% + 1
  WEND
  IF UE.FOUND% THEN \
    EP.BUSCA.INDICE.TV = UE.I% \
  ELSE \
    EP.BUSCA.INDICE.TV = -1
END FUNCTION
!
FUNCTION EP.BUSCA.MONTO.TV(UE.TIPO.VAR$) PUBLIC
  INTEGER*2 indiceTV%
  INTEGER*4 EP.BUSCA.MONTO.TV
  STRING UE.TIPO.VAR$
  
  indiceTV% = EP.BUSCA.INDICE.TV(UE.TIPO.VAR$)
  IF indiceTV% > -1 THEN \
    EP.BUSCA.MONTO.TV = TS.TENDVAMT(indiceTV%) \
  ELSE \
    EP.BUSCA.MONTO.TV = 0
END FUNCTION
!
!
FUNCTION EP.GET.DATA(UE.DISP.MESSAGE$) PUBLIC
   STRING EP.GET.DATA,requestAnswer$,UE.DISP.MESSAGE$, UE.MSR.DATA$, UE.KEYB.DATA$, UE.SAVDISP$
!
   CALL EP.SAVE.KEYS
   UE.MSR.DATA$ = ""
   UE.KEYB.DATA$ = ""
   UE.SAVDISP$ = TS.SAVDISP1$ + TS.SAVDISP2$
   TS.SAVDISP1$ = LEFT$(UE.DISP.MESSAGE$,20) 
   TS.SAVDISP2$ = MID$(UE.DISP.MESSAGE$,21,20)
   TS.IO.MOTORKEY = 0                      ! Set for no input yet
   TS.IO.STATE = 11                        ! Enter/Data state
   WHILE TS.IO.MOTORKEY = 0                ! While still no input
     UNLOCKDEV 32, TS.IO.STATE, PRIORITY   ! Keyed as account number
     IF EVENT% NE 41 THEN \
     BEGIN                                 ! Not MSR already
       TS.LINETYPE = 9                    ! Redisplay prompt
       CALL TSDSEC01                      ! Prompt for Customer ID
     ENDIF                                ! Not MSR in Kiosk mode
       TS.ER.RETURN = -1                  ! Return from error
       UNLOCKDEV  41                      ! Unlock the MSR
     TS.ER.RETURN = -1                    ! Return from error
     WAIT  32, 41; 10                     ! Wait for input
     WHILE NOT EVENT%                     ! While no input
       TS.ER.RETURN = -1                  ! Return from error
       WAIT  32, 41; 100                  ! Wait for input
     WEND
     TS.ER.RETURN = 0
! 
     IF EVENT% = 32 THEN \                ! Process keyboard entry
     BEGIN                                ! Input from I/O processor
       TS11.OVRFLAG = 34                  ! Mark for exit 14
       CALL TSCSECRK                      ! Parse input / exit 14
       IF TS.IO.MOTORKEY = 73 THEN \      ! Clear key
       BEGIN  
         IF LEN(TS.IO.DATA$(1)) > 0 OR    \ Data Entered with clear key
            TS.TEMP1I2 > 1 THEN \         ! More than 1 key
         BEGIN        
           TS.IO.MOTORKEY = 0             ! Try again
         ENDIF                            ! Clear after data
       ENDIF \ 
       ELSE \                             ! Not clear key
       BEGIN  
         IF TS.IO.KEYS(10) = 80 THEN \    ! Keyed data
         BEGIN 
           IF LEN(TS.IO.DATA$(10)) > 0 THEN  \ any option taken
           BEGIN 
!             IF (VAL(TS.IO.DATA$(10)) < 1 OR    \ Not valid option 
!                VAL(TS.IO.DATA$(10)) > 2 ) THEN \
!             BEGIN                            !
!               TS.LINETYPE = 8                ! Operator Guidance
!               TS.LINEDATA = 6                ! Data out of range
!               CALL TSCSEC08                  ! Guidance with Clear
!               TS.IO.MOTORKEY = 0             ! Try again
!             ENDIF \                          ! Good option or no option
!             ELSE \                           ! Valid option
!             BEGIN  
               UE.KEYB.DATA$ = TS.IO.DATA$(10) ! Move option keyed
               UE.MSR.DATA$  = ""             ! Not data read
!            ENDIF                            ! Valid option
           ENDIF \
           ELSE \
           BEGIN
             TS.LINETYPE = 8                ! Operator Guidance
             TS.LINEDATA = 6                ! Data out of range
             CALL TSCSEC08                  ! Guidance with Clear
             TS.IO.MOTORKEY = 0             ! Try again
           ENDIF
         ENDIF                              ! Keyed data
       ENDIF                                ! Not clear key
     ENDIF \ 
     ELSE \                                 ! Not I/O processor
     BEGIN   
!      
       IF EVENT% = 41 THEN \                ! Input from MSR
       BEGIN
         UE.KEYB.DATA$ = ""
         TS.ER.RETURN = -1                  ! Return from error
         READ #  41 ; LINE UE.MSR.DATA$     ! Read MSR
         IF LEN(UE.MSR.DATA$) < 6 OR        \ Insufficient data
            TS.ER.RETURN = 0 THEN \
         BEGIN     ! Bad read
           TS.IO.MOTORKEY = 73              ! Clear key
         ENDIF \
         ELSE \
         BEGIN                              ! Data read
           TS.IO.DEVICE = 1                 ! Force a keyed account
           TS.IO.MOTORKEY = 80              ! Show input ready
         ENDIF                              ! Data read
         TS.ER.RETURN = 0
       ENDIF                                ! Input from MSR
     ENDIF                                  ! Not I/O processor
   WEND                                     ! While still no input
! 
   TS11.OVRFLAG = 0                         ! Clear exit marker
   TS.SAVDISP1$ = LEFT$(UE.SAVDISP$,20)     ! Restore prior display
   TS.SAVDISP2$ = MID$(UE.SAVDISP$,21,20)
   UE.SAVDISP$ = ""
   TS.IO.NEXTSTATE = 10                     ! Restore state = MAIN
   TS.IO.STATE = 10                         ! Restore state = MAIN
   UNLOCKDEV 32, TS.IO.STATE                ! Restore state = MAIN
!
   ! Retorna el medio de entrada en 1 digito (1=Teclado 2=MSR 3=Scanner)
   ! y el dato entrado a continuación
   IF UE.MSR.DATA$ <> "" THEN \
      requestAnswer$ = "2" + UE.MSR.DATA$ \
   ELSE \
      requestAnswer$ = STR$(TS.IO.DEVICE) + UE.KEYB.DATA$
   CALL EP.RESTORE.KEYS
   TS.IO.DEVICE = 1
   EP.GET.DATA = requestAnswer$
END FUNCTION
!
!
!  Read MSG data
!
SUB EP.GET.TRX.TYPE(UE.DISP.MESSAGE$,UE.MSR.DATA$,UE.KEYB.DATA$) PUBLIC
   STRING UE.DISP.MESSAGE$, UE.MSR.DATA$, UE.KEYB.DATA$, UE.SAVDISP$
!
   UE.MSR.DATA$ = ""
   UE.SAVDISP$ = TS.SAVDISP1$ + TS.SAVDISP2$
   TS.SAVDISP1$ = LEFT$(UE.DISP.MESSAGE$,20) 
   TS.SAVDISP2$ = MID$(UE.DISP.MESSAGE$,21,20)
   TS.IO.MOTORKEY = 0                      ! Set for no input yet
   TS.IO.STATE = 7                         ! Acct/Date state
   WHILE TS.IO.MOTORKEY = 0                ! While still no input
     UNLOCKDEV 32, TS.IO.STATE, PRIORITY   ! Keyed as account number
     IF EVENT% NE 41 THEN \
     BEGIN                                 ! Not MSR already
       TS.LINETYPE = 9                    ! Redisplay prompt
       CALL TSDSEC01                      ! Prompt for Customer ID
     ENDIF                                ! Not MSR in Kiosk mode
       TS.ER.RETURN = -1                  ! Return from error
       UNLOCKDEV  41                      ! Unlock the MSR
     TS.ER.RETURN = -1                    ! Return from error
     WAIT  32, 41; 10                     ! Wait for input
     WHILE NOT EVENT%                     ! While no input
       TS.ER.RETURN = -1                  ! Return from error
       WAIT  32, 41; 100                  ! Wait for input
     WEND
     TS.ER.RETURN = 0
! 
     IF EVENT% = 32 THEN \                ! Process keyboard entry
     BEGIN                                ! Input from I/O processor
       TS11.OVRFLAG = 34                  ! Mark for exit 14
       CALL TSCSECRK                      ! Parse input / exit 14
       IF TS.IO.MOTORKEY = 73 THEN \      ! Clear key
       BEGIN  
         IF LEN(TS.IO.DATA$(1)) > 0 OR    \ Data Entered with clear key
            TS.TEMP1I2 > 1 THEN \         ! More than 1 key
         BEGIN        
           TS.IO.MOTORKEY = 0             ! Try again
         ENDIF                            ! Clear after data
       ENDIF \ 
       ELSE \                             ! Not clear key
       BEGIN  
         IF TS.IO.KEYS(10) = 80 THEN \    ! Keyed data
         BEGIN 
           IF LEN(TS.IO.DATA$(10)) > 0 THEN  \ any option taken
           BEGIN 
             IF (VAL(TS.IO.DATA$(10)) < 1 OR    \ Not valid option 
                VAL(TS.IO.DATA$(10)) > 2 ) THEN \
             BEGIN                            !
               TS.LINETYPE = 8                ! Operator Guidance
               TS.LINEDATA = 6                ! Data out of range
               CALL TSCSEC08                  ! Guidance with Clear
               TS.IO.MOTORKEY = 0             ! Try again
             ENDIF \                          ! Good option or no option
             ELSE \                           ! Valid option
             BEGIN  
               UE.KEYB.DATA$ = TS.IO.DATA$(10) ! Move option keyed
               UE.MSR.DATA$  = ""             ! Not data read
             ENDIF                            ! Valid option
           ENDIF \
           ELSE \
           BEGIN
             TS.LINETYPE = 8                ! Operator Guidance
             TS.LINEDATA = 6                ! Data out of range
             CALL TSCSEC08                  ! Guidance with Clear
             TS.IO.MOTORKEY = 0             ! Try again
           ENDIF
         ENDIF                              ! Keyed data
       ENDIF                                ! Not clear key
     ENDIF \ 
     ELSE \                                 ! Not I/O processor
     BEGIN   
!      
       IF EVENT% = 41 THEN \                ! Input from MSR
       BEGIN
         UE.KEYB.DATA$ = ""
         TS.ER.RETURN = -1                  ! Return from error
         READ #  41 ; LINE UE.MSR.DATA$     ! Read MSR
         IF LEN(UE.MSR.DATA$) < 6 OR        \ Insufficient data
            TS.ER.RETURN = 0 THEN \
         BEGIN     ! Bad read
           TS.IO.MOTORKEY = 73              ! Clear key
         ENDIF \
         ELSE \
         BEGIN                              ! Data read
           TS.IO.DEVICE = 1                 ! Force a keyed account
           TS.IO.MOTORKEY = 80              ! Show input ready
         ENDIF                              ! Data read
         TS.ER.RETURN = 0
       ENDIF                                ! Input from MSR
     ENDIF                                  ! Not I/O processor
   WEND                                     ! While still no input
! 
   TS11.OVRFLAG = 0                         ! Clear exit marker
   TS.SAVDISP1$ = LEFT$(UE.SAVDISP$,20)     ! Restore prior display
   TS.SAVDISP2$ = MID$(UE.SAVDISP$,21,20)
   UE.SAVDISP$ = ""
   TS.IO.NEXTSTATE = 10                     ! Restore state = MAIN
   TS.IO.STATE = 10                         ! Restore state = MAIN
   UNLOCKDEV 32, TS.IO.STATE                ! Restore state = MAIN
!
END SUB
!-----------------------------------------------------------
!  	Electronic funds transfer API routine
!-----------------------------------------------------------
!                  EFT Common Application Routines
!
!
SUB EP.PARSE.MSRTEST.REQUEST PUBLIC
!
    EP.APPL.STATUS$  = ""
!
    IF EP.M.LEN% >= 10 THEN \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
!      
END SUB
!
SUB EP.PARSE.DISPLAY.REQUEST PUBLIC
!
    EP.DISP.MESSAGE$ = ""
    EP.DISP.PARAM$   = ""
    EP.DISP.OPER$    = ""
    EP.DISP.CUST$    = ""
    EP.APPL.STATUS$  = ""
!
    IF EP.M.LEN% >= 10 THEN \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    IF EP.M.LEN% >= 12 THEN \
      EP.DISP.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,12,40)
    IF EP.M.LEN% >= 52 THEN \
      EP.DISP.PARAM$ = MID$(EP.AMJ.MESSAGE$,52,1)
    IF EP.M.LEN% >= 53 THEN \
      EP.DISP.OPER$ = MID$(EP.AMJ.MESSAGE$,53,1)
    IF EP.M.LEN% >= 54 THEN \
      EP.DISP.CUST$ = MID$(EP.AMJ.MESSAGE$,54,1)
!      
END SUB
!
!
SUB EP.PARSE.PRT.HEADER PUBLIC
!
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUST$    = ""
    EP.PRT.VOUC$    = ""
    EP.PRT.CUT$     = ""
    EP.APPL.STATUS$ = ""
!
    IF EP.M.LEN% >= 10 THEN \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    IF EP.M.LEN% >= 12 THEN \
      EP.PRT.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,12,38)
    IF EP.M.LEN% >= 50 THEN \
      EP.PRT.PARAM$ = MID$(EP.AMJ.MESSAGE$,50,1)
    IF EP.M.LEN% >= 51 THEN \
      EP.PRT.CUST$ = MID$(EP.AMJ.MESSAGE$,51,1)
    IF EP.M.LEN% >= 52 THEN \
      EP.PRT.VOUC$ = MID$(EP.AMJ.MESSAGE$,52,1)
    IF EP.M.LEN% >= 53 THEN \
      EP.PRT.CUT$ = MID$(EP.AMJ.MESSAGE$,53,1)
!      
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "Prt Head="+STR$(LEN(EP.PRT.MESSAGE$)) +\
!                 " len= "+STR$(EP.M.LEN%)+\
!                 " pparm="+EP.PRT.PARAM$+ \
!                 " stat= "+EP.APPL.STATUS$                 
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------
END SUB
!
SUB EP.PARSE.PRT.LINE PUBLIC
!
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUST$    = ""
    EP.PRT.VOUC$    = ""
    EP.APPL.STATUS$ = ""
!
    IF EP.M.LEN% >= 10 THEN \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    IF EP.M.LEN% >= 12 THEN \
      EP.PRT.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,12,38)
    IF EP.M.LEN% >= 50 THEN \
      EP.PRT.PARAM$ = MID$(EP.AMJ.MESSAGE$,50,1)
    IF EP.M.LEN% >= 51 THEN \
      EP.PRT.CUST$ = MID$(EP.AMJ.MESSAGE$,51,1)
    IF EP.M.LEN% >= 52 THEN \
      EP.PRT.VOUC$ = MID$(EP.AMJ.MESSAGE$,52,1)
!      
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "Prt line="+STR$(LEN(EP.PRT.MESSAGE$)) +\
!                 " len= "+STR$(EP.M.LEN%)+\
!                 " pparm="+EP.PRT.PARAM$+ \
!                 " stat= "+EP.APPL.STATUS$                 
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------
END SUB
!
SUB EP.PARSE.PRT.CLOSE PUBLIC
!
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUT$     = ""
    EP.APPL.STATUS$ = ""
!
    IF EP.M.LEN% >= 10 THEN \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    IF EP.M.LEN% >= 12 THEN \
      EP.PRT.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,12,38)
    IF EP.M.LEN% >= 50 THEN \
      EP.PRT.PARAM$ = MID$(EP.AMJ.MESSAGE$,50,1)
    IF EP.M.LEN% >= 51 THEN \
      EP.PRT.CUT$ = MID$(EP.AMJ.MESSAGE$,51,1)
!      
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "Prt close="+STR$(LEN(EP.DISP.MESSAGE$)) +\
!                 " len= "+STR$(EP.M.LEN%)+\
!                 " pparm="+EP.PRT.PARAM$+ \
!                 " stat= "+EP.APPL.STATUS$
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------
END SUB
!
SUB EP.PARSE.READNVRAM.REQUEST PUBLIC
!
	EP.nvramPointer%  = 0
	EP.nvramFormat$ = ""
	EP.APPL.STATUS$   = ""
!
	IF EP.M.LEN% >= 10 THEN \
		EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
	IF EP.M.LEN% >= 12 THEN \
		EP.nvramPointer% = INT%(VAL(MID$(EP.AMJ.MESSAGE$,12,10)))
	IF EP.M.LEN% >= 22 THEN \
		EP.nvramFormat$ = STR$(VAL(MID$(EP.AMJ.MESSAGE$,22,2)))
	IF EP.M.LEN% >= 24 THEN \
		EP.nvramFormat$ = MID$(EP.AMJ.MESSAGE$,24,1) + EP.nvramFormat$ \
	ELSE \
		EP.nvramFormat$ = "C" + EP.nvramFormat$
END SUB
!
SUB EP.PARSE.WRITENVRAM.REQUEST PUBLIC
	Integer*2 nvramLen%
!
	EP.nvramMessage$ = ""
	EP.nvramPointer%  = 0
	EP.nvramFormat$ = ""
	EP.APPL.STATUS$   = ""
!
	IF EP.M.LEN% >= 10 THEN \
		EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
	IF EP.M.LEN% >= 12 THEN \
		EP.nvramMessage$ = MID$(EP.AMJ.MESSAGE$,12,60)
	IF EP.M.LEN% >= 72 THEN \
		EP.nvramPointer% = INT%(VAL(MID$(EP.AMJ.MESSAGE$,72,10)))
	IF EP.M.LEN% >= 82 THEN \
	BEGIN
		nvramLen% = VAL(MID$(EP.AMJ.MESSAGE$,82,2))
		EP.nvramFormat$ = STR$(nvramLen%)
	ENDIF
	IF EP.M.LEN% >= 84 THEN \
		EP.nvramFormat$ = MID$(EP.AMJ.MESSAGE$,84,1) + EP.nvramFormat$
	IF LEFT$(EP.nvramFormat$,1) = "P" THEN \
		nvramLen% = nvramLen% * 2
	EP.nvramMessage$ = LEFT$(EP.nvramMessage$,nvramLen%)
END SUB
!
SUB EP.PARSE.PARTIALANSWER.REQUEST PUBLIC
	INTEGER*2 tmpIndex%
	EP.APPL.STATUS$   = ""
	IF EP.M.LEN% >= 10 THEN \
		EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
	IF EP.M.LEN% >= 12 THEN \
	BEGIN
		tmpIndex% = INT%(VAL(MID$(EP.AMJ.MESSAGE$,12,3)))
		IF EP.M.LEN% >= 15 AND tmpIndex% >= 0 AND tmpIndex% < 300 THEN \
		begin
			EP.APPLMGR.DATA$(tmpIndex%) = MID$(EP.AMJ.MESSAGE$,15,EP.M.LEN% - 14)
		endif
	ENDIF
END SUB
!
SUB EP.PARSE.DATA.REQUEST PUBLIC
!
    EP.DISP.MESSAGE$  = ""
    EP.INI.RANGE$     = ""
    EP.END.RANGE$     = ""
    EP.APPL.STATUS$   = ""
!
    IF EP.M.LEN% >= 10 THEN \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    IF EP.M.LEN% >= 12 THEN \
      EP.DISP.MESSAGE$ = MID$(EP.AMJ.MESSAGE$,12,40)
    IF EP.M.LEN% >= 52 THEN \
      EP.INI.RANGE$ = MID$(EP.AMJ.MESSAGE$,52,20)
    IF EP.M.LEN% >= 72 THEN \
      EP.END.RANGE$ = MID$(EP.AMJ.MESSAGE$,72,20)
!      
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!   TS.SAVPRT$ = "Data req="+STR$(LEN(EP.DISP.MESSAGE$)) +\
!                 " len= "+STR$(EP.M.LEN%)   +\
!                 " stat= "+EP.APPL.STATUS$
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------
END SUB
!
SUB EP.PARSE.DENTRY.REQUEST PUBLIC
!
    EP.DE1.DATA$ = ""
    EP.DE2.DATA$ = ""
    EP.DE3.DATA$ = ""
    EP.DE4.DATA$ = ""
    EP.DE5.DATA$ = ""
    EP.DE6.DATA$ = ""
    EP.APPL.STATUS$ = ""
!
    IF EP.M.LEN% >= 10 THEN \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    IF EP.M.LEN% >= 12 THEN \
      EP.DE1.DATA$ = MID$(EP.AMJ.MESSAGE$,12,12)
    IF EP.M.LEN% >= 24 THEN \
      EP.DE2.DATA$ = MID$(EP.AMJ.MESSAGE$,24,20)
    IF EP.M.LEN% >= 44 THEN \
      EP.DE3.DATA$ = MID$(EP.AMJ.MESSAGE$,44,20)
    IF EP.M.LEN% >= 64 THEN \
      EP.DE4.DATA$ = MID$(EP.AMJ.MESSAGE$,64,20)
    IF EP.M.LEN% >= 84 THEN \
      EP.DE5.DATA$ = MID$(EP.AMJ.MESSAGE$,84,20)
    IF EP.M.LEN% >= 104 THEN \
      EP.DE6.DATA$ = MID$(EP.AMJ.MESSAGE$,104,20)

!      
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "Dentry="+STR$(LEN(EP.DE1.DATA$)) +\
!                 " len= "+STR$(EP.M.LEN%)   +\
!                 " stat= "+EP.APPL.STATUS$
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------
END SUB
!
!
SUB EP.PARSE.DENTRY99.REQUEST PUBLIC
  INTEGER*2 keyIndex%
!
    EP.DE1.DATA$ = ""
    EP.DE2.DATA$ = ""
    EP.DE3.DATA$ = ""
    EP.DE4.DATA$ = ""
    EP.DE5.DATA$ = ""
    EP.DE6.DATA$ = ""
    EP.APPL.STATUS$ = ""
!
    IF EP.M.LEN% >= 10 THEN \
      EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    IF EP.M.LEN% >= 12 THEN \
      EP.DE1.DATA$ = MID$(EP.AMJ.MESSAGE$,12,12)
    IF EP.M.LEN% >= 24 THEN \
      EP.DE2.DATA$ = MID$(EP.AMJ.MESSAGE$,24,255)
! Remueve asteriscos a la derecha
    keyIndex% = MATCH("*",EP.DE2.DATA$,1)
    IF keyIndex% <> 0 THEN \
      EP.DE2.DATA$ = LEFT$(EP.DE2.DATA$,keyIndex% - 1 )
!      
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "S&F="+STR$(LEN(EP.DE1.DATA$)) +\
!                 " len= "+STR$(EP.M.LEN%)   +\
!                 " stat= "+EP.APPL.STATUS$
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------
END SUB
!
Sub EP.PARSE.DATA.REGULAR PUBLIC
!
	EP.DATA.REGULAR$ = ""
    EP.APPL.STATUS$ = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = Mid$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      EP.DATA.REGULAR$ = Mid$(EP.AMJ.MESSAGE$,12,Len(EP.AMJ.MESSAGE$) - 11)
!      
End Sub
!
SUB EP.ADD.DATA.REGULAR.UE(pData$) PUBLIC
  STRING pData$,tmpData$(1),newData6$,tmpData6$,tmpSep$
  Integer*2 lastIndex%,newIndex%,dataCounter%
  Integer*1 isPack%
  !
  Dim tmpData$(6)
  ! El primer digito indica si hay que empaquetar
  If Left$(pData$,1) = "0" Then \
  	isPack% = 0 \
  Else \
  	isPack% = -1
  pData$ = rTrim$(Right$(pData$,Len(pData$) - 1))
  !
  dataCounter% = 1
  lastIndex% = 0
  newIndex% = 1
  While newIndex% > 0 And dataCounter% <= 6
  	newIndex% = Match(":",pData$,lastIndex% + 1)
  	If newIndex% > 0 And dataCounter% < 6 Then \
  	Begin
  		tmpData$(dataCounter%) = Mid$(pData$,lastIndex% + 1,newIndex% - lastIndex% - 1)
  		lastIndex% = newIndex%
  	EndIf Else \
  	Begin
  		tmpData$(dataCounter%) = Mid$(pData$,lastIndex% + 1,Len(pData$) - lastIndex%)
  	EndIf
  	If dataCounter% < 6 And (isPack% Or dataCounter% = 1) Then \
  		tmpData$(dataCounter%) = Pack$(tmpData$(dataCounter%))
  	dataCounter% = dataCounter% + 1
  Wend
  !
  ! Si hay separadores ':' en el campo 6, los busca para garantizar que estos
  ! separadores no se empaqueten
  If tmpData$(6) <> "" Then \
  Begin
	  newData6$ = ""
	  lastIndex% = 0
	  newIndex% = 1
	  While newIndex% > 0
	  	newIndex% = Match(":",tmpData$(6),lastIndex% + 1)
	  	If newIndex% > 0 Then \
	  	Begin
	  		tmpData6$ = Mid$(tmpData$(6),lastIndex% + 1,newIndex% - lastIndex% - 1)
	  		tmpSep$ = Mid$(tmpData$(6),newIndex%,1)
	  		lastIndex% = newIndex%
	  	EndIf Else \
	  	Begin
	  		tmpData6$ = Mid$(tmpData$(6),lastIndex% + 1,Len(tmpData$(6)) - lastIndex%)
	  		tmpSep$ = ""
	  	EndIf
	  	If isPack% Then \
	  		tmpData6$ = Pack$(tmpData6$)
	  	newData6$ = newData6$ + tmpData6$ + tmpSep$
	  Wend
	  tmpData$(6) = newData6$
  EndIf
  !
  TS.USERDATA$ = tmpData$(1) + ":" + tmpData$(2) + ":" + tmpData$(3) \
    + ":" + tmpData$(4) + ":" + tmpData$(5) + ":" + tmpData$(6)
     
  TS.TEMP1I1 = 99                  
  CALL TSTPEC01
!
END SUB
!
Sub EP.ADD.DATA.REGULAR Public
	Call EP.ADD.DATA.REGULAR.UE( EP.DATA.REGULAR$ )
End Sub
!
SUB EP.REVERSE.TRX PUBLIC
!
  IF EP.TRX.PEND$ <> "" THEN \
  BEGIN 
    EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) + \
        RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.TRX.PEND$ + \
        EP.DE.INVOICE.NBR$ + EP.FECHA.PEND$ + "00" 
    !
    !-------------------------------------------------------------------
    ! 2021-09-07 jsv
    ! Se agrega el número de tarjeta o número de cuenta en el mensaje 
    ! que se envía a Java para solicitar reverso de una operación
    !-------------------------------------------------------------------
    EP.MESSAGE$ = EP.MESSAGE$ + EP.REV.ACCOUNT$
    !-------------------------------------------------------------------
    !
    EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
    EP.MESSAGE$ = EP.REV.APPL$ + EP.REV.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
    CALL EP.SEND.TO.THREADER
    EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
    IF EP.M.LEN% >= 8 THEN \
    BEGIN
      EP.REV.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
    ENDIF \
    ELSE \
    BEGIN
      EP.REV.STATUS$ = "0"
    ENDIF
    IF EP.REV.STATUS$ = "0" OR   \
       EP.REV.STATUS$ = "1" THEN \
    BEGIN
      !EXIT SUB
      GOTO REVERSE.END
    ENDIF
!       
    IF EP.REV.STATUS$ = "9" OR   \
       EP.REV.STATUS$ = "A" THEN \
    BEGIN
      CALL EP.PARSE.DENTRY.REQUEST
      EP.REVTRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
      EP.REV.APROB$ = MID$(EP.DE2.DATA$,19,2) 
      CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
           EP.DE5.DATA$, EP.DE6.DATA$)
      IF EP.REV.APROB$ = "C4" THEN \     ! no hay trx disponible en host para reversar
      BEGIN 
        EP.REV.COUNT% = EP.REV.COUNT% + 1
        IF EP.REV.COUNT% >= 3 THEN \
        BEGIN 
          EP.TOHOST.FOUND% = -1
          EP.TRX.PEND$ = "" 
          EP.REV.FUNCTION$ = ""
          EP.FECHA.PEND$ = ""
          EP.REV.COUNT% = 0
          EP.REV.APROB$ = ""
        ENDIF  
      ENDIF \
      ELSE \
      IF EP.REVTRX.STATUS$ = "0" THEN  \
      BEGIN
        EP.TOHOST.FOUND% = -1
        EP.TRX.PEND$ = "" 
        EP.REV.FUNCTION$ = ""
        EP.FECHA.PEND$ = ""
        EP.REV.COUNT% = 0
        EP.REV.APROB$ = ""
      ENDIF  
    ENDIF   
  ENDIF
!
REVERSE.END:
! Si persiste la transacción pendiente de
! reverso, se graba en un arreglo de reversos
! pendientes para que no se bloqueen las
! demás aplicaciones tef por este reverso
IF EP.TRX.PEND$ <> "" THEN \
BEGIN
	EP.TRX.REV.PEND$(VAL(EP.REV.APPL$),0) = EP.MESSAGE$
	EP.TRX.REV.PEND$(VAL(EP.REV.APPL$),1) = "0"
	EP.TOHOST.FOUND% = -1
	EP.TRX.PEND$ = "" 
	EP.REV.FUNCTION$ = ""
	EP.FECHA.PEND$ = ""
	EP.REV.COUNT% = 0
	EP.REV.APROB$ = ""
ENDIF
!
!--------------------------------------------------------------------
! 2022-03.23 jsv
! La remoción de líneas de voucher sólo aplica para la aplicación 08
!--------------------------------------------------------------------
If EP.REV.APPL$ = "08" Then Begin
	!--------------------------------------------------------------------
	! 2021-09-07 jsv
	! Se invoca rutina para eliminar líneas de voucher
	!--------------------------------------------------------------------
	Call EP.REMOVE.VOUCHER(EP.REV.VOUCHER.START%, EP.REV.VOUCHER.END%)
	!--------------------------------------------------------------------
Endif
!
END SUB
!
FUNCTION EP.SEND.REVERSE(reverseAppl$) PUBLIC
	String reverseAppl$
	Integer*1 EP.SEND.REVERSE,sendAnswer%
	Integer*2 reverseIndex%
	!
	sendAnswer% = -1
	reverseIndex% = Int%(Val(reverseAppl$))
	If reverseIndex% >= 0 And reverseIndex% < 100 Then \
	Begin
		If EP.TRX.REV.PEND$(reverseIndex%,0) <> "" Then \
		Begin
			sendAnswer% = 0
			EP.MESSAGE$ = EP.TRX.REV.PEND$(reverseIndex%,0)
			CALL EP.SEND.TO.THREADER
			EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
			IF EP.M.LEN% >= 8 THEN \
			BEGIN
				EP.REV.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
			ENDIF \
			ELSE \
			BEGIN
				EP.REV.STATUS$ = "0"
			ENDIF
			IF EP.REV.STATUS$ = "0" OR   \
					EP.REV.STATUS$ = "1" THEN \
			BEGIN
				GOTO SEND.REVERSE.END
			ENDIF
			IF EP.REV.STATUS$ = "9" OR   \
					EP.REV.STATUS$ = "A" THEN \
			BEGIN
				CALL EP.PARSE.DENTRY.REQUEST
				EP.REVTRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
				EP.REV.APROB$ = MID$(EP.DE2.DATA$,19,2) 
				CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
					EP.DE5.DATA$, EP.DE6.DATA$)
				IF EP.REV.APROB$ = "C4" THEN \     ! no hay trx disponible en host para reversar
				BEGIN 
					EP.TRX.REV.PEND$(reverseIndex%,1) = STR$(VAL(EP.TRX.REV.PEND$(reverseIndex%,1)) + 1)
					IF VAL(EP.TRX.REV.PEND$(reverseIndex%,1)) >= 3 THEN \
					BEGIN
						sendAnswer% = -1
						EP.TRX.REV.PEND$(reverseIndex%,0) = ""
						EP.TRX.REV.PEND$(reverseIndex%,1) = ""
					ENDIF  
				ENDIF \
				ELSE \
				IF EP.REVTRX.STATUS$ = "0" THEN  \
				BEGIN
					sendAnswer% = -1
					EP.TRX.REV.PEND$(reverseIndex%,0) = ""
					EP.TRX.REV.PEND$(reverseIndex%,1) = ""
				ENDIF
			ENDIF
		ENDIF
	ENDIF
!
SEND.REVERSE.END:
	EP.SEND.REVERSE = sendAnswer%
END FUNCTION
!    
SUB EP.REPRINT.VOUCHER(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
        EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$)PUBLIC  
! tpv application interface data
!
  STRING EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
    EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$
!
!
  EP.REPRINT% = -1
  EP.STATE% = 0
  EP.EXCEPTION$ = ""
  EP.CAJERO$      = RIGHT$(STRING$(10,"0") + UNPACK$(TS.OPER$),10)
  EP.ECR.DATETIME$ = DATE$ + TIME$
  IF EP.APPROV.FOUND% AND EP.VOUCHER.FOUND% THEN \
  BEGIN 
    IF EP.ECR.FUNCTION$ = "10" OR   \
       EP.ECR.FUNCTION$ = "11" OR   \
       EP.ECR.FUNCTION$ = "12" OR   \
       EP.ECR.FUNCTION$ = "13" OR   \
       EP.ECR.FUNCTION$ = "55" OR \
       EP.ECR.FUNCTION$ = "56" THEN \
    BEGIN 
      EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) + \
        RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.ECR.TRANSNUM$ + \
        EP.DE.INVOICE.NBR$ + EP.DE.FECHA.PROC$ + "00" +                          \
        EP.DE.AUTH$ + EP.DE.CARD.BIN$ + EP.DE.CARD$ + EP.DE.FECHA.VENC$ +        \
        EP.DE.FECHA.PROC$ + EP.DE.RRN$ + EP.DE.CUOTAS$ +                         \
        "nombre cliente                "+ EP.DE.AMOUNT$ +                        \ 
        EP.DE.IVA$ + EP.DE.IVA.BASE$ + EP.DE.PROPINA$ + EP.DE.CASHBACK$
      EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
      EP.MESSAGE$ = EP.APPL$ + EP.ECR.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
      CALL EP.SEND.TO.THREADER
    ENDIF 
    WHILE (EP.STATE% < 4) AND (EP.EXCEPTION$ = "")
      EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
      IF EP.M.LEN% < 8 THEN \
      BEGIN
        EP.TRX.STATUS$ = "0"
        EP.AMJ.STATUS$ = "1"
        EXIT SUB
      ENDIF \
      ELSE \
      BEGIN
        EP.AMJ.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
      ENDIF 
!      IF EP.AMJ.STATUS$ = " " THEN EP.AMJ.STATUS$ = "2"   ! OJO ELIMINAR
      IF EP.AMJ.STATUS$ = "0" OR   \
         EP.AMJ.STATUS$ = "1" THEN \
      BEGIN
        EXIT SUB
      ENDIF
 !     
      IF EP.AMJ.STATUS$ = "2" THEN \        ! Transaccion con respuesta de host concluida
      BEGIN
        IF EP.M.LEN% < 9 THEN \
        BEGIN
          EP.TRX.STATUS$ = "1" 
          EXIT SUB
        ENDIF
        EP.TRX.STATUS$  = MID$(EP.AMJ.MESSAGE$,9,1)
        EP.RESP.FOUND%  = -1
        CALL EP.PARSE.THREADER.RESPONSE
        EP.APPROV.CODE$ = EP.H.APPROV.CODE$
        IF EP.H.APPROV.DESC$ = STRING$(20," ") THEN \
          EP.APPROV.DESC$ = "CODIGO RESPUESTA=" + EP.APPROV.CODE$ \
        ELSE \
          EP.APPROV.DESC$ = EP.H.APPROV.DESC$
        IF EP.TRX.STATUS$ <> "0" THEN \
        BEGIN
          EP.STATE% = 4
          EXIT SUB        
        ENDIF
        IF EP.APPROV.CODE$ = "00" THEN \
        BEGIN 
          EP.STATE% = 2
          IF EP.APPROV.DESC$ = STRING$(20," ") THEN \ 
            CALL TRANSLATE.ISO.CODE(EP.APPROV.CODE$,EP.APPROV.DESC$)
          CALL EP.DISPLAY.A.MESSAGE(EP.APPROV.DESC$)
          EP.APPROV.FOUND% = -1
        ENDIF \
        ELSE  \
        BEGIN
          EP.STATE% = 4
          EXIT SUB        
        ENDIF
      ENDIF \
      ELSE  \                          !  Otros estados de transaccion 
      BEGIN
        IF EP.AMJ.STATUS$ = "3" THEN \
        BEGIN
          CALL EP.PARSE.DISPLAY.REQUEST
          CALL EP.DISPLAY.A.MESSAGE(EP.DISP.MESSAGE$)
        ENDIF   
        IF EP.AMJ.STATUS$ = "4" THEN \
        BEGIN
          CALL EP.PARSE.PRT.HEADER
          CALL EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
          EP.PRT.CUT$ = "0"
          CALL EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
          EP.LAST.POINT2% = EP.POINTER2%
        ENDIF   
        IF EP.AMJ.STATUS$ = "5" THEN \
        BEGIN
          CALL EP.PARSE.PRT.LINE
          IF EP.PRT.VOUC$ = "1" THEN \
            CALL EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
          IF EP.PRT.CUST$ = "1" THEN \
            CALL EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
        ENDIF   
        IF EP.AMJ.STATUS$ = "6" THEN \       !  Transacccion finalizada satisfactoriamente
        BEGIN
          CALL EP.PARSE.PRT.CLOSE
          CALL EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
          EP.AMJ.STATUS$ = "2"               !  Sale como transaccion con respuesta del host
          EP.STATE% = 4                      !   Fin de ciclo  
        ENDIF   
      ENDIF
      IF  (EP.STATE% < 4) AND (EP.EXCEPTION$ = "") THEN \   ! Si se continua el ciclo
      BEGIN    
        EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) + \
          RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.ECR.TRANSNUM$ + \
          EP.DE.INVOICE.NBR$ + EP.DE.FECHA.PROC$ + EP.APPL.STATUS$
          EP.MSGLEN$  = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
          EP.MESSAGE$ = EP.APPL$ + EP.ECR.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
!      call ep.line.print("cont="+str$(len(ep.message$)),4100H)    
        CALL EP.SEND.TO.THREADER
      ENDIF
    WEND
  ENDIF 
!
END SUB
!
!

SUB EP.SELECT.VOUCHER.CRO(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$) PUBLIC
!
  STRING EP.ERRFX$
!
! tpv application interface data
!
  STRING EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
    EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$
!
! DATA INITIALIZATION
!
  EP.FECHA.INIC$  = DATE$ + TIME$
  EP.AMJ.STATUS$  = "0"
  EP.TRX.STATUS$  = "1"
  EP.COMERCIO$    = ""
  EP.EPAY.TERMINAL$=""
  EP.CUOTAS.QTY$  = ""
  EP.FECHA.POSTEO$= ""
  EP.FECHA.PROC$  = ""
  EP.COD.PROC$    = ""
  EP.CENTRAL.CHEQUE$ = ""
  EP.TIPO.ID$     = ""
  EP.ID.GIRADOR$  = ""
  EP.TEL.GIRADOR$ = ""
  EP.COD.BANCO$   = ""
  EP.CTA.CTE$     = ""
  EP.NRO.CHEQUE$  = ""
  EP.RRN$         = ""
  EP.FRANQUICIA$  = ""
  EP.BANCO$       = ""
  EP.PRODUCTO$    = ""
  EP.USER.DATA$   = ""
  EP.GOOD.END%    = 0
  EP.STATE%       = 1
  EP.EXCEPTION$   = ""
  EP.MSR.DATA$    = ""
  EP.KEYB.DATA$   = ""
  EP.REPRINT% = -1
!
  CALL EP.SAVE.KEYS
  EP.KEYB.DATA$ = ""
  CALL EP.GET.KBDATA(EP.GUIDE.MESSAGE$(27),"1" , "4", \
    EP.KEYB.DATA$)
!
  IF TS.IO.MOTORKEY = 73 THEN \
  BEGIN
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "K"
    CALL EP.RESTORE.KEYS
    EXIT SUB
  ENDIF
!
  IF EP.KEYB.DATA$ = "1" THEN \
  BEGIN
    EP.ECR.FUNCTION$ = "10" 
    EP.FUNCTION$     = "02"
    EP.REV.FUNCTION$ = "06"
    EP.ANUL.FUNCTION$ = "04"
  ENDIF \
  ELSE \
  IF EP.KEYB.DATA$ = "2" THEN \
  BEGIN
    EP.ECR.FUNCTION$ = "11" 
    EP.FUNCTION$     = "03" 
    EP.REV.FUNCTION$ = "07"
    EP.ANUL.FUNCTION$ = "05"
  ENDIF \
  ELSE \  
  IF EP.KEYB.DATA$ = "3" THEN \
  BEGIN 
    EP.ECR.FUNCTION$ = "12"
    EP.FUNCTION$     = "04"
    EP.REV.FUNCTION$ = "08"
  ENDIF \
  ELSE \  
  IF EP.KEYB.DATA$ = "4" THEN \
  BEGIN
    EP.ECR.FUNCTION$ = "13"
    EP.FUNCTION$     = "05"
    EP.REV.FUNCTION$ = "09"
  ENDIF \
  ELSE \
  BEGIN
    EP.AMJ.STATUS$   = "2"
    EP.TRX.STATUS$   = "K"
    CALL EP.RESTORE.KEYS
    EXIT SUB
  ENDIF
!
  EP.KEYB.DATA$ = ""
  CALL EP.GET.KBDATA(EP.GUIDE.MESSAGE$(23),"1" , "999999", \
  EP.KEYB.DATA$)
  IF TS.IO.MOTORKEY = 73 THEN \
  BEGIN
    EP.AMJ.STATUS$   = "2"
    EP.TRX.STATUS$   = "K"
    CALL EP.RESTORE.KEYS
    EXIT SUB
  ENDIF
  EP.ECR.TRANSNUM$ = RIGHT$(STRING$(6,"0") + EP.KEYB.DATA$ ,6)
!
  CALL EP.RESTORE.KEYS

  EP.DE1102.FOUND% = 0
  EP.DE1103.FOUND% = 0
  EP.DE1203.FOUND% = 0
  EP.APPROV.FOUND% = 0
  EP.EFT.FOUND% = 0
  TS.ER.RETURN = -1
  OPEN "R::EFTTRX" KEYED RECL 158 AS EP.IOPARM% NOWRITE NODEL
  IF NOT TS.ER.RETURN  THEN \   ! si no abre
  BEGIN 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "V"
    EXIT SUB
  ENDIF
!
  EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) +   \
    EP.APPL$ + EP.FUNCTION$ + EP.ECR.TRANSNUM$ + "01"
  TS.ER.RETURN = -1 
  READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%         \
    KEY EP.KEY$ ;                                     \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,  \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$,         \
    EP.C$, EP.A$
  IF NOT TS.ER.RETURN THEN \  ! si no existe
  BEGIN 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "O"
    CLOSE EP.IOPARM%
    EXIT SUB
  ENDIF \
  ELSE  \
  BEGIN
    EP.DE.CARD$    = MID$(EP.DE2.DATA$,13,4)
    EP.DE.ALLCARD$   = LEFT$(EP.DE2.DATA$ + STRING$(20,"0"),20)
    EP.DE.INVOICE.NBR$ = LEFT$(MID$(EP.DE6.DATA$,11,6) + STRING$(6,"0"),6)
  ENDIF
!    

  EP.KEY$ = RIGHT$(STRING$(4,"0") + TS.TERMINAL$,4) +   \
    EP.APPL$ + EP.FUNCTION$ + EP.ECR.TRANSNUM$ + "02"
  EP.SAVE.KEY$ = EP.KEY$
  TS.ER.RETURN = -1 
  READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
  IF NOT TS.ER.RETURN THEN \  ! si no existe
  BEGIN  
    EP.AMJ.STATUS$ = "2"
     EP.TRX.STATUS$ = "O"
     CLOSE EP.IOPARM%
     EXIT SUB
  ENDIF
!----------------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "si abre key=" +  EP.KEY$+":" 
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF 
!----------------------------------------------------------------
  EP.DE1102.FOUND% = -1
  EP.DE.ISOCOD$  = MID$(EP.DE2.DATA$,19,2)
  EP.DE.POSTEO$  = LEFT$(EP.DE3.DATA$,4)
  EP.DE.CODPRO$  = MID$(EP.DE3.DATA$,5,6)
!  EP.DE.CARD$    = MID$(EP.DE3.DATA$,11,4)
  EP.DE.AUTH$    = MID$(EP.DE3.DATA$,15,6)
  EP.DE.AMOUNT$  = LEFT$(EP.DE4.DATA$,10)
  EP.DE.PROPINA$ = "00" + MID$(EP.DE5.DATA$,13,8)
  EP.DE.RRN$     = LEFT$(EP.DE6.DATA$,12)
  EP.DE.CUOTAS$  = MID$(EP.DE6.DATA$,13,2)  
  EP.DE.TIPOVAR$ = MID$(EP.DE6.DATA$,15,2)
  IF EP.DE.ISOCOD$ = "00" THEN  \
  BEGIN
    EP.APPROV.FOUND% = -1
  ENDIF \
  ELSE \
  BEGIN
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "T" 
    CLOSE EP.IOPARM%
    EXIT SUB
  ENDIF
  EP.KEY$ = LEFT$(EP.SAVE.KEY$,14) + "03" 
  TS.ER.RETURN = -1 
  READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
  IF NOT TS.ER.RETURN THEN \  ! si no existe
  BEGIN 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "U" 
    CLOSE EP.IOPARM%
    EXIT SUB
  ENDIF
!
  EP.DE.APPL$       = MID$(EP.DE1.DATA$,EP.LEN.CLAVE% + 1,2)
  EP.DE.TRANSNUM$   = MID$(EP.DE1.DATA$,EP.LEN.CLAVE% + 5,6) 
  EP.DE.IVA.BASE$   = LEFT$(EP.DE2.DATA$,10)
  EP.DE.CASHBACK$   = MID$(EP.DE2.DATA$,11,10)
  EP.DE.FECHA.PROC$ = LEFT$(EP.DE3.DATA$,12)
  EP.DE.CARD.BIN$   = MID$(EP.DE5.DATA$,13,6)
  EP.DE.FECHA.VENC$ = MID$(EP.DE6.DATA$,13,4) 
  EP.VOUCHER.FOUND% = -1
  EP.CAJERO$        = RIGHT$(STRING$(10,"0") + UNPACK$(TS.OPER$),10)
!       
  EP.KEY$ = RIGHT$(STRING$(4,"0") + TS.TERMINAL$,4) +    \
     EP.APPL$ + EP.REV.FUNCTION$+ EP.ECR.TRANSNUM$ + "04"

  TS.ER.RETURN = -1 
  READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
!
  IF TS.ER.RETURN THEN \           ! si existe
  BEGIN 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "U"
    CLOSE EP.IOPARM%
    EXIT SUB
  ENDIF
!
!       
  EP.KEY$ = RIGHT$(STRING$(4,"0") + TS.TERMINAL$,4) +    \
     EP.APPL$ + EP.REV.FUNCTION$ + EP.ECR.TRANSNUM$ + "05"

  TS.ER.RETURN = -1 
  READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    KEY EP.KEY$ ;                                         \
    EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
    EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
!
  IF TS.ER.RETURN THEN \                                  ! si existe
  BEGIN 
    EP.AMJ.STATUS$ = "2"
    EP.TRX.STATUS$ = "U"
    CLOSE EP.IOPARM%
    EXIT SUB
  ENDIF
!
  IF EP.ECR.FUNCTION$ = "10" OR   \ 
     EP.ECR.FUNCTION$ = "11" THEN \
  BEGIN     
    EP.KEY$ = RIGHT$(STRING$(4,"0") + TS.TERMINAL$,4) +    \
       EP.APPL$ + EP.ANUL.FUNCTION$ + EP.ECR.TRANSNUM$ + "03"

    TS.ER.RETURN = -1 
    READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
      KEY EP.KEY$ ;                                         \
      EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
      EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
!
    IF TS.ER.RETURN THEN \           ! si existe
    BEGIN 
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "L"
      CLOSE EP.IOPARM%
      EXIT SUB
    ENDIF
  ENDIF  
!
  CLOSE EP.IOPARM%
END SUB
!    
!
!
!
SUB ACCOUNT.BALANCE(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$) PUBLIC
!
  STRING EP.ERRFX$
!
! tpv application interface data
!
  STRING EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,             \
    EP.TAX.EPAY$, EP.VALOR.EFT$, EP.ECR.TRANSNUM$, EP.APPROV.CODE$,\
    EP.PROPINA$, EP.USER.DATA$, EP.AUTH.NUMBER$, EP.APPROV.DESC$, EP.RRN$,       \
    EP.CARD.NUMBER$, EP.TIPO.VAR$
!
!
END SUB
!
!-----------------------------------------------------------
!  	recover routines
!
SUB EPAY.START.RECOVER.EFT PUBLIC 
!
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "start recover="+ STR$(EP.MAX.TRX%)
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------
  EP.LINEA% = 0
  DIM EP.VOUCHER$(300)
  DIM EP.CUT.VOUCHER$(300)
  DIM EP.EFT.LINE$(100)
  DIM EP.TRX.APPL$(EP.MAX.TRX%)
  DIM EP.TRX.FUNCTION$(EP.MAX.TRX%)
  DIM EP.TRX.TRANSNUM$(EP.MAX.TRX%)
  DIM EP.TRX.RESP%(EP.MAX.TRX%)
  DIM EP.TRX.APPROV%(EP.MAX.TRX%)
  DIM EP.TRX.VOUCHER%(EP.MAX.TRX%)
  DIM EP.TRX.TOHOST1%(EP.MAX.TRX%) 
  DIM EP.TRX.TOHOST2%(EP.MAX.TRX%)
  DIM EP.TRX.ANUL%(EP.MAX.TRX%)   
  DIM EP.TRX.REVERSO%(EP.MAX.TRX%)
  DIM EP.TRX.PEND%(EP.MAX.TRX%)
  DIM EP.TRX.FECHA$(EP.MAX.TRX%)
  DIM EP.PROMO.ID$(10)
  DIM EP.PROMO.STATUS$(10)
  DIM EP.PROMO.POINTS$(10) 
  DIM EP.PROMO.REDEEM$(10)
  DIM EP.PROMO.AMTPUR$(10)
  DIM EP.PROMO.QTYPUR$(10)     
  EP.CAJERO$ = RIGHT$(STRING$(10,"0") + UNPACK$(TS.OPER$),10)
!
END SUB
!
!
SUB EPAY.RECOVER.EFT PUBLIC 
!
  IF TS.TEMP1I2 = 11 THEN \
  BEGIN 
    EP.J% = 3
    EP.B$ = SL.STR.ENTRY$ + ":"
    CALL EP.GETUNPK
    IF LEN(EP.A$) > EP.LEN.CLAVE% THEN  \   ! Valid record
    BEGIN
      IF LEFT$(EP.A$,EP.LEN.CLAVE%) = EP.EFT.CLAVE$ THEN \ ! EFT record
      BEGIN
        EP.DE.APPL$     = MID$(EP.A$,EP.LEN.CLAVE% + 1,2)
        EP.DE.FUNCTION$ = MID$(EP.A$,EP.LEN.CLAVE% + 3,2)
        EP.DE.TRANSNUM$ = MID$(EP.A$,EP.LEN.CLAVE% + 5,6)
        EP.DE.TIPO$     = MID$(EP.A$,EP.LEN.CLAVE% + 11,2)
        IF EP.DE.TIPO$ = "01" THEN    \
        BEGIN
          EP.LINEA% = EP.LINEA% + 1
          EP.REV.APPL$ = EP.DE.APPL$ 
          EP.TRX.APPL$(EP.LINEA%) = EP.DE.APPL$
          EP.TRX.TRANSNUM$(EP.LINEA%) = EP.DE.TRANSNUM$
!          EP.EPAY.TRANSNUM$ = EP.DE.TRANSNUM$ 
          EP.TRX.FUNCTION$(EP.LINEA%) = EP.DE.FUNCTION$
          CALL EP.GETUNPK
          EP.DE2.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE3.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE4.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE5.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE6.DATA$ = EP.A$
          EP.DE.ALLCARD$ = EP.DE2.DATA$
          EP.DE.CARD$   = MID$(EP.DE2.DATA$,13,4)
          EP.DE.INVOICE.NBR$ =  RIGHT$(STRING$(6,"0") + MID$(EP.DE6.DATA$,11,6),6)
          EP.TRX.FECHA$(EP.LINEA%) = LEFT$(EP.DE5.DATA$,12)
          EP.APPROV.FOUND% = 0
          EP.VOUCHER.FOUND% = 0
        ENDIF     ! end del tipo 01                        
!                      
        IF EP.DE.TIPO$ = "02" THEN    \
        BEGIN
          EP.TRX.RESP%(EP.LINEA%) = -1
          CALL EP.GETUNPK
          EP.DE2.DATA$ = EP.A$
          EP.DE.ISOCOD$ = MID$(EP.A$,19,2)
          IF EP.DE.ISOCOD$ = "00" THEN \
          BEGIN
            EP.TRX.APPROV%(EP.LINEA%) = -1
            EP.APPROV.FOUND% = -1
          ENDIF 
          CALL EP.GETUNPK
          EP.DE3.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE4.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE5.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE6.DATA$ = EP.A$
          IF EP.DE.APPL$ = "01" THEN  \
          BEGIN 
            IF EP.DE.FUNCTION$ = "11"  OR \
               EP.DE.FUNCTION$ = "12"  OR \
               EP.DE.FUNCTION$ = "14"  THEN \    ! Es transaccion de tarjeta
            BEGIN
              EP.DE1102.FOUND% = -1
              EP.DE.ISOCOD$ = MID$(EP.DE2.DATA$,19,2)
              EP.DE.POSTEO$ = LEFT$(EP.DE3.DATA$,4)
              EP.DE.CODPRO$ = MID$(EP.DE3.DATA$,5,6)
              EP.DE.CARD$   = MID$(EP.DE3.DATA$,11,4)
              EP.DE.AUTH$   = MID$(EP.DE3.DATA$,15,6)
              EP.DE.AMOUNT$ = LEFT$(EP.DE4.DATA$,10)
              EP.DE.IVA$    = MID$(EP.DE4.DATA$,11,10)
              EP.DE.PROPINA$ = "00" + MID$(EP.DE5.DATA$,13,8)
              EP.DE.RRN$    = LEFT$(EP.DE6.DATA$,12)
              EP.DE.CUOTAS$ = MID$(EP.DE6.DATA$,13,2)
              EP.DE.TIPOVAR$ = MID$(EP.DE6.DATA$,15,2) 
            ENDIF \
            ELSE \
            BEGIN   
              EP.DE.ISOCOD$     = MID$(EP.DE2.DATA$,19,2)
              EP.DE.CENTRAL.CHEQUE$ = MID$(EP.DE3.DATA$,11,1)
              EP.DE.COD.BANCO$  = MID$(EP.DE3.DATA$,13,2) + STRING$(8," ")
              EP.DE.AUTH$       = MID$(EP.DE3.DATA$,15,6)
              EP.DE.AMOUNT$     = LEFT$(EP.DE4.DATA$,10)
              EP.DE.NRO.CHEQUE$ = MID$(EP.DE4.DATA$,11,9)
              EP.DE.CTA.CTE$    = MID$(EP.DE5.DATA$,8,13)
              EP.DE.RRN$        = LEFT$(EP.DE6.DATA$,12)
              EP.DE.TIPOVAR$    = MID$(EP.DE6.DATA$,15,2)
            ENDIF
          ENDIF \
          ELSE \
          IF EP.DE.APPL$ = "08" THEN  \      ! aplicacion tarjeta CRO
          BEGIN 
            IF EP.DE.FUNCTION$ = "02"  OR \
               EP.DE.FUNCTION$ = "03"  OR \
               EP.DE.FUNCTION$ = "04"  OR \
               EP.DE.FUNCTION$ = "05"  OR \
               EP.DE.FUNCTION$ = "51"  OR \
               EP.DE.FUNCTION$ = "52"  THEN \    ! Es transaccion de tarjeta
            BEGIN
              EP.DE1102.FOUND% = -1
              EP.DE.ISOCOD$ = MID$(EP.DE2.DATA$,19,2)
              EP.DE.POSTEO$ = LEFT$(EP.DE3.DATA$,4)
              EP.DE.CODPRO$ = MID$(EP.DE3.DATA$,5,6)
              EP.DE.AUTH$   = MID$(EP.DE3.DATA$,15,6)
              EP.DE.AMOUNT$ = LEFT$(EP.DE4.DATA$,10)
              EP.DE.IVA$    = MID$(EP.DE4.DATA$,11,10)
              EP.DE.PROPINA$ = "00" + MID$(EP.DE5.DATA$,13,8)
              EP.DE.RRN$    = LEFT$(EP.DE6.DATA$,12)
              EP.DE.CUOTAS$ = MID$(EP.DE6.DATA$,13,2)
              EP.DE.TIPOVAR$ = MID$(EP.DE6.DATA$,15,2) 
            ENDIF 
          ENDIF \
          ELSE \                   ! aplicacion de fidelidad
          BEGIN
            EP.DE1102.FOUND% = -1
            EP.DE.ISOCOD$ = MID$(EP.DE2.DATA$,19,2)
            EP.DE.POSTEO$ = LEFT$(EP.DE3.DATA$,4)
            EP.DE.CODPRO$ = MID$(EP.DE3.DATA$,5,6)
            EP.DE.CARD$   = MID$(EP.DE3.DATA$,11,4)
            EP.DE.AUTH$   = MID$(EP.DE3.DATA$,15,6)
            EP.DE.AMOUNT$ = LEFT$(EP.DE4.DATA$,10)
            EP.DE.PUNTOS$    = MID$(EP.DE4.DATA$,11,10)
            EP.DE.RRN$    = LEFT$(EP.DE6.DATA$,12)
          ENDIF  
        ENDIF  ! end del tipo 02
!                                              
        IF EP.DE.TIPO$ = "03" THEN \
        BEGIN
          EP.TRX.VOUCHER%(EP.LINEA%) = -1
          EP.VOUCHER.FOUND% = -1
          CALL EP.GETUNPK
          EP.DE2.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE3.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE4.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE5.DATA$ = EP.A$
          CALL EP.GETUNPK
          EP.DE6.DATA$ = EP.A$
          IF EP.DE.APPL$ = "08" THEN \
          BEGIN 
            EP.DE.IVA.BASE$   = LEFT$(EP.DE2.DATA$,10)
            EP.DE.CASHBACK$   = MID$(EP.DE2.DATA$,11,10)
            EP.DE.FECHA.PROC$ = LEFT$(EP.DE3.DATA$,12)
            EP.DE.ANULTRANSNUM$ = LEFT$(EP.DE4.DATA$,6) 
            EP.DE.CARD.BIN$   = MID$(EP.DE5.DATA$,13,6)
            EP.DE.FECHA.VENC$ = MID$(EP.DE6.DATA$,13,4)
          ENDIF \
          ELSE \
          BEGIN
            EP.DE.FECHA.PROC$ = LEFT$(EP.DE3.DATA$,12)
            EP.DE.ANULTRANSNUM$ = LEFT$(EP.DE4.DATA$,6) 
            EP.DE.CARD.BIN$   = MID$(EP.DE5.DATA$,13,6)
            EP.DE.FECHA.VENC$ = MID$(EP.DE6.DATA$,13,4)
          ENDIF
          IF (EP.DE.APPL$ = "01" AND EP.DE.FUNCTION$ = "12")  OR \   !  anula en la misma transaccion
             (EP.DE.APPL$ <> "01" AND \
             (EP.DE.FUNCTION$ = "04"  OR EP.DE.FUNCTION$ = "05"))  THEN \ !  anula en la misma transaccion
          BEGIN
            EP.EFT.FOUND% = 0
            EP.POS% = 1
            WHILE NOT EP.EFT.FOUND%  AND EP.POS% <= EP.MAX.TRX% 
              IF EP.TRX.TRANSNUM$(EP.POS%) = EP.DE.ANULTRANSNUM$ THEN \
              BEGIN
                EP.EFT.FOUND% = -1
              ENDIF \
              ELSE \
              BEGIN
                EP.POS% = EP.POS% + 1
              ENDIF
            WEND
            IF EP.EFT.FOUND% THEN \
            BEGIN
              EP.TRX.ANUL%(EP.POS%) = -1 
            ENDIF
          ENDIF   ! end anula en la misma trx
        ENDIF    ! end del tipo 03
!    AQUI VA EL CALL A REPRINT             
        IF EP.APPROV.FOUND% AND EP.VOUCHER.FOUND% AND \
             NOT TS.RETV.IN.PROGRESS THEN \
        BEGIN  
          EP.FUNCTION$ = "00"
          IF EP.DE.APPL$ = "01" THEN \
          BEGIN 
            IF EP.DE.FUNCTION$ = "11"  OR \
               EP.DE.FUNCTION$ = "12"  OR \
               EP.DE.FUNCTION$ = "14"  THEN \    ! Es transaccion de tarjeta
              EP.FUNCTION$ = "19" \
            ELSE \
              EP.FUNCTION$ = "20"
            EP.TAX.EPAY$       = EP.DE.IVA$
  !            EP.TAX.EFT$        = EP.DE.IVA$
            EP.IVA.BASE$       = EP.DE.IVA.BASE$
            EP.PROP.EPAY$      = EP.DE.PROPINA$
            EP.CASHBACK$       = EP.DE.CASHBACK$
          ENDIF \
          ELSE  \
          BEGIN 
            IF EP.DE.FUNCTION$ = "02"  THEN \
              EP.FUNCTION$ = "10" \
            ELSE \
            IF EP.DE.FUNCTION$ = "03"  THEN \
              EP.FUNCTION$ = "11" \
            ELSE \
            IF EP.DE.FUNCTION$ = "04"  THEN \
              EP.FUNCTION$ = "12" \
            ELSE \
            IF EP.DE.FUNCTION$ = "05"  THEN \    ! Es transaccion de tarjeta
              EP.FUNCTION$ = "13" \
            ELSE \  
            IF EP.DE.FUNCTION$ = "51"  THEN \    ! Es transaccion de avance tarjeta
              EP.FUNCTION$ = "55" \
            ELSE \  
            IF EP.DE.FUNCTION$ = "52"  THEN \    ! Es transaccion de anulacion avanca tarjeta
              EP.FUNCTION$ = "56" 
          ENDIF
          EP.CAJERO$         = RIGHT$(STRING$(10,"0") + UNPACK$(TS.OPER$),10)
          EP.EFT.TRX%        = -1
          EP.EFT.DETAIL%     = -1
          EP.EFT.SMA%        = -1
          EP.DE.TRX.STATUS$  = ""
          EP.DE.AMJ.STATUS$  = ""
          EP.DE.APPROV.CODE$ = ""
          EP.DE.APPROV.DESC$ = ""
          EP.DE.USER.DATA$   = ""  
          IF EP.DE.APPL$ = "08" THEN \
           CALL EP.REPRINT.VOUCHER(EP.DE.APPL$, EP.FUNCTION$, EP.DE.TRX.STATUS$, EP.DE.AMJ.STATUS$, \
               EP.DE.TRANSNUM$, EP.DE.APPROV.CODE$, EP.DE.APPROV.DESC$, EP.DE.USER.DATA$)  
                
        ENDIF   ! end del reprint
!                          
        IF EP.DE.TIPO$ = "04"  OR \       ! respuesta TO de appl manager
           EP.DE.TIPO$ = "05" THEN \      ! reversos
        BEGIN
          EP.EFT.FOUND% = 0
          EP.POS% = 1
          WHILE NOT EP.EFT.FOUND%  AND EP.POS% <= EP.MAX.TRX% 
            IF EP.TRX.TRANSNUM$(EP.POS%) = "" THEN \
            BEGIN
              EP.LINEA% = EP.POS%
              EP.TRX.TRANSNUM$(EP.POS%) = EP.DE.TRANSNUM$
              EP.EFT.FOUND% = -1
            ENDIF \
            ELSE \
            IF EP.TRX.TRANSNUM$(EP.POS%) = EP.DE.TRANSNUM$ THEN \
            BEGIN
              EP.EFT.FOUND% = -1
            ENDIF \
            ELSE  \
            BEGIN
              EP.POS% = EP.POS% + 1
            ENDIF
          WEND
          IF EP.EFT.FOUND% THEN \    ! reverso found
          BEGIN 
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            IF MID$(EP.A$,17,1) = "0" THEN \       !  finaliza normalmente
            BEGIN    
              EP.TRX.REVERSO%(EP.POS%) = 0
              IF EP.DE.TIPO$ = "04" THEN \
              BEGIN 
                EP.TRX.TOHOST1%(EP.POS%) = -1
              ENDIF \
              ELSE \
              BEGIN
                EP.TRX.TOHOST2%(EP.POS%) = -1
              ENDIF
            ENDIF \
            ELSE  \
            BEGIN
              EP.TRX.REVERSO%(EP.POS%) = -1
            ENDIF
          ENDIF                               ! end reverso found                                                 
        ENDIF                                 ! end tipo 04 o 05         
      ENDIF                                   ! end EFT record    
    ENDIF                                     ! end valid record
  ENDIF
  IF TS.TEMP1I2 = 05  OR \
     TS.TEMP1I2 = 06  THEN \
  BEGIN 
    IF EP.EFT.SMA% THEN \
      EP.EFT.SMA% = 0
  ENDIF
END SUB
!
!
SUB EPAY.END.RECOVER.EFT PUBLIC
!
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "end recover="+ STR$(EP.LINEA%)
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------

  DIM EP.TRX.PEND%(EP.MAX.TRX%) 
  FOR EP.M% = 1 TO EP.LINEA% 
    IF EP.TRX.TRANSNUM$(EP.M%) <> "" AND  \    ! si hubo transaccion 
       NOT EP.TRX.TOHOST1%(EP.M%)    AND  \    ! si no hay reverso appl manager
       NOT EP.TRX.TOHOST2%(EP.M%)    THEN \    ! si no hay reverso host
    BEGIN
      IF EP.TRX.RESP%(EP.M%) THEN  \           ! si obtuvo respuesta
      BEGIN
        IF EP.TRX.APPROV%(EP.M%) THEN \        ! si fue aprobada
        BEGIN 
          IF NOT EP.TRX.VOUCHER%(EP.M%) THEN \ ! si no obtuvo voucher
          BEGIN
            EP.TRX.PEND%(EP.M%) = -1           ! se marca como pendiente
            EP.TRX.PEND$ = EP.TRX.TRANSNUM$(EP.M%)
            EP.FECHA.PEND$ = EP.TRX.FECHA$(EP.M%)
            IF EP.DE.APPL$ = "01"  THEN \
              EP.REV.FUNCTION$ = "15" \
            ELSE \
            BEGIN
              IF EP.TRX.FUNCTION$(EP.M%) = "02"  THEN  \        ! deteccion de necesidad de reversos 
                EP.REV.FUNCTION$ = "06"     \        ! 
              ELSE \
              IF EP.TRX.FUNCTION$(EP.M%) = "03"  THEN  \        ! 
                EP.REV.FUNCTION$ = "07"     \        ! 
              ELSE \  
              IF EP.TRX.FUNCTION$(EP.M%) = "04" THEN   \        ! 
                EP.REV.FUNCTION$ = "08"     \        ! 
              ELSE \
              IF EP.TRX.FUNCTION$(EP.M%) = "05" THEN   \        ! 
                EP.REV.FUNCTION$ = "09"              ! 
            ENDIF 
          ENDIF  
        ENDIF  
      ENDIF \
      ELSE \                                   ! si no obtuvo respuesta                                  
      BEGIN 
        EP.TRX.PEND%(EP.M%) = -1
        EP.TRX.PEND$ = EP.TRX.TRANSNUM$(EP.M%)
        EP.FECHA.PEND$ = EP.TRX.FECHA$(EP.M%)
        IF EP.DE.APPL$ = "01"  THEN \
          EP.REV.FUNCTION$ = "13" \
        ELSE \
        BEGIN
          IF EP.TRX.FUNCTION$(EP.M%) = "02"  THEN  \        ! deteccion de necesidad de reversos 
            EP.REV.FUNCTION$ = "06"     \        ! 
          ELSE \
          IF EP.TRX.FUNCTION$(EP.M%) = "03"  THEN  \        ! 
            EP.REV.FUNCTION$ = "07"     \        ! 
          ELSE \  
          IF EP.TRX.FUNCTION$(EP.M%) = "04" THEN   \        ! 
            EP.REV.FUNCTION$ = "08"     \        ! 
          ELSE \
          IF EP.TRX.FUNCTION$(EP.M%) = "05" THEN   \        ! 
            EP.REV.FUNCTION$ = "09"              ! 
        ENDIF 
      ENDIF
    ENDIF
  NEXT EP.M% 
  !
  IF EP.TRX.TRANSNUM$(EP.LINEA%) <> "" AND  \    ! si ULTIMA TRANSACCION 
     NOT EP.TRX.TOHOST1%(EP.LINEA%)  AND  \    ! si no hay reverso appl manager
     NOT EP.TRX.TOHOST2%(EP.LINEA%)  THEN \    ! si no hay reverso host
  BEGIN
    IF EP.DE.APPL$ = "01" THEN \
    BEGIN
      IF EP.TRX.RESP%(EP.LINEA%)    AND \          ! si obtuvo respuesta
         EP.TRX.APPROV%(EP.LINEA%)  AND \          ! si fue aprobada
         EP.TRX.VOUCHER%(EP.LINEA%) AND \          ! si obtuvo voucher
         EP.EFT.SMA% THEN \                        ! no ha entrado a Supermarket Application
      BEGIN
        CALL EP.SAVE.KEYS
        DIM TS.IO.KEYS(10)                         ! Clear input
        IF EP.DE.FUNCTION$ = "12" OR \
           EP.DE.FUNCTION$ = "14" THEN  \
          TS.IO.KEYS(1) = 70
        TS.IO.KEYS(3)  = 78                        ! Asterisk key
        TS.IO.DATA$(3) = RIGHT$(EP.DE.TIPOVAR$,1)  ! Tender variety
        TS.IO.KEYS(7)  = VAL(LEFT$(EP.DE.TIPOVAR$,1)) + 90 ! Tender key
        TS.IO.DATA$(7) = EP.DE.AMOUNT$             ! Tender amount
        TS.IO.DATA$(9) = EP.DE.CARD$ + EP.DE.AUTH$ + \
            EP.TRX.TRANSNUM$(EP.LINEA%)
        TS.IO.KEYS(9)  = 90
  !      IF SL.TE.TENDTYPE = 3 THEN \ Food stamps so
  !        TS.TRX.STATUS = -1 ! set F.S. bal due
        CALL TSTDEC01
        CALL EP.RESTORE.KEYS
        TS.TEMP1$ = "TRX CONTABILIZADA!!!"
        TS.TEMP2$ = STRING$(20," ") 
        TS.LINETYPE = 12
        CALL TSCSEC08
      ENDIF
      IF EP.TRX.RESP%(EP.LINEA%)    AND \          ! si obtuvo respuesta
         EP.TRX.APPROV%(EP.LINEA%)  AND \          ! si fue aprobada
         EP.TRX.VOUCHER%(EP.LINEA%) AND \          ! si obtuvo voucher
         EP.EFT.SMA% THEN \                        ! no ha entrado a Supermarket Application
      BEGIN 
        EP.POINTER2%  = EP.LAST.POINT2% - 1
        EP.EFT.FOUND% = 0
        EP.F% = 0
        EP.M% = EP.POINTER1% 
        WHILE NOT EP.EFT.FOUND% AND EP.M% > 0
          EP.POS% = MATCH("Firma:", EP.VOUCHER$(EP.M%),1)
          IF EP.POS% <> 0 THEN \
            EP.F% = EP.M%
          EP.VOUCHER$(EP.M%) = "anul" + MID$(EP.VOUCHER$(EP.M%),5,\
            LEN(EP.VOUCHER$(EP.M%)) - 4) 
          EP.M% = EP.M% - 1
          EP.POS% = MATCH(LEFT$(EP.GUIDE.MESSAGE$(18),20),EP.VOUCHER$(EP.M%),1) 
          IF EP.POS% <> 0 THEN \
            EP.EFT.FOUND% = -1
        WEND
        IF EP.F% <> 0 THEN \
        BEGIN
          EP.VOUCHER$(EP.F% - 1) = "ESTA TRANSACCION NO SE REALIZO CORREC-"
          EP.VOUCHER$(EP.F%)     = "TAMENTE POR TAL RAZÓN ESTE VOUCHER NO "
          EP.VOUCHER$(EP.F% + 1) = "ES VALIDO COMO SOPORTE DE UNA TRANSAC-"
          EP.VOUCHER$(EP.F% + 2) = "CIÓN FINANCIERA, POR FAVOR REALICE    "
          EP.VOUCHER$(EP.F% + 3) = "NUEVAMENTE UNA TRANSACCIÓN. ESTA TRAN-"
          EP.VOUCHER$(EP.F% + 4) = "SACCION NO SERA CARGADA AL TARJETAHA- "
          EP.VOUCHER$(EP.F% + 5) = "BIENTE, POR FAVOR CONSERVE ESTE RECIBO"
          EP.VOUCHER$(EP.F% + 6) = "PARA CUALQUIER RECLAMACIÓN            "
        ENDIF
        EP.TRX.PEND%(EP.LINEA%) = -1
        EP.TRX.PEND$ = EP.TRX.TRANSNUM$(EP.LINEA%)
        EP.FECHA.PEND$ = EP.TRX.FECHA$(EP.LINEA%)
        EP.REV.FUNCTION$ = "15" 
      ENDIF
    ENDIF \   ! end aplicacion tef
    ELSE \    ! applicaciones diferentes a tef
    BEGIN
      IF EP.TRX.RESP%(EP.LINEA%)    AND \          ! si obtuvo respuesta
         EP.TRX.APPROV%(EP.LINEA%)  AND \          ! si fue aprobada
         EP.TRX.VOUCHER%(EP.LINEA%) THEN \         ! si obtuvo voucher
      BEGIN
        IF EP.DE.FUNCTION$ = "02" THEN \
        BEGIN 
          EP.I4% = VAL(EP.DE.AMOUNT$)
          EP.I1% = FORMAT.AMOUNT(EP.I4%)
          CALL EP.LINE.PRINT("Abono a Saldo  :Vlr abono ="+ \
            RIGHT$(STRING$(10," ") + TS.TEMP1$, 10), 6100H)
        ENDIF \
        ELSE  \
        IF EP.DE.FUNCTION$ = "03" THEN \
        BEGIN 
          EP.I4% = VAL(EP.DE.PUNTOS$)
          EP.I1% = FORMAT.AMOUNT(EP.I4%)
          CALL EP.LINE.PRINT("Compra con TCRO:Vlr compra="+ \
            RIGHT$(STRING$(10," ") + TS.TEMP1$, 10), 6100H)
        ENDIF \
        ELSE  \
        IF EP.DE.FUNCTION$ = "04" THEN \
        BEGIN 
          EP.I4% = VAL(EP.DE.AMOUNT$) * -1
          EP.I1% = FORMAT.AMOUNT(EP.I4%)
          CALL EP.LINE.PRINT("Anulacion abono:Vlr abono ="+ \
            RIGHT$(STRING$(10," ") + TS.TEMP1$, 10), 6100H)
        ENDIF \
        ELSE  \
        IF EP.DE.FUNCTION$ = "05" THEN \
        BEGIN 
          EP.I4% = VAL(EP.DE.PUNTOS$) * -1
          EP.I1% = FORMAT.AMOUNT(EP.I4%)
          CALL EP.LINE.PRINT("Anula compra   :Vlr compra="+ \
            RIGHT$(STRING$(10," ") + TS.TEMP1$, 10), 6100H)
        ENDIF       
        CALL EP.LINE.PRINT("Voucher="+EP.DE.TRANSNUM$ + "  Auth="+EP.DE.AUTH$,6200H)

      ENDIF
    ENDIF
  ENDIF  
END SUB
!
FUNCTION INGRESO.CLAVE PUBLIC
  INTEGER*2 UE.I%
  INTEGER*1 UE.FOUND%, INGRESO.CLAVE
!  
PEDIR.CLAVE:
  INGRESO.CLAVE = 0
  EP.KEYB.DATA$ = ""
  CALL EP.GET.AUTH("Digite clave Autori?", "1", "99999999", \
  EP.KEYB.DATA$)
!
  IF TS.IO.MOTORKEY = 73 THEN \
  BEGIN
    CALL EP.RESTORE.KEYS
    EXIT FUNCTION          
  ENDIF
!
  IF MID$(TS.IO.HDR$,11,1) = "0"  THEN \       \ No manager's key
  BEGIN 
    CALL EP.DISPLAY.AN.ERROR("FALTA LLAVE DE SUPERVISOR !!!!!")
    GOTO PEDIR.CLAVE
  ENDIF
  IF LEN(TS.IO.HDR$) > 11 THEN \
    TS.IO.HDR$ = LEFT$(TS.IO.HDR$,10) + "0" + \                ! se suprime llave de supervisor
                 MID$(TS.IO.HDR$,12,LEN(TS.IO.HDR$) - 11) \
  ELSE \
    TS.IO.HDR$ = LEFT$(TS.IO.HDR$,10) + "0" 
    
!  
  UE.FOUND% = 0
  UE.I% = 1
  WHILE UE.I% <= 20 AND NOT UE.FOUND%
    IF VAL(EP.KEYB.DATA$) = TO.OVRIDNUM(UE.I%) THEN \
      UE.FOUND% = -1 \
    ELSE \
      UE.I% = UE.I% + 1
  WEND 
  IF UE.FOUND% THEN \
    INGRESO.CLAVE = -1 \
  ELSE \
  BEGIN 
    CALL EP.DISPLAY.AN.ERROR("CLAVE INVALIDA !!!!!")
    GOTO PEDIR.CLAVE
  ENDIF
END FUNCTION
!
!
!--------------------------------------------------------------
! Retorna una string alineando con ceros a la izquierda un
! valor numerico
!--------------------------------------------------------------
Function formatLeftZeroes(intValue%,zeroes%) Public
    Integer*4 intValue%
    Integer*1 zeroes%
    String formatLeftZeroes
    If Len(Str$(intValue%)) <= zeroes% Then \
        formatLeftZeroes = Right$(String$(zeroes%,"0") + Str$(intValue%),zeroes%) \
    Else \
        formatLeftZeroes = Str$(intValue%)
End Function
!
Function getFormattedActualDate Public
	String getFormattedActualDate,actualYear$,formattedMonth$,formattedDay$,separator$,mesesDesc$
	Integer*2 actualMonth%,actualDay%
	!
	mesesDesc$ = "EneFebMarAbrMayJunJulAgoSepOctNovDic"
	separator$ = "/"
	!
	actualMonth% = Int%(Val(mid$(date$,3,2)))
	actualDay% = Int%(Val(mid$(date$,5,2)))
	actualYear$ = "20" + mid$(date$,1,2)
	!
	formattedMonth$ = mid$(mesesDesc$,actualMonth%*3 - 2,3)
	formattedDay$ = formatLeftZeroes(actualDay%,2)
	!
	getFormattedActualDate = 			\
		formattedMonth$ + separator$ +	\
		formattedDay$ + separator$ +	\
		actualYear$
End Function
!
Function getStoreLine Public
  String getStoreLine
  getStoreLine =                                                             \
    getFormattedActualDate											 + " " + \! date
    mid$(time$,1,2) + ":" + mid$(time$,3,2)                          + " " + \! time
    formatLeftZeroes(Int%(Val(ts.store$)),4)                         + " " + \! store number
    formatLeftZeroes(Int%(Val(ts.terminal$)),2)                      + " " + \! terminal number
    formatLeftZeroes(sl.hd.transnum + 1,4)                           + " " + \! trx number
    Str$(Val(unpack$(ts.oper$)))                                              ! operator
End Function
!
!
!
Function replaceVariables(lineDesc$, alignMode%)
  integer*1 endFound%, alignMode%
  integer*2  charPos%, jInt2%, kInt2%, fieldLen%, lineLen%, fieldIni%, letraInt2%, \
             realFieldLen%
  string lineDesc$, letraStr$, replaceString$, realFieldValue$, workLine$, variableLabel$
  string replaceVariables
  !
  ! Dependiendo de la alineación deseada, cambia el identificador de variable
  If alignMode% = 2 Then 			\	! Alineación al centro
  Begin
  	variableLabel$ = gv.inicioCampoVarM$
	EndIf \
	Else If alignMode% = 1 Then \	! Alineación a la derecha
	Begin
		variableLabel$ = gv.inicioCampoVarR$
	EndIf Else \									! Alineación a la izquierda
	Begin
		variableLabel$ = gv.inicioCampoVar$
	EndIf
!
!
!  significado de las letras para los bonos y los vouchers
!
! 0 = logo 10      asc(48) 
! 1 = logo 1 
! 2 = logo 2
! 3 = logo 3
! 4 = logo 4
! 5 = logo 5
! 6 = logo 6
! 7 = logo 7
! 8 = logo 8
! 9 = logo 9
! : = sin definir
! ; = sin definir
! < = sin definir
! = = sin definir
! > = sin definir
! ? = sin definir
! @ = INDICADOR DE INICIO DE CAMPO  gv.inicioCampoVar$
! A = nombre de la tienda
! B = identificacion de comercio
! C = identificacion del cajero
! D = nombre del cajero
! E = identificacion de cliente
! F = numero de tarjeta de cliente
! G = segmento cliente
! H = nombre tarjeta habiente
! I = tipo de cuenta tarjeta cliente
! J = valor total compra  
! K = valor productos vendidos
! L = valor descuentos a producto
! M = valor descuentos financieros 
! N = valor base compra
! O = valor IVA total
! P = valor medios de pago      asc(80)
! Q = acumulado compras total  
! R = acumulado puntos total 
! S = acumulado compras periodo  
! T = acumulado puntos periodo 
! U = acumulado puntos redimidos total
! V = acumulado puntos redimidos periodo
! W = additional I4 user exit field 1     gv.I4UserField1%  
! X = additional I4 user exit field 2     gv.I4UserField2% 
! Y = additional I4 user exit field 3     gv.I4UserField3% 
! Z = additional I4 user exit field 4     gv.I4UserField4% 
! [ = sin definir
! \ = sin definir
! ] = sin definir
! ^ = sin definir
! - = sin definir
! . = reservado                       asc(96)
! a = private card account number     asc(97)
! b = private card tender amount
! c = balance due
! d = change amount
! e = tender verification status
! f = manager’s override indicator
! g = store data print line (h + i + j + k + l + m)
! h = date     
! i = time
! j = transaction number
! k = terminal number
! l = store number
! m = fiscal transaction number
! n = user field 1 integer*2 ts.i2.user1
! o = user field 2 integer*2 ts.i2.user2
! p = user field 3 integer*4 ts.i4.user3  asc(112)
! q = user field 4 integer*4 ts.i4.user4
! r = reference number
! s = signature
! t = tender type
! u = tender descriptor
! v = user Field 5 string
! w = User Field 6 string
! x = User Field 7 string
! y = Store Address 1
! z = Store Address 2
! { = sin definir
! | = sin defini
! } = sin definir                    asc(125)
!
!   carga algunos datos de la cadena
!   
  !ts.sname$ = TO.HEADERLINE2$
  ep.fecha.proc$ = date$
  gv.tipoVarTarjPriv$ = "41" 
  gv.tarjetaCliente$ = ue.clf.cliente$
!
!
  charPos% = 1
  jInt2%   = 1
  workLine$ = lineDesc$
  lineLen% = len(lineDesc$) 
  while charPos% <> 0 and jInt2% < lineLen%
    charPos% = match(variableLabel$,workLine$,jInt2%)
    if charPos% = 0  or \
       charPos% >= len(workLine$) then \
      goto FIN.LETRAS 
!
    fieldLen% = 1
    letraStr$ = mid$(workLine$,charPos% + 1, 1)
    While mid$(workLine$, charPos% + fieldLen%, 1) = letraStr$
        fieldLen% = fieldLen% + 1
    Wend
!
    if len(letraStr$) > 0 then \
      letraInt2% = asc(letraStr$) \
    else \
    begin
      letraInt2% = 0
      goto AGAIN
    endif  
!
    replaceString$ = "not found"          !      Default value 
!    
    if letraInt2% >= 48 and \
       letraInt2% <= 63 then \    ! numeros
    begin 
      letraInt2% = letraInt2% - 47 
      on letraInt2% gosub n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,n2p,npyc,nlt,neq,ngt,nqst
    endif \
    else  \
    if letraInt2% >= 65 and \
       letraInt2% <= 95 then \    ! mayusculas
    begin
      letraInt2% = letraInt2% - 64 
      on letraInt2% gosub UA,UB,UC,UD,UE,UF,UG,UH,UI,UJ,UK,UL,UM,UN,UO,UP,UQ,UR,US,UT,UU,UV,UW,UX,UY,UZ,\
                   USB,UBS,URSB,UGRR,UMNS
    endif \
    else  \
    if letraInt2% >= 97 and \
       letraInt2% <= 125 then \   ! minusculas
    begin
      letraInt2% = letraInt2% - 96
      on letraInt2% gosub La,Lb,Lc,Ld,LLe,Lf,Lg,Lh,Li,Lj,Lk,LL,Lm,Ln,Lo,Lp,Lq,Lr,Ls,LLt,Lu,Lv,Lw,Lx,Ly,Lz,\
                   Lcrci,Lpipe,Lcrcd 
    endif \
    else  \
    begin
      goto AGAIN
    endif  
!
!    to.userexit(20) = 0
!    call ep.line.print("letra="  + letraStr$, 4100H) 
!    call ep.line.print("accion=" + str$(letraInt2%), 4100H) 
!    call ep.line.print("rep="    + replaceString$, 4100H) 
!    to.userexit(20) = -1
!    
!
    if fieldLen% <= 2 then \
        realFieldLen% = Len(replaceString$) \
    else \
        realFieldLen% = fieldLen%
 !        
!    realFieldValue$ = left$(replaceString$ + String$(realFieldLen%," "),realFieldLen%)
!    workLine$ = left$(workLine$,charPos% - 1) + \
!        realFieldValue$ + \
!        mid$(workLine$,charPos% + fieldLen%, len(workLine$) - charPos% - fieldLen% + 1)
!
  ! Aplica la alineación deseada
  If alignMode% = 2 Then 			\	! Alineación al centro
  Begin
  	realFieldValue$ = right$(String$(realFieldLen%/2," ")+ replaceString$ ,realFieldLen%/2)
  	realFieldValue$ = left$(realFieldValue$ + String$(realFieldLen% - realFieldLen%/2," ") ,realFieldLen% - realFieldLen%/2)
	EndIf \
	Else If alignMode% = 1 Then \	! Alineación a la derecha
	Begin
		realFieldValue$ = right$(String$(realFieldLen%," ")+ replaceString$ ,realFieldLen%)
	EndIf Else \									! Alineación a la izquierda
	Begin
		realFieldValue$ = left$(replaceString$ + String$(realFieldLen%," "),realFieldLen%)
	EndIf
	!
    workLine$ = left$(workLine$,charPos% - 1) + \
        realFieldValue$ + \
        mid$(workLine$,charPos% + FieldLen%, len(workLine$) - charPos% - FieldLen% + 1)
!
!    call ep.line.print("rep="+replaceString$, 4100h) 
!
AGAIN:        
    jInt2% = charPos% + 1  
  wend
  goto FIN.LETRAS  
! 
!    numeros
!                  
n0:                !0 = logo 10                              
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(10)                                
        return                                                                   
n1:                !1 = logo 1                            
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(1)                               
        return                                                                   
n2:                !2 = logo 2                            
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(2)                               
        return                                                                   
n3:                !3 = logo 3                            
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(3)                               
        return                                                                   
n4:                !4 = logo 4                            
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(4)                               
        return                                                                   
n5:                !5 = logo 5                           
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(5)                               
        return                                                                   
n6:                !6 = logo 6 
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(6)             
        return                                                                   
n7:                !7 = logo 7                                              
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(7)             
        return                                                                   
n8:                !8 = logo 8                                              
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(8)             
        return                                                                   
n9:                !9 = logo 9                                              
          replaceString$ = CHR$(29) + CHR$(47) + CHR$(0) + CHR$(9)             
        return                                                                   
n2p:               !: = sin definir                                              
          replaceString$ =  ":"                                               
        return                                                                   
npyc:              !; = sin definir                                              
          replaceString$ =  ";"                                               
        return                                                                   
nlt:               !< = sin definir                                              
          replaceString$ =  "<"                                               
        return                                                                   
neq:               != = sin definir                                              
          replaceString$ =  "="                                               
        return                                                                   
ngt:               !> = sin definir                                              
          replaceString$ =  ">"                                               
        return                                                                   
nqst:              !? = sin definir                                              
          replaceString$ =  "?"                                               
        return                                                                   
!                                                                                
!  mayusculas                                                                    
!                                                                                
UA:                !A = nombre de la tienda                                      
          !replaceString$ = rtrim$(ts.sname$)
          replaceString$ = rtrim$(FISSUC$)
        return                                                                   
UB:                !B = identificacion de comercio                               
          replaceString$ = gv.idComercio$                                      
        return                                                                   
UC:                !C = identificacion del cajero                               
          replaceString$ = str$(val(unpack$(ts.oper$)))                                 
        return                                                                   
UD:                !D = nombre del cajero                                               
          if gv.nombreCajero$ = "" then \
            call getOperatorName$
          replaceString$ = rtrim$(gv.nombreCajero$)                                 
        return                                                                   
UE:                !E = identificacion de cliente                                            
          replaceString$ = gv.idCliente$                          
        return                                                                   
UF:                !F = numero de tarjeta de cliente                                       
          replaceString$ = gv.tarjetaCliente$                                      
        return                                                                   
UG:                !G = segmento cliente                                        
          replaceString$ = gv.segmentoCliente$                  
        return                                                                   
UH:                !H = nombre tarjeta habiente                                  
          replaceString$ = rtrim$(gv.nombreCliente$)                                     
        return                                                                   
UI:                !I = tipo de cuenta tarjeta cliente                                   
          replaceString$ = gv.tipoCuenta$                                     
        return                                                                   
UJ:                !J = valor total compra                                      
          ep.i4% = ts.totals(0,0,0) + iva.tdes% + totalDesctoPago%
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$
        return                                                                   
UK:                !K = valor productos vendidos   
          ep.i4% = gv.acumVentasTrx%                                             
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)             
          replaceString$ = ts.temp1$              
        return                                                                   
UL:                !L = valor descuentos a producto                                     
          ep.i4% = ts.disc.save(0,0)                                             
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$
        return                                                                   
UM:                !M = valor descuentos financieros                                         
          ep.i4% = ts.disc.save(0,0)                                             
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UN:                !N = valor base compra                                        
          ep.i4% = MEX.VAT.TOTAL(0)                                             
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UO:                !O = valor IVA total                                               
          ep.i4% = TS.TOTALS(0,0,0)    \                        ! Jun 29/06 Tot Compra
                   - IC%                    \                   ! Suma del Imp Consumo
                   - (NFF.EXCLU%-IVA.TDES%-totalDesctoPago%) \  ! Total Exento 0xIVATDES%
                   - MEX.VAT.TOTAL(0)                                             
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UP:                !P = valor medios de pago                                       
          ep.i4% = ts.tendered(0)                                           
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UQ:                !Q = acumulado compras total                                  
          ep.i4% = gv.acumComprasTotal%                                             
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UR:                !R = acumulado puntos total                                            
          ep.i4% = gv.acumPuntosTotal%                                                   
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
US:                !S = acumulado compras periodo                                            
          ep.i4% = gv.acumComprasPeriodo%                                            
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UT:                !T = acumulado puntos periodo                                           
          ep.i4% = gv.acumPuntosPeriodo%                                             
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UU:                !U = acumulado puntos redimidos total                              
          ep.i4% = gv.acumPuntosRedimTotal%  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UV:                !V = total puntos redimidos periodo                                            
          ep.i4% = gv.acumPuntosRedimPeriodo%  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UW:                !W = additional I4 user exit field 1                                        
          ep.i4% = gv.I4UserField1%  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UX:                !X = additional I4 user exit field 2                             
          ep.i4% = gv.I4UserField2%  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UY:                !Y = additional I4 user exit field 3                             
          ep.i4% = gv.I4UserField3%  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
UZ:                !Z = additional I4 user exit field 4                                              
          ep.i4% = gv.I4UserField4%  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
USB:               ![ = sin definir                                              
          replaceString$ = "["                                                
        return                                                                   
UBS:               !\ = sin definir                                              
          replaceString$ = "\"                                                
        return                                                                   
URSB:              !] = sin definir                                              
          replaceString$ = "]"                                                
        return                                                                   
UGRR:              !^ = sin definir                                              
          replaceString$ = "^"                                                
        return                                                                   
UMNS:              !- = sin definir                                              
          replaceString$ = "-"                                                
        return                                                                   
!                                                                                
!    minusculas                                                                  
!                                                                                
La:                !a = private card account number                                           
          replaceString$ = ts.acnum$                                         
        return                                                                   
Lb:                !b = valor pagado con tarjeta PRIVADA                                            
          ep.i4% = EP.BUSCA.MONTO.TV(gv.tipoVarTarjPriv$)  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)             
          replaceString$ = ts.temp1$
        return                                                                   
Lc:                !c = balance due                                              
          ep.i4% = ts.baldue(0)                                                  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
Ld:                !d = change amount                                            
          ep.i4% = ts.baldue(1)                                                  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
Lle:                !e = tender verification status                              
          replaceString$ = str$(sl.te.status)                                
        return                                                                   
Lf:                !f = manager’s override indicator                             
          replaceString$ = unpack$(sl.override$)                             
        return                                                                   
Lg:                !g = store data print line (h + i + j + k + l + m)                             
!          replaceString$ = "20"+mid$(date$,1,2) + "/" + \
!          mid$(date$,3,2) + "/" + \                                   
!          mid$(date$,5,2) + " " + \                                            
!          mid$(time$,1,2) + ":" + \
!          mid$(time$,3,2)+"."+mid$(time$,5,2) + " " + \
!          right$(string$(9," ") + str$(sl.hd.transnum + 1), 9) + " " + \            
!          ts.terminal$ + " " + ts.store$ + " " + unpack$(ts.oper$)                                               
           replaceString$ = getStoreLine
        return                                                                   
Lh:                !h = date                                                     
          replaceString$ = "20"+mid$(unpack$(sl.hd.datetime$),1,2) + "/" + \
          mid$(unpack$(sl.hd.datetime$),3,2) + "/" + \                                   
          mid$(unpack$(sl.hd.datetime$),5,2)                                             
        return                                                                   
Li:                !i = time                                                     
          replaceString$ = mid$(unpack$(sl.hd.datetime$),7,2) + ":" + \
            mid$(unpack$(sl.hd.datetime$),9,2)  !+"."+mid$(unpack$(sl.hd.datetime$),11,2)                                            
        return                                                                   
Lj:                !j = transaction number                                       
          ep.i4% = sl.hd.transnum + 1                                            
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
Lk:                !k = terminal number                                          
        replaceString$ = ts.terminal$                                        
        return                                                                   
Ll:                !l = store number                                             
        replaceString$ = ts.store$                                           
        return                                                                   
Lm:                !m = fiscal transaction number                                          
        replaceString$ = N.TRX$                                   
        return                                                                   
Ln:                !n = user field 1                                             
          ep.i4% = ts.i2.user1                                                  
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
Lo:                !o = user field 2                                             
          ep.i4% = ts.i2.user2                                                   
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
Lp:                !p = user field 3                                             
          ep.i4% = ts.i4.user3                                                   
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
Lq:                !q = user field 4                                             
          ep.i4% = ts.i4.user4                                                   
          ep.i1% = format.amount(ep.i4%)                                         
          !replaceString$ = right$(string$(11," ") + ts.temp1$, 11)
          replaceString$ = ts.temp1$             
        return                                                                   
Lr:                !r = reference number                                         
          replaceString$ = ts.ref.number$                                    
        return                                                                   
Ls:                !s = signature                                                
          replaceString$ = ts.signature$                                     
        return                                                                   
Llt:                !t = tender type                                             
          replaceString$ = ts.tend.type$                                     
        return                                                                   
Lu:                !u = tender descriptor                                        
          replaceString$ = ts.tend.descrip$                                  
        return                                                                   
Lv:                !v = user field 5                                             
          replaceString$ = ts.user5$                                         
        return                                                                   
Lw:                !w = user field 6                                             
          replaceString$ = ts.user6$                                         
        return                                                                   
Lx:                !x = user field 7                                             
          replaceString$ = ts.user7$                                         
        return                                                                   
Ly:                !y = store address 1                                          
          replaceString$ = ts.saddr1$                                        
        return                                                                   
Lz:                !z = store address 2                                          
          replaceString$ = ts.saddr2$                                        
        return                                                                   
Lcrci:                !{ = sin definir                                           
          replaceString$ = "{"                                                
        return                                                                   
Lpipe:                !| = sin definir                                           
          replaceString$ = "|"                                                
        return                                                                   
Lcrcd:                !} = sin definir                                           
          replaceString$ = "}"                                                
        return                                                                   
                                                                                 
! 
FIN.LETRAS:
!
!  call ep.line.print(workLine$, 4100H) 
  replaceVariables = workLine$
!
end function
!
Function replaceFields(lineDesc$) Public
	String lineDesc$,replaceFields,fullAnswer$
	! Reemplazo de variables con alineación a la izquierda
	fullAnswer$ = replaceVariables(lineDesc$, 0)
	! Reemplazo de variables con alineación a la derecha
	fullAnswer$ = replaceVariables(fullAnswer$, 1)
	! Reemplazo de variables con alineación al centro
	fullAnswer$ = replaceVariables(fullAnswer$, 2)	
	!
	replaceFields = fullAnswer$
End Function
!
SUB EP.PRINT.LINEAIND.REQUEST PUBLIC
	String lineToPrint$,printerParam$,journalParam$,formatParam$
	Integer*2 paramToPrint%
	Integer*1 tmpUE20%,tmpUE60%
	!
	tmpUE20% = TO.USEREXIT(20)
	tmpUE60% = TO.USEREXIT(60)
	TO.USEREXIT(20) = 0
	TO.USEREXIT(60) = 0
	!
	paramToPrint% = 100H
	!
	EP.APPL.STATUS$   = ""
	!
	IF EP.M.LEN% >= 52 THEN \
	BEGIN
		EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
		lineToPrint$ = replaceFields(MID$(EP.AMJ.MESSAGE$,12,38))
		printerParam$ = MID$(EP.AMJ.MESSAGE$,50,1)
		journalParam$ = MID$(EP.AMJ.MESSAGE$,51,1)
		formatParam$ = MID$(EP.AMJ.MESSAGE$,52,1)
		IF printerParam$ NE "0" THEN \
			paramToPrint% = paramToPrint% XOR 16384
		IF journalParam$ NE "0" THEN \
			paramToPrint% = paramToPrint% XOR 8192
!		IF formatParam$ = "@" THEN \	! Código de barras
!			CALL printCodigoBarra(lineToPrint$) \
!		ELSE \
!			CALL EP.LINE.PRINT(lineToPrint$,paramToPrint%)
		!
		If formatParam$ = "N" Then Begin
			Write #34; Chr$(1bh) + Chr$(47h) + Chr$(01h) ! Negrilla
		Endif
		!
		CALL EP.LINE.PRINT(lineToPrint$,paramToPrint%)
		!
		If formatParam$ = "C" Then Begin
			! Corte de papel
			CALL EP.SAVE.PRINT
			TS.LINETYPE = 18 
			TS.LINEDATA = 0
			CALL TSPREC01
			CALL EP.RESTORE.PRINT
		Endif Else If formatParam$ = "N" Then Begin
			Write #34; Chr$(1bh) + Chr$(47h) + Chr$(00h) ! Cancela Negrilla
		Endif
	ENDIF
	TO.USEREXIT(20) = tmpUE20%
	TO.USEREXIT(60) = tmpUE60%
END SUB
!
!*********************************************************************
!                         (UEEPAY01.TPV)
!                           (Ver 1.00)
!
! This user performs: 
! - Print E-payment bouchers 
!*********************************************************************

SUB UEAPPL01 PUBLIC
!
!
  IF EP.EFT.ACTIVO% THEN \     ! user option 6 = Y
  BEGIN             
    CALL EPAY.RESET.VOUCHER
    EP.EFT.TRX% = 0
    TO.XCHGLIM = EP.XCHGLIM%
    EP.EFT.DETAIL% = 0
  !  CALL EP.LINE.PRINT("Paso por 1",4100H)
  ENDIF
  EP.TRIES.COUNT% = 0
     
END SUB
!
!*********************************************************************
!                         (UEEPAY02.TPV)
!                           (Ver 1.00)
!
! This user performs: 
! - Print E-payment bouchers 
!*********************************************************************
!
SUB UEAPPL02 PUBLIC
  IF (EP.EFT.ACTIVO% AND \
     EP.EFT.TRX% AND EP.POINTER1% > 0) OR EP.ENVIO.RECAUDO.TCRO% THEN \
  BEGIN
    CALL PRINT.EFT.HEADER       
    CALL EPAY.VOUCHER
    CALL PRINT.EFT.HEADER
    EP.INTERCHG% = -1
    CALL EPAY.VOUCHER
    EP.trxRecaudoT% = 0
    EP.ENVIO.RECAUDO.TCRO% = 0
  ENDIF
  EP.TRIES.COUNT% = 0
  Call printFranqRes
  EP.EFT.TRX% = 0
  !
  Call restoreVerifType
END SUB
!
!
!*********************************************************************
!                         (UEAPPL04.TPV)
!                           (Ver 1.00)
!
! This user performs: 
! - Print E-payment Vouchers 
!*********************************************************************
!
SUB UEAPPL04 PUBLIC
  IF EP.EFT.ACTIVO% THEN \ !   eft activo
  BEGIN             
    CALL EPAY.RESET.VOUCHER
    EP.EFT.TRX% = 0  
    EP.EFT.DETAIL% = 0
    TO.XCHGLIM = EP.XCHGLIM% 
  ENDIF
END SUB
!
!
!*********************************************************************
!                         (UEAPPL05.TPV)
!                           (Ver 2.00)
!
! This user performs: 
! - Print E-payment bouchers 
!*********************************************************************
!
SUB UEAPPL05 PUBLIC
  CALL EP.LINE.PRINT("EP.EFT.ACTIVO%="+str$(EP.EFT.ACTIVO%),2100H)
  CALL EP.LINE.PRINT("APPROVAL CODE:"+EP.APPROV.CODE$,2100H)
  CALL EP.LINE.PRINT("EP.EFT.TRX%="+str$(EP.EFT.TRX%),2100H)
  CALL EP.LINE.PRINT("TS.PROCEDURE="+str$(TS.PROCEDURE),2100H)
  
!  IF EP.EFT.ACTIVO% THEN \                ! user option 6 = Y
!  BEGIN                 
!  	! SE QUITA LA CONDICION TS.PROCEDURE = 2
!    IF EP.EFT.TRX%     THEN \
!    BEGIN 
!      CALL PRINT.EFT.HEADER 
!      CALL EPAY.VOUCHER
!      CALL INTERCHG.VOUCHER
!    ENDIF
!    
!  ENDIF

  IF EP.EFT.ACTIVO% THEN \
  BEGIN
    ! SE QUITA LA CONDICION TS.PROCEDURE = 1
    IF EP.EFT.TRX%     THEN \
    BEGIN 
      CALL PRINT.EFT.HEADER       
      CALL EPAY.VOUCHER
      CALL PRINT.EFT.HEADER
      EP.INTERCHG% = -1
      CALL EPAY.VOUCHER
    ENDIF
  ENDIF
  
  EP.TRIES.COUNT% = 0
  Call printFranqRes
  EP.EFT.TRX% = 0
  Call restoreVerifType
END SUB
!
!
FUNCTION applManagerFunction0(msgToSend$,resetVoucher%,pNoMsg%) PUBLIC
	String applManagerFunction0,msgToSend$,finalAnswer$,requestAnswer$
	Integer*1 resetVoucher%,pNoMsg%,printVoucher%,userPreviousValue%
	!
	EP.applMgr.running% = -1
	!
	DIM EP.APPLMGR.DATA$(300)
	!
	EP.MESSAGE$ = msgToSend$
	requestAnswer$ = ""
	finalAnswer$ = ""
	printVoucher% = 0
	If resetVoucher% Then \
		CALL EPAY.RESET.VOUCHER
	!
	EP.STATE% = 1
	EP.EXCEPTION$ = ""
	!
	CALL EP.SEND.TO.THREADER
	WHILE (EP.STATE% < 4) AND (EP.EXCEPTION$ = "")
		EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
		IF EP.M.LEN% < 8 THEN \
		BEGIN
			EP.TRX.STATUS$ = "0"
			EP.AMJ.STATUS$ = "1"
			GOTO CancelarFuncion
		ENDIF 
		EP.AMJ.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
		IF EP.AMJ.STATUS$ = "0" OR   \
			EP.AMJ.STATUS$ = "1" THEN \
		BEGIN
			GOTO CancelarFuncion
		ENDIF
		IF EP.AMJ.STATUS$ = "2" THEN \        ! Transaccion con respuesta de host concluida
		BEGIN
			IF EP.M.LEN% < 11 THEN \
			BEGIN
				EP.TRX.STATUS$ = "1" 
				GOTO CancelarFuncion
			ENDIF
			EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
			EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
			IF EP.TRX.STATUS$ <> "0" THEN \
			BEGIN
				EP.STATE% = 4
				! Si viene mensaje lo despliega
				! De lo contrario, interpreta el código de respuesta
				If pNoMsg% = 0 Then Begin
					If EP.M.LEN% >= 31 Then Begin
						Call EP.DISPLAY.AN.ERROR(Mid$(EP.AMJ.MESSAGE$,12,40))
					Endif Else Begin
						CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
						CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
					Endif
				Endif
				GOTO CancelarFuncion
			ENDIF \
			ELSE \
			BEGIN
				finalAnswer$ = MID$(EP.AMJ.MESSAGE$,12,LEN(EP.AMJ.MESSAGE$) - 11)
				IF EP.M.LEN% >= 39 And pNoMsg% = 0 THEN \
				BEGIN
					EP.APPROV.CODE$ = MID$(EP.AMJ.MESSAGE$,18,2)
					EP.APPROV.DESC$ = MID$(EP.AMJ.MESSAGE$,20,20)
					IF EP.APPROV.CODE$ = "00" THEN  \
					BEGIN
						EP.STATE% = 2
						CALL EP.DISPLAY.A.MESSAGE(EP.APPROV.DESC$)
					ENDIF \
					ELSE IF EP.APPROV.CODE$ NE "  " THEN \
					BEGIN
						CALL EP.DISPLAY.AN.ERROR(EP.APPROV.DESC$)
						EP.STATE% = 4
						!GOTO CancelarFuncion
					ENDIF
				ENDIF
			ENDIF
		ENDIF \
		ELSE  \                          !  Otros estados de transaccion 
		BEGIN
			IF EP.AMJ.STATUS$ = "3" THEN \
			BEGIN
				CALL EP.PARSE.DISPLAY.REQUEST
				IF EP.DISP.PARAM$ = "1" THEN \
					CALL EP.DISPLAY.AN.ERROR(EP.DISP.MESSAGE$) \
				ELSE \
					CALL EP.DISPLAY.A.MESSAGE(EP.DISP.MESSAGE$)
			ENDIF   
			IF EP.AMJ.STATUS$ = "4" THEN \
			BEGIN
				CALL EP.PARSE.PRT.HEADER
				CALL EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
				EP.PRT.CUT$ = "0"
				CALL EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
			ENDIF   
			IF EP.AMJ.STATUS$ = "5" THEN \
			BEGIN
				CALL EP.PARSE.PRT.LINE
				IF EP.PRT.VOUC$ = "1" THEN \
					CALL EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
				IF EP.PRT.CUST$ = "1" THEN \
					CALL EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
			ENDIF   
			IF EP.AMJ.STATUS$ = "6" THEN \       !  Transacccion finalizada satisfactoriamente
			BEGIN
				printVoucher% = -1
				CALL EP.PARSE.PRT.CLOSE
				CALL EP.STORE.VOUCHER(EP.PRT.MESSAGE$,EP.PRT.CUT$)
				EP.AMJ.STATUS$ = "2"               !  Sale como transaccion con respuesta del host
				EP.STATE% = 4                      !   Fin de ciclo  
			ENDIF   
			IF EP.AMJ.STATUS$ = "7" THEN \
			BEGIN 
				CALL EP.PARSE.DATA.REQUEST
				CALL EP.SAVE.KEYS
				EP.KEYB.DATA$ = ""
				userPreviousValue% = TO.USEREXIT(14)
				TO.USEREXIT(14) = 0
				CALL EP.GET.KBDATA(EP.DISP.MESSAGE$, EP.INI.RANGE$, EP.END.RANGE$, \
					EP.KEYB.DATA$)
				TO.USEREXIT(14) = userPreviousValue%
				CALL EP.RESTORE.KEYS
			ENDIF
			IF EP.AMJ.STATUS$ = "8" THEN \
			BEGIN
				CALL EP.PARSE.DENTRY.REQUEST
				EP.INICIO.FOUND% = -1
				CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
					EP.DE5.DATA$, EP.DE6.DATA$)
			ENDIF   
			IF EP.AMJ.STATUS$ = "9" THEN \
			BEGIN
				CALL EP.PARSE.DENTRY.REQUEST
				CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
					EP.DE5.DATA$, EP.DE6.DATA$)
				EP.STATE% = 4                       !   Fin de ciclo  
				IF EP.TRX.STATUS$ = "0" THEN  \
				BEGIN
					EP.TOHOST.FOUND% = -1
				ENDIF  
			ENDIF   
			IF EP.AMJ.STATUS$ = "A" THEN \
			BEGIN
				CALL EP.PARSE.DENTRY.REQUEST
				EP.TRX.STATUS$ = MID$(EP.DE6.DATA$,17,1)
				CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
					EP.DE5.DATA$, EP.DE6.DATA$)
				EP.STATE% = 4                       !   Fin de ciclo  
				IF EP.TRX.STATUS$ = "0" THEN  \
				BEGIN
					EP.TOHOST.FOUND% = -1
				ENDIF  
			ENDIF
			IF EP.AMJ.STATUS$ = "B" THEN \
			BEGIN
				CALL EP.PARSE.DENTRY99.REQUEST
				CALL EP.ADD.DATA.ENTRY99(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$ , EP.DE3.DATA$, \
					EP.DE4.DATA$, EP.DE5.DATA$ , EP.DE6.DATA$)
			ENDIF
			IF EP.AMJ.STATUS$ = "E" THEN \
			BEGIN
				CALL EP.PARSE.DATA.REQUEST
				requestAnswer$ = EP.GET.DATA(EP.DISP.MESSAGE$)
			ENDIF
			IF EP.AMJ.STATUS$ = "F" THEN \
			BEGIN
				CALL EP.PARSE.READNVRAM.REQUEST
				requestAnswer$ = EP.READ.NVRAM(EP.nvramPointer%,EP.nvramFormat$)
			ENDIF						
			IF EP.AMJ.STATUS$ = "G" THEN \
			BEGIN
				CALL EP.PARSE.WRITENVRAM.REQUEST
				CALL EP.WRITE.NVRAM(EP.nvramMessage$,EP.nvramPointer%,EP.nvramFormat$)
			ENDIF
			IF EP.AMJ.STATUS$ = "H" THEN \
			BEGIN
				CALL EP.PARSE.PARTIALANSWER.REQUEST
			ENDIF
			IF EP.AMJ.STATUS$ = "I" THEN \
			BEGIN
				CALL EP.PRINT.LINEAIND.REQUEST
			ENDIF
			IF EP.AMJ.STATUS$ = "Z" THEN \
			BEGIN
				Call EP.PARSE.DATA.REGULAR
				Call EP.ADD.DATA.REGULAR.UE(EP.DATA.REGULAR$)
			ENDIF

		ENDIF
		IF EP.APPL.STATUS$ = "99" THEN \	! El applmgr termina el ciclo
		BEGIN
			EP.STATE% = 4
			! Si no ha recibido una respuesta la saca de acá
			! siempre y cuando el trx.status sea 0
			IF finalAnswer$ = "" AND MID$(EP.AMJ.MESSAGE$,9,1) = "0" AND LEN(EP.AMJ.MESSAGE$) > 11 THEN \
				finalAnswer$ = MID$(EP.AMJ.MESSAGE$,12,LEN(EP.AMJ.MESSAGE$) - 11)
		ENDIF
		IF  (EP.STATE% < 4) AND (EP.EXCEPTION$ = "") THEN \   ! Si se continua el ciclo
		BEGIN
			! Le cambia el estado según la respuesta recibida
			EP.MESSAGE$ = MID$(msgToSend$,8,48) + EP.APPL.STATUS$
			IF EP.AMJ.STATUS$ = "7" THEN \
				EP.MESSAGE$ = EP.MESSAGE$ + EP.KEYB.DATA$ \
			ELSE IF EP.AMJ.STATUS$ = "E" THEN \
				EP.MESSAGE$ = EP.MESSAGE$ + requestAnswer$
			EP.MSGLEN$  = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
			EP.MESSAGE$ = LEFT$(msgToSend$,4) + EP.MSGLEN$ + EP.MESSAGE$
			CALL EP.SEND.TO.THREADER
		ENDIF
	WEND
	GOTO FinalizarFuncion
CancelarFuncion:
	finalAnswer$ = ""
FinalizarFuncion:
	EP.APPL.STATUS$ = "00"
	EP.STATE% = 1
! 	IF printVoucher% THEN \
! 	BEGIN
!		CALL PRINT.EFT.HEADER.CUT 
!		CALL EPAY.VOUCHER
! 	ENDIF
	EP.DE.AMJ.STATUS$ = EP.AMJ.STATUS$
	EP.DE.TRX.STATUS$ = EP.TRX.STATUS$
	!
	EP.applMgr.running% = 0
	!
	applManagerFunction0 = finalAnswer$
END FUNCTION
!
FUNCTION applManagerFunction(msgToSend$,resetVoucher%) PUBLIC
	String applManagerFunction,msgToSend$,finalAnswer$,requestAnswer$
	Integer*1 resetVoucher%,printVoucher%,userPreviousValue%
	!
	applManagerFunction = applManagerFunction0(msgToSend$,resetVoucher%,0)
End Function
!
Function EP.invocarApplManager1(epAppl$,epFunction$,parteVariable$,usaConsecutivo%, pNoMsg%, pIncrem%) Public
	String EP.invocarApplManager1,epAppl$,epFunction$,parteVariable$,epCajero$,epInvoiceNbr$,consecutivo$
	Integer*1 usaConsecutivo%, pNoMsg%
	Integer*2 pIncrem%
	String tmpDatetimeBak$, tmpStatusBak$, tmpAmjStatusBak$
	Integer*1 tmpStateBak%
	!
	epCajero$		= RIGHT$(STRING$(10,"0") + UNPACK$(TS.OPER$),10)
	epInvoiceNbr$	= RIGHT$(STRING$(6,"0")+STR$(SL.HD.TRANSNUM+pIncrem%),6)
	!
	!-----------------------------------------------------------------
	! 2018-05-21 jsv
	! Se almacena valor original de ciertas variables claves
	!-----------------------------------------------------------------
	tmpDatetimeBak$ = EP.ECR.DATETIME$
	tmpStatusBak$ = EP.APPL.STATUS$
	tmpAmjStatusBak$ = EP.AMJ.STATUS$
	tmpStateBak% = EP.STATE%
	!-----------------------------------------------------------------
	!
	IF usaConsecutivo% Then \
	Begin
		EP.SAVE.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
		EP.EPAY.TRANSNUM$ = EP.NEW.TRANSNUM
		EP.ECR.TRANSNUM$  = EP.EPAY.TRANSNUM$  !  returns transaction number used
		consecutivo$ = EP.EPAY.TRANSNUM$
	Endif Else \
		consecutivo$ = STRING$(6,"0")
	!
	EP.ECR.DATETIME$ = DATE$ + TIME$
	EP.MESSAGE$ = EP.TERMINAL$ 	+ \ !
		epCajero$ + consecutivo$ + epInvoiceNbr$ 		+ \ !
		EP.ECR.DATETIME$ + "00"							+ \	! Parte fija
		parteVariable$										! Parte variable
	!
	EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
	EP.MESSAGE$ = epAppl$ + epFunction$ + EP.MSGLEN$ + EP.MESSAGE$
	!
	EP.invocarApplManager1 = applManagerFunction0(EP.MESSAGE$,0,pNoMsg%)
	!
	!-----------------------------------------------------------------
	! 2018-05-21 jsv
	! Se restituye valor original de ciertas variables claves
	!-----------------------------------------------------------------
	EP.ECR.DATETIME$ = tmpDatetimeBak$
	EP.APPL.STATUS$ = tmpStatusBak$
	EP.AMJ.STATUS$ = tmpAmjStatusBak$
	EP.STATE% = tmpStateBak%
	!-----------------------------------------------------------------
End Function
!
Function EP.invocarApplManager0(epAppl$,epFunction$,parteVariable$,usaConsecutivo%, pNoMsg%) Public
	String EP.invocarApplManager0,epAppl$,epFunction$,parteVariable$
	Integer*1 usaConsecutivo%, pNoMsg%
	!
	EP.invocarApplManager0 = EP.invocarApplManager1(epAppl$,epFunction$,parteVariable$,usaConsecutivo%, pNoMsg%, 1)
End Function
!
FUNCTION EP.invocarApplManager(epAppl$,epFunction$,parteVariable$,usaConsecutivo%) PUBLIC
	String EP.invocarApplManager,epFunction$,parteVariable$,epAppl$,epCajero$,epInvoiceNbr$,consecutivo$
	Integer*1 usaConsecutivo%
	!
	EP.invocarApplManager = EP.invocarApplManager0(epAppl$,epFunction$,parteVariable$,usaConsecutivo%, 0)
END FUNCTION
!
SUB UEAPPL06A PUBLIC
	String applAnswer$,msgToSend$
	IF TS.PROCEDURE = 26 THEN \	! Invocación de non-sales menu
	BEGIN     
		EP.EFT.TRX% = 0  
		EP.EFT.DETAIL% = 0
		TO.XCHGLIM = EP.XCHGLIM% 
		TS.USER.RETURN = -1
		EP.APPL$ = "00"  
		EP.ECR.FUNCTION$ = "01"
		EP.TRX.STATUS$ = "0"
		EP.AMJ.STATUS$ = "0"
		EP.USER.DATA$ = ""
		!
		EP.CAJERO$        = RIGHT$(STRING$(10,"0") + UNPACK$(TS.OPER$),10)
		EP.ECR.DATETIME$  = DATE$ + TIME$
		EP.EPAY.TRANSNUM$ = EP.NEW.TRANSNUM
		!
		msgToSend$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) + \
			RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.EPAY.TRANSNUM$ + \
			STRING$(6,"0") + EP.ECR.DATETIME$ + "00"
		msgToSend$ = EP.APPL$ + EP.ECR.FUNCTION$ + \
			RIGHT$(STRING$(3,"0") + STR$(LEN(msgToSend$)),3) + \
			msgToSend$
		applAnswer$ = applManagerFunction(msgToSend$,0)
		!
		IF LEN(applAnswer$) >= 4 THEN \
		BEGIN
			EP.APPL$ = LEFT$(applAnswer$,2)
			EP.ECR.FUNCTION$ = MID$(applAnswer$,3,2)
			! Invoca el applmanager con la aplicacion y funcion recibidas
			EP.ECR.DATETIME$  = DATE$ + TIME$
			EP.EPAY.TRANSNUM$ = EP.NEW.TRANSNUM
			msgToSend$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) + \
				RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.EPAY.TRANSNUM$ + \
				STRING$(6,"0") + EP.ECR.DATETIME$ + "00"
			msgToSend$ = EP.APPL$ + EP.ECR.FUNCTION$ + \
				RIGHT$(STRING$(3,"0") + STR$(LEN(msgToSend$)),3) + \
				msgToSend$
			applAnswer$ = applManagerFunction(msgToSend$,-1)
		ENDIF
		TS.USER.RETURN = -1
	ENDIF Else If EP.DYKEY.PRC% > 0 And TS.PROCEDURE = EP.DYKEY.PRC% Then Begin
		If EP.DYKEY.ACT% Then Begin
			EP.DYKEY.ACT% = 0
			!Call EP.DISPLAY.AN.ERROR("Solicitud de llave  desactivada")
			Call EP.DISPLAY.AN.ERROR(EP.DYKEY.MSG2$)
		Endif Else Begin
			EP.DYKEY.ACT% = -1
			!Call EP.DISPLAY.AN.ERROR("Solicitud de llave  activada")
			Call EP.DISPLAY.AN.ERROR(EP.DYKEY.MSG1$)
		Endif
		TS.USER.RETURN = -1
	Endif
END SUB
!
!
!*********************************************************************
!                         (UEEPAY06.TPV)
!                           (Ver 1.00)
!
! This user performs: 
! - start parameters loading procedure
!*********************************************************************
!
SUB UEAPPL06 PUBLIC
!*********************************************************************
!                         (UEEPAY06.TPV)
!                           (Ver 1.00)
!
! This user performs: 
! - start parameters loading procedure
!*********************************************************************
!
  IF EP.EFT.ACTIVO% THEN \ ! user option 6 = Y
  BEGIN 
    IF TS.PROCEDURE >= 20 AND  \       !
       TS.PROCEDURE <= 25 THEN \
    BEGIN     
      IF TS.PROCEDURE <> 22 THEN \
        CALL EPAY.RESET.VOUCHER
      EP.EFT.TRX% = 0  
      EP.EFT.DETAIL% = 0
      TO.XCHGLIM = EP.XCHGLIM% 
      TS.USER.RETURN = -1
      EP.APPL$ = "01"  
      IF TS.PROCEDURE = 20 THEN \           ! Carga de parametros TEF
        EP.ECR.FUNCTION$ = "02" \
      ELSE \
      IF TS.PROCEDURE = 21 THEN \           ! Carga de working key
        EP.ECR.FUNCTION$ = "03" \
      ELSE \
      IF TS.PROCEDURE = 22 THEN \           ! Reimpresion de voucher
        EP.ECR.FUNCTION$ = "19" \
      ELSE \            
      IF TS.PROCEDURE = 23 THEN \           ! Logon en Host
        EP.ECR.FUNCTION$ = "04" \
      ELSE \
      IF TS.PROCEDURE = 24 THEN \           ! Consulta de saldo de tarjeta credito
        EP.ECR.FUNCTION$ = "18" \
      ELSE \
      IF TS.PROCEDURE = 25 THEN \           ! Solicitud de prueba de comunicacion de pinpad
        EP.ECR.FUNCTION$ = "40" 
  !          
      EP.TRX.STATUS$ = "0"
      EP.AMJ.STATUS$ = "0"
      EP.USER.DATA$ = ""
      IF EP.ECR.FUNCTION$ = "01" OR   \
         EP.ECR.FUNCTION$ = "18" OR   \
         EP.ECR.FUNCTION$ = "19" OR   \
         EP.ECR.FUNCTION$ = "20" OR   \
         EP.ECR.FUNCTION$ = "31" OR   \
         EP.ECR.FUNCTION$ = "34" THEN \
      BEGIN
        IF EP.ECR.FUNCTION$ = "18" THEN \ ! Consulta de saldo
        BEGIN 
          EP.ECR.TRANSNUM$ = STRING$(6,"0")
          CALL ACCOUNT.BALANCE(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
             EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$) 
        ENDIF \           
        ELSE \ 
        IF EP.ECR.FUNCTION$ = "19" OR   \ ! Impresion de voucher
           EP.ECR.FUNCTION$ = "20" THEN \  
        BEGIN
          EP.ECR.TRANSNUM$ = STRING$(6,"0")
          EP.ECR.FUNCTION$ = ""
          EP.APPROV.CODE$ = ""
          CALL EP.SELECT.VOUCHER.CRO(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
              EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$) 
          IF EP.ECR.FUNCTION$ = "19" OR   \
             EP.ECR.FUNCTION$ = "20" THEN \
          BEGIN
              CALL EPAY.RESET.VOUCHER
              CALL PRINT.EFT.HEADER.CUT
              CALL EP.REPRINT.VOUCHER(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
                EP.ECR.TRANSNUM$, EP.APPROV.CODE$, EP.APPROV.DESC$, EP.USER.DATA$) 
          ENDIF
        ENDIF                             ! fin de impresion de voucher
  !          
  !    evaluate communication status
  !
  !      call ep.line.print("apcode="+ep.approv.code$+" amj="+ep.amj.status$+" trx="+ep.trx.status$, 4100H)       
  !
        IF EP.AMJ.STATUS$ <> "2"  THEN \ !    evaluate HOST command reponse
        BEGIN
          CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
          CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
        ENDIF \
        ELSE \
        IF EP.TRX.STATUS$ <> "0" THEN \    !    whether communication error found
        BEGIN
          CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
          CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
        ENDIF\
        ELSE \
        IF EP.APPROV.CODE$ = "00" THEN \
        BEGIN
  !        CALL EP.DISPLAY.A.MESSAGE(EP.MESSAGE.DESC$)   !   approval OK. download transaction data
  !    
          EP.EFT.TRX% = -1
          CALL PRINT.EFT.HEADER.CUT 
          CALL EPAY.VOUCHER
        ENDIF \
        ELSE \
        BEGIN 
          CALL EP.DISPLAY.AN.ERROR(EP.APPROV.DESC$)
  !        TS.IO.MOTORKEY = 0
        ENDIF
        TS.USER.RETURN = -1
      ENDIF \
      ELSE \
    ENDIF
  ! 
  ENDIF
END SUB
!
Sub appl.enableMessageDisplay(pMsg1$, pMsg2$) Public
	String pMsg1$, pMsg2$
	!
	appl.displayMsg% = -1
	appl.displayMsg1$ = pMsg1$
	appl.displayMsg2$ = pMsg2$
End Sub
!
Sub appl.disableMessageDisplay Public
	appl.displayMsg% = 0
	appl.displayMsg1$ = ""
	appl.displayMsg2$ = ""
End Sub
!
!-------------------------------------------------------------------- UEEPAY06.TPV!-------------------------------------------------------------------- UEEPAY06.TPV
!*********************************************************************
!                         (UEEPAY07.TPV)
!                           (Ver 1.00)
!
! This user performs: 
! - Load initial operation parameters 
!*********************************************************************
!
SUB UEAPPL07 PUBLIC
  EP.VERIF.TV.IDX% = -1
  EP.IOPARM%  = 95           !! session number for parameters
  EP.ERRNCODE% = 0
  EP.XCHGLIM% = TO.XCHGLIM
  gv.inicioCampoVar$ = "["   ! Caracter que indica campo variable con alineación a la izquierda
  gv.inicioCampoVarR$= "]"   ! Caracter que indica campo variable con alineación a la derecha
  gv.inicioCampoVarM$= "|"   ! Caracter que indica campo variable con alineación al centro
  ts.sname$ = TO.HEADERLINE2$
  CALL EPAY.INITIALIZATION
  !
  Call appl.disableMessageDisplay
END SUB
!
Sub UEAPPL14 Public
	If 														\
			TS.IO.MOTORKEY >= 91 And 	\ ! Secuencia de medio de pago
			TS.IO.MOTORKEY <= 96 			\
	Then Begin
		! Si no es un medio de pago TEF, invoca rutina 
		! para restaurar la opción de verificación de medio de pago
		! que previamente ha sido modificada
		If Not RESET14.isTef Then Begin
			Call restoreVerifType
		Endif
	Endif
End Sub
!
!*********************************************************************
!                         (UEAPPL20.TPV)
!                           (Ver 1.00)
!
! This user performs: 
! - Print E-payment bouchers 
!*********************************************************************
!
SUB UEAPPL20 PUBLIC
  IF EP.EFT.ACTIVO% THEN \     !
  BEGIN    
  !  if ts.procedure = 2  then \
  !  begin
  !    TS.PRTBUF$ = RIGHT$("00"+STR$(TS.LINETYPE),2)+"="+ \
  !                 RIGHT$("  "+STR$(EP.EFT.DETAIL%),2)+"="+ \
  !                 MID$(TS.PRTBUF$, 7, 32)
  !  endif
    IF TS.LINETYPE = 6     AND  \
       TS.LINEDATA = 1     THEN \
    BEGIN
      EP.SAVE.GLINE$ = TS.PRTBUF$
    ENDIF
  !  IF EP.EFT.DETAIL%  THEN \
  !  BEGIN
  !    EP.EFT.RETR% = -1
  !    TO.USEREXIT(20) = 0
  !    CALL EP.LINE.PRINT("20PROC"+STR$(TS.PROCEDURE)+ " LINE=" + STR$(TS.LINETYPE), 4100H)
  !    CALL EP.LINE.PRINT("SMA="+STR$(EP.EFT.SMA%)+ " FIN=" + STR$(EP.DET.FIN%), 4100H)
  !    TO.USEREXIT(20) = -1
  !  ENDIF
  ENDIF
END SUB  
!   UEAPPL33.TPV
!   ***************************************************************************
!   **    Date       :  Marzo de 2003                                        **
!   **    Project    :  EFT                                           **
!   **    Include in :  EAMTSU33.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **    Author     :  JORGE ALBERTO CADENA  jcadena@supercabletv.net.co    **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!
SUB UEAPPL33 PUBLIC
!IF TO.USEROPTS(6) THEN \
!BEGIN 
  CALL EPAY.END.RECOVER.EFT 
!ENDIF
END SUB
!--------------------------------------------------------------FIN UEAPPL33.TPV
!   UEAPPL53.TPV
!   ***************************************************************************
!   **    Date       :  Marzo de 2003                                        **
!   **    Project    :  Pedidos                                              **
!   **    Include in :  EAMTSU53.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **    Author     :  JORGE ALBERTO CADENA  jcadena@sky.net.co             **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!
SUB UEAPPL53 PUBLIC
!IF TO.USEROPTS(6) THEN \
!BEGIN 
  IF TS.TEMP1I2 = 11 THEN \
  BEGIN 
    CALL EPAY.RECOVER.EFT 
  ENDIF
!ENDIF
END SUB
!-----------------------------------------------------------
!   UEAPPL57.TPV
!   ***************************************************************************
!   **    Date       :  Marzo de 2003                                        **
!   **    Project    :  EFT                                           **
!   **    Include in :  EAMTSU57.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **    Author     :  JORGE ALBERTO CADENA  jcadena@supercabletv.net.co    **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!
SUB UEAPPL57 PUBLIC
!IF TO.USEROPTS(6) THEN \
!BEGIN 
  CALL EPAY.START.RECOVER.EFT 
!ENDIF
END SUB
!
!
!*********************************************************************
!                         (UEAPPL60.TPV)
!                           (Ver 1.00)
!
! This user performs: 
! - Print E-payment bouchers 
!*********************************************************************
!
SUB UEAPPL60 PUBLIC
  IF EP.EFT.ACTIVO% THEN \     ! TEF OK
  BEGIN    
    IF TS.LINETYPE = 2  AND  \
       EP.EFT.DETAIL%   AND  \
       EP.EFT.SMA%      AND  \
       EP.DET.FIN%      THEN \
    BEGIN
      TO.USEREXIT(60) = 0
      CALL EP.LINE.PRINT("Recibo=" + EP.EPAY.TRANSNUM$ + " Aut=" + EP.H.AUTH.NUMBER$,4100H)
      If EP.VALOR.SALDO$ <> "" And EP.VALOR.SALDO$ <> "-000000001" Then Begin
      	Call FORMAT.AMOUNT(Int%(Val(EP.VALOR.SALDO$)))
		CALL EP.LINE.PRINT("Nuevo saldo disponible: " + TS.TEMP1$,4100H)
      Endif
      TO.USEREXIT(60) = -1
    ENDIF
!
    IF TS.LINETYPE = 2  AND  \
      EP.EFT.SMA%   THEN    \   ! es una transaccion de eft pendiente
    BEGIN
      EP.EFT.SMA% = 0           ! ya esta recibida por supermarket application
!    IF EP.TV.CHG% THEN  \
!    BEGIN
!      TO.TENDLIMITS(EP.TV.POS%,0) = EP.MAX.TV.SAV% 
!      TO.TENDLIMITS(EP.TV.POS%,1) = EP.CHG.TV.SAV% 
!      EP.TV.CHG% = 0
!    ENDIF 
!    IF EP.VERIF.TV.SAV% <> 99 THEN \
!    BEGIN 
!      TO.TENDOPTS(EP.TV.POS%,7) = EP.VERIF.TV.SAV% 
!    ENDIF
    ENDIF
!  
    IF TS.LINETYPE = 2  AND  \              ! linea de medio de pago
       EP.EFT.DETAIL%   AND  \              ! hay detalles pendientes
       NOT EP.DET.FIN%  THEN \              ! detalle inmediato
    BEGIN
      TO.USEREXIT(60) = 0
      CALL PRINT.EFT.DETAIL(EP.VALOR.EPAY$, EP.TAX.EPAY$)
      EP.EFT.HEADER% = 0
      TO.USEREXIT(60) = -1
    ENDIF
!  
    IF TS.PROCEDURE = 2 AND \                ! correccion de medio de pago
       TS.LINETYPE = 2  AND \                ! linea de medio de pago
       EP.EFT.DETAIL%   AND \                ! hay detalles pendientes
       EP.DET.FIN% THEN     \                ! esta condicionado al final
    BEGIN
      TO.USEREXIT(60) = 0
      CALL PRINT.EFT.DETAIL(EP.VALOR.EPAY$, EP.TAX.EPAY$)
      EP.EFT.HEADER% = 0
      TO.USEREXIT(60) = -1
    ENDIF
!  
    IF TS.LINETYPE = 6  AND \                ! final de la trx
       TS.LINEDATA = 2  AND \                ! linea de G
       EP.EFT.DETAIL%   AND \                ! hay detalles de EFT pendientes
       EP.DET.FIN% THEN     \                ! esta condicionado para el final
    BEGIN
      TO.USEREXIT(60) = 0
      CALL PRINT.EFT.DETAIL(EP.VALOR.EPAY$, EP.TAX.EPAY$)
      EP.EFT.HEADER% = 0
      TO.USEREXIT(60) = -1
    ENDIF
!  
  ENDIF
END SUB
!--------------------------------------------------------------FIN UEAPPL57.TPV
!
SUB UEAPPL23 PUBLIC
	If warningGeneral% Then \
	Begin
		If Ucase$(Left$(TS.DISP1$,4)) = "B117" Then warningSMA% = -1
		warningGeneral% = 0
	EndIf
	If appl.displayMsg% Then Begin
		TS.DISP1$ = appl.displayMsg1$
		TS.DISP2$ = appl.displayMsg2$
	Endif
	If ep.disp.cust$ = "2" Then Begin
		TS2.DISP1$ = TS.DISP1$
		TS2.DISP2$ = TS.DISP2$
		ep.disp.cust$ = "0"
	Endif
END SUB
!
SUB UEAPPL37 PUBLIC
	warningGeneral% = -1
END SUB
!--------------------------------------------------------------FIN UEAPPL37.TPV
!
Sub EP.DYKEY.CHECK Public
	If EP.DYKEY.ACT% Then Begin
		If Match(":" + Str$(TS.IO.MOTORKEY) + ":", EP.DYKEY.KEY$, 1) Then Begin
			If Mid$(TS.IO.HDR$, 11, 1) = "0" Then Begin
				TS.GUIDANCE = 1004
				TS.IO.MOTORKEY = 0
				DIM TS.IO.KEYS(10)
				DIM TS.IO.DATA$(10)
			Endif
		Endif
	Endif
End Sub
!
END

