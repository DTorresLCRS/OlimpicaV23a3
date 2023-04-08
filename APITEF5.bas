!*********************************************************************
!  (APITEF.BAS)
!  (Ver 5.00)   ( Olimpica )
!  Author : Jorge Alberto Cadena Calderon
!           La Cadena Retail Solutions Ltda.
!
!
%ENVIRON T
!
! Supermarket application global data !
!%INCLUDE \ADX_UPGM\EAMTSWKG.J86

%INCLUDE EAMTSWKG.J86

!%INCLUDE \ADX_UPGM\EAMTOPTS.J86

%INCLUDE EAMTOPTS.J86

!%INCLUDE \ADX_UPGM\EAMTRANS.J86

%INCLUDE EAMTRANS.J86

%INCLUDE C:\APPL\APIVARI.J86

!%INCLUDE APIVARI.J86

String APITEF5.externalTefData$, APITEF5.externalAccount$, APITEF5.externalCustomer$

! Campos opcionales para identificar una operación
String Global EP.OPT.CONVENIO$, EP.OPT.BOLSILLO$

! Parámetros de tef genérico
String tefUDKey$, tefAppl$(2), tefRequest$, tefResponse$, tefVouchers$(1), tefEntity$, tefParamData$, tefBalanceConsecut$, tefBalanceDatetime$, tefBalanceUserdata$
Integer*2 tefApplCount%, tefVoucherCount%, tefVoucherSize%, tefIdTypeLen%
                           !
! tpv application interface global data
!
  Integer*1 global warningSMA%
!
  INTEGER*1                 \
    EP.EFT.CMP.TRX%        ,\
    EP.EFT.CMP.OK%         ,\
    EP.TEF.DETAIL%         ,\
    EP.TEF.SMA%            ,\
    EP.FLAG.DEV%           ,\
    EP.TEF.ACTIVO%         ,\
    EP.TEF.SALDO.ACTIVO%   ,\
    EP.TEF.CASHB.ACTIVO%   ,\
    EP.TEF.RED.ACTIVO%     ,\
    EP.CUT.VOUCHER.TEF%(1) ,\
    EP.medioPagoTef%

  INTEGER*2                 \
    EP.TEF.POINTER1%          

  STRING                    \
    EP.VOUCHER.TEF$(1)    
    
  STRING  GLOBAL            \
     EP.TV.COMPENSAR$      ,\ 
     EP.CLF.PREF.PPAL$     ,\
     EP.CLF.PREF.ALT$      ,\
     TP.PREF.FISICO$       ,\
     TP.CUPON.FISICO$      ,\
     TP.PREF.CONDICIONADO$ ,\
     TP.PREF.MEDIO.PAGO$   ,\
     TP.PREF.CUPON.TEXTO$  ,\
     TP.PREF.BONO.PREMIO$
     
  STRING                    \
     EP.TEF.CEDULA$        ,\ 
     EP.TEF.BOLSILLO$      ,\
     EP.TEF.PRODUCTO$      ,\
	   EP.TEF.TOKEN$         ,\
	   EP.APPL.JAVA$         ,\
	   EP.BIN.PRODUCTO$
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
!
FUNCTION MATCHB(P1$,P2$,P3) EXTERNAL
  INTEGER*2 MATCHB
  STRING P1$
  STRING P2$
  INTEGER*2 P3
END FUNCTION

!---------------------------------------------------------------------------
!  User defined routines
!
SUB INT4.TO.HEXA(ERRVALUE%,ERRVALUE$) EXTERNAL
  INTEGER*4 ERRVALUE%
  STRING ERRVALUE$
  INTEGER*4 HX%,SX%,THE.SUM%,S%
  STRING ERRFX$,Z$
END SUB
!
! Rutina para inicializar variables de TCRO
! Se usa en este modulo para prevenir conflictos
! con Compensar, si alguna variable se queda prendida 
! a raiz de un intento fallido de TCRO
Sub RESET14.TCRO External
End Sub
!
!   *** ASYNCRONOUS ERROR HANDLING  *********
!
SUB ASYNC.ERR(RFLAG,OVER) EXTERNAL
  INTEGER*2 RFLAG
  STRING OVER
  STRING ERRFX$
END SUB
!
Sub asignarVariableGlobal(pName$, pValue$) External
	String pName$, pValue$
End Sub
!
Function consultarVariableGlobal(pName$) External
	String consultarVariableGlobal, pName$
End Function
!
SUB ERRORTRAP EXTERNAL
  !*ERROR ASSEMBLY ROUTINE *!
END SUB
!
Sub traceApplication(pMsg$, pAppl$) External
	String pMsg$, pAppl$
End Sub
!
Function getInstitucion External
	Integer*2 getInstitucion
End Function
!
Function isNumeric(pString$) External
	Integer*1 isNumeric
	String pString$
End Function
!
SUB EP.GETUNPK EXTERNAL                                  !
END SUB                                                ! siguiente campo 
!
SUB EP.SAVE.PRINT  EXTERNAL
END SUB
!
SUB EP.RESTORE.PRINT EXTERNAL
END SUB
!
SUB EP.LINE.PRINT(LINEA$,ESTACION%) EXTERNAL
    STRING LINEA$
    INTEGER*2 ESTACION%
END SUB
!
SUB EP.SAVE.DISPLAY EXTERNAL
END SUB
!
SUB EP.RESTORE.DISPLAY EXTERNAL
END SUB
!
SUB EP.DISPLAY.A.MESSAGE(UE.DISP.MESSAGE$) EXTERNAL 
  STRING UE.DISP.MESSAGE$, UE.WORK$
END SUB
!
SUB EP.DISPLAY.AN.ERROR(UE.DISP.MESSAGE$) EXTERNAL 
  STRING UE.DISP.MESSAGE$, UE.WORK$
END SUB
!
SUB EP.SAVE.KEYS EXTERNAL
  INTEGER*2 I.2% 
END SUB
!
SUB EP.RESTORE.KEYS EXTERNAL
  INTEGER*2 I.2% 
END SUB
!
Sub printDebug(pMessage$) External
	String pMessage$
End Sub
!
Sub menpag.ignoreTender External
End Sub
!
Sub disc.voidDiscountExt External
End Sub
!
Sub clearEntity External
End Sub
!
Sub reasignEntity(pTipoVariedad%, pEntity%) External
	Integer*2 pTipoVariedad%, pEntity%
End Sub
!
Function disc.validateTender(pAmount%, pTenderType$, pEntity%, pAccount$) External
	Integer*4 pAmount%
	Integer*1 disc.validateTender
	String pTenderType$, pAccount$
	Integer*2 pEntity%
End Function
!
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
!--------------------------------------------------------------------------
!   Application manager routines
!---------------------------------------------------------
!
Sub putPendingReverse(pAppl$, pFunction$, pConsecutive$, pInvoice$, pDate$, pAmt%, pType%, pVar%, \
		pDE1$, pDE2$, pDE3$, pDE4$, pDE5$, pDE6$) External
	String pAppl$, pFunction$, pConsecutive$, pInvoice$, pDate$
	String pDE1$, pDE2$, pDE3$, pDE4$, pDE5$, pDE6$
	Integer *2 pType%, pVar%
	Integer*4 pAmt%
End Sub
!
SUB EP.ADD.DATA.ENTRY(UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6) EXTERNAL
  STRING UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6
END SUB
!
!   Add a data entry string (string 99) routine
!
SUB EP.ADD.DATA.ENTRY99(UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6) EXTERNAL
  STRING UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6
END SUB
!
!
SUB EP.GET.KBDATA(UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, \
    UE.KEYB.DATA$) EXTERNAL
  STRING UE.REQ.MESSAGE$, UE.KEYB.DATA$, UE.INI.RANGE$, UE.END.RANGE$,\
         UE.WORK$, UE.SAVDISP$ 
END SUB
!
!  Print vouchers
!
SUB EP.STORE.EFTLINE(UE.PRINT.LINE$) EXTERNAL
  STRING UE.PRINT.LINE$
END SUB
!
!
!
FUNCTION EP.NEW.TRANSNUM  EXTERNAL
  STRING EP.NEW.TRANSNUM, UE.WORK.TRANSNUM$
END FUNCTION
!


!
!
SUB TRANSLATE.COMM.CODE(UE.CODE$,UE.CODE.DESC$) EXTERNAL
  STRING UE.CODE$,UE.CODE.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
!
END SUB
!
!
SUB TRANSLATE.APPL.CODE(UE.CODE$,UE.CODE.DESC$) EXTERNAL
  STRING UE.CODE$,UE.CODE.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
END SUB
!
!
SUB TRANSLATE.ISO.CODE(UE.CODE$,UE.CODE.DESC$) EXTERNAL
  STRING UE.CODE$,UE.CODE.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
END SUB
!
!
SUB TRANSLATE.GUIDE.CODE(UE.CODE$,UE.CODE.DESC$) EXTERNAL
  STRING UE.CODE$,UE.CODE.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
END SUB
!
SUB TRANSLATE.TV.CODE(UE.GROUP.DESC$,UE.CODE$) EXTERNAL
  STRING UE.CODE$,UE.GROUP.DESC$
  INTEGER*2 EP.I%
  INTEGER*1 EP.FOUND%
END SUB
!
SUB EP.BUSCA.LIMITE.TV(UE.TIPO.VAR$) EXTERNAL
  INTEGER*2 UE.I%
  INTEGER*1 UE.FOUND%
  STRING UE.TIPO.VAR$

END SUB

!
SUB CAL.TAX.TOTALS(EP.TAX.VAL%) EXTERNAL
  INTEGER*4 EP.TAX.VAL%
  INTEGER*2 IFOR%
END SUB
!
!
SUB CAL.TAX.BASE(EP.TAX.BASE%) EXTERNAL
  INTEGER*4 EP.TAX.BASE%
  INTEGER*2 IFOR%, POS%
END SUB
!
!  Read MSR data
!
SUB EP.GET.TRX.TYPE(UE.DISP.MESSAGE$,UE.MSR.DATA$,UE.KEYB.DATA$) EXTERNAL
   STRING UE.DISP.MESSAGE$, UE.MSR.DATA$, UE.KEYB.DATA$, UE.SAVDISP$
!
END SUB
!
Function EP.invocarApplManager1(epAppl$,epFunction$,parteVariable$,usaConsecutivo%, pNoMsg%, pIncrem%) External
	String EP.invocarApplManager1,epAppl$,epFunction$,parteVariable$
	Integer*1 usaConsecutivo%, pNoMsg%
	Integer*2 pIncrem%
End Function
!
FUNCTION EP.invocarApplManager(epAppl$,epFunction$,parteVariable$,usaConsecutivo%) EXTERNAL
	String EP.invocarApplManager,epFunction$,parteVariable$,epAppl$,epCajero$,epInvoiceNbr$,consecutivo$
	Integer*1 usaConsecutivo%
END FUNCTION
!
Sub readPropData(pFilename$, pPropData$, pSession%, pReturn%) External
	String pFilename$, pPropData$
	Integer*2 pSession%, pReturn%
End Sub
!
Sub getNextField(pData$, pLastIndex%, pSeparator$, pNextField$) External
	String pNextField$, pData$, pSeparator$
	Integer*2 pLastIndex%
End Sub
!
Function getProperty(pProperty$, pData$) External
	String getProperty, pProperty$, pData$
End Function
!
Function hexToInt(pHex$)
	Integer*4 hexToInt, tmpAnswer%
	Integer*2 tmpLen%, tmpCounter%, tmpValue%
	String pHex$, tmpChar$
	!
	tmpAnswer% = 0
	pHex$ = UCase$(pHex$)
	tmpLen% = Len(pHex$)
	For tmpCounter% = tmpLen% To 1 Step -1
		tmpChar$ = Mid$(pHex$, tmpCounter%, 1)
		tmpValue% = Asc(tmpChar$)
		If tmpValue% >= 48 And tmpValue% <= 57 Then Begin
			tmpValue% = tmpValue% - 48
		Endif Else Begin
			tmpValue% = tmpValue% - 55
		Endif
		tmpAnswer% = tmpAnswer% + tmpValue% * 16 ^ (tmpLen% - tmpCounter%)
	Next tmpCounter%
	hexToInt = tmpAnswer%
End Function

Sub defaultGenericTefParam
	tefUDKey$ = "20201001"
	tefApplCount% = 1
	Dim tefAppl$(1, 1)
	tefAppl$(1, 0) = "09"
	tefAppl$(1, 1) = "LEA TARJ/ENTRE TOKEN"
	tefIdTypeLen% = 2
End Sub

Sub initGenericTefParam(pParamData$)
	String pParamData$, tmpParamValue$, tmpField$, tmpTefData$
	Integer*2 tmpIndex%, tmpCounter%
	!
	tefUDKey$ = getProperty("GENERIC_TEF_UDKEY", pParamData$)
	tefIdTypeLen% = Int%(Val(getProperty("IDTYPE_LENGTH", pParamData$)))
	tmpTefData$ = ""
	tefApplCount% = 0
	tmpParamValue$ = getProperty("TENDER_MENU_" + Str$(tefApplCount% + 1), pParamData$)
	While Len(tmpParamValue$) > 0
		tefApplCount% = tefApplCount% + 1
		tmpIndex% = 0
		Call getNextField(tmpParamValue$, tmpIndex%, ",", tmpField$) ! appl name
		Call getNextField(tmpParamValue$, tmpIndex%, ",", tmpField$) ! appl id
		tmpField$ = Right$("00" + tmpField$, 2)
		tmpTefData$ = tmpTefData$ + tmpField$ + ","
		tmpField$ = getProperty("INPUT_ACCOUNT_DESC_" + tmpField$, pParamData$) ! input account descriptor
		If tmpField$ = "" Then Begin
			tmpField$ = "@EMPTY"
		Endif
		tmpTefData$ = tmpTefData$ + tmpField$ + ","
		tmpParamValue$ = getProperty("TENDER_MENU_" + Str$(tefApplCount% + 1), pParamData$)
	Wend
	!
	If tefApplCount% <= 0 Then Begin
		Call defaultGenericTefParam
	Endif Else Begin
		Dim tefAppl$(tefApplCount%, 1)
		tmpIndex% = 0
		For tmpCounter% = 1 To tefApplCount%
			Call getNextField(tmpTefData$, tmpIndex%, ",", tmpField$) ! appl id
			tefAppl$(tmpCounter%, 0) = tmpField$
			Call getNextField(tmpTefData$, tmpIndex%, ",", tmpField$) ! input account descriptor
			tefAppl$(tmpCounter%, 1) = tmpField$
		Next tmpCounter%
	Endif
End Sub

Function getInputAccountDescriptor(pAppl$)
	String getInputAccountDescriptor, pAppl$, tmpAnswer$
	Integer*2 tmpCounter%
	Integer*1 tmpFound%
	!
	tmpFound% = 0
	tmpCounter% = 1
	tmpAnswer$ = ""
	While tmpFound% = 0 And tmpCounter% <= tefApplCount%
		If tefAppl$(tmpCounter%, 0) = pAppl$ Then Begin
			tmpFound% = -1
			tmpAnswer$ = tefAppl$(tmpCounter%, 1)
		Endif Else Begin
			tmpCounter% = tmpCounter% + 1
		Endif
	Wend
	!
	If tmpAnswer$ = "" Then Begin
		tmpAnswer$ = "ENTRE NRO. CUENTA"
	Endif
	getInputAccountDescriptor = tmpAnswer$
End Function

Function getValuePerFunction(pInfo$, pFunction$)
	String getValuePerFunction, pInfo$, pFunction$
	String tmpValue$
	Integer*2 tmpIndex%, tmpIndex1%
	!
	If Match(":", pInfo$, 1) = 0 Then Begin
		! Hay un solo valor en pInfo$
		tmpValue$ = pInfo$
	Endif Else Begin
		If Left$(pInfo$, 1) <> ":" Then Begin
			pInfo$ = ":" + pInfo$
		Endif
		If Right$(pInfo$, 1) <> ":" Then Begin
			pInfo$ = pInfo$ + ":"
		Endif
		tmpIndex% = Match(":" + pFunction$ + "@", pInfo$, 1)
		If tmpIndex% = 0 Then Begin
			tmpIndex% = Match(":XX@", pInfo$, 1)
			If tmpIndex% = 0 Then Begin
				tmpIndex% = 1
			Endif
		Endif
		tmpIndex1% = Match(":", pInfo$, tmpIndex% + 1)
		tmpValue$ = Mid$(pInfo$, tmpIndex% + 1, tmpIndex1% - tmpIndex% - 1)
	Endif
	tmpIndex% = Match("@", tmpValue$, 1)
	getValuePerFunction = Right$(tmpValue$, Len(tmpValue$) - tmpIndex%)
End Function

Sub APITEF5.parseTefDef(pTefDef$, pFunction$, pApplName$, pApplId$, pTV$, pEntity$, pSale%, pVoid%, pCash%, pRequestId%, pBalanceReq%) Public
	String pTefDef$, pFunction$, pApplName$, pApplId$, pTV$, pEntity$
	Integer*1 pSale%, pVoid%, pCash%, pRequestId%, pBalanceReq%
	String tmpField$
	Integer*2 tmpIndex%
	!
	tmpIndex% = 0
	Call getNextField(pTefDef$, tmpIndex%, ",", tmpField$) ! appl name
	pApplName$ = tmpField$
	Call getNextField(pTefDef$, tmpIndex%, ",", tmpField$) ! appl id
	pApplId$ = Right$("00" + tmpField$, 2)
	Call getNextField(pTefDef$, tmpIndex%, ",", tmpField$) ! tender
	pTV$ = getValuePerFunction(tmpField$, pFunction$)
	Call getNextField(pTefDef$, tmpIndex%, ",", tmpField$) ! entity
	If pApplId$ = "09" Then Begin
		pTV$ = EP.TV.COMPENSAR$
		pEntity$ = "000"
	Endif Else Begin
		pEntity$ = getValuePerFunction(tmpField$, pFunction$)
	Endif
	Call getNextField(pTefDef$, tmpIndex%, ",", tmpField$) ! sale
	pSale% = Int%(Val(tmpField$))
	Call getNextField(pTefDef$, tmpIndex%, ",", tmpField$) ! void
	pVoid% = Int%(Val(tmpField$))
	Call getNextField(pTefDef$, tmpIndex%, ",", tmpField$) ! cash
	pCash% = Int%(Val(tmpField$))
	Call getNextField(pTefDef$, tmpIndex%, ",", tmpField$) ! requestId
	pRequestId% = Int%(Val(tmpField$))
	Call getNextField(pTefDef$, tmpIndex%, ",", tmpField$) ! balanceReq
	pBalanceReq% = Int%(Val(tmpField$))
End Sub

Sub printTefVouchers
	Integer*2 tmpCounter%
	!
	For tmpCounter% = 1 To tefVoucherCount%
		!Call EP.invocarApplManager(Left$(tefVouchers$(tmpCounter%), 2), "10", Right$(tefVouchers$(tmpCounter%), Len(tefVouchers$(tmpCounter%)) - 2), 0)
		Call EP.invocarApplManager1(Left$(tefVouchers$(tmpCounter%), 2), "10", Right$(tefVouchers$(tmpCounter%), Len(tefVouchers$(tmpCounter%)) - 2), 0, 0, 0)
	Next tmpCounter%
End Sub

Sub saveTefVoucher(pInfo$)
	String pInfo$, tmpVoucher$(1)
	Integer*2 tmpCounter%
	!
	Call printDebug("saveTefVoucher(" + pInfo$ + ")")
	!
	tefVoucherCount% = tefVoucherCount% + 1
	If tefVoucherCount% > tefVoucherSize% Then Begin
		Dim tmpVoucher$(tefVoucherSize%)
		For tmpCounter% = 1 To tefVoucherSize%
			tmpVoucher$(tmpCounter%) = tefVouchers$(tmpCounter%)
		Next tmpCounter%
		Dim tefVouchers$(tefVoucherSize% + 10)
		For tmpCounter% = 1 To tefVoucherSize%
			tefVouchers$(tmpCounter%) = tmpVoucher$(tmpCounter%)
		Next tmpCounter%
		tefVoucherSize% = tefVoucherSize% + 10
	Endif
	tefVouchers$(tefVoucherCount%) = pInfo$
End Sub

Sub saveTefVouchers
	Integer*2 tmpCounter%, tmpIndex%
	String tmpField$, tmpApplFunc$, tmpRequest$, tmpResponse$
	Integer*1 tmpTrim%
	!
	Call printDebug("saveTefVouchers")
	!
	tefVoucherSize% = 10
	Dim tefVouchers$(tefVoucherSize%)
	tefVoucherCount% = 0
	For tmpCounter% = 1 To SL.END
		If SL.STR$(tmpCounter%) <> "" Then Begin
			Call printDebug("String[" + Str$(tmpCounter%) + "] type=" + Str$(Asc(SL.STR$(tmpCounter%))))
			If Asc(SL.STR$(tmpCounter%)) = 99h Then Begin
				tmpIndex% = 0
				Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! String type
				Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! User data key
				If Unpack$(tmpField$) = tefUDKey$ Then Begin
					tmpTrim% = 0
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Appl Function
					tmpApplFunc$ = tmpField$
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Request
					tmpRequest$ = tmpField$
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Response
					tmpResponse$ = tmpField$
					Call saveTefVoucher(tmpApplFunc$ + ":" + tmpRequest$ + ":" + tmpResponse$)
					!-----------------------------------------------------------------
					! Si los campos se pasan de 198, hay que recortarlos
					!-----------------------------------------------------------------
					If Len(tmpRequest$) > 198 Then Begin
						tmpTrim% = -1
						tmpRequest$ = Left$(tmpRequest$, 198)
					Endif
					If Len(tmpResponse$) > 198 Then Begin
						tmpTrim% = -1
						tmpResponse$ = Left$(tmpResponse$, 198)
					Endif
					If tmpTrim% Then Begin
						SL.STR$(tmpCounter%) = 				\
								Pack$("99") + ":" + 			\! User data
								Pack$(tefUDKey$) + ":" + 	\! Llave
								tmpApplFunc$ + ":" + 			\! Aplicación Función
								tmpRequest$ + ":" + 			\! Request
								tmpResponse$ 							 ! Response
					Endif
					!-----------------------------------------------------------------
				Endif
			Endif
		Endif
	Next tmpCounter%
End Sub

Function selectTefAppl
	String selectTefAppl, tmpAnswer$
	!
	!selectTefAppl = EP.invocarApplManager("09", "00", "", 0)
	!
	If Len(APITEF5.externalTefData$) > 0 Then Begin
		tmpAnswer$ = APITEF5.externalTefData$
		APITEF5.externalTefData$ = ""
	Endif Else Begin
		tmpAnswer$ = EP.invocarApplManager1("09", "00", "", 0, -1, 1)
	Endif
	!
	selectTefAppl = tmpAnswer$
End Function

Function requestForId
	String requestForId
	!
	requestForId = EP.invocarApplManager("09", "50", "", 0)
End Function

Sub saveTefUD(pApplFunc$)
	String pApplFunc$
	!
	TS.USERDATA$ = \
		Pack$(tefUDKey$) + ":" + 	\! User data key
		pApplFunc$ + ":" + 				\! appl + function
		tefRequest$ + ":" + 			\! request
		tefResponse$ 							 ! response
	!
	TS.TEMP1I1 = 99
	Call TSTPEC01
End Sub

Function getTracks(pPan$, pExpireDate$)
	String getTracks, pPan$, pExpireDate$, tmpAnswer$
	Integer*2 tmpLen1%, tmpLen2%, tmpCounter%
	!
	tmpLen1% = Len(pPan$)
	tmpLen2% = Len(pExpireDate$)
	tmpAnswer$ = ""
	!
	! track2
	For tmpCounter% = 1 To tmpLen1%
		tmpAnswer$ = tmpAnswer$ + Chr$(hexToInt("0" + Mid$(pPan$, tmpCounter%, 1)))
	Next tmpCounter%
	tmpAnswer$ = tmpAnswer$ + Chr$(0DH)
	For tmpCounter% = 1 To tmpLen2%
		tmpAnswer$ = tmpAnswer$ + Chr$(hexToInt("0" + Mid$(pExpireDate$, tmpCounter%, 1)))
	Next tmpCounter%
	tmpAnswer$ = tmpAnswer$ + String$(17, Chr$(0))
	tmpAnswer$ = tmpAnswer$ + Chr$(4CH)
	!
	! track1
	tmpAnswer$ = tmpAnswer$ + Chr$(22H)
	For tmpCounter% = 1 To tmpLen1%
		tmpAnswer$ = tmpAnswer$ + Chr$(hexToInt("1" + Mid$(pPan$, tmpCounter%, 1)))
	Next tmpCounter%
	tmpAnswer$ = tmpAnswer$ + Chr$(3EH)
	tmpAnswer$ = tmpAnswer$ + String$(26, Chr$(0))
	tmpAnswer$ = tmpAnswer$ + Chr$(3EH)
	For tmpCounter% = 1 To tmpLen2%
		tmpAnswer$ = tmpAnswer$ + Chr$(hexToInt("1" + Mid$(pExpireDate$, tmpCounter%, 1)))
	Next tmpCounter%
	tmpAnswer$ = tmpAnswer$ + String$(49, Chr$(0))
	!
	getTracks = tmpAnswer$
End Function
!
!
SUB EP.GET.TARJ.NBR(UE.DISP.MESSAGE$,UE.MSR.DATA$,UE.KEYB.DATA$,UE.ALL.CARD$, \
                    UE.FECHA.VENC$, pToken$)
   STRING UE.DISP.MESSAGE$, UE.MSR.DATA$, UE.KEYB.DATA$, UE.SAVDISP$, UE.A$, \
          UE.ALL.CARD$, UE.FECHA.VENC$, pToken$
   INTEGER*2 UE.I%, UE.J%
!
   !
   Call traceApplication("EP.GET.TARJ.NBR(" + UE.DISP.MESSAGE$ + "...", "APITEF5")
   !
   pToken$ = ""
   UE.MSR.DATA$ = ""
   UE.ALL.CARD$ = ""
   UE.SAVDISP$  = TS.SAVDISP1$ + TS.SAVDISP2$
   TS.SAVDISP1$ = LEFT$(UE.DISP.MESSAGE$,20) 
   TS.SAVDISP2$ = MID$(UE.DISP.MESSAGE$,21,20)
   TS.IO.MOTORKEY = 0                      ! Set for no input yet
   TS.IO.STATE  = 7                        ! Acct/Date state
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
         IF TS.IO.KEYS(10) = 80 THEN \    ! any Keyed data
         BEGIN 
!           call ep.line.print("l10="+str$(LEN(TS.IO.DATA$(10)))+":"+TS.IO.DATA$(10)+">"+ TP.PREF.MEDIO.PAGO$,4100h)
!           IF LEFT$(TS.IO.DATA$(10), LEN(TP.PREF.MEDIO.PAGO$)) = TP.PREF.MEDIO.PAGO$ THEN  \ ! its a
!           BEGIN                            !
!             TS.LINETYPE = 8                ! Operator Guidance
!             TS.LINEDATA = 6                ! Data out of range
!             CALL TSCSEC08                  ! Guidance with Clear
!             TS.IO.MOTORKEY = 0             ! Try again
!           ENDIF                           ! Good option or no option
!
           Call traceApplication("Lectura Scanner/teclado: " + TS.IO.DATA$(10), "APITEF5")
           
           IF (VAL(TS.IO.DATA$(10)) < 1000 OR    \ Not valid option 
              VAL(TS.IO.DATA$(10)) > 9999999999999999999 ) THEN \
           BEGIN                            !
             TS.LINETYPE = 8                ! Operator Guidance
             TS.LINEDATA = 6                ! Data out of range
             CALL TSCSEC08                  ! Guidance with Clear
             TS.IO.MOTORKEY = 0             ! Try again
           ENDIF \                          ! Good option or no option
           ELSE \                           ! Valid option
           BEGIN  
             !UE.KEYB.DATA$ = TS.IO.DATA$(10) ! Move option keyed
             !UE.ALL.CARD$  = TS.IO.DATA$(10) ! Move option keyed
             !UE.MSR.DATA$  = ""             ! Not data read
             !
             !
             !UE.MSR.DATA$ = getTracks(TS.IO.DATA$(10), "9912")
			 !
			 pToken$ = TS.IO.DATA$(10)
			 !
             Dim TS.IO.KEYS(10)
             Dim TS.IO.DATA$(10)
			 !
			 UE.ALL.CARD$ = ""
			 UE.FECHA.VENC$ = String$(4, "0")
			 !
	         !UE.I% = MATCHB(CHR$(13),UE.MSR.DATA$,1) ! Look for separator 1
	         !IF UE.I% = 0 THEN UE.I% = 99            ! Separator 1 not found
	         !UE.J% = MATCHB(CHR$(61),UE.MSR.DATA$,1) ! Look for separator 2
	         !IF UE.J% = 0 THEN UE.J% = 99            ! Separator 2 not found
	         !IF UE.J% < UE.I% THEN UE.I% = UE.J%     ! Pick best separator
	         !UE.A$ = LEFT$(UE.MSR.DATA$,UE.I%-1)     ! Isolate account number
	         !TS.IO.DATA$(2) = UNPACK$(PACK$(UE.A$))  ! Move MSR account
	         !UE.ALL.CARD$ = TS.IO.DATA$(2)           ! Move MSR account
	         !IF UE.I% = 99 THEN BEGIN                   ! No separator found
	         !  TS.IO.DATA$(4) = STRING$(4,"0")       ! No expiration date
	         !ENDIF ELSE BEGIN                        ! Separator found
	         !  UE.A$ = MID$(UE.MSR.DATA$,(UE.I%+1),4)! Isolate expiration date  ?=
	         !  TS.IO.DATA$(4) = UNPACK$(PACK$(UE.A$))! Move MSR date
	         !ENDIF
			 !UE.FECHA.VENC$ = TS.IO.DATA$(4)
			 !
	         TS.IO.DEVICE = 1                 ! Force a keyed account
	         TS.IO.MOTORKEY = 80              ! Show input ready
           ENDIF                            ! Valid option
         ENDIF \                            ! end any keyed data 
         ELSE \
         BEGIN
           TS.LINETYPE = 8                ! Operator Guidance
           TS.LINEDATA = 6                ! Data out of range
           CALL TSCSEC08                  ! Guidance with Clear
           TS.IO.MOTORKEY = 0             ! Try again
         ENDIF
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
         BEGIN 
           UE.I% = MATCHB(CHR$(13),UE.MSR.DATA$,1) ! Look for separator 1
           IF UE.I% = 0 THEN UE.I% = 99            ! Separator 1 not found
           UE.J% = MATCHB(CHR$(61),UE.MSR.DATA$,1) ! Look for separator 2
           IF UE.J% = 0 THEN UE.J% = 99            ! Separator 2 not found
           IF UE.J% < UE.I% THEN UE.I% = UE.J%     ! Pick best separator
           UE.A$ = LEFT$(UE.MSR.DATA$,UE.I%-1)     ! Isolate account number
           TS.IO.DATA$(2) = UNPACK$(PACK$(UE.A$))  ! Move MSR account
           UE.ALL.CARD$ = TS.IO.DATA$(2)           ! Move MSR account
           IF UE.I% = 99 THEN BEGIN                   ! No separator found
             TS.IO.DATA$(4) = STRING$(4,"0")       ! No expiration date
           ENDIF ELSE BEGIN                        ! Separator found
             UE.A$ = MID$(UE.MSR.DATA$,(UE.I%+1),4)! Isolate expiration date  ?=
             TS.IO.DATA$(4) = UNPACK$(PACK$(UE.A$))! Move MSR date
           ENDIF
           UE.FECHA.VENC$ = TS.IO.DATA$(4)
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
!
!  rutina para la lectura del MSR
! 
SUB EP.GET.MSR.TRACKS(UE.DISP.MESSAGE$,UE.MSR.DATA$,UE.MSR.TRACK1$,UE.MSR.TRACK2$, \
    UE.ALL.CARD$, UE.FECHA.VENC$, UE.NOMBRE.CTE$) PUBLIC
   STRING UE.DISP.MESSAGE$, UE.MSR.DATA$, UE.MSR.TRACK1$, UE.MSR.TRACK2$, UE.SAVDISP$, UE.A$, \
          UE.ALL.CARD$, UE.FECHA.VENC$ , UE.NOMBRE.CTE$
   INTEGER*2 UE.I%, UE.J%, UE.L1%, UE.L2%, UE.POS%
!
   UE.MSR.DATA$ = ""
   UE.ALL.CARD$ = ""
   UE.MSR.TRACK1$ = ""
   UE.MSR.TRACK2$ = ""
   UE.FECHA.VENC$ = ""
   UE.NOMBRE.CTE$ = ""
   
   UE.SAVDISP$  = TS.SAVDISP1$ + TS.SAVDISP2$
   TS.SAVDISP1$ = LEFT$(UE.DISP.MESSAGE$,20) 
   TS.SAVDISP2$ = MID$(UE.DISP.MESSAGE$,21,20)
   TS.IO.MOTORKEY = 0                      ! Set for no input yet
   TS.IO.STATE  = 7                        ! Acct/Date state
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
         TS.IO.MOTORKEY = 0 
	     GOTO CONTINUAR
       ENDIF 
     ENDIF \ 
     ELSE \                                 ! Not I/O processor
     BEGIN   
!      
       IF EVENT% = 41 THEN \                ! Input from MSR
       BEGIN
         TS.ER.RETURN = -1                  ! Return from error
         READ #  41 ; LINE UE.MSR.DATA$     ! Read MSR
         IF LEN(UE.MSR.DATA$) < 6 OR        \ Insufficient data
            TS.ER.RETURN = 0 THEN \
         BEGIN     ! Bad read
           TS.IO.MOTORKEY = 73              ! Clear key
         ENDIF \
         ELSE \
         BEGIN 
		   UE.L1% = ASC(MID$(UE.MSR.DATA$,38,1))
		   UE.L2% = ASC(MID$(UE.MSR.DATA$,39,1))
           IF UE.L1% = 255 THEN \ 
		   BEGIN  
		     CALL EP.DISPLAY.AN.ERROR("TRACK 2 ERROR") ! Display error message.
		     WAIT;3000 
		   ENDIF \ 
		   ELSE \ 
		   BEGIN \ 
		     UE.MSR.TRACK2$ = MID$(UE.MSR.DATA$,1,UE.L1%) ! Here you would convert the input to 
			                                              ! characters and display it. 
             UE.I% = MATCHB(CHR$(13),UE.MSR.TRACK2$,1)    ! Look for separator 1
             IF UE.I% = 0 THEN UE.I% = 99                 ! Separator 1 not found
             UE.J% = MATCHB(CHR$(61),UE.MSR.TRACK2$,1)    ! Look for separator 2
             IF UE.J% = 0 THEN UE.J% = 99                 ! Separator 2 not found
             IF UE.J% < UE.I% THEN UE.I% = UE.J%          ! Pick best separator
             UE.A$ = LEFT$(UE.MSR.TRACK2$,UE.I%-1)        ! Isolate account number
             UE.ALL.CARD$ = UNPACK$(PACK$(UE.A$))         ! Move MSR account
             IF UE.I% = 99 THEN \
			 BEGIN                     ! No separator found
               UE.FECHA.VENC$ = STRING$(4,"0")            ! No expiration date
             ENDIF \
			 ELSE \
			 BEGIN                             ! Separator found
               UE.A$ = MID$(UE.MSR.TRACK2$,(UE.I%+1),4)   ! Isolate expiration date  ?=
               UE.FECHA.VENC$ = UNPACK$(PACK$(UE.A$))     ! Move MSR date
             ENDIF
		   ENDIF 
! Process track 1 data. 
		   IF UE.L2% = 255 THEN \ 
		   BEGIN  
		     CALL EP.DISPLAY.AN.ERROR("TRACK 1 ERROR")    ! Display error message.
			 WAIT;3000 
		   ENDIF \ 
		   ELSE \ 
		   BEGIN \ 
		     UE.MSR.TRACK1$ = MID$(UE.MSR.DATA$,40,UE.L2%) ! Here you would convert the input to 
	                               		                  ! characters and display it. 
             UE.NOMBRE.CTE$ = ""
             UE.I% = MATCHB(">",UE.MSR.TRACK1$,1) ! Look for separator 1
             UE.J% = MATCHB(">",UE.MSR.TRACK1$,UE.I% + 1) ! Look for separator 2
             IF UE.J% = 0 AND UE.J% = 0 THEN \        ! Separator 2 not found
             BEGIN
               UE.I% = MATCHB(CHR$(29),UE.MSR.TRACK1$,1) ! Look for separator 1
               UE.J% = MATCHB(CHR$(29),UE.MSR.TRACK1$,UE.I% + 1) ! Look for separator 2
             ENDIF                       
             IF UE.J% <> 0 THEN \                    ! separator 2 found
             BEGIN     
               UE.A$ = MID$(UE.MSR.TRACK1$,UE.I%+1,UE.J%-UE.I%-1)
               FOR UE.POS% = 1 TO (UE.J% - UE.I% - 1)
                  UE.NOMBRE.CTE$ = UE.NOMBRE.CTE$ + CHR$(ASC(MID$(UE.A$,UE.POS%,1)) + 32 )
               NEXT UE.POS%
               IF LEN(UE.NOMBRE.CTE$) > 38 THEN UE.NOMBRE.CTE$ = LEFT$(UE.NOMBRE.CTE$,38)
             ENDIF \
             ELSE \
             BEGIN
               UE.NOMBRE.CTE$ = "SIN NOMBRE"
             ENDIF

           ENDIF
         ENDIF                              ! Data read
         TS.ER.RETURN = 0
       ENDIF                                ! Input from MSR
     ENDIF                                  ! Not I/O processor
CONTINUAR:
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
!
Sub changeTV(pNewTV$)
	String pNewTV$
	Integer*2 tmpTendType0%, tmpTendType1%, tmpTendVar1%
	!
	Call EP.BUSCA.LIMITE.TV(pNewTV$)
	!
	! Ajusta los totales
	tmpTendType0% = SL.TE.TENDTYPE
	tmpTendType1% = Int%(Val(Left$(pNewTV$, 1)))
	tmpTendVar1% = Int%(Val(Right$(pNewTV$, 1)))
	SL.TE.TENDTYPE  = tmpTendType1%
	SL.TE.TENDVAR   = tmpTendVar1%
	TS.IO.DATA$(3) = Str$(SL.TE.TENDVAR)
	TS.IO.KEYS(7) = SL.TE.TENDTYPE + 90
	!
	If TS.NUMTNDRS(TS.TDR.INDEX) > 0 Then Begin
		TS.NUMTNDRS(EP.TV.POS%) = TS.NUMTNDRS(EP.TV.POS%) + 1
		TS.NUMTNDRS(TS.TDR.INDEX) = TS.NUMTNDRS(TS.TDR.INDEX) - 1
		TS.TENDERED(tmpTendType0%) = TS.TENDERED(tmpTendType0%) - SL.TE.AMTTENDE
		TS.TENDERED(tmpTendType1%) = TS.TENDERED(tmpTendType1%) + SL.TE.AMTTENDE
	Endif
	!
	TS.TENDVAMT(EP.TV.POS%) = TS.TENDVAMT(EP.TV.POS%) + SL.TE.AMTTENDE
	TS.TENDVAMT(TS.TDR.INDEX) = TS.TENDVAMT(TS.TDR.INDEX) - SL.TE.AMTTENDE
	!
	TS.TDR.INDEX = EP.TV.POS%
End Sub
!
Sub getConvenioTender(pTV$, pEntity%)
	String pTV$, tmpKey$, tmpValue$
	Integer*2 pEntity%
	!
	Call printDebug("getConvenioTender(" + pTV$ + ", " + Str$(pEntity%) + ")")
	!
	pTV$ = ""
	pEntity% = 0
	tmpKey$ = \
			"TENDER_DEF_" + EP.APPL$ + "_" + \
			EP.ECR.FUNCTION$ + "_" + \
			EP.OPT.CONVENIO$ + "_" + EP.OPT.BOLSILLO$
	tmpValue$ = getProperty(tmpKey$, tefParamData$)
	Call printDebug("key="+tmpKey$)
	If Len(tmpValue$) = 0 Then Begin
		tmpKey$ = \
				"TENDER_DEF_" + EP.APPL$ + "_" + \
				"XX" + "_" + \
				EP.OPT.CONVENIO$ + "_" + EP.OPT.BOLSILLO$
		tmpValue$ = getProperty(tmpKey$, tefParamData$)
		Call printDebug("key="+tmpKey$)
	Endif
	If Len(tmpValue$) = 0 Then Begin
		tmpKey$ = \
				"TENDER_DEF_" + EP.APPL$ + "_" + \
				EP.ECR.FUNCTION$ + "_" + \
				EP.OPT.CONVENIO$ + "_" + "XX"
		tmpValue$ = getProperty(tmpKey$, tefParamData$)
		Call printDebug("key="+tmpKey$)
	Endif
	If Len(tmpValue$) = 0 Then Begin
		tmpKey$ = \
				"TENDER_DEF_" + EP.APPL$ + "_" + \
				"XX" + "_" + \
				EP.OPT.CONVENIO$ + "_" + "XX"
		tmpValue$ = getProperty(tmpKey$, tefParamData$)
		Call printDebug("key="+tmpKey$)
	Endif
	Call printDebug("value="+tmpValue$)
	If Len(tmpValue$) > 1 Then Begin
		pTV$ = Left$(tmpValue$, 2)
		If Len(tmpValue$) > 2 Then Begin
			pEntity% = Int%(Val(Right$(tmpValue$, Len(tmpValue$) - 2)))
		Endif
	Endif
	!
	Call printDebug("tv="+pTV$+","+Str$( pEntity%))
End Sub
!
! En esta rutina se revisa si el medio de pago
! (tipo, variedad y entidad) cambia con el convenio 
! y bolsillo recibido en la respuesta
Sub postEvaluateTender
	String tmpTV0$, tmpTV1$
	Integer*2 tmpEntity0%, tmpEntity1%
	Integer*1 tmpVerifyDisc%
	!
	Call printDebug("postEvaluateTender EP.OPT.CONVENIO$=" + EP.OPT.CONVENIO$ + " EP.OPT.BOLSILLO$="+EP.OPT.BOLSILLO$)
	tmpVerifyDisc% = 0
	If EP.OPT.CONVENIO$ <> "" And EP.OPT.BOLSILLO$ <> "" Then Begin
		tmpTV0$ = Str$(SL.TE.TENDTYPE) + Str$(SL.TE.TENDVAR)
		tmpEntity0% = getInstitucion
		Call getConvenioTender(tmpTV1$, tmpEntity1%)
		If Len(tmpTV1$) > 0 Then Begin
			If tmpTV0$ <> tmpTV1$ Then Begin
				tmpVerifyDisc% = -1
				Call changeTV(tmpTV1$)
			Endif
			If tmpEntity0% <> tmpEntity1% Then Begin
				tmpVerifyDisc% = -1
				Call reasignEntity(Int%(Val(tmpTV1$)), tmpEntity1%)
			Endif
		Endif
	Endif
	If tmpVerifyDisc% And TS.PROCEDURE < 1 Then Begin
		tmpVerifyDisc% = disc.validateTender(SL.TE.AMTTENDE, tmpTV1$, tmpEntity1%, EP.OPT.BOLSILLO$)
		If tmpVerifyDisc% = 2 Then tmpVerifyDisc% = 1
   	If tmpVerifyDisc% <> 1 Then Begin
			If tmpVerifyDisc% = 0 Then Begin
				!Se otorgó un descuento que no correspondía
				!al medio de pago en el cual finalmente se va a
				!contabilizar la transacción.
				!Es necesario anular el descuento
				Call EP.DISPLAY.AN.ERROR("Tarj no corresponde Se anula descuento")
				Call disc.voidDiscountExt
			Endif Else Begin
				!No se ofreció un descuento al cual se tiene
				!derecho por el medio de pago ingresado.
				!Es necesario anular la transacción TEF
				EP.APPROV.CODE$ = "97"
				Call clearEntity
				EP.APPROV.DESC$ = "Hay dcto disponible"
			Endif
		Endif
	Endif
End Sub

FUNCTION EP.CHECK.DIGIT(UE.ACCOUNT.NBR$) EXTERNAL 
  STRING UE.ACCOUNT.NBR$, EP.CHECK.DIGIT, UE.ADD.QTY$
  INTEGER*2 UE.POS%, UE.LEN%, UE.TOTAL%, UE.10%, UE.ADD.QTY%
END FUNCTION

!-----------------------------------------------------------
!  	Electronic funds transfer API routine
!-----------------------------------------------------------
!                  EFT Common Application Routines
!
SUB EP.STORE.VOUCHER.TEF(UE.PRINT.LINE$,UE.PRT.CUT$) PUBLIC 
  STRING UE.PRINT.LINE$, UE.PRT.CUT$
  EP.TEF.POINTER1% = EP.TEF.POINTER1% + 1
  IF LEN(UE.PRINT.LINE$) <= 38 THEN \
    EP.VOUCHER.TEF$(EP.TEF.POINTER1%) = UE.PRINT.LINE$ \
  ELSE \
    EP.VOUCHER.TEF$(EP.TEF.POINTER1%) = LEFT$(UE.PRINT.LINE$,38)
  EP.CUT.VOUCHER.TEF%(EP.TEF.POINTER1%) = VAL(UE.PRT.CUT$) 
END SUB
!
SUB EP.PRINT.TEF.VOUCHER PUBLIC
  INTEGER*2 EP.COUNT%,voucherEstacion%
  !
  voucherEstacion% = 6100H
  !
  TO.TRAILERLINE2$ = STRING$(38,"_")
  IF EP.TEF.POINTER1% >= 2 THEN \
  BEGIN
    CALL EP.SAVE.PRINT
    TO.USEREXIT(20) = 0
    FOR EP.COUNT% = 1 TO EP.TEF.POINTER1%
      IF EP.CUT.VOUCHER.TEF%(EP.COUNT%) = 1 THEN \
      BEGIN 
        CALL EP.LINE.PRINT(EP.SAVE.GLINE$,voucherEstacion%) 
        TS.LINETYPE = 18 
        TS.LINEDATA = 99
        TO.HEADERLINE1$  = STRING$((38-LEN(EP.TEF.PRODUCTO$))/2," ") + EP.TEF.PRODUCTO$
        TO.HEADERLINE2$  = STRING$(38," ")
        CALL TSPREC01         
        TO.HEADERLINE1$ = EP.SAV.HD1$
        TO.HEADERLINE2$ = EP.SAV.HD2$
      ENDIF  \
      ELSE \
        CALL EP.LINE.PRINT(EP.VOUCHER.TEF$(EP.COUNT%),voucherEstacion%) 
    NEXT EP.COUNT% 
  ENDIF
!  CALL EP.LINE.PRINT(EP.SAVE.GLINE$,4100H) 
!  TO.HEADERLINE1$ = EP.SAV.HD1$
!  TO.HEADERLINE2$ = EP.SAV.HD2$
!  TS.LINETYPE = 18 
!  TS.LINEDATA = 0
!  CALL TSPREC01
  CALL EP.LINE.PRINT(EP.SAVE.GLINE$,voucherEstacion%) 
  TO.HEADERLINE1$ = EP.SAV.HD1$
  TO.HEADERLINE2$ = EP.SAV.HD2$
  TO.TRAILERLINE1$ = EP.SAV.TR1$
  TO.TRAILERLINE2$ = EP.SAV.TR2$
  CALL EP.RESTORE.PRINT
  TO.USEREXIT(20) = -1
END SUB
!
SUB EP.RESET.TEF.VOUCHER PUBLIC
  DIM EP.VOUCHER.TEF$(300)
  DIM EP.CUT.VOUCHER.TEF%(300)
  EP.TEF.POINTER1%  = 0
END SUB

SUB EP.PRINT.TEF.HEADER PUBLIC
  INTEGER*2 EP.COUNT%
!
  IF EP.TEF.POINTER1% >= 2 THEN \
  BEGIN
    TO.USEREXIT(20) = 0
    TO.USEREXIT(60) = 0
    CALL EP.SAVE.PRINT
    TS.LINETYPE = 29
    TS.SAVPRT$ = STRING$(38," ")
    TS.SAVPRT.OPT = 4100H
    CALL TSPREC01
!    TO.HEADERLINE1$  = LEFT$("       TARJETA COMPENSAR     ",38)
!    TO.HEADERLINE2$  = STRING$(38," ")
    TS.LINETYPE = 18 
!    TS.LINEDATA = 99
    TS.LINEDATA = 0
    CALL TSPREC01
    TO.HEADERLINE1$ = EP.SAV.HD1$
    TO.HEADERLINE2$ = EP.SAV.HD2$
    TO.TRAILERLINE1$ = EP.SAV.TR1$
    TO.TRAILERLINE2$ = EP.SAV.TR2$
    CALL EP.RESTORE.PRINT
    TO.USEREXIT(20) = -1
    TO.USEREXIT(60) = -1
  ENDIF
END SUB
!
SUB EP.PRINT.TEF.HEADER.CUT PUBLIC
  INTEGER*2 EP.COUNT%
  TO.USEREXIT(20) = 0
  TO.USEREXIT(60) = 0
  CALL EP.SAVE.PRINT
!  TO.HEADERLINE1$  = EP.ESC$+CHR$(58)+LEFT$(EP.EFT.LINE$(1),30)
  TO.HEADERLINE1$  = STRING$((38-LEN(EP.TEF.PRODUCTO$))/2," ") + EP.TEF.PRODUCTO$
  TO.HEADERLINE2$  = STRING$(38," ")
  TS.LINETYPE = 18 
  TS.LINEDATA = 0
  CALL TSPREC01
  TO.HEADERLINE1$ = EP.SAV.HD1$
  TO.HEADERLINE2$ = EP.SAV.HD2$
  TO.TRAILERLINE1$ = EP.SAV.TR1$
  TO.TRAILERLINE2$ = EP.SAV.TR2$
  CALL EP.RESTORE.PRINT
  TO.USEREXIT(20) = -1
  TO.USEREXIT(60) = -1
END SUB


SUB UETEF02 PUBLIC
  IF EP.EFT.ACTIVO% AND \
     EP.EFT.CMP.TRX%    THEN \
  BEGIN
!    CALL EP.PRINT.TEF.HEADER       
    CALL EP.PRINT.TEF.VOUCHER
    CALL EP.PRINT.TEF.HEADER.CUT
    EP.INTERCHG% = -1
    CALL EP.PRINT.TEF.VOUCHER
    CALL EP.PRINT.TEF.HEADER
  ENDIF
  CALL EP.RESET.TEF.VOUCHER
  EP.medioPagoTef% = 0
  EP.EFT.CMP.TRX%  = 0
  EP.TEF.DETAIL% = 0
  EP.TEF.SMA%    = 0
  EP.FLAG.DEV%    = 0
  !
  If tefVoucherCount% > 0 Then Begin
  	Call printTefVouchers
  Endif
  !
  tefVoucherCount% = 0
  !
  Dim tefVouchers$(0)
END SUB
!
SUB UETEF05 PUBLIC
  IF EP.EFT.ACTIVO% AND \
     EP.EFT.CMP.TRX%    THEN \
  BEGIN
!    CALL EP.PRINT.TEF.HEADER       
    !
    If ts.io.keys(1) <> 70 Or ts.io.keys(6) <> 81 Then Begin
	    CALL EP.PRINT.TEF.VOUCHER
	    CALL EP.PRINT.TEF.HEADER.CUT
	    EP.INTERCHG% = -1
	    CALL EP.PRINT.TEF.VOUCHER
	    CALL EP.PRINT.TEF.HEADER
	  Endif
	  !
  ENDIF
  !
  CALL EP.RESET.TEF.VOUCHER
  EP.EFT.CMP.TRX%  = 0
  EP.TEF.DETAIL% = 0
  EP.TEF.SMA%    = 0
  EP.FLAG.DEV%    = 0
  !
  If tefVoucherCount% > 0 Then Begin
  	Call printTefVouchers
  Endif
  !
  tefVoucherCount% = 0
  !
  Dim tefVouchers$(0)
END SUB


SUB UETEF07 PUBLIC
  String tmpPropData$
  Integer*2 tmpReturn%
  !
  Call EP.LINE.PRINT("APITEF5 v.3.0 Feb-2022", 2100h)
  !
  IF EP.EFT.ACTIVO% THEN \
  BEGIN
    CALL EP.RESET.TEF.VOUCHER
    EP.EFT.CMP.TRX%    = 0
    EP.TEF.DETAIL% = 0
    EP.TEF.SMA%    = 0
    EP.FLAG.DEV%   = 0
  ENDIF
  TP.PREF.MEDIO.PAGO$ = "603959"   ! azulito para garantizar la existencia del prefijo
  EP.TV.COMPENSAR$ = "64"   ! azulito mientras entra tef generico
  EP.medioPagoTef% = 0
  !
  ! Carga de parámetros para TEF Genérico
  tefParamData$ = ""
  Call readPropData("r::adx_idt1:apitef5.pro", tmpPropData$, EP.IOPARM%, tmpReturn%)
  If tmpReturn% = 0 Then Begin
  	tefParamData$ = tmpPropData$
  	Call initGenericTefParam(tmpPropData$)
  Endif Else Begin
  	Call defaultGenericTefParam
  Endif
END SUB


SUB EP.SEND.TO.THREADER EXTERNAL
!
!   call Java appl manager routine
END SUB
!
SUB EP.PARSE.THREADER.RESPONSE EXTERNAL
!
END SUB
!
Sub EP.PARSE.DATA.REGULAR External
End Sub
!
Sub EP.ADD.DATA.REGULAR External
End Sub
!
SUB EP.PARSE.DISPLAY.REQUEST EXTERNAL
!
END SUB
!
!
SUB EP.PARSE.PRT.HEADER EXTERNAL
!
END SUB
!
SUB EP.PARSE.PRT.LINE EXTERNAL
!
END SUB
!
SUB EP.PARSE.PRT.CLOSE EXTERNAL
!
END SUB
!
SUB EP.PARSE.DATA.REQUEST EXTERNAL
!
END SUB
!
SUB EP.PARSE.DENTRY.REQUEST EXTERNAL
!
END SUB
!
SUB EP.PARSE.DENTRY99.REQUEST EXTERNAL
!
END SUB

SUB EPAY.REVERSE.TRX EXTERNAL

END SUB
!
SUB EP.REVERSE.TRX EXTERNAL
!
END SUB
!
FUNCTION EP.SEND.REVERSE(reverseAppl$) EXTERNAL
	String reverseAppl$
	Integer*1 EP.SEND.REVERSE
END FUNCTION
!
SUB EP.REVERSE.ANULTTL PUBLIC
!
  EP.TRX.PEND$     = EP.EPAY.TRANSNUM$
  EP.FECHA.PEND$   = EP.ECR.DATETIME$
  EP.REV.FUNCTION$ = "09"
  CALL EP.REVERSE.TRX
!
END SUB
!
FUNCTION EP.identificarProducto(pToken$, pAppl$)
	String pToken$, pAppl$, parteVariable$,respuestaAppl$
	Integer*1 EP.identificarProducto
	!
	EP.BIN.PRODUCTO$ = ""
	parteVariable$ = \
		EP.VALOR.EPAY$ + EP.TAX.EPAY$ + EP.IVA.BASE$ + \
		EP.PROP.EPAY$ + RIGHT$(STRING$(10,"0") + STR$(TS.BALDUE(0)),10) + \
		"00" + Left$(pToken$ + STRING$(20," "), 20) + STRING$(4," ") + \
		STRING$(20," ") + STRING$(4," ") + \
		EP.MSR.DATA$
	!
	respuestaAppl$ = EP.invocarApplManager(pAppl$,"21",parteVariable$,0)
	If Len(respuestaAppl$) >= 54 Then \
	Begin
		EP.TEF.CEDULA$ = MID$(respuestaAppl$,9,20)
		EP.TEF.BOLSILLO$ = MID$(respuestaAppl$,29,4)
		EP.TIPO.VAR$ = MID$(respuestaAppl$,33,2)
		EP.TEF.PRODUCTO$ = MID$(respuestaAppl$,35,20)
		EP.BIN.PRODUCTO$ = MID$(respuestaAppl$,55,6)
		EP.identificarProducto = -1
	Endif Else \
		EP.identificarProducto = 0
END FUNCTION   
!
!
SUB EP.APITEF.14(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,       \
  EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$,    \
  EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$, pBalanceReq%) PUBLIC
!
  STRING EP.ERRFX$, tmpTefData$, tmpApplName$, tmpAccountDesc$
  Integer*1 pBalanceReq%, tmpSale%, tmpVoid%, tmpCash%, tmpRequestId%, tmpValidFunction%
!
! tpv application interface data
!
   STRING              \
    EP.APPL$,          \
    EP.ECR.FUNCTION$,  \
    EP.TRX.STATUS$,    \
    EP.AMJ.STATUS$,    \
    EP.INVOICE.NBR$,   \
    EP.ECR.TRANSNUM$,  \
    EP.VALOR.EFT$,     \
    EP.TAX.EFT$,       \
    EP.CUOTAS.QTY$,    \
    EP.APPROV.CODE$,   \
    EP.APPROV.DESC$,   \
    EP.CARD.NUMBER$,   \
    EP.AUTH.NUMBER$,   \
    EP.TIPO.VAR$,      \
    EP.USER.DATA$

  	EP.TEF.CEDULA$ = ""
	EP.TEF.BOLSILLO$ = ""
	EP.TEF.PRODUCTO$ = ""
	EP.TEF.TOKEN$ = ""
	!
	tefEntity$ = "0"
	!

! Transmision control data
!
  IF NOT EP.SEND.REVERSE(EP.APPL$) THEN BEGIN
    EP.AMJ.STATUS$ = EP.REV.STATUS$
    EP.TRX.STATUS$ = EP.REVTRX.STATUS$
    EXIT SUB    
  ENDIF
  !
  If Not EP.SEND.REVERSE( "11" ) Then Begin
  	EP.APPL$ = "11"
  	EP.AMJ.STATUS$ = EP.REV.STATUS$
  	EP.TRX.STATUS$ = EP.REVTRX.STATUS$
  	Exit Sub
  Endif
  !
!  IF EP.TRX.PEND$ <> "" THEN   \
!  BEGIN
!    CALL EP.REVERSE.TRX 
!    IF EP.TRX.PEND$ <> "" THEN \
!    BEGIN
!      EP.AMJ.STATUS$ = EP.REV.STATUS$
!      EP.TRX.STATUS$ = EP.REVTRX.STATUS$
!      EXIT SUB      
!    ENDIF
!  ENDIF
!
  EP.FECHA.INIC$  = DATE$ + TIME$
  EP.CAJERO$      = RIGHT$(STRING$(10,"0") + UNPACK$(TS.OPER$),10)
  EP.AMJ.STATUS$  = "2"
  EP.TRX.STATUS$  = "0"
  EP.APPL.STATUS$ = "00"
  EP.INVOICE.NBR$ = RIGHT$(STRING$(6,"0")+STR$(SL.HD.TRANSNUM+1),6)
  EP.VALOR.EPAY$  = RIGHT$(STRING$(10,"0") + EP.VALOR.EFT$, 10)
  EP.TAX.EPAY$    = RIGHT$(STRING$(10,"0") + EP.TAX.EFT$, 10) 
  EP.IVA.BASE$    = RIGHT$(STRING$(10,"0") + EP.IVA.BASE$, 10)
  EP.CASHBACK$    = RIGHT$(STRING$(10,"0") + EP.CASHBACK$,10)
  EP.PROP.EPAY$   = STRING$(10,"0")
  EP.RRN$         = ""
  EP.USER.DATA$   = ""
  EP.STATE%       = 1
  EP.EXCEPTION$   = ""
  EP.ANUL.TRANSNUM$ = STRING$(6,"0")
  EP.ANUL.AUTH$     = STRING$(6,"0")
  EP.ANUL.INVOICE$  = STRING$(6,"0")
  EP.ANUL.POSTEO$   = STRING$(4,"0")
  EP.ANUL.CARD$     = STRING$(20,"0")
  !
  !EP.ANUL.RRN$      = STRING$(12,"0")
  !
  EP.ANUL.RRN$      = STRING$(32,"0")
!
  CALL EP.SAVE.KEYS
!
  EP.FUNCTION$ = EP.ECR.FUNCTION$
!
  IF EP.FUNCTION$ = "01" THEN \
  BEGIN    
    IF NOT EP.SALDO.ACTIVO% THEN   \
    BEGIN
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "s"
      EXIT SUB
    ENDIF
  ENDIF 
!
!  
!  
  EP.FUNCTION$    = EP.ECR.FUNCTION$
  EP.KEYB.DATA$ = ""
  EP.MSR.DATA$  = ""
!  CALL EP.GET.KBDATA("1=TARJ COMPENSAR    BORRAR=SALIR", "1" , "1", \
!    EP.KEYB.DATA$)
                     
!  CALL EP.GET.TRX.TYPE("1=BONO   2=DESCTO   BORRAR=SALIR",EP.MSR.DATA$,EP.KEYB.DATA$)
!
!  IF TS.IO.MOTORKEY = 73 THEN \  ! si digitan borrar
!  BEGIN
!    EP.AMJ.STATUS$ = "2"         ! termina transaccion
!    EP.TRX.STATUS$ = "K"         ! abandono de operador
!    EXIT SUB
!  ENDIF
!
!
EP.KEYB.DATA$ = "1"
!  IF EP.KEYB.DATA$ = "1" THEN \    ! 
!  BEGIN

    !-----------------------------------------------------------------------------
    ! 2020-11-03 jsv
    ! Ahora se despliega menú de tef genérico
    !-----------------------------------------------------------------------------
    !EP.APPL$ = "09"               ! aplicacion de tarjeta compensar
    !EP.medioPagoTef% = -1
    !EP.TIPO.VAR$ = EP.TV.COMPENSAR$
    !-----------------------------------------------------------------------------
    !
!  ENDIF
!	 
!  CALL EP.BUSCA.LIMITE.TV(EP.TIPO.VAR$)
!
!  IF EP.KEYB.DATA$ = "1" THEN \    !
!  BEGIN  
    !
    tmpTefData$ = selectTefAppl
    If Len(tmpTefData$) = 0 Then Begin
    	EP.AMJ.STATUS$ = "2"         ! termina transaccion
    	EP.TRX.STATUS$ = "K"         ! abandono de operador
    	EP.medioPagoTef% = 0
    	EXIT SUB
    Endif
    Call APITEF5.parseTefDef(tmpTefData$, EP.FUNCTION$, tmpApplName$, EP.APPL$, EP.TIPO.VAR$, tefEntity$, tmpSale%, tmpVoid%, tmpCash%, tmpRequestId%, pBalanceReq%)
    !
    ! Valida si la aplicación seleccionada soporta la operación actual
    tmpValidFunction% = -1
    If EP.FUNCTION$ = "03" Then Begin
    	! Compra
    	tmpValidFunction% = tmpSale%
  	Endif Else If EP.FUNCTION$ = "05" Or EP.FUNCTION$ = "12" Then Begin
  		! Anulación
  		tmpValidFunction% = tmpVoid%
  	Endif Else If EP.FUNCTION$ = "51" Then Begin
  		! Retiro en efectivo
  		tmpValidFunction% = tmpCash%
  	Endif
  	!
  	If Not tmpValidFunction% Then Begin
			EP.AMJ.STATUS$ = "2"         ! termina transaccion
			EP.TRX.STATUS$ = "s"         ! transacción no permitida
			EP.medioPagoTef% = 0
			EXIT SUB
		Endif  		
    !
    EP.medioPagoTef% = -1
    !
    EP.KEYB.DATA$ = ""
    EP.MSR.DATA$  = ""
    TO.USEREXIT(14) = 0
    tmpAccountDesc$ = getInputAccountDescriptor(EP.APPL$)
    If EP.APPL$ = "09" Then Begin
	    CALL EP.GET.TARJ.NBR(tmpAccountDesc$,EP.MSR.DATA$,  EP.KEYB.DATA$,   EP.ALL.CARD$, \
	                    EP.FECHA.VENC$, EP.TEF.TOKEN$)
	    If Len(EP.TEF.TOKEN$) > 0 Then Begin
    		EP.APPL.JAVA$ = "11"     ! operaciones con token
    	Endif Else Begin
    		EP.APPL.JAVA$ = EP.APPL$
    	Endif
	  Endif Else Begin
	  	EP.APPL.JAVA$ = EP.APPL$
	  	If Len(APITEF5.externalAccount$) > 0 Then Begin
	  		EP.ALL.CARD$ = APITEF5.externalAccount$
	  		APITEF5.externalAccount$ = ""
	  		TS.IO.MOTORKEY = 80
	  	Endif Else If tmpAccountDesc$ = "@EMPTY" Then Begin
	  		EP.ALL.CARD$ = String$(20, "9")
	  		TS.IO.MOTORKEY = 80
	  	Endif Else Begin
	  		Call EP.GET.KBDATA(tmpAccountDesc$, "1" , "9X20", EP.ALL.CARD$)
	  	Endif
	  	EP.FECHA.VENC$ = "0000"
	  	If Len(APITEF5.externalCustomer$) > 0 Then Begin
	  		EP.TEF.CEDULA$ = APITEF5.externalCustomer$
	  	Endif
	  Endif
    TO.USEREXIT(14) = -1
    
!  ENDIF
!
  If 																						\
		TS.IO.MOTORKEY = 73 Or 											\ ! si digitan borrar
		(EP.MSR.DATA$ = "" And 											\
		 EP.TEF.TOKEN$ = "" And 										\
		 EP.ALL.CARD$ = "") 												\ ! o no ingresan datos
  Then Begin
    EP.AMJ.STATUS$ = "2"         ! termina transaccion
    EP.TRX.STATUS$ = "K"         ! abandono de operador
    EP.medioPagoTef% = 0
    EXIT SUB
  Endif
  !
  If tmpRequestId% Then Begin
  	EP.TEF.CEDULA$ = requestForId
  	If Len(EP.TEF.CEDULA$) < tefIdTypeLen% Then Begin
  		EP.AMJ.STATUS$ = "2"
  		EP.TRX.STATUS$ = "K"
  		EP.medioPagoTef% = 0
  		Exit Sub
  	Endif
  	If tmpAccountDesc$= "@EMPTY" Then Begin
  		EP.ALL.CARD$ = Right$(EP.TEF.CEDULA$, Len(EP.TEF.CEDULA$) - tefIdTypeLen%)
  	Endif
  Endif
  !
!
  If EP.APPL$ = "09" Or EP.APPL$ = "11" Then Begin
	  ! Invoca funcion de java para identificar el tipo de producto
	  ! y solicitar el bolsillo deseado
	  IF NOT EP.identificarProducto(EP.TEF.TOKEN$, EP.APPL.JAVA$) THEN \
	  BEGIN
	    EP.AMJ.STATUS$ = "2"         ! termina transaccion
	    EP.TRX.STATUS$ = "K"         ! abandono de operador
	    EP.medioPagoTef% = 0
	    EXIT SUB
	  ENDIF
	Endif
  CALL EP.BUSCA.LIMITE.TV(EP.TIPO.VAR$)
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "funct1="+ EP.FUNCTION$+  \
!                 " keyb="+ EP.KEYB.DATA$ + \
!                 " ecr="+ EP.ECR.FUNCTION$
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------
!
  EP.ANUL.TYPE% = 0
!
  IF EP.FUNCTION$ = "05" THEN \   ! si es anulacion 
  BEGIN
    IF TS.PROCEDURE < 1 THEN \    ! si es venta normal
      EP.ANUL.TYPE% = 1 \         ! anulacion de compra en actual trx
    ELSE \
      EP.ANUL.TYPE% = 2           ! anulacion de compra anterior trx
  ENDIF
!---------------------------------------------------------
!  IF NOT EP.NO.TRACE% THEN \
!  BEGIN
!    TS.LINETYPE = 29
!    TS.SAVPRT$ = "funct2="+ EP.FUNCTION$ + \
!                 " keyb="+ EP.KEYB.DATA$ + \
!                 " ecr="+ EP.ECR.FUNCTION$
!    TS.SAVPRT.OPT = 4100H
!    CALL TSPREC01
!  ENDIF
!---------------------------------------------------------
!
!  call ep.line.print("14lnmsr="+str$(len(EP.MSR.DATA$))+"kb="+EP.KEYB.DATA$, 4100H) 
!  call ep.line.print("card="+EP.ALL.CARD$+"fv="+EP.FECHA.VENC$, 4100H) 
!
!  
!     ojo falta dialogo para abono a medio de pago
!
  IF EP.FUNCTION$ = "04" OR    \   ! anulacion de abono a medio de pago
     EP.FUNCTION$ = "05" THEN  \   ! anulacion de compra
  BEGIN    
    EP.KEYB.DATA$ = ""
    CALL EP.GET.KBDATA("NRO VOUCHER ANULAR?", "1" , "999999", \
    EP.KEYB.DATA$)
!
    IF TS.IO.MOTORKEY = 73 THEN \
    BEGIN
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "K"
      EXIT SUB
    ENDIF
    EP.ECR.TRANSNUM$ = EP.KEYB.DATA$
  ENDIF
!
!  
  IF EP.FUNCTION$ = "04" OR    \      ! anulacion de abono a medio de pago
     EP.FUNCTION$ = "05" THEN \       ! anulacion de transaccion de COMPRA
  BEGIN
    IF LEN(EP.ECR.TRANSNUM$) > 0 THEN \
    BEGIN 
      IF VAL(EP.ECR.TRANSNUM$) = 0 THEN \     ! No viene trx para anular
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "N"
        EXIT SUB
      ENDIF    
    ENDIF
  ENDIF
!
!    
!  call ep.line.print("APPL="+EP.APPL$ + " FUNC="+EP.FUNCTION$, 4100H) 

  IF EP.FUNCTION$ = "04" OR    \   ! anulacion de abono a medio de pago
     EP.FUNCTION$ = "05" THEN \          ! anulacion de transaccion de COMPRA
  BEGIN
    IF TS.PROCEDURE = 2 OR  \              ! Anulacion de una transaccion anterior 
       TS.TOTALS(0,0,0) < 0 THEN \         ! es devolucion  
    BEGIN 
      EP.DE1102.FOUND% = 0
      EP.DE1103.FOUND% = 0
      EP.DE1203.FOUND% = 0
      EP.APPROV.FOUND% = 0
      EP.EFT.FOUND% = 0
      TS.ER.RETURN = -1
      OPEN "R::$FTTRX" KEYED RECL 158 AS EP.IOPARM% NOWRITE NODEL
      IF NOT TS.ER.RETURN  THEN \   ! si no abre
      BEGIN 
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "V"
        EXIT SUB
      ENDIF
!
      IF EP.FUNCTION$ = "04" THEN \
        EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
         EP.APPL$ + "02"+ EP.ECR.TRANSNUM$ + "01" \
      ELSE \
        EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
         EP.APPL$ + "03"+ EP.ECR.TRANSNUM$ + "01" 
      TS.ER.RETURN = -1 
!      call ep.line.print("KEY1="+EP.KEY$, 4100H) 
      
      !
      !//TODO: Importante: Ampliar archivo EFTTRX para soportar RRN de 32
      !
      READ FORM "C4 C12 4C20 2C40 C2"; #EP.IOPARM%         \
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
        EP.DE.ALLCARD$ = LEFT$(EP.DE2.DATA$ + STRING$(20,"0"),20)
        EP.DE.INVOICE.NBR$ = LEFT$(MID$(EP.DE6.DATA$,11,6) + STRING$(6,"0"),6)
      ENDIF
!    
      IF EP.FUNCTION$ = "04" THEN \
        EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
         EP.APPL$ + "02"+ EP.ECR.TRANSNUM$ + "02" \
      ELSE \
        EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
         EP.APPL$ + "03"+ EP.ECR.TRANSNUM$ + "02" 
      TS.ER.RETURN = -1 
!      call ep.line.print("KEY2="+EP.KEY$, 4100H) 

      READ FORM "C4 C12 4C20 2C40 C2"; #EP.IOPARM%         \
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
      ELSE \
      BEGIN
        EP.DE1102.FOUND% = -1
        EP.DE.ISOCOD$    = MID$(EP.DE2.DATA$,19,2)
        EP.DE.POSTEO$    = LEFT$(EP.DE3.DATA$,4)
        EP.DE.CODPRO$    = MID$(EP.DE3.DATA$,5,6)
        EP.DE.CARD$      = MID$(EP.DE3.DATA$,11,4)
        EP.DE.AUTH$      = MID$(EP.DE3.DATA$,15,6)
        EP.DE.AMOUNT$    = LEFT$(EP.DE4.DATA$,10)
        EP.DE.PROPINA$   = "00" + MID$(EP.DE5.DATA$,13,8)
        !
        !EP.DE.RRN$       = LEFT$(EP.DE6.DATA$,12)
        !EP.DE.CUOTAS$    = MID$(EP.DE6.DATA$,13,2)  
        !EP.DE.TIPOVAR$   = MID$(EP.DE6.DATA$,15,2)
        !
        EP.DE.RRN$       = LEFT$(EP.DE6.DATA$,32)
        EP.DE.CUOTAS$    = MID$(EP.DE6.DATA$,33,2)  
        EP.DE.TIPOVAR$   = MID$(EP.DE6.DATA$,35,2)
        !
        IF EP.DE.ISOCOD$ = "00" THEN \
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
      ENDIF
      IF EP.FUNCTION$ = "04" THEN \
        EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
         EP.APPL$ + "02"+ EP.ECR.TRANSNUM$ + "03" \
      ELSE \
        EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
         EP.APPL$ + "03"+ EP.ECR.TRANSNUM$ + "03" 
      TS.ER.RETURN = -1 
      READ FORM "C4 C12 4C20 2C40 C2"; #EP.IOPARM%             \
        KEY EP.KEY$ ;                                     \
        EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,  \
        EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$,         \
        EP.C$, EP.A$
      IF NOT TS.ER.RETURN THEN \  ! si no existe
      BEGIN 
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "U" 
        CLOSE EP.IOPARM%
        EXIT SUB
      ENDIF 
!       
      EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
         EP.APPL$ + "10"+ EP.ECR.TRANSNUM$ + "03"

      TS.ER.RETURN = -1 
      READ FORM "C4 C12 4C20 2C40 C2"; #EP.IOPARM%         \
        KEY EP.KEY$ ;                                     \
        EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,  \
        EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$,         \
        EP.C$, EP.A$
!
      IF TS.ER.RETURN THEN \           ! si existe
      BEGIN 
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "L"
        CLOSE EP.IOPARM%
        EXIT SUB
      ENDIF

      CLOSE EP.IOPARM%
!
      IF EP.FUNCTION$ = "04" OR \
         EP.FUNCTION$ = "05" THEN \
      IF VAL(EP.VALOR.EFT$) <> VAL(EP.DE.AMOUNT$) THEN \ ! No coincide valor de transaccion
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "M"
        EXIT SUB
      ENDIF
!       
      EP.ANUL.TRANSNUM$ = EP.ECR.TRANSNUM$
      EP.ANUL.INVOICE$  = EP.DE.INVOICE.NBR$
      EP.ANUL.POSTEO$   = EP.DE.POSTEO$
      EP.ANUL.CODPRO$   = EP.DE.CODPRO$
      EP.ANUL.CARD$     = EP.DE.ALLCARD$
      EP.ANUL.AUTH$     = EP.DE.AUTH$
!    
    ENDIF \                 ! fin de transaccion de anulacion de transaccion anterior 
    ELSE \                  ! anulacion de una autorizacion dentro de la misma transaccion 
    BEGIN
      EP.DE1102.FOUND% = 0
      EP.APPROV.FOUND% = 0
      EP.DE1103.FOUND% = 0
      EP.DE1203.FOUND% = 0
      FOR EP.M% = 1 TO SL.END 
        IF SL.STR$(EP.M%) <> "" THEN \
        BEGIN 
          IF ASC(SL.STR$(EP.M%)) = 11H THEN \
          BEGIN
            EP.B$ = SL.STR$(EP.M%) + ":"
            EP.J% = 3 
            CALL EP.GETUNPK
            IF LEFT$(EP.A$,EP.LEN.CLAVE%) = EP.EFT.CLAVE$ THEN \  
            BEGIN
              IF EP.FUNCTION$ = "04" THEN \
                EP.KEY$ = EP.APPL$ + "02"+ EP.ECR.TRANSNUM$ + "02" \
              ELSE \
                EP.KEY$ = EP.APPL$ + "03"+ EP.ECR.TRANSNUM$ + "02" 
!
              IF MID$(EP.A$,EP.LEN.CLAVE% + 1,12) = EP.KEY$ THEN \
              BEGIN
                EP.DE1102.FOUND% = -1
                CALL EP.GETUNPK   
                EP.DE.ISOCOD$ = MID$(EP.A$,19,2)
                CALL EP.GETUNPK
                EP.DE.POSTEO$ = LEFT$(EP.A$,4)
                EP.DE.CODPRO$ = MID$(EP.A$,5,6)
                EP.DE.CARD$ = MID$(EP.A$,11,4)
                EP.DE.AUTH$ = MID$(EP.A$,15,6)
                CALL EP.GETUNPK
                EP.DE.AMOUNT$ = LEFT$(EP.A$,10)
                EP.DE.IVA$ = MID$(EP.A$,11,10)
                CALL EP.GETUNPK
                EP.DE.PROPINA$ = "00" + MID$(EP.A$,13,8)
                CALL EP.GETUNPK
                !
                !EP.DE.RRN$ =  LEFT$(EP.A$,12)
                !EP.DE.CUOTAS$ = MID$(EP.A$,13,2)  
                !EP.DE.TIPOVAR$ = MID$(EP.A$,15,2)
                !
                EP.DE.RRN$ =  LEFT$(EP.A$,32)
                EP.DE.CUOTAS$ = MID$(EP.A$,33,2)  
                EP.DE.TIPOVAR$ = MID$(EP.A$,35,2)
                !
                IF EP.DE.ISOCOD$ = "00" THEN \
                BEGIN
                  EP.APPROV.FOUND% = -1
                ENDIF
              ENDIF
!
              IF EP.FUNCTION$ = "04" THEN \
                EP.KEY$ = EP.APPL$ + "02"+ EP.ECR.TRANSNUM$ + "03" \
              ELSE \
                EP.KEY$ = EP.APPL$ + "03"+ EP.ECR.TRANSNUM$ + "03" 
!
              IF MID$(EP.A$,EP.LEN.CLAVE% + 1,12) = EP.KEY$ THEN \
              BEGIN
                EP.DE1103.FOUND% = -1
                CALL EP.GETUNPK
                EP.DE.IVA.BASE$ = LEFT$(EP.A$,10)
                EP.DE.CASHBACK$ = MID$(EP.A$,11,10)
              ENDIF 
!
              EP.KEY$ = EP.APPL$ + EP.FUNCTION$       ! solo aplicacion mas funcion de anulacion
!
              IF MID$(EP.A$,EP.LEN.CLAVE% + 1,4) = EP.KEY$ AND \ ! si alguna trx anulacion anterior
                 MID$(EP.A$,EP.LEN.CLAVE% +11,2) = "03"    THEN \ ! terminada exitosamente
              BEGIN
                CALL EP.GETUNPK
                EP.DE.IVA.BASE$ = LEFT$(EP.A$,10)
                EP.DE.CASHBACK$ = MID$(EP.A$,11,10)
                CALL EP.GETUNPK
                CALL EP.GETUNPK
                IF LEN(EP.A$) > 0 THEN \
                BEGIN
                  IF LEFT$(EP.A$,6) = EP.ECR.TRANSNUM$ THEN \   ! con el mismo numero trx ya anulada
                  BEGIN
                    EP.DE1203.FOUND% = -1 
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      NEXT EP.M%
!
!---------------------------------------------------------
!    IF NOT EP.NO.TRACE% THEN \
!    BEGIN
!      TS.LINETYPE = 29
!      TS.SAVPRT$ = "d1102="+ STR$(EP.DE1102.FOUND%) + \
!                   " d1103="+STR$(EP.DE1103.FOUND%) + \
!                   " d1203="+STR$(EP.DE1203.FOUND%) 
!      TS.SAVPRT.OPT = 4100H
!      CALL TSPREC01
!    ENDIF
!---------------------------------------------------------
      IF NOT EP.DE1102.FOUND% THEN \           ! no existe respuesta de transaccion
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "O"
        EXIT SUB
      ENDIf
      IF NOT EP.APPROV.FOUND% THEN \
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "T"
        EXIT SUB
      ENDIF    
      IF NOT EP.DE1103.FOUND% THEN \         ! no se termino transaccion voucher
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "U"
        EXIT SUB
      ENDIF
      IF EP.DE1203.FOUND% THEN \         ! Ya existe anulacion anterior
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "L"
        EXIT SUB
      ENDIF    
      IF VAL(EP.VALOR.EPAY$) <> VAL(EP.DE.AMOUNT$) THEN \ ! No coincide valor de transaccion
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "M"
        EXIT SUB
      ENDIF   
      EP.ANUL.TRANSNUM$ = EP.ECR.TRANSNUM$
      EP.ANUL.POSTEO$   = EP.DE.POSTEO$
      EP.ANUL.CODPRO$   = EP.DE.CODPRO$
!      EP.ANUL.CARD$     = EP.DE.CARD$
      EP.ANUL.AUTH$     = EP.DE.AUTH$
      EP.ANUL.RRN$      = EP.DE.RRN$
      EP.ANUL.CUOTAS$   = EP.DE.CUOTAS$ 
      EP.TAX.EPAY$      = EP.DE.IVA$
      EP.IVA.BASE$      = EP.DE.IVA.BASE$
      EP.TAX.BASE$      = EP.IVA.BASE$
      EP.PROP.EPAY$     = EP.DE.PROPINA$
      EP.CASHBACK$      = EP.DE.CASHBACK$
      EP.CAJERO$        = RIGHT$(STRING$(10,"0") + UNPACK$(TS.OPER$),10)
      EP.TIPO.VAR$      = EP.DE.TIPOVAR$
    ENDIF                                  ! fin de transaccion de anulacion dentro de misma transaccion 
  ENDIF
!
!  call ep.line.print("14apl="+ep.appl$+" fun="+ep.ecr.function$+" st=" + ep.trx.status$+" amj"+ep.amj.status$, 4100H) 
!  call ep.line.print("tv="+ep.tipo.var$+" vl="+EP.VALOR.EPAY$+" max="+str$(EP.MAX.TV%), 4100H) 



!  IF VAL(EP.VALOR.EFT$) > EP.MAX.TV% THEN \
!  BEGIN
!    EP.AMJ.STATUS$ = "2"
!    EP.TRX.STATUS$ = "P"
!    EXIT SUB
!  ENDIF   
!

  IF TS.PROCEDURE < 1 AND  \
     TS.INTRX         THEN \    ! transaccion de ventas
  BEGIN 
    IF EP.CB.MAXIMP% > 0 THEN \     ! cash back activo
    BEGIN 
      EP.CB.IMP% = VAL(EP.CASHBACK$)
      IF EP.CB.IMP% > EP.CHG.TV%     OR \
         EP.CB.IMP% > EP.CB.MAXIMP%  OR \
         FLOAT(EP.CB.IMP%) / FLOAT(ABS(VAL(EP.VALOR.EPAY$))) * 100.0  > \
         FLOAT(EP.CB.PORC%)        THEN \
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "Q"
        EXIT SUB
      ENDIF
    ENDIF ELSE \                    ! sin cash back activo
    BEGIN
      IF (VAL(EP.VALOR.EPAY$) - TS.BALDUE(0)) > EP.CHG.TV% THEN \
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "Q"
        EXIT SUB
      ENDIF
    ENDIF 
  ENDIF 
!
!  call ep.line.print("F14LEN MSR="+STR$(LEN(EP.MSR.DATA$)), 4100H) 
  
  
END SUB
!
!
SUB EP.APITEF.32(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,            \
        EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$,                                   \
        EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$) PUBLIC
!
   STRING              \
    EP.APPL$,          \
    EP.ECR.FUNCTION$,  \
    EP.TRX.STATUS$,    \
    EP.AMJ.STATUS$,    \
    EP.INVOICE.NBR$,   \
    EP.ECR.TRANSNUM$,  \
    EP.VALOR.EFT$,     \
    EP.TAX.EFT$,       \
    EP.CUOTAS.QTY$,    \
    EP.APPROV.CODE$,   \
    EP.APPROV.DESC$,   \
    EP.CARD.NUMBER$,   \
    EP.AUTH.NUMBER$,   \
    EP.TIPO.VAR$,      \
    EP.USER.DATA$,     \
	tmpCard$, tmpCardDE$, tmpApprovCode$
!
!  call ep.line.print("32apl="+ep.appl$+" ECRfun="+ep.ecr.function$+" func=" + ep.function$, 4100H) 
!
  !
  ! Si el nro de tarjeta está vacío, quiere decir que se digitó un
  ! token
  If Len(EP.ALL.CARD$) = 0 Then Begin
    tmpCard$ = EP.TEF.TOKEN$
    tmpCardDE$ = Left$(EP.BIN.PRODUCTO$ + String$(16, "0"), 16)
  Endif Else Begin
    tmpCard$ = EP.ALL.CARD$
    tmpCardDE$ = tmpCard$
  Endif
  !
  EP.INICIO.FOUND%  = 0
  If Len(tefBalanceDatetime$) > 0 Then Begin
  	EP.ECR.DATETIME$ = tefBalanceDatetime$
  	tefBalanceDatetime$ = ""
  Endif Else Begin
  	EP.ECR.DATETIME$  = DATE$ + TIME$   
  Endif
  If Len(tefBalanceUserdata$) > 0 Then Begin
  	EP.USER.DATA$ = tefBalanceUserdata$
  	tefBalanceUserdata$ = ""
  Endif Else Begin
  	EP.USER.DATA$     = EP.MSR.DATA$
  Endif
!  
  If Len(tefBalanceConsecut$) > 0 Then Begin
  	EP.EPAY.TRANSNUM$ = tefBalanceConsecut$
  	EP.ECR.TRANSNUM$  = EP.EPAY.TRANSNUM$
  	tefBalanceConsecut$ = ""
  Endif Else Begin
	  EP.SAVE.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
	  EP.EPAY.TRANSNUM$ = EP.NEW.TRANSNUM
	  EP.ECR.TRANSNUM$  = EP.EPAY.TRANSNUM$  !  returns transaction number used
	Endif
	EP.INVOICE.NBR$   = RIGHT$(STRING$(6,"0")+STR$(SL.HD.TRANSNUM + 1),6)
  EP.CUOTAS.QTY$    = "00"
  

!
!  call ep.line.print("funcion="+EP.function$, 4100H) 
!
  IF EP.FUNCTION$ = "01"  THEN \        ! Consulta 
    EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) + \
      RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.EPAY.TRANSNUM$ + \
      EP.INVOICE.NBR$ + EP.ECR.DATETIME$ + EP.APPL.STATUS$ + \
      LEFT$(EP.ALL.CARD$ + STRING$(20," "),20)+ EP.USER.DATA$
!
  IF EP.FUNCTION$ = "02"  OR \        ! abono a medio de pago
     EP.FUNCTION$ = "03"  OR \        ! compra 
     EP.FUNCTION$ = "51" THEN \       ! avance en efectivo
  BEGIN
    If EP.TEF.CEDULA$ <> "" OR EP.TEF.BOLSILLO$ <> "" Then Begin
	    tefRequest$ = \
	    		EP.VALOR.EPAY$ + EP.TAX.EPAY$ + EP.IVA.BASE$ + EP.PROP.EPAY$ + RIGHT$(STRING$(10,"0") + STR$(TS.BALDUE(0)),10) + \
	    		EP.CUOTAS.QTY$ + LEFT$(tmpCard$ + STRING$(20," "),20) + EP.FECHA.VENC$ + \
	    		LEFT$(EP.TEF.CEDULA$ + STRING$(20," "),20) + LEFT$(EP.TEF.BOLSILLO$ + STRING$(4," "),4)
	    EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) +                 \
	      RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.EPAY.TRANSNUM$ +     \
	      EP.INVOICE.NBR$ + EP.ECR.DATETIME$ + EP.APPL.STATUS$ +                        \
	      tefRequest$ + \
	      EP.USER.DATA$
	  Endif Else Begin
	    tefRequest$ = \
	    		EP.VALOR.EPAY$ + EP.TAX.EPAY$ + EP.IVA.BASE$ + EP.PROP.EPAY$ + RIGHT$(STRING$(10,"0") + STR$(TS.BALDUE(0)),10) + \
	    		EP.CUOTAS.QTY$ + LEFT$(EP.ALL.CARD$ + STRING$(20," "),20) + EP.FECHA.VENC$ + \
	    		STRING$(20," ") + STRING$(4," ")
	    EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) +                 \
	      RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.EPAY.TRANSNUM$ +     \
	      EP.INVOICE.NBR$ + EP.ECR.DATETIME$ + EP.APPL.STATUS$ +                        \
	      tefRequest$ + \
	      EP.USER.DATA$
	  Endif
  ENDIF
      
!      LEFT$(NOMCLI$ + STRING$(30," "),30) +  \                        ! suprimido temporalmente
!      LEFT$(EP.IDCTE$ + STRING$(11," "),11) + EP.USER.DATA$           ! suprimido temporalmente

!  IF EP.FUNCTION$ = "02"  OR   \        ! abono a medio de pago
!     EP.FUNCTION$ = "03"  THEN \        ! compra
!    EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) +                 \
!      RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.EPAY.TRANSNUM$ +     \
!      EP.INVOICE.NBR$ + EP.ECR.DATETIME$ + EP.APPL.STATUS$ +                        \
!      EP.VALOR.EPAY$ + EP.TAX.EPAY$ + EP.IVA.BASE$ + EP.PROP.EPAY$ + EP.CASHBACK$ + \
!      EP.ALL.CARD$ + EP.USER.DATA$
!
  IF EP.FUNCTION$ = "04"  OR   \        ! anulacion de abono a medio de pago
     EP.FUNCTION$ = "05"  OR   \
     EP.FUNCTION$ = "52"  THEN \        ! anulacion de compra
  BEGIN
    If EP.TEF.CEDULA$ <> "" OR EP.TEF.BOLSILLO$ <> "" Then Begin
	    tefRequest$ = \
	    		EP.VALOR.EPAY$ + EP.ANUL.AUTH$ + EP.ANUL.TRANSNUM$ +                          \
	    		EP.ANUL.INVOICE$ + LEFT$(tmpCard$ + STRING$(20," "),20) + EP.FECHA.VENC$ + \
	    		LEFT$(EP.TEF.CEDULA$ + STRING$(20," "),20) + LEFT$(EP.TEF.BOLSILLO$ + STRING$(4," "),4) + \
	    		EP.ANUL.RRN$
	    EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) + \
	      RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.EPAY.TRANSNUM$ +     \
	      EP.INVOICE.NBR$ + EP.ECR.DATETIME$ + EP.APPL.STATUS$ +                        \
	      tefRequest$ + \
	      EP.USER.DATA$
	  Endif Else Begin
	    tefRequest$ = \
	    		EP.VALOR.EPAY$ + EP.ANUL.AUTH$ + EP.ANUL.TRANSNUM$ +                          \
	    		EP.ANUL.INVOICE$ + LEFT$(EP.ALL.CARD$ + STRING$(20," "),20) + EP.FECHA.VENC$ + \
	    		STRING$(20," ") + STRING$(4," ") + \
	    		EP.ANUL.RRN$
	    EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) + \
	      RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.EPAY.TRANSNUM$ +     \
	      EP.INVOICE.NBR$ + EP.ECR.DATETIME$ + EP.APPL.STATUS$ +                        \
	      tefRequest$ + \
	      EP.USER.DATA$
	  Endif
  ENDIF 
      
!      LEFT$(NOMCLI$ + STRING$(30," "),30) +  \                       ! suprimido temporalmente
!      LEFT$(EP.IDCTE$ + STRING$(11," "),11) + EP.USER.DATA$          ! suprimido temporalmente
!
  EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
  EP.MESSAGE$ = EP.APPL.JAVA$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
!
  CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.ECR.FUNCTION$ +    \
     EP.EPAY.TRANSNUM$ + "01" ,                                           \ DE-1
     tmpCardDE$,                                                          \ DE-2 
     EP.FECHA.POSTEO$ + EP.TIPO.LECT$ ,                                   \ DE-3
     EP.VALOR.EFT$ + EP.TAX.EFT$,                                         \ DE-4
     EP.ECR.DATETIME$ ,                                                   \ DE-5
     EP.CAJERO$ + EP.INVOICE.NBR$ + EP.FECHA.VENC$)                       ! DE-6
  EP.INICIO.FOUND% = -1
!
  CALL EP.SEND.TO.THREADER
!---------------------------------------------------------    
!    IF LEN(EP.MESSAGE$) > 40 THEN \
!    BEGIN
!      CALL EP.LINE.PRINT(LEFT$(EP.MESSAGE$,40),4100H)
!      CALL EP.LINE.PRINT(MID$(EP.MESSAGE$,41,40),4100H)
!    ENDIF
!    IF LEN(EP.MESSAGE$) > 80 THEN \
!    BEGIN
!      CALL EP.LINE.PRINT(MID$(EP.MESSAGE$,81,40),4100H)
!    ENDIF
!    IF LEN(EP.MESSAGE$) > 120 THEN \
!    BEGIN
!      CALL EP.LINE.PRINT(MID$(EP.MESSAGE$,121,40),4100H)
!    ENDIF
!-----------------------------------------------------------
!
  EP.RESP.FOUND%    = 0
  EP.APPROV.FOUND%  = 0
  EP.VOUCHER.FOUND% = 0
  EP.TOHOST.FOUND%  = 0
  WHILE (EP.STATE% < 4) AND (EP.EXCEPTION$ = "")
    EP.M.LEN% = LEN(EP.AMJ.MESSAGE$)
    IF EP.M.LEN% < 8 THEN \
    BEGIN
      EP.TRX.STATUS$ = "0"
      EP.AMJ.STATUS$ = "1"
      EXIT SUB
    ENDIF 
    EP.AMJ.STATUS$ = MID$(EP.AMJ.MESSAGE$,8,1)
    IF EP.AMJ.STATUS$ = "0" OR   \
       EP.AMJ.STATUS$ = "1" THEN \
    BEGIN
      EXIT SUB
    ENDIF
    IF EP.M.LEN% < 9 THEN \
    BEGIN
      EP.TRX.STATUS$ = "1"
      EXIT SUB
    ENDIF 
    EP.TRX.STATUS$    = MID$(EP.AMJ.MESSAGE$,9,1)
!    CALL EP.DISPLAY.AN.ERROR("TRX.STAT="+EP.TRX.STATUS$)
    IF EP.AMJ.STATUS$ = "2" THEN \        ! Transaccion con respuesta de host concluida
    BEGIN
      IF EP.M.LEN% < 9 THEN \
      BEGIN
        EP.TRX.STATUS$ = "1" 
        EXIT SUB
      ENDIF
      tefResponse$ = Right$(EP.AMJ.MESSAGE$, EP.M.LEN% - 11)
      EP.RESP.FOUND%    = -1
      CALL EP.PARSE.THREADER.RESPONSE
      EP.APPROV.CODE$ = EP.H.APPROV.CODE$
      EP.APPROV.DESC$ = EP.H.APPROV.DESC$
      If EP.APPROV.CODE$ <> "00" Then Begin
      	!----------------------------------------------------------------------------------------------
      	! 2020-08-26 jsv
      	! Se modifica el mensaje de respuesta para que en la primera línea de 20 caracteres
      	! muestre el código de error
      	!----------------------------------------------------------------------------------------------
      	EP.APPROV.DESC$ = Left$("ERROR " + EP.APPROV.CODE$ + String$(20, " "), 20) + EP.APPROV.DESC$
      	!----------------------------------------------------------------------------------------------
      	!
      Endif Else If EP.APPL$ <> "09" And EP.APPL$ <> "11" Then Begin
      	! Se debe evaluar si cambia el medio de pago debido a 
				! datos de convenio y bolsillo
				Call postEvaluateTender
				EP.APPL.STATUS$ = "99"
			Endif
      EP.AUTH.NUMBER$ = EP.H.AUTH.NUMBER$
      EP.CARD.NUMBER$ = EP.H.CARD.NUMBER$
!      EP.FECHA.PROC$  = DATE$ + TIME$
!
      IF EP.TRX.STATUS$ <> "0" THEN \
      BEGIN
        EP.STATE% = 4
        EXIT SUB        
      ENDIF
      EP.TIPO.AUTH$ = "2"
      EP.RRN$ = EP.RRN$ + EP.FRANQUICIA$ + EP.BANCO$
      If isNumeric( EP.APPROV.CODE$) Then Begin
      	tmpApprovCode$ = EP.APPROV.CODE$
      Endif Else Begin
      	tmpApprovCode$ = "99"
      Endif
      CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$ + EP.EPAY.TRANSNUM$ + "02" , \ !DE-1
           EP.COMERCIO$ + EP.EPAY.TERMINAL$ + tmpApprovCode$,                                     \ !DE-2 
           EP.FECHA.POSTEO$ + EP.COD.PROC$ + LEFT$(EP.PRODUCTO$,4) + EP.AUTH.NUMBER$,             \ !DE-3
           EP.VALOR.EPAY$ + EP.TAX.EPAY$,                                                         \ !DE-4
           EP.FECHA.PROC$ + EP.ISO.TRX$ ,                                                         \ !DE-5
           EP.RRN$ + EP.CUOTAS.QTY$ + EP.TIPO.VAR$ + EP.TRX.STATUS$ + "000")                        !DE-6

      IF EP.APPROV.CODE$ = "00" THEN \ ! Trx aprobada
      BEGIN 
        ! Almacena user data con request y response para posterior generación de vouchers
      	! en el caso de aplicaciones diferentes a 09 y 11
      	If EP.APPL$ <> "09" And EP.APPL$ <> "11" Then Begin
      		Call saveTefUD(EP.APPL$ + EP.FUNCTION$)
      	Endif
      	!
        EP.STATE% = 2
        IF EP.APPROV.DESC$ = STRING$(20," ") THEN \ 
          CALL TRANSLATE.ISO.CODE(EP.APPROV.CODE$,EP.APPROV.DESC$)
        CALL EP.DISPLAY.A.MESSAGE(EP.APPROV.DESC$)
        EP.APPROV.FOUND% = -1
        If EP.APPL.STATUS$ = "99" Then Begin
	        EP.STATE% = 4                      !   Fin de ciclo  
	        !
	        Call putPendingReverse(\
	        		EP.APPL.JAVA$, EP.FUNCTION$, EP.EPAY.TRANSNUM$, EP.INVOICE.NBR$, EP.ECR.DATETIME$, \
	        		Int%(Val(EP.VALOR.EPAY$)), SL.TE.TENDTYPE, SL.TE.TENDVAR, \
					EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$  + EP.EPAY.TRANSNUM$ + "03", \
					EP.IVA.BASE$ + EP.CASHBACK$, \
					EP.FECHA.PROC$ + EP.SW.VERSION$, \
					EP.ANUL.TRANSNUM$ + EP.AGENCIA$ + EP.MINIPAGARE$ + EP.DEFUSER1$ + \
					EP.USER1$ + EP.USER2$ + EP.USER3$, \
					DATE$ + TIME$ + EP.CARD.BIN$ + EP.SW.VERSION$, \
					EP.ANUL.RRN$ + EP.FECHA.VENC$)
	        EP.VOUCHER.FOUND% = -1
        Endif
      ENDIF \
      ELSE  \
      BEGIN
        If EP.APPROV.CODE$ = "97" Then Begin
        	! Se debe enviar reverso
        	Call putPendingReverse(\
	        		EP.APPL.JAVA$, EP.FUNCTION$, EP.EPAY.TRANSNUM$, EP.INVOICE.NBR$, EP.ECR.DATETIME$, \
	        		Int%(Val(EP.VALOR.EPAY$)), SL.TE.TENDTYPE, SL.TE.TENDVAR, \
							EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$  + EP.EPAY.TRANSNUM$ + "03", \
							EP.IVA.BASE$ + EP.CASHBACK$, \
							EP.FECHA.PROC$ + EP.SW.VERSION$, \
							EP.ANUL.TRANSNUM$ + EP.AGENCIA$ + EP.MINIPAGARE$ + EP.DEFUSER1$ + \
							EP.USER1$ + EP.USER2$ + EP.USER3$, \
							DATE$ + TIME$ + EP.CARD.BIN$ + EP.SW.VERSION$, \
							EP.ANUL.RRN$ + EP.FECHA.VENC$)
        Endif
        EP.STATE% = 4
        EXIT SUB        
      ENDIF
    ENDIF \
    ELSE  \                          !  Otros estados de transaccion 
    BEGIN
      IF EP.AMJ.STATUS$ = "3" THEN \
      BEGIN
        CALL EP.PARSE.DISPLAY.REQUEST
        If EP.DISP.PARAM$ = "1" Then Begin
        	Call EP.DISPLAY.AN.ERROR(EP.DISP.MESSAGE$)
        Endif Else Begin
        	Call EP.DISPLAY.A.MESSAGE(EP.DISP.MESSAGE$)
        Endif
      ENDIF Else \   
      IF EP.AMJ.STATUS$ = "4" THEN \
      BEGIN
        CALL EP.PARSE.PRT.HEADER
        CALL EP.STORE.VOUCHER.TEF(EP.PRT.MESSAGE$,EP.PRT.CUT$)
        EP.PRT.CUT$ = "0"
        CALL EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
      ENDIF Else \
      IF EP.AMJ.STATUS$ = "5" THEN \
      BEGIN
        CALL EP.PARSE.PRT.LINE
        IF EP.PRT.VOUC$ = "1" THEN \
          CALL EP.STORE.VOUCHER.TEF(EP.PRT.MESSAGE$,EP.PRT.CUT$)
        IF EP.PRT.CUST$ = "1" THEN \
          CALL EP.STORE.EFTLINE(EP.PRT.MESSAGE$)
      ENDIF Else \ 
      IF EP.AMJ.STATUS$ = "6" THEN \       !  Transacccion finalizada satisfactoriamente
      BEGIN
        CALL EP.PARSE.PRT.CLOSE
        CALL EP.STORE.VOUCHER.TEF(EP.PRT.MESSAGE$,EP.PRT.CUT$)
        EP.AMJ.STATUS$ = "2"               !  Sale como transaccion con respuesta del host
        EP.STATE% = 4                      !   Fin de ciclo  
        !
        Call putPendingReverse(\
        		EP.APPL.JAVA$, EP.FUNCTION$, EP.EPAY.TRANSNUM$, EP.INVOICE.NBR$, EP.ECR.DATETIME$, \
        		Int%(Val(EP.VALOR.EPAY$)), SL.TE.TENDTYPE, SL.TE.TENDVAR, \
				EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$  + EP.EPAY.TRANSNUM$ + "03", \
				EP.IVA.BASE$ + EP.CASHBACK$, \
				EP.FECHA.PROC$ + EP.SW.VERSION$, \
				EP.ANUL.TRANSNUM$ + EP.AGENCIA$ + EP.MINIPAGARE$ + EP.DEFUSER1$ + \
				EP.USER1$ + EP.USER2$ + EP.USER3$, \
				DATE$ + TIME$ + EP.CARD.BIN$ + EP.SW.VERSION$, \
				EP.ANUL.RRN$ + EP.FECHA.VENC$)
        !
        !CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$  + EP.EPAY.TRANSNUM$ + "03", \ DE-1
        !     EP.IVA.BASE$ + EP.CASHBACK$,                                      \ DE-2
        !     EP.FECHA.PROC$ + EP.SW.VERSION$,                                  \ DE-3
        !     EP.ANUL.TRANSNUM$ + EP.AGENCIA$ + EP.MINIPAGARE$ + EP.DEFUSER1$ + \
        !     EP.USER1$ + EP.USER2$ + EP.USER3$ ,                               \ DE-4
        !     DATE$ + TIME$ + EP.CARD.BIN$ + EP.SW.VERSION$,                    \ DE-5
        !     EP.ANUL.RRN$ + EP.FECHA.VENC$)                                    ! DE-6 
        !
        EP.VOUCHER.FOUND% = -1
      ENDIF Else \   
      IF EP.AMJ.STATUS$ = "7" THEN \
      BEGIN 
        CALL EP.PARSE.DATA.REQUEST
        CALL EP.SAVE.KEYS
        EP.KEYB.DATA$ = ""
        EP.INI.RANGE$ = STR$(VAL(EP.INI.RANGE$))
        EP.END.RANGE$ = STR$(VAL(EP.END.RANGE$))
        CALL EP.GET.KBDATA(EP.DISP.MESSAGE$, EP.INI.RANGE$, EP.END.RANGE$, \
          EP.KEYB.DATA$)
        CALL EP.RESTORE.KEYS
      ENDIF Else \
      IF EP.AMJ.STATUS$ = "8" THEN \
      BEGIN
        CALL EP.PARSE.DENTRY.REQUEST
        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
             EP.DE5.DATA$, EP.DE6.DATA$)
      ENDIF Else \   
      IF EP.AMJ.STATUS$ = "9"  OR  \
         EP.AMJ.STATUS$ = "A" THEN \
      BEGIN
        CALL EP.PARSE.DENTRY.REQUEST
!        EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
             EP.DE5.DATA$, EP.DE6.DATA$)
        EP.STATE% = 4                       !   Fin de ciclo
!        CALL EP.DISPLAY.AN.ERROR("de1="+EP.DE1.DATA$) 
!        CALL EP.DISPLAY.AN.ERROR("TRX.STAT="+EP.TRX.STATUS$) 
!        CALL EP.DISPLAY.AN.ERROR("Tohost="+str$(EP.TOHOST.FOUND%)) 
        IF EP.TRX.STATUS$ = "0" THEN  \
        BEGIN
          EP.TOHOST.FOUND% = -1
        ENDIF  
      ENDIF Else \   
      IF EP.AMJ.STATUS$ = "B" THEN \
      BEGIN
        CALL EP.PARSE.DENTRY99.REQUEST
        CALL EP.ADD.DATA.ENTRY99(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$ , EP.DE3.DATA$, \
                 EP.DE4.DATA$, EP.DE5.DATA$ , EP.DE6.DATA$)
      ENDIF Else \
      If EP.AMJ.STATUS$ = "Z" Then Begin
      	Call EP.PARSE.DATA.REGULAR
				Call EP.ADD.DATA.REGULAR
    	Endif Else Begin
    		! Código no reconocido. La aplicación se limitará 
    		! a actualizar el estado
    		EP.APPL.STATUS$ = MID$(EP.AMJ.MESSAGE$,10,2)
    	Endif
    ENDIF
    IF  (EP.STATE% < 4) AND (EP.EXCEPTION$ = "") THEN \   ! Si se continua el ciclo
    BEGIN    
    EP.MESSAGE$ = EP.CHAIN$ + RIGHT$(STRING$(4,"0")+ TS.STORE$,4) + \
      RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.CAJERO$ + EP.EPAY.TRANSNUM$ + \
      EP.INVOICE.NBR$ + EP.ECR.DATETIME$ + EP.APPL.STATUS$ 
      IF EP.AMJ.STATUS$ = "7" THEN \
        EP.MESSAGE$ = EP.MESSAGE$ + EP.KEYB.DATA$
      EP.MSGLEN$  = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
      EP.MESSAGE$ = EP.APPL.JAVA$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
      IF EP.FUNCTION$ = "01"  OR  \        ! 
         EP.FUNCTION$ = "02"  OR  \
         EP.FUNCTION$ = "03"  OR  \
         EP.FUNCTION$ = "04"  OR  \
         EP.FUNCTION$ = "05"  OR  \
         EP.FUNCTION$ = "51"  OR  \
         EP.FUNCTION$ = "52" THEN \
      BEGIN
        CALL EP.SEND.TO.THREADER
      ENDIF
    ENDIF
  WEND
!

  IF NOT EP.APPROV.FOUND% THEN \
     EP.medioPagoTef% = 0
  EP.TRX.PEND$     = ""
  EP.REV.FUNCTION$ = ""
  EP.FECHA.PEND$   = ""
  EP.REV.COUNT%    = 0
  EP.REV.APROB$    = ""
!
  IF EP.FUNCTION$ = "02"  OR  \        ! deteccion de necesidad de reversos 
     EP.FUNCTION$ = "03"  OR  \        ! 
     EP.FUNCTION$ = "04"  OR  \        ! 
     EP.FUNCTION$ = "05"  OR  \
     EP.FUNCTION$ = "51"  OR  \
     EP.FUNCTION$ = "52" THEN \        ! 
  BEGIN
    IF EP.INICIO.FOUND%      AND   \
       NOT EP.TOHOST.FOUND% THEN   \
    BEGIN
      IF EP.RESP.FOUND% THEN       \
      BEGIN
        IF EP.APPROV.FOUND% THEN   \
        BEGIN
          IF NOT EP.VOUCHER.FOUND% THEN \
          BEGIN
            EP.TRX.PEND$ = EP.EPAY.TRANSNUM$ 
            EP.FECHA.PEND$ = EP.ECR.DATETIME$
          ENDIF
        ENDIF 
      ENDIF \
      ELSE  \
      BEGIN
        EP.TRX.PEND$   = EP.EPAY.TRANSNUM$ 
        EP.FECHA.PEND$ = EP.ECR.DATETIME$
        EP.DE.INVOICE.NBR$ = EP.INVOICE.NBR$
      ENDIF
    ENDIF
!
    EP.REV.FUNCTION$ = "00"
    EP.REV.APPL$ = EP.APPL.JAVA$

    IF EP.TRX.PEND$ <> "" THEN    \
    BEGIN 
      IF EP.FUNCTION$ = "02"  THEN  \        ! deteccion de necesidad de reversos 
        EP.REV.FUNCTION$ = "06"     \        ! 
      ELSE \
      IF EP.FUNCTION$ = "03"  THEN  \        ! 
        EP.REV.FUNCTION$ = "07"     \        ! 
      ELSE \  
      IF EP.FUNCTION$ = "04" THEN   \        ! 
        EP.REV.FUNCTION$ = "08"     \        ! 
      ELSE \
      IF EP.FUNCTION$ = "05" THEN   \        ! 
        EP.REV.FUNCTION$ = "09"     \         ! 
      ELSE \
      IF EP.FUNCTION$ = "51" THEN   \        ! 
        EP.REV.FUNCTION$ = "53"     \         ! 
      ELSE \
      IF EP.FUNCTION$ = "52" THEN   \        ! 
        EP.REV.FUNCTION$ = "54"              ! 
      CALL EP.REVERSE.TRX
    ENDIF
  ENDIF 
!
END SUB
!
!
!-----------------------------------------------------------
!  	recover routines
!
SUB EPAY.START.RECOVER.EFT EXTERNAL
!
END SUB
!
!
SUB EPAY.RECOVER.EFT EXTERNAL
!
END SUB
!
!
SUB EPAY.END.RECOVER.EFT EXTERNAL

END SUB
!
SUB EP.INIT.KBD  PUBLIC
!
  DIM TS.IO.KEYS(10)
  DIM TS.IO.DATA$(10)
  TS.IO.MOTORKEY = 0
END SUB
!  
Sub inquiryForBalance(pBalanceValue%, pReturn%)
	Integer*4 pBalanceValue%
	Integer*2 pReturn%, tmpCounter%, tmpCurrentStan%, tmpInitialStan%
	String tmpRequest$, tmpResponse$, tmpCard$
	!
	Call asignarVariableGlobal("CURRENT_STAN", "")
	pReturn% = 0
	!
	If Len(EP.ALL.CARD$) = 0 Then Begin
		tmpCard$ = EP.TEF.TOKEN$
	Endif Else Begin
		tmpCard$ = EP.ALL.CARD$
	Endif
	!
	tmpRequest$ = \
			EP.VALOR.EPAY$ + EP.TAX.EPAY$ + EP.IVA.BASE$ + EP.PROP.EPAY$ + Right$(String$(10, "0") + Str$(TS.BALDUE(0)), 10) + \
			EP.CUOTAS.QTY$ + Left$(tmpCard$ + String$(20," "), 20) + EP.FECHA.VENC$ + \
			Left$(EP.TEF.CEDULA$ + String$(20, " "), 20) + Left$(EP.TEF.BOLSILLO$ + String$(4, " "), 4) + EP.USER.DATA$
	tmpResponse$ = EP.invocarApplManager1(EP.APPL$, "01", tmpRequest$, -1, 0, 1)
	If Len(tmpResponse$) > 0 Then Begin
		If Mid$(tmpResponse$, 7, 2) = "00" Then Begin
			pReturn% = -1
			tefBalanceConsecut$ = EP.EPAY.TRANSNUM$
			tefBalanceDatetime$ = EP.ECR.DATETIME$
			pBalanceValue% = Int%(Val(Mid$(tmpResponse$, 29, 10)))
			If Len(EP.USER.DATA$) = 0 Then Begin
				tefBalanceUserdata$ = Left$(tmpResponse$, 6)
			Endif
		Endif
	Endif
	!
	! Actualiza el STAN
	tmpCurrentStan% = Int%(Val(consultarVariableGlobal("CURRENT_STAN")))
	If tmpCurrentStan% > 0 Then Begin
		tmpInitialStan% = Int%(Val(EP.EPAY.TRANSNUM$))
		tmpCounter% = tmpInitialStan% + 1
		While tmpCounter% < tmpCurrentStan%
			EP.SAVE.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
			EP.EPAY.TRANSNUM$ = EP.NEW.TRANSNUM
			tmpCounter% = tmpCounter% + 1
		Wend
	Endif
End Sub
!
!*********************************************************************
!                         (UETEF14)
!                         (Ver 4.00)
!
! This routine performs the following
! - Process a e-payment tender  entry
! - Calls to electr= 176 THEN \ process
!*********************************************************************
!
SUB UETEF14 PUBLIC
String tmpCard$
Integer*1 tmpBalanceReq%
Integer*2 tmpReturn%
Integer*4 tmpBalanceValue%, tmpTaxValue%, tmpTaxBase%
!
tmpBalanceReq% = 0
tmpBalanceValue% = 0
tmpTaxValue% = 0
tmpTaxBase% = 0
tmpReturn% = 0
tefBalanceConsecut$ = ""
tefBalanceDatetime$ = ""
tefBalanceUserdata$ = ""
!
! -----------------------------------------------------------
! Arreglo para evitar ingreso directo a 4 slash BONOS 
!  JAC Marzo 11/2013
!
IF (TS.IO.KEYS(7) = 90 + VAL(LEFT$(EP.TV.COMPENSAR$,1))) AND \
   (TS.IO.KEYS(3) = 78) AND \
   (TS.IO.DATA$(3) = RIGHT$(EP.TV.COMPENSAR$,1)) THEN  \
BEGIN
  CALL EP.DISPLAY.AN.ERROR("Error de secuencia")
  CALL EP.INIT.KBD
  EXIT SUB
ENDIF

! fin de arreglos
!-----------------------------------------------------------

!
  IF TS.IO.KEYS(7) = 179 THEN \             !  medio de pago TEF
  BEGIN 
    IF (NOT EP.EFT.ACTIVO%) \
       OR TS.TRAINING  THEN \
    BEGIN
      CALL EP.DISPLAY.AN.ERROR("PROCESO NO DISPONIBLE")
      CALL EP.INIT.KBD
      EXIT SUB
    ENDIF
    !
	! Control de numero de transacciones TEF en una transaccion de venta
	If warningSMA% = -1 Then \
	Begin
		CALL EP.DISPLAY.AN.ERROR("Cierre trx e inicie una nueva")
		CALL EP.INIT.KBD
		EXIT SUB
	Endif    
    ! 
    ! Limpia variables de modulo TCRO antes de iniciar una nueva
    ! transaccion de Compensar
    Call RESET14.TCRO
    !
    EP.APPL$ = "09"                    !  TEF COMPENSAR  
    EP.APPL.JAVA$ = EP.APPL$
    EP.TRX.STATUS$   = "1"             !  sin comunicaciones 
    EP.AMJ.STATUS$   = "0"             !  sin iniciar transaccion
    EP.EFT.CMP.OK% = 0
!  
!    call EP.DISPLAY.AN.ERROR("procedure="+str$(ts.procedure))
    IF TS.PROCEDURE < 1 AND \
       TS.INTRX   THEN      \       ! transaccion de venta normal
    BEGIN
!      IF NOT TS.BAL.TAKEN THEN \       ! OJO  VERIFICAR
!      BEGIN 
!        TS.GUIDANCE = 1020
!        TS.IO.MOTORKEY = 0
!      ENDIF
!
      IF TS.IO.KEYS(1) = 70 THEN  \       ! si es anular
      BEGIN 
        EP.ECR.FUNCTION$ = "05"          !  void trx
        EP.ECR.TRANSNUM$ = RIGHT$(STRING$(6,"0")+TS.IO.DATA$(3),6)  ! Numero de transaccion por anular
      ENDIF \
      ELSE  \
      BEGIN
        EP.ECR.FUNCTION$ = "03"           ! inicio nueva transaccion compra
      ENDIF
!       
      IF TS.BALDUE(0)   < 0   AND \               ! si es devolucion
         TS.IO.DATA$(7) = "0" THEN \              ! VALOR DIGITADO = 0
      BEGIN 
        EP.FLAG.DEV% = -1
        EP.ECR.FUNCTION$ = "05"                   ! se asigna  void trx
        EP.VALOR.EFT$ = STR$(ABS(TS.BALDUE(0)))  ! se asigna el valor a pagar como valor de trx
      ENDIF \
      ELSE \                                      ! si no es devolucion
      BEGIN 
        IF TS.IO.DATA$(7) = "" THEN \               ! no hay valor digitado 
        BEGIN
          TS.IO.DATA$(7) = STR$(ABS(TS.BALDUE(0)))  ! se asigna el valor a pagar como valor de trx
        ENDIF 
        EP.VALOR.EFT$ = RIGHT$(STRING$(10,"0") + TS.IO.DATA$(7), 10) ! valor digitado
        EP.FLAG.DEV%  = 0
      ENDIF
!
      EP.TAX.EPAY% = 0
      EP.TAX.BASE% = 0
!
!      IF ABS(VAL(EP.VALOR.EFT$)) <= ABS(TS.BALDUE(0)) THEN \            ! calculo del IVA
!      BEGIN 				
!        EP.TAX.EPAY% = USR.TARJ.IVA(VAL(EP.VALOR.EFT$))       ! SUPRIMIR COMENTARIO PARA EL EXITO 
!        EP.TAX.BASE% = USR.TARJ.IVA2(VAL(EP.VALOR.EFT$))      ! SUPRIMIR COMENTARIO PARA EL EXITO
!      ENDIF \  
!      ELSE \
!      BEGIN
!        EP.TAX.EPAY% = USR.TARJ.IVA(TS.BALDUE(0))              ! SUPRIMIR COMENTARIO PARA EL EXITO
!        EP.TAX.BASE% = USR.TARJ.IVA2(TS.BALDUE(0))             ! SUPRIMIR COMENTARIO PARA EL EXITO
!      ENDIF
!      
!---------------------------------------------------------
!   CALL EP.LINE.PRINT("ANTES iva= "+STR$(EP.TAX.EPAY%)+ " base="+ STR$(EP.TAX.BASE%), 4100H)
!-----------------------------------------------------
!---------------------------------------------------------
!   CALL EP.DISPLAY.AN.ERROR("iva= "+STR$(EP.TAX.EPAY%)+ " base="+ STR$(EP.TAX.BASE%))
!   CALL EP.DISPLAY.AN.ERROR("VLR="+ EP.VALOR.EFT$ + " TOT="+STR$(TS.TOTALS(0,0,0)) + \
!      " DUE=" + STR$(TS.BALDUE(0)))
!-----------------------------------------------------
!
      EP.CASHBACK$ = STRING$(10,"0") 
      IF EP.CB.MAXIMP% > 0 AND     \
         ABS(VAL(EP.VALOR.EFT$)) > ABS(TS.BALDUE(0)) THEN \            ! calculo del Cask Back
      BEGIN
        EP.CASHBACK$ = RIGHT$(STRING$(10,"0") + STR$(ABS(VAL(EP.VALOR.EFT$)) - \
          ABS(TS.BALDUE(0))),10) 
      ENDIF
      EP.TAX.EFT$ = RIGHT$(STRING$(10,"0")+STR$(EP.TAX.EPAY%),10) ! valor iva
      EP.TAX.EPAY$ = EP.TAX.EFT$
      EP.IVA.BASE$ = RIGHT$(STRING$(10,"0")+STR$(EP.TAX.BASE%),10) ! valor iva
    ENDIF \                       ! fin de transaccion de venta
    ELSE  \                            
    IF TS.PROCEDURE = 1 THEN \    ! Transaccion de tender cashing
    BEGIN
!
      IF TS.IO.KEYS(1) = 70 THEN  \       ! si es anular
      BEGIN 
        EP.ECR.FUNCTION$ = "52"           !  void trx
      ENDIF \
      ELSE  \
      BEGIN
        EP.ECR.FUNCTION$ = "51"           ! inicio nueva transaccion avance
      ENDIF
!    
      EP.VALOR.EFT$ = RIGHT$(STRING$(10,"0") + TS.IO.DATA$(7),10) ! valor digitado
!
      EP.TAX.EPAY% = 0
      EP.TAX.BASE% = 0
!
!      IF ABS(VAL(EP.VALOR.EFT$)) <= ABS(TS.BALDUE(0)) THEN \            ! calculo del IVA
!      BEGIN 				
!        EP.TAX.EPAY% = USR.TARJ.IVA(VAL(EP.VALOR.EFT$))       ! SUPRIMIR COMENTARIO PARA EL EXITO 
!        EP.TAX.BASE% = USR.TARJ.IVA2(VAL(EP.VALOR.EFT$))      ! SUPRIMIR COMENTARIO PARA EL EXITO
!      ENDIF \  
!      ELSE \
!      BEGIN
!        EP.TAX.EPAY% = USR.TARJ.IVA(TS.BALDUE(0))              ! SUPRIMIR COMENTARIO PARA EL EXITO
!        EP.TAX.BASE% = USR.TARJ.IVA2(TS.BALDUE(0))             ! SUPRIMIR COMENTARIO PARA EL EXITO
!      ENDIF
!      
!---------------------------------------------------------
!   CALL EP.LINE.PRINT("ANTES iva= "+STR$(EP.TAX.EPAY%)+ " base="+ STR$(EP.TAX.BASE%), 4100H)
!-----------------------------------------------------
!---------------------------------------------------------
!   CALL EP.DISPLAY.AN.ERROR("iva= "+STR$(EP.TAX.EPAY%)+ " base="+ STR$(EP.TAX.BASE%))
!   CALL EP.DISPLAY.AN.ERROR("VLR="+ EP.VALOR.EFT$ + " TOT="+STR$(TS.TOTALS(0,0,0)) + \
!      " DUE=" + STR$(TS.BALDUE(0)))
!-----------------------------------------------------
!
      EP.CASHBACK$ = STRING$(10,"0") 
      EP.TAX.EFT$ =  STRING$(10,"0") ! valor iva
      EP.TAX.EPAY$ = EP.TAX.EFT$
      EP.IVA.BASE$ = STRING$(10,"0") ! valor base devolucion IVA
    ENDIF \                        ! fin de transaccon de intercambio de medio de pago
    ELSE \
    IF TS.PROCEDURE = 2 THEN \    ! Transaccion de intercambio de medio de pago
    BEGIN
      IF TS.IO.KEYS(1) = 70 THEN \       ! con la tecla slash
      BEGIN 
        EP.ECR.FUNCTION$ = "05"          !  void trx
        EP.ECR.TRANSNUM$ = RIGHT$(STRING$(6,"0")+TS.IO.DATA$(3),6)  ! Numero de transaccion por anular
      ENDIF \
      ELSE  \
      BEGIN
        EP.ECR.FUNCTION$ = "03"           ! inicio nueva transaccion compra
      ENDIF
      IF VAL(TS.IO.DATA$(7)) = 0   THEN \
      BEGIN
        IF EP.ECR.FUNCTION$ = "05" THEN \
        BEGIN 
          TS.GUIDANCE = 1003
          TS.IO.MOTORKEY = 0
        ENDIF \
        ELSE  \
        BEGIN 
          TS.IO.DATA$(7) = EP.VALOR.EFT$
        ENDIF
      ENDIF \
      ELSE \
      BEGIN 
        IF EP.ECR.FUNCTION$ = "05" THEN \
        BEGIN 
          EP.VALOR.EFT$ = RIGHT$(STRING$(10,"0") + TS.IO.DATA$(7),10) ! valor digitado
        ENDIF \
        ELSE \
        BEGIN 
          IF VAL(EP.VALOR.EFT$) <> VAL(TS.IO.DATA$(7)) THEN \
          BEGIN 
            TS.GUIDANCE = 1041
            TS.IO.MOTORKEY = 0 
          ENDIF
        ENDIF
      ENDIF
      IF EP.ECR.FUNCTION$ = "03" AND \
         NOT EP.EFT.CMP.TRX%        THEN \
      BEGIN
        TS.GUIDANCE = 1003
        TS.IO.MOTORKEY = 0
      ENDIF
    ENDIF \                        ! fin de transaccon de intercambio de medio de pago
    ELSE \                         ! Transaccion no de venta diferente a TEF                              
    BEGIN 
      call EP.DISPLAY.AN.ERROR("proceso inhabilitado"+"vuelva a iniciar"+str$(ts.procedure))
      TS.GUIDANCE = 1003
      TS.IO.MOTORKEY = 0
    ENDIF 
!
    IF TS.GUIDANCE = 0  THEN  \   ! trx ok
    BEGIN    
      EP.DONACION$ = STRING$(10,"0")
      EP.FILLER4$  = STRING$(10,"0")
      EP.PROPINA$  = STRING$(10,"0")
      EP.APPROV.CODE$  = ""        !  no ISO8583 message
      EP.CARD.NUMBER$  = ""        !  no card number
      EP.AUTH.NUMBER$  = ""        !  no autorization number
      EP.USER.DATA$    = ""

      TO.USEREXIT(14) = 0
      CALL EP.APITEF.14(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,   \
        EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, \   
        EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$, tmpBalanceReq%)
      TO.USEREXIT(14) = -1

      IF EP.AMJ.STATUS$ <> "2"  THEN \ !    evaluate HOST command reponse
      BEGIN
        CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
        CALL EP.DISPLAY.A.MESSAGE(EP.MESSAGE.DESC$)
        TS.IO.MOTORKEY = 0
      ENDIF \
      ELSE \
      IF EP.TRX.STATUS$ <> "0" THEN \    !    whether communication error found
      BEGIN
        CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
        CALL EP.DISPLAY.A.MESSAGE(EP.MESSAGE.DESC$)
        TS.IO.MOTORKEY = 0
      ENDIF\
      ELSE \
      BEGIN
        DIM TS.IO.KEYS(10) 
        DIM TS.IO.DATA$(10)
        IF EP.ECR.FUNCTION$ = "04" OR \
           EP.ECR.FUNCTION$ = "05" OR \
           EP.ECR.FUNCTION$ = "52" THEN \
        BEGIN 
          TS.IO.KEYS(1) = 70
  
        ENDIF Else If tmpBalanceReq% Then Begin
        	! Se requiere hacer consulta para saber el valor 
        	! a pagar
        	Call inquiryForBalance(tmpBalanceValue%, tmpReturn%)
        	If Not tmpReturn% Then Begin
        		Dim TS.IO.KEYS(10)
        		Dim TS.IO.DATA$(10)
        		TS.IO.MOTORKEY = 0
        		TS.IO.DEVICE = 1
        		Exit Sub
        	Endif Else Begin
        		If Val(EP.VALOR.EPAY$) > 0 Then Begin
        			tmpTaxValue% = Round(Val(EP.TAX.EPAY$) * Float(tmpBalanceValue%) / Val(EP.VALOR.EPAY$), 0, 0)
        			tmpTaxBase%  = Round(Val(EP.IVA.BASE$) * Float(tmpBalanceValue%) / Val(EP.VALOR.EPAY$), 0, 0)
        		Endif
        		EP.TAX.EFT$   = Right$(String$(10, "0") + Str$(tmpTaxValue%), 10)
        		EP.VALOR.EFT$ = Right$(String$(10, "0") + Str$(tmpBalanceValue%), 10)
        		EP.IVA.BASE$  = Right$(String$(10, "0") + Str$(tmpTaxBase%), 10)
        		EP.TAX.EPAY$  = EP.TAX.EFT$
        		EP.VALOR.EPAY$= EP.VALOR.EFT$
        	Endif
        Endif
        IF EP.FLAG.DEV% THEN \
          TS.IO.DATA$(7) = "0" \
        ELSE \
          TS.IO.DATA$(7) = EP.VALOR.EFT$
        If Len(EP.ALL.CARD$) = 0 Then Begin
		  		tmpCard$ = EP.TEF.TOKEN$
				Endif Else Begin
		  		tmpCard$ = EP.ALL.CARD$
				Endif
		
				TS.IO.KEYS(7)  = 90 + VAL(LEFT$(EP.TIPO.VAR$,1))
        TS.IO.KEYS(3)  = 78  
        TS.IO.DATA$(3) = RIGHT$(EP.TIPO.VAR$,1)
        TS.IO.MOTORKEY = TS.IO.KEYS(7)
        TS.IO.DATA$(9) = tmpCard$
        TS.IO.KEYS(9)  = 90
        TS.IO.DEVICE   = 1
        EP.EFT.CMP.OK% = -1
        IF TS.PROCEDURE = 2 THEN \
          TO.XCHGLIM = 0
        !
        ! Si viene entidad preasignada, invoca módulo 
        ! de entidades para inhibir ingreso manual de ésta
        If Val(tefEntity$) > 0 Then Begin
        	Call menpag.ignoreTender
        Endif Else Begin
        	tefEntity$ = "0"
        Endif
        !
      ENDIF 
    ENDIF     ! end trx ok
  ENDIF     ! end EFT key
!  
  IF TS.PROCEDURE = 2 THEN \
  BEGIN 
!    call ep.line.print("procedure = 2",4100H)
  
!    call ep.line.print("key1="+str$(ts.io.keys(1))+ \
!          " key6="+str$(ts.io.keys(6))+\
!          " eft="+str$(EP.EFT.CMP.TRX%)+ \
!          " temp="+str$(ts.temp1i2),4100H)
  
    IF TS.IO.KEYS(1) = 70 AND \          ! anulado 
       TS.IO.KEYS(6) = 81 AND \          ! total
       EP.EFT.CMP.TRX%        AND \          ! hay una trx
       TS.TEMP1I2 = 2 THEN     \
    BEGIN
      CALL EP.REVERSE.ANULTTL   ! anular anterior correccion
    ENDIF 
  ENDIF
!  
  IF TS.IO.KEYS(1) = 82 AND  \                   ! Suspender
     TS.IO.KEYS(2) = 80 AND  \                   ! Enter
     EP.EFT.CMP.TRX%        THEN \                   ! trx EFT activa
  BEGIN
    CALL EP.RESET.TEF.VOUCHER
    TO.XCHGLIM     = EP.XCHGLIM%
    EP.EFT.CMP.TRX%    = 0
    EP.TEF.DETAIL% = 0
    EP.TEF.SMA%    = 0
    EP.FLAG.DEV%   = 0
  ENDIF 
!


END SUB
!
Sub UETEF14.1 Public
	If Val(tefEntity$) > 0 And EP.EFT.CMP.OK% <> 0 Then Begin
		Call reasignEntity(Int%(Val(EP.TIPO.VAR$)), Int%(Val(tefEntity$)))
	Endif
	tefEntity$ = "0"
End Sub
!
!*********************************************************************
!                            (UETEF32)
!                           (Ver 4.00)
!
! This routine performs the following
! - Process a e-payment tender  entry
!*********************************************************************
!
SUB UETEF32  PUBLIC
String tmpCard$
!
  IF EP.EFT.ACTIVO%  THEN \  
  BEGIN
    IF EP.EFT.CMP.OK% THEN \                !  medio de pago TEF
    BEGIN
      EP.EFT.CMP.OK% = 0                    !  no inicia de nuevo
      EP.TRX.STATUS$   = "1"            !  sin communications 
      EP.AMJ.STATUS$   = "0"            !  sin iniciar transaccion
      EP.APPROV.CODE$  = ""             !  sin aprobacion 
      !
      TS.USER.RETURN = 0
      !
!                                     
!         AQUI VAN AJUSTES POR RUTINAS DE USUARIO
!
      CALL EP.APITEF.32(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,  \
        EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, \
        EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$)

!    
!   call ep.line.print("32AMJst=" + EP.AMJ.STATUS$ + \                                                 !
!            " trxst=" + EP.TRX.STATUS$ + \
!            " cod=" + ep.approv.code$ , 4100H)
!   call ep.line.print("veft="+EP.VALOR.EFT$+"epay="+EP.VALOR.EPAY$,4100h)
!   call ep.line.print("data(7)="+ts.io.data$(7)+"te="+str$(SL.TE.AMTTENDE),4100h)

     IF EP.AMJ.STATUS$ <> "2"  THEN \  !    evaluate HOST command reponse
      BEGIN
        CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
        CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
        TS.IO.MOTORKEY = 0
        TS.USER.RETURN = 99
      ENDIF \
      ELSE \
      IF EP.TRX.STATUS$ <> "0" THEN \   !    whether communication error found
      BEGIN
        CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
        CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
        TS.IO.MOTORKEY = 0
        TS.USER.RETURN = 99
      ENDIF\
      ELSE \
      IF EP.APPROV.CODE$ = "00"  THEN \
      BEGIN
!      
!    approval OK. download transaction data
!     
!        TS.IO.KEYS(7)  = 90 + VAL(LEFT$(EP.TIPO.VAR$,1))
!        TS.IO.KEYS(3)  = 78  
!        TS.IO.DATA$(3) = RIGHT$(EP.TIPO.VAR$,1)
!        TS.IO.MOTORKEY = TS.IO.KEYS(7)
!        TS.USER.RETURN  = 80
        !
				If Len(EP.ALL.CARD$) = 0 Then Begin
				  !tmpCard$ = EP.TEF.TOKEN$
				  tmpCard$ = Left$(EP.CARD.BIN$ + String$(16, "0"), 16)
				Endif Else Begin
				  tmpCard$ = EP.ALL.CARD$
				Endif
				!
				If EP.APPL$ = "09" Or EP.APPL$ = "11" Then Begin
					EP.EFT.CMP.TRX%     = -1
				Endif
				!
        EP.TEF.DETAIL%  = -1
        EP.TEF.SMA%     = -1
        TS.ACNUM$       = tmpCard$
        !		
        CALL EP.LINE.PRINT("Autorizacion="+EP.AUTH.NUMBER$+" Recibo="+EP.EPAY.TRANSNUM$,6100h)
        IF TS.PROCEDURE = 2 THEN \
          TO.XCHGLIM    = 0
      ENDIF \
      ELSE \
      BEGIN 
!      CALL TRANSLATE.ISO.CODE(EP.APPROV.CODE$,EP.MESSAGE.DESC$)
        CALL EP.DISPLAY.AN.ERROR(EP.APPROV.DESC$)
        TS.USER.RETURN  = 18        ! rechazado en linea 
!        EP.EFT.CMP.TRX%     = 0
!        EP.TEF.DETAIL%  = 0
!        EP.TEF.SMA%     = 0      
      ENDIF
    ENDIF     ! end trx ok
!    IF EP.VERIF.TV.SAV$ <> 2 THEN \
!      TO.TENDOPTS(EP.TV.POS%,7) = EP.VERIF.TV.SAV%   ! devuelve el tipo de verificacion anterior
  ENDIF
!
END SUB         ! end EFT 32 ROUTINE
!
SUB UETEF40 PUBLIC
	! Si el tipo variedad corresponde a tef
	! se aprueba aunque no cumpla la validación de 
	! límites
	IF EP.medioPagoTef% = -1 THEN \
	BEGIN
		EP.medioPagoTef% = 0
		TS.USER.RETURN = -1
	ENDIF
END SUB
!
Sub UETEF20 Public
	If 															\
			(TS.LINETYPE = 6 And 				\
			 TS.LINEDATA = 1 And 				\
			 Not TS.TRAINING And  			\
			 TS.PROCEDURE < 1 And 			\
			 TS.TRX.STATUS <> 100 And 	\
			 (TS.IO.KEYS(1) <> 82 Or 		\
			  TS.IO.KEYS(6) <> 81)) Or 	\
			(TS.LINETYPE = 12 And 			\
			 TS.LINEDATA = 2 And 				\
			 (TS.PROCEDURE = 1 Or 			\
			  TS.PROCEDURE = 2) And 		\
			 Not TS.TRAINING ) 					\
	Then Begin
		Call saveTefVouchers
	Endif
End Sub
!
Function APITEF5.getTenderMenu(pIndex%) Public
	String APITEF5.getTenderMenu
	Integer*2 pIndex%
	!
	APITEF5.getTenderMenu = getProperty("TENDER_MENU_" + Str$(pIndex%), tefParamData$)
End Function
!
Sub APITEF5.executePayment(pPaymentData$, pAccount$, pCustomer$) Public
	String pPaymentData$, pAccount$, pCustomer$
	!
	APITEF5.externalTefData$ = pPaymentData$
	APITEF5.externalAccount$ = pAccount$
	APITEF5.externalCustomer$ = pCustomer$
	!
	TS.IO.DEVICE = 1
	Dim TS.IO.KEYS(10)
	Dim TS.IO.DATA$(10)
	TS.IO.MOTORKEY = 179
	TS.IO.KEYS(7) = 179
End Sub
!
END

