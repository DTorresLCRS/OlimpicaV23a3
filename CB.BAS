!***********************************************************************************************
!	FILE NAME	:	CB.BAS
!	AUTHOR		:	Javier Sarmiento Valdivieso
!					La Cadena Retail Solutions Ltda
!	SUBJECT		:	Modulo para transacciones de corresponsales bancarios
!
!***********************************************************************************************
!	RELEASE HISTORY
!***********************************************************************************************
!	V. 1.0	2013-12-15	Javier Sarmiento Valdivieso
!	Versión inicial
!
!	V. 1.1	2017-09-06	Javier Sarmiento Valdivieso
!	Se hacen los siguientes ajustes para reforzar la secuencia de ingreso de un recaudo:
!	Agregar en la rutina enterCodeForPlu lógica para limpiar las variables de secuencias,
!	poner el TS.IO.DEVICE en 1 y el TS.IO.STATE en 10 (main)
!
!	V. 1.2	2021-02-08	Javier Sarmiento Valdivieso
!	Mantenimiento del módulo para que, en caso de pagos, se realice el ingreso del PLU antes de 
!	hacer la operación contra el datáfono. En el caso en el cual la operación sea rechazada, se 
!	realizará una secuencia automática de anulación de la transacción.
!	Se quita llamado en user exit 20 y se agrega llamado en user exit 23
!	Se implementa invocación automática de TEF
!
!	V. 1.3	2021-04-20	Javier Sarmiento Valdivieso
!	Mantenimiento del módulo para permitir invocar aplicaciones java externas con el fin de 
!	capturar datos, en vez de solicitarlos directamente al usuario
!
!	V. 1.4	2021-11-11	Javier Sarmiento Valdivieso
!	Se realiza ajuste para que, en caso de que el datáfono retorne un monto de transacción 
!	diferente al especificado inicialmente en un recaudo, se anule el PLU ingresado y se 
!	ingrese automáticamente el PLU con el valor modificado
!	Se realiza ajuste para controlar medios de pago en recaudos
!
!	V. 1.5	2022-06-28	Javier Sarmiento Valdivieso
!	Se agrega lógica para que en aquellos recaudos o pagos que no piden monto en la caja, 
!	se continúe ejecutando la operación contra el datáfono antes de agregar el PLU, ya que 
! el valor del PLU está sujeto a dicho monto y debe venir en la respuesta del datáfono
!
!	V. 1.6	2022-08-24	Javier Sarmiento Valdivieso
! Se realizan ajustes a secuencia de consulta de saldo porque se está presentando error 
! de aplicación en REDEBAN
!
!***********************************************************************************************
!
%ENVIRON T
!
!-----------------------------------------------------------------------------------------------
! DEFINICION DE VARIABLES
!-----------------------------------------------------------------------------------------------
!
! VARIABLES GLOBALES DE SMA
String Global 		\
	TS.TERMINAL$, 	\
	TS.OPER$, 			\
	TS.PSWD$,				\
	TS.IO.DATA$(1), \
	TS.USERDATA$, 	\
	IR.LINKEDTO$, 	\
	IR.ITEMCODE$,		\
	IR.ITEMNAME$,		\
	TS.ACNUM$,			\
	TS.PRTBUF$,			\
	SL.STR$(1),			\
	TS.IO.HDR$,			\
	SL.IT.ITEMCODE$,\
	TS.DISP1$,			\
	TS.DISP2$,			\
	TS.SDESC$(1),		\
	TE.TR.TRANTYPE$
Integer*4 Global 	\
	SL.HD.TRANSNUM, \
	SL.IT.XPRICE, 	\
	TE.TR.AMT(1), 	\
	TO.CDLIMIT1, 		\
	TO.CDLIMIT2,		\
	TS.TOTALS(3),		\
	TO.NEGAMT(1),		\
	TS.XPRICE,			\
	TO.TENDLIMITS(2),\
	SL.TE.AMTTENDE,	\
	TS.STACKERR(1),	\
	TO.VOIDLIMIT
Integer*2 Global 	\
	TS.IO.MOTORKEY, \
	TS.IO.KEYS(1), 	\
	TS.PROCEDURE, 	\
	TS.TEMP1I1,			\
	TS.IO.INPUT.READY,\
	TO.NEGCNT(1),		\
	TS.AUTO.BAL,		\
	TS.TEMP1I2,		\
	TS.LINETYPE,	\
	TS.LINEDATA,	\
	TS.TRX.STATUS,	\
	TS.BAL.TAKEN,	\
	SL.TE.TENDTYPE,	\
	SL.TE.TENDVAR,	\
	TS.ER.RETURN,		\
	SL.NEXT,				\
	SL.END,					\
	TS.INTRX,				\
	TS.IO.STATE,		\
	TS.IO.DEVICE,		\
	TS.USER.RETURN
Integer*1 Global 	\
	IR.INDICAT0,		\
	SL.HD.TRANTYPE,	\
	TO.USEREXIT(1)
!
!
! VARIABLES GLOBALES DE USUARIO
Integer*2 Global 	\
	EP.IOPARM%,			\
	EP.settlementKey%
!
Integer*1 Global USR.exitFunction%
!
! VARIABLES LOCALES DEL MÓDULO
Integer*4 			\
	CB.voidLimit%,\
	CB.negAmt7%,	\
	CB.maxAmt%
Integer*2 			\
	CB.KEY%, 		\
	CB.ACTUALNET%,	\
	CB.negCnt7%,	\
	CB.FINDEX%,		\
	CB.FINDEX1%,	\
	CB.dataIndex%(1),\
	CB.dataIndexSize%
Integer*1 			\
	CB.status%,		\
	CB.neg%,			\
	CB.trxPend%,	\
	CB.userData%,	\
	CB.skipTendVal%
String 				\
	CB.UFIELDS$, 	\
	CB.UDKEY$,		\
	CB.ERRORKEYS$,	\
	CB.AFAPPLS$,	\
	CB.TV$,				\
	CB.propData$
	!
	!------------------------------------------------------------------------------------
	! 2021-02-08 V. 1.2 jsv
	! Ya no se va a usar la referencia de línea de artículo para identificar un pago
	!------------------------------------------------------------------------------------
	!CB.lineaArticulo$
	!
	!------------------------------------------------------------------------------------
	!
String 				\
	CB.AMOUNT$, 	\
	CB.CODE1$, 		\
	CB.CODE2$,	\
	CB.TYPE$,		\
	CB.AUTH.NUMBER$,	\
	CB.CARD.NUMBER$,	\
	CB.RECIBO$,	\
	CB.APPLFUNC$,	\
	CB.APPLFUNC1$,\
	CB.ITEMNAME$
!
!-----------------------------------------------------------------------------------------------
! RUTINAS EXTERNAS DE SMA
!-----------------------------------------------------------------------------------------------
Sub TSTPEC01 External
End Sub
!
Sub TSTDEC01 External ! Access tender
End Sub
!
Function WRITE.MATRIX.SL.STR(SESS.NO, INDEX, ELEMENTS) External
	Integer*2 SESS.NO, INDEX, ELEMENTS
End Function
!
Sub TSHIECET External
End Sub
!
!-----------------------------------------------------------------------------------------------
! RUTINAS EXTERNAS DE USUARIO
!-----------------------------------------------------------------------------------------------
!
Function getStoreLine External
  String getStoreLine
End Function
!
Sub EP.SAVE.PRINT External
End Sub
!
Sub EP.RESTORE.PRINT External
End Sub
!
Sub saveLastSequence External
End Sub
!
Sub restoreLastSequence External
End Sub
!
Function getLastSequence External
	String getLastSequence
End Function
!
Sub addLastSequence(pSequence$) External
	String pSequence$
End Sub
!
Sub launchAutoTefTrx(pAmount%, pApplFunc$) External
	Integer*4 pAmount%
	String pApplFunc$
End Sub
!
Sub TEFII.FORMAT.setDataAppl(pData$) External
	String pData$
End Sub
!
Function TEFII.FUNCT.ISSYNCBYDEMAND External
	Integer*1 TEFII.FUNCT.ISSYNCBYDEMAND
End Function
!
Function EP.IS.ITEM.LINE.20 External
	Integer*1 EP.IS.ITEM.LINE.20
End Function
!
Function EP.BUSCA.INDICE.TV(UE.TIPO.VAR$) External
	Integer*2 EP.BUSCA.INDICE.TV
	String UE.TIPO.VAR$
End Function
!
Sub EP.SAVE.KEYS External
End Sub
!
Sub EP.RESTORE.KEYS External
End Sub
!
Function TPONLINE.getCustId External
	String TPONLINE.getCustId
End Function
!
Function tef.getDefaultTV External
	String tef.getDefaultTV
End Function
!
Function tef.getDefaultEntity External
	Integer*2 tef.getDefaultEntity
End Function
!
Function isApprovCode(pCode$) External
	Integer*1 isApprovCode
	String pCode$
End Function
!
Sub reasignEntity(pTipoVariedad%, pEntity%) External
	Integer*2 pTipoVariedad%, pEntity%
End Sub
!
Function TEFII.FUNCT.getConstant(pIndex%, pField$) External
	Integer*2 pIndex%
	String TEFII.FUNCT.getConstant, pField$
End Function
!
Function TEFII.FUNCT.GETAMT External
	String TEFII.FUNCT.GETAMT
End Function
!
Function TEFII.FUNCT.GETAMTINDEX External
	Integer*2 TEFII.FUNCT.GETAMTINDEX
End Function
!
Function TEFII.FUNCT.TRX01$(TRAMA$, FUNNUMBER%, pNotifyAcc%) External
	String TEFII.FUNCT.TRX01$, TRAMA$
	Integer*1 FUNNUMBER%
	Integer*2 pNotifyAcc%
End Function
!
Function tef.getActualNet External
	Integer*2 tef.getActualNet
End Function
!
Function TEFII.FUNCT.GETFINDEX(pApplFunc$) External
	Integer*2 TEFII.FUNCT.GETFINDEX
	String pApplFunc$
End Function
!
Sub TEFII.FUNCT.resetConstants(pIndex%) External
	Integer*2 pIndex%
End Sub
!
Sub TEFII.FUNCT.assignConstant(pIndex%, pField$, pValue$) External
	Integer*2 pIndex%
	String pField$, pValue$
End Sub
!
Function TEFII.FORMAT.GETFIELD$(INDEX%) External
	Integer*2 INDEX%
	String TEFII.FORMAT.GETFIELD$
End Function
!
Sub disc.temporaryInactive External
End Sub
!
Function EP.invocarApplManager0(epAppl$,epFunction$,parteVariable$,usaConsecutivo%, pNoMsg%) External
	String EP.invocarApplManager0, epAppl$, epFunction$, parteVariable$
	Integer*1 usaConsecutivo%, pNoMsg%
End Function
!
Sub asignarVariableGlobal(pName$, pValue$) External
	String pName$, pValue$
End Sub
!
Sub printDebug(pMsg$) External
	String pMsg$
End Sub
!
Sub readPropData(pFilename$, pPropData$, pSession%, pReturn%) External
	String pFilename$, pPropData$
	Integer*2 pSession%, pReturn%
End Sub
!
Function getProperty(pProperty$, pData$) External
	String getProperty, pProperty$, pData$
End Function
!
Sub EP.DISPLAY.AN.ERROR(UE.DISP.MESSAGE$) External 
	String UE.DISP.MESSAGE$
End Sub
!
Sub EP.DISPLAY.A.MESSAGE(UE.DISP.MESSAGE$) External
	String UE.DISP.MESSAGE$
End Sub
!
Sub EP.LINE.PRINT(LINEA$,ESTACION%) External
	String LINEA$
	Integer*2 ESTACION%
End Sub
!
Function TEFII.FORMAT.RTRIM$(VALUE$) External
	String VALUE$, TEFII.FORMAT.RTRIM$
!	Integer*2 tmpCounter%
!	Integer*1 tmpContinue%
!	!
!	tmpContinue% = -1
!	tmpCounter% = Len(VALUE$)
!	While tmpContinue% = -1 And tmpCounter% >= 1
!		If Asc(Mid$(VALUE$, tmpCounter%, 1)) = 32 Then \ 
!			tmpCounter% = tmpCounter% - 1 \
!		Else \
!			tmpContinue% = 0
!	Wend
!	TEFII.FORMAT.RTRIM$ = Left$(VALUE$, tmpCounter%)
End Function
!
Sub EP.GET.KBDATA(UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, UE.KEYB.DATA$) External
	String UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, UE.KEYB.DATA$
End Sub
!
Sub tef.getTender(pFranquicia$, pBin$, pApplFunc$, pProduct$, pTender$, pEntity%) External
	String pFranquicia$, pBin$, pApplFunc$, pProduct$, pTender$
	Integer*2 pEntity%
End Sub
!
Sub TRANSLATE.APPL.CODE(UE.CODE$,UE.CODE.DESC$) External
	String UE.CODE$,UE.CODE.DESC$
End Sub
!
Sub TRANSLATE.COMM.CODE(UE.CODE$,UE.CODE.DESC$) External
	String UE.CODE$,UE.CODE.DESC$
End Sub
!
Sub setRepeatLastSequence(pValue%) External
	Integer*1 pValue%
End Sub
!
Sub getNextField(pData$, pLastIndex%, pSeparator$, pNextField$) External
	String pNextField$, pData$, pSeparator$
	Integer*2 pLastIndex%
End Sub
!
Sub postKeySequence(pSequence$, pReturn%) External
	String pSequence$
	Integer*2 pReturn%
End Sub
!
Sub autoPrChg.changePrice(pCode$, pCurrentXPrice%, pNewXPrice%) External
	String pCode$
	Integer*4 pCurrentXPrice%, pNewXPrice%
End Sub
!
Function isIncluded(pValue%, pIncludedRange$, pExcludedRange$) External
	Integer*1 isIncluded
	Real pValue%
	String pIncludedRange$, pExcludedRange$
End Function
!
Sub TEFII.FUNCT.setFieldsToRestore(pFields$) External
	String pFields$
End Sub
!
!-----------------------------------------------------------------------------------------------
! RUTINAS LOCALES DEL MÓDULO
!-----------------------------------------------------------------------------------------------
Sub tenderValidation(tmpReturn%, tmpMessage$)
	Integer*2 tmpReturn%
	String tmpMessage$, tmpTV$
	!
	tmpReturn% = -1
	!
	If CB.skipTendVal% Then Begin
		CB.skipTendVal% = 0
	Endif Else Begin
		! Tipo variedad
		If TS.IO.DATA$(3) = "" Then Begin
			tmpTV$ = Str$(TS.IO.KEYS(7) - 90) + "1"
		Endif Else Begin
			tmpTV$ = Str$(TS.IO.KEYS(7) - 90) + TS.IO.DATA$(3)
		Endif
		Call printDebug("tenderValidation tv=" + tmpTV$)
		If tmpTV$ <> "11" Then Begin
			tmpReturn% = 0
			tmpMessage$ = "SOLO SE PERMITE PAGOEN EFECTIVO"
		Endif Else If Val(TS.IO.DATA$(7)) > 0 And Val(TS.IO.DATA$(7)) < TS.TOTALS(0, 0, 0) Then Begin
			tmpReturn% = 0
			tmpMessage$ = "NO SE PERMITEN PAGOSPARCIALES"
		Endif
	Endif
End Sub
!
Sub totalizar
	TS.AUTO.BAL = 1
	Dim TS.IO.KEYS(10) ! Clear input and
	TS.IO.MOTORKEY = 81
	TS.IO.KEYS(6) = 81 ! substitute a TOTAL key
	TS.TEMP1I2 = 1 !
End Sub
!
!------------------------------------------------------------------------------------
! 2021-02-08 V. 1.2 jsv
! Ya no se va a usar la referencia de línea de artículo para identificar un pago
!------------------------------------------------------------------------------------
!Function isItemLine60
!	Integer*1 isItemLine60
!	!
!	If \
!			Mid$(TS.PRTBUF$, 9,15) = Mid$(CB.lineaArticulo$, 9,15) And \
!			Mid$(TS.PRTBUF$,28,10) = Mid$(CB.lineaArticulo$,28,10) Then \
!		isItemLine60 = -1 \
!	Else \
!		isItemLine60 = 0
!End Function
!
!------------------------------------------------------------------------------------
!
Sub resetVariables
	CB.AMOUNT$ = ""
	CB.CODE1$ = ""
	CB.CODE2$ = ""
	CB.status% = 0
	CB.neg% = 0
	CB.maxAmt% = 0
	CB.TV$ = ""
	CB.AUTH.NUMBER$ = ""
	CB.CARD.NUMBER$ = ""
	CB.RECIBO$ = ""
	CB.TYPE$ = ""
	CB.APPLFUNC$ = ""
	CB.APPLFUNC1$ = ""
	CB.FINDEX% = 0
	CB.FINDEX1% = 0
	!
	CB.trxPend% = 0
	CB.userData% = 0
	!
	CB.skipTendVal% = 0
End Sub
!
Function getConfirmFunction(pFunction$)
	String getConfirmFunction, pFunction$, tmpParam$
	!
	tmpParam$ = "CB.CONFIRMFUNC." + pFunction$ + "." + Right$(String$(3, "0") + Str$( tef.getActualNet ), 3)
	!
	Call printDebug("getConfirmFunction(" + pFunction$ + ") Param=" + tmpParam$)
	!
	getConfirmFunction = getProperty(tmpParam$, CB.propData$)
End Function
!
Function getPersistFields(pFunction$)
	String getPersistFields, pFunction$, tmpParam$
	!
	tmpParam$ = "CB.PERSISTFIELDS." + pFunction$ + "." + Right$(String$(3, "0") + Str$( tef.getActualNet ), 3)
	!
	Call printDebug("getPersistFields(" + pFunction$ + ") Param=" + tmpParam$)
	!
	getPersistFields = getProperty(tmpParam$, CB.propData$)
End Function
!
Function getItemName(pFunction$)
	String getItemName, pFunction$, tmpParam$
	!
	tmpParam$ = "CB.ITEMNAME." + pFunction$ + "." + Right$(String$(3, "0") + Str$( tef.getActualNet ), 3)
	!
	Call printDebug("getItemName(" + pFunction$ + ") Param=" + tmpParam$)
	!
	getItemName = getProperty(tmpParam$, CB.propData$)
End Function
!
Function getTenderSeq(pFunction$)
	String getTenderSeq, pFunction$, tmpParam$
	!
	tmpParam$ = "CB.TENDERSEQ." + pFunction$ + "." + Right$(String$(3, "0") + Str$( tef.getActualNet ), 3)
	!
	Call printDebug("getTenderSeq(" + pFunction$ + ") Param=" + tmpParam$)
	!
	getTenderSeq = getProperty(tmpParam$, CB.propData$)
End Function
!
Function generateHeader(pTerminal%, pTransnum%, pTrantype%, pNumstring%, \
		pOperator$, pPassword$, \
		pGrosspos%, pGrossneg%, pRingtime%, pTendertime%, pSpecialtime%, pInactivetime%, pIndicat1%)
	String generateHeader, tmpAnswer$, tmpSep$, pOperator$, pPassword$
	Integer*2 pTerminal%, pTransnum%, pTrantype%, pNumstring%
	Integer*4 pGrosspos%, pGrossneg%, pRingtime%, pTendertime%, pSpecialtime%, pInactivetime%, pIndicat1%
	!
	tmpSep$ = Chr$(3AH)
	!
	tmpAnswer$ = Pack$("00") + tmpSep$ + Pack$(Right$(String$(4, "0") + Str$(pTerminal%), 4)) + tmpSep$ + Pack$(Right$(String$(4, "0") + Str$(pTransnum%), 4)) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + Pack$(Left$(Date$ + Time$, 10)) + tmpSep$ + Pack$(Right$(String$(2, "0") + Str$(pTrantype%), 2)) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + Pack$(Str$(pNumstring%)) + tmpSep$ + Pack$(pOperator$) + tmpSep$ + Pack$(pPassword$) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + Pack$(Str$(pGrosspos%)) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + Pack$(Str$(pGrossneg%)) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + Pack$(Str$(pRingtime%)) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + Pack$(Str$(pTendertime%)) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + Pack$(Str$(pSpecialtime%)) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + Pack$(Str$(pInactivetime%)) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + Pack$(Str$(pIndicat1%))
	!
	generateHeader = tmpAnswer$
End Function
!
Sub processTender(pTenderVar$, pTenderAmount%, pTenderAccount$)
	String pTenderVar$, pTenderAccount$, previousAccount$
	Integer*4 pTenderAmount%, previousTendAmt%, previousTendLimit%
	Integer*2 previousTendType%, previousTendVar%, tmpIndex%
	!
	Call printDebug("processTender(" + pTenderVar$ + "," + str$(pTenderAmount%) + ")")
	!
	tmpIndex% = EP.BUSCA.INDICE.TV(pTenderVar$)
	If tmpIndex% <> -1 Then Begin
		previousTendLimit% = TO.TENDLIMITS(tmpIndex%, 0)
		Call printDebug("previousTendLimit%=" + str$(previousTendLimit%))
		! Limite maximo para integer de 4
		TO.TENDLIMITS(tmpIndex%, 0) = 2147483647
	Endif
	!
	Call printDebug("saving properties...")
	previousTendAmt% = SL.TE.AMTTENDE
	previousTendType% = SL.TE.TENDTYPE
	previousTendVar% = SL.TE.TENDVAR
	previousAccount$ = TS.ACNUM$
	Call EP.SAVE.KEYS		! Save current key state
	!
	Call printDebug("assigning new properties...")
	SL.TE.AMTTENDE = pTenderAmount%
	SL.TE.TENDTYPE = Val(Left$(pTenderVar$, 1))
	SL.TE.TENDVAR = Val(Right$(pTenderVar$, 1))
	TS.ACNUM$ = pTenderAccount$
	!
	Dim TS.IO.KEYS(10)	! Clear input
	TS.IO.KEYS(3) = 78	! Asterisk key
	TS.IO.DATA$(3) = Str$(SL.TE.TENDVAR)	! Tender variety
	TS.IO.KEYS(7) = SL.TE.TENDTYPE + 90		! Tender key
	TS.IO.DATA$(7) = STR$(SL.TE.AMTTENDE)	! Tender amount
	If TS.ACNUM$ <> "" Then Begin 			! Account # present
		TS.IO.KEYS(9) = 90 					! Verify key
		TS.IO.DATA$(9) = TS.ACNUM$ 			! Account number
	EndIf
	If SL.TE.TENDTYPE = 3 Then Begin 		! Food stamps so
		TS.TRX.STATUS = -1					! set F.S. bal due
	Endif
	Call printDebug("calling TSTDEC01...")
	Call TSTDEC01							! Call for tender processing
	!
	Call printDebug("restoring properties...")
	! Restore previous values
	Call EP.RESTORE.KEYS
	SL.TE.AMTTENDE = previousTendAmt%
	SL.TE.TENDTYPE = previousTendType%
	SL.TE.TENDVAR = previousTendVar%
	TS.ACNUM$	= previousAccount$
	!
	If tmpIndex% <> -1 Then Begin
		TO.TENDLIMITS(tmpIndex%,0) = previousTendLimit%
	Endif
	!
	Call printDebug("end processTender")
	!
End Sub
!
Sub CB.autoFinish
	TS.AUTO.BAL = 1
	Dim TS.IO.KEYS(10) ! Clear input and
	TS.IO.MOTORKEY = 81
	TS.IO.KEYS(6) = 81 ! substitute a TOTAL key
	TS.TEMP1I2 = 1 !
End Sub
!
Function getAFAppls
	String getAFAppls
	Integer*2 newNet%
	!
	newNet% = tef.getActualNet
	If newNet% <> CB.ACTUALNET% Then Begin
		CB.ACTUALNET% = newNet%
		CB.AFAPPLS$ = "," + getProperty("CB.AUTOFINISH.APPLS." + Right$(String$(3, "0") + Str$(CB.ACTUALNET%), 3), CB.propData$) + ","
	Endif
	getAFAppls = CB.AFAPPLS$
End Function
!
Function isAutoFinishAppl(pApplFunc$)
	Integer*1 isAutoFinishAppl
	String pApplFunc$, tmpAFAppls$
	Integer*2 tmpIndex%
	!
	tmpAFAppls$ = getAFAppls
	!
	Call printDebug("isAutoFinishAppl(" + pApplFunc$ + ") appls=" + tmpAFAppls$)
	!
	tmpIndex% = Match("," + pApplFunc$ + ",", tmpAFAppls$, 1)
	If tmpIndex% > 0 Then Begin
		isAutoFinishAppl = -1
	Endif Else Begin
		isAutoFinishAppl = 0
	Endif
End Function
!
Function cb.errorParamKey(numKey%)
!
    String cb.errorParamKey, numKey$, parToSearch$   
    Integer*2 indexMsgError%, indexErrPar%, indexNextMsg%, numKey%
	numKey$ = str$(numKey%)
	If CB.CODE1$ <> "" Then Begin
		parToSearch$ = ","+numKey$+":"
		indexErrPar% = Match(parToSearch$,CB.ERRORKEYS$,1)
		If indexErrPar%>0 Then Begin
			indexMsgError% = Len(parToSearch$)+indexErrPar%
			indexNextMsg% = Match(",",CB.ERRORKEYS$,indexMsgError%)
			cb.errorParamKey = Mid$(CB.ERRORKEYS$,indexMsgError%,indexNextMsg%-indexMsgError%)
		Endif Else Begin
			cb.errorParamKey = ""
		Endif
	Endif Else Begin
		cb.errorParamKey = ""
	Endif
!
END Function
!
Function getUFields
	String getUFields
	Integer*2 newNet%
	!
	newNet% = tef.getActualNet
	If newNet% <> CB.ACTUALNET% Then Begin
		CB.ACTUALNET% = newNet%
		CB.UFIELDS$ = getProperty("CB.UFIELDS." + Right$(String$(3, "0") + Str$(CB.ACTUALNET%), 3), CB.propData$)
	Endif
	getUFields = CB.UFIELDS$
End Function
!
Function getUFieldsValues
	String getUFieldsValues, tmpFields$, tmpAnswer$
	Integer*2 tmpIndex0%, tmpIndex1%
	!
	tmpAnswer$ = ""
	tmpFields$ = getUFields
	tmpIndex0% = 0
	tmpIndex1% = Match(",", tmpFields$, tmpIndex0% + 1)
	While tmpIndex1% > 0
		tmpAnswer$ = tmpAnswer$ + TEFII.FORMAT.GETFIELD$(Int%(Val(Mid$(tmpFields$, tmpIndex0% + 1, tmpIndex1% - tmpIndex0% - 1)))) + ":"
		tmpIndex0% = tmpIndex1%
		tmpIndex1% = Match(",", tmpFields$, tmpIndex0% + 1)
	Wend
	tmpIndex1% = Len(tmpFields$) + 1
	getUFieldsValues = tmpAnswer$ + TEFII.FORMAT.GETFIELD$(Int%(Val(Mid$(tmpFields$, tmpIndex0% + 1, tmpIndex1% - tmpIndex0% - 1))))
End Function
!
Sub writeCBUserData(pApplFunc$, pType$, pAddData$)
	String pApplFunc$, pType$, pAddData$, pFullAnswer$
	!
	If Not CB.userData% Then Begin
		CB.userData% = -1
		TS.USERDATA$ = Pack$(CB.UDKEY$ + pApplFunc$ + "0" + pType$) + ":" + \
			getUFieldsValues + pAddData$
		TS.TEMP1I1 = 99                  
		Call TSTPEC01
	Endif
End Sub
!
Function generateUserData(pApplFunc$, pType$, pAddData$)
	String generateUserData, tmpAnswer$, tmpSep$, pApplFunc$, pType$, pAddData$
	!
	tmpSep$ = Chr$(3AH)
	!
	tmpAnswer$ = Pack$("99") + tmpSep$ + Pack$(CB.UDKEY$ + pApplFunc$ + "0" + pType$) + tmpSep$
	tmpAnswer$ = tmpAnswer$ + getUFieldsValues + pAddData$
	!
	generateUserData = tmpAnswer$
End Function
!
Sub balanceQuery(pApplFunc$, pType$, pAddData$)
	String pApplFunc$, pType$, pAddData$
	!
	SL.STR$(0) = generateHeader(Int%(Val(TS.TERMINAL$)), SL.HD.TRANSNUM + 1, 7, 1, \
		Unpack$(TS.OPER$), Unpack$(TS.PSWD$), \
		0, 0, 7, 30, 30, 30, 0)
	SL.STR$(1) = generateUserData(pApplFunc$, pType$, pAddData$)
	!
	TS.ER.RETURN = -1
	SL.NEXT = 0
	SL.END = 1
	Call WRITE.MATRIX.SL.STR(27, SL.NEXT, SL.END - SL.NEXT + 1)
	!
	SL.HD.TRANSNUM = SL.HD.TRANSNUM + 1
	!
	SL.END = 0
	TS.ER.RETURN = 0
	SL.HD.TRANTYPE = 0
	!
End Sub
!
Sub splitNumericalValues(pFullValue$, pValue1$, pValue2$, pValue3$, pValue4$)
	String pFullValue$, pValue1$, pValue2$, pValue3$, pValue4$
	Integer*2 tmpCounter%, tmpLen%, tmpAsc%
	Integer*1 tmpFound%
	!
	tmpLen% = Len(pFullValue$)
	tmpCounter% = 1
	tmpFound% = 0
	pValue1$ = ""
	While tmpFound% = 0 And tmpCounter% <= tmpLen%
		tmpAsc% = Asc(Mid$(pFullValue$, tmpCounter%, 1))
		! Acepta dígitos y signo (-)
		If (tmpAsc% < 48 Or tmpAsc% > 57) And tmpAsc% <> 45 Then Begin
			tmpFound% = -1
		Endif Else Begin
			pValue1$ = pValue1$ + Mid$(pFullValue$, tmpCounter%, 1)
		Endif
		tmpCounter% = tmpCounter% + 1
	Wend
	!
	tmpFound% = 0
	pValue2$ = ""
	While tmpFound% = 0 And tmpCounter% <= tmpLen%
		tmpAsc% = Asc(Mid$(pFullValue$, tmpCounter%, 1))
		! Acepta dígitos y signo (-)
		If (tmpAsc% < 48 Or tmpAsc% > 57) And tmpAsc% <> 45 Then Begin
			tmpFound% = -1
		Endif Else Begin
			pValue2$ = pValue2$ + Mid$(pFullValue$, tmpCounter%, 1)
		Endif
		tmpCounter% = tmpCounter% + 1
	Wend
	!
	tmpFound% = 0
	pValue3$ = ""
	While tmpFound% = 0 And tmpCounter% <= tmpLen%
		tmpAsc% = Asc(Mid$(pFullValue$, tmpCounter%, 1))
		! Acepta dígitos y signo (-)
		If (tmpAsc% < 48 Or tmpAsc% > 57) And tmpAsc% <> 45 Then Begin
			tmpFound% = -1
		Endif Else Begin
			pValue3$ = pValue3$ + Mid$(pFullValue$, tmpCounter%, 1)
		Endif
		tmpCounter% = tmpCounter% + 1
	Wend
	!
	tmpFound% = 0
	pValue4$ = ""
	While tmpFound% = 0 And tmpCounter% <= tmpLen%
		tmpAsc% = Asc(Mid$(pFullValue$, tmpCounter%, 1))
		! Acepta dígitos y signo (-)
		If (tmpAsc% < 48 Or tmpAsc% > 57) And tmpAsc% <> 45 Then Begin
			tmpFound% = -1
		Endif Else Begin
			pValue4$ = pValue4$ + Mid$(pFullValue$, tmpCounter%, 1)
		Endif
		tmpCounter% = tmpCounter% + 1
	Wend
	!
End Sub
!
Sub enterCodeForPlu(pCode$)
	String pCode$
	Integer*4 tmpValue%
	!
	Call printDebug("enterCodeForPlu(" + pCode$ + ")")
	!
	TS.IO.MOTORKEY = 0
	Dim TS.IO.DATA$(10)
	Dim TS.IO.KEYS(10)
	TS.IO.DEVICE = 1
	TS.IO.STATE = 10
	!
	tmpValue% = Int%(Val(pCode$))
	If tmpValue% < 0 Then Begin
		TS.IO.KEYS(1) = 70
		pCode$ = Str$(Abs(tmpValue%))
		CB.neg% = -1
	Endif
	TS.IO.DATA$(2) = pCode$
	TS.IO.KEYS(2) = 80
	TS.IO.MOTORKEY = 80
End Sub
!
Sub parseCbAnswer(pFullAnswer$)
	String pFullAnswer$
	!
	CB.AUTH.NUMBER$ = Mid$(pFullAnswer$, 12, 6)
	CB.CARD.NUMBER$ = Mid$(pFullAnswer$, 40, 4)
	CB.RECIBO$ = Mid$(pFullAnswer$, 110, 10)
End Sub
!
Sub printHeader
	Integer*1 tmpUe20%, tmpUe60%, tmpCounter%
	!
	tmpUe20% = TO.USEREXIT(20)
	tmpUe60% = TO.USEREXIT(60)
	TO.USEREXIT(20) = 0
	TO.USEREXIT(60) = 0
	!
	Call EP.SAVE.PRINT
	Call EP.LINE.PRINT(TS.SDESC$(174), 6100H)
	Call EP.LINE.PRINT(getStoreLine, 6100H)
	!
	For tmpCounter% = 1 To 7
		Call EP.LINE.PRINT("", 6100H)
	Next tmpCounter%
	!
	Call EP.RESTORE.PRINT
	!
	TO.USEREXIT(20) = tmpUe20%
	TO.USEREXIT(60) = tmpUe60%
End Sub
!
Sub printAuthLines
	Integer*1 tmpUe20%, tmpUe60%
	!
	tmpUe20% = TO.USEREXIT(20)
	tmpUe60% = TO.USEREXIT(60)
	TO.USEREXIT(20) = 0
	TO.USEREXIT(60) = 0
	!
	! Escribe leyenda en tirilla y en journal
	Call EP.LINE.PRINT("Nro. Autoriz: " + CB.AUTH.NUMBER$, 6100H)
	Call EP.LINE.PRINT("Nro. Recibo : " + CB.RECIBO$, 6100H)
	!
	TO.USEREXIT(20) = tmpUe20%
	TO.USEREXIT(60) = tmpUe60%	
End Sub
!
Sub applyCBTrx(pApplFunc$, pType$, pFullAnswer$, pMiscData$, pLocalAmt%)
	String pApplFunc$, pType$, pFullAnswer$, pMiscData$
	Integer*4 pLocalAmt%
	String tmpTV$, tmpFranquicia$, tmpBin$, tmpProduct$, tmpAddData$
	Integer*2 tmpEntity%
	!
	!
	tmpAddData$ = ""
	!
	tmpFranquicia$ = Mid$(pFullAnswer$ ,100, 10)
	tmpBin$ = Mid$(pFullAnswer$ ,134, 6)
	tmpProduct$ = Mid$(pFullAnswer$ ,120, 10)
	Call parseCbAnswer(pFullAnswer$)
	!	
	If pType$ = "1" Then Begin
		! Se trata de un retiro
		! Debe buscar el tipo variedad y la entidad
		! Estos datos pueden estar predefinidos segun la opcion de menu
		! seleccionada. Si no es asi, se deben buscar los datos
		! correspondientes a la franquicia y tipo de cuenta
		!
		If Len(CB.TV$) > 2 Then Begin
			tmpTV$ = Left$(CB.TV$, 2)
			tmpEntity% = Int%(Val(Right$(CB.TV$, Len(CB.TV$) - 2)))
			CB.TV$ = ""
		Endif Else Begin
			Call tef.getTender(tmpFranquicia$, tmpBin$, pApplFunc$, tmpProduct$, tmpTV$, tmpEntity%)
			!
			If tmpTV$ = "00" Then Begin
				tmpTV$ = tef.getDefaultTV
				tmpEntity% = tef.getDefaultEntity
			Endif
		Endif
		!
		tmpAddData$ = ":" + tmpTV$ + ":" + Str$(tmpEntity%)
		!
		If TS.PROCEDURE < 1 Then Begin
			TS.PROCEDURE = 1
			TE.TR.TRANTYPE$ = Pack$("01")
			TS.IO.STATE = 20
			Call printHeader
		Endif
		!
		Call printAuthLines
	Endif
	!
	If pType$ <> "5" Or Val(CB.CODE1$) > 0 Then Begin
		! Escribe user data únicamente cuando no se trata
		! de una consulta de saldo
		!
		!------------------------------------------------------------------------------------
		! 2021-02-08 V. 1.2 jsv
		!	Se agrega filtro para no escribir user data en caso de pagos o transferencias
		!------------------------------------------------------------------------------------
		!
		!Call writeCBUserData(pApplFunc$, pType$, tmpAddData$)
		!
		If pType$ <> "2" And pType$ <> "3" Then Begin
			Call writeCBUserData(pApplFunc$, pType$, tmpAddData$)
		Endif Else If pType$ = "2" And CB.trxPend% = 0 Then Begin
			Call writeCBUserData(pApplFunc$, pType$, tmpAddData$)
			!Call printAuthLines
		Endif
		!
		!------------------------------------------------------------------------------------
	Endif
	!
	Call printDebug(													\!
			"applyCBTrx pType$=" + pType$ + 			\!
			" pLocalAmt%=" + Str$(pLocalAmt%) + 	\!
			" CB.trxPend%=" + Str$(CB.trxPend%))
	If pType$ = "5" Then Begin
		! Para consulta de saldo, el valor siempre debe ser $1
		CB.AMOUNT$ = "1"
	Endif Else Begin
		! Operaciones diferentes a consulta de saldo
		If CB.trxPend% = -1 Then Begin
			CB.AMOUNT$ = Str$( pLocalAmt% )
		Endif Else Begin
			CB.AMOUNT$ = TEFII.FUNCT.GETAMT
			!-----------------------------------------------------------
			! 2022-08-25 jsv
			! ya no se debe quitar decimales, se envían montos enteros
			!-----------------------------------------------------------
			!If 																						\!
			!		Int%(Val(CB.AMOUNT$)) <> pLocalAmt% And 	\!
			!		Len(CB.AMOUNT$) > 2 And 									\!
			!		tef.getActualNet = 2 											\!
			!Then Begin
			!	! Le quita los dos decimales
			!	CB.AMOUNT$ = Left$(CB.AMOUNT$, Len(CB.AMOUNT$) - 2)
			!Endif
			!-----------------------------------------------------------
			!
		Endif
	Endif
	!
	Call printDebug("applyCBTrx.CB.AMOUNT$="+CB.AMOUNT$)
	!
	If pType$ = "1" Then Begin
		! Retiro
		! Debe iniciar secuencia de pago
		Call printDebug("Retiro exitoso")
		Call printDebug("tv=" + tmpTV$)
		Call printDebug("entity="+Str$(tmpEntity%))
		!
		TS.IO.KEYS(7)  = 90 + Val(Left$(tmpTV$, 1))
		TS.IO.KEYS(3)  = 78
		TS.IO.DATA$(3) = Mid$(tmpTV$, 2, 1)
		TS.IO.DATA$(9) = CB.AUTH.NUMBER$
		TS.IO.KEYS(9)  = 90
		TS.IO.DATA$(7) = CB.AMOUNT$
		TS.IO.MOTORKEY = TS.IO.KEYS(7)
		!
		Call reasignEntity(Int%(Val(tmpTV$)), tmpEntity%)
		!
		Call printDebug("TS.IO.KEYS(7)="+Str$(TS.IO.KEYS(7)))
		Call printDebug("TS.IO.KEYS(3)="+Str$(TS.IO.KEYS(3)))
		Call printDebug("TS.IO.DATA$(3)="+TS.IO.DATA$(3))
		Call printDebug("TS.IO.KEYS(9)="+Str$(TS.IO.KEYS(9)))
		Call printDebug("TS.IO.DATA$(7)="+TS.IO.DATA$(7))
	Endif Else If pType$ = "2" Then Begin
		! Pago
		! Debe registrar producto miscelaneo correspondiente
		! a la entrada de dinero
		Call disc.temporaryInactive
		Call printDebug("Pago o deposito al plu [" + pMiscData$ + "]")
		Call enterCodeForPlu(CB.CODE1$)
	Endif Else If pType$ = "3" Then Begin
		! Transferencia
		! Debe registrar dos productos miscelaneos 
		! uno para la entrada y otro para la salida de dinero
		!
		! Desactiva descuentos
		Call disc.temporaryInactive
		!
		! Desactiva límites de desembolsos
		TO.NEGCNT(7) = 9999
		TO.NEGAMT(7) = 99999999
		!
		Call printDebug("Transferencia entre plu [" + CB.CODE1$ + " y " + CB.CODE2$ + "]")
		Call enterCodeForPlu(CB.CODE1$)
	Endif Else If pType$ = "5" Then Begin
		! Consulta de saldo
		!----------------------------------------------------------------------------------
		! 2017-10-20
		! Se realiza cambio para que la consulta de saldo ingrese un PLU en vez de realizar
		! una transacción de entrenamiento, en caso de que el PLU sea diferente de vacío
		!----------------------------------------------------------------------------------
		!! Debe generar en forma automática una transacción de entrenamiento para que quede registrada
		!! la consulta de saldo en el log de transacciones
		!Call balanceQuery(pApplFunc$, pType$, tmpAddData$)
		!Call EP.DISPLAY.AN.ERROR("Consulta exitosa")
		!
		Call EP.DISPLAY.AN.ERROR("Consulta exitosa")
		If Val(CB.CODE1$) > 0 Then Begin
			Call enterCodeForPlu(CB.CODE1$)
		Endif Else Begin
			Call balanceQuery(pApplFunc$, pType$, tmpAddData$)
		Endif
		!----------------------------------------------------------------------------------
	Endif
	!
	!Una vez ingresado el item de pago, UNICAMENTE debe permitir el pago de la transacción, o la ANULACION del item
End Sub
!
Function buildMessage(pApplFunc$, pTime$, pStatus$, pInfo$)
	String buildMessage, pApplFunc$, pTime$, pStatus$, pInfo$, tmpMessage$, tmpLen$
	!
	tmpMessage$ = \
		Right$(String$(6, "0") + TS.TERMINAL$, 6) + 				\! Numero de terminal
		Right$(String$(6, "0") + Str$(SL.HD.TRANSNUM + 1), 6) + 	\! Numero de transaccion
		pTime$ + 													\! Fecha y hora
		pStatus$ + 													\! Estado
		pInfo$ 														 ! Informacion adicional
	!
	tmpLen$ = Right$(String$(3, "0") + Str$(Len(tmpMessage$)), 3)
	buildMessage = pApplFunc$ + tmpLen$ + tmpMessage$
End Function
!
Function buildInitialMessage(pApplFunc$, pTime$, pAmount$)
	String buildInitialMessage, pApplFunc$, pTime$, pAmount$
	!
	buildInitialMessage = buildMessage( 							\!
		pApplFunc$, 												\! Aplicacion + funcion
		pTime$, 													\! Fecha y hora
		"00", 														\! Estado inicial
		Right$(String$(12, "0") + pAmount$, 12) + 					\! Monto transaccion
		Right$(String$(12, "0") + Unpack$(TS.OPER$), 12) 			\! Cajero
	)
End Function
!
Sub datafCBTrx(pApplFunc$, pAmount$, pFuncNumber%, pAnswer%, pFullAnswer$)
	Integer*1 pFuncNumber%, pAnswer%, tmpExit%
	String pApplFunc$, pAmount$, pFullAnswer$, tmpMsg$, tmpResponse$, tmpStatus$, tmpTrxStatus$, tmpApplStatus$, tmpResponseCode$, tmpTime$, tmpResponseMsg$
	!
	tmpTime$ = DATE$ + TIME$
	tmpMsg$ = buildInitialMessage(pApplFunc$, tmpTime$, pAmount$)
	!
	tmpExit% = 0
	Call TEFII.FORMAT.setDataAppl("")
	tmpResponse$ = TEFII.FUNCT.TRX01$(tmpMsg$, pFuncNumber%, 0)
	While Not tmpExit%
		If Len(tmpResponse$) < 8 Then Begin
			tmpExit% = 2
		Endif Else Begin
			tmpStatus$ = Mid$(tmpResponse$, 8, 1)
			If tmpStatus$ = "0" Or tmpStatus$ = "1" Then Begin
				tmpExit% = 2
				Call TRANSLATE.APPL.CODE(tmpStatus$, tmpResponseMsg$)
				Call ep.display.an.error(tmpResponseMsg$)
			Endif Else Begin
				tmpTrxStatus$ = Mid$(tmpResponse$, 9, 1)
				If tmpTrxStatus$ <> "0" Then Begin
					tmpExit% = 2
					Call TRANSLATE.COMM.CODE(tmpTrxStatus$, tmpResponseMsg$)
					Call ep.display.an.error(tmpResponseMsg$)
				Endif Else Begin
					tmpApplStatus$ = Mid$(tmpResponse$, 10, 2)
					If tmpStatus$ = "2" Then Begin
						pFullAnswer$ = tmpResponse$
						tmpResponseCode$ = Mid$(tmpResponse$, 18, 2)
						If isApprovCode(tmpResponseCode$) Then \
							tmpExit% = -1 \
						Else \
							tmpExit% = 1
					Endif Else If tmpStatus$ = "3" Then Begin
						Call EP.DISPLAY.A.MESSAGE(Mid$(tmpResponse$, 12, 40))
					Endif
				Endif
			Endif
		Endif
		If Not tmpExit% Then Begin
			If tmpApplStatus$ = "99" Then Begin
				tmpExit% = 1
			Endif Else Begin
				tmpMsg$ = buildMessage(pApplFunc$, tmpTime$, tmpApplStatus$, "")
				Call TEFII.FORMAT.setDataAppl("")
				tmpResponse$ = TEFII.FUNCT.TRX01$(tmpMsg$, pFuncNumber%, 0)
			Endif
		Endif
	Wend
	pAnswer% = tmpExit%
End Sub
!
Sub parseDataIndex(pData$)
	String pData$
	Integer*2 tmpLen%, tmpCounter%
	!
	tmpLen% = Len(pData$)
	If tmpLen% = 0 Then Begin
		Dim CB.dataIndex%(0)
		CB.dataIndexSize% = 0
	Endif Else Begin
		CB.dataIndexSize% = (tmpLen% + 1) / 2 - 1
		Dim CB.dataIndex%( CB.dataIndexSize% )
		For tmpCounter% = 0 To CB.dataIndexSize%
			CB.dataIndex%( tmpCounter% ) = Int%( Val( Mid$(pData$, tmpCounter%*2 + 1, 2) ) )
		Next tmpCounter%
	Endif
End Sub
!
Function requestJavaForData(pRequest$)
	String requestJavaForData, pRequest$, tmpAppl$, tmpFunc$, tmpReqt$
	!
	If Len(pRequest$) < 5 Then Begin
		pRequest$ = Left$(pRequest$ + String$(5, "0"), 5)
	Endif
	tmpAppl$ = Mid$(pRequest$, 2, 2)
	tmpFunc$ = Mid$(pRequest$, 4, 2)
	tmpReqt$ = Right$(pRequest$, Len(pRequest$) - 5)
	requestJavaForData = EP.invocarApplManager0(tmpAppl$, tmpFunc$, tmpReqt$, 0, -1)
End Function
!
Sub asignDataFromJava(pFIndex%, pResponse$)
	String pResponse$, tmpField$
	Integer*2 pFIndex%, tmpCounter%, tmpIndex%
	!
	tmpIndex% = 0
	For tmpCounter% = 0 To CB.dataIndexSize%
		Call getNextField(pResponse$, tmpIndex%, ",", tmpField$)
		Call printDebug("assignConstant " + Str$(pFIndex%) + "," + Str$(CB.dataIndex%(tmpCounter%)) + "," + tmpField$)
		Call TEFII.FUNCT.assignConstant(pFIndex%, Str$(CB.dataIndex%(tmpCounter%)), tmpField$)
	Next tmpCounter%
End Sub
!
Sub requestUserData(pParam$, pFIndex%, pConstant%, pConfirm%)
	String pParam$, realParam$, tmpRequest$, tmpField$, tmpData$
	Integer*2 pFIndex%
	Integer*1 pConstant%, pConfirm%, tmpContinue%, tmpNoConstant%
	Integer*2 tmpIndex0%, tmpIndex1%, tmpIndex2%
	!
	Call printDebug("requestUserData(" + pParam$ + ",...)")
	!
	pConfirm% = -1
	!
	realParam$ = TEFII.FORMAT.RTRIM$(pParam$)
	If Len(realParam$) > 0 Then Begin
		tmpIndex0% = 0
		tmpContinue% = -1
		While tmpContinue%
			tmpNoConstant% = 0
			tmpIndex1% = Match(":", realParam$, tmpIndex0% + 1)
			If tmpIndex1% = 0 Then Begin
				tmpContinue% = 0
				tmpIndex1% = Len(realParam$) + 1
			Endif
			tmpRequest$ = Mid$(realParam$, tmpIndex0% + 1, tmpIndex1% - tmpIndex0% - 1)
			tmpIndex0% = Match("[", tmpRequest$, 1)
			If tmpIndex0% > 0 Then Begin
				tmpIndex2% = Match("]", tmpRequest$, tmpIndex0% + 1)
				If tmpIndex2% = 0 Then tmpIndex2% = Len(tmpRequest$) + 1
				!
				!--------------------------------------------------------------
				! 2021-04-20 jsv
				! El identificador de campo se reemplaza por un 
				! arreglo de enteros para permitir asignación múltiple
				! de campos. Por esta razón, ya no se usa la variable local
				! dataIndex%, sino el arreglo CB.dataIndex%
				!--------------------------------------------------------------
				!dataIndex% = Int%(Val(Mid$(tmpRequest$, tmpIndex0% + 1, tmpIndex2% - tmpIndex0% - 1)))
				!
				!--------------------------------------------------------------
				!
				tmpField$ = Mid$(tmpRequest$, tmpIndex0% + 1, tmpIndex2% - tmpIndex0% - 1)
				tmpRequest$ = Left$(tmpRequest$, tmpIndex0% - 1)
				!
				Call printDebug("Field:" + tmpField$ + " Value:" + tmpRequest$)
				!
				If Ucase$(Left$(tmpField$, 8)) = "GLOBVAR_" Then Begin
					! Especificación de un valor para asignárselo a una 
					! variable global que pueda ser usada por otra 
					! aplicación
					tmpField$ = Right$(tmpField$, Len(tmpField$) - 8)
					If Len(tmpField$) > 0 Then Begin
						Call asignarVariableGlobal(tmpField$, tmpRequest$)
					Endif
				Endif Else Begin
					Call parseDataIndex( tmpField$ )
					TS.IO.MOTORKEY = 0
					If Left$(tmpRequest$, 1) = "@" Then Begin
						! Invocación de clase Java para obtener los datos
						tmpData$ = requestJavaForData(tmpRequest$)
					Endif Else If pConstant% Then Begin
						tmpData$ = tmpRequest$
						If Left$(tmpData$, 1) = "#" Then Begin
							! Si el dato tiene como prefijo el signo #, quiere decir
							! que es un campo que viene de la transacción anterior
							tmpData$ = Right$(tmpData$, Len(tmpData$) - 1)
							Call printDebug("Extrayendo campo " + tmpData$ + " de trx anterior")
							tmpData$ = TEFII.FORMAT.GETFIELD$(Int%(Val(tmpData$)))
							Call printDebug("Valor = " + tmpData$)
						Endif
					Endif Else Begin
						If TS.PROCEDURE = 1 And TEFII.FUNCT.GETAMTINDEX = CB.dataIndex%(0) Then Begin
							tmpData$ = "1"
							tmpNoConstant% = -1
						Endif Else Begin
							tmpData$ = "0"
						Endif
						While tmpData$ = "0"
							Call EP.GET.KBDATA(tmpRequest$, "1", "99999999999999999999", tmpData$)
							If TS.IO.MOTORKEY = 73 Then tmpData$ = ""
							If tmpData$ = "0" Then Begin
								Call EP.DISPLAY.AN.ERROR("Debe entrar un valor")
							Endif
						Wend
					Endif
					If TS.IO.MOTORKEY = 73 Then Begin
						tmpContinue% = 0
						pConfirm% = 0
					Endif Else Begin
						If Left$(tmpRequest$, 1) = "@" Then Begin
							If Len(tmpData$) = 0 Then Begin
								tmpContinue% = 0
								pConfirm% = 0
							Endif Else Begin
								Call asignDataFromJava(pFIndex%, tmpData$)
							Endif
						Endif Else If tmpNoConstant% = 0 Then Begin
							!Call printDebug("assignConstant " + Str$(pFIndex%) + "," + Str$(dataIndex%) + "," + tmpData$)
							!Call TEFII.FUNCT.assignConstant(pFIndex%, Str$(dataIndex%), tmpData$)
							Call printDebug("assignConstant " + Str$(pFIndex%) + "," + Str$(CB.dataIndex%(0)) + "," + tmpData$)
							Call TEFII.FUNCT.assignConstant(pFIndex%, Str$(CB.dataIndex%(0)), tmpData$)
						Endif
					Endif
				Endif
			Endif
			tmpIndex0% = tmpIndex1%
		Wend
	Endif
End Sub
!
Sub parseMenuData(pFullData$, pApplFunc$, pType$, pDataInp$, pDataOut$, pMiscData$)
	String pFullData$, pApplFunc$, pType$, pDataInp$, pDataOut$, pMiscData$
	Integer*2 tmpIndex0%, tmpIndex1%
	!
	pApplFunc$ = ""
	pType$ = ""
	pDataInp$ = ""
	pDataOut$ = ""
	pMiscData$ = ""
	!
	pFullData$ = pFullData$ + ";"
	tmpIndex0% = 0
	tmpIndex1% = Match(";", pFullData$, tmpIndex0% + 1)
	If tmpIndex1% > 0 Then Begin
		pApplFunc$ = Mid$(pFullData$, tmpIndex0% + 1, tmpIndex1% - tmpIndex0% - 1)
		tmpIndex0% = tmpIndex1%
		tmpIndex1% = Match(";", pFullData$, tmpIndex0% + 1)
		If tmpIndex1% > 0 Then Begin
			pType$ = Mid$(pFullData$, tmpIndex0% + 1, tmpIndex1% - tmpIndex0% - 1)
			tmpIndex0% = tmpIndex1%
			tmpIndex1% = Match(";", pFullData$, tmpIndex0% + 1)
			If tmpIndex1% > 0 Then Begin
				pDataInp$ = Mid$(pFullData$, tmpIndex0% + 1, tmpIndex1% - tmpIndex0% - 1)
				tmpIndex0% = tmpIndex1%
				tmpIndex1% = Match(";", pFullData$, tmpIndex0% + 1)
				If tmpIndex1% > 0 Then Begin
					pDataOut$ = Mid$(pFullData$, tmpIndex0% + 1, tmpIndex1% - tmpIndex0% - 1)
					tmpIndex0% = tmpIndex1%
					tmpIndex1% = Match(";", pFullData$, tmpIndex0% + 1)
					If tmpIndex1% > 0 Then Begin
						pMiscData$ = TEFII.FORMAT.RTRIM$(Mid$(pFullData$, tmpIndex0% + 1, tmpIndex1% - tmpIndex0% - 1))
						!tmpIndex0% = tmpIndex1%
						!tmpIndex1% = Match(";", pFullData$, tmpIndex0% + 1)
					Endif
				Endif
			Endif
		Endif
	Endif
End Sub
!
Function displayMenu(pFilter$)
	String displayMenu, pFilter$, tmpMessage$
	!
	tmpMessage$ = \
		Right$(String$(3, "0") + Str$(tef.getActualNet), 3) + \
		pFilter$
	displayMenu = EP.invocarApplManager0("86", "00", tmpMessage$, 0, -1)
End Function
!
Function displayPagosMenu
	String displayPagosMenu
	!
	displayPagosMenu = displayMenu("023")
End Function
!
Function displayRetirosMenu
	String displayRetirosMenu
	!
	displayRetirosMenu = displayMenu("01")
End Function
!
Sub executeCBTrx(pFilter$, pApplFunc$, pType$, pMiscData$, pFIndex%, pAnswer%)
	Integer*1 pAnswer%
	String pFilter$, pApplFunc$, pType$, pMiscData$, tmpParam$
	String pDataInp$, pDataOut$
	Integer*2 pFIndex%, tmpReturn%
	!
	pAnswer% = 0
	pApplFunc$ = ""
	pType$ = "0"
	pMiscData$ = ""
	!
	tmpParam$ = displayMenu(pFilter$)
	If tmpParam$ <> "" Then Begin
		Call parseMenuData(tmpParam$, pApplFunc$, pType$, pDataInp$, pDataOut$, pMiscData$)
		If pType$ = "6" Then Begin
			! Secuencia de teclado especificada en pDataInp$
			If pDataOut$ <> "" Then Begin
				Call requestUserData(pDataOut$, pFIndex%, -1, pAnswer%)
			Endif
			Call postKeySequence(pDataInp$, tmpReturn%)
			If tmpReturn% Then Begin
				Call EP.DISPLAY.AN.ERROR("REVISE SECUENCIA    EN CBMENU")
			Endif
		Endif Else If pType$ > "0" And pType$ < "4" Or pType$ = "5" Then Begin
			! Si es una operación en la cual va a ingresar dinero a la caja
			! se debe verificar que la caja no esté pidiendo recogida
			If pType$ <> "1" And pType$ <> "5" And TO.CDLIMIT2 <= TE.TR.AMT(1) Then Begin
				Call EP.DISPLAY.AN.ERROR("DEBE HACER RECOGIDA ANTES DE ESTA TRX")
				pAnswer% = 0
			Endif Else Begin
				pFIndex% = TEFII.FUNCT.GETFINDEX(pApplFunc$)
				If pFIndex% = -1 Then Begin
					Call EP.DISPLAY.AN.ERROR(Left$("Funcion " + pApplFunc$ + String$(20, " "), 20) + "No definida")
					pAnswer% = 0
				Endif Else Begin
					Call TEFII.FUNCT.resetConstants(pFIndex%)
					If pDataInp$ <> "" Then Begin
						Call requestUserData(pDataInp$, pFIndex%, 0, pAnswer%)
					Endif Else Begin
						pAnswer% = -1
					Endif
					If pAnswer% Then Begin
						If pDataOut$ <> "" Then Begin
							Call requestUserData(pDataOut$, pFIndex%, -1, pAnswer%)
						Endif
					Endif
				Endif
			Endif
		Endif
	Endif
	!
End Sub
!
Sub executeSettlement
	Integer*2 tmpFIndex%
	Integer*1 tmpReturnCode%
	String tmpAnswer$
	!
	tmpFIndex% = TEFII.FUNCT.GETFINDEX("8599")
	If tmpFIndex% = -1 Then Begin
		Call EP.DISPLAY.AN.ERROR("Funcion de cierre   no habilitada")
	Endif Else Begin
		Call datafCBTrx("8599", "0", tmpFIndex%, tmpReturnCode%, tmpAnswer$)
		If tmpReturnCode% = -1 Then Begin
			Call EP.DISPLAY.AN.ERROR("Cierre realizado conexito en datafono")
		Endif Else Begin
			Call EP.DISPLAY.AN.ERROR("Error en cierre")
		Endif
	Endif
End Sub
!
Sub voidTransaction
	TO.VOIDLIMIT = 99999999
	Dim TS.IO.KEYS(10)
	Dim TS.IO.DATA$(10)
	TS.IO.KEYS(1) = 70
	TS.IO.KEYS(6) = 81
	TS.IO.MOTORKEY = 81
End Sub
!
Sub executePendingTrx(pReturn%, pMessage$)
	Integer*1 pReturn%
	String pMessage$, tmpFullAnswer$, tmpLastSequence$
	!
	pMessage$ = ""
	CB.trxPend% = 0
	Call datafCBTrx(CB.APPLFUNC$, CB.AMOUNT$, CB.FINDEX%, pReturn%, tmpFullAnswer$)
	Call printDebug("ret=" + Str$(pReturn%))
	If pReturn% <> -1 And pReturn% <> 2 Then Begin
		pMessage$ = "Operacion no        autorizada"
	Endif Else If pReturn% = 2 Then Begin
		! Se debe intentar una recuperación de transacción
		CB.FINDEX% = TEFII.FUNCT.GETFINDEX("8649")
		If CB.FINDEX% > 0 Then Begin
			If TEFII.FUNCT.ISSYNCBYDEMAND Then Begin
				Call EP.DISPLAY.AN.ERROR("DEBE RECUPERAR TRX     PRESIONE BORRAR")
			Endif
			Call saveLastSequence
			Call datafCBTrx("8649", CB.AMOUNT$, CB.FINDEX%, pReturn%, tmpFullAnswer$)
			If pReturn% <> -1 Then Begin
				pMessage$ = "Error en            recuperacion"
			Endif Else Begin
				tmpLastSequence$ = getLastSequence
				Call restoreLastSequence
				Call addLastSequence(tmpLastSequence$)
			Endif
		Endif
	Endif
	If pReturn% <> -1 Then Begin
		If pMessage$ = "" Then pMessage$ = "Operacion cancelada"
	Endif Else Begin
		Call parseCbAnswer(tmpFullAnswer$)
		!
		! Se debe grabar user data de la operación e imprimir leyenda
		! en journal y tirilla
		!Call writeCBUserData(CB.APPLFUNC$, CB.TYPE$, "")
		!Call printAuthLines
	Endif
End Sub
!
Sub executeConfirmTrx(pReturn%, pMessage$)
	Integer*1 pReturn%
	String pMessage$, tmpFullAnswer$, tmpLastSequence$, tmpPersist$
	!
	pMessage$ = ""
	tmpPersist$ = getPersistFields( CB.APPLFUNC1$ )
	Call TEFII.FUNCT.setFieldsToRestore(tmpPersist$)
	Call datafCBTrx(CB.APPLFUNC1$, CB.AMOUNT$, CB.FINDEX1%, pReturn%, tmpFullAnswer$)
	If pReturn% <> -1 And pReturn% <> 2 Then Begin
		pMessage$ = "Operacion no        autorizada"
	Endif Else If pReturn% = 2 Then Begin
		! Se debe intentar una recuperación de transacción
		CB.FINDEX% = TEFII.FUNCT.GETFINDEX("8649")
		If CB.FINDEX% > 0 Then Begin
			If TEFII.FUNCT.ISSYNCBYDEMAND Then Begin
				Call EP.DISPLAY.AN.ERROR("DEBE RECUPERAR TRX     PRESIONE BORRAR")
			Endif
			Call saveLastSequence
			Call datafCBTrx("8649", CB.AMOUNT$, CB.FINDEX%, pReturn%, tmpFullAnswer$)
			If pReturn% <> -1 Then Begin
				pMessage$ = "Error en            recuperacion"
			Endif Else Begin
				tmpLastSequence$ = getLastSequence
				Call restoreLastSequence
				Call addLastSequence(tmpLastSequence$)
			Endif
		Endif
	Endif
	If pReturn% <> -1 And pMessage$ = "" Then pMessage$ = "Operacion cancelada"
End Sub
!
!-----------------------------------------------------------------------------------------------
! USER EXITS
!-----------------------------------------------------------------------------------------------
!
Sub CB.02 Public
	Call resetVariables
	TO.VOIDLIMIT = CB.voidLimit%
End Sub
!
Sub CB.05 Public
	Call resetVariables
End Sub
!
Sub CB.07 Public
	Integer*2 tmpReturn%
	String tmpVersion$
	!
	tmpVersion$ = "Corresp. Bancarios V. 1.6	2022-08-24"
	!
	CB.voidLimit% = TO.VOIDLIMIT
	!
	Call resetVariables
	!
	CB.propData$ = ""
	Call readPropData("R::ADX_IDT1:CBPARM.DAT", CB.propData$, EP.IOPARM%, tmpReturn%)
	!
	CB.KEY% = Int%(Val(getProperty("CB.KEY", CB.propData$)))
	CB.ACTUALNET% = 0
	CB.UFIELDS$ = getUFields
	CB.UDKEY$ = getProperty("CB.USERDATA.KEY", CB.propData$)
	CB.ERRORKEYS$ = "," + getProperty("CB.ERRORKEYS", CB.propData$) + ","
	CB.AFAPPLS$ = getAFAppls
	!
	CB.negCnt7% = TO.NEGCNT(7)
	CB.negAmt7% = TO.NEGAMT(7)
	!
	CB.status% = 0
	!
	!Call EP.LINE.PRINT("Corresponsales Bancarios v. 1.1", 2100h)
	Call EP.LINE.PRINT(tmpVersion$, 2100h)
End Sub
!
Sub CB.08 Public
	String tmpItemname$
	!
	If CB.CODE1$ <> "" And \
			(Val(Unpack$(IR.ITEMCODE$)) = Val(CB.CODE1$) Or \
			 Val(Unpack$(IR.ITEMCODE$)) = Val(CB.CODE2$)) Then Begin
		!
		! Apaga el bit de precio requerido
		If (IR.INDICAT0 And 16) Then IR.INDICAT0 = IR.INDICAT0 XOR 16
		! Apaga el bit de peso/precio requerido
		If (IR.INDICAT0 And 64) Then IR.INDICAT0 = IR.INDICAT0 XOR 64
		! Apaga el bit de no a la venta
	    If (IR.INDICAT0 And 4) Then IR.INDICAT0 = IR.INDICAT0 XOR 4			
		!
		If CB.CODE2$ <> "" And Val(Unpack$(IR.ITEMCODE$)) = Val(CB.CODE1$) Then Begin
			IR.LINKEDTO$ = Pack$(Right$(String$(12, "0") + CB.CODE2$, 12))
		Endif
		!
		tmpItemname$ = getItemName( CB.APPLFUNC$ )
		If Len(tmpItemname$) > 0 Then Begin
			IR.ITEMNAME$ = tmpItemname$
		Endif
		CB.ITEMNAME$ = IR.ITEMNAME$
		!
		USR.exitFunction% = -1
	Endif
End Sub
!
Sub CB.14.0 public
	String tmpMessage$, tmpTenderSeq$
	!
	!------------------------------------------------------------------------------------
	! 2021-02-08 V. 1.2 jsv
	!	Se chequea si hay que anular la transacción
	! Se agrega validación para resetear variables en caso de que se detecte un ingreso 
	!	fallido de PLUs de corresponsales
	!------------------------------------------------------------------------------------
	!
	If CB.status% = -1 Then Begin
		Call voidTransaction
	Endif Else If CB.status% = -2 Then Begin
		CB.status% = -3
		Call totalizar
	Endif Else If CB.status% = -3 Then Begin
		CB.status% = 3
		CB.skipTendVal% = -1
		tmpTenderSeq$ = getTenderSeq( CB.APPLFUNC$ )
		If Len(tmpTenderSeq$) > 0 Then Begin
			tmpTenderSeq$ = Left$(tmpTenderSeq$ + String$(14, "0"), 14)
			tmpTenderSeq$ = String$(12, "X") + tmpTenderSeq$
		Endif
		Call launchAutoTefTrx( Int%( Val( CB.AMOUNT$ ) ), tmpTenderSeq$ )
	Endif Else If TS.INTRX = 0 And CB.CODE1$ <> "" Then Begin
		! Se ha presentado un error al intentar ingresar los PLUs 
		! de corresponsalía bancaria. Se hace reset de variables
		Call resetVariables
	Endif
	!
	!------------------------------------------------------------------------------------
	!
	tmpMessage$ = cb.errorParamKey(TS.IO.MOTORKEY)
	If tmpMessage$ <> "" Then Begin
		Call EP.DISPLAY.AN.ERROR(tmpMessage$)
		TS.IO.MOTORKEY = 0
		Dim TS.IO.DATA$(10)
		Dim TS.IO.KEYS(10)
	Endif
End Sub
!
Sub CB.14.1 Public
	Integer*1 tmpRequest%, tmpVoidTrx%, tmpReturn1%
	Integer*2 tmpFIndex%, tmpReturn%
	String tmpMiscData$, tmpApplFunc$, tmpType$, tmpAmount$, tmpMessage$, tmpFullAnswer$, tmpData2$
	String tmpSplit1$, tmpSplit2$, tmpSplit3$, tmpSplit4$, tmpConfirm$
	!
	tmpAmount$ = "0"
	tmpVoidTrx% = 0
	!
	tmpMessage$ = cb.errorParamKey(TS.IO.MOTORKEY)
	!
	If tmpMessage$ <> "" Then Begin
		Call EP.DISPLAY.AN.ERROR(tmpMessage$)
		TS.IO.MOTORKEY = 0
		Dim TS.IO.DATA$(10)
		Dim TS.IO.KEYS(10)	
	Endif Else If CB.KEY% > 0 And TS.IO.MOTORKEY = CB.KEY% Then Begin
		tmpData2$ = TS.IO.DATA$(2)
		If TS.PROCEDURE < 1 Then Begin
			! Pago o transferencia
			! Si ya hay productos ingresados no debe permitir esta transacción
			! a menos que se trate de una anulación
			! y que no se introdujera tarjeta plata
			If TPONLINE.getCustId <> "" Then Begin
				tmpRequest% = 1
				tmpMessage$ = "Operacion no valida para acumular"
			Endif Else If TS.IO.KEYS(1) = 70 Then Begin
				If TS.TOTALS(0, 0, 0) <> 0 And CB.CODE1$ <> "" Then Begin
					Call executeCBTrx("04", tmpApplFunc$, tmpType$, tmpMiscData$, tmpFIndex%, tmpRequest%)
				Endif Else Begin
					tmpRequest% = 1
					tmpMessage$ = "Operacion no valida"
				Endif
			Endif Else If TS.TOTALS(0, 0, 0) <> 0 Then Begin
				tmpRequest% = 1
				tmpMessage$ = "Operacion no valida"
			Endif Else Begin
				Call executeCBTrx("012356", tmpApplFunc$, tmpType$, tmpMiscData$, tmpFIndex%, tmpRequest%)
				If tmpType$ = "6" Then Begin
					Exit Sub
				Endif Else If tmpType$ = "1" Then Begin
					! Retiro seleccionado desde menú general de opciones
					tmpData2$ = TEFII.FUNCT.getConstant(tmpFIndex%, Str$(TEFII.FUNCT.GETAMTINDEX))
					If tmpData2$ = "" Then Begin
						tmpRequest% = 2
						tmpMessage$ = "Debe especificar    valor"
					Endif Else Begin
						tmpAmount$ = tmpData2$
					Endif
				Endif
			Endif
		Endif Else If TS.PROCEDURE = 1 Then Begin
			! Retiro
			If tmpData2$ = "" Then Begin
				tmpRequest% = 2
				tmpMessage$ = "Debe especificar    valor"
			Endif Else Begin
				tmpAmount$ = tmpData2$
				Call executeCBTrx("01", tmpApplFunc$, tmpType$, tmpMiscData$, tmpFIndex%, tmpRequest%)
			Endif
		Endif Else Begin
			tmpRequest% = 1
			tmpMessage$ = "Operacion no valida"
		Endif
		!
		TS.IO.MOTORKEY = 0
		Dim TS.IO.DATA$(10)
		Dim TS.IO.KEYS(10)
		!
		If tmpRequest% = -1 Then Begin
			!If tmpType$ <> "1" Then tmpAmount$ = TEFII.FORMAT.GETFIELD$(40)
			If tmpType$ <> "1" Then Begin
				tmpAmount$ = TEFII.FUNCT.getConstant(tmpFIndex%, Str$(TEFII.FUNCT.GETAMTINDEX))
			Endif
			Call printDebug("cb amount=" + tmpAmount$)
			!
			CB.CODE1$ = ""
			CB.CODE2$ = ""
			CB.TV$ = ""
			Call splitNumericalValues(tmpMiscData$, tmpSplit1$, tmpSplit2$, tmpSplit3$, tmpSplit4$)
			!
			CB.TYPE$ = tmpType$
			!
			Call printDebug("opType=" + tmpType$)
			!
			If tmpType$ = "1" Then Begin
				If tmpSplit1$ <> "" Then Begin
					CB.maxAmt% = Int%(Val(tmpSplit1$))
					If tmpSplit2$ <> "" Then Begin
						CB.TV$ = tmpSplit2$
					Endif
				Endif Else Begin
					CB.maxAmt% = 0
				Endif
			Endif Else If tmpType$ = "2" Then Begin
				CB.CODE1$ = tmpSplit1$
				If tmpSplit2$ <> "" Then Begin
					CB.maxAmt% = Int%(Val(tmpSplit2$))
				Endif Else Begin
					CB.maxAmt% = 0
				Endif
			Endif Else If tmpType$ = "3" Then Begin
				CB.CODE1$ = tmpSplit1$
				CB.CODE2$ = tmpSplit2$
				If tmpSplit3$ <> "" Then Begin
					CB.maxAmt% = Int%(Val(tmpSplit3$))
				Endif Else Begin
					CB.maxAmt% = 0
				Endif		
			Endif Else If tmpType$ = "5" Then Begin
				CB.CODE1$ = tmpSplit1$
				CB.maxAmt% = 0
			Endif
			!
			If CB.maxAmt% > 0 And Val(tmpAmount$) > CB.maxAmt% Then Begin
				tmpRequest% = 3
				tmpMessage$ = "Monto max permitido " + Str$(CB.maxAmt%)
			Endif Else Begin
				!------------------------------------------------------------------------------------
				! 2021-02-08 V. 1.2 jsv
				! Si la operación es de pago o de transferencia, únicamente se realiza cuando se 
				! hayan ingresado exitosamente los PLUs a la transacción
				!------------------------------------------------------------------------------------
				!
				!------------------------------------------------------------------------------------
				! 2022-06-28 jsv
				! Si el monto está en ceros, se asume que no se solicitó y que el datáfono debe 
				! obtenerlo. En ese caso, sí se debe realizar la operación en este punto
				!------------------------------------------------------------------------------------
				!
				!Call datafCBTrx(tmpApplFunc$, tmpAmount$, tmpFIndex%, tmpRequest%, tmpFullAnswer$)
				!
				CB.trxPend% = 0
				CB.APPLFUNC1$ = getConfirmFunction(tmpApplFunc$)
				CB.FINDEX1% = TEFII.FUNCT.GETFINDEX( CB.APPLFUNC1$ )
				If CB.FINDEX1% = -1 Then Begin
					CB.APPLFUNC1$ = ""
				Endif
				Call printDebug("CB.14.1a CB.APPLFUNC1$=" + CB.APPLFUNC1$ + " CB.FINDEX1%=" + Str$(CB.FINDEX1%))
				If 														\
						(tmpType$ = "2" And 			\
						 Val(tmpAmount$) > 0 And 	\
						 CB.APPLFUNC1$ = "") Or 	\
						tmpType$ = "3" 	\
				Then Begin
					CB.trxPend% = -1
					tmpRequest% = -1
					tmpFullAnswer$ = String$(200, "0")
					CB.APPLFUNC$ = tmpApplFunc$
					CB.FINDEX% = tmpFIndex%
				Endif Else Begin
					Call datafCBTrx(tmpApplFunc$, tmpAmount$, tmpFIndex%, tmpRequest%, tmpFullAnswer$)
				Endif
				!
				If tmpRequest% <> -1 And tmpRequest% <> 2 Then Begin
					tmpMessage$ = "Operacion no        autorizada"
				Endif Else If tmpRequest% = 2 Then Begin
					! Se debe intentar una recuperación de transacción
					tmpFIndex% = TEFII.FUNCT.GETFINDEX("8649")
					If tmpFIndex% > 0 Then Begin
						If TEFII.FUNCT.ISSYNCBYDEMAND Then Begin
							Call EP.DISPLAY.AN.ERROR("DEBE RECUPERAR TRX     PRESIONE BORRAR")
						Endif
						Call datafCBTrx("8649", tmpAmount$, tmpFIndex%, tmpRequest%, tmpFullAnswer$)
						If tmpRequest% <> -1 Then Begin
							tmpMessage$ = "Error en            recuperacion"
						Endif
					Endif
				Endif
			Endif
		Endif
		!
		If tmpRequest% <> -1 Then Begin
			If tmpMessage$ = "" Then tmpMessage$ = "Operacion cancelada"
			Call EP.DISPLAY.AN.ERROR(tmpMessage$)
			Call resetVariables
		Endif Else Begin
			!QQQ
			Call applyCBTrx(tmpApplFunc$, tmpType$, tmpFullAnswer$, tmpMiscData$, Int%(Val(tmpAmount$)))
		Endif
	Endif Else If EP.settlementKey% > 0 And TS.IO.MOTORKEY = EP.settlementKey% Then Begin
		If TS.PROCEDURE < 1 And TS.INTRX = 0 Then Begin
			If Mid$(TS.IO.HDR$, 11, 1) = "0"  Then Begin
				Call EP.DISPLAY.AN.ERROR("CIERRE REQUIERE     LLAVE DE SUPERVISOR")
			Endif Else Begin
				tmpConfirm$ = ""
				Call EP.GET.KBDATA("Confirmacion cierre Intro=Si  Borr=No", "0", "1", tmpConfirm$)
				If tmpConfirm$ <> "" Then Begin
					Call executeSettlement
				Endif
			Endif
		Endif Else Begin
			Call EP.DISPLAY.AN.ERROR("CIERRE NO PERMITIDO")
		Endif
		TS.IO.MOTORKEY = 0
		Dim TS.IO.DATA$(10)
		Dim TS.IO.KEYS(10)
	Endif Else If CB.CODE1$ <> "" And CB.neg% = 0 Then Begin
		! Si se está en una transacción con depósito, sólo se permite pagar o invocar la anulación
		If 																\
				(TS.IO.KEYS(1) <> 70 Or TS.IO.KEYS(6) <> 81) And 		\! No es Anulado total
				(TS.IO.KEYS(1) <> 0 Or TS.IO.KEYS(6) <> 81) And 		\! No es total
				(TS.IO.MOTORKEY <> 73) And 								\! No es clear
				(TS.IO.KEYS(10) <> 80) And 								\! No es ingreso de numero de cuenta
				(TS.IO.KEYS(5) <> 61) And 								\! No es conex/disconex
				(TS.IO.KEYS(7) < 91 Or TS.IO.KEYS(7) > 99) Then Begin 	 ! No es Ingreso de pago
			Call EP.DISPLAY.AN.ERROR("SOLO SE PERMITE     PAGAR")
			TS.IO.MOTORKEY = 0
			Dim TS.IO.DATA$(10)
			Dim TS.IO.KEYS(10)			
		Endif Else If TS.IO.KEYS(7) >= 91 And TS.IO.KEYS(7) <= 99 Then Begin
			! Secuencia de medio de pago: Sólo se permite efectivo, y que sea por el total
			Call tenderValidation(tmpReturn%, tmpMessage$)
			Call printDebug("CB.14.1b CB.APPLFUNC1$=" + CB.APPLFUNC1$ + " CB.FINDEX1%=" + Str$(CB.FINDEX1%) + " CB.trxPend%=" + Str$(CB.trxPend%))
			If tmpReturn% = -1 Then Begin
				tmpReturn1% = -1
				If CB.APPLFUNC1$ <> "" Then Begin
					Call executeConfirmTrx(tmpReturn1%, tmpMessage$)
				Endif Else If CB.trxPend% = -1 Then Begin
					tmpReturn1% = 0
					Call printDebug("invoking executePendingTrx")
					Call executePendingTrx(tmpReturn1%, tmpMessage$)
					Call printDebug("ret=" + Str$(tmpReturn1%) + " msg=" + tmpMessage$)
				Endif
				If tmpReturn1% <> -1 Then Begin
					tmpVoidTrx% = -1
					tmpReturn% = Int%(tmpReturn1%)
					Call printDebug("final ret=" + Str$(tmpReturn%))
				Endif Else Begin
					Call writeCBUserData(CB.APPLFUNC$, CB.TYPE$, "")
					Call printAuthLines
				Endif
			Endif
			If tmpReturn% <> -1 Then Begin
				Call EP.DISPLAY.AN.ERROR(tmpMessage$)
				TS.IO.MOTORKEY = 0
				Dim TS.IO.DATA$(10)
				Dim TS.IO.KEYS(10)
				!
				If tmpVoidTrx% Then Begin
					Call resetVariables
					CB.status% = -1 ! Estado que obliga a la anulación total de la transacción
				Endif
			Endif
		Endif
	Endif Else If CB.CODE1$ <> "" And CB.neg% = -1 And TS.IO.MOTORKEY = 73 And TS.IO.KEYS(1) = 73 Then Begin
		TS.IO.MOTORKEY = 0
		Dim TS.IO.DATA$(10)
		Dim TS.IO.KEYS(10)
	Endif
End Sub
!
!------------------------------------------------------------------------------------
! 2021-02-08 V. 1.2 jsv
! Ya no se va a usar la referencia de línea de artículo para identificar un pago
!------------------------------------------------------------------------------------
!Sub CB.20 Public
!	If EP.IS.ITEM.LINE.20 And CB.CODE1$ <> "" Then Begin
!			CB.lineaArticulo$ = TS.PRTBUF$
!	Endif
!End Sub
!
!------------------------------------------------------------------------------------
!
Sub CB.23 Public
	If CB.status% = -1 Then Begin
		TS.DISP1$ = "Presione cualquier  "
		TS.DISP2$ = "tecla para anular   "
	Endif Else If CB.status% = -2 Then Begin
		TS.DISP1$ = " OPERACION EXITOSA  "
		TS.DISP2$ = "  PRESIONE BORRAR   "
	Endif Else If CB.status% = -3 Then Begin
		TS.DISP1$ = "Presione cualquier  "
		TS.DISP2$ = "tecla para finalizar"
	Endif
End Sub
!
Sub CB.30 Public
	If CB.status% = 1 Then Begin
		TS.TRX.STATUS = 1
		TS.BAL.TAKEN = -1
		!
		! Restaura los límites de desembolsos
		TO.NEGCNT(7) = CB.negCnt7%
		TO.NEGAMT(7) = CB.negAmt7%
		!
		CB.status% = 2
		If CB.TYPE$ = "5" Then Begin
			Call processTender("11", Int%(Val(CB.AMOUNT$)), "")
		Endif Else Begin
			Call processTender("11", 0, "")
		Endif
	Endif
End Sub
!
Sub CB.43 Public
	If CB.CODE1$ <> "" Then Begin
		If Val(Unpack$(IR.ITEMCODE$)) = Val(CB.CODE1$) Or Val(Unpack$(IR.ITEMCODE$)) = Val(CB.CODE2$) Then Begin
			Call printDebug("UE43.CB.AMOUNT$="+CB.AMOUNT$)
			SL.IT.XPRICE = Int%(Val(CB.AMOUNT$))
		Endif
	Endif
End Sub
!
Sub CB.60 Public
	If TS.LINETYPE = 7 AND TS.LINEDATA = 1 And CB.status% = 1 Then Begin
		TS.TRX.STATUS = 1
		TS.BAL.TAKEN = -1
		CB.status% = 2
		!
		! Restaura los límites de desembolsos
		TO.NEGCNT(7) = CB.negCnt7%
		TO.NEGAMT(7) = CB.negAmt7%
		!
		If CB.TYPE$ = "5" Then Begin
			Call processTender("11", Int%(Val(CB.AMOUNT$)), "")
		Endif Else Begin
			Call processTender("11", 0, "")
		Endif
	Endif
	!
	!------------------------------------------------------------------------------------
	! 2021-02-08 V. 1.2 jsv
	! La impresión de la leyenda en tirilla y journal para pagos se pasa para la user 
	! exit 67, cuando se confirme el ingreso de los PLUs en la transacción
	!------------------------------------------------------------------------------------
	!
	!Endif Else If isItemLine60 Then Begin
	!	If 			\
	!			(CB.CODE2$ <> "" And Val(Unpack$(IR.ITEMCODE$)) = Val(CB.CODE2$)) Or \
	!			(CB.CODE2$ = "" And CB.CODE1$ <> "" And Val(Unpack$(IR.ITEMCODE$)) = Val(CB.CODE1$)) Then Begin
	!		! Escribe leyenda en tirilla y en journal
	!		Call EP.LINE.PRINT("Nro. Autoriz: " + CB.AUTH.NUMBER$, 6100H)
	!		Call EP.LINE.PRINT("Nro. Recibo : " + CB.RECIBO$, 6100H)
	!	Endif
	!Endif
	!
	!------------------------------------------------------------------------------------
End Sub
!
Sub CB.67 Public
	Integer*1 tmpRequest%, tmpAF%
	String tmpFullAnswer$, tmpMessage$, tmpLastSequence$, tmpNewAmount$
	Integer*4 tmpNewAmount%
	!
	!------------------------------------------------------------------------------------
	! 2021-02-08 V. 1.2 jsv
	!	Cuando el PLU ingresado corresponde a un depósito o al 2do ítem de una 
	!	transferencia, se hace la operación contra el datáfono. En caso de que ésta falle 
	!	o sea rechazada, se debe anular la transacción
	!------------------------------------------------------------------------------------
	!
	If Len(CB.APPLFUNC$) > 0 Then Begin
		tmpAF% = isAutoFinishAppl( CB.APPLFUNC$ )
	Endif Else Begin
		tmpAF% = 0
	Endif
	!
	If 																								\!
			((CB.TYPE$ = "2" And tmpAF% <> 0) Or 					\!
			(CB.TYPE$ = "3" And 													\! Transferencia
			 Val(SL.IT.ITEMCODE$) = Val(CB.CODE2$))) And 	\! 2do ítem de la transferencia
			CB.trxPend% = -1 															\! Operación pendiente contra el datáfono
	Then Begin
		tmpMessage$ = ""
		CB.trxPend% = 0
		Call datafCBTrx(CB.APPLFUNC$, CB.AMOUNT$, CB.FINDEX%, tmpRequest%, tmpFullAnswer$)
		If tmpRequest% <> -1 And tmpRequest% <> 2 Then Begin
			tmpMessage$ = "Operacion no        autorizada"
		Endif Else If tmpRequest% = 2 Then Begin
			! Se debe intentar una recuperación de transacción
			CB.FINDEX% = TEFII.FUNCT.GETFINDEX("8649")
			If CB.FINDEX% > 0 Then Begin
				If TEFII.FUNCT.ISSYNCBYDEMAND Then Begin
					Call EP.DISPLAY.AN.ERROR("DEBE RECUPERAR TRX     PRESIONE BORRAR")
				Endif
				Call saveLastSequence
				Call datafCBTrx("8649", CB.AMOUNT$, CB.FINDEX%, tmpRequest%, tmpFullAnswer$)
				If tmpRequest% <> -1 Then Begin
					tmpMessage$ = "Error en            recuperacion"
				Endif Else Begin
					tmpLastSequence$ = getLastSequence
					Call restoreLastSequence
					Call addLastSequence(tmpLastSequence$)
				Endif
			Endif
		Endif
		If tmpRequest% <> -1 Then Begin
			If tmpMessage$ = "" Then tmpMessage$ = "Operacion cancelada"
			Call EP.DISPLAY.AN.ERROR(tmpMessage$)
			Call resetVariables
			CB.status% = -1 ! Estado que obliga a la anulación total de la transacción
		Endif Else Begin
			Call parseCbAnswer(tmpFullAnswer$)
			!
			! Se debe grabar user data de la operación e imprimir leyenda
			! en journal y tirilla
			Call writeCBUserData(CB.APPLFUNC$, CB.TYPE$, "")
			Call printAuthLines
			!
			If CB.TYPE$ = "2" And TS.XPRICE > 0 Then Begin
				!tmpNewAmount% = Int%( Val( TEFII.FUNCT.getConstant(tmpFIndex%, Str$(TEFII.FUNCT.GETAMTINDEX)) ))
				tmpNewAmount$ = TEFII.FUNCT.GETAMT
				!-----------------------------------------------------------
				! 2022-08-25 jsv
				! ya no se debe quitar decimales, se envían montos enteros
				!-----------------------------------------------------------
				!If tef.getActualNet = 2 Then Begin
				!	! Le quita los dos decimales
				!	tmpNewAmount$ = Left$(tmpNewAmount$, Len(tmpNewAmount$) - 2)
				!Endif
				!-----------------------------------------------------------
				!
				tmpNewAmount% = Int%( Val( tmpNewAmount$ ) )
				If tmpNewAmount% > 0 Then Begin
					If tmpNewAmount% <> TS.XPRICE Then Begin
						Call autoPrChg.changePrice(CB.CODE1$, TS.XPRICE, tmpNewAmount%)
					Endif Else If tmpAF% Then Begin
						CB.status% = -2
						!Call totalizar
						Call TSHIECET
					Endif
				Endif Else Begin
					Call EP.DISPLAY.AN.ERROR("MONTO INVALIDO TRX  " + Str$(tmpNewAmount%))
					Call resetVariables
					CB.status% = -1 ! Estado que obliga a la anulación total de la transacción
				Endif
			Endif
		Endif
	Endif
	!
	!------------------------------------------------------------------------------------
	!
	! Si es una transferencia o una anulación
	! debe seguir con la secuencia de 0 + Efectivo
	! para terminar la transacción
	If CB.CODE2$ <> "" Or (CB.CODE1$ <> "" And TS.XPRICE < 0) Or CB.TYPE$ = "5" Then Begin
		CB.status% = 1
		!Call CB.autoFinish
		Call totalizar
	Endif
End Sub
!
