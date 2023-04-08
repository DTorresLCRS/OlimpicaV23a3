!*********************************************************************
!  (APISCAP.BAS)
!  SCAP Basic para 4690
!  (Ver 1.00)
!  Author : Javier Sarmiento Valdivieso
!           La Cadena Retail Solutions Ltda.
!
!
%ENVIRON T
!
! Supermarket application global data !
!
%INCLUDE EAMTSWKG.J86
!
%INCLUDE EAMTOPTS.J86
!
%INCLUDE EAMTRANS.J86
!
%INCLUDE EAMITEMR.J86
!
INTEGER GLOBAL FAST.EXIT
!
! User application global data
%INCLUDE C:\APPL\APIVARI.J86
integer*2 GLOBAL			\!
	PRT4610.ENABLE			!! IBM 4610 Printer enable flag
String Global ue.clf.cliente$
Integer*4 Global MEX.VAT.TOTAL(1), NFF.EXCLU%
!
String temporalJavaParam$(1)
!
String DISC.dummyAccount$
!
Integer*1 tmpLayoutIndicator%
!
! Variables para descuento directo a transacción
! que requiere confirmación (día sin iva)
String DISC.confirmMessage$, DISC.confirmAllowedTenders$, DISC.confirmExcludedTenders$, DISC.confirmRejectTenderMessage$
String DISC.confirmRequestUdKey$, DISC.confirmItemUdKey$
String DISC.confirmList$
Integer*1 DISC.confirm%, DISC.confirmChange%
Integer*4 DISC.totalDiscount%, DISC.confirmVrTotal%
!
! Variables adicionales para día sin iva 2021
String DISC.confirmActivIn$, DISC.confirmActivEx$, DISC.confirmRemissionTxt$, DISC.optionalLines$(1)
Integer*4 DISC.headerUserFieldBit%
Integer*2 DISC.optionalLineCnt%
Integer*1 DISC.isConfirmDiscActive%, DISC.recoverDiscounts%
!
! Variables para modalidad de contingencia fiscal
Integer*1 CONTING.active% 			      !
!
! Variables para facturación electrónica
Integer*1 FE.active%             ! -1=Sin asignación   0=No   1=Sí(Remisión)   2=Operaciones terceros   3=Órdenes de crédito
Integer*1 FE.userRequest%        ! Flag para indicar si se pregunta al usuario por facturación electrónica
Integer*1 FE.forceVoid%          ! Flag para indicar que se está forzando una anulación automática
Integer*4 FE.minAmount%          ! Monto mínimo a partir del cual es obligatoria la facturación electrónica
Integer*1 FE.exPlu%              ! Flag para indicar que se registró un producto que no aplica para FE
String FE.dummyItem$             ! Código de artículo dummy usado para anulación automática
String FE.requestMessage$        ! Mensaje que se despliega para preguntar por facturación electrónica
String FE.exPlus$                ! Lista de PLUs para los cuales no se debe desplegar mensaje de facturación electrónica
String FE.forceVoidMsg$          ! Mensaje que se despliega para iniciar anulación automática
String FE.requestDeKey$          ! Clave de data entry para almacenar la respuesta del cliente a la pregunta sobre fe
Integer*1 FE.skipEquivDoc%       ! Flag para indicar si se inhibe el llamado al módulo fiscal para transacciones marcadas como FE
String FE.paramContent$          ! Contenido del archivo de parámetros SCAP.PRO para consulta de parámetros varios
Integer*1 FE.preference%         ! Preferencia de cliente en cuanto a facturación electrónica
String Global FE.emailWarning$   ! Advertencia cuando el cliente no tiene email registrado
!
String CONTING.fDataUdKey$
String Global 		\
	FISNIVRT$,			\
	FISNFVRT$,			\
	FISNRA$,				\
	FISFRA$
!
Integer*2 localDisplayArray%(1)
String localDisplayArray$(1)
!
! Variables para emular numeración fiscal en el día sin Iva
String 						\
	DISC.NFF.NUT$,	\
	DISC.NFF.PF1$,	\
	DISC.NFF.TRLOG$
String Global 		\
	NFF.SAVPRT$,		\
	NFF.AUX$,				\
	NFF.NET$,				\
	NFF.IT1$,				\
	NFF.IT2$,				\
	NFF.IT3$,				\
	NFF.IT4$,				\
	NFF.IT5$,				\
	NFF.IT6$,				\
	NFF.IT7$,				\
	NFF.IT8$,				\
	NFF.LINF$,			\
	UE.RFO4$
Integer*1 Global 	\
	NFF.SOLODV%
!
!
! Variables para cupones personalizados
String DISC.customerCouponPref$, DISC.customerCouponUDKey1$, DISC.customerCouponUDKey2$, DISC.customerCouponMessage$
String DISC.customerCouponRecovery$(2)
Integer*2 DISC.customerCouponLeng%, DISC.customerCouponPrefLen%
Integer*2 DISC.customerCouponRecoveryCnt%
!
! Variables para cambio de puntos por pesos
Integer*4 PPP.points%, PPP.maxPoints%, PPP.totalBeforeDisc%, PPP.otpRetries%
String PPP.errorMsg$
String PPP.headerUDKey$, PPP.detailUDKey$, PPP.voidUDKey$
Integer*1 PPP.inRecovery%, PPP.suspending%
!
! Número de factura
!String Global NFF.NUT$
!
String global				 \!
	TE.TR.OPERATOR$			,\!
	gv.tarjetaCliente$		,\!
	gv.idCliente$			,\!
	gv.segmentoCliente$		,\!
	gv.nombreCliente$
! 
Integer*4 global			 \!
	gv.acumPuntosTotal%		,\!
	gv.acumPuntosPeriodo%	,\!
	gv.acumPuntosTrx%		,\!
	gv.bonifPuntosTrx%		,\!
	gv.acumPuntosRedimTotal%,\!
	gv.acumPuntosRedimTrx%	,\!
	gv.acumPuntosRedimPeriodo%

String parse.promocion$,parse.regla$,parse.beneficiario$,parse.descripcionBeneficio$,\
	parse.tipoBeneficio$,parse.proveedor$,parse.grupoDescuento$,	\
	parse.tarifaIva$,parse.dpto$,parse.segmento$,parse.tipoRegla$,parse.strPeso$,parse.strIndiceIva$, parse.layoutInd$
Integer*4 parse.valorDescuento%,parse.baseDescuento%, parse.Und%
!
String scap.Bonos$(1)
!
! Descuentos por tarifa iva
Integer*4 GLOBAL gv.descto.iva%(2), totalDesctoPago%
!
! Montos x medio de pago e institucion
! montosMP%(3)
! Indice 1= Tipo	Indice 2= Variedad	Indice 3= Indice de la Institucion
! institucionesMP%(1)
! Indice 1= Valor de la institucion donde la posición cero corresponde a ninguna
Integer*4 montosMP%(3)
Integer*2 institucionesMP%(1),tmpInstitucion%,trxSign%
Integer*1 institutCounter%,institutMax%,terminalStartUp%
!
Integer*1 waitingForTender%, voidTender%
!--------------------------------------------------------------
! User application private data
String \
	EP.applMgrAnswer$, EP.lineaArticulo$, EP.datosArticulo$(1), \
	EP.idCliente$, EP.cedulaCliente$, EP.scapPlu$, EP.prevTrailer1$, EP.prevTrailer2$,\
	EP.scapDE.key$,EP.scapDcto.function$,EP.scapCupon.function$,EP.scapBono.function$,\
	EP.scapDiscountLine$(1),EP.sepOtherData$
Integer*2 EP.scapPendientes%,EP.scapTaxIndex%
Integer*1 EP.scapArticuloNotificado%,EP.scapTrx%,EP.scapRecupInProgress%,EP.scapBonosCorte%,EP.datosClienteImpresion%
! User application global data
Integer*1 global EP.datosClienteScap%
!
! Condiciones comprometidas para obtener un descuento por medio de pago
String EP.scapMedioPagoDMP$,EP.scapInstitucionDMP$,EP.scapDataDMP$,EP.scapAcctDMP$
Integer*4 EP.scapValorPagoDMP%, EP.scapValorDctoDMP%, EP.scapValorTeoricoDMP%
Integer*1 EP.scapPendingDMP%, EP.scapIgnoreAccount%, EP.scapExternalInvoke%
! Variables equivalentes para el caso de anulación
String EP.scapMedioPagoAnulDMP$,EP.scapInstitucionAnulDMP$,EP.scapAcctAnulDMP$
Integer*4 EP.scapValorPagoAnulDMP%, EP.scapValorDctoAnulDMP%, EP.scapValorTeoricoAnulDMP%
! Acumulado dctos por medio de pago y descuentos transaccion en modo recuperacion
Integer*4 EP.acumDctosMP%, EP.acumDctosTrx%
!
! Variables a respaldar por scap en un momento dado
Integer*2 scapBakInt2%(1)
String scapBakStr$(1)
!
Integer*2 Global TS.SV.LINETYPE
!
! Variable para inactivar temporalmente el módulo de promociones, durante una transacción determinada
Integer*1 EP.scap.temporaryInactive%
!
! Variables de ITEMEXTE
String Global 				\
	ITEMEXTE.CODE$, 		\
	ITEMEXTE.KEY$, 			\
	ITEMEXTE.LONGDESCRIP$, 	\
	ITEMEXTE.MU$, 			\
	ITEMEXTE.PROVIDER$, 	\
	ITEMEXTE.DATE$, 		\
	ITEMEXTE.LEVELS$, 		\
	ITEMEXTE.LOCATION$, 	\
	ITEMEXTE.TRADEMARK$,	\
	ITEMEXTE.USERFIELD$
Integer*4 Global 			\
	ITEMEXTE.CONTENT%
Integer*1 Global 			\
	ITEMEXTE.FOUND%, 		\
	ITEMEXTE.LABELS%, 		\
	ITEMEXTE.USERFLAG%
!
!---------------------------------------------------------------
!  Standard supermarket routines
!---------------------------------------------------------------
Sub TSBDEC01 External
End Sub
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
! write entry to summary log array
FUNCTION WRITE.SL.STR(INDEX, SOURCE$) EXTERNAL
	INTEGER*2 WRITE.SL.STR
	INTEGER*2 INDEX
	STRING SOURCE$
END FUNCTION
!
!****************************************************************************!
!* FORMAT.AMOUNT:   Converts an Integer*4 to a string and edits into        *!
!* a single string TS.TEMP1$  No local error handling in the subroutine.    *!
!****************************************************************************!
FUNCTION FORMAT.AMOUNT(EP.INT4) EXTERNAL 
	INTEGER*1 FORMAT.AMOUNT
	INTEGER*4 EP.INT4 
END FUNCTION
!
!----------------------------------------------------------------
! User External Routines
!----------------------------------------------------------------
!
Sub appl.enableMessageDisplay(pMsg1$, pMsg2$) External
	String pMsg1$, pMsg2$
End Sub
!
Sub appl.disableMessageDisplay External
End Sub
!
! Rutinas para uso en la emulación de numeración fiscal
Sub SESION.34 External
End Sub
!
Sub SESION.36 External
End Sub
!
Function isIncluded(pValue%, pIncludedRange$, pExcludedRange$) External
	Integer*1 isIncluded
	Real pValue%
	String pIncludedRange$, pExcludedRange$
End Function
!
Function TPONLINE.getCedula External
	String TPONLINE.getCedula
End Function
!
Sub readPropData(pFilename$, pPropData$, pSession%, pReturn%) External
	String pFilename$, pPropData$
	Integer*2 pSession%, pReturn%
End Sub

Function getProperty(pProperty$, pData$) External
	String getProperty, pProperty$, pData$
End Function

Function getBooleanProperty(pProperty$, pData$) External
	Integer*1 getBooleanProperty
	String pProperty$, pData$
End Function

Function getIntProperty(pProperty$, pData$, pMinValue%, pMaxValue%) External
	Integer*4 getIntProperty, pMinValue%, pMaxValue%
	String pProperty$, pData$
End Function

Sub asignarVariableGlobal(pName$, pValue$) External
	String pName$, pValue$
End Sub

Function consultarVariableGlobal(pName$) External
	String consultarVariableGlobal, pName$
End Function

Function EP.invocarApplManager0(epAppl$, epFunction$, parteVariable$, usaConsecutivo%, pNoMsg%) External
	String EP.invocarApplManager0, epAppl$, epFunction$, parteVariable$
	Integer*1 usaConsecutivo%, pNoMsg%
End Function

Sub getNextField(pData$, pLastIndex%, pSeparator$, pNextField$) External
	String pNextField$, pData$, pSeparator$
	Integer*2 pLastIndex%
End Sub

Function ITEMADDI.getGenero External
	String ITEMADDI.getGenero
End Function
!
Function ITEMADDI.getFiller External
	String ITEMADDI.getFiller
End Function

Sub EP.SAVE.KEYS External
End Sub
!
Sub EP.RESTORE.KEYS External
End Sub
!
Function EP.BUSCA.INDICE.TV(UE.TIPO.VAR$) External
	Integer*2 EP.BUSCA.INDICE.TV
	String UE.TIPO.VAR$
End Function
!
SUB EP.LINE.PRINT(LINEA$,ESTACION%) EXTERNAL
	STRING LINEA$
	INTEGER*2 ESTACION%
END SUB
!
SUB EP.ADD.DATA.ENTRY99(UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6) EXTERNAL
	STRING UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6
END SUB
!
Sub EP.ADD.DATA.ENTRY(UE.DATA1, UE.DATA2, UE.DATA3, UE.DATA4, UE.DATA5, UE.DATA6) External
	String UE.DATA1, UE.DATA2, UE.DATA3, UE.DATA4, UE.DATA5, UE.DATA6
End Sub
!
Function isApplMgrRunning External
	Integer*1 isApplMgrRunning
End Function
!
FUNCTION applManagerFunction(msgToSend$,resetVoucher%) EXTERNAL
String applManagerFunction,msgToSend$
	Integer*1 resetVoucher%
END FUNCTION
!
Function replaceFields(lineDesc$) External
	String lineDesc$,replaceFields
End Function
!
FUNCTION EP.NEW.TRANSNUM  EXTERNAL
  STRING EP.NEW.TRANSNUM, UE.WORK.TRANSNUM$
END FUNCTION
!
SUB EP.DISPLAY.AN.ERROR(UE.DISP.MESSAGE$) EXTERNAL 
  STRING UE.DISP.MESSAGE$, UE.WORK$
END SUB
!
SUB EP.DISPLAY.A.MESSAGE(UE.DISP.MESSAGE$) EXTERNAL 
  STRING UE.DISP.MESSAGE$, UE.WORK$
END SUB
!
Function lRemove$(line$,charCode%) External
    String lRemove$,line$
    Integer*1 charCode%
End Function
!
Function rRemove$(line$,charCode%) External
	String rRemove$,line$
	Integer*1 charCode%
End Function
!
!-----------------------------------------------------------------------------
! Retorna un Integer*2 con la institución asociada al medio de pago actual
! en caso de que aplique. En caso de que no aplique, retorna cero
!-----------------------------------------------------------------------------
Function getInstitucion External
	Integer*2 getInstitucion
End Function
!
Sub EP.GET.KBDATA(UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, UE.KEYB.DATA$) External
	String UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, UE.KEYB.DATA$
End Sub
!
Sub printDebug(pMsg$) External
	String pMsg$
End Sub
!
Function parseError External
	String parseError
End Function
!
!------------------------------------------------------------
! 2020-04-06 jsv
! Rutina para validar medio de pago por los artículos 
! ingresados en la transacción
!------------------------------------------------------------
Sub TENDSALE.validateTender(pTendType%, pTendVar%, pEntity%, pBin$, pReturn%, pMessage$) External
	String pBin$, pMessage$
	Integer*2 pTendType%, pTendVar%, pEntity%, pReturn%
End Sub
!
!------------------------------------------------------------------------------------
!   Java API routines
!------------------------------------------------------------------------------------
Sub JavaCall.Initialize.Request(ClassName,MethodName,TheRequest) External
	String ClassName, MethodName, TheRequest
End Sub

Sub JavaCall.AddParameter.String(TheRequest,TheParameter) External
	String TheRequest, TheParameter
End Sub

Sub JavaCall.InvokeMethod.ReturnString(TheRequest,ReturnValue, Exception) External
	String TheRequest, ReturnValue, Exception
End Sub
!
!------------------------------------------------------------
!
!----------------------------------------------------------------
! User Routines
!----------------------------------------------------------------
!
Sub setScapRecoveryState(pValue%)
	Integer*1 pValue%
	!
	EP.scapRecupInProgress% = pValue%
	If pValue% Then Begin
		Call asignarVariableGlobal("scapRecupInProgress", "true" )
	Endif Else Begin
		Call asignarVariableGlobal("scapRecupInProgress", "false" )
	Endif
End Sub
!
Sub saveLocalDisplay
	Dim localDisplayArray%(7)
	localDisplayArray%(1) = ts.linetype
	localDisplayArray%(2) = ts.linedata
	localDisplayArray%(3) = ts.linedata2
	localDisplayArray%(4) = ts.linedata3
	localDisplayArray%(5) = ts.xxmod
	localDisplayArray%(6) = ts.yymod
	localDisplayArray%(7) = ts.ds.nosave
	!
	Dim localDisplayArray$(2)
	localDisplayArray$(1) = ts.savdisp1$
	localDisplayArray$(2) = ts.savdisp2$
End Sub
!
Sub restoreLocalDisplay
	ts.temp1$   = localDisplayArray$(1)
	ts.temp2$   = localDisplayArray$(2)
	ts.linetype = 12
	call tsdsec01
	!
	ts.linetype  = localDisplayArray%(1)
	ts.linedata  = localDisplayArray%(2)
	ts.linedata2 = localDisplayArray%(3)
	ts.linedata3 = localDisplayArray%(4)
	ts.xxmod     = localDisplayArray%(5)
	ts.yymod     = localDisplayArray%(6)
	ts.ds.nosave = localDisplayArray%(7)
	!                    
	ts.savdisp1$ = localDisplayArray$(1)
	ts.savdisp2$ = localDisplayArray$(2)
End Sub
!
Sub confirmDiscGenConsecuts(pCount%)
	Integer*2 pCount%, tmpCounter%
	!
	For tmpCounter% = 2 To pCount%
		Call EP.NEW.TRANSNUM
	Next tmpCounter%
End Sub
!
Function getRealInvoiceNo
	String getRealInvoiceNo
	!
	getRealInvoiceNo = " "
End Function
!
Function FE.isActive Public
	Integer*1 FE.isActive
	!
	FE.isActive = FE.active%
End Function
!
Function FE.isSkipEquivDoc Public
	Integer*1 FE.isSkipEquivDoc
	!
	FE.isSkipEquivDoc = FE.skipEquivDoc%
End Function
!
Function CONTING.isActive Public
	Integer*1 CONTING.isActive
	!
	CONTING.isActive = CONTING.active%
End Function
!
Function DISC.getConfirmDiscAmount Public
	Integer*4 DISC.getConfirmDiscAmount, tmpAnswer%
	Integer*2 tmpCounter%, tmpIndex%
	String tmpField$
	!
	tmpAnswer% = 0
	For tmpCounter% = 1 To SL.END
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 99H Then Begin
				tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
				If tmpIndex% > 0 Then Begin
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
					If Unpack$(tmpField$) = DISC.confirmItemUdKey$ Then Begin
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 2
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 3
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 4
						tmpAnswer% = tmpAnswer% + Int%( Val( tmpField$ ) )
					Endif
				Endif
			Endif
		Endif
	Next tmpCounter%
	DISC.getConfirmDiscAmount = tmpAnswer%
End Function
!
Function DISC.getJavaConfirmDiscAmount Public
	Integer*4 DISC.getJavaConfirmDiscAmount
	!
	DISC.getJavaConfirmDiscAmount = Int%( Val(consultarVariableGlobal("DISC_TOTAL")) )
End Function
!
Function DISC.getNUTFromDE
	Integer*2 tmpCounter%, tmpIndex%
	String DISC.getNUTFromDE, tmpField$, tmpAnswer$
	!
	tmpAnswer$ = ""
	tmpCounter% = 1
	While tmpCounter% <= SL.END And Len(tmpAnswer$) = 0
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 99H Then Begin
				tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
				If tmpIndex% > 0 Then Begin
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
					If Unpack$(tmpField$) = "21" Then Begin
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 2
						tmpAnswer$ = Mid$(tmpField$, 4, 4) + Unpack$(Mid$(tmpField$, 8, 8))
						DISC.NFF.PF1$ = Mid$(tmpField$, 4, 4)
						DISC.NFF.TRLOG$ = Mid$(tmpField$, 8, 8)
					Endif
				Endif
			Endif
		Endif
		tmpCounter% = tmpCounter% + 1
	Wend
	!
	DISC.getNUTFromDE = tmpAnswer$
End Function
!
Function getOptionalTitle
	String getOptionalTitle
	!
	If FE.active% = 1 Then Begin
		getOptionalTitle = DISC.confirmRemissionTxt$
	Endif Else If FE.active% > 1 Then Begin
		getOptionalTitle = getProperty("TITLE_TRXTYPE" + Str$(FE.active%), FE.paramContent$)
	Endif Else Begin
		getOptionalTitle = ""
	Endif
End Function
!
Function getOptionalLineCount
	Integer*2 getOptionalLineCount
	!
	If FE.active% = 1 Then Begin
		getOptionalLineCount = DISC.optionalLineCnt%
	Endif Else If FE.active% > 1 Then Begin
		getOptionalLineCount = Int%(Val( getProperty("OPCIONAL_LINES_COUNT" + Str$(FE.active%), FE.paramContent$) ))
	Endif Else Begin
		getOptionalLineCount = 0
	Endif
End Function
!
Function getOptionalLine(pIndex%)
	String getOptionalLine
	Integer*2 pIndex%
	!
	If FE.active% = 1 Then Begin
		getOptionalLine = DISC.optionalLines$(pIndex%)
	Endif Else If FE.active% > 1 Then Begin
		getOptionalLine = getProperty("OPTIONAL_LINE" + Str$(FE.active%) + "_" + Str$(pIndex%), FE.paramContent$)
	Endif Else Begin
		getOptionalLine = ""
	Endif
End Function
!
Sub DISC.printOptionalLines
	Integer*2 tmpCounter%, tmpCount%
	!
	TO.USEREXIT(20) = 0
	TO.USEREXIT(60) = 0
	!
	tmpCount% = getOptionalLineCount
	For tmpCounter% = 1 To tmpCount%
		Call EP.LINE.PRINT(getOptionalLine(tmpCounter%), 6100h)
	Next tmpCounter%
	!
	TO.USEREXIT(20) = -1
	TO.USEREXIT(60) = -1
End Sub
!
Sub DISC.NFF.IMPFAC
	Integer*2 tmpFillerLen%
	!
	If TS.TRX.STATUS <> 100 Then Begin
		NFF.LINF$ = UE.RFO4$ + "*" + getOptionalTitle + \
				Mid$(DISC.NFF.NUT$, 1, 4) + \
				"-" + Right$(DISC.NFF.NUT$, 9) + "*"
		tmpFillerLen% = (38 - Len(NFF.LINF$)) / 2
		NFF.LINF$ = String$(tmpFillerLen%, " ") + NFF.LINF$
		NFF.SAVPRT$ = NFF.LINF$
		Call SESION.34 ! Imp CR Lin Buff 
		Call SESION.36 ! Imp SJ Lin Buff 
		!
		Call DISC.printOptionalLines
	Endif
End Sub
!
Sub DISC.NFF.ASIGNANUM
	String tmpPrefix$
	!
	If FE.active% > 0 Then Begin
		tmpPrefix$ = getProperty("FE_PREFIX" + Str$(FE.active%), FE.paramContent$)
		If Len(tmpPrefix$) > 0 Then Begin
			tmpPrefix$ = Left$(tmpPrefix$, 1)
		Endif Else Begin
			tmpPrefix$ = "0"
		Endif
	Endif Else Begin
		tmpPrefix$ = "0"
	Endif
	!
	tmpPrefix$ = tmpPrefix$ + Right$(Date$, 2)
	!
	DISC.NFF.PF1$ = Right$(String$(4, "0") + TS.STORE$ , 4)
	DISC.NFF.TRLOG$ = String$(7, "0") + tmpPrefix$ + \
			Right$(String$(3, "0") + TS.TERMINAL$ , 3) + \
			Right$(String$(3, "0") + Str$(SL.HD.TRANSNUM + 1), 3)
	DISC.NFF.NUT$ = DISC.NFF.PF1$ + DISC.NFF.TRLOG$
	DISC.NFF.TRLOG$ = Pack$(DISC.NFF.TRLOG$)
End Sub
!
Sub DISC.FNFF.NUM Public
	String tmpNUT$
	!
	If TS.TRAINING Then Begin 
		NFF.SAVPRT$ = "TRX EN ENTRENAMIENTO"
		Call SESION.34
		Exit Sub
	EndIf 
	!
	tmpNUT$ = DISC.getNUTFromDE
	If Len(tmpNUT$) > 0 Then Begin
		DISC.NFF.NUT$ = tmpNUT$
		Call DISC.NFF.IMPFAC
	Endif Else Begin
		Call DISC.NFF.ASIGNANUM
		Call DISC.NFF.IMPFAC
		TS.TEMP1I1 = 99                                    ! Envia al LOG Num.Fact
		NFF.AUX$ = "TR"                                    !
		If NFF.SOLODV% = 2 Then NFF.AUX$ = "DV"            !
		NFF.AUX$ = NFF.AUX$ + "+"                          !
		If (TS.GROSSPOS - TS.GROSSNEG) < 0 Then Begin
			NFF.AUX$ = Left$(NFF.AUX$, 2) + "-"
		Endif
		NFF.NET$ = Str$(Abs(TS.GROSSPOS + TS.GROSSNEG))    !
		NFF.NET$ = Pack$(NFF.NET$)                         ! Vta Neta
		NFF.IT1$ = Pack$(NFF.IT1$)                         ! Plan A
		NFF.IT2$ = Pack$(NFF.IT2$)                         ! Plan B
		NFF.IT3$ = Pack$(NFF.IT3$)                         ! Plan C
		NFF.IT6$ = Pack$(NFF.IT6$)                         ! Plan D
		NFF.IT7$ = Pack$(NFF.IT7$)                         ! Vr Exento
		NFF.IT8$ = Pack$(NFF.IT8$)                         ! Impto Consumo
		TS.USERDATA$ = Pack$("21") + ":" + NFF.AUX$     + \! Arma Str Usuario con
									DISC.NFF.PF1$+DISC.NFF.TRLOG$+":"+NFF.NET$   + \! #Fact, Vr/Vta Neto
									":" + NFF.IT1$ + ":" + NFF.IT2$    + \! y Vrs de Iva
									":" + NFF.IT3$ + ":" + NFF.IT4$    + \!
									":" + NFF.IT5$ + ":" + NFF.IT6$    + \!
									":" + NFF.IT7$ + ":" + NFF.IT8$       !
		Call TSTPEC01                                      ! Invoca Subrut WR-Log
	Endif
	!
	NFF.IT1$ = "" : NFF.IT2$ = "" : NFF.IT3$ = ""      !
	NFF.IT4$ = "" : NFF.IT5$ = "" : NFF.IT6$ = ""      !
	NFF.IT7$ = "" : NFF.IT8$ = ""                      !
	TS.USERDATA$ = ""                                  !
End Sub
!
Sub CONTING.saveUD
	If Len(CONTING.fDataUdKey$) > 0 Then Begin
		TS.USERDATA$ = 																							\!
				Pack$(CONTING.fDataUdKey$) + ":" + 											\! Data1
				Right$(FISNIVRT$, 12) + ":" +														\! Data2 = Rango inicial de numeración
				Right$(FISNFVRT$, 12) + ":" +														\! Data3 = Rango final de numeración
				FISNRA$ + ":" + 											 									\! Data4 = Número de resolución fiscal
				"20" + Unpack$(FISFRA$) 																 ! Data5 = Fecha inicial de resolución fiscal
		TS.TEMP1I1 = 99
		Call TSTPEC01
	Endif
End Sub
!
Sub finalizeConfirmDiscount(pConfirmDiscAmt%)
	Integer*2 tmpRequestCount%, tmpCounter%, tmpCounter1%, tmpIndex%
	Integer*1 tmpFound%
	String tmpField$, tmpData$, tmpReturn$
	Integer*4 pConfirmDiscAmt%
	!
	!-----------------------------------------------------
	! 2021-10-04 jsv
	! Se marca header de la transacción con atributo de 
	! venta remisionable sin numeración fiscal.
	! si se ha presentado exención del iva
	!-----------------------------------------------------
	pConfirmDiscAmt% = DISC.getConfirmDiscAmount
	If 																		\
			DISC.headerUserFieldBit% > 0 And 	\! Se ha definido un bit de usuario
			pConfirmDiscAmt% <> 0 						\! Hay beneficio de día sin iva
	Then Begin
		SL.HD.INDICAT1 = SL.HD.INDICAT1 Xor DISC.headerUserFieldBit%
	Endif
	!
	!If FE.minAmount% > 0 And Abs(TS.TOTALS(0, 0, 0)) >= FE.minAmount% And FE.exPlu% = 0 And FE.active% < 1 Then Begin
	!	FE.active% = 1
	!Endif
	!
	!-----------------------------------------------------
	! 2021-10-14 jsv
	! Grabación de user data con información fiscal 
	! complementaria cuando la tienda está en modo 
	! contingencia
	!-----------------------------------------------------
	If CONTING.active% = 1 Then Begin
		Call CONTING.saveUD
	Endif
	!-----------------------------------------------------
	!
	If pConfirmDiscAmt% > 0 And trxSign% = 1 Then Begin
		tmpData$ = ""
		tmpFound% = 0
		tmpCounter% = SL.END
		While tmpCounter% > 0 And tmpFound% = 0
			If SL.STR$(tmpCounter%) <> "" Then Begin
				If Asc(SL.STR$(tmpCounter%)) = 99H Then Begin
					tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
					If Unpack$(tmpField$) = DISC.confirmRequestUdKey$ Then Begin
						tmpFound% = -1
						tmpData$ = getRealInvoiceNo
						For tmpCounter1% = 1 To 5
							Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 2 - 6
							If tmpField$ <> "" Then Begin
								tmpData$ = tmpData$ + ":" + tmpField$
							Endif
						Next tmpCounter1%
					Endif
				Endif
			Endif
			tmpCounter% = tmpCounter% - 1
		Wend
		!
		If tmpFound% Then Begin
			tmpReturn$ = EP.invocarApplManager0("75", "02", tmpData$, -1, -1)
			Call confirmDiscGenConsecuts(Int%(Val(tmpReturn$)))
		Endif
	Endif
End Sub
!
Sub printTotalLine(pTotal%)
	Integer*4 pTotal%
	String tmpTotal$
	!
	Call FORMAT.AMOUNT(pTotal%)
	tmpTotal$ = " **SUBTOTAL/TOTAL --->   $" + Right$(String$(11, " ") + TS.TEMP1$, 11)
	TO.USEREXIT(20) = 0
	TO.USEREXIT(60) = 0
	Write #34; Chr$(1bh) + Chr$(47h) + Chr$(01h) ! Negrilla
	Call EP.LINE.PRINT(tmpTotal$, 6100h)
	Write #34; Chr$(1bh) + Chr$(47h) + Chr$(00h) ! Cancela Negrilla
	TO.USEREXIT(20) = -1
	TO.USEREXIT(60) = -1
End Sub
!
! Remueve anteriores user datas de descuentos
! que requieren confirmación, en el caso de que
! el usuario haya oprimido la tecla TOTAL más 
! de una vez
Sub removeConfirmPreviousUDs
	Integer*2 tmpCounter%, tmpCounter1%, tmpIndex%
	String tmpField$
	Integer*1 tmpFound%
	!
	!Call printDebug("removeConfirmPreviousUDs SL.END=" + Str$(SL.END) + " " + Unpack$(Left$(SL.STR$(SL.END), 1)))
	!
	tmpFound% = 0
	tmpCounter% = SL.END
	While tmpCounter% > 0 And tmpFound% = 0
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 99H Then Begin
				tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
				Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
				If \
						Unpack$(tmpField$) = DISC.confirmRequestUdKey$ Or \
						Unpack$(tmpField$) = DISC.confirmItemUdKey$ \
				 Then Begin
				 	SL.END = SL.END - 1
					For tmpCounter1% = tmpCounter% To SL.END
						SL.STR$(tmpCounter1%) = SL.STR$(tmpCounter1% + 1)
					Next tmpCounter1%
					SL.STR$(SL.END + 1) = ""
					If Unpack$(tmpField$) = DISC.confirmRequestUdKey$ Then Begin
						tmpFound% = -1
					Endif
				Endif
			Endif
		Endif
		tmpCounter% = tmpCounter% - 1
	Wend
End Sub
!
Sub printConfirmDiscounts
	String tmpParam$
	!
	Call removeConfirmPreviousUDs
	!
	!Call printTotalLine(DISC.confirmVrTotal%)
	Call printTotalLine( trxSign% * (TS.TOTALS(0, 0, 0) + EP.acumDctosTrx%) )
	!
	TO.USEREXIT(20) = 0
	TO.USEREXIT(60) = 0
	!
	tmpParam$ = Left$(DISC.confirmRequestUdKey$ + String$(10, "0"), 10) + \
		Left$(DISC.confirmItemUdKey$ + String$(10, "0"), 10)
	Call EP.invocarApplManager0("75", "01", tmpParam$, 0, -1)
	!
	TO.USEREXIT(20) = -1
	TO.USEREXIT(60) = -1
	!
End Sub
!
Sub resetBonusLines
	Dim scap.bonos$(300)
End Sub
!
Sub saveBonusLines
	Integer*2 tmpCounter%
	Integer*1 tmpEnd%
	!
	Call resetBonusLines
	tmpEnd% = 0
	tmpCounter% = 0
	!
	While tmpEnd% = 0 And tmpCounter% <= 300
		If EP.APPLMGR.DATA$(tmpCounter%) <> "" Then Begin
			scap.bonos$(tmpCounter%) = EP.APPLMGR.DATA$(tmpCounter%)
			tmpCounter% = tmpCounter% + 1
		Endif Else \
			tmpEnd% = -1
	Wend
End Sub
!
!Sub totalizar
!	TS.AUTO.BAL = 1
!	DIM TS.IO.KEYS(10) ! Clear input and
!	TS.IO.MOTORKEY = 81
!	TS.IO.KEYS(6) = 81 ! substitute a TOTAL key
!	TS.TEMP1I2 = 1 !
!	TS.TRX.STATUS = 1
!	TS.BAL.TAKEN = -1
!End Sub
!
!Sub secuenciaMedioPago
!	Call totalizar
!	
!End Sub
!
Function getTenderVerifType(pTV$)
	Integer*1 getTenderVerifType
	Integer*2 tmpIndex%
	String pTV$
	!
	tmpIndex% = EP.BUSCA.INDICE.TV(pTV$)
	If tmpIndex% > -1 Then \
		getTenderVerifType = TO.TENDOPTS(tmpIndex%,7) \
	Else \
		getTenderVerifType = 0
End Function
!
Sub acumularScapDataDMP
	EP.scapDataDMP$ = EP.scapDataDMP$ + "@" + \
		Right$(String$(10,"0") + Str$(EP.scapValorPagoDMP%),10) + \
		EP.scapMedioPagoDMP$ + \
		Right$(String$(5,"0") + EP.scapInstitucionDMP$,5) + \
		Right$(String$(10,"0") + Str$(EP.scapValorDctoDMP%),10) + \
		Left$(EP.scapAcctDMP$ + String$(18,"0"),18) + \
		Right$(String$(10, "0") + Str$(EP.scapValorTeoricoDMP%), 10)
End Sub
!
Function getScapDataDMPIndex(pAmt%, pMP$, pInst%)
	Integer*2 getScapDataDMPIndex, pInst%, tmpAnswer%
	Integer*4 pAmt%
	String pMP$, tmpKey$
	!
	tmpKey$ = "@" + \
		Right$(String$(10,"0") + Str$(Abs(pAmt%)),10) + \
		pMP$ + \
		Right$(String$(5,"0") + Str$(pInst%),5)
	!
	!call ep.display.an.error(tmpKey$+"/"+EP.scapDataDMP$)
	tmpAnswer% = Match(tmpKey$,EP.scapDataDMP$,1)
	If tmpAnswer% > 0 Then \
		getScapDataDMPIndex = tmpAnswer% \
	Else Begin
		! Busca el registro con institucion en ceros
		tmpKey$ = "@" + \
			Right$(String$(10,"0") + Str$(Abs(pAmt%)),10) + \
			pMP$ + \
			String$(5,"0")		
		getScapDataDMPIndex = Match(tmpKey$,EP.scapDataDMP$,1)
	Endif
End Function
!
Function getScapDataDMPDcto(pAmt%, pMP$, pInst%)
	Integer*4 getScapDataDMPDcto, pAmt%
	Integer*2 pInst%, tmpIndex%
	String pMP$
	!
	tmpIndex% = getScapDataDMPIndex(pAmt%, pMP$, pInst%)
	If tmpIndex% > 0 Then \
		getScapDataDMPDcto = Int%(Val(Mid$(EP.scapDataDMP$, tmpIndex% + 18, 10))) \
	Else \
		getScapDataDMPDcto = 0
End Function
!
Function recuperarScapDataDMP(pAmt%, pMP$, pInst%)
	Integer*1 recuperarScapDataDMP
	Integer*4 pAmt%
	Integer*2 pInst%, tmpIndex%
	String pMP$, tmpData$
	!
	!Call ep.display.an.error(str$(pAmt%) + "," + pMP$ + "," + str$(pInst%))
	tmpIndex% = getScapDataDMPIndex(pAmt%, pMP$, pInst%)
	If tmpIndex% > 0 Then Begin
		tmpData$ = Mid$(EP.scapDataDMP$, tmpIndex% + 1, 55)
		EP.scapValorPagoDMP% = Int%(Val(Mid$(tmpData$, 1,10)))
		EP.scapMedioPagoDMP$ = Mid$(tmpData$,11,2)
		EP.scapInstitucionDMP$ = Mid$(tmpData$,13,5)
		EP.scapValorDctoDMP% = Int%(Val(Mid$(tmpData$,18,10)))
		EP.scapAcctDMP$ = Mid$(tmpData$,28,18)
		EP.scapValorTeoricoDMP% = Int%(Val(Mid$(tmpData$, 46,10)))
		recuperarScapDataDMP = -1
	Endif Else \
		recuperarScapDataDMP = 0
End Function
!
Sub removerScapDataDMP
	Integer*2 tmpIndex%
	!
	tmpIndex% = getScapDataDMPIndex(EP.scapValorPagoDMP%, EP.scapMedioPagoDMP$, Int%(Val(EP.scapInstitucionDMP$)))
	If tmpIndex% > 0 Then Begin
		EP.scapDataDMP$ = Left$(EP.scapDataDMP$,tmpIndex% - 1) + \
			Right$(EP.scapDataDMP$, Len(EP.scapDataDMP$) - tmpIndex% - 55)
	Endif
End Sub
!
Sub scapSaveDisplay
	Dim scapBakInt2%(9)
	scapBakInt2%(1) = TS.LINETYPE
	scapBakInt2%(2) = TS.LINEDATA
	scapBakInt2%(3) = TS.LINEDATA2
	scapBakInt2%(4) = TS.LINEDATA3
	scapBakInt2%(5) = TS.XXMOD
	scapBakInt2%(6) = TS.YYMOD
	scapBakInt2%(7) = TS.DS.NOSAVE
	scapBakInt2%(8) = TS.SV.LINETYPE
	scapBakInt2%(8) = LED.REQD
	!
	Dim scapBakStr$(6)
	scapBakStr$(1) = TS.SAVDISP1$
	scapBakStr$(2) = TS.SAVDISP2$
	scapBakStr$(3) = TS.DISP1$
	scapBakStr$(4) = TS.DISP2$
	scapBakStr$(5) = TS2.DISP1$
	scapBakStr$(6) = TS2.DISP2$
End Sub
!
Sub scapRestoreDisplay
	TS.LINETYPE 	   = 	scapBakInt2%(1)
	TS.LINEDATA        =    scapBakInt2%(2)
	TS.LINEDATA2       =    scapBakInt2%(3)
	TS.LINEDATA3       =    scapBakInt2%(4)
	TS.XXMOD           =    scapBakInt2%(5)
	TS.YYMOD           =    scapBakInt2%(6)
	TS.DS.NOSAVE       =    scapBakInt2%(7)
	TS.SV.LINETYPE     =    scapBakInt2%(8)
	LED.REQD           =    scapBakInt2%(8)
	!
	TS.SAVDISP1$  = scapBakStr$(1)
	TS.SAVDISP2$  = scapBakStr$(2)
	TS.DISP1$     = scapBakStr$(3)
	TS.DISP2$     = scapBakStr$(4)
	TS2.DISP1$    = scapBakStr$(5)
	TS2.DISP2$    = scapBakStr$(6)
End Sub
!
FUNCTION isSecuenciaAnulacion
	Integer*1 isSecuenciaAnulacion
	IF TS.IO.KEYS(1) = 70 THEN \
		isSecuenciaAnulacion = -1 \
	ELSE \
		isSecuenciaAnulacion = 0
END FUNCTION
!
FUNCTION isArticuloAnulacion
	Integer*1 isArticuloAnulacion
	!
	!Call printDebug("isArticuloAnulacion:    TS.XPRICE=" + Str$(TS.XPRICE))
	!
	IF TS.XPRICE < 0 THEN \
		isArticuloAnulacion = -1 \
	ELSE \
		isArticuloAnulacion = 0
END FUNCTION
!
SUB imprimirLinea(pLinea$,pEstacion%)
	STRING pLinea$
	INTEGER*2 pEstacion%
	!
	TO.USEREXIT(20) = 0
	TO.USEREXIT(60) = 0
	CALL EP.LINE.PRINT(pLinea$,pEstacion%)
	TO.USEREXIT(20) = -1
	TO.USEREXIT(60) = -1
END SUB
!
! Retorna el índice de una institución determinada
! dentro del arreglo de índices de la transacción
Function getInstitucionIndex(valorInstitucion%)
	Integer*1 getInstitucionIndex,tmpCounter%,tmpFound%
	Integer*2 valorInstitucion%
	!
	If valorInstitucion% = 0 Then \
		tmpCounter% = 0 \
	Else \
	Begin
		tmpCounter% = 1
		tmpFound% = 0
		While tmpCounter% <= institutCounter% And tmpFound% = 0
			If institucionesMP%(tmpCounter%) = valorInstitucion% Then \
			Begin
				tmpFound% = -1
			Endif \
			Else \
				tmpCounter% = tmpCounter% + 1
		Wend
		If tmpFound% = 0 Then \
		Begin
			If institutCounter% < institutMax% Then \
			Begin
				institutCounter% = institutCounter% + 1
				tmpCounter% = institutCounter%
				institucionesMP%(tmpCounter%) = valorInstitucion%
			Endif \
			Else \
				tmpCounter% = -1
		Endif
	Endif
	getInstitucionIndex = tmpCounter%
End Function
!
! Recibe un valor que puede tener más de 8 bits
! y entrega los 8 bits de la derecha
Function convertIndicat(pIndicat%)
	Integer*4 convertIndicat, pIndicat%
	!
	! Si viene negativo, le suma 256 hasta
	! volverlo positivo
	While pIndicat% < 0
		pIndicat% = pIndicat% + 256
	Wend
	!
	! Si es mayor de 255 realiza corrimientos
	! hasta tomar los 8 bits de la derecha
	While pIndicat% > 255
		pIndicat% = pIndicat% - Shift(pIndicat%, 8) * 256
	Wend
	!
	convertIndicat = pIndicat%
End Function
!
!-------------------------------------------------------------------
! Arma y retorna el string correspodiente a
! tipo (1 digito) + 
! variedad (1 digito) + 
! institucion (5 digitos) + 
! valor (10 digitos)
!-------------------------------------------------------------------
Function getMedioPagoString(indiceTipo%,indiceVariedad%,indiceInstitucion%)
	Integer*1 indiceTipo%,indiceVariedad%,indiceInstitucion%
	Integer*4 montoTVInt%
	String getMedioPagoString,montoTV$
	!
	!call ep.display.an.error("indiceTipo%="+str$(indiceTipo%))
	!call ep.display.an.error("indiceVariedad%="+str$(indiceVariedad%))
	!call ep.display.an.error("indiceInstitucion%="+str$(indiceInstitucion%))
	!call ep.display.an.error("monto="+str$(montosMP%(indiceTipo%,indiceVariedad%,indiceInstitucion%)))
	If montosMP%(indiceTipo%,indiceVariedad%,indiceInstitucion%) <> 0 Then \
	Begin
		montoTVInt% = trxSign% * montosMP%(indiceTipo%,indiceVariedad%,indiceInstitucion%)
		If montoTVInt% < 0 Then \
			montoTV$ = "-" + Right$(String$(9,"0") + Str$(Abs(montoTVInt%)),9) \
		Else \
			montoTV$ = Right$(String$(10,"0") + Str$(montoTVInt%),10)
		getMedioPagoString = \
			Str$(indiceTipo%) + \
			Str$(indiceVariedad%) + \
			Right$(String$(5,"0") + Str$(institucionesMP%(indiceInstitucion%)),5) + \
			montoTV$
	Endif Else \
		getMedioPagoString = ""
End Function
!
!----------------------------------------------------------------
! Obtiene los datos de un articulo
! y los graba en el arreglo EP.datosArticulo$
! en las siguientes posiciones
! 0: Código artículo
! 1: Cantidad
! 2: Precio unitario
! 3: Flag anulación (0=No 1=Si)
! 4: Flag pesable (0=No 1=Si)
! 5: Indicat1
!----------------------------------------------------------------
SUB EP.getDatosArticulo
	Integer*4 cantidadArticulo%
	String codigoArticulo$,flagAnulacion$,flagNormal$
	String tmpItemCode$, tmpIndicat2$, tmpDepartme$, tmpFamily$, tmpMpgroup$, tmpSalequan$, tmpSalepric$, tmpLinked$, tmpName$
	Integer*2 tmpUser1%,tmpUser2%
	Integer*1 tmpIndicat0%, tmpIndicat1%, tmpIndicat1A%
!
	DIM EP.datosArticulo$(10)
	!
	!Call printDebug("EP.getDatosArticulo  trxSign%=" + Str$(trxSign%))
	!
	If trxSign% = -1 Then \
	Begin
		flagAnulacion$ = "0"
		flagNormal$ = "1"
	Endif Else \
	Begin
		flagAnulacion$ = "1"
		flagNormal$ = "0"
	Endif
!
! Si está en recuperación toma el ir.itemcode$
! de lo contrario, toma el itemcode$
	!IF EP.scapRecupInProgress% THEN \
	!Begin
	!	!codigoArticulo$ = RIGHT$(STRING$(12,"0") + UNPACK$(IR.ITEMCODE$),12)
	!	On Error goto readArticuloError
	!	Read Form "C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2";               \!
	!		#4 Key IR.ITEMCODE$;tmpItemCode$,tmpIndicat0%,                \!
	!		tmpIndicat1%,tmpIndicat1A%,tmpIndicat2$,tmpDepartme$,tmpFamily$,       \!
	!		tmpMpgroup$,tmpSalequan$,tmpSalepric$,tmpLinked$,tmpName$,tmpUser1%,tmpUser2% !

	!	If Unpack$(tmpIndicat2$) = "05" Then Begin
	!		codigoArticulo$ = RIGHT$(STRING$(12,"0") + Unpack$(tmpSalequan$ + tmpSalepric$),12)
	!	Endif Else \
	!		codigoArticulo$ = RIGHT$(STRING$(12,"0") + UNPACK$(IR.ITEMCODE$),12)
	!	!cantidadArticulo% = SL.IE.QTYORWGT
	!	Goto readArticuloEnd
	!	readArticuloError:
	!		codigoArticulo$ = RIGHT$(STRING$(12,"0") + UNPACK$(IR.ITEMCODE$),12)
	!		Resume readArticuloEnd
	!	readArticuloEnd:
	!EndIf ELSE \
	!Begin
	!	codigoArticulo$ = RIGHT$(STRING$(12,"0") + UNPACK$(EP.scapPlu$),12)
	!	!cantidadArticulo% = INT%(VAL(TS.IO.DATA$(6)))
	!EndIf
	!
	codigoArticulo$ = Right$(String$(12, "0") + Unpack$(ITEMEXTE.CODE$), 12)
	!
	EP.datosArticulo$(0) = codigoArticulo$
	!
	cantidadArticulo% = SL.IE.QTYORWGT
!
	IF cantidadArticulo% <= 0 THEN \
		cantidadArticulo% = 1
	EP.datosArticulo$(1) = RIGHT$(STRING$(6,"0") + STR$(cantidadArticulo%),6)
	!
	If SL.IE.SALEQUAN > 0 Then \
		EP.datosArticulo$(2) = RIGHT$(STRING$(10,"0") + STR$(Round(Float(SL.IE.SALEPRIC)/Float(SL.IE.SALEQUAN),0,0)),10) \
	Else \
		EP.datosArticulo$(2) = RIGHT$(STRING$(10,"0") + STR$(SL.IE.SALEPRIC),10)
	!
	IF isArticuloAnulacion THEN \
		EP.datosArticulo$(3) = flagAnulacion$ \
	ELSE \
		EP.datosArticulo$(3) = flagNormal$
	!
	IF (IR.INDICAT0 AND 40H) NE 0 THEN \
		EP.datosArticulo$(4) = "1" \
	ELSE \
		EP.datosArticulo$(4) = "0"
	!
	!
	!---------------------------------------------------------------------------
	! jsv-2017/02/27
	!---------------------------------------------------------------------------
	! Se debe usar la variable SL.IT.INDICAT1 en vez de la variable IR.INDICAT1
	!If SL.IT.INDICAT1 < 0 Then Begin
	!	EP.datosArticulo$(5) = Str$(256 + SL.IT.INDICAT1)
	!Endif Else Begin
	!	EP.datosArticulo$(5) = Str$(SL.IT.INDICAT1)
	!Endif
	!
	EP.datosArticulo$(5) = Str$(convertIndicat(SL.IT.INDICAT1))
	!
	!Call printDebug("SL.IT.INDICAT1=" + Str$(SL.IT.INDICAT1) +"  SL.IE.INDICAT1=" + Str$(SL.IE.INDICAT1))
	!
	!IF IR.INDICAT1 < 0 THEN \
	!	EP.datosArticulo$(5) = STR$(256 + IR.INDICAT1) \
	!ELSE \
	!	EP.datosArticulo$(5) = STR$(IR.INDICAT1)
	!---------------------------------------------------------------------------
	!
	EP.datosArticulo$(5) = RIGHT$(STRING$(3,"0") + EP.datosArticulo$(5),3)
END SUB
!
!-------------------------------------------------------
! Genera un String con tipo variedad (2 caracteres) y
! monto pagado (10 digitos) para todas las formas de
! pago de la transacción
!-------------------------------------------------------
FUNCTION EP.GET.PAGOS.TV
	String EP.GET.PAGOS.TV,partialAnswer$
	Integer*1 i%,j%,k%
	!
	partialAnswer$ = ""
	For i% = 1 To 6
		For j% = 1 To 6
			For k% = 0 To institutCounter%
				partialAnswer$ = partialAnswer$ + getMedioPagoString(i%,j%,k%)
			Next k%
		Next j%
	Next i%
	EP.GET.PAGOS.TV = partialAnswer$
END FUNCTION
!
FUNCTION EP.IS.ITEM.LINE.20 PUBLIC
	Integer*1 EP.IS.ITEM.LINE.20
	Integer*2 valueChar%
	!
!	call ep.display.an.error("TS.LINETYPE="+str$(TS.LINETYPE))
!	call ep.display.an.error("TS.PRTBUF$="+TS.PRTBUF$)
	IF TS.LINETYPE <= 1 AND LEN(TS.PRTBUF$) >= 36 THEN \
	BEGIN
		valueChar% = ASC(MID$(TS.PRTBUF$,36,1))
		IF valueChar% >= 48 AND valueChar% <= 57 THEN \
			EP.IS.ITEM.LINE.20 = -1 \
		ELSE \
			EP.IS.ITEM.LINE.20 = 0
	ENDIF \
	ELSE \
		EP.IS.ITEM.LINE.20 = 0
END FUNCTION
!
FUNCTION EP.IS.ITEM.LINE.60 Public
	Integer*1 EP.IS.ITEM.LINE.60
	!
	IF \
			MID$(TS.PRTBUF$, 9,15) = MID$(EP.lineaArticulo$, 9,15) AND \
			MID$(TS.PRTBUF$,28,10) = MID$(EP.lineaArticulo$,28,10) THEN \
		EP.IS.ITEM.LINE.60 = -1 \
	ELSE \
		EP.IS.ITEM.LINE.60 = 0
END FUNCTION
!
FUNCTION EP.invocarApplManager(epAppl$,epFunction$,parteVariable$,usaConsecut%) PUBLIC
	String EP.invocarApplManager,epFunction$,parteVariable$,epAppl$,epCajero$,epInvoiceNbr$,consecut$
	Integer*1 usaConsecut%
	!
	epCajero$		= RIGHT$(STRING$(10,"0") + UNPACK$(TS.OPER$),10)
	epInvoiceNbr$	= RIGHT$(STRING$(6,"0")+STR$(SL.HD.TRANSNUM+1),6)
	!
	If usaConsecut% Then \
	Begin
		EP.SAVE.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
		EP.EPAY.TRANSNUM$ = EP.NEW.TRANSNUM
		EP.ECR.TRANSNUM$  = EP.EPAY.TRANSNUM$  !  returns transaction number used
		consecut$ = EP.EPAY.TRANSNUM$
	Endif Else \
		consecut$ = String$(6,"0")
	!
	EP.MESSAGE$ = EP.TERMINAL$ + epCajero$ + consecut$	+ \
		epInvoiceNbr$ + DATE$ + TIME$ + "00"					+ \	! Parte fija
		parteVariable$												! Parte variable
	!
	EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
	EP.MESSAGE$ = epAppl$ + epFunction$ + EP.MSGLEN$ + EP.MESSAGE$
	!
	EP.invocarApplManager = applManagerFunction(EP.MESSAGE$,0)
END FUNCTION
!
SUB EP.desplegarDatosClienteScap
	! Despliega nombre de cliente
	TO.USEREXIT(23) = 0
	CALL EP.DISPLAY.AN.ERROR("Bienvenido(a) Sr(a) " + LEFT$(gv.nombreCliente$,20))
	TO.USEREXIT(23) = -1	
END SUB
!
SUB EP.imprimirDatosClienteScap Public
	String acumPuntosTotal$,acumPuntosPeriodo$,acumPuntosRedimTotal$,acumPuntosRedimPeriodo$,\
		acumPuntosTrx$,acumPuntosRedimTrx$,bonifPuntosTrx$
	IF NOT EP.datosClienteImpresion% THEN \
	BEGIN
		IF 										\
				EP.datosClienteScap% = 4 AND	\
				TS.PROCEDURE < 1 AND			\
				TS.INTRX AND 					\
				TS.LINETYPE = 6 AND 			\
				TS.LINEDATA = 1 THEN 			\
		BEGIN
			CALL FORMAT.AMOUNT(gv.acumPuntosTotal%)
			acumPuntosTotal$ = RIGHT$(STRING$(10," ") + TS.TEMP1$,10)
			CALL FORMAT.AMOUNT(gv.acumPuntosPeriodo%)
			acumPuntosPeriodo$ = RIGHT$(STRING$(10," ") + TS.TEMP1$,10)
			!
			CALL FORMAT.AMOUNT(gv.acumPuntosRedimTotal%)
			acumPuntosRedimTotal$ = RIGHT$(STRING$(10," ") + TS.TEMP1$,10)
			CALL FORMAT.AMOUNT(gv.acumPuntosRedimPeriodo%)
			acumPuntosRedimPeriodo$ = RIGHT$(STRING$(10," ") + TS.TEMP1$,10)
			!
			CALL FORMAT.AMOUNT(gv.acumPuntosTrx%)
			acumPuntosTrx$ = RIGHT$(STRING$(10," ") + TS.TEMP1$,10)
			CALL FORMAT.AMOUNT(gv.bonifPuntosTrx%)
			bonifPuntosTrx$ = RIGHT$(STRING$(10," ") + TS.TEMP1$,10)
			CALL FORMAT.AMOUNT(gv.acumPuntosRedimTrx%)
			acumPuntosRedimTrx$ = RIGHT$(STRING$(10," ") + TS.TEMP1$,10)
			!
			CALL imprimirLinea("+" + STRING$(36,"-") + "+",6100H)
			CALL imprimirLinea("|   *** Informacion de cliente ***   |",6100H)
			CALL imprimirLinea("|Nombre : " + LEFT$(gv.nombreCliente$ + STRING$(27," "),27) + "|",6100H)
			CALL imprimirLinea("|Tarjeta: " + LEFT$(gv.tarjetaCliente$ + STRING$(27," "),27) + "|",6100H)
			CALL imprimirLinea("+" + STRING$(36,"-") + "+",6100H)
			CALL imprimirLinea("|          Resumen de puntos         |",6100H)
			CALL imprimirLinea("+---+----------+----------+----------+",6100H)
			CALL imprimirLinea("|   | Transacc | Periodo  |  Total   |",6100H)
			CALL imprimirLinea("+---+----------+----------+----------+",6100H)
			CALL imprimirLinea("|Acm|" + acumPuntosTrx$ + "|" + acumPuntosPeriodo$ + "|" + acumPuntosTotal$ + "|",6100H)
			CALL imprimirLinea("|Bnf|" + bonifPuntosTrx$ + "|    ---   |    ---   |",6100H)
			CALL imprimirLinea("|Rdm|" + acumPuntosRedimTrx$ + "|" + acumPuntosRedimPeriodo$ + "|" + acumPuntosRedimTotal$ + "|",6100H)
			CALL imprimirLinea("+---+----------+----------+----------+",6100H)
			EP.datosClienteImpresion% = -1
		ENDIF
	ENDIF
END SUB
!
SUB EP.obtenerDatosCliente
	String datosCliente$,codigoRespuesta$
	! Por ahora se desactiva la parte de obtener
	! datos de cliente mediante retail switch
	! La idea es que esto sea parametrizable
!	ON ERROR GOTO errorObtenerDatosCliente
!	IF EP.datosClienteScap% = 1 THEN \
!	BEGIN
!		datosCliente$ = EP.invocarApplManager("02","15","")
!		IF LEN(datosCliente$) >= 8 THEN \
!		BEGIN
!			codigoRespuesta$ = MID$(datosCliente$,7,2)
!			IF codigoRespuesta$ = "00" AND LEN(datosCliente$) >= 150 THEN \
!			BEGIN
!				EP.datosClienteScap% = 2
!				gv.tarjetaCliente$ = rRemove$(MID$(datosCliente$,29,20),32)
!				gv.idCliente$ = rRemove$(MID$(datosCliente$,49,20),32)
!				gv.nombreCliente$ = rRemove$(MID$(datosCliente$,69,20),32) + " " + rRemove$(MID$(datosCliente$,89,20),32)
!				gv.acumPuntosTotal% = INT%(VAL(MID$(datosCliente$,111,10)))
!				gv.acumPuntosPeriodo% = INT%(VAL(MID$(datosCliente$,121,10)))
!				gv.acumPuntosRedimTotal% = INT%(VAL(MID$(datosCliente$,131,10)))
!				gv.acumPuntosRedimPeriodo% = INT%(VAL(MID$(datosCliente$,141,10)))
!				!
!				CALL EP.desplegarDatosClienteScap
!			ENDIF ELSE \
!			BEGIN
!				EP.datosClienteScap% = 3
!			ENDIF
!		ENDIF
!	ENDIF
!	errorObtenerDatosCliente:
END SUB
!
SUB EP.refrescarValoresSistema
	CALL EP.invocarApplManager("02","18","",0)
END SUB
!
SUB EP.cargaAutomaticaPromociones
	!----------------------------------------
	! 2019-07-12 jsv
	! Se envia la variable que indica si 
	! hay un cajero en sesion
	!----------------------------------------
	!CALL EP.invocarApplManager("02","12","",0)
	!
	If TS.SIGNED.ON Then Begin
		Call EP.invocarApplManager("02", "12", \
			"1" + Unpack$(TE.TR.OPERATOR$), 0)
	Endif Else Begin
		Call EP.invocarApplManager("02", "12", \
			"0", 0)
	Endif
	!
	!----------------------------------------
END SUB
!
Function getParametroSCAP(pParam$)
	String getParametroSCAP, pParam$
	!
	getParametroSCAP = EP.invocarApplManager("02","17",pParam$,0)
End Function
!
Function isAllowPluAfterTend
	Integer*1 isAllowPluAfterTend
	!
	If getParametroSCAP("permitirArticulosDespuesMedioPago") = "1" Then \
		isAllowPluAfterTend = -1 \
	Else \
		isAllowPluAfterTend = 0
End Function
!
Function getValorActualTrx(pVrArtActual)
	Integer*4 getValorActualTrx, pVrArtActual
	!
	!Call printDebug("getValorActualTrx(" + Str$(pVrArtActual) + \
	!		")  TS.TOTALS(0,0,0)=" + Str$(TS.TOTALS(0,0,0)) + \
	!		" TS.DISC.SAVE(0,0)=" + Str$(TS.DISC.SAVE(0,0)))
	!
	!getValorActualTrx = TS.TOTALS(0,0,0) + pVrArtActual - TS.DISC.SAVE(0,0)
	getValorActualTrx = TS.TOTALS(0,0,0) + pVrArtActual
End Function
!
Sub saveRequestDE
	Call EP.ADD.DATA.ENTRY(	\!
			FE.requestDeKey$, 	\! Llave data entry request FE
			Str$(FE.active%), 	\! Respuesta 0=No  1=Sí
			"", 								\!
			"", 								\!
			"", 								\!
			"" 									\!
	)
End Sub
!
Sub CONTING.requestForFE(pNoDisplay%)
	Integer*1 pNoDisplay%
	String tmpConfirm$, tmpMessage$
	!
	!Call printDebug("CONTING.requestForFE(" + Str$(pNoDisplay%) + ") FE.active%=" + Str$(FE.active%))
	If FE.active% = -1 Then Begin
		If FE.preference% > -1 Then Begin
			FE.active% = FE.preference%
		Endif Else If FE.userRequest% = -1 Then Begin
			If pNoDisplay% Then Begin
				tmpConfirm$ = "9"
			Endif Else Begin
				TO.USEREXIT(14) = 0
				Call ep.save.keys
				Call saveLocalDisplay
				tmpConfirm$ = "0"
				tmpMessage$ = Left$(FE.requestMessage$ + String$(20, " "), 20) + "1=SI  9=NO"
				While tmpConfirm$ <> "1" And tmpConfirm$ <> "9"
					Call EP.GET.KBDATA(tmpMessage$, "1", "9", tmpConfirm$)
					If tmpConfirm$ <> "1" And tmpConfirm$ <> "9" Then Begin
						Call EP.DISPLAY.AN.ERROR("DEBE INGRESAR 1 o 9")
					Endif
				Wend
				Call ep.restore.keys
				Call restoreLocalDisplay
				TO.USEREXIT(14) = -1
			Endif
			If tmpConfirm$ = "1" Then Begin
				FE.active% = 1
			Endif Else Begin
				FE.active% = 0
			Endif
			Call saveRequestDE
		Endif
	Endif
End Sub
!
! Recibe la notificación de que se ha marcado la transacción como 
! operación de terceros
Sub DISC.notifyThirdPartyTrx(pFlag%) Public
	Integer*1 pFlag%
	!
	Call printDebug("DISC.notifyThirdPartyTrx(" + Str$(pFlag%) + ")")
	FE.active% = pFlag%
End Sub
!
SUB EP.notificarLecturaClienteExt(pCliente$, pAlterno$, pSegments$, pTotalPoints%, pPeriodPoints%, pFlagFE%) Public
	String pCliente$, pAlterno$, pSegments$
	Integer*4 pTotalPoints%, pPeriodPoints%
	Integer*1 pFlagFE%
	!
	!Call printDebug("notificarLecturaClienteExt pFlagFE%=" + Str$(pFlagFE%))
	FE.preference% = pFlagFE%
	!
	!--------------------------------------------------------------------
	! 2022-03-15 jsv
	! Al recibir notificación de ingreso cliente, se debe invocar 
	! despliegue de diálogo para preguntar sobre facturación electrónica
	!--------------------------------------------------------------------
	Call CONTING.requestForFE(0)
	!
	EP.idCliente$ = lRemove$(pCliente$, 48)
	EP.cedulaCliente$ = lRemove$(pAlterno$, 48)
	CALL EP.invocarApplManager("02",\
			"05",\
			Left$(pCliente$ + String$(20, " "), 20) + \ 
				Left$(pAlterno$ + String$(20, " "), 20) + \
				Right$(String$(10, "0") + Str$(Abs(getValorActualTrx(0))), 10) + \
				Right$(String$(10, "0") + Str$(pTotalPoints%), 10) + \
				Right$(String$(10, "0") + Str$(pPeriodPoints%), 10) + \
				pSegments$,\
			0)
	! Coloca pendientes en 1 para obligar
	! a obtener beneficios en el total
	EP.scapPendientes% = 1
	EP.datosClienteScap% = 1
	TS.BAL.TAKEN = 1	!Obliga a totalizar
END SUB
!
Sub DISC.setCustomerData(pCliente$, pAlterno$) Public
	String pCliente$, pAlterno$
	!
	EP.idCliente$ = lRemove$(pCliente$, 48)
	EP.cedulaCliente$ = lRemove$(pAlterno$, 48)
End Sub
!
SUB EP.notificarLecturaCliente
	!IF ue.clf.cliente$ NE EP.idCliente$ THEN \
	!BEGIN
	!	EP.idCliente$ = ue.clf.cliente$
	!	CALL EP.invocarApplManager("02","05",EP.idCliente$,0)
	!	! Coloca pendientes en 1 para obligar
	!	! a obtener beneficios en el total
	!	EP.scapPendientes% = 1
	!	EP.datosClienteScap% = 1
	!ENDIF
END SUB
!
SUB EP.notificarLecturaArticulo
	String parteVariable$, tmpSign$
	Integer*1 artSign%
	Integer*4 tmpTrxValue%, tmpXValue%
	!
	If trxSign% = 0 Then \
	Begin
		IF TS.TOTALS(0,0,0) = 0 THEN \
		Begin
			IF isArticuloAnulacion THEN \
				trxSign% = -1 \
			ELSE \
				trxSign% = 1
		Endif Else \
			trxSign% = SGN(TS.TOTALS(0,0,0))
	Endif
	!
	CALL EP.getDatosArticulo
	!
	!If EP.datosArticulo$(3) = "1" Then artSign% = -1 Else artSign% = 1
	If TS.XPRICE < 0 Then artSign% = -1 Else artSign% = 1
	!
	If EP.datosArticulo$(4) = "1" Then Begin
		tmpXValue% = 1000
	Endif Else Begin
		tmpXValue% = 1
	Endif
	tmpXValue% = Round(Val( EP.datosArticulo$(2) ) * Val( EP.datosArticulo$(1) ) / Float(tmpXValue%), 0, 0)
	!
	tmpTrxValue% = getValorActualTrx(Int%(artSign% * tmpXValue%))
	If tmpTrxValue% < 0 Then \
		tmpSign$ = "-" \
	Else \
		tmpSign$ = "0"
	!
	parteVariable$ = \
		EP.datosArticulo$(0) + \ ! codigo articulo
		EP.datosArticulo$(1) + \ ! cantidad
		EP.datosArticulo$(3) + \ ! flag anulacion
		EP.datosArticulo$(2) + \ ! precio
		EP.datosArticulo$(4) + \ ! flag pesable
		EP.datosArticulo$(5) + \ ! indicat1
		Right$(String$(4,"0") + Str$(SL.IT.DEPARTME),4) + \ ! dpto
		tmpSign$ + Right$(String$(9, "0") + Str$(Abs(tmpTrxValue%)), 9) + \ ! valor actual trx
		Left$(Unpack$(ITEMEXTE.LEVELS$) + String$(22, "0"), 22) + \ ! Niveles de jerarquía comercial
		Unpack$(ITEMADDI.getGenero) + \ ! Género
		Right$(String$(6, "0") + Unpack$(ITEMEXTE.TRADEMARK$), 6)  ! Código de marca
	!
	EP.scapPendientes% = 1
	CALL EP.invocarApplManager("02","03",parteVariable$,0)
END SUB
!
Function isCustomerCoupon(datoEntrado$)
	Integer*1 isCustomerCoupon, tmpAnswer%
	String datoEntrado$
	!
	!Call printDebug("isCustomerCoupon(" + datoEntrado$ + ")")
	!Call printDebug("DISC.customerCouponLeng%=" + Str$(DISC.customerCouponLeng%))
	!Call printDebug("DISC.customerCouponPref$=" + DISC.customerCouponPref$)
	!Call printDebug("DISC.customerCouponPrefLen%=" + Str$(DISC.customerCouponPrefLen%))
	!
	tmpAnswer% = 0
	If DISC.customerCouponLeng% > 0 And Len(datoEntrado$) > 0 Then Begin
		If Len(datoEntrado$) = DISC.customerCouponLeng% Then Begin
			If Left$(datoEntrado$, DISC.customerCouponPrefLen%) = DISC.customerCouponPref$ Then Begin
				tmpAnswer% = -1
			Endif
		Endif
	Endif
	isCustomerCoupon = tmpAnswer%
End Function
!
Function calculateDiscount(pPromoRegla$)
	Integer*4 calculateDiscount, tmpAnswer%, tmpPartial%
	String pPromoRegla$, tmpField$
	Integer*2 tmpCounter%, tmpIndex%
	!
	!Call printDebug("calculateDiscount(" + pPromoRegla$ + ")")
	tmpAnswer% = 0
	For tmpCounter% = 1 To SL.END
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 99H Then Begin
				tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
				Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
				!Call printDebug("Data1=" + Unpack$(tmpField$))
				If Unpack$(tmpField$) = "50000001020400000000" Then Begin
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 2
					tmpField$ = Unpack$(tmpField$)
					!Call printDebug("promoRegla=" + Left$(tmpField$, 14))
					If Left$(tmpField$, 14) = pPromoRegla$ Then Begin
						tmpPartial% = Int%(Val(Mid$(tmpField$, 33, 9)))
						If Mid$(tmpField$, 42, 1) = "1" Then Begin
							tmpPartial% = tmpPartial% * (-1)
						Endif
						tmpAnswer% = tmpAnswer% + tmpPartial%
						!Call printDebug("Partial=" + Str$(tmpPartial%) + " Cumm=" + Str$(tmpAnswer%))
					Endif
				Endif
			Endif
		Endif
	Next tmpCounter%
	!
	calculateDiscount = tmpAnswer%
End Function
!
Sub saveCustomerCouponResponse(pResponse$, pKey$)
	String pResponse$, pKey$
	!
	TS.USERDATA$ = 																							\!
			Pack$(pKey$) + ":" + 																		\! Data1
			Mid$(pResponse$,   1,  47) + ":" +											\! Data2 = Información general de la respuesta
			Mid$(pResponse$,  48,  72) + ":" + 											\! Data3 = Identificación del cupón y de la promoción
			Mid$(pResponse$, 120, 144)  														 ! Data4 = Características de la promoción
	TS.TEMP1I1 = 99
	Call TSTPEC01
End Sub
!
Sub applyRecoveredCustomerCoupons
	Integer*2 tmpCounter1%, tmpCounter2%, tmpIndex%
	String tmpRequest$, tmpField$, tmpData3$, tmpData4$
	!
	tmpCounter2% = 0
	For tmpCounter1% = 1 To SL.END
		If SL.STR$(tmpCounter1%) = DISC.customerCouponRecovery$(tmpCounter2%, 0) Then Begin
			SL.STR$(tmpCounter1%) = DISC.customerCouponRecovery$(tmpCounter2%, 1)
			tmpIndex% = Match(":", SL.STR$(tmpCounter1%), 1)
			Call getNextField(SL.STR$(tmpCounter1%), tmpIndex%, ":", tmpField$) ! Data 1
			Call getNextField(SL.STR$(tmpCounter1%), tmpIndex%, ":", tmpField$) ! Data 2
			If Mid$(tmpField$, 13, 2) = "00" Then Begin
				Call getNextField(SL.STR$(tmpCounter1%), tmpIndex%, ":", tmpField$) ! Data 3
				tmpData3$ = tmpField$
				Call getNextField(SL.STR$(tmpCounter1%), tmpIndex%, ":", tmpField$) ! Data 4
				tmpData4$ = tmpField$
				tmpRequest$ = tmpData3$ + tmpData4$
				Call EP.invocarApplManager0("53", "12", tmpRequest$, 0, -1)
			Endif
			tmpCounter2% = tmpCounter2% + 1
		Endif
	Next tmpCounter1%
End Sub
!
Sub recoverContingRequest(pData$)
	String pData$, tmpField$
	Integer*2 tmpIndex%
	!
	tmpIndex% = Match(":", pData$, 1)
	Call getNextField(pData$, tmpIndex%, ":", tmpField$) ! Data 1
	If Unpack$(tmpField$) = FE.requestDeKey$ Then Begin
		Call getNextField(pData$, tmpIndex%, ":", tmpField$) ! Data 2
		FE.active% = Int%(Val(Unpack$(tmpField$)))
	Endif
End Sub
!
Sub recoverCustomerCouponDiscount(pData$)
	String pData$, tmpRequest$, tmpResponse$, tmpField$, tmpData3$, tmpAnswerCode$, tmpAnswerMsg$
	Integer*2 tmpIndex%
	!
	tmpIndex% = Match(":", pData$, 1)
	Call getNextField(pData$, tmpIndex%, ":", tmpField$) ! Data 1
	Call getNextField(pData$, tmpIndex%, ":", tmpField$) ! Data 2
	If Mid$(tmpField$, 13, 2) = "00" Then Begin
		Call getNextField(pData$, tmpIndex%, ":", tmpField$) ! Data 3
		tmpData3$ = tmpField$
		tmpRequest$ = Left$(tmpData3$, 13) + Mid$(tmpData3$, 26, 16) + Mid$(tmpData3$, 42, 1)
		tmpResponse$ = EP.invocarApplManager0("53", "11", tmpRequest$, 0, -1)
		tmpAnswerCode$ = Mid$(tmpResponse$, 13, 2)
		If tmpAnswerCode$ = "00" Then Begin
			tmpAnswerMsg$ = DISC.customerCouponMessage$
		Endif Else Begin
			tmpAnswerMsg$ = Mid$(tmpResponse$, 15, 20)
		Endif
		Call EP.DISPLAY.AN.ERROR(tmpAnswerMsg$)
		If DISC.customerCouponRecoveryCnt% <= 20 Then Begin
			DISC.customerCouponRecovery$(DISC.customerCouponRecoveryCnt%, 0) = pData$
			DISC.customerCouponRecovery$(DISC.customerCouponRecoveryCnt%, 1) = \
				Pack$(DISC.customerCouponUDKey1$) + ":" + 							\! Data1
				Mid$(tmpResponse$,   1,  47) + ":" +										\! Data2 = Información general de la respuesta
				Mid$(tmpResponse$,  48,  72) + ":" + 										\! Data3 = Identificación del cupón y de la promoción
				Mid$(tmpResponse$, 120, 144)  													 ! Data4 = Características de la promoción
			DISC.customerCouponRecoveryCnt% = DISC.customerCouponRecoveryCnt% + 1
		Endif
	Endif
End Sub
!
Sub confirmCustomerCouponDiscount(pData$)
	String pData$, tmpResponse$
	!
	tmpResponse$ = EP.invocarApplManager0("53", "02", pData$, -1, -1)
	Call EP.DISPLAY.A.MESSAGE(Mid$(tmpResponse$, 15, 20))
	Call saveCustomerCouponResponse(tmpResponse$, DISC.customerCouponUDKey2$)
End Sub
!
Sub confirmCustomerCouponDiscounts
	Integer*2 tmpCounter%, tmpIndex%
	String tmpField$, tmpData3$, tmpData4$, tmpPromoRegla$, tmpRequest$, tmpResponse$
	!
	!Call printDebug("confirmCustomerCouponDiscounts DISC.customerCouponUDKey1$=" + DISC.customerCouponUDKey1$)
	For tmpCounter% = 1 To SL.END
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 99H Then Begin
				tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
				Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
				If Unpack$(tmpField$) = DISC.customerCouponUDKey1$ Then Begin
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 2
					If Mid$(tmpField$, 13, 2) = "00" Then Begin
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 3
						tmpData3$ = tmpField$
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 4
						tmpData4$ = tmpField$
						tmpPromoRegla$ = Mid$(tmpData3$, 46, 10) + "0001"
						!Call printDebug("promoRegla=" + tmpPromoRegla$)
						If calculateDiscount(tmpPromoRegla$) <> 0 Then Begin
							tmpRequest$ = tmpData3$ + tmpData4$
							Call confirmCustomerCouponDiscount(tmpRequest$)
						Endif
					Endif
				Endif
			Endif
		Endif
	Next tmpCounter%
End Sub
!
Function wasCustomerCouponAlreadyRead(pCustomerCoupon$)
	Integer*1 wasCustomerCouponAlreadyRead, tmpAnswer%
	String pCustomerCoupon$, tmpField$
	Integer*2 tmpCounter%, tmpIndex%
	!
	tmpAnswer% = 0
	tmpCounter% = 1
	While tmpCounter% <= SL.END And tmpAnswer% = 0
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 99H Then Begin
				tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
				Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
				If Unpack$(tmpField$) = DISC.customerCouponUDKey1$ Then Begin
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 2
					If Mid$(tmpField$, 13, 2) = "00" Then Begin
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 3
						If Left$(tmpField$, 13) = pCustomerCoupon$ Then Begin
							tmpAnswer% = -1
						Endif
					Endif
				Endif
			Endif
		Endif
		tmpCounter% = tmpCounter% + 1
	Wend
	!
	wasCustomerCouponAlreadyRead = tmpAnswer%
End Function
!
Sub parseQueryCustomerCouponResponse(pResponse$)
	String pResponse$, tmpAnswerCode$, tmpAnswerMsg$
	!
	tmpAnswerCode$ = Mid$(pResponse$, 13, 2)
	If tmpAnswerCode$ = "00" Then Begin
		tmpAnswerMsg$ = DISC.customerCouponMessage$
	Endif Else Begin
		tmpAnswerMsg$ = Mid$(pResponse$, 15, 20)
	Endif
	!
	! Despliega mensaje de respuesta
	Call EP.DISPLAY.AN.ERROR(tmpAnswerMsg$)
	!
	! Almacena user data
	Call saveCustomerCouponResponse(pResponse$, DISC.customerCouponUDKey1$)
End Sub
!
Sub queryForCustomerCoupon(pCustCoupon$, pCustId$, pDevice%)
	String pCustCoupon$, pCustId$, tmpRequest$, tmpResponse$, tmpDevice$
	Integer*2 pDevice%
	!
	If pDevice% = 3 Then Begin
		tmpDevice$ = "S"
	Endif Else Begin
		tmpDevice$ = "N"
	Endif
	tmpRequest$ = \
			Right$(String$(13, "0") + pCustCoupon$, 13) + \
			Right$(String$(16, "0") + pCustId$, 16) + \
			tmpDevice$
	tmpResponse$ = EP.invocarApplManager0("53", "01", tmpRequest$, 0, -1)
	Call parseQueryCustomerCouponResponse(tmpResponse$)
End Sub
!
FUNCTION EP.isCupon(datoEntrado$)
	Integer*1 EP.isCupon
	String datoEntrado$,anulacion$,respuesta$
	!
	IF datoEntrado$ = "" THEN \
		datoEntrado$ = UNPACK$(IR.ITEMCODE$)
	IF datoEntrado$ = "" THEN \
		EP.isCupon = 0 \
	ELSE \
	BEGIN
		!
		IF isSecuenciaAnulacion THEN \
			anulacion$ = "1" \
		ELSE \
			anulacion$ = "0"
		respuesta$ = EP.invocarApplManager("02","20",anulacion$ + datoEntrado$,0)
		IF respuesta$ = "1" THEN \
			EP.isCupon = -1 \
		ELSE \
			EP.isCupon = 0
	ENDIF
END FUNCTION
!
Sub parseOtherData(lineaOriginal$)
	String lineaOriginal$,linea$
	Integer*2 tmpIndex%,tmpIndex0%
	!
	!Call printDebug("parseOtherData(" + lineaOriginal$ + ")")
	!
	parse.promocion$ = Left$(lineaOriginal$,10)
	parse.regla$ = Mid$(lineaOriginal$,11,4)
	parse.beneficiario$ = Mid$(lineaOriginal$,15,12)
	parse.valorDescuento% = \ 
		Val(Mid$(lineaOriginal$,27,2)) * \
		Val(Mid$(lineaOriginal$,29,10))
	parse.baseDescuento% = \
		Val(Mid$(lineaOriginal$,39,10))
	parse.Und% = Int%(Val(Mid$(lineaOriginal$,49,10)))
	parse.descripcionBeneficio$ = Mid$(lineaOriginal$,59,26)
	!
	!Call printDebug("parse.valorDescuento%=" + Str$(parse.valorDescuento%))
	!
	linea$ = Right$(lineaOriginal$,Len(lineaOriginal$) - 84 )
	tmpIndex0% = 0
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.tipoBeneficio$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	!Call printDebug("parse.tipoBeneficio$=" + parse.tipoBeneficio$)
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.proveedor$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.grupoDescuento$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.tarifaIva$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.dpto$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.segmento$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.tipoRegla$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.strPeso$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.strIndiceIva$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	tmpIndex% = Match(EP.sepOtherData$, linea$, tmpIndex0% + 1)
	parse.layoutInd$ = Mid$(linea$, tmpIndex0% + 1, tmpIndex% - tmpIndex0% - 1)
	tmpIndex0% = tmpIndex%
	!
	!Call printDebug("parse.layoutInd$=" + parse.layoutInd$)
	!
End Sub
!
SUB EP.registrarUserData(promocion$,regla$,beneficiario$,grupoDescuento$,tipoRegla$,strValor$,strSigno$,tarifaIva$,	 \!
		dpto$,proveedor$,segmento$,strBase$,strPeso$,strIndiceIva$,funcion$)
	STRING promocion$,regla$,beneficiario$,grupoDescuento$,tipoRegla$,strValor$,strSigno$,tarifaIva$,	 \!
		dpto$,proveedor$,segmento$,strBase$,strPeso$,strIndiceIva$,funcion$,tmpData3$,tmpData4$, tmpData6$, unid$
	!
	If Len(strBase$) > 10 Then Begin
		unid$ = Right$(strBase$, Len(strBase$) - 10)
		strBase$ = Left$(strBase$, 10)
	Endif Else Begin
		unid$ = ""
	Endif
	!
	If strSigno$ = "1" Then Begin 													 ! Anulacion dcto
		tmpData3$ = Right$(String$(10,"0") + Str$(EP.scapValorPagoAnulDMP%),10) +	\!
				Right$(String$(10,"0") + Str$(EP.scapValorDctoAnulDMP%),10) +		\!
				Right$(String$(10,"0") + Str$(EP.scapValorTeoricoAnulDMP%),10) +		\!
				EP.scapMedioPagoAnulDMP$ +											\!
				EP.scapInstitucionAnulDMP$
		tmpData4$ = EP.scapAcctAnulDMP$
		!
		! Borra las variables de anulacion descuento x medio de pago
		EP.scapValorPagoAnulDMP% = 0
		EP.scapValorDctoAnulDMP% = 0
		EP.scapValorTeoricoAnulDMP% = 0
		EP.scapMedioPagoAnulDMP$ = ""
		EP.scapInstitucionAnulDMP$ = ""
		EP.scapAcctAnulDMP$ = ""
	Endif Else Begin 																 ! Descuento normal
		tmpData3$ = Right$(String$(10,"0") + Str$(EP.scapValorPagoDMP%),10) +		\!
				Right$(String$(10,"0") + Str$(EP.scapValorDctoDMP%),10) +			\!
				Right$(String$(10, "0") + Str$(EP.scapValorTeoricoDMP%), 10) +			\!
				EP.scapMedioPagoDMP$ +												\!
				EP.scapInstitucionDMP$
		tmpData4$ = EP.scapAcctDMP$	
	Endif
	!
	tmpData6$ = EP.cedulaCliente$
	If Mod(Len(tmpData6$), 2) > 0 Then Begin
		tmpData6$ = "F" + tmpData6$
	Endif
	tmpData6$ = tmpData6$ + Chr$(51) + Chr$(58) + Chr$(51) + Chr$(58) + Chr$(51) + Chr$(58) + unid$
	!
	CALL EP.ADD.DATA.ENTRY99(																						 \!
		EP.scapDE.key$ + funcion$ + "00000000"																						,\! CLAVE SCAP
		promocion$ + regla$ + beneficiario$ + grupoDescuento$ + tipoRegla$ + strValor$ + strSigno$ + tarifaIva$ +	 \! 
		dpto$ + proveedor$ + segmento$ + strBase$ + strPeso$ + strIndiceIva$										,\! data 2
		tmpData3$																									,\! data 3
		tmpData4$																									,\! data 4
		EP.idCliente$																								,\! data 5
		tmpData6$																									 \! data 6
	)
END SUB
!
Sub DISC.adjustDiscountTax(pDiscAmt%, pTaxRate$, pTaxIndex$) Public
	Integer*4 pDiscAmt%
	String pTaxRate$, pTaxIndex$
	!
	gv.descto.iva%(Int%(Val(pTaxIndex$)), 0) = gv.descto.iva%(Int%(Val(pTaxIndex$)), 0) - pDiscAmt%
	gv.descto.iva%(Int%(Val(pTaxIndex$)), 1) = gv.descto.iva%(Int%(Val(pTaxIndex$)), 1) - \
					Round(Float(pDiscAmt%) / (1 + float(val(pTaxRate$)) / 10000),0,0)
End Sub
!
SUB EP.registrarDescuento(promocion$,regla$,valorDescuento%,baseDescuento%, unid%,beneficiario$,proveedor$,grupoDescuento$,	\
		tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$) Public
	INTEGER*4 valorDescuento%,baseDescuento%, unid%
	INTEGER*1 indiceIva%
	STRING promocion$,regla$,beneficiario$,proveedor$,grupoDescuento$,tipoDescuento$,strValor$,strBase$,strSigno$,		\
		tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$, strUnid$
	!
	ON ERROR GOTO errorRegistrarDescuento
	TS.BALDUE(0) = TS.BALDUE(0) + valorDescuento%
	indiceIva% = Int%(Val(strIndiceIva$))
	SL.HD.GROSSNEG = SL.HD.GROSSNEG - valorDescuento%
	TS.GROSSNEG    = TS.GROSSNEG - valorDescuento%
	TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) + valorDescuento%
	TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - valorDescuento%
	SL.DI.AMTDISCO = ABS(valorDescuento%)
	!
	IF valorDescuento% > 0 THEN \
	BEGIN
		tipoDescuento$ = "04"
		strSigno$ = "1"
	ENDIF ELSE \
	BEGIN
		tipoDescuento$ = "03"
		strSigno$ = "0"
	ENDIF
	strValor$ = RIGHT$(STRING$(9,"0") + STR$(ABS(valorDescuento%)),9)
	strBase$ = RIGHT$(STRING$(10,"0") + STR$(ABS(baseDescuento%)),10)
	strUnid$ = Right$(String$(10, "0") + Str$(Abs(unid%)), 10)
	!
	! User data
	If Len(promocion$) > 0 And promocion$ <> String$(10, "0") Then Begin
		CALL EP.registrarUserData(promocion$,regla$,beneficiario$,grupoDescuento$,tipoRegla$,strValor$,strSigno$,tarifaIva$,	 \!
			dpto$,proveedor$,segmento$,strBase$ + strUnid$,strPeso$,strIndiceIva$,EP.scapDcto.function$)
	Endif
	!
	! Registro de descuento
	IF baseDescuento% > 0 THEN \
		sl.di.disrate = round(float(abs(valorDescuento%)) / float(baseDescuento%) * 10000,0,0) \
	ELSE \
		sl.di.disrate = 0
	sl.end = sl.end + 1
	call write.sl.str(sl.end, pack$(tipoDescuento$)+":" +					\
		pack$(grupoDescuento$) + ":" + pack$(str$(sl.di.disrate))+ ":" +	\
		pack$(str$(abs(valorDescuento%))) + ":")
	!
	goto finRegistrarDescuento
	errorRegistrarDescuento:
		resume finRegistrarDescuento
	finRegistrarDescuento:
END SUB
!
SUB EP.registrarCupon(promocion$,regla$,valorDescuento%,baseDescuento%,beneficiario$,proveedor$,grupoDescuento$,	\
		tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$)
	INTEGER*4 valorDescuento%,baseDescuento%
	INTEGER*1 indiceIva%
	STRING promocion$,regla$,beneficiario$,proveedor$,grupoDescuento$,tipoRegistro$,strValor$,strBase$,strSigno$,	\
		tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$,indic1$,indic2$,indic3$
	!
	ON ERROR GOTO errorRegistrarCupon
	!
	TS.BALDUE(0) = TS.BALDUE(0) + valorDescuento%
	indiceIva% = Int%(Val(strIndiceIva$))
	SL.HD.GROSSNEG = SL.HD.GROSSNEG - valorDescuento%
	TS.GROSSNEG    = TS.GROSSNEG - valorDescuento%
	TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) + valorDescuento%
	!TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - valorDescuento%
	!SL.DI.AMTDISCO = ABS(valorDescuento%)
	!
	tipoRegistro$ = "01"	! Item Entry
	IF valorDescuento% > 0 THEN \
	BEGIN
		strSigno$ = "1"
		indic2$ = pack$(str$(80H))	! Cancel key pressed
	ENDIF ELSE \
	BEGIN
		strSigno$ = "0"
		indic2$ = ""
	ENDIF
	!
	strValor$ = RIGHT$(STRING$(9,"0") + STR$(ABS(valorDescuento%)),9)
	strBase$ = RIGHT$(STRING$(10,"0") + STR$(ABS(baseDescuento%)),10)
	!
	! User data
	CALL EP.registrarUserData(promocion$,regla$,beneficiario$,grupoDescuento$,tipoRegla$,strValor$,strSigno$,tarifaIva$,	 \!
		dpto$,proveedor$,segmento$,strBase$,strPeso$,strIndiceIva$,EP.scapCupon.function$)
	!
	! Registro de Item Entry
	indic1$ = ""
	If lRemove$(lRemove$(proveedor$,48),32) <> "" Then \
		indic3$ = pack$("61") \
	Else \
		indic3$ = pack$("71")
	sl.end = sl.end + 1
	call write.sl.str(sl.end, pack$(tipoRegistro$)+":" +			\!
		pack$(beneficiario$) + ":" + 								\!
		pack$(str$(abs(valorDescuento%))) + ":" + "000" + ":" +		\!
		indic1$ + ":" + indic2$ + ":" + indic3$						\!
	)
	!
	goto finRegistrarCupon
	errorRegistrarCupon:
		resume finRegistrarCupon
	finRegistrarCupon:
END SUB
!
Sub printOptionalLayout(pLayoutKey$)
	String pLayoutKey$
	!
	If Len(pLayoutKey$) > 0 Then Begin
		Call EP.invocarApplManager("02", "50", pLayoutKey$, 0)
	Endif
End Sub
!
SUB EP.imprimirLineaBeneficio(lineaBeneficio$, pLayoutKey$)
	String lineaBeneficio$, pLayoutKey$, lineaImpresion$,lineaImpresion1$,descripcionBeneficio$,sgnValor$,beneficiario$
	Integer*4 valorDescuento%
	!
	!Call printDebug("EP.imprimirLineaBeneficio(" + lineaBeneficio$ + ", " + pLayoutKey$ + ")")
	!
	beneficiario$ = lRemove$(Mid$(lineaBeneficio$,15,12),48)
	!
	!If beneficiario$ <> "" Then \
	!	lineaImpresion1$ = "***Dcto a PLU " + beneficiario$ + "***" \
	!Else \
	!	lineaImpresion1$ = ""
	!
	beneficiario$ = Right$(String$(7, " ") + beneficiario$, 7)
	!
	valorDescuento% = \ 
		Val(Mid$(lineaBeneficio$,27,2)) * \
		Val(Mid$(lineaBeneficio$,29,10))
	!descripcionBeneficio$ = Right$(lineaBeneficio$,Len(lineaBeneficio$) - 38)
	!descripcionBeneficio$ = Mid$(lineaBeneficio$,49,26)
	descripcionBeneficio$ = lRemove$(Mid$(lineaBeneficio$,59,26), 32)
	!
	!Call printDebug("valorDescuento%=" + Str$(valorDescuento%))
	!
	CALL FORMAT.AMOUNT(valorDescuento%)
	!
	!lineaImpresion$ = \
	!	RIGHT$(STRING$(26," ") + descripcionBeneficio$,26) + \
	!	RIGHT$(STRING$(11," ") + TS.TEMP1$,11)
	!
	lineaImpresion$ = beneficiario$ + " " + \
		Left$(descripcionBeneficio$ + String$(18, " "), 18) + \
		RIGHT$(STRING$(11," ") + TS.TEMP1$,11)

	!
		!call imprimirLinea("",4100h)
		Call printOptionalLayout(pLayoutKey$)
		if prt4610.enable  then \
		begin
			write #34; chr$(1bh) + chr$(47h) +  chr$(01h)				! enables Emphasized Mode
			call imprimirLinea(lineaImpresion$,6100h)
			!
			!------------------------------------------------
			! 2019-03-22 jsv
			! Se elimina la linea Dcto a PLU
			!------------------------------------------------
			!If lineaImpresion1$ <> "" Then Begin
			!	write #34; "      " + CHR$(1BH)+CHR$(20H)+CHR$(01h)
			!	call imprimirLinea(lineaImpresion1$ + CHR$(1BH)+CHR$(20H)+CHR$(03h), 6100h)
			!	!write #34; CHR$(1BH)+CHR$(20H)+CHR$(03h)
			!Endif
			!------------------------------------------------
			!
			!call imprimirLinea("",4100h)
			write #34; chr$(1bh) + chr$(47h) +  chr$(00h)       !  Cancel Emphasized Mode
		endif \
		else Begin
			call imprimirLinea(lineaImpresion$, 6100H)
			!
			!------------------------------------------------
			! 2019-03-22 jsv
			! Se elimina la linea Dcto a PLU
			!------------------------------------------------
			!If lineaImpresion1$ <> "" Then Begin
			!	call imprimirLinea(String$(6," ") + lineaImpresion1$,6100h)
			!Endif
			!------------------------------------------------
			!
			!call imprimirLinea("",4100h)
		Endif
END SUB
!
Sub EP.imprimirBeneficio(pValorDescuento%, pBenef$, pDescrip$) Public
	String pBenef$, pDescrip$
	String lineaImpresion$,lineaImpresion1$,descripcionBeneficio$,sgnValor$,beneficiario$
	Integer*4 pValorDescuento%
	!
	beneficiario$ = lRemove$(pBenef$, 48)
	!
	beneficiario$ = Right$(String$(7, " ") + beneficiario$, 7)
	!
	descripcionBeneficio$ = lRemove$(pDescrip$, 32)
	!
	CALL FORMAT.AMOUNT(pValorDescuento%)
	!
	lineaImpresion$ = beneficiario$ + " " + \
		Left$(descripcionBeneficio$ + String$(18, " "), 18) + \
		RIGHT$(STRING$(11," ") + TS.TEMP1$,11)
	If prt4610.enable Then Begin
		Write #34; chr$(1bh) + chr$(47h) +  chr$(01h)				! enables Emphasized Mode
		Call imprimirLinea(lineaImpresion$,6100h)
		!
		!------------------------------------------------
		! 2019-03-22 jsv
		! Se elimina la linea Dcto a PLU
		!------------------------------------------------
		!If lineaImpresion1$ <> "" Then Begin
		!	write #34; "      " + CHR$(1BH)+CHR$(20H)+CHR$(01h)
		!	call imprimirLinea(lineaImpresion1$ + CHR$(1BH)+CHR$(20H)+CHR$(03h), 6100h)
		!	!write #34; CHR$(1BH)+CHR$(20H)+CHR$(03h)
		!Endif
		!------------------------------------------------
		!
		!call imprimirLinea("",4100h)
		Write #34; chr$(1bh) + chr$(47h) +  chr$(00h)       !  Cancel Emphasized Mode
	Endif Else Begin
		Call imprimirLinea(lineaImpresion$, 6100H)
		!
		!------------------------------------------------
		! 2019-03-22 jsv
		! Se elimina la linea Dcto a PLU
		!------------------------------------------------
		!If lineaImpresion1$ <> "" Then Begin
		!	call imprimirLinea(String$(6," ") + lineaImpresion1$,6100h)
		!Endif
		!------------------------------------------------
		!
		!call imprimirLinea("",4100h)
	Endif
END SUB
!
SUB almacenarLinea(promocion$,regla$,beneficiario$,descripcionBeneficioOrig$,valorDescuento%,\
		baseDescuentoOrig%,tipoBeneficio$,proveedor$,grupoDescuento$,	\
		tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$)
	String promocion$,regla$,beneficiario$,promoRegla$,descripcionBeneficio$,descripcionBeneficioOrig$,sgnValor$
	String tipoBeneficio$,proveedor$,grupoDescuento$,	\
		tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$,otherData$
	Integer*4 valorDescuento%,valorAcumulado%,baseDescuento%,baseDescuentoOrig%,baseAcumulada%, undAcumuladas%, tmpUndPartial%
	Integer*2 tmpCounter%
	Integer*1 tmpFound%,tmpNotFound%
	!
	!Call printDebug("almacenarLinea")
	!
	descripcionBeneficio$ = Right$(String$(26," ") + descripcionBeneficioOrig$,26)
	otherData$ = tipoBeneficio$ + EP.sepOtherData$ + proveedor$ + EP.sepOtherData$ + grupoDescuento$
	otherData$ = otherData$ + EP.sepOtherData$ + tarifaIva$ + EP.sepOtherData$ + dpto$ + EP.sepOtherData$ + segmento$
	otherData$ = otherData$ + EP.sepOtherData$ + tipoRegla$ + EP.sepOtherData$ + strPeso$ + EP.sepOtherData$ + strIndiceIva$
	otherData$ = otherData$ + EP.sepOtherData$ + Str$(tmpLayoutIndicator%)
	otherData$ = otherData$ + EP.sepOtherData$
	!
	If valorDescuento% < 0 Then \
		sgnValor$ = "-1" \
	Else \
		sgnValor$ = "01"
	baseDescuento% = Abs(baseDescuentoOrig%) * Val(sgnValor$)
	!
	promoRegla$ = promocion$ + regla$ + beneficiario$
	tmpCounter% = 0
	tmpFound% = 0
	tmpNotFound% = 0
	!
	While tmpFound% = 0 And tmpNotFound% = 0 And tmpCounter% <= 300
		If EP.scapDiscountLine$(tmpCounter%) = "" Then \
			tmpNotFound% = -1 \
		Else If Left$(EP.scapDiscountLine$(tmpCounter%),26) = promoRegla$ And \
				(Val(beneficiario$) > 0 Or tmpLayoutIndicator% = 0) Then Begin ! Solo consolida beneficios en descuento a producto
			tmpFound% = -1
			descripcionBeneficio$ = Mid$(EP.scapDiscountLine$(tmpCounter%),59,26)
		Endif Else \
			tmpCounter% = tmpCounter% + 1 
	Wend
	!
	tmpUndPartial% = -Sgn(valorDescuento%)
	If Val(strPeso$) <> 0 Then Begin
		tmpUndPartial% = tmpUndPartial% * Abs(Int%(Val(strPeso$)))
	Endif
	!
	!Call printDebug("benef=" + beneficiario$ + " unds=" + Str$(tmpUndPartial%))
	!
	If tmpFound% = -1 Then \
	Begin
		valorAcumulado% = valorDescuento% + \ 
			Val(Mid$(EP.scapDiscountLine$(tmpCounter%),27,2)) * \
			Val(Mid$(EP.scapDiscountLine$(tmpCounter%),29,10))
		baseAcumulada% = baseDescuento% + \
			Val(Mid$(EP.scapDiscountLine$(tmpCounter%),27,2)) * \
			Val(Mid$(EP.scapDiscountLine$(tmpCounter%),39,10))
		!
		undAcumuladas% = tmpUndPartial% - Val(Mid$(EP.scapDiscountLine$(tmpCounter%),27,2)) * Val(Mid$(EP.scapDiscountLine$(tmpCounter%), 49, 10))
		!
		!Call printDebug("cumm unds=" + Str$(undAcumuladas%))
		!
		If valorAcumulado% < 0 Then \
			sgnValor$ = "-1" \
		Else \
			sgnValor$ = "01"	
	EndIf Else \
	Begin
		valorAcumulado% = valorDescuento%
		baseAcumulada% = baseDescuento%
		undAcumuladas% = tmpUndPartial%
	Endif
	!
	EP.scapDiscountLine$(tmpCounter%) = promoRegla$ + \
		sgnValor$ + \
		Right$(String$(10,"0") + Str$(Abs(valorAcumulado%)),10) + \
		Right$(String$(10,"0") + Str$(Abs(baseAcumulada%)),10) + \
		Right$(String$(10, "0") + Str$(Abs(undAcumuladas%)), 10) + \
		descripcionBeneficio$ + \ 
		otherData$
	!
	!Call printDebug("EP.scapDiscountLine$(" + Str$(tmpCounter%) + ")=" + EP.scapDiscountLine$(tmpCounter%))
END SUB
!
SUB EP.procesarLineaBeneficio(lineaBeneficio$)
	String promocion$,regla$,lineaBeneficio$,tipoBeneficio$,descripcionBeneficio$,\
		beneficiario$,proveedor$,lineaImpresion$,grupoDescuento$,tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$
	Integer*4 valorDescuento%,valorPuntos%,baseDescuento%
	Integer*1 signoInt%
	!
	!Call printDebug("EP.procesarLineaBeneficio(" + lineaBeneficio$ + ")")
	!
	tmpLayoutIndicator% = 0
	!
	ON ERROR GOTO errorProcesarLineaBeneficio
	IF LEN(lineaBeneficio$) >= 57 THEN \
	BEGIN
		tipoBeneficio$ = MID$(lineaBeneficio$,1,1)
		IF tipoBeneficio$ = "0" OR tipoBeneficio$ = "2" THEN \
		BEGIN
			signoInt% = -1
		ENDIF \
		ELSE \
		BEGIN
			signoInt% = 1
		ENDIF
		If trxSign% <> 0 Then \
			signoInt% = signoInt% * trxSign%
		valorDescuento% = INT%(VAL(MID$(lineaBeneficio$,2,10)))
		valorPuntos% = INT%(VAL(MID$(lineaBeneficio$,14,10)))
		descripcionBeneficio$ = MID$(lineaBeneficio$,24,20)
		promocion$ = MID$(lineaBeneficio$,44,10)
		regla$ = MID$(lineaBeneficio$,54,4)
		IF LEN(lineaBeneficio$) >= 58 THEN \
			beneficiario$ = MID$(lineaBeneficio$,58,12) \
		ELSE \
			beneficiario$ = STRING$(12,"0")
		IF LEN(lineaBeneficio$) >= 70 THEN \
			baseDescuento% = INT%(VAL(MID$(lineaBeneficio$,70,10))) \
		ELSE \
			baseDescuento% = 0	
		IF LEN(lineaBeneficio$) >= 80 THEN \
			proveedor$ = MID$(lineaBeneficio$,80,15) \
		ELSE \
			proveedor$ = STRING$(15,"0")
		IF LEN(lineaBeneficio$) >= 95 THEN \
			grupoDescuento$ = MID$(lineaBeneficio$,95,4) \
		ELSE \
			grupoDescuento$ = "7777"
		IF LEN(lineaBeneficio$) >= 99 THEN \
			tarifaIva$ = MID$(lineaBeneficio$,99,4) \
		ELSE \
			tarifaIva$ = STRING$(4,"0")
		IF LEN(lineaBeneficio$) >= 103 THEN \
			dpto$ = MID$(lineaBeneficio$,103,4) \
		ELSE \
			dpto$ = STRING$(4,"0")
		IF LEN(lineaBeneficio$) >= 107 THEN \
			segmento$ = MID$(lineaBeneficio$,107,5) \
		ELSE \
			segmento$ = STRING$(5,"0")
		IF LEN(lineaBeneficio$) >= 112 THEN \
			tipoRegla$ = MID$(lineaBeneficio$,112,2) \
		ELSE \
			tipoRegla$ = STRING$(2,"0")
		IF LEN(lineaBeneficio$) >= 114 THEN \
			strPeso$ = MID$(lineaBeneficio$,114,6) \
		ELSE \
			strPeso$ = STRING$(6,"0")
		IF LEN(lineaBeneficio$) >= 120 THEN \
			strIndiceIva$ = MID$(lineaBeneficio$,120,2) \
		ELSE \
			strIndiceIva$ = STRING$(2,"0")
		!
		If Right$(lineaBeneficio$, 2) = "@L" Then Begin
			tmpLayoutIndicator% = 1
		Endif
		!
		!Call printDebug("valorDescuento%=" + Str$(valorDescuento%))
		!
		IF valorDescuento% > 0 THEN \
		BEGIN
			valorDescuento% = valorDescuento%*signoInt%
			IF VAL(beneficiario$) > 0 Then Begin
				gv.descto.iva%(Int%(Val(strIndiceIva$)),0) = gv.descto.iva%(Int%(Val(strIndiceIva$)),0) - valorDescuento%
				! iva
				gv.descto.iva%(Int%(Val(strIndiceIva$)),1) = gv.descto.iva%(Int%(Val(strIndiceIva$)),1) - \
					round(float(valorDescuento%) / (1 + float(val(tarifaIva$)) / 10000),0,0)
			Endif Else Begin
				totalDesctoPago% = totalDesctoPago% - valorDescuento%
			Endif
			!
			IF NOT EP.scapRecupInProgress% THEN \
			BEGIN
				CALL FORMAT.AMOUNT(valorDescuento%)
				lineaImpresion$ = \
					RIGHT$(STRING$(26," ") + descripcionBeneficio$,26) + \
					RIGHT$(STRING$(11," ") + TS.TEMP1$,11)			
				CALL EP.DISPLAY.A.MESSAGE(lineaImpresion$)
				CALL almacenarLinea(promocion$,regla$,beneficiario$,descripcionBeneficio$,valorDescuento%,\
						baseDescuento%,tipoBeneficio$,proveedor$,grupoDescuento$,	\
						tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$)
				IF tipoBeneficio$ = "3" OR tipoBeneficio$ = "2" THEN \
					CALL EP.registrarCupon(promocion$,regla$,valorDescuento%,baseDescuento%,beneficiario$,		\
						proveedor$,grupoDescuento$,tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$)
				!ELSE \
				!	CALL EP.registrarDescuento(promocion$,regla$,valorDescuento%,baseDescuento%,beneficiario$,	\
				!		proveedor$,grupoDescuento$,tarifaIva$,dpto$,segmento$,tipoRegla$,strPeso$,strIndiceIva$)
			ENDIF
		ENDIF
	ENDIF
	goto finProcesarLineaBeneficio
	errorProcesarLineaBeneficio:
		resume finProcesarLineaBeneficio
	finProcesarLineaBeneficio:
END SUB
!
SUB EP.procesarBeneficios
	String lineaBeneficio$
	Integer*1 procesoBeneficios%
	Integer*2 counter%, tmpMatch%
	!
	tmpMatch% = 0
	!
	!Call printDebug("EP.procesarBeneficios. DISC.confirmList$=" + DISC.confirmList$)
	!
	counter% = 0
	procesoBeneficios% = -1
	!
	Dim EP.scapDiscountLine$(300)
	WHILE procesoBeneficios%
		lineaBeneficio$ = EP.APPLMGR.DATA$(counter%)
		IF lineaBeneficio$ = "" THEN \
			procesoBeneficios% = 0 \
		ELSE \
		BEGIN
			CALL EP.procesarLineaBeneficio(lineaBeneficio$)
			counter% = counter% + 1
		ENDIF
	WEND
	!
	counter% = 0
	procesoBeneficios% = -1
	WHILE procesoBeneficios%
		lineaBeneficio$ = EP.scapDiscountLine$(counter%)
		!
		!Call printDebug("lineaBeneficio$=" + lineaBeneficio$)
		!
		IF lineaBeneficio$ = "" THEN \
			procesoBeneficios% = 0 \
		ELSE \
		BEGIN
			CALL parseOtherData(lineaBeneficio$)
			If parse.valorDescuento% <> 0 Then Begin
				IF parse.tipoBeneficio$ <> "3" AND parse.tipoBeneficio$ <> "2" THEN \
					CALL EP.registrarDescuento(parse.promocion$,parse.regla$,parse.valorDescuento%,parse.baseDescuento%, parse.Und%,parse.beneficiario$,	\
						parse.proveedor$,parse.grupoDescuento$,parse.tarifaIva$,parse.dpto$,parse.segmento$,parse.tipoRegla$,parse.strPeso$,parse.strIndiceIva$)
				!
				tmpMatch% = Match(parse.promocion$ + parse.regla$, DISC.confirmList$, 1)
				If tmpMatch% Then Begin
					DISC.confirmChange% = -1
				Endif
				!
				If parse.layoutInd$ = "1" Then Begin
					Call printTotalLine(trxSign% * (TS.TOTALS(0, 0, 0) + EP.acumDctosTrx% - parse.valorDescuento%))
					Call EP.imprimirLineaBeneficio(lineaBeneficio$, parse.promocion$ + parse.regla$)
				Endif Else Begin
					If parse.valorDescuento% > 0 Or tmpMatch% = 0 Then Begin
						CALL EP.imprimirLineaBeneficio(lineaBeneficio$, "")
					Endif
				Endif
			Endif
			counter% = counter% + 1
		ENDIF
	WEND
END SUB
!
SUB EP.obtenerBeneficios(pFlag$)
	String pFlag$
	Integer*2 tmpCount%, tmpCounter%
	!
	ON ERROR GOTO errorObtenerBeneficios
	!
	!Call printDebug("Invoca EP.obtenerBeneficios")
	!
	Call asignarVariableGlobal("ACTUAL_REEDEMED_COUNT", "0" )
	EP.applMgrAnswer$ = EP.invocarApplManager("02","04",pFlag$,-1)
	!
	!Call printDebug("Ans: " + EP.applMgrAnswer$)
	!
	IF LEN(EP.applMgrAnswer$) = 3 THEN \
		EP.scapPendientes% = INT%(VAL(EP.applMgrAnswer$)) \
	ELSE \
		EP.scapPendientes% = 0
	!
	CALL EP.procesarBeneficios
	!
	goto finObtenerBeneficios
	errorObtenerBeneficios:
		Call printDebug(parseError)
		resume finObtenerBeneficios
	finObtenerBeneficios:
	tmpCount% = Int%(Val( consultarVariableGlobal("ACTUAL_REEDEMED_COUNT") )) - 1
	For tmpCounter% = 1 To tmpCount%
		Call EP.NEW.TRANSNUM
	Next tmpCounter%
END SUB
!
Sub DISC.confirmDiscounts
	String pMsg$, tmpConfirm$
	!
	If DISC.isConfirmDiscActive% Then Begin
		TO.USEREXIT(14) = 0
		Call ep.save.keys
		Call EP.GET.KBDATA(DISC.confirmMessage$, "0", "0", tmpConfirm$)
		Call ep.restore.keys
		TO.USEREXIT(14) = -1
		If tmpConfirm$ = "0" Then Begin
			DISC.confirm% = 1
		Endif Else Begin
			DISC.confirm% = 0
		Endif
	Endif Else Begin
		DISC.confirm% = 0
	Endif
	Call asignarVariableGlobal("DISC_CONFIRMATION", Str$( DISC.confirm% ) )
End Sub
!
Sub PPP.resetVariables
	Call asignarVariableGlobal("CHANGE_ERROR", "" )
	Call asignarVariableGlobal("POINTS_TO_CHANGE", "-1" )
	Call asignarVariableGlobal("MAX_CHANGE_AMOUNT", "0")
	PPP.errorMsg$ = ""
	PPP.points% = -1
	PPP.maxPoints% = 0
	PPP.totalBeforeDisc% = 0
End Sub
!
Sub DISC.updateConfirmFlag
	String tmpGlobalVar$
	!
	tmpGlobalVar$ = consultarVariableGlobal("DISC_CONFIRMATION")
	DISC.confirm% = Int%(Val(tmpGlobalVar$))
	!
	tmpGlobalVar$ = consultarVariableGlobal("POINTS_TO_CHANGE")
	PPP.points% = Int%(Val(tmpGlobalVar$))
	!
	tmpGlobalVar$ = consultarVariableGlobal("MAX_CHANGE_AMOUNT")
	PPP.maxPoints% = Int%(Val(tmpGlobalVar$))
	PPP.errorMsg$ = consultarVariableGlobal("CHANGE_ERROR")
End Sub
!
Sub PPP.changeStatus(pStatus%)
	Integer*4 pStatus%
	!
	PPP.points% = pStatus%
	Call asignarVariableGlobal("POINTS_TO_CHANGE", Str$(pStatus%))
End Sub
!
Sub PPP.saveUDs
	String tmpUDKeys$
	!
	!Call printTotalLine(PPP.totalBeforeDisc%)
	!
	tmpUDKeys$ = \
		Left$(PPP.headerUDKey$ + String$(20, " "), 20) + \
		Left$(PPP.detailUDKey$ + String$(20, " "), 20)
	Call EP.invocarApplManager0("80", "09", tmpUDKeys$, 0, -1)
	Call PPP.changeStatus(-2)
End Sub
!
Sub PPP.endRecovery
	! Envía a Java solicitud para recuperar estado de puntos por pesos
	PPP.inRecovery% = 0
	Call EP.invocarApplManager0("80", "11", "4", 0, -1)
	PPP.points% = -2
End Sub
!
Sub PPP.recovery
	Integer*2 tmpCounter1%, tmpIndex%
	String tmpField$, tmpFullData$
	Integer*1 tmpPPPEntry%
	!
	tmpPPPEntry% = 0
	If TS.TEMP1I2 = 99 Then Begin
		tmpIndex% = Match(":", SL.STR.ENTRY$, 1)
		Call getNextField(SL.STR.ENTRY$, tmpIndex%, ":", tmpField$) ! Data 1
		!
		Call printDebug("PPP.recovery --> " + Unpack$(tmpField$))
		!
		If Unpack$(tmpField$) = PPP.headerUDKey$ Then Begin
			! Encabezado de cambio de puntos por pesos
			tmpPPPEntry% = -1
			PPP.inRecovery% = -1
			tmpFullData$ = "1"
			For tmpCounter1% = 1 To 5
				Call getNextField(SL.STR.ENTRY$, tmpIndex%, ":", tmpField$) ! Data 2 - 6
				tmpFullData$ = tmpFullData$ + tmpField$ + ","
			Next tmpCounter1%
		Endif Else If Unpack$(tmpField$) = PPP.detailUDKey$ Then Begin
			! Detalle de puntos por pesos
			tmpPPPEntry% = -1
			tmpFullData$ = "2"
			For tmpCounter1% = 1 To 4
				Call getNextField(SL.STR.ENTRY$, tmpIndex%, ":", tmpField$) ! Data 2 - 5
				tmpFullData$ = tmpFullData$ + tmpField$
			Next tmpCounter1%
		Endif Else If Unpack$(tmpField$) = PPP.voidUDKey$ Then Begin
			! Anulación de puntos por pesos
			tmpPPPEntry% = -1
			tmpFullData$ = "3"
			Call getNextField(SL.STR.ENTRY$, tmpIndex%, ":", tmpField$) ! Data 2
			tmpFullData$ = tmpFullData$ + tmpField$
		Endif
	Endif
	!
	If tmpPPPEntry% Then Begin
		If Left$(tmpFullData$, 1) = "3" And PPP.inRecovery% = -1 Then Begin
			Call PPP.endRecovery
		Endif
		Call EP.invocarApplManager0("80", "11", tmpFullData$, 0, -1)
	Endif Else If tmpPPPEntry% = 0 And PPP.inRecovery% = -1 Then Begin
		Call PPP.endRecovery
	Endif
End Sub
!
Sub PPP.notifyEOT(pVoid$)
	String pVoid$
	Integer*1 tmpConsecut%
	!
	If pVoid$ = "1" Then Begin
		tmpConsecut% = -1
	Endif Else Begin
		tmpConsecut% = 0
	Endif
	Call EP.invocarApplManager0("80", "10", pVoid$, tmpConsecut%, -1)
	PPP.points% = -1
End Sub
!
Function PPP.voidRedemption
	String PPP.voidRedemption
	!
	PPP.voidRedemption = EP.invocarApplManager0("80", "05", PPP.voidUDKey$, -1, -1)
End Function
!
Function PPP.confirmVoid(pMessage$)
	Integer*1 PPP.confirmVoid, tmpAnswer%
	String pMessage$, tmpConfirm$
	!
	tmpAnswer% = 0
	TO.USEREXIT(14) = 0
	Call ep.save.keys
	!
	Call EP.GET.KBDATA(Left$(pMessage$ + String$(20, " "), 20) + "Intro=Si  Borrar=No", "0", "0", tmpConfirm$)
	If tmpConfirm$ = "0" Then Begin
		tmpAnswer% = Int%(Val(PPP.voidRedemption))
		If tmpAnswer% = 0 Then Begin
			Call EP.DISPLAY.AN.ERROR("SE PRESENTO ERROR   AL ANULAR")
		Endif Else Begin
			PPP.points% = -1
		Endif
	Endif
	!
	Call ep.restore.keys
	TO.USEREXIT(14) = -1
	!
	PPP.confirmVoid = tmpAnswer%
End Function
!
Sub PPP.compareOtp(pOtp$, pStatus%)
	Integer*4 pStatus%, tmpStatus%, tmpTriesCounter%
	String pOtp$, tmpOtp$
	!
	tmpTriesCounter% = 0
	!
	!Call saveLocalDisplay
	!
	While tmpTriesCounter% < PPP.otpRetries%
		tmpOtp$ = ""
		Call EP.GET.KBDATA("Ingrese clave OTP", "1", "9999999999", tmpOtp$)
		If TS.IO.MOTORKEY = 73 Then Begin
			! Cancelación de usuario
			tmpStatus% = 0
			tmpTriesCounter% = PPP.otpRetries%
		Endif Else If Val(pOtp$) = Val(tmpOtp$) Then Begin
			tmpTriesCounter% = PPP.otpRetries%
			tmpStatus% = pStatus%
		Endif Else Begin
			Call EP.DISPLAY.AN.ERROR("Clave incorrecta")
			tmpTriesCounter% = tmpTriesCounter% + 1
			tmpStatus% = 0
		Endif
	Wend
	!
	!Call restoreLocalDisplay
	!
	pStatus% = tmpStatus%
End Sub
!
Sub PPP.validateOtp(pStatus%)
	Integer*4 pStatus%
	String tmpAnswer$
	!
	tmpAnswer$ = EP.invocarApplManager0("80", "20", Str$( pStatus% ), 0, -1)
	If Left$(tmpAnswer$, 1) = "2" Then Begin
		! Se presentó error al obtener OTP
		pStatus% = 0
	Endif Else If Left$(tmpAnswer$, 1) = "1" Then Begin
		! Debe pedir otp y comparar con el otp recibido en el servidor
		Call PPP.compareOtp( Right$(tmpAnswer$, Len(tmpAnswer$) - 1), pStatus% )
	Endif
End Sub
!
Sub PPP.confirmDiscount
	String tmpConfirm$
	Integer*4 tmpStatus%
	!
	TO.USEREXIT(14) = 0
	Call ep.save.keys
	confirmPPP.start:
	If PPP.maxPoints% > 0 Then Begin
		Call EP.GET.KBDATA("Desea cambiar ptos? Intro=Si  Borrar=No", "0", "0", tmpConfirm$)
		If tmpConfirm$ = "0" Then Begin
			confirmPPP.select:
			Call EP.GET.KBDATA(\
					"Puntos a cambiar    1=" + Str$(PPP.maxPoints%) + " 2=Otro #", "1", "2", tmpConfirm$)
			If TS.IO.MOTORKEY = 73 Then Begin
				Goto confirmPPP.start
			Endif Else If Val(tmpConfirm$) < 1 Or Val(tmpConfirm$) > 2 Then Begin
				Call EP.DISPLAY.AN.ERROR("Seleccione opcion   entre 1 y 2")
				Goto confirmPPP.select
			Endif Else If tmpConfirm$ = "1" Then Begin
				tmpStatus% = PPP.maxPoints%
			Endif Else Begin
				confirmPPP.other:
				Call EP.GET.KBDATA("Ingrese # a cambiar", "1000", String$(Len(Str$(PPP.maxPoints%)), "9"), tmpConfirm$)
				If TS.IO.MOTORKEY = 73 Then Begin
					Goto confirmPPP.start
				Endif Else If Val(tmpConfirm$) > PPP.maxPoints% Then Begin
					Call EP.DISPLAY.AN.ERROR("# maximo a cambiar  " + Str$(PPP.maxPoints%))
					Goto confirmPPP.other
				Endif Else Begin
					tmpStatus% = Int%(Val(tmpConfirm$))
				Endif
			Endif
		Endif Else Begin
			Call EP.GET.KBDATA("Esta seguro?        Intro=Si  Borrar=No", "0", "0", tmpConfirm$)
			If tmpConfirm$ = "0" Then Begin
				tmpStatus% = 0
			Endif Else Begin
				Goto confirmPPP.start
			Endif
		Endif
	Endif Else Begin
		tmpStatus% = 0
	Endif
	!
	If tmpStatus% > 0 Then Begin
		! Invoca validación por otp
		Call PPP.validateOtp(tmpStatus%)
	Endif
	!
	Call PPP.changeStatus(tmpStatus%)
	!
	Call ep.restore.keys
	TO.USEREXIT(14) = -1
End Sub
!
Sub reevaluateBenefits
	Integer*2 triesCounter%
	!
	Call EP.invocarApplManager("02", "27", "", 0)
	triesCounter% = 0
	EP.scapPendientes% = 1
	While EP.scapPendientes% > 0 And triesCounter% < 300
		Call EP.obtenerBeneficios("0")
		If EP.scapPendientes% > 0 Then Begin
			triesCounter% = triesCounter% + 1
			Wait ; 200
		EndIf
	Wend
End Sub
!
SUB EP.obtenerBeneficiosTransaccion(incluyeMP$)
	String incluyeMP$,montoTrx$
	Integer*4 montoTrxInt%, tmpTotalDiscount%
	!
	If consultarVariableGlobal("redemptionPending") = "true" Then Begin
		Call reevaluateBenefits
	Endif
	!
	DISC.confirmChange% = 0
	obtenerBeneficiosTransaccion.start:
	
	!montoTrxInt% = trxSign% * (TS.TOTALS(0,0,0) + EP.acumDctosMP%)
	montoTrxInt% = trxSign% * (TS.TOTALS(0, 0, 0) + EP.acumDctosTrx%)
	!
	If montoTrxInt% < 0 Then \
		montoTrx$ = "-" + Right$(String$(9,"0") + Str$(Abs(montoTrxInt%)),9) \
	Else \
		montoTrx$ = Right$(String$(10,"0") + Str$(montoTrxInt%),10)	
	!
	IF incluyeMP$ = "1" THEN \
		CALL EP.invocarApplManager(		\
			"02","06",montoTrx$ + incluyeMP$ + EP.GET.PAGOS.TV,0) \
	ELSE \
		CALL EP.invocarApplManager(		\
			"02","06",montoTrx$ + incluyeMP$,0)
	!
	CALL EP.procesarBeneficios
	!
	Call DISC.updateConfirmFlag
	!
	!If EP.scapRecupInProgress% = 0 Then Begin
		If DISC.confirm% = -2 Then Begin
			Call DISC.confirmDiscounts
			If DISC.confirm% = 1 Then Begin
				DISC.confirmList$ = consultarVariableGlobal("CONFIRM_LIST")
				DISC.confirmVrTotal% = trxSign% * (TS.TOTALS(0, 0, 0) + EP.acumDctosTrx%)
				If PPP.points% = -1 Then Begin
					Call PPP.changeStatus(-3)
				Endif
				Goto obtenerBeneficiosTransaccion.start
			Endif
		Endif
		!
		If Len(PPP.errorMsg$) > 0 And PPP.errorMsg$ <> "null" Then Begin
			Call EP.DISPLAY.AN.ERROR(PPP.errorMsg$)
			Call PPP.resetVariables
		Endif Else If PPP.points% = -1 Or PPP.points% = -3 Then Begin
			If PPP.suspending% Then Begin
				Call PPP.changeStatus(0)
				PPP.suspending% = 0
			Endif Else Begin
				Call PPP.confirmDiscount
				If PPP.points% > 0 Then Begin
					Call asignarVariableGlobal("CHANGE_CONSECUT", EP.NEW.TRANSNUM)
					PPP.totalBeforeDisc% = trxSign% * (TS.TOTALS(0, 0, 0) + EP.acumDctosTrx%)
					Goto obtenerBeneficiosTransaccion.start
				Endif
			Endif
		Endif Else If PPP.points% = -4 Then Begin
			Call PPP.saveUDs
		Endif
		!
		tmpTotalDiscount% = DISC.getJavaConfirmDiscAmount
		If DISC.confirmChange% Then Begin
			Call printConfirmDiscounts
			DISC.confirmChange% = 0
		Endif
		DISC.totalDiscount% = tmpTotalDiscount%
	!Endif
END SUB
!
SUB EP.otorgarBeneficiosMedioPago
	String montoTrx$,tmpAnswer$
	Integer*4 montoTrxInt%
	!
	If EP.scapMedioPagoDMP$ = "" Then Begin
		!
		!-------------------------------------------------------------------------
		! 2021-10-28 jsv
		! En el total de transaccion no se tiene en cuenta la exencion de iva
		!-------------------------------------------------------------------------
		!montoTrxInt% = trxSign% * TS.TOTALS(0,0,0)
		montoTrxInt% = trxSign% * TS.TOTALS(0,0,0) + DISC.totalDiscount%
		!-------------------------------------------------------------------------
		!
		If montoTrxInt% < 0 Then \
			montoTrx$ = "-" + Right$(String$(9,"0") + Str$(Abs(montoTrxInt%)),9) \
		Else \
			montoTrx$ = Right$(String$(10,"0") + Str$(montoTrxInt%),10)	
		!
		tmpAnswer$ = EP.invocarApplManager("02","01",montoTrx$,0)
		If Len(tmpAnswer$) > 28 Then \
			tmpAnswer$ = Right$(tmpAnswer$, Len(tmpAnswer$) - 28) \
		Else \
			tmpAnswer$ = ""
		!
		If Len(tmpAnswer$) >= 32 Then Begin
			EP.scapValorPagoDMP% = Int%(Val(Mid$(tmpAnswer$, 1,10)))
			EP.scapValorDctoDMP% = Int%(Val(Mid$(tmpAnswer$,11,10)))
			EP.scapValorTeoricoDMP% = Int%(Val(Mid$(tmpAnswer$,21,10)))
			EP.scapMedioPagoDMP$ = Mid$(tmpAnswer$,31,2)
			If Len(tmpAnswer$) > 32 Then Begin
				EP.scapInstitucionDMP$ = Mid$(tmpAnswer$,33,5)
				If Len(tmpAnswer$) > 37 Then Begin
					EP.scapAcctDMP$ = Mid$(tmpAnswer$,38,Len(tmpAnswer$) - 37)
				Endif Else \
					EP.scapAcctDMP$ = ""
			Endif Else Begin
				EP.scapInstitucionDMP$ = "0"
				EP.scapAcctDMP$ = ""
			Endif
			Call acumularScapDataDMP
			CALL EP.procesarBeneficios
		Endif
	Endif
END SUB
!
Function EP.validarBeneficiosMedioPago(pTipo$, pVariedad$, pInstitucion$, pAccount$)
	Integer*1 EP.validarBeneficiosMedioPago
	String pTipo$, pVariedad$, pInstitucion$, pAccount$, tmpAnswer$
	String montoTrx$
	Integer*4 montoTrxInt%
	!
	If EP.scap.temporaryInactive% Then Begin
		EP.validarBeneficiosMedioPago = -1
	Endif Else Begin
		pInstitucion$ = Right$(String$(5,"0") + pInstitucion$,5)
		pAccount$ = Left$(pAccount$ + String$(18,"0"),18)
		!
		!-------------------------------------------------------------------------
		! 2021-10-28 jsv
		! En el total de transaccion no se tiene en cuenta la exencion de iva
		!-------------------------------------------------------------------------
		!montoTrxInt% = trxSign% * TS.TOTALS(0,0,0)
		montoTrxInt% = trxSign% * TS.TOTALS(0,0,0) + DISC.totalDiscount%
		!-------------------------------------------------------------------------
		!
		If montoTrxInt% < 0 Then \
			montoTrx$ = "-" + Right$(String$(9,"0") + Str$(Abs(montoTrxInt%)),9) \
		Else \
			montoTrx$ = Right$(String$(10,"0") + Str$(montoTrxInt%),10)	
		!
		tmpAnswer$ = EP.invocarApplManager("02","01",montoTrx$ + pTipo$ + pVariedad$ + pInstitucion$ + pAccount$,0)
		If Len(tmpAnswer$) > 28 Then \
			tmpAnswer$ = Mid$(tmpAnswer$, 29, 2) \
		Else \
			tmpAnswer$ = ""
		!
		If tmpAnswer$ = "XX" Then \
			EP.validarBeneficiosMedioPago = 0 \
		Else \
			EP.validarBeneficiosMedioPago = -1
	Endif
End Function
!
Sub EP.notificarMedioPagoScap(indiceTipo%,indiceVariedad%,valorInstitucion%,valorPagado%, pAccountNo$, pRealValor%)
	Integer*1 indiceTipo%,indiceVariedad%
	Integer*2 valorInstitucion%
	Integer*4 valorPagado%, pRealValor%
	String parteVariable$,valorPagadoStr$, pAccountNo$
	!
	If valorPagado% < 0 Then \
		valorPagadoStr$ = "-" \
	Else \
		valorPagadoStr$ = "+"
	valorPagadoStr$ = valorPagadoStr$ + Right$(String$(10,"0") + Str$(Abs(valorPagado%)),10)
	!
	parteVariable$ = \
		valorPagadoStr$ + \
		Str$(indiceTipo%) + \
		Str$(indiceVariedad%) + \
		Right$(String$(5,"0") + Str$(valorInstitucion%),5) + \
		Left$(pAccountNo$ + String$(18,"0"),18) + \
		Right$(String$(10,"0") + Str$(Abs(pRealValor%)),10)
	!
	Call EP.invocarApplManager("02","13",parteVariable$,0)
End Sub
!
Sub disc.voidDiscount
	Integer*4 expectedAmt%,realAmt%
	String expectedTV$,expectedInst$, accountNo$
	!
	Call removerScapDataDMP
	expectedTV$ = EP.scapMedioPagoDMP$
	expectedInst$ = EP.scapInstitucionDMP$
	!expectedAmt% = EP.scapValorPagoDMP% + EP.scapValorDctoDMP%
	expectedAmt% = EP.scapValorTeoricoDMP%
	realAmt% = EP.scapValorPagoDMP%
	If Len(EP.scapAcctDMP$) > 0 Then \
		accountNo$ = Left$(EP.scapAcctDMP$,18) \
	Else \
		accountNo$ = ""
	!
	! Respalda la información en variables AnulDMP para la grabación del Userdata
	EP.scapMedioPagoAnulDMP$ = EP.scapMedioPagoDMP$
	EP.scapInstitucionAnulDMP$ = EP.scapInstitucionDMP$
	EP.scapAcctAnulDMP$ = EP.scapAcctDMP$
	EP.scapValorPagoAnulDMP% = EP.scapValorPagoDMP%
	EP.scapValorDctoAnulDMP% = EP.scapValorDctoDMP%
	EP.scapValorTeoricoAnulDMP% = EP.scapValorTeoricoDMP%
	!
	EP.scapMedioPagoDMP$ = ""
	EP.scapInstitucionDMP$ = ""
	EP.scapValorPagoDMP% = 0
	EP.scapValorDctoDMP% = 0
	EP.scapValorTeoricoDMP% = 0
	EP.scapAcctDMP$ = ""
	Call EP.notificarMedioPagoScap(Int%(Val(Left$(expectedTV$,1))),Int%(Val(Mid$(expectedTV$,2,1))),Int%(Val(expectedInst$)),-expectedAmt%, accountNo$, realAmt%)
	CALL EP.obtenerBeneficiosTransaccion("0")	
End Sub
!
Sub disc.voidDiscountExt Public
	Call disc.voidDiscount
	EP.scapExternalInvoke% = -1
End Sub
!
Sub acumulaMedioPago(indiceTipo%,indiceVariedad%,valorInstitucion%,valorPagado%, pAccountNo$)
	Integer*1 indiceTipo%,indiceVariedad%,indiceInstitucion%
	Integer*2 valorInstitucion%
	Integer*4 valorPagado%, valorPagadoRestante%
	String pAccountNo$
	!
	!Call ep.display.an.error(str$(indiceTipo%)+"/"+str$(indiceVariedad%)+"/"+str$(valorInstitucion%)+"/"+pAccountNo$)
	If trxSign% = 1 And montosMP%(0,0,0) + valorPagado% > TS.TOTALS(0,0,0) Then \
		 valorPagado% = TS.TOTALS(0,0,0) - montosMP%(0,0,0)
	montosMP%(0,0,0) = montosMP%(0,0,0) + valorPagado%
	!
	indiceInstitucion% = getInstitucionIndex(valorInstitucion%)
	!
	!call ep.display.an.error("valorPagado%="+str$(valorPagado%))
	!call ep.display.an.error("indiceTipo%="+str$(indiceTipo%))
	!call ep.display.an.error("indiceVariedad%="+str$(indiceVariedad%))
	!call ep.display.an.error("valorInstitucion%="+str$(valorInstitucion%))
	!call ep.display.an.error("indiceInstitucion%="+str$(indiceInstitucion%))
	!
	If indiceInstitucion% > -1 Then \
	Begin
		!call ep.display.an.error("monto antes="+str$(montosMP%(indiceTipo%,indiceVariedad%,indiceInstitucion%)))
		montosMP%(indiceTipo%,indiceVariedad%,indiceInstitucion%) = montosMP%(indiceTipo%,indiceVariedad%,indiceInstitucion%) + valorPagado%
		!call ep.display.an.error("monto despues="+str$(montosMP%(indiceTipo%,indiceVariedad%,indiceInstitucion%)))
	Endif
	!
	If EP.scapMedioPagoDMP$ <> "" Then Begin
		EP.scapMedioPagoDMP$ = ""
		EP.scapInstitucionDMP$ = ""
		EP.scapValorPagoDMP% = 0
		EP.scapValorDctoDMP% = 0
		EP.scapValorTeoricoDMP% = 0
		EP.scapAcctDMP$ = ""
	Endif Else Begin
		If valorPagado% < 0 And trxSign% = 1 Then Begin
			! Se trata de una anulación
			If recuperarScapDataDMP(valorPagado%, Str$(indiceTipo%) + Str$(indiceVariedad%), valorInstitucion%) Then Begin
				Call disc.voidDiscount
			Endif Else \
				Call EP.notificarMedioPagoScap(indiceTipo%,indiceVariedad%,valorInstitucion%,valorPagado%, pAccountNo$,valorPagado%)
		Endif Else \
			Call EP.notificarMedioPagoScap(indiceTipo%,indiceVariedad%,valorInstitucion%,valorPagado%, pAccountNo$,valorPagado%)
	Endif
	If 	\
			valorPagado% < 0 And trxSign% = -1 And \ 
			valorPagado% <> TS.BALDUE(0) And \ 
			SL.TE.TENDTYPE * 10 + SL.TE.TENDVAR <> 11 Then Begin
		valorPagadoRestante% = -TS.BALDUE(0)
		Call EP.notificarMedioPagoScap(1, 1, 0, valorPagadoRestante%, "", valorPagadoRestante%)
	Endif
	!
	If EP.scapExternalInvoke% Then Begin
		Call EP.obtenerBeneficiosTransaccion("0")
		EP.scapExternalInvoke% = 0
	Endif
	!
	!
	!If TS.BALDUE(0) <> 0 Then Begin
	!	! Beneficios por medio de pago
	!	CALL EP.otorgarBeneficiosMedioPago
	!Endif
End Sub
!
SUB EP.procesarLineaBono(lineaBono$)
	String promocion$,regla$,lineaBono$,beneficiario$,strValor$,strSigno$,tarifaIva$,dpto$,strPeso$,strBase$,\
		proveedor$,grupoDescuento$,segmento$,tipoRegla$,textoLinea$,prefijo4610$,prefijo4610Reset$(1)
	Integer*4 valorDescuento%,baseDescuento%
	Integer*2 numeroLinea%,prevLineType%,prevLineData%,formatoLinea%
	Integer*1 corte%,resetCounter%
	!
	ON ERROR GOTO errorProcesarLineaBono
	DIM prefijo4610Reset$(5)
	!
	IF LEN(lineaBono$) >= 57 THEN \
	BEGIN
		beneficiario$ = STRING$(12,"0")
		tarifaIva$ = STRING$(4,"0")
		dpto$ = STRING$(4,"0")
		strPeso$ = STRING$(6,"0")
		!
		numeroLinea% = INT%(VAL(MID$(lineaBono$,1,3)))
		!corte% = INT%(VAL(MID$(lineaBono$,4,1)))
		corte% = 0
		textoLinea$ = MID$(lineaBono$,5,38)
		formatoLinea% = INT%(VAL(MID$(lineaBono$,43,5)))
		promocion$ = MID$(lineaBono$,48,10)
		regla$ = MID$(lineaBono$,58,4)
		IF LEN(lineaBono$) >= 62 THEN \
			valorDescuento% = INT%(VAL(MID$(lineaBono$,62,10))) \
		ELSE \
			valorDescuento% = 0
		strValor$ = RIGHT$(STRING$(10,"0") + STR$(valorDescuento%),10)
		IF LEN(lineaBono$) >= 72 THEN \
			baseDescuento% = INT%(VAL(MID$(lineaBono$,72,10))) \
		ELSE \
			baseDescuento% = 0
		strBase$ = RIGHT$(STRING$(10,"0") + STR$(baseDescuento%),10)
		IF LEN(lineaBono$) >= 82 THEN \
			proveedor$ = MID$(lineaBono$,82,15) \
		ELSE \
			proveedor$ = STRING$(15,"0")
		IF LEN(lineaBono$) >= 97 THEN \
			grupoDescuento$ = MID$(lineaBono$,97,4) \
		ELSE \
			grupoDescuento$ = "7777"
		IF LEN(lineaBono$) >= 101 THEN \
			segmento$ = MID$(lineaBono$,101,5) \
		ELSE \
			segmento$ = STRING$(5,"0")
		IF LEN(lineaBono$) >= 106 THEN \
			tipoRegla$ = MID$(lineaBono$,106,2) \
		ELSE \
			tipoRegla$ = STRING$(2,"0")
		!
		IF numeroLinea% = 0 THEN \
			CALL EP.registrarUserData(promocion$,regla$,beneficiario$,grupoDescuento$,tipoRegla$,strValor$,strSigno$,tarifaIva$,	 \!
				dpto$,proveedor$,segmento$,strBase$,strPeso$,"00",EP.scapBono.function$)
		!
		! Impresion de la linea
		TO.USEREXIT(20) = 0
		TO.USEREXIT(60) = 0
		IF numeroLinea% = 0 Then Begin
			If corte% = 1 Then Begin
				prevLineType% = TS.LINETYPE
				prevLineData% = TS.LINEDATA
				TS.LINETYPE = 18 
				TS.LINEDATA = 0
				Call TSPREC01
				TS.LINETYPE = prevLineType%
				TS.LINEDATA = prevLineData%
			Endif Else Begin
				Call EP.LINE.PRINT(String$(38,"="),4100H)
			Endif
		Endif
		IF NOT EP.scapBonosCorte% THEN \
		BEGIN
			EP.scapBonosCorte% = -1
			EP.prevTrailer1$ = TO.TRAILERLINE1$
			EP.prevTrailer2$ = TO.TRAILERLINE2$
			If corte% = 1 Then Begin
				TO.TRAILERLINE1$ = "SCAP LCRS "
				TO.TRAILERLINE1$ = STRING$((38 - LEN(TO.TRAILERLINE1$))/2," ") + TO.TRAILERLINE1$
				TO.TRAILERLINE2$ = "***"
				TO.TRAILERLINE2$ = STRING$((38 - LEN(TO.TRAILERLINE2$))/2," ") + TO.TRAILERLINE2$
			Endif
		ENDIF
		prefijo4610$ = ""
		IF PRT4610.ENABLE THEN \
		BEGIN
			IF formatoLinea% AND 1 THEN			\! Código de barras
			BEGIN
				!
			ENDIF \
			ELSE IF formatoLinea% AND 2 THEN	\! Logo
			BEGIN
				!
			ENDIF ELSE 							\! Texto
			BEGIN
				textoLinea$ = replaceFields(textoLinea$)
				! Quita los espacios a la derecha
				textoLinea$ = rRemove$(textoLinea$,32)
				!
				IF formatoLinea% AND 4 THEN		\! Doble alto
				BEGIN
					prefijo4610$ = prefijo4610$ + CHR$(27) + "h" + CHR$(1)
					prefijo4610Reset$(1) = CHR$(27) + "h" + CHR$(0)
				ENDIF
				!
				IF formatoLinea% AND 8 THEN		\! Doble ancho
				BEGIN
					prefijo4610$ = prefijo4610$ + CHR$(27) + "W" + CHR$(1)
					prefijo4610Reset$(2) = CHR$(27) + "W" + CHR$(0)
				ENDIF
				!
				IF formatoLinea% AND 16 THEN	\! Negrilla
				BEGIN
					prefijo4610$ = prefijo4610$ + CHR$(27) + "G" + CHR$(1)
					prefijo4610Reset$(3) = CHR$(27) + "G" + CHR$(0)
				ENDIF
				!
				IF formatoLinea% AND 32 THEN	\! Alineacion centro
				BEGIN
					prefijo4610$ = prefijo4610$ + CHR$(27) + "a" + CHR$(1)
					prefijo4610Reset$(4) = CHR$(27) + "a" + CHR$(0)
				ENDIF ELSE \
				IF formatoLinea% AND 64 THEN	\! Alineacion derecha
				BEGIN
					prefijo4610$ = prefijo4610$ + CHR$(27) + "a" + CHR$(2)
					prefijo4610Reset$(4) = CHR$(27) + "a" + CHR$(0)
				ENDIF
				!
			ENDIF
		ENDIF
		CALL EP.LINE.PRINT(prefijo4610$ + LEFT$(textoLinea$,38),4100H)
		FOR resetCounter% = 1 TO 5
			IF prefijo4610Reset$(resetCounter%) <> "" THEN \
				WRITE #34; prefijo4610Reset$(resetCounter%)
		NEXT resetCounter%
		!
		TO.USEREXIT(20) = -1
		TO.USEREXIT(60) = -1	
	ENDIF
	goto finProcesarLineaBono
	errorProcesarLineaBono:
		resume finProcesarLineaBono
	finProcesarLineaBono:
END SUB
!
SUB EP.procesarBonos
	String lineaBono$
	Integer*1 procesoBonos%
	Integer*2 counter%
	!
	!Call printDebug("procesarBonos")
	!Call printDebug("buf="+TS.PRTBUF$)
	!Call printDebug("trl="+TO.TRAILERLINE1$)
	counter% = 0
	procesoBonos% = -1
	!
	WHILE procesoBonos%
		lineaBono$ = scap.bonos$(counter%)
		Call printDebug("[" + lineaBono$ + "]")
		IF lineaBono$ = "" THEN \
			procesoBonos% = 0 \
		ELSE \
		BEGIN
			CALL EP.procesarLineaBono(lineaBono$)
			counter% = counter% + 1
		ENDIF
	WEND
	If counter% > 0 Then \
		Call EP.LINE.PRINT(String$(38,"="),4100H)
	Call resetBonusLines
END SUB
!
SUB EP.actualizarCliente
	String datosCliente$,codigoRespuesta$
	! Por ahora se desactiva la parte de 
	! actualizar cliente en linea por retail switch
	! La idea es que esto sea parametrizable
	ON ERROR GOTO errorActualizarCliente
	datosCliente$ = EP.invocarApplManager("02","16","",-1)
	IF LEN(datosCliente$) >= 58 THEN \
	BEGIN
		gv.acumPuntosTrx% = INT%(VAL(MID$(datosCliente$,29,10)))
		gv.acumPuntosRedimTrx% = INT%(VAL(MID$(datosCliente$,39,10)))
		gv.bonifPuntosTrx% = INT%(VAL(MID$(datosCliente$,49,10)))
		EP.datosClienteScap% = EP.datosClienteScap% + 2
	ENDIF
	Goto endActualizarCliente
	errorActualizarCliente:
		Resume endActualizarCliente
	endActualizarCliente:
END SUB
!
Function getHeaderUserFieldBit
	Integer*4 getHeaderUserFieldBit
	Integer*2 tmpIndex%
	!
	If FE.active% = 1 Then Begin
		tmpIndex% = Int%( Val(getProperty("FE_HEADER_USERFIELD_BIT", FE.paramContent$) ))
	Endif Else If FE.active% > 1 Then Begin
		tmpIndex% = Int%( Val(getProperty("FE_HEADER_USERFIELD_BIT" + Str$(FE.active%), FE.paramContent$) ))
	Endif Else Begin
		tmpIndex% = -1
	Endif
	!
	If tmpIndex% < 22 Or tmpIndex% > 26 Then Begin
		getHeaderUserFieldBit = -1
	Endif Else Begin
		getHeaderUserFieldBit = 2 ^ tmpIndex%
	Endif
End Function
!
Sub markHeaderForFE Public
	Integer*4 tmpBit%
	!
	Call printDebug("markHeaderForFE FE.minAmount=" + Str$(FE.minAmount%) + \
			" MEX.VAT.TOTAL(0)=" + Str$(MEX.VAT.TOTAL(0)) + \
			" NFF.EXCLU=" + Str$(NFF.EXCLU%))
	If FE.minAmount% > 0 And (MEX.VAT.TOTAL(0) + NFF.EXCLU%) >= FE.minAmount% And FE.exPlu% = 0 And FE.active% < 1 Then Begin
		FE.active% = 1
	Endif
	!
	tmpBit% = getHeaderUserFieldBit
	!
	If tmpBit% > 0 Then Begin 	! Se ha definido un bit de usuario
		If 																												\!
				Not (SL.HD.INDICAT1 And tmpBit%) 											\! No se ha marcado header
		Then Begin
			!Call printDebug("markHeaderForFE.indicat1.0=" + Str$(SL.HD.INDICAT1))
			SL.HD.INDICAT1 = SL.HD.INDICAT1 Xor tmpBit%
			!Call printDebug("markHeaderForFE.indicat1.1=" + Str$(SL.HD.INDICAT1))
		Endif
	Endif
End Sub
!
Sub updateRedemptionStrings
	Integer*2 tmpCounter%, tmpIndex%, tmpIndex1%, tmpIndex0%
	String tmpField$
	!
	For tmpCounter% = 1 To SL.END
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 99H Then Begin
				tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
				If tmpIndex% > 0 Then Begin
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
					If Unpack$(tmpField$) = "20221201" Then Begin
						tmpIndex0% = tmpIndex%
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 2 Código de respuesta
						If tmpField$ = "00" Then Begin
							tmpIndex1% = Match(":", SL.STR$(tmpCounter%), tmpIndex0% + 1)
							If tmpIndex1% > 0 Then Begin
								SL.STR$(tmpCounter%) = \
										Left$(SL.STR$(tmpCounter%), tmpIndex0%) + "VD" + \
										Right$(SL.STR$(tmpCounter%), Len(SL.STR$(tmpCounter%)) - tmpIndex1% + 1)
							Endif Else Begin
								SL.STR$(tmpCounter%) = \
										Left$(SL.STR$(tmpCounter%), tmpIndex0%) + "VD"
							Endif
						Endif
					Endif
				Endif
			Endif
		Endif
	Next tmpCounter%
	!
End Sub
!
Sub checkRedeemedPoints(pFlag$)
	String pFlag$
	Integer*2 tmpCount%, tmpCounter%
	!
	Call asignarVariableGlobal("ACTUAL_REEDEMED_COUNT", "0" )
	Call EP.invocarApplManager("02", "26", pFlag$, -1)
	!
	tmpCount% = Int%(Val( consultarVariableGlobal("ACTUAL_REEDEMED_COUNT") )) - 1
	For tmpCounter% = 1 To tmpCount%
		Call EP.NEW.TRANSNUM
	Next tmpCounter%
End Sub
!
SUB EP.notificarFinTransaccion
	String flagAnulacion$
	Integer*4 tmpConfirmDiscAmt%
	!
	Call printDebug("notificarFinTransaccion")
	!
	If (TS.IO.KEYS(1) = 70 Or TS.IO.KEYS(1) = 82) AND ts.io.keys(6) = 81 Then \ ! Anulado total o Suspender total
		flagAnulacion$ = "1" \
	Else \
		flagAnulacion$ = "0"
	!
	If TS.IO.KEYS(1) <> 82 Then Begin
		Call checkRedeemedPoints(flagAnulacion$)
	Endif
	!
	CALL EP.invocarApplManager("02","10",flagAnulacion$,0)
	If flagAnulacion$ = "0" Then Begin
		!Call finalizeConfirmDiscount
		!
		!CALL EP.procesarBonos
		!
		Call saveBonusLines
		!
		Call finalizeConfirmDiscount(tmpConfirmDiscAmt%)
		!
		!Call markHeaderForFE
		!
		Call confirmCustomerCouponDiscounts
		!
		CALL EP.actualizarCliente
		!
		!If tmpConfirmDiscAmt% <> 0 Then Begin
		!	Call DISC.printOptionalLines
		!Endif
	Endif
	!
	If PPP.points% = -2 Then Begin
		!If TS.IO.KEYS(6) = 81 And TS.IO.KEYS(1) = 82 Then Begin
		!	Call PPP.saveUDs
		!Endif
		!
		Call PPP.notifyEOT(flagAnulacion$)
	Endif
END SUB
!
SUB EP.notificarMediosPago
	CALL EP.obtenerBeneficiosTransaccion("1")
END SUB
!
Sub rechazarSecuenciaMP(pMsg$)
	String pMsg$,anularDcto$
	!
	TO.USEREXIT(14) = 0
	Call EP.GET.KBDATA(Left$(pMsg$ + String$(20," "),20) + "PLU  =Anula descto", "0", "0", anularDcto$)
	TO.USEREXIT(14) = -1
	Dim TS.IO.KEYS(10)
	Dim TS.IO.DATA$(10)
	TS.IO.MOTORKEY = 0
	If anularDcto$ = "0" Then Begin
		Call disc.voidDiscount
	Endif
End Sub
!
Sub UESCAP.DMP Public
	If EP.scapPendingDMP% = 3 And EP.scapRecupInProgress% = 0 Then Begin
		EP.scapPendingDMP% = 0
		CALL EP.otorgarBeneficiosMedioPago
	Endif
End Sub
!
Function validateAccountNumber(pAccount$, pAccountRef$)
	Integer*1 validateAccountNumber, tmpFound%
	Integer*2 tmpFiller%, tmpCounter%, tmpCount%, tmpStep%
	String pAccount$, pAccountRef$, tmpIni$, tmpEnd$
	!
	If pAccountRef$ <> "" Then Begin
		tmpFiller% = 18
		tmpStep% = tmpFiller% * 2
		pAccount$ = Left$(pAccount$ + String$(tmpFiller%,"0"),tmpFiller%)
		tmpCount% = Len(pAccountRef$)
		tmpFound% = 0
		tmpCounter% = 1
		While tmpFound% = 0 And tmpCounter% < tmpCount%
			tmpIni$ = Mid$(pAccountRef$, tmpCounter%, 18)
			tmpEnd$ = Mid$(pAccountRef$, tmpCounter% + 18, 18)
			If pAccount$ >= tmpIni$ And pAccount$ <= tmpEnd$ Then \
				tmpFound% = -1 \
			Else \
				tmpCounter% = tmpCounter% + tmpStep%
		Wend
		validateAccountNumber = tmpFound%
	Endif Else Begin
		validateAccountNumber = -1
	Endif
End Function
!
!---------------------------------------------------------------------------------------------------------
! Valida el monto del pago contra un monto eventualmente comprometido por la aplicación de descuentos
!---------------------------------------------------------------------------------------------------------
Function disc.validateAmount Public
	Integer*1 disc.validateAmount
	Integer*4 tenderAmtExpected%, tenderAmt%
	!
	If EP.scapMedioPagoDMP$ = "" Then Begin
		disc.validateAmount = -1
	Endif Else Begin
		tenderAmtExpected% = trxSign% * EP.scapValorPagoDMP%
		tenderAmt% = SL.TE.AMTTENDE
		If tenderAmt% <> tenderAmtExpected% And (tenderAmt% < TS.BALDUE(0) Or (TS.BALDUE(0) - tenderAmtExpected%) > 1) Then Begin
			disc.validateAmount = 0
		Endif Else Begin
			disc.validateAmount = -1
		Endif
	Endif
End Function
!
!---------------------------------------------------------------------------------------------------------
! Valida un medio de pago contra un descuento otorgado y
! puede retornar los siguientes valores:
! -1 : No se otorgaron descuentos por medio de pago, y el medio de pago tenía derecho a descuentos -> Rechazo
!  0 : El medio de pago no corresponde al descuento otorgado -> Rechazo
!  1 : El medio de pago corresponde al descuento otorgado -> OK
!  2 : No se otorgaron descuentos por medio de pago, y el medio de pago no tiene derecho a descuentos -> OK
!
!---------------------------------------------------------------------------------------------------------
Function disc.validateTender(pAmount%, pTenderType$, pEntity%, pAccount$) Public
	Integer*4 pAmount%
	Integer*1 disc.validateTender, tmpAnswer%
	String pTenderType$, pAccount$
	Integer*2 pEntity%
	!
	Call printDebug("disc.validateTender " + Str$(pAmount%) + "/" + pTenderType$ + "/" + Str$(pEntity%) + "/" + pAccount$)
	Call printDebug("EP.scapMedioPagoDMP$=*"+EP.scapMedioPagoDMP$+"*")
	!
	! Si el valor es contrario al signo de la transacción, no valida dctos
	If pAmount% < 0 And trxSign% = 1 Then Begin
		tmpAnswer% = 1
	Endif Else Begin
		If EP.scapMedioPagoDMP$ = "" Then Begin
			! Hay que chequear si el medio de pago es acreedor a descuentos
			! Si es así, es necesario rechazar el pago
			If Not EP.validarBeneficiosMedioPago(Left$(pTenderType$, 1), Mid$(pTenderType$, 2, 1), Str$(pEntity%), pAccount$) Then \
				tmpAnswer% = -1 \
			Else \
				tmpAnswer% = 2
		Endif Else Begin
			If \
					EP.scapMedioPagoDMP$ <> pTenderType$ Or \
					Int%(Val(EP.scapInstitucionDMP$)) <> 0 And pEntity% <> Int%(Val(EP.scapInstitucionDMP$)) Then Begin
				tmpAnswer% = 0
			Endif Else If validateAccountNumber(pAccount$, EP.scapAcctDMP$) = 0 Then Begin
				tmpAnswer% = 0
			Endif Else Begin
				tmpAnswer% = 1
			Endif
		Endif
	Endif
	!
	Call printDebug("answer = " + Str$(tmpAnswer%))
	disc.validateTender = tmpAnswer%
End Function
!
Sub disc.ignoreAccount Public
	EP.scapIgnoreAccount% = -1
End Sub
!
Sub disc.temporaryInactive Public
	EP.scap.temporaryInactive% = -1
End Sub
!
Function disc.getDMPTender Public
	String disc.getDMPTender
	!
	disc.getDMPTender = EP.scapMedioPagoDMP$
End Function
!
Function disc.getDMPEntity Public
	String disc.getDMPEntity
	!
	disc.getDMPEntity = EP.scapInstitucionDMP$
End Function
!
Sub paramDiaSinIva2021(pData$)
	String pData$
	Integer*2 tmpCounter%, tmpIndex%
	!
	DISC.confirmActivIn$ = getProperty("CONFIRM_ACTIVATION_IN", pData$)
	DISC.confirmActivEx$ = getProperty("CONFIRM_ACTIVATION_EX", pData$)
	DISC.isConfirmDiscActive% = isIncluded(Val(  TS.TERMINAL$ ), DISC.confirmActivIn$, DISC.confirmActivEx$)
	DISC.confirmRemissionTxt$ = getProperty("CONFIRM_REMISSION_TEXT", pData$)
	tmpIndex% = Int%( Val(getProperty("CONFIRM_HEADER_USERFIELD_BIT", pData$) ))
	If tmpIndex% < 22 Or tmpIndex% > 26 Then Begin
		DISC.headerUserFieldBit% = -1
	Endif Else Begin
		DISC.headerUserFieldBit% = 2 ^ tmpIndex%
	Endif
	DISC.optionalLineCnt% = Int%( Val(getProperty("CONFIRM_OPCIONAL_LINES_COUNT", pData$) ))
	If DISC.optionalLineCnt% > 0 Then Begin
		Dim DISC.optionalLines$( DISC.optionalLineCnt% )
		For tmpCounter% = 1 To DISC.optionalLineCnt%
			DISC.optionalLines$( tmpCounter% ) = getProperty("CONFIRM_OPTIONAL_LINE_" + Str$( tmpCounter% ), pData$)
		Next tmpCounter%
	Endif Else Begin
		Dim DISC.optionalLines$(0)
	Endif
End Sub
!
Sub CONTING.forceTransactionVoid Public
	! Activa flag para forzar anulación
	Dim TS.IO.KEYS(10)
	Dim TS.IO.DATA$(10)
	TS.IO.DEVICE = 1
	FE.forceVoid% = -1
	! Solicita registro de artículo dummy
	TS.IO.DATA$(2) = FE.dummyItem$
	TS.IO.KEYS(2) = 80
	TS.IO.MOTORKEY = 80
End Sub
!
Sub resetContingActive
	! Valor por default = -1 que significa pendiente por asignación
	FE.active% = -1
	FE.preference% = -1
	!
	FE.forceVoid% = 0
	FE.exPlu% = 0
End Sub
!
Function isFEExPlu(pCode$)
	Integer*1 isFEExPlu, tmpAnswer%
	String pCode$, tmpCode$
	!
	tmpAnswer% = 0
	On Error Goto isFEExPlu.err
	!
	tmpCode$ = "," + Str$(Val(pCode$)) + ","
	If Match(tmpCode$, FE.exPlus$, 1) > 0 Then Begin
		tmpAnswer% = -1
	Endif
	!
	Goto isFEExPlu.end
	!
	isFEExPlu.err:
	Resume isFEExPlu.end
	!
	isFEExPlu.end:
	Call printDebug("isFEExPlu(" + pCode$ + ")=" + Str$(tmpAnswer%))
	isFEExPlu = tmpAnswer%
End Function
!
Sub paramContingency(pData$)
	String pData$
	!
	FE.exPlus$ = "," + getProperty("FE_EX_PLUS", pData$) + ","
	FE.userRequest% = getBooleanProperty("FE_USER_REQUEST", pData$)
	FE.skipEquivDoc% = getBooleanProperty("FE_SKIP_EQUIV_DOC", pData$)
	FE.minAmount% = getIntProperty("FE_MIN_AMOUNT", pData$, 0, 999999999)
	FE.requestMessage$ = getProperty("FE_REQ_MESSAGE", pData$)
	FE.forceVoidMsg$ = getProperty("FE_FORCE_VOID_MESSAGE", pData$)
	FE.forceVoidMsg$ = Left$(FE.forceVoidMsg$ + String$(40, " "), 40)
	FE.requestDeKey$ = getProperty("FE_REQUEST_DE_KEY", pData$)
	FE.dummyItem$ = getProperty("FE_DUMMY_ITEM", pData$)
	FE.emailWarning$ = getProperty("FE_NO_EMAIL_WARNING", pData$)
	!
	CONTING.active% = getBooleanProperty("CONTING_ACTIVE", pData$)
	If CONTING.active% Then Begin
		FE.userRequest% = 0
	Endif
	Call resetContingActive
	!
	CONTING.fDataUdKey$ = getProperty("CONTING_FDATA_UD_KEY", pData$)
End Sub
!
Sub DISC.voidConfirmDiscount(pUD$, pDiscount$, pDiscAmt%, pSign$, pDescrip$)
	String pUD$, pDiscount$, pSign$, pDescrip$, tmpNewSign$, tmpNewDiscSign$, tmpChar$, tmpNewDiscount$
	Integer*2 tmpIndex%
	Integer*4 pDiscAmt%
	!
	tmpIndex% = Match(":", pUD$, 1)
	TS.USERDATA$ = Right$(pUD$, Len(pUD$) - tmpIndex%)
	If pSign$ = "0" Then Begin
		tmpNewSign$ = "1"
		tmpNewDiscSign$ = "04"
	Endif Else Begin
		tmpNewSign$ = "0"
		tmpNewDiscSign$ = "03"
		pDiscAmt% = -pDiscAmt%
	Endif
	!
	TS.BALDUE(0) = TS.BALDUE(0) + pDiscAmt%
	SL.HD.GROSSNEG = SL.HD.GROSSNEG - pDiscAmt%
	TS.GROSSNEG    = TS.GROSSNEG - pDiscAmt%
	TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) + pDiscAmt%
	TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - pDiscAmt%
	SL.DI.AMTDISCO = Abs(pDiscAmt%)
	!
	tmpChar$ = Pack$(Left$(Unpack$(Mid$(TS.USERDATA$, 32, 1)), 1) + tmpNewSign$)
	TS.USERDATA$ = Left$(TS.USERDATA$, 31) + tmpChar$ + Right$(TS.USERDATA$, Len(TS.USERDATA$) - 32)
	TS.TEMP1I1 = 99
	Call TSTPEC01
	!
	tmpIndex% = Match(":", pDiscount$, 1)
	tmpNewDiscount$ = Right$(pDiscount$, Len(pDiscount$) - tmpIndex%)
	SL.END = SL.END + 1
	Call write.sl.str(SL.END, Pack$(tmpNewDiscSign$) + ":" + \
		tmpNewDiscount$)
	!
	Call EP.imprimirBeneficio(pDiscAmt%, "", pDescrip$)
End Sub
!
Sub DISC.voidConfirmDiscounts(pVoidFlag%)
	Integer*1 pVoidFlag%
	Integer*2 tmpCounter%, tmpIndex%, tmpEnd%
	String tmpField$, tmpCurrentUD$, tmpCurrentSign$, tmpCurrentPromo$
	Integer*4 tmpCurrentDiscAmt%
	!
	pVoidFlag% = 0
	!
	Call removeConfirmPreviousUDs
	!
	tmpCurrentUD$ = ""
	tmpCurrentSign$ = ""
	tmpCurrentDiscAmt% = 0
	tmpCurrentPromo$ = ""
	tmpEnd% = SL.END
	For tmpCounter% = 1 To tmpEnd%
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 99H Then Begin
				! User data
				tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
				Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
				If Unpack$(tmpField$) = "50000001020400000000" Then Begin
					! Scap User Data
					Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 2
					If Mid$(Unpack$(tmpField$), 31, 2) = "05" Then Begin
						! Transaction discount
						tmpCurrentUD$ = SL.STR$(tmpCounter%)
						tmpCurrentSign$ = Mid$(Unpack$(tmpField$), 42, 1)
						tmpCurrentDiscAmt% = Int%(Val(Mid$(Unpack$(tmpField$), 33, 9)))
						tmpCurrentPromo$ = Left$(Unpack$(tmpField$), 10) + "-" + Mid$(Unpack$(tmpField$), 11, 4)
					Endif
				Endif
			Endif Else If Asc(SL.STR$(tmpCounter%)) = 03H Or Asc(SL.STR$(tmpCounter%)) = 04H Then Begin
				! Discount or Discount Void
				If Len(tmpCurrentUD$) > 0 Then Begin
					If \
							(tmpCurrentSign$ = "0" And Asc(SL.STR$(tmpCounter%)) = 03H) Or \
							(tmpCurrentSign$ = "1" And Asc(SL.STR$(tmpCounter%)) = 04H) \
					Then Begin
						tmpIndex% = Match(":", SL.STR$(tmpCounter%), 1)
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 1
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 2
						Call getNextField(SL.STR$(tmpCounter%), tmpIndex%, ":", tmpField$) ! Data 3
						If Int%(Val(Unpack$(tmpField$))) = tmpCurrentDiscAmt% Then Begin
							Call DISC.voidConfirmDiscount(\
									tmpCurrentUD$, SL.STR$(tmpCounter%), tmpCurrentDiscAmt%, tmpCurrentSign$, tmpCurrentPromo$)
							pVoidFlag% = -1
						Endif
					Endif
					tmpCurrentUD$ = ""
					tmpCurrentSign$ = ""
					tmpCurrentDiscAmt% = 0
					tmpCurrentPromo$ = ""
				Endif
			Endif
		Endif
	Next tmpCounter%
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
Sub voidRedemptBenefits
	Integer*2 tmpCount%, tmpCounter%
	!
	Call asignarVariableGlobal("ACTUAL_REEDEMED_COUNT", "0" )
	Call EP.invocarApplManager("02", "26", "1", -1)
	Call EP.procesarBeneficios
	!
	tmpCount% = Int%(Val( consultarVariableGlobal("ACTUAL_REEDEMED_COUNT") )) - 1
	For tmpCounter% = 1 To tmpCount%
		Call EP.NEW.TRANSNUM
	Next tmpCounter%
End Sub
!
!----------------------------------------------------------------
! User Exits
!----------------------------------------------------------------
Sub UESCAP01 Public
	Call CONTING.requestForFE( isFEExPlu( TS.IO.DATA$(2) ) )
End Sub
!
SUB UESCAP02 PUBLIC
	Dim DISC.customerCouponRecovery$(20, 1)
	DISC.customerCouponRecoveryCnt% = 0
	!
	DISC.confirm% = -1
	DISC.confirmList$ = ""
	DISC.totalDiscount% = 0
	DISC.confirmVrTotal% = 0
	Call asignarVariableGlobal("DISC_CONFIRMATION", "-1" )
	Call asignarVariableGlobal("DISC_TOTAL", "0" )
	Call asignarVariableGlobal("redemptionPending", "false" )
	!
	Call PPP.resetVariables
	!
	EP.scap.temporaryInactive% = 0
	EP.scapExternalInvoke% = 0
	totalDesctoPago% = 0
	Call resetBonusLines
	EP.acumDctosMP% = 0
	EP.acumDctosTrx% = 0
	trxSign% = 0
	EP.scapTaxIndex% = -1
	EP.idCliente$ = ""
	EP.cedulaCliente$ = ""
	EP.lineaArticulo$ = ""
	EP.scapPendientes% = 0
	EP.scapArticuloNotificado% = 0
	EP.scapTrx% = 0
	!EP.scapRecupInProgress% = 0
	Call setScapRecoveryState(0)
	EP.datosClienteScap% = 0
	EP.datosClienteImpresion% = 0
	IF EP.scapBonosCorte% THEN \
	BEGIN
		EP.scapBonosCorte% = 0
		TO.TRAILERLINE1$ = EP.prevTrailer1$
		TO.TRAILERLINE2$ = EP.prevTrailer2$
	ENDIF
	!
	DIM gv.descto.iva%(10,1)
	!
	institutCounter% = 0
	Dim montosMP%(10,10,institutMax%)
	Dim institucionesMP%(institutMax%)
	!
	EP.scapMedioPagoDMP$ = ""
	EP.scapInstitucionDMP$ = ""
	EP.scapValorPagoDMP% = 0
	EP.scapValorDctoDMP% = 0
	EP.scapValorTeoricoDMP% = 0
	EP.scapDataDMP$ = ""
	EP.scapAcctDMP$ = ""
	EP.scapIgnoreAccount% = 0
	!
	Call resetContingActive
END SUB
!
Sub UESCAP05 Public
	Call resetContingActive
End Sub
!
SUB UESCAP07 PUBLIC
	String tmpPropData$
	Integer*2 tmpReturn%
	!
	DISC.dummyAccount$ = "123456"
	!
	Dim DISC.customerCouponRecovery$(20, 1)
	DISC.customerCouponRecoveryCnt% = 0
	!
	EP.scap.temporaryInactive% = 0
	totalDesctoPago% = 0
	!
	Call asignarVariableGlobal("STORE", Right$(String$(4, "0") + TS.STORE$ , 4))
	!
	Call asignarVariableGlobal("TERMINAL", Right$(String$(3, "0") + TS.TERMINAL$ , 3))
	!
	Call EP.refrescarValoresSistema
	!
	Call readPropData("R::C:/SCAPPOS/SCAP.PRO", tmpPropData$, EP.IOPARM%, tmpReturn%)
	DISC.confirmMessage$ = getProperty("CONFIRM_DISCOUNTS_MESSAGE", tmpPropData$)
	DISC.confirmAllowedTenders$ = getProperty("CONFIRM_ALLOWED_TENDERS", tmpPropData$) + ","
	DISC.confirmExcludedTenders$ = getProperty("CONFIRM_EXCLUDED_TENDERS", tmpPropData$) + ","
	DISC.confirmRejectTenderMessage$ = getProperty("CONFIRM_REJECT_TENDER_MSG", tmpPropData$)
	DISC.confirmRequestUdKey$ = getProperty("CONFIRM_REQUEST_UD_KEY", tmpPropData$)
	DISC.confirmItemUdKey$ = getProperty("CONFIRM_ITEM_UD_KEY", tmpPropData$)
	!
	PPP.headerUDKey$ = getProperty("PPP_HEADER_UD_KEY", tmpPropData$)
	PPP.detailUDKey$ = getProperty("PPP_DETAIL_UD_KEY", tmpPropData$)
	PPP.voidUDKey$ = getProperty("PPP_VOID_UD_KEY", tmpPropData$)
	PPP.inRecovery% = 0
	PPP.suspending% = 0
	!
	DISC.customerCouponPref$ = getProperty("CUST_COUPON_PREF", tmpPropData$)
	DISC.customerCouponPrefLen% = Len( DISC.customerCouponPref$ )
	If DISC.customerCouponPrefLen% = 0 Then Begin
		DISC.customerCouponLeng% = 0
		DISC.customerCouponUDKey1$ = ""
		DISC.customerCouponUDKey2$ = ""
		DISC.customerCouponMessage$ = ""
	Endif Else Begin
		DISC.customerCouponLeng% = Int%( Val(getProperty("CUST_COUPON_LENG", tmpPropData$) ))
		If DISC.customerCouponLeng% < DISC.customerCouponPrefLen% Then Begin
			DISC.customerCouponLeng% = DISC.customerCouponPrefLen%
		Endif
		DISC.customerCouponUDKey1$ = getProperty("CUST_COUPON_UD1_KEY", tmpPropData$)
		DISC.customerCouponUDKey2$ = getProperty("CUST_COUPON_UD2_KEY", tmpPropData$)
		DISC.customerCouponMessage$ = getProperty("CUST_COUPON_MESSAGE", tmpPropData$)
	Endif
	!
	Call paramDiaSinIva2021( tmpPropData$ )
	Call paramContingency( tmpPropData$ )
	!
	PPP.otpRetries% = getIntProperty("PPP_OTP_RETRIES", tmpPropData$, 1, 5)
	!
	FE.paramContent$ = tmpPropData$
	tmpPropData$ = ""
	!
	DISC.confirm% = -1
	Call asignarVariableGlobal("DISC_CONFIRMATION", "-1" )
	Call asignarVariableGlobal("DISC_TOTAL", "0" )
	DISC.confirmList$ = ""
	DISC.totalDiscount% = 0
	DISC.confirmVrTotal% = 0
	!
	Call PPP.resetVariables
	!
	Call resetBonusLines
	EP.acumDctosMP% = 0
	EP.acumDctosTrx% = 0
	EP.sepOtherData$ = ":"
	If terminalStartUp% <> 100 Then \
	Begin
		trxSign% = 0
		EP.scapTaxIndex% = -1
		EP.idCliente$ = ""
		EP.cedulaCliente$ = ""
		EP.lineaArticulo$ = ""
		EP.scapPendientes% = 0
		EP.scapArticuloNotificado% = 0
		EP.scapTrx% = 0
		!EP.scapRecupInProgress% = 0
		Call setScapRecoveryState(0)
		EP.datosClienteScap% = 0
		EP.datosClienteImpresion% = 0
		!
		DIM gv.descto.iva%(10,1)
		!
		institutCounter% = 0
		institutMax% = 10
		Dim montosMP%(10,10,institutMax%)
		Dim institucionesMP%(institutMax%)	
		!
		! Pendiente por parametrizar desde SCAPWeb
		EP.scapDE.key$ = "50000001"
		EP.scapDcto.function$ = "0204"
		EP.scapCupon.function$ = "0220"
		EP.scapBono.function$ = "0221"
		!
		terminalStartUp% = 100
	EndIf
	EP.scapMedioPagoDMP$ = ""
	EP.scapInstitucionDMP$ = ""
	EP.scapValorPagoDMP% = 0
	EP.scapValorDctoDMP% = 0
	EP.scapValorTeoricoDMP% = 0
	EP.scapDataDMP$ = ""
	EP.scapAcctDMP$ = ""
	EP.scapIgnoreAccount% = 0
	!
	DISC.recoverDiscounts% = 0
END SUB
!
SUB UESCAP08 PUBLIC
	If PPP.points% = -2 Then Begin
		! Se ha otorgado previamente un beneficio de cambio 
		! de puntos por pesos. Si se desea seguir ingresando o 
		! anulando artículos, es necesario anular dicho cambio
		If PPP.confirmVoid("Anula puntos x pesos") = 0 Then Begin
			Dim TS.IO.KEYS(10)
			Dim TS.IO.DATA$(10)
			TS.IO.MOTORKEY = 0
			FAST.EXIT = -1
			Exit Sub
		Endif
	Endif Else If PPP.points% = 0 Then Begin
		Call PPP.voidRedemption
	Endif
	!
	If Not EP.scap.temporaryInactive% Then Begin
		EP.scapPlu$ = ITEMCODE$
	Endif
END SUB
!
SUB UESCAP10 PUBLIC
	! verifica si hay
	! nueva versión de promociones para realizar un
	! cargue automático
	IF NOT (TS.INTRX) And isApplMgrRunning = 0 THEN \
		Call EP.cargaAutomaticaPromociones
END SUB
!
!--------------------------------------------------------------
! Si hay procesos pendientes de scap al
! momento de totalizar, es necesario
! invocar la rutina de recuperar beneficios
! hasta que no queden procesos pendientes
!--------------------------------------------------------------
SUB UESCAP14 PUBLIC
	Integer*2 triesCounter%, tmpDevice%
	String tmpCustCoupon$
	Integer*4 tmpConfirmDiscAmt%
	!
	If DISC.recoverDiscounts% Then Begin
		DISC.recoverDiscounts% = 0
		Dim TS.IO.KEYS(10)
		Dim TS.IO.DATA$(10)
		TS.IO.MOTORKEY = 81
		TS.IO.KEYS(6) = 81
		Call appl.disableMessageDisplay
	Endif
	!
	If FE.forceVoid% = -2 Then Begin
		FE.forceVoid% = 0
		Call voidTransaction
		Call appl.disableMessageDisplay
	Endif
	!
	If Not EP.scap.temporaryInactive% Then Begin
		IF TS.PROCEDURE < 1 THEN \
		BEGIN
			!
			! No debe dejar suspender si hay compromiso de pago por descuento
			If TS.IO.KEYS(6) = 81 And TS.IO.KEYS(1) = 82 Then Begin
				If EP.scapMedioPagoDMP$ <> "" Then Begin
					Call rechazarSecuenciaMP(" Susp. no permitida ")
					Exit Sub
				Endif Else If PPP.points% = -2 Then Begin
					! Se ha otorgado previamente un beneficio de cambio 
					! de puntos por pesos. Si se desea suspender, 
					! es necesario anular dicho cambio
					Dim TS.IO.KEYS(10)
					Dim TS.IO.DATA$(10)
					TS.IO.MOTORKEY = 0
					If PPP.confirmVoid("Debe anular puntos") Then Begin
						TS.IO.KEYS(6) = 81
						TS.IO.MOTORKEY = 81
						PPP.suspending% = -1
					Endif Else Begin
						Exit Sub
					Endif
				Endif Else Begin
					! En cualquier otro caso de suspensión, debe 
					! anular posibles beneficios relacionados con
					! redención de puntos
					Call voidRedemptBenefits
				Endif
				!tmpConfirmDiscAmt% = DISC.getJavaConfirmDiscAmount
				!If tmpConfirmDiscAmt% <> 0 Then Begin
				!	! Se debe anular el descuento del día sin iva
				!	! antes de suspender transacción
				!	Call DISC.voidConfirmDiscounts(DISC.confirmList$)
				!Endif
			Endif
			!
			IF TS.IO.KEYS(6) = 81 AND 	\
			(TS.INTRX) AND \
			(NOT isSecuenciaAnulacion And EP.scapMedioPagoDMP$ = "" And TS.IO.KEYS(1) <> 82) THEN	\
			BEGIN
				! Tecla Total
				CALL EP.notificarLecturaCliente
				CALL EP.obtenerDatosCliente
				triesCounter% = 0
				WHILE EP.scapPendientes% > 0 And triesCounter% < 300
					CALL EP.obtenerBeneficios("0")
					IF EP.scapPendientes% > 0 THEN \
					Begin
						triesCounter% = triesCounter% + 1
						WAIT ; 200
					EndIf
				WEND
				! Beneficios por transacción
				CALL EP.obtenerBeneficiosTransaccion("0")
				!! Beneficios por medio de pago
				!CALL EP.otorgarBeneficiosMedioPago
				EP.scapPendingDMP% = 1
			ENDIF ELSE IF TS.IO.KEYS(2) = 80 THEN \
			BEGIN
				! Lectura o digitación en la POS
				!
				If EP.scapMedioPagoDMP$ <> "" Then Begin
					Call rechazarSecuenciaMP("Solo puede pagar")
				Endif Else If \
						TS.IO.DATA$(2) <> "" And \
						TS.TENDERED(0) <> 0 And \
						isAllowPluAfterTend = 0 And \
						TS.PROCEDURE < 1 And \
						TS.INTRX <> 0 Then Begin
				! Si previamente se han introducido medios de pago
				! parciales, debe consultar parametro scap para decidir
				! si permite seguir o aborta
					Call EP.DISPLAY.AN.ERROR("debe anular descuenty/o medios de pago")
					Dim TS.IO.KEYS(10)
					Dim TS.IO.DATA$(10)
					TS.IO.MOTORKEY = 0
				Endif Else If isCustomerCoupon(TS.IO.DATA$(2)) Then Begin
					! Cupón personalizado. Requiere que se haya identificado
					! cliente en la caja
					tmpCustCoupon$ = TS.IO.DATA$(2)
					tmpDevice% = TS.IO.DEVICE
					TS.IO.DEVICE = 1
					Dim TS.IO.KEYS(10)
					Dim TS.IO.DATA$(10)
					TS.IO.MOTORKEY = 0
					If Len( TPONLINE.getCedula ) = 0 Then Begin
						Call EP.DISPLAY.AN.ERROR("SE REQUIERE INGRESO DE CLIENTE")
					Endif Else If wasCustomerCouponAlreadyRead(tmpCustCoupon$) Then Begin
						Call EP.DISPLAY.AN.ERROR("CUPON PROMOCIONAL   YA FUE INGRESADO")
					Endif Else Begin
						Call queryForCustomerCoupon(tmpCustCoupon$, TPONLINE.getCedula, tmpDevice%)
					Endif
				Endif Else If EP.isCupon(TS.IO.DATA$(2)) Then Begin
					!
					! Si se trata de un cupon SCAP
					! aborta la secuencia normal
					! para que SCAP se encargue
					! Imprime constancia de la entrada del cupón
					Call imprimirLinea("Cupon: " + TS.IO.DATA$(2),6100H)
					! Procesa posibles beneficios
					! otorgados por el cupón
					CALL EP.procesarBeneficios
					Dim TS.IO.KEYS(10)
					Dim TS.IO.DATA$(10)
					TS.IO.MOTORKEY = 0
				Endif
			Endif
		ENDIF
	Endif
END SUB
!
! Validacion de secuencia de medio de pago
!Sub UESCAP14A Public
!	String tenderType$, tenderVar$
!	Integer*4 tenderAmt%,tenderAmtExpected%
!	!
!	If TS.PROCEDURE < 1 And TS.IO.KEYS(7) >= 91 And TS.IO.KEYS(7) <= 96 Then Begin
!		! Medio de pago
!		tenderType$  = str$(ts.io.keys(7) - 90)
!		If ts.io.keys(3) = 78 Then \
!			tenderVar$ = ts.io.data$(3) \
!		Else \
!			tenderVar$ = "1"
!		tenderAmt% = Int%(Val(ts.io.data$(7)))
!		!
!		If tenderAmt% <> 0 Then Begin
!			If TS.IO.KEYS(1) = 70 And tenderAmt% > 0 Then tenderAmt% = -tenderAmt%
!			tenderAmtExpected% = trxSign% * EP.scapValorPagoDMP%
!			!
!			If EP.scapMedioPagoDMP$ <> "" Then Begin
!				If (tenderAmt% <> tenderAmtExpected% And (tenderAmt% < TS.BALDUE(0) Or tenderAmtExpected% < TS.BALDUE(0))) Or \
!						(Int%(Val(EP.scapInstitucionDMP$)) <> 0 And getInstitucion <> Int%(Val(EP.scapInstitucionDMP$))) Or \
!						EP.scapMedioPagoDMP$ <> tenderType$ + tenderVar$ Or \
!						validateAccountNumber(TS.IO.DATA$(9)) = 0 Then Begin
!					Call rechazarSecuenciaMP("No coincide pago")
!					!Call rechazarSecuenciaMP(Str$(EP.scapValorPagoDMP%)+" "+EP.scapMedioPagoDMP$+" "+\
!					!	EP.scapInstitucionDMP$ + " " + \
!					!	Str$(tenderAmt%) + " " + tenderType$ + tenderVar$ + " " + Str$(getInstitucion))
!					Exit Sub
!				Endif
!			Endif Else Begin
!				If TS.IO.KEYS(1) <> 70 And trxSign% = 1 Then Begin
!					If Not EP.validarBeneficiosMedioPago(tenderType$, tenderVar$, "00000") Then Begin
!						Call EP.DISPLAY.AN.ERROR("Puede obtener dctos por medio de pago")
!						Dim TS.IO.KEYS(10)
!						Dim TS.IO.DATA$(10)
!						TS.IO.MOTORKEY = 0
!						Call EP.otorgarBeneficiosMedioPago
!					Endif
!				Endif
!			Endif
!		Endif
!	Endif
!End Sub
!
Function getAccountNumber(pUE%)
	String getAccountNumber
	Integer*1 pUE%
	!
	If pUE% = 52 Then Begin
		getAccountNumber = TS.IO.DATA$(9)
	Endif Else If TS.IO.DATA$(10) <> "" Or TS.IO.DATA$(9) = DISC.dummyAccount$ Then Begin
		getAccountNumber = TS.IO.DATA$(10)
	Endif Else Begin
		getAccountNumber = TS.IO.DATA$(9)
	Endif
End Function
!
Function validateEventTender(pTender$, pList$)
	Integer *1 validateEventTender, tmpFound%
	String pTender$, pList$, tmpOptions$(1)
	Integer*2 tmpCounter%
	!
	pTender$ = Left$(pTender$ + "**", 2)
	Dim tmpOptions$(3)
	tmpOptions$(0) = pTender$
	tmpOptions$(1) = "**"
	tmpOptions$(2) = Left$(pTender$, 1) + "*"
	tmpOptions$(3) = "*" + Right$(pTender$, 1)
	!
	tmpFound% = 0
	tmpCounter% = 0
	!
	While tmpCounter% < 4 And tmpFound% = 0
		If Match(tmpOptions$(tmpCounter%) + ",", pList$, 1) > 0 Then Begin
			tmpFound% = -1
		Endif Else Begin
			tmpCounter% = tmpCounter% + 1
		Endif
	Wend
	!
	validateEventTender = tmpFound%
End Function
!
!------------------------------------------------------------------
! 2020-04-06 jsv
! Rutina para invocar validación medio de pago por los artículos 
! ingresados en la transacción
!------------------------------------------------------------------
Sub callTendsale(pTendType%, pTendVar%, pEntity%, pBin$, pRejectCause%, pMessage$)
	String pBin$, pMessage$, tmpTender$
	Integer*1 pRejectCause%
	Integer*2 pTendType%, pTendVar%, pEntity%, tmpReturn%
	!
	pRejectCause% = 0
	!
	! Valida si hay restricciones de medio de pago por
	! un descuento que haya requerido confirmación
	If DISC.getConfirmDiscAmount > 0 And trxSign% = 1 Then Begin
		tmpTender$ = Str$(pTendType%) + Str$(pTendVar%)
		If \
				validateEventTender(tmpTender$, DISC.confirmAllowedTenders$) = 0 Or 	\! Medio de pago NO en la lista de medios permitidos
				validateEventTender(tmpTender$, DISC.confirmExcludedTenders$) = -1 		\! Medio de pago en la lista de medios excluídos
		Then Begin
			pRejectCause% = -1
			pMessage$ = DISC.confirmRejectTenderMessage$
		Endif
	Endif
	!
	If pRejectCause% = 0 Then Begin
		tmpReturn% = 0
		Call TENDSALE.validateTender(pTendType%, pTendVar%, pEntity%, pBin$, tmpReturn%, pMessage$)
		If tmpReturn% = 0 Then Begin
			pRejectCause% = -1
		Endif
	Endif
End Sub
!
! Validacion de secuencia de medio de pago
Sub UESCAP.validateTV(pUE%) Public
	String tenderType$, tenderVar$, tenderAccount$, mensajeRechazo1$, mensajeRechazo2$
	Integer*4 tenderAmt%,tenderAmtExpected%
	Integer*1 pUE%, isTender%, causalRechazo%
	!
	If Not EP.scap.temporaryInactive% Then Begin
		causalRechazo% = 0
		If TS.PROCEDURE < 1 Then Begin
			isTender% = 0
			tenderAccount$ = getAccountNumber(pUE%)
			If waitingForTender% <> 0 And tenderAccount$ <> "" Then Begin
				waitingForTender% = 0
				isTender% = -1
				If TS.IO.KEYS(7) >= 91 And TS.IO.KEYS(7) <= 96 Then Begin
					tenderType$  = str$(ts.io.keys(7) - 90)
					If ts.io.keys(3) = 78 Then \
						tenderVar$ = ts.io.data$(3) \
					Else \
						tenderVar$ = "1"				
				Endif Else Begin
					tenderType$ = Str$(SL.TE.TENDTYPE)
					tenderVar$ = Str$(SL.TE.TENDVAR)
				Endif
				tenderAmt% = SL.TE.AMTTENDE
			Endif Else If TS.IO.KEYS(7) >= 91 And TS.IO.KEYS(7) <= 96 Then Begin
				If EP.scapPendientes% > 0 Then Begin
					TS.BAL.TAKEN = 1	!Obliga a totalizar
					Exit Sub
				Endif
				!
				waitingForTender% = 0
				If TS.IO.KEYS(1) = 70 Then \
					voidTender% = -1 \
				Else \
					voidTender% = 0
				!
				tenderType$  = str$(ts.io.keys(7) - 90)
				If ts.io.keys(3) = 78 Then \
					tenderVar$ = ts.io.data$(3) \
				Else \
					tenderVar$ = "1"
				!
				tenderAmt% = Int%(Val(ts.io.data$(7)))
				If TS.IO.KEYS(1) = 70 And tenderAmt% > 0 Then tenderAmt% = -tenderAmt%
				!
				! 2018-07-11 jsv
				! Se agrega validación para que la secuencia 0+efectivo se deje pasar
				! únicamente cuando la transacción es negativa
				If tenderAmt% <> 0 Or TS.BALDUE(0) > 0 Then Begin
					If getTenderVerifType(tenderType$ + tenderVar$) <> 0 And tenderAccount$ = "" Then Begin
						waitingForTender% = -1
						isTender% = 0
					Endif Else Begin
						isTender% = -1
					Endif
				Endif Else \
					isTender% = 0
			Endif Else Begin
				waitingForTender% = 0
				voidTender% = 0
			Endif
			!
			If isTender% Then Begin
				Call callTendsale(\
						Int%(Val( tenderType$ )), Int%(Val( tenderVar$ )), getInstitucion, Left$(tenderAccount$ + String$(6, "0"), 6), causalRechazo%, mensajeRechazo1$)
				If causalRechazo% = 0 Then Begin
					tenderAmtExpected% = trxSign% * EP.scapValorPagoDMP%
					!
					If EP.scapMedioPagoDMP$ <> "" And EP.scapIgnoreAccount% = 0 Then Begin
						!Call ep.display.an.error(str$(tenderAmt%) + "@" + tenderType$ + tenderVar$ + "@" + EP.scapInstitucionDMP$)
						!Call ep.display.an.error(str$(tenderAmtExpected%) + "@" + EP.scapMedioPagoDMP$ + "@" + Str$(getInstitucion))
						!Call ep.display.an.error("Acct:" + tenderAccount$)
						!
						If tenderAmt% <> tenderAmtExpected% And (tenderAmt% < TS.BALDUE(0) Or (TS.BALDUE(0) - tenderAmtExpected%) > 1) Then Begin
							causalRechazo% = 1
							mensajeRechazo1$ = "monto"
							mensajeRechazo2$ = Str$(tenderAmt%) + ":" + Str$(tenderAmtExpected%)
						Endif Else If EP.scapMedioPagoDMP$ <> tenderType$ + tenderVar$ Then Begin
							causalRechazo% = 3
							mensajeRechazo1$ = "tipo/var"
							mensajeRechazo2$ = tenderType$ + tenderVar$ + ":" + EP.scapMedioPagoDMP$
						Endif Else If Int%(Val(EP.scapInstitucionDMP$)) <> 0 And getInstitucion <> Int%(Val(EP.scapInstitucionDMP$)) Then Begin
							causalRechazo% = 2
							mensajeRechazo1$ = "entidad"
							mensajeRechazo2$ = Str$(getInstitucion) + ":" + EP.scapInstitucionDMP$
						Endif Else If validateAccountNumber(tenderAccount$, EP.scapAcctDMP$) = 0 Then Begin
							causalRechazo% = 4
							mensajeRechazo1$ = "cuenta"
							mensajeRechazo2$ = tenderAccount$
						Endif
						!
						!If (tenderAmt% <> tenderAmtExpected% And (tenderAmt% < TS.BALDUE(0) Or tenderAmtExpected% < TS.BALDUE(0))) Or \
						!		(Int%(Val(EP.scapInstitucionDMP$)) <> 0 And getInstitucion <> Int%(Val(EP.scapInstitucionDMP$))) Or \
						!		EP.scapMedioPagoDMP$ <> tenderType$ + tenderVar$ Or \
						!		validateAccountNumber(tenderAccount$) = 0 Then Begin
						!	Call rechazarSecuenciaMP("No coincide pago")
						!	!Call rechazarSecuenciaMP(Str$(EP.scapValorPagoDMP%)+" "+EP.scapMedioPagoDMP$+" "+\
						!	!	EP.scapInstitucionDMP$ + " " + \
						!	!	Str$(tenderAmt%) + " " + tenderType$ + tenderVar$ + " " + Str$(getInstitucion))
						!	Exit Sub
						!
						!
						!Endif
						!
						If causalRechazo% <> 0 Then Begin
							mensajeRechazo1$ = \
								Left$("No coincide " + mensajeRechazo1$ + String$(20, " "), 20)
							Call EP.DISPLAY.AN.ERROR(mensajeRechazo1$ + mensajeRechazo2$)
							Call rechazarSecuenciaMP("CLEAR=Reintenta pago")
						Endif
						!
					Endif Else Begin
						EP.scapIgnoreAccount% = 0
						If (TS.IO.KEYS(1) <> 70 And voidTender% = 0) And trxSign% = 1 Then Begin
							voidTender% = 0
							If Not EP.validarBeneficiosMedioPago(tenderType$, tenderVar$, Str$(getInstitucion), tenderAccount$) Then Begin
								Call EP.DISPLAY.AN.ERROR("Puede obtener dctos por medio de pago")
								Dim TS.IO.KEYS(10)
								Dim TS.IO.DATA$(10)
								TS.IO.MOTORKEY = 0
								Call EP.otorgarBeneficiosMedioPago
							Endif
						Endif
					Endif
				Endif Else Begin
					Call EP.DISPLAY.AN.ERROR(mensajeRechazo1$)
					Dim TS.IO.KEYS(10)
					Dim TS.IO.DATA$(10)
					TS.IO.MOTORKEY = 0					
				Endif
			Endif
		Endif
	Endif
End Sub
!
!--------------------------------------------------------------
! Si hay una linea de artículo en la 20
! se envía la notificación a SCAP para
! que se encole y posteriormente recuperar
! los beneficios. Si hay procesos pendientes
! se trata de recuperar los beneficios
! antes de notificar el nuevo artículo.
!--------------------------------------------------------------
SUB UESCAP20 PUBLIC
	Integer*2 tmpLineType%, tmpLineData%
	!
	If Not EP.scap.temporaryInactive% Then Begin
		tmpLineType% = TS.LINETYPE
		tmpLineData% = TS.LINEDATA
		IF TS.PROCEDURE < 1 THEN \
		BEGIN
			tmpInstitucion% = getInstitucion
			CALL EP.notificarLecturaCliente
			CALL EP.obtenerDatosCliente
	!
			IF EP.IS.ITEM.LINE.20 THEN \
			BEGIN
				IF EP.scapPendientes% > 0 THEN \
					CALL EP.obtenerBeneficios("1")
				EP.lineaArticulo$ = TS.PRTBUF$
				CALL EP.notificarLecturaArticulo
				EP.scapArticuloNotificado% = -1
				EP.scapTrx% = -1
			ENDIF \
			ELSE IF TS.LINETYPE = 5 THEN \
			BEGIN
				IF EP.scapPendientes% > 0 THEN \
					CALL EP.obtenerBeneficios("1")		
			ENDIF \
			ELSE IF TS.LINETYPE = 18 AND				\! Trailer line
					(TS.INTRX) AND						\! checkout trx
					(NOT isSecuenciaAnulacion) AND				\! Not voided trans.
					TS.PRTBUF$ = TO.TRAILERLINE1$ THEN	\   ! Trailer line 1
			BEGIN
				CALL EP.procesarBonos
			ENDIF ELSE IF \
					TS.LINETYPE = 6 AND \
					TS.LINEDATA = 1 AND \ 
					(TS.INTRX <> 0 Or \ 
					 (TS.IO.KEYS(1) = 82 AND ts.io.keys(6) = 81) ) \
			THEN BEGIN
				CALL EP.notificarFinTransaccion
			ENDIF
		ENDIF
		TS.LINETYPE = tmpLineType%
		TS.LINEDATA = tmpLineData%
	Endif
END SUB
!
Sub UESCAP30 Public
	If Not EP.scap.temporaryInactive% Then Begin
		If EP.scapPendingDMP% = 1 Then Begin
			! Beneficios por medio de pago
			EP.scapPendingDMP% = 2
		Endif
	Endif
End Sub
!
Sub UESCAP63 Public
	If Not EP.scap.temporaryInactive% Then Begin
		If EP.scapPendingDMP% = 2 Then Begin
			! Beneficios por medio de pago
			EP.scapPendingDMP% = 3
			Call UESCAP.DMP
			Call TSBDEC01
		Endif
	Endif
End Sub
!
SUB UESCAP33 PUBLIC
	Integer*2 triesCounter%
	Integer*1 tmpVoidFlag%
	Integer*4 montoTrxInt%
	String montoTrx$
	!
	tmpVoidFlag% = 0
	If PPP.inRecovery% = -1 Then Begin
		Call PPP.endRecovery
	Endif
	!
	IF EP.scapRecupInProgress% THEN \
	BEGIN
		CALL EP.notificarLecturaCliente
		triesCounter% = 0
		WHILE EP.scapPendientes% > 0 And triesCounter% < 300
			CALL EP.obtenerBeneficios("0")
			IF EP.scapPendientes% > 0 THEN \
			Begin
				triesCounter% = triesCounter% + 1
				WAIT ; 200
			EndIf
		WEND
		! Beneficios por transacción
		!CALL EP.obtenerBeneficiosTransaccion("0")
		! Pide los beneficios a transacción pero no los procesa
		! con el fin de que no se dupliquen
		montoTrxInt% = trxSign% * (TS.TOTALS(0, 0, 0) + EP.acumDctosTrx%)
		If montoTrxInt% < 0 Then Begin
			montoTrx$ = "-" + Right$(String$(9,"0") + Str$(Abs(montoTrxInt%)),9)
		Endif Else Begin
			montoTrx$ = Right$(String$(10,"0") + Str$(montoTrxInt%),10)
		Endif
		Call EP.invocarApplManager(		\
			"02","06",montoTrx$ + "0",0)
		!
		EP.acumDctosMP% = 0
		EP.acumDctosTrx% = 0
		!EP.scapRecupInProgress% = 0
		Call setScapRecoveryState(0)
		!
		Call updateRedemptionStrings
		!
		!Call DISC.voidConfirmDiscounts(tmpVoidFlag%)
		 tmpVoidFlag% = 0
	        !
		!CALL EP.DISPLAY.A.MESSAGE("TRANSACC. RECUPERADA")
		!
		If tmpVoidFlag% Then Begin
			DISC.recoverDiscounts% = -1
			Call appl.enableMessageDisplay("PRESIONE BORRAR PARA", "RECALCULAR DCTOS")
		Endif Else Begin
			CALL EP.DISPLAY.A.MESSAGE("TRANSACC. RECUPERADA")
		Endif
	ENDIF
	!
	If DISC.customerCouponRecoveryCnt% > 0 Then Begin
		Call applyRecoveredCustomerCoupons
		Dim DISC.customerCouponRecovery$(20, 1)
		DISC.customerCouponRecoveryCnt% = 0
	Endif
	!
	!If (SL.HD.INDICAT1 And CONTING.headerUserFieldBit%) Then Begin
	!	CONTING.active% = 1
	!Endif Else Begin
	!	CONTING.active% = 0
	!Endif
END SUB
!
SUB UESCAP44 PUBLIC
	Integer*2 counter%
	IF EP.scapRecupInProgress% THEN \
	BEGIN
		for counter% = 1 to 4
			ts.disc.save(counter%,0) = 0
		next counter%
		ts.disc.save(0,0) = sl.di.amtdisco
		IF EP.scapTaxIndex% > 0 THEN \
			ts.disc.save(EP.scapTaxIndex%,0) = sl.di.amtdisco
		EP.scapTaxIndex% = -1
	ENDIF
END SUB
!
SUB UESCAP53 PUBLIC
	Integer*2 sepIndex1%,sepIndex2%
	String dataN$,taxIndex$,tmpSign$
	IF NOT EP.scapRecupInProgress% Then Begin
		!EP.scapRecupInProgress% = -1
		Call setScapRecoveryState(-1)
	Endif
	!
	Call PPP.recovery
	!
	IF TS.TEMP1I2 = 99 THEN \
	BEGIN
		! Busca el data1
		sepIndex1% = MATCH(CHR$(3AH),SL.STR.ENTRY$,1)
		IF sepIndex1% > 0 THEN \
		BEGIN
			sepIndex2% = MATCH(CHR$(3AH),SL.STR.ENTRY$,sepIndex1%+ 1)
			IF sepIndex2% > 0 THEN \
			BEGIN
				dataN$ = UNPACK$(MID$(SL.STR.ENTRY$,sepIndex1% + 1,sepIndex2% - sepIndex1% - 1))
				IF dataN$ = EP.scapDE.key$ + EP.scapDcto.function$ + "00000000" THEN \
				BEGIN
					sepIndex1% = sepIndex2%
					sepIndex2% = MATCH(CHR$(3AH),SL.STR.ENTRY$,sepIndex1%+ 1)
					IF sepIndex2% > 0 THEN \
						dataN$ = UNPACK$(MID$(SL.STR.ENTRY$,sepIndex1% + 1,sepIndex2% - sepIndex1% - 1)) \
					ELSE \
						dataN$ = UNPACK$(MID$(SL.STR.ENTRY$,sepIndex1% + 1,LEN(SL.STR.ENTRY$) - sepIndex1%))
					tmpSign$ = Mid$(dataN$, 42, 1)
					!
					! Acumulación de descuentos a transacción en 
					! modo recuperación
					If Mid$(dataN$, 31, 2) = "05" Or Mid$(dataN$, 31, 2) = "10" Then Begin
						If tmpSign$ = "1" Then Begin
							EP.acumDctosTrx% = EP.acumDctosTrx% - Int%(Val(Mid$(dataN$, 33, 9)))
						Endif Else Begin
							EP.acumDctosTrx% = EP.acumDctosTrx% + Int%(Val(Mid$(dataN$, 33, 9)))
						Endif
					Endif
					!
					IF LEN(dataN$) >= 88 THEN \
					BEGIN
						taxIndex$ = MID$(dataN$,87,2)
						IF \
								ASC(LEFT$(taxIndex$,1)) >= 48 AND ASC(LEFT$(taxIndex$,1)) <= 57 AND \
								ASC(RIGHT$(taxIndex$,1)) >= 48 AND ASC(RIGHT$(taxIndex$,1)) <= 57 THEN \
							EP.scapTaxIndex% = VAL(taxIndex$)
							!
							!-------------------------------------------------------------------------------
							!2019-08-12 jsv
							! Si hay artículo beneficiario, afecta los totales de IVA
							!-------------------------------------------------------------------------------
							If Val(Mid$(dataN$, 15, 12)) > 0 Then Begin
								If tmpSign$ = "1" Then Begin
									gv.descto.iva%(EP.scapTaxIndex%,0) = gv.descto.iva%(EP.scapTaxIndex%,0) + Int%(Val(Mid$(dataN$, 33, 9)))
									! iva
									gv.descto.iva%(EP.scapTaxIndex%,1) = gv.descto.iva%(EP.scapTaxIndex%,1) + \
											round(float(Int%(Val(Mid$(dataN$, 33, 9)))) / (1 + float(val(Mid$(dataN$, 43, 4))) / 10000),0,0)
								Endif Else Begin
									gv.descto.iva%(EP.scapTaxIndex%,0) = gv.descto.iva%(EP.scapTaxIndex%,0) - Int%(Val(Mid$(dataN$, 33, 9)))
									! iva
									gv.descto.iva%(EP.scapTaxIndex%,1) = gv.descto.iva%(EP.scapTaxIndex%,1) - \
											round(float(Int%(Val(Mid$(dataN$, 33, 9)))) / (1 + float(val(Mid$(dataN$, 43, 4))) / 10000),0,0)									
								Endif
							Endif
							!-------------------------------------------------------------------------------
					ENDIF
					!
					! En el data3 puede venir informacion de beneficio por medio de pago
					If sepIndex2% > 0 Then Begin
						sepIndex1% = sepIndex2%
						sepIndex2% = MATCH(CHR$(3AH),SL.STR.ENTRY$,sepIndex1%+ 1)
						IF sepIndex2% > 0 THEN \
							dataN$ = UNPACK$(MID$(SL.STR.ENTRY$,sepIndex1% + 1,sepIndex2% - sepIndex1% - 1)) \
						ELSE \
							dataN$ = UNPACK$(MID$(SL.STR.ENTRY$,sepIndex1% + 1,LEN(SL.STR.ENTRY$) - sepIndex1%))
						If Len(dataN$) >= 32 Then Begin
							EP.scapValorPagoDMP% = Int%(Val(Mid$(dataN$, 1,10)))
							EP.scapValorDctoDMP% = Int%(Val(Mid$(dataN$,11,10)))
							EP.scapValorTeoricoDMP% = Int%(Val(Mid$(dataN$, 21, 10)))
							EP.scapMedioPagoDMP$ = Mid$(dataN$,31,2)
							!
							! Acumulacion de descuentos por medio de pago
							! en modo recuperación
							If tmpSign$ = "1" Then \
								EP.acumDctosMP% = EP.acumDctosMP% - EP.scapValorDctoDMP% \
							Else \
								EP.acumDctosMP% = EP.acumDctosMP% + EP.scapValorDctoDMP%
							!
							If Len(dataN$) > 32 Then \
								EP.scapInstitucionDMP$ = Mid$(dataN$,33,5) \
							Else \
								EP.scapInstitucionDMP$ = "0"
							!													!
							! en el data4 puede venir información sobre el rango de cuentas asociado al descuento por medio de pago
							If sepIndex2% > 0 Then Begin
								sepIndex1% = sepIndex2%
								sepIndex2% = MATCH(CHR$(3AH),SL.STR.ENTRY$,sepIndex1%+ 1)
								IF sepIndex2% > 0 THEN \
									dataN$ = UNPACK$(MID$(SL.STR.ENTRY$,sepIndex1% + 1,sepIndex2% - sepIndex1% - 1)) \
								ELSE \
									dataN$ = UNPACK$(MID$(SL.STR.ENTRY$,sepIndex1% + 1,LEN(SL.STR.ENTRY$) - sepIndex1%))
								EP.scapAcctDMP$ = dataN$
							Endif
							!
							If tmpSign$ = "1" Then Begin
								Call removerScapDataDMP
								!
								! En la recuperación no debe notificar medios de pago al motor de promociones
								! se arranca de ceros
								!Call EP.notificarMedioPagoScap(Int%(Val(Left$(EP.scapMedioPagoDMP$,1))),\
								!	Int%(Val(Mid$(EP.scapMedioPagoDMP$,2,1))),\
								!	Int%(Val(EP.scapInstitucionDMP$)),\
								!	-EP.scapValorPagoDMP% - EP.scapValorDctoDMP%, Left$(EP.scapAcctDMP$ + String$(18,"0"),18),\
								!	EP.scapValorPagoDMP%)
								!
								EP.scapMedioPagoDMP$ = ""
								EP.scapInstitucionDMP$ = ""
								EP.scapValorPagoDMP% = 0
								EP.scapValorDctoDMP% = 0
								EP.scapValorTeoricoDMP% = 0
								EP.scapAcctDMP$ = ""								
							Endif Else Begin
								Call acumularScapDataDMP
								!
								!Call EP.notificarMedioPagoScap(Int%(Val(Left$(EP.scapMedioPagoDMP$,1))),\
								!	Int%(Val(Mid$(EP.scapMedioPagoDMP$,2,1))),\
								!	Int%(Val(EP.scapInstitucionDMP$)),\
								!	EP.scapValorPagoDMP% + EP.scapValorDctoDMP%, Left$(EP.scapAcctDMP$ + String$(18,"0"),18),\
								!	EP.scapValorPagoDMP%)
								!
							Endif
						Endif
					Endif
				Endif Else If dataN$ = DISC.customerCouponUDKey1$ Then Begin
					Call recoverCustomerCouponDiscount(SL.STR.ENTRY$)
				Endif
			ENDIF
		ENDIF
	ENDIF Else If TS.TEMP1I2 = 11 Then Begin
		Call recoverContingRequest(SL.STR.ENTRY$)
	Endif
END SUB
!
!
!--------------------------------------------------------------
! Si hay procesos pendientes de scap al
! momento de imprimir la linea del articulo
! se invoca una vez la rutina de recuperar
! beneficios
!--------------------------------------------------------------
SUB UESCAP60 PUBLIC
	Integer*2 triesCounter%
	Integer*4 valorPagado%
	String accountNo$
	!Integer*2 variedad%
	If Not EP.scap.temporaryInactive% Then Begin
		IF TS.PROCEDURE < 1 AND (TS.INTRX) THEN \
		BEGIN
			IF EP.scapArticuloNotificado% AND EP.scapRecupInProgress% THEN \
			BEGIN
				IF EP.IS.ITEM.LINE.60 THEN \
				BEGIN
					EP.scapArticuloNotificado% = 0
					!
					! Si está en modo recuperación debe quedarse
					! en esta rutina hasta terminar proceso
					! de posibles beneficios
					EP.scapPendientes% = 1
					triesCounter% = 0
					WHILE EP.scapPendientes% > 0 And triesCounter% < 300
						CALL EP.obtenerBeneficios("0")
						IF EP.scapPendientes% > 0 THEN \
						Begin
							triesCounter% = triesCounter% + 1
							WAIT ; 200
						EndIf			
					WEND
				ENDIF
			ENDIF
			IF TS.LINETYPE = 2 AND EP.scapTrx% THEN \
			BEGIN
				valorPagado% = SL.TE.AMTTENDE
				accountNo$ = TS.ACNUM$
				IF TS.BALDUE(0) < 0 And SL.TE.TENDTYPE * 10 + SL.TE.TENDVAR = 11 THEN \
					valorPagado% = valorPagado% + TS.BALDUE(0)
				!Call acumulaMedioPago(SL.TE.TENDTYPE,SL.TE.TENDVAR,tmpInstitucion%,SL.TE.AMTTENDE)
				Call acumulaMedioPago(SL.TE.TENDTYPE,SL.TE.TENDVAR,tmpInstitucion%,valorPagado%,accountNo$)
				!
				!-------------------------------------------------------
				! Se inhibe este código porque después de pagar la transacción
				! no se deben otorgar beneficios
				!-------------------------------------------------------
				!
				!IF TS.BALDUE(0) <= 0 THEN \
				!Begin
				!	! Simula el comportamiento de la tecla Total
				!	CALL EP.notificarLecturaCliente
				!	CALL EP.obtenerDatosCliente
				!	triesCounter% = 0
				!	WHILE EP.scapPendientes% > 0 And triesCounter% < 300
				!		CALL EP.obtenerBeneficios("0")
				!		IF EP.scapPendientes% > 0 THEN \
				!		Begin
				!			triesCounter% = triesCounter% + 1
				!			WAIT ; 200
				!		EndIf
				!	WEND
				!	!
				!	CALL EP.obtenerBeneficiosTransaccion("1")
				!EndIf
				!-------------------------------------------------------
				!
				!If trxSign% = 1 And TS.BALDUE(0) > 0 Then \
				!	Call EP.otorgarBeneficiosMedioPago
			ENDIF
		ENDIF
	Endif
END SUB
!
Sub UESCAP67 Public
	If FE.forceVoid% Then Begin
		FE.forceVoid% = -2
		Call printDebug("UESCAP67: " + Left$(FE.forceVoidMsg$, 20) + "." + Mid$(FE.forceVoidMsg$, 21, 20))
		Call appl.enableMessageDisplay(Left$(FE.forceVoidMsg$, 20), Mid$(FE.forceVoidMsg$, 21, 20))
	Endif Else If isFEExPlu( Unpack$(ITEMCODE$) ) Then Begin
		FE.exPlu% = -1
	Endif
End Sub
!