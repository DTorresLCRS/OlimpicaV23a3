!*********************************************************************
!                         (UETCROVA.TPV)
!                           (Ver 1.00)
!
! This routine performs the following
! - Defines data structure
!*********************************************************************
!
%ENVIRON T

%INCLUDE \ADX_UPGM\EAMTSWKG.J86

%INCLUDE \ADX_UPGM\EAMTOPTS.J86

%INCLUDE \ADX_UPGM\EAMTRANS.J86

%INCLUDE \APPL\APIVARI.J86

!----------------------------------------------------------------------------------
! 2021-09-06 jsv
! Información complementaria para reversos de secuencia TCRO
!----------------------------------------------------------------------------------
String Global EP.REV.ACCOUNT$
Integer*2 Global EP.REV.VOUCHER.START%, EP.REV.VOUCHER.END%
!----------------------------------------------------------------------------------

String Global TCOSEQ.expireDate$
Integer*2 Global TCOSEQ.entity%, TCOSEQ.tendType%

! 2018-05-12 jsv
! Se agregan variables para almacenar información
! adicional opcional en el data entry de entidades
String Global EP.OPTIONAL.DATA$
Integer*2 Global EP.DATA1.INDEX%, EP.DATA2.INDEX%

  INTEGER*1 GLOBAL       \
   PROCE%,               \
   EP.ENVIO.RECAUDO.TCRO%

  STRING GLOBAL          \
    EP.SAVE.AUT$

	INTEGER*2 GLOBAL SL.ITNEGPRC

!  INTEGER*2 GLOBAL       \
!     TS11.OVRFLAG

  STRING    GLOBAL       \
    UE.LISTA.TARIFAS$,   \
    CONSERF$,            \
    TT.PCR.NOMBRE,       \
    NOCUO$,              \!
    EP.TCRO.ALT.TV$       ! tipo variedad alterno TCRO
!
String Global EP.VALOR.SALDO$
!
    !
!  display, print, keyboard routines
!
!  STRING   GLOBAL        \
!    KBD.USER.SAVE$(1),   \  
!    DIS.USER.SAVE$(1),   \ 
!    PRT.USER.SAVE$(1),   \
!    EP.SAVE.HEADER$

!  INTEGER*2  GLOBAL      \
!    KBD.USER.SAVE(1),    \
!    DIS.USER.SAVE(1),    \
!    PRT.USER.SAVE(1)

  REAL   GLOBAL          \
    KIV1,                \
    KIV2,                \
    KIV3,                \
    KIV4
  
  INTEGER*4  GLOBAL      \
\    EP.TAX.EPAY%,        \
\    EP.TAX.BASE%,        \
    IPOTAA%,             \
    IPOTBB%,             \
    IPOTCC%,             \
    IPOTDD%,             \
    EP.TPVTIVT%,         \
    EP.TPVTIV1%,         \
    EP.TPVTIV2%,         \
    EP.TPVTIV3%,         \
    EP.TPVTIV4%,         \
    EP.BASEIV1%,         \
    EP.BASEIV2%,         \
    EP.BASEIV3%,         \
    EP.BASEIV4%,         \
    EP.BASE.DEVOL%

  INTEGER*1              \
    EP.TCRO%,            \ ! variable de control de estado 
    EP.TALP%,            \ ! variable de control de estado
    EP.TCRO.OK%,         \ ! variable de control de estado 
    EP.RECAUDO.OK%,      \ ! variable de control de estado 
    EP.FECHA.OK%        ,\
    debugTcro%          ,\
    takeAcct%           ,\
    EP.VERIFY.32%       ,\
    validacionRecaudosTCRO%,\
    EP.secuenciaRecaudo%

  INTEGER*4              \
    EP.DUMMY%
    
  STRING                 \
    EP.NOMBRE$,          \
    EP.DUMMY$,           \
    EP.ACCT.STATUS$,     \
    EP.TCRO.CARD$,       \
    tvBin$,              \
    EP.AUTORIZ.RECAUDO.TCRO$,\
    EP.FVCTO.RECAUDO.TCRO$
! 
! jsv.add.2008/03/27
! Se independizan variables
! tef para tcro con el fin
! de garantizar que no se
! sobreescriban por otros procesos
  STRING                 \!
    EP.TCRO.VALOR.EFT$  ,\!
    EP.TCRO.TAX.EFT$    ,\!
    EP.TCRO.TAX.BASE$
!
!*********************************************************************
!                         (UETCROSU.TPV)
!                           (Ver 1.00)
!
! This routine performs the following
! - Calculate check digit 
!*********************************************************************
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

SUB NUMSERF EXTERNAL
END SUB
!
!---------------------------------------------------------
! 2018-05-12 jsv
! Rutina para escribir log por aplicación
!---------------------------------------------------------
Sub traceApplication(pMsg$, pAppl$) External
	String pMsg$, pAppl$
End Sub
!
Sub reasignEntity(pTipoVariedad%, pEntity%) External
	Integer*2 pTipoVariedad%, pEntity%
End Sub
!
Sub menpag.ignoreTender External
End Sub
!
!---------------------------------------------------------
! 2018-05-22 jsv
! Función para tener acceso al valor de llave de data entry
! de entidades
!---------------------------------------------------------
Function MENPAG.getDataEntryKey External
	String MENPAG.getDataEntryKey
End Function
!
Sub printDebug(pMessage$) External
	String pMessage$
End Sub
!
Function TPONLINE.getCustId External
	String TPONLINE.getCustId
End Function
!
Function isMiscPlu(pPlu$) External
	Integer*1 isMiscPlu
	String pPlu$
End Function
!
SUB EP.TARJ.CRED.14(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
           EP.INVOICE.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.CUOTAS.QTY$, \
           EP.APPROV.CODE$, EP.APPROV.DESC$, \
           EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$, EP.USER.DATA$) EXTERNAL
!
  STRING EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
           EP.INVOICE.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.CUOTAS.QTY$, \
           EP.APPROV.CODE$, EP.APPROV.DESC$, \
           EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$, EP.USER.DATA$          
           
END SUB
!
SUB EP.TARJ.CRED.32(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
        EP.INVOICE.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.CUOTAS.QTY$,  \
        EP.APPROV.CODE$, EP.APPROV.DESC$, \
        EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$, EP.USER.DATA$) EXTERNAL
!
STRING EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
        EP.INVOICE.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.CUOTAS.QTY$,  \
        EP.APPROV.CODE$, EP.APPROV.DESC$, \
        EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$, EP.USER.DATA$
!       
END SUB
!        
SUB TRANSLATE.COMM.CODE(UE.VALUE16$,UE.VALUE17$)   EXTERNAL
  STRING UE.VALUE16$, UE.VALUE17$
END SUB
!
SUB TRANSLATE.APPL.CODE(UE.VALUE18$,UE.VALUE19$)   EXTERNAL
  STRING UE.VALUE18$, UE.VALUE19$
END SUB
!
SUB TRANSLATE.ISO.CODE(UE.VALUE20$,UE.VALUE21$)   EXTERNAL
  STRING UE.VALUE20$, UE.VALUE21$
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
Sub sendReverse(pAppl$, pFunction$, pConsecutive$, pInvoice$, pDate$, pAccount$, pPointer1%, pPointer2%) External
	String pAppl$, pFunction$, pConsecutive$, pInvoice$, pDate$, pAccount$
	Integer*2 pPointer1%, pPointer2%
End Sub
!
!------------------------------------------------------------------------------------
!   Java API routines
!------------------------------------------------------------------------------------
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
!------------------------------------------------------------------------------------
!
Function getFranquicia(pBin$)
	String getFranquicia, pBin$, tmpReturn$, tmpException$
	String tmpRequest$, tmpClassName$, tmpMethodName$
	!
	Call traceApplication("getFranquicia(" + pBin$ + ")", "APITCRO")
	!
	tmpRequest$ = "C$"
	tmpClassName$ = "com.olimpica.utils.ConsultaFranquicia"
	tmpMethodName$ = "getFranquicia"
	tmpReturn$ = ""
	tmpException$ = ""
	!
	Call Javacall.Initialize.Request(tmpClassName$, tmpMethodName$, tmpRequest$)
	!
	Call Javacall.AddParameter.String(tmpRequest$, pBin$)
	Call Javacall.AddParameter.String(tmpRequest$, Right$(String$(4, "0") + EP.CHAIN$, 4))
	Call Javacall.AddParameter.String(tmpRequest$, Right$(String$(4, "0") + TS.STORE$, 4))
	Call Javacall.AddParameter.String(tmpRequest$, Right$(String$(6, "0") + TS.TERMINAL$, 6))
	!
	Call Javacall.InvokeMethod.ReturnString(tmpRequest$, tmpReturn$, tmpException$)
	!
	Call traceApplication("Ret=" + tmpReturn$ + " Ex=" + tmpException$, "APITCRO")
	!
	If Len(tmpException$) > 0 Then Begin
		getFranquicia = ""
	Endif Else Begin
		getFranquicia = tmpReturn$
	Endif
End Function
!
Sub debugKeys
	Integer*2 tmpCounter%
	!
	Call printDebug("TS.IO.STATE =" + Str$(TS.IO.STATE) + " TS.IO.NEXTSTATE=" + Str$(TS.IO.NEXTSTATE))
	Call printDebug("TS.IO.DEVICE=" + Str$(TS.IO.DEVICE))
	For tmpCounter% = 1 To 10
		Call printDebug("keys(" + Str$(tmpCounter%) + ")=" + Str$(TS.IO.KEYS(tmpCounter%)) + "," + TS.IO.DATA$(tmpCounter%))
	Next tmpCounter%
End Sub
!
SUB RESET14.TCRO PUBLIC
	EP.TCRO% = 0
	EP.TALP% = 0
	takeAcct% = 0
	EP.TCRO.CARD$ = ""
	EP.TCRO.OK% = 0
	EP.VERIFY.32% = 0
END SUB
!
Function RESET14.isTCRO Public
	Integer*1 RESET14.isTCRO
	!
	If EP.TCRO.OK% Then Begin
		RESET14.isTCRO = -1
	Endif Else Begin
		RESET14.isTCRO = 0
	Endif
End Function
!
SUB CALCULO.IVAS.BASES PUBLIC
  INTEGER*1 UE.POS%                      !
!
  EP.BASE.DEVOL% = 0
!
  EP.TPVTIV1% = (TS.TOTALS(1,0,0) - IPOTAA%) -      \! Iva plan A
                (TS.TOTALS(1,0,0) - IPOTAA%) / KIV1  !
  EP.BASEIV1% =  TS.TOTALS(1,0,0) - EP.TPVTIV1%
  UE.POS%     =  MATCH("1",UE.LISTA.TARIFAS$,1)
  IF UE.POS% <> 0 THEN \
    EP.BASE.DEVOL% = EP.BASE.DEVOL% + EP.BASEIV1%
!               
  EP.TPVTIV2% = (TS.TOTALS(2,0,0) - IPOTBB%) -      \!
                (TS.TOTALS(2,0,0) - IPOTBB%) / KIV2  !
  EP.BASEIV2% =  TS.TOTALS(2,0,0) - EP.TPVTIV2%             
  UE.POS%     =  MATCH("2",UE.LISTA.TARIFAS$,1)
  IF UE.POS% <> 0 THEN \
    EP.BASE.DEVOL% = EP.BASE.DEVOL% + EP.BASEIV2%
!
  EP.TPVTIV3% = (TS.TOTALS(3,0,0) - IPOTCC%) -      \!
                (TS.TOTALS(3,0,0) - IPOTCC%) / KIV3  !
  EP.BASEIV3% =  TS.TOTALS(3,0,0) - EP.TPVTIV3%             
  UE.POS%     =  MATCH("3",UE.LISTA.TARIFAS$,1)
  IF UE.POS% <> 0 THEN \
    EP.BASE.DEVOL% = EP.BASE.DEVOL% + EP.BASEIV3%
!
  EP.TPVTIV4% = (TS.TOTALS(4,0,0) - IPOTDD%) -      \!
                (TS.TOTALS(4,0,0) - IPOTDD%) / KIV4  !  
  EP.BASEIV4% =  TS.TOTALS(4,0,0) - EP.TPVTIV4%             
  UE.POS%     =  MATCH("4",UE.LISTA.TARIFAS$,1)
  IF UE.POS% <> 0 THEN \
    EP.BASE.DEVOL% = EP.BASE.DEVOL% + EP.BASEIV4%
!
  EP.TPVTIVT% = EP.TPVTIV1% + EP.TPVTIV2% + EP.TPVTIV3% + EP.TPVTIV4%
!
  IF EP.TPVTIVT% < 0 THEN EP.TPVTIVT% = 0            !
!
END SUB                                              !
!-----------------------------------------------------------------------------
FUNCTION valida.digito.chequeo(accountNumber$)  EXTERNAL
  integer*1 valida.digito.chequeo
  string accountNumber$
END FUNCTION

FUNCTION EP.CHECK.DIGIT(UE.ACCOUNT.NBR$) EXTERNAL 
  STRING UE.ACCOUNT.NBR$, EP.CHECK.DIGIT, UE.ADD.QTY$
  INTEGER*2 UE.POS%, UE.LEN%, UE.TOTAL%, UE.10%, UE.ADD.QTY%
END FUNCTION

!SUB EP.LINE.PRINT(LINEA$,ESTACION%) EXTERNAL
!    STRING LINEA$
!    INTEGER*2 ESTACION%
!END SUB
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
!
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

!---------------------------------------------------------
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
!
FUNCTION EP.READ.TRANSNUM EXTERNAL
  STRING EP.READ.TRANSNUM, UE.WORK.TRANSNUM$
!  
END FUNCTION
!
SUB EP.WRITE.TRANSNUM(UE.EPAY.TRANSNUM$) EXTERNAL
  STRING UE.WORK.TRANSNUM$, UE.EPAY.TRANSNUM$
!
END SUB
!
FUNCTION EP.NEW.TRANSNUM EXTERNAL
  STRING EP.NEW.TRANSNUM, UE.WORK.TRANSNUM$
!
END FUNCTION
!
FUNCTION applManagerFunction(msgToSend$,resetVoucher%) EXTERNAL
String applManagerFunction,msgToSend$
	Integer*1 resetVoucher%
END FUNCTION
!
SUB EP.GETUNPK EXTERNAL                                !
END SUB                                                ! siguiente campo 
!
SUB TRANSLATE.TV.CODE(UE.GROUP.DESC$,UE.CODE$) EXTERNAL
	STRING UE.CODE$,UE.GROUP.DESC$
END SUB
!
FUNCTION EP.BUSCA.INDICE.TV(UE.TIPO.VAR$) EXTERNAL
	INTEGER*2 EP.BUSCA.INDICE.TV
	STRING UE.TIPO.VAR$
END FUNCTION
!
!---------------------------------
! validacion medio de pago uedscpag
!---------------------------------
!SUB DSCPAG14B EXTERNAL
!END SUB
!
FUNCTION EP.BUSCA.MONTO.TV(UE.TIPO.VAR$) EXTERNAL
	INTEGER*4 EP.BUSCA.MONTO.TV
	STRING UE.TIPO.VAR$
END FUNCTION
!---------------------------------
! tender institutions
!---------------------------------
SUB MENPAG14 EXTERNAL
END SUB
!
! Retorna -1 si el tipo variedad
! especificado solicita entidad
FUNCTION isTVEntity(requestedTV$) EXTERNAL
	String requestedTV$
	Integer*1 isTVEntity
END FUNCTION
!
Function EP.invocarApplManager(epAppl$,epFunction$,parteVariable$,usaConsecutivo%) External
	String EP.invocarApplManager,epFunction$,parteVariable$,epAppl$
	Integer*1 usaConsecutivo%
End Function
!
Function getTipoVariedadTCRO(pCardNo$)
	String getTipoVariedadTCRO, pCardNo$, tmpAnswer$
	String bakVar$(1)
	Integer*2 bakVar%(1)
	!
	Call printDebug("getTipoVariedadTCRO(" + pCardNo$ + ")")
	!
	Dim bakVar$(10)
	Dim bakVar%(10)
	!
	! Respalda variables
	bakVar$( 1) = EP.MESSAGE$
	bakVar$( 2) = EP.EXCEPTION$
	bakVar$( 3) = EP.TRX.STATUS$
	bakVar$( 4) = EP.AMJ.STATUS$
	bakVar$( 5) = EP.APPL.STATUS$
	bakVar$( 6) = EP.AMJ.MESSAGE$
	bakVar$( 7) = EP.APPROV.CODE$
	bakVar$( 8) = EP.APPROV.DESC$
	bakVar$( 9) = EP.DE.AMJ.STATUS$
	bakVar$(10) = EP.DE.TRX.STATUS$
	bakVar%( 1) = EP.STATE%
	!
	tmpAnswer$ = EP.invocarApplManager("08","90",pCardNo$,0)
	!
	! Restaura variables
	EP.MESSAGE$ = bakVar$( 1)
	EP.EXCEPTION$ = bakVar$( 2)
	EP.TRX.STATUS$ = bakVar$( 3)
	EP.AMJ.STATUS$ = bakVar$( 4)
	EP.APPL.STATUS$ = bakVar$( 5)
	EP.AMJ.MESSAGE$ = bakVar$( 6)
	EP.APPROV.CODE$ = bakVar$( 7)
	EP.APPROV.DESC$ = bakVar$( 8)
	EP.DE.AMJ.STATUS$ = bakVar$( 9)
	EP.DE.TRX.STATUS$ = bakVar$(10)
	EP.STATE% = bakVar%( 1)
	!
	Call printDebug("getTipoVariedadTCRO=" + tmpAnswer$)
	!
	getTipoVariedadTCRO = tmpAnswer$
End Function
!
!--------------------------------------------------------------
!  Busca y retorna el cambio
!--------------------------------------------------------------
FUNCTION getAmtChange PUBLIC
	Integer*4 getAmtChange,changeValue%
	Integer*2 tmpCounter%,strType%
	!
	changeValue% = 0
	for tmpCounter% = 1 to sl.end
		if sl.str$(tmpCounter%) <> "" then \
		begin
			strType% = asc(sl.str$(tmpCounter%))
			if strType% = 09h then \   ! linea tipo cambio
			begin
				ep.b$ = sl.str$(tmpCounter%) + ":"
				ep.j% = 3
				call ep.getunpk
				call ep.getunpk
				changeValue% = changeValue% + int%(val(ep.a$))
			endif
		endif
	next tmpCounter%
	getAmtChange = changeValue%
END FUNCTION
!
!--------------------------------------------------------------
! Revisa si un pago determinado fue TCRO
!--------------------------------------------------------------
Function isTCROTrx(pAmount%, pTV$, pAccount$)
	Integer*4 pAmount%
	String pTV$, pAccount$, pAmount$
	Integer*1 isTCROTrx, tmpAnswer%, entry1Found%
	Integer*2 tmpCounter%
	!
	Call traceApplication("isTCROTrx(" + Str$(pAmount%) + ", " + pTV$ + ", " + pAccount$ + ")", "APITCRO")
	!
	pAmount$ = Right$(String$(10,"0") + Str$(pAmount%),10)
	tmpAnswer% = 0
	tmpCounter% = 1
	entry1Found% = 0
	While tmpCounter% <= SL.END And tmpAnswer% = 0
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 11H Then Begin
				ep.b$ = sl.str$(tmpCounter%) + ":"
				ep.j% = 3 
				Call ep.getunpk
				If Left$(ep.a$, Len(EP.EFT.CLAVE$)) = EP.EFT.CLAVE$ Then Begin
					If Right$(ep.a$,2) = "01" Then Begin
						Call ep.getunpk
						!Call ep.display.an.error(ep.a$)
						If ep.a$ = pAccount$ Then Begin
							entry1Found% = -1
						Endif
					Endif Else If Right$(ep.a$,2) = "02" Then Begin
						If entry1Found% Then Begin
							Call ep.getunpk
							!Call ep.display.an.error(ep.a$)
							If Right$(ep.a$,2) = "00" Then Begin
								Call ep.getunpk
								Call ep.getunpk
								!Call ep.display.an.error(ep.a$)
								If Left$(ep.a$,10) = pAmount$ Then Begin
									Call ep.getunpk
									Call ep.getunpk
									!Call ep.display.an.error(ep.a$)
									If Mid$(ep.a$,15,2) = pTV$ Then Begin
										tmpAnswer% = -1
									Endif
								Endif Else Begin
									entry1Found% = 0
								Endif
							Endif Else Begin
								entry1Found% = 0
							Endif
						Endif
					Endif
				Endif Else If EP.A$ = MENPAG.getDataEntryKey Then Begin
					!---------------------------------------------------------
					! 2018-05-22 jsv
					! En el data entry de entidades también puede haber 
					! información de pagos tcro
					!---------------------------------------------------------
        	Call EP.GETUNPK	! Data2
        	Call EP.GETUNPK	! Data3
        	If EP.A$ = pTV$ Then Begin ! Coincide tipo variedad
	        	Call EP.GETUNPK	! Data4
	        	Call EP.GETUNPK	! Data5
	        	If Left$(EP.A$, 10) = pAmount$ Then Begin ! Coincide monto
		        	Call EP.GETUNPK	! Data6
		        	Call EP.GETUNPK	! Data7
		        	If Len(EP.A$) > 0 Then Begin ! Hay información en el data7
			        	Call EP.GETUNPK	! Data8
			        	Call EP.GETUNPK	! Data9
			        	If EP.A$ = pAccount$ Then Begin ! Coincide nro de cuenta
			        		tmpAnswer% = -1
			        	Endif
			        Endif
		        Endif
	        Endif        	
				Endif
			Endif
		Endif
		tmpCounter% = tmpCounter% + 1
	Wend
	!
	Call traceApplication("isTCROTrx=" + Str$(tmpAnswer%), "APITCRO")
	!
	isTCROTrx = tmpAnswer%
End Function
!
!--------------------------------------------------------------
! Revisa si un pago determinado fue anulado
!--------------------------------------------------------------
Function searchVoidTrx(pAmount%, pTV$, pAccount$, pStartIndex%)
	Integer*4 pAmount%
	String pTV$, pAccount$
	Integer*1 searchVoidTrx, tmpAnswer%
	Integer*2 tmpCounter%, pStartIndex%
	!
	tmpAnswer% = 0
	tmpCounter% = pStartIndex% + 1
	While tmpCounter% <= SL.END And tmpAnswer% = 0
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 06H Then Begin
				ep.b$ = sl.str$(tmpCounter%) + ":"
				ep.j% = 3 
				Call ep.getunpk
				If pTV$ = ep.a$ Then Begin
					! Coincide tipo variedad
					Call ep.getunpk
					If pAmount% = Int%(Val(ep.a$)) Then Begin
						! Coincide monto
						Call ep.getunpk
						Call ep.getunpk
						If pAccount$ = ep.a$ Then Begin
							! Coincide numero de cuenta
							tmpAnswer% = -1
						Endif
					Endif
				Endif
			Endif
		Endif
		tmpCounter% = tmpCounter% + 1
	Wend
	searchVoidTrx = tmpAnswer%
End Function
!
!--------------------------------------------------------------
! Busca un pago realizado con el monto especificado, con el fin
! de garantizar que no haya problemas con SMA al tratar de devolver
! un pago parcial. 
!--------------------------------------------------------------
Function searchTcroTrx(pAmount%)
	String tmpTV$, tmpAuth$, tmpConsecut$, searchTcroTrx, tmpAcctno$
	Integer*4 pAmount%
	Integer*1 tmpAnswer%
	Integer*2 tmpCounter%
	!
	tmpAnswer% = 0
	tmpCounter% = 1
	While tmpCounter% <= SL.END And tmpAnswer% <> -1
		If SL.STR$(tmpCounter%) <> "" Then Begin
			If Asc(SL.STR$(tmpCounter%)) = 05H Then Begin
				ep.b$ = sl.str$(tmpCounter%) + ":"
				ep.j% = 3 
				Call ep.getunpk
				! Almacena tv en memoria
				tmpTV$ = ep.a$
				!Call ep.display.an.error("tmpTV$="+tmpTV$)
				Call ep.getunpk
				!Call ep.display.an.error(ep.a$)
				If pAmount% = Int%(Val(ep.a$)) Then Begin
					! Coincide monto
					Call ep.getunpk
					Call ep.getunpk
					tmpAcctno$ = ep.a$
					! Revisa si se trata de un pago TCRO
					If isTCROTrx(pAmount%, tmpTV$, tmpAcctno$) Then Begin
						! Debe revisar para ver si existe una anulación
						! de esta transacción
						If Not searchVoidTrx(pAmount%, tmpTV$, tmpAcctno$, tmpCounter%) Then Begin
							tmpAnswer% = -1
						Endif Else Begin
							tmpAnswer% = 1
						Endif
					Endif
				Endif
			Endif
		Endif
		tmpCounter% = tmpCounter% + 1
	Wend
	!Call ep.display.an.error("tmpAnswer%="+str$(tmpAnswer%))
	If tmpAnswer% = -1 Then Begin
		searchTcroTrx = tmpTV$
	Endif Else If tmpAnswer% = 1 Then Begin
		searchTcroTrx = "XX"
	Endif Else Begin
		searchTcroTrx = ""
	Endif
End Function
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
SUB EP.GET.KBDATA(UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, \
    UE.KEYB.DATA$) PUBLIC
  STRING UE.REQ.MESSAGE$, UE.KEYB.DATA$, UE.INI.RANGE$, UE.END.RANGE$,\
         UE.WORK$, UE.SAVDISP$ 
  Integer*1 ue14%
  !
  ue14% = TO.USEREXIT(14)
  TO.USEREXIT(14) = 0
  !
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
       IF LEN(TS.IO.DATA$(10)) > 0 OR   \ Data Entered
          TS.TEMP1I2 > 1 THEN           \ More than 1 key
         TS.IO.MOTORKEY = 0             ! Try again
    ENDIF \
    ELSE \
    BEGIN                               ! Not clear key
      IF TS.IO.KEYS(10) = 80 THEN \     ! Keyed data
      BEGIN
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
  !
  TO.USEREXIT(14) = ue14%
  !
  TS.IO.NEXTSTATE = 10                     ! Restore state = MAIN
  TS.IO.STATE = 10                         ! Restore state = MAIN
  UNLOCKDEV 32, TS.IO.STATE                ! Restore state = MAIN
END SUB

FUNCTION INGRESO.CLAVE EXTERNAL 
  INTEGER*1 INGRESO.CLAVE
END FUNCTION
!
FUNCTION VALIDACION.TCRO.LOCAL(cardNumber$)
	STRING cardNumber$
	INTEGER*1 VALIDACION.TCRO.LOCAL,validAnswer%
	!
	validAnswer% = 0
	!
	TS.TS11WERR$ = ""                                       !
	EP.KEY$ = PACK$(EP.TIPO.VAR$) +                         \
		PACK$(STRING$(24-LEN(cardNumber$),"0") + cardNumber$)                               !
	READ FORM "C13 C30";#6 KEY EP.KEY$;                    \!
		EP.DUMMY$,EP.NOMBRE$                               !
	IF TS.TS11WERR$ = "" THEN \                             !
	BEGIN                         !                         !
		TT.PCR.NOMBRE = EP.NOMBRE$
		READ FORM "C1 C12 C1 3I4";#11 KEY EP.KEY$;           \!
			EP.DUMMY$,EP.DUMMY$,EP.ACCT.STATUS$,EP.DUMMY%,EP.DUMMY%,EP.DUMMY% !
		IF VAL(UNPACK$(EP.ACCT.STATUS$)) <> 56 THEN \
			validAnswer% = -1 \
		ELSE \
			validAnswer% = 2
	ENDIF
	!
	VALIDACION.TCRO.LOCAL = validAnswer%
END FUNCTION
!
FUNCTION SECUENCIA.RECAUDO.TCRO
	String tarjetaEntrada$,tipoRecaudo$,secuenciaRecaudo$
	Integer*1 SECUENCIA.RECAUDO.TCRO
	Integer*4 montoRecaudo%
	Integer*2 flagAnul%
	!
	IF 									\
		TS.PROCEDURE < 1 AND 			\
		TS.IO.KEYS(2) = 80 AND			\
		LEN(TS.IO.DATA$(2)) = 16 THEN	\
	BEGIN
		flagAnul% = TS.IO.KEYS(1)
		tarjetaEntrada$ = TS.IO.DATA$(2)
		EP.MESSAGE$ = EP.TERMINAL$ + RIGHT$(STRING$(10,"0")+ UNPACK$(TS.OPER$),10) + "000000"	+ \
			"000000" + DATE$ + TIME$ + "00"											+ \	! Parte fija
			tarjetaEntrada$																	    ! Parte variable
		EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
		EP.MESSAGE$ = "0800" + EP.MSGLEN$ + EP.MESSAGE$
		!
		secuenciaRecaudo$ = applManagerFunction(EP.MESSAGE$,0)
		IF secuenciaRecaudo$ <> "XX" THEN \
		BEGIN
			DIM TS.IO.KEYS(10)                       ! CLEAR KEYED DATA
			DIM TS.IO.DATA$(10)                      ! CLEAR KEYED DATA
			!
			IF LEN(secuenciaRecaudo$) >= 11 THEN \
			BEGIN
				TS.IO.KEYS(1) = flagAnul%
				tipoRecaudo$ = LEFT$(secuenciaRecaudo$,1)
				montoRecaudo% = VAL(MID$(secuenciaRecaudo$,2,10))
				!
				TS.IO.DATA$(5) = STR$(montoRecaudo%)
				IF tipoRecaudo$ = "1" THEN \
				BEGIN
					TS.IO.KEYS(5) = 236
					TS.IO.MOTORKEY = 236
				ENDIF ELSE \
				BEGIN
					TS.IO.KEYS(5) = 237
					TS.IO.MOTORKEY = 237
				ENDIF
				!
				TS.IO.KEYS(6) = 72
				TS.IO.DATA$(6) = tarjetaEntrada$
				!
				TS.IO.KEYS(10) = 121
				TS.IO.DATA$(10) = "3"
			ENDIF
			SECUENCIA.RECAUDO.TCRO = -1
		ENDIF ELSE \
			SECUENCIA.RECAUDO.TCRO = 0
	ENDIF ELSE \
		SECUENCIA.RECAUDO.TCRO = 0
END FUNCTION
!
FUNCTION VALIDACION.TCRO.REMOTA(cardNumber$)
	STRING cardNumber$,zeroesValue$,epFunction$,validAnswer$
	INTEGER*1 VALIDACION.TCRO.REMOTA
	!
	epFunction$ = "01"
	EP.NOMBRE$ = ""
	!
	EP.MESSAGE$ = EP.TERMINAL$ + EP.CAJERO$ + "000000"	+ \
		EP.INVOICE.NBR$ + DATE$ + TIME$ + "00"											+ \	! Parte fija
		LEFT$(cardNumber$ + STRING$(20," "),20) 										    ! Parte variable
	EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
	EP.MESSAGE$ = EP.APPL$ + epFunction$ + EP.MSGLEN$ + EP.MESSAGE$
	!
	EP.AUTORIZ.RECAUDO.TCRO$ = "000000"
	EP.VALOR.SALDO$ = "-000000001"
	validAnswer$ = applManagerFunction(EP.MESSAGE$,0)
	IF LEN(validAnswer$) >= 8 THEN \
	BEGIN
		IF MID$(validAnswer$,7,2) = "00" THEN \
		BEGIN
			EP.AUTORIZ.RECAUDO.TCRO$ = MID$(validAnswer$,3,6)
			EP.FVCTO.RECAUDO.TCRO$ = MID$(validAnswer$,111,4)
			VALIDACION.TCRO.REMOTA = -1
			EP.ACCT.STATUS$ = PACK$("50")
			IF LEN(validAnswer$) >= 100 THEN \
				EP.NOMBRE$ = MID$(validAnswer$,81,20)
			IF LEN(validAnswer$) >= 114 THEN \
				EP.FVCTO.RECAUDO.TCRO$ = MID$(validAnswer$,111,4) \
			ELSE \
				EP.FVCTO.RECAUDO.TCRO$ = "0000"
			If Len(validAnswer$) >= 134 Then EP.VALOR.SALDO$ = Mid$(validAnswer$,125,10)
		ENDIF ELSE \
			VALIDACION.TCRO.REMOTA = 0
	ENDIF ELSE \
		VALIDACION.TCRO.REMOTA = 1
	TT.PCR.NOMBRE = EP.NOMBRE$
END FUNCTION
!
FUNCTION ENVIO.RECAUDO.TCRO(ecrFunc$,transnum$,allCard$,valorEft$,valorSaldo$)
	INTEGER*1 ENVIO.RECAUDO.TCRO
	STRING zeroesValue$,epFunction$,ecrFunc$,transnum$,allCard$,valorEft$,valorSaldo$,respuestaRecaudo$,pagoEfect$,cambio$
	!
	pagoEfect$ = Right$(String$(10,"0") + Str$(EP.BUSCA.MONTO.TV("11")),10)
	cambio$ = Right$(String$(10,"0") + str$(getAmtChange),10)
	
	zeroesValue$ = STRING$(10,"0")
	IF ecrFunc$ = "02" THEN \
		epFunction$ = "14" \
	ELSE \
		epFunction$ = "15"
	!
	EP.MESSAGE$ = EP.TERMINAL$ + EP.CAJERO$ + transnum$	+ \
		EP.INVOICE.NBR$ + DATE$ + TIME$ + "00"										+ \	! Parte fija
		valorEft$ + zeroesValue$ + zeroesValue$ + valorSaldo$ + zeroesValue$ 		+ \ ! Parte variable
			"00" + LEFT$(allCard$ + STRING$(20," "),20) + STRING$(12," ")           + \
			RIGHT$(STRING$(6,"0") + EP.AUTORIZ.RECAUDO.TCRO$,6)                     + \
			"00" + RIGHT$(STRING$(4,"0") + EP.FVCTO.RECAUDO.TCRO$,4) + pagoEfect$   + \
			cambio$ + EP.NOMBRE$
	EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
	EP.MESSAGE$ = EP.APPL$ + epFunction$ + EP.MSGLEN$ + EP.MESSAGE$
	!
	respuestaRecaudo$ = applManagerFunction(EP.MESSAGE$,0)
	!
	IF EP.DE.AMJ.STATUS$ = "2" AND EP.DE.TRX.STATUS$ = "0" and left$(respuestaRecaudo$,1) = "1" THEN \
		ENVIO.RECAUDO.TCRO = 0 \
	ELSE \
		ENVIO.RECAUDO.TCRO = 1
END FUNCTION
!
Function isStringRecaudo21(pData1$)
	Integer*1 isStringRecaudo21
	String pData1$
	!
	If 																\!
			Len(pData1$) = 12 + Len(EP.EFT.CLAVE$) And 				\!
			Left$(pData1$, Len(EP.EFT.CLAVE$)) = EP.EFT.CLAVE$ And 	\!
			Right$(pData1$, 2) = "21" Then Begin
		isStringRecaudo21 = -1
	Endif Else Begin
		isStringRecaudo21 = 0
	Endif
End Function
!
Function isThereRecaudos Public
	Integer*1 isThereRecaudos, tmpAnswer%, isNotThere%
	Integer*2 tmpCounter%, tmpAsc%
	String tmpData$
	!
	tmpAnswer% = 0
	isNotThere% = 0
	tmpCounter% = sl.end
	While tmpAnswer% = 0 And isNotThere% = 0 And tmpCounter% >= 1
		If sl.str$(tmpCounter%) <> "" Then Begin
			tmpAsc% = Asc(sl.str$(tmpCounter%))
			If tmpAsc% = 11h Then Begin   ! linea tipo data entry 11
				ep.b$ = sl.str$(tmpCounter%) + ":"
				ep.j% = 3
				Call ep.getunpk
				tmpData$ = ep.a$
				If isStringRecaudo21(tmpData$) Then Begin
					tmpAnswer% = -1
				Endif
			Endif Else If tmpAsc% = 01h Then Begin  ! linea tipo item entry
				ep.b$ = sl.str$(tmpCounter%) + ":"
				ep.j% = 3
				Call ep.getunpk
				tmpData$ = ep.a$
				If Not isMiscPlu(tmpData$) Then Begin
					isNotThere% = -1
				Endif
			Endif
		Endif
		!
		tmpCounter% = tmpCounter% - 1
	Wend
	!
	isThereRecaudos = tmpAnswer%
End Function
!
Sub itemEntryValidation(pCode$, pReturn%, pMessage$)
	Integer*1 pReturn%, tmpMisc%, tmpRec%
	String pCode$, pMessage$
	!
	pReturn% = -1
	pMessage$ = ""
	!
	tmpMisc% = isMiscPlu(pCode$)
	tmpRec% = isThereRecaudos
	!
	If TS.IO.KEYS(1) = 70 And tmpMisc% <> 0 Then Begin
		! Si se intenta anulacion de un recaudo se debe rechazar
		pReturn% = 0
		pMessage$ = "NO SE PERMITE ANULACDE RECAUDO"
	Endif Else If tmpMisc% = 0 And tmpRec% <> 0 Then Begin
		! No debe permitir registrar un producto regular cuando
		! hay recaudos previos
		pReturn% = 0
		pMessage$ = "SOLO SE PERMITEN    RECAUDOS"
	Endif Else If tmpMisc% <> 0 And tmpRec% = 0 And TS.TOTALS(0, 0, 0) <> 0 Then Begin
		! No debe permitir un recaudo cuando ya han registrado productos regulares
		pReturn% = 0
		pMessage$ = "RECAUDO NO PERMITIDOEN TRX DE VENTA"
	Endif Else If tmpMisc% <> 0 And TPONLINE.getCustId <> "" Then Begin
		! No debe permitir un recaudo cuando hay ingresado tarjeta plata
		pReturn% = 0
		pMessage$ = "RECAUDO NO PERMITIDOCON TARJETA PLATA"
	Endif
End Sub
!
SUB VALIDACION.ENVIO.RECAUDOS.TCRO
	Integer*2 tmpCounter%,tmpCounterData%,saldoCounter%,saldoCount%
	Integer*1 partialSend%,saldoFound%
	String tmpData$(1),ecrFunc$,transnum$,allCard$,valorEft$,valorSaldo$,saldos$(2)
	!
	Dim tmpData$(6)
	Dim saldos$(100,1)
	saldoCount% = 0
	!
	for tmpCounter% = 1 to sl.end
		if sl.str$(tmpCounter%) <> "" then \
		begin
			if asc(sl.str$(tmpCounter%)) = 11h then \   ! linea tipo data entry 11
			begin
				ep.b$ = sl.str$(tmpCounter%) + ":"
				ep.j% = 3
				for tmpCounterData% = 1 to 6
					call ep.getunpk
					tmpData$(tmpCounterData%) = ep.a$
				next tmpCounterData%
				!
				!if 																		\
				!		len(tmpData$(1)) = 12 + len(EP.EFT.CLAVE$)				and		\
				!		left$(tmpData$(1),len(EP.EFT.CLAVE$)) = EP.EFT.CLAVE$	and		\
				!		right$(tmpData$(1),2) = "21" 							then	\
				!begin
				!
				If isStringRecaudo21(tmpData$(1)) Then Begin
					ecrFunc$ = Mid$(tmpData$(1),len(EP.EFT.CLAVE$) + 3,2)
					transnum$ = Mid$(tmpData$(1),len(EP.EFT.CLAVE$) + 5,6)
					allCard$ = tmpData$(2)
					valorEft$ = left$(tmpData$(4),10)
					valorSaldo$ = mid$(tmpData$(4),11,10)
					!
					! Busca la tarjeta en el arreglo de saldos
					!
					If Val(valorSaldo$) > 0 Then Begin
						saldoFound% = 0
						saldoCounter% = 0
						While saldoFound% = 0 And saldoCounter% < saldoCount%
							If saldos$(saldoCounter%,0) = allCard$ Then \
								saldoFound% = -1 \
							Else \
								saldoCounter% = saldoCounter% + 1
						Wend
						!
						If saldoFound% Then Begin
							valorSaldo$ = Right$(String$(10,"0") + Str$(Int%(Val(saldos$(saldoCounter%,1))) + Int%(Val(valorEft$))),10)
							saldos$(saldoCounter%,1) = valorSaldo$
						Endif Else Begin
							If saldoCount% < 101 Then Begin
								saldoCount% = saldoCount% + 1
								saldos$(saldoCount% - 1,0) = allCard$
								saldos$(saldoCount% - 1,1) = valorSaldo$
							Endif
						Endif
					Endif
					!
					CALL EP.DISPLAY.A.MESSAGE("Enviando recaudo...")
					partialSend% = ENVIO.RECAUDO.TCRO(ecrFunc$,transnum$,allCard$,valorEft$,valorSaldo$)
					EP.ENVIO.RECAUDO.TCRO% = -1
					CALL EP.ADD.DATA.ENTRY(left$(tmpData$(1),len(tmpData$(1)) - 2) + "20" ,				\! DE-1
						tmpData$(2),																	\! DE-2 
						tmpData$(3),																	\! DE-3
						tmpData$(4),																	\! DE-4
						tmpData$(5) + str$(partialSend%) + str$(partialSend%),							\! DE-5
						tmpData$(6))																	 ! DE-6
					If partialSend% = 0 Then \
						CALL EP.DISPLAY.A.MESSAGE("Envio recaudo OK") \
					Else \
						CALL EP.DISPLAY.A.MESSAGE("Almacenamiento      recaudo OK")
				endif
			endif
		endif
	next tmpCounter%
END SUB
!
Sub APITCRO.identifyTender Public
	If 																						\!
			(TS.IO.MOTORKEY = 94 Or TS.IO.MOTORKEY = 90 + Val(Left$(EP.TV.TJCR$, 1))) And 	\! Ingreso planes de credito
			 TS.IO.KEYS(3) = 78 And 														\! Coloca Slash
			(TS.IO.DATA$(3) = "1"  Or 														\! Variedad 1 compra con tarjeta CRO
			 TS.IO.DATA$(3) = "2" Or TS.IO.DATA$(3) = Right$(EP.TV.TJCR$, 1)) 				\! Variedad 2   compra con ALP
	Then Begin
		If TS.IO.DATA$(7) = "" Then Begin
			EP.VALOR.EFT$ = Str$(Abs(TS.BALDUE(0)))
		Endif Else Begin
			EP.VALOR.EFT$ = Right$(String$(10, "0") + Str$(Abs(Val((TS.IO.DATA$(7))))), 10)
		Endif
		!
		Call CALCULO.IVAS.BASES
		EP.TAX.EPAY%  = EP.TPVTIVT%
		EP.TAX.BASE%  = EP.BASE.DEVOL%
		EP.CASHBACK$  = String$(10, "0")
		EP.DONACION$  = String$(8, "0")
		If Abs(Val(EP.VALOR.EFT$)) < Abs(TS.TOTALS(0, 0, 0)) Then Begin
			EP.TAX.EPAY% = Round(Val(EP.VALOR.EFT$) / Float(Abs(TS.TOTALS(0, 0, 0))) \
				* EP.TAX.EPAY%, 0, 0)
			EP.TAX.BASE% = Round(Val(EP.VALOR.EFT$) / Float(Abs(TS.TOTALS(0, 0, 0))) \
				* EP.TAX.BASE%, 0, 0)
		Endif
		EP.TAX.EFT$   = Right$(String$(10, "0") + Str$(EP.TAX.EPAY%), 10) ! valor iva
		EP.TAX.BASE$  = Right$(String$(10, "0") + Str$(EP.TAX.BASE%), 10) ! valor BASE
		!
		EP.EFT.CLAVE$    = "50000000"
		EP.APPL$         = "08"
		! 
		If TS.PROCEDURE = 1 Then Begin ! Tender cashing
			If TS.IO.KEYS(1) = 70 Then Begin ! Es anulacion avance
				EP.ECR.FUNCTION$ = "52"
			Endif Else Begin ! es avance normal
				EP.ECR.FUNCTION$ = "51"
			Endif
		Endif Else Begin ! compra
			If TS.IO.KEYS(1) = 70 Then Begin ! Es anulacion compra
				If TS.TOTALS(0, 0, 0) < 0 Then Begin
					EP.ECR.FUNCTION$ = "03"
				Endif Else Begin
					EP.ECR.FUNCTION$ = "05"
				Endif
			Endif Else Begin ! es compra  
				If TS.TOTALS(0, 0, 0) < 0 Then Begin ! Verifica que sea negativo
					EP.ECR.FUNCTION$ = "05"
				Endif Else Begin
					EP.ECR.FUNCTION$ = "03"
				Endif
			Endif
		Endif
		!
		EP.INVOICE.NBR$  = Right$(String$(6, "0") + Str$(SL.HD.TRANSNUM + 1), 6)  ! numero de terminal y tienda
		EP.CUOTAS.QTY$   = Right$(String$(2, "0") + NOCUO$, 2)
		EP.COMERCIO$     = Right$(String$(10, "0") + TS.STORE$, 10)
		EP.EPAY.TERMINAL$= Right$(String$(8, "0") + TS.TERMINAL$, 8)
		EP.FECHA.POSTEO$ = Right$(DATE$, 4)
		EP.FECHA.PROC$   = DATE$ + TIME$
		EP.PROPINA$      = String$(8, "0")
		EP.RRN$          = Right$(EP.COMERCIO$, 3) + Right$(EP.EPAY.TERMINAL$, 3) + \
			Right$(EP.EPAY.TRANSNUM$, 6)
		EP.CAJERO$       = Right$(String$(10, "0") + Unpack$(TS.OPER$), 10)
		EP.FECHA.VENC$   = String$(4, "0")
		If TS.IO.KEYS(1) = 70  Then Begin
			EP.COD.PROC$     = "163000"
		Endif Else Begin
			EP.COD.PROC$     = "063000"
		Endif
		EP.COD.PROC.L$   = EP.COD.PROC$  
		!
		If TS.PROCEDURE < 1 Then Begin
			If TS.IO.KEYS(1) = 70 Then Begin
				EP.TIPO.VAR$ = searchTcroTrx(Int%(Val(EP.VALOR.EFT$)))
				If EP.TIPO.VAR$ = "" Or EP.TIPO.VAR$ = "XX" Then Begin
					EP.TIPO.VAR$ = EP.TV.TJCR$
				Endif
			Endif Else Begin
				EP.TIPO.VAR$ = EP.TV.TJCR$
			Endif
		Endif Else Begin
			EP.TIPO.VAR$ = EP.TV.TJCR$
		Endif
		EP.ANUL.TRANSNUM$= String$(6, "0")
		EP.ANUL.RRN$     = String$(12, "0")
		!
		If EP.TIPO.VAR$ <> EP.TV.TJCR$ Then Begin
			TS.IO.KEYS(7) = 90 + Int%(Val(Left$(EP.TIPO.VAR$,1)))
			TS.IO.MOTORKEY = TS.IO.KEYS(7)
			TS.IO.DATA$(3) = Right$(EP.TIPO.VAR$, 1)
			EP.TCRO% = -1                      ! se activa CRO
		Endif Else Begin
			If TS.IO.DATA$(3) = "1" Or TS.IO.DATA$(3) = Right$(EP.TV.TJCR$, 1) Then Begin ! Si es compra CRO
				EP.TCRO% = -1                      ! se activa CRO
			Endif Else Begin
				If TS.IO.DATA$(3) = "2" Then Begin ! Si es compra ALP
					EP.TALP% = -1                     ! Se activa APL
				Endif
			Endif
		Endif
		!    
		takeAcct% = -1            ! debe tomar numero de cuenta
		EP.ALL.CARD$ = ""
		!    
	Endif
End Sub
!
Function isVoidFunction(pFunction$)
	Integer*1 isVoidFunction
	String pFunction$
	!
	If \
			pFunction$ = "04" Or 	\! Anulación recaudo
			pFunction$ = "05" Or 	\! Anulación compra
			pFunction$ = "52" 		\! Anulación avance
	Then Begin
		isVoidFunction = -1
	Endif Else Begin
		isVoidFunction = 0
	Endif
End Function
!
!*********************************************************************
!                         (UETCRO14.TPV)
!                           (Ver 1.00)
!
! This routine performs the following
! - Mask account for CRO and ALP payment entries
!*********************************************************************
!
!
SUB UETCRO07  PUBLIC
  validacionRecaudosTCRO% = 0
  EP.ENVIO.RECAUDO.TCRO% = 0
  EP.SW.TCRO$ = "31"
  CALL EP.LINE.PRINT("APP TCRO v"+LEFT$(EP.SW.TCRO$,1)+"."+\
    RIGHT$(EP.SW.TCRO$,1) + " Ene07",2100H)
  CALL EP.DISPLAY.A.MESSAGE("APP TCRO v"+LEFT$(EP.SW.TCRO$,1)+"."+\
    RIGHT$(EP.SW.TCRO$,1) + " Ene07")
  WAIT ; 3000
END SUB  
!
!
SUB UETCRO14 PUBLIC
  Integer*1 validaTCro%, tmpReturn%
  Integer*2 previousData3%
  String tmpItemCode$, tmpMessage$
  Integer*2 tmpCounter%, tmpState%, tmpState1%, tmpDevice%
  !
  String tmpFranquicia$
!
!
! Invoca las rutinas para limpiar variables que se hayan podido
! quedar asignadas, antes de gestionar un medio de pago
!IF 											\!
!	TS.PROCEDURE < 1  AND 					\! Proc Vta
!	TS.IO.KEYS(4)  <> 100 AND ( 			\!	Tecla No-venta
!	(TS.IO.MOTORKEY > 90 AND 				\! Medio de pago
!	TS.IO.MOTORKEY < 99) OR 				\!
!	TS.IO.MOTORKEY = 151) THEN 				\!
!BEGIN
!
If 											\!
	((TS.IO.MOTORKEY > 90 And 				\! Medio de pago
	TS.IO.MOTORKEY < 99) Or 				\!
	TS.IO.MOTORKEY = 151) And  				\!
	TCOSEQ.expireDate$ = "" Then Begin 		\!
	Call RESET14.TCRO
Endif
!
tmpItemCode$ = ""
!
EP.secuenciaRecaudo% = SECUENCIA.RECAUDO.TCRO
!
If EP.secuenciaRecaudo% Then Begin
	tmpItemCode$ = "999901"
Endif Else If TS.IO.DATA$(2) <> "" And TS.IO.KEYS(2) = 80 Then Begin
	tmpItemCode$ = TS.IO.DATA$(2)
Endif
!
If tmpItemCode$ <> "" Then Begin
	Call itemEntryValidation(tmpItemCode$, tmpReturn%, tmpMessage$)
	If tmpReturn% = 0 Then Begin
		Call ep.display.an.error(tmpMessage$)
		DIM TS.IO.KEYS(10)
		DIM TS.IO.DATA$(10)
		TS.IO.MOTORKEY = 0
		Exit Sub		
	Endif
Endif
!
!
! CONTROLA QUE NO SE HAGA ANULADO TOTAL SIN ANTES DEVOLVER TRANSACCIONES
  IF TS.IO.KEYS(1) = 70 AND           \! Anulado
     TS.IO.KEYS(6) = 81 AND           \! Total
     TS.TEMP1I2 = 2     AND           \!
     TS.PROCEDURE < 1   AND           \!
     TS.TENDERED(0) <> 0 THEN         \!
  BEGIN
    DIM TS.IO.KEYS(10)
    DIM TS.IO.DATA$(10)
    TS.IO.MOTORKEY = 0
    CALL EP.DISPLAY.AN.ERROR("DEVUELVA MEDIOS PAGO")
    EXIT SUB
  ENDIF
  !
  IF TS.IO.MOTORKEY = 170 THEN \      ! Si es avance CRO
  BEGIN
    IF EP.TRIES.COUNT% >= 1 THEN  \      ! Si es diferente de tender cashing
    BEGIN               
      CALL EP.DISPLAY.AN.ERROR("DEMASIADOS INTENTOS")
      DIM TS.IO.KEYS(10)            
      DIM TS.IO.DATA$(10)
      TS.IO.MOTORKEY = 0            
      EP.TCRO.CARD$ = ""
    ENDIF \                           ! Fin
    ELSE \
    IF TS.PROCEDURE <> 1 THEN  \      ! Si es diferente de tender cashing
    BEGIN                             
       TS.GUIDANCE = 1003             ! Avisa al cajero
       TS.IO.MOTORKEY = 0             ! Reduce proceso
       EP.TCRO.CARD$ = ""
    ENDIF \                           ! Fin
    ELSE \
    IF TS.IO.KEYS(10) = 69 THEN \     ! y Tecla de Cuota
    BEGIN                             
       EP.ALL.CARD$ = ""
       !TS.IO.KEYS(7)   = 94           ! Se Coloca MEDIO DE PAGO CRO
       TS.IO.KEYS(7) = 90 + VAL(LEFT$(EP.TV.TJCR$,1))
       TS.IO.MOTORKEY  = TS.IO.KEYS(7)! 
       TS.IO.KEYS(3)   = 78           ! Se Coloca Slash
       !TS.IO.DATA$(3)  = "1"          ! Con Num Mini-Pag
       TS.IO.DATA$(3) = RIGHT$(EP.TV.TJCR$,1)
       NOCUO$          = RIGHT$("00"+TS.IO.DATA$(10),2) ! Toma Cuotas
       TS.IO.DATA$(10) = ""                             ! .
       TS.IO.KEYS (10) = 0                              ! .
    ENDIF \
    ELSE  \
    BEGIN                             ! Si hay error
       TS.GUIDANCE = 1003             ! Avisa al cajero
       TS.IO.MOTORKEY = 0             ! Reduce proceso
       EP.TCRO.CARD$ = ""
    ENDIF                             ! Fin
  ENDIF                                               ! Fin
!
  IF TS.IO.MOTORKEY = 236 OR         \! Si es recaudo CRO
     TS.IO.MOTORKEY = 237 THEN       \! o recaudo ALP
  BEGIN
!    CALL EP.LINE.PRINT("KEY="+STR$(TS.IO.MOTORKEY),4100H) 
!    CALL EP.LINE.PRINT("STATE="+STR$(TS.IO.STATE),4100H) 
!    CALL EP.LINE.PRINT("DATA0="+TS.IO.DATA$(0),4100H) 
!    CALL EP.LINE.PRINT("DATA1="+TS.IO.DATA$(1),4100H) 
!    CALL EP.LINE.PRINT("DATA2="+TS.IO.DATA$(2),4100H) 
!    CALL EP.LINE.PRINT("DATA3="+TS.IO.DATA$(3),4100H) 
!    CALL EP.LINE.PRINT("DATA4="+TS.IO.DATA$(4),4100H) 
!    CALL EP.LINE.PRINT("DATA5="+TS.IO.DATA$(5),4100H) 
!    CALL EP.LINE.PRINT("DATA6="+TS.IO.DATA$(6),4100H) 
!    CALL EP.LINE.PRINT("DATA7="+TS.IO.DATA$(7),4100H) 
!    CALL EP.LINE.PRINT("DATA8="+TS.IO.DATA$(8),4100H) 
!    CALL EP.LINE.PRINT("DATA9="+TS.IO.DATA$(9),4100H) 
!    CALL EP.LINE.PRINT("DATA10="+TS.IO.DATA$(10),4100H) 
!    CALL EP.LINE.PRINT("MOTORKEY="+STR$(TS.IO.MOTORKEY),4100H) 

	! Si se introdujo la secuencia antigua de Recaudo
	! se rechaza la transaccion
	If Not EP.secuenciaRecaudo% Then \
	Begin
		TS.IO.MOTORKEY = 0             ! Reduce proceso
		CALL EP.DISPLAY.AN.ERROR("SECUENCIA DE RECAUDOINVALIDA")
		TS.IO.DATA$(6) = ""            ! anula cuenta
		EP.TCRO.CARD$ = ""
		EXIT SUB                       ! Fin
	Endif

    IF TS.IO.DATA$(6) <> "" THEN \
    BEGIN 
      !---------------------------------------------------------------
      ! jsv.change.2008-10-14
      ! Para recaudos la tarjeta se debe ingresar por teclado
      ! pero debe ser ingresada completa. Por lo tanto se inhabilita
      ! la funcionalidad de completar el número de tarjeta ingresado
!      IF LEN(TS.IO.DATA$(6)) <= 7 AND  \
!         TS.IO.KEYS(6) = 72  THEN      \
!      BEGIN
!        IF TS.IO.MOTORKEY = 236 THEN   \
!          TS.IO.DATA$(6)  = "8998009" + RIGHT$(STRING$(7,"0") + TS.IO.DATA$(6), 7) + "0"
!        IF TS.IO.MOTORKEY = 237 THEN   \
!          TS.IO.DATA$(6)  = "8999009" + RIGHT$(STRING$(7,"0") + TS.IO.DATA$(6), 7) + "0"
!        TS.IO.DATA$(6) = TS.IO.DATA$(6) + EP.CHECK.DIGIT(TS.IO.DATA$(6))
!      ENDIF
      !---------------------------------------------------------------
!      
      IF LEN(TS.IO.DATA$(6)) <> 16 THEN \ ! Si la longitud no es 16 bytes
      BEGIN                            ! Si hay error
!        TS.GUIDANCE = 1006             ! Avisa al cajero
        TS.IO.MOTORKEY = 0             ! Reduce proceso
        CALL EP.DISPLAY.AN.ERROR("NRO DE CUENTA DEBE  SER DE 16 DIGITOS")
        TS.IO.DATA$(6) = ""            ! anula cuenta
        EP.TCRO.CARD$ = ""
        EXIT SUB                       ! Fin
      ENDIF                             
!      
      IF EP.CHECK.DIGIT(LEFT$(TS.IO.DATA$(6),15))  <> RIGHT$(TS.IO.DATA$(6),1) THEN \ ! Si digito de chequeo no coincide
      BEGIN                            ! Si hay error
!        TS.GUIDANCE = 1006             ! Avisa al cajero
        TS.IO.MOTORKEY = 0             ! Reduce proceso
        CALL EP.DISPLAY.AN.ERROR("DIGITO DE CHEQUEO   INVALIDO")
        TS.IO.DATA$(6) = ""            ! anula cuenta
        EP.TCRO.CARD$ = ""
        EXIT SUB                       ! Fin
      ENDIF                             
!                 
      EP.ALL.CARD$   = TS.IO.DATA$(6) 
      EP.TCRO.CARD$ = EP.ALL.CARD$
      EP.FECHA.VENC$ = STRING$(4,"0")
      EP.FINALIDAD$  = MID$(TS.IO.DATA$(10),2,1)
!
      IF TS.IO.MOTORKEY = 236 THEN \
        EP.TIPO.PROD$ = "01" \
      ELSE \
        EP.TIPO.PROD$ = "11"
!
      EP.VALOR.EFT$ = RIGHT$(STRING$(10,"0")+TS.IO.DATA$(5),10) 
      IF INT%(VAL(EP.VALOR.EFT$)) > EP.RC.MAXIMP% THEN \
      BEGIN
        TS.IO.MOTORKEY = 0             ! Reduce proceso
        CALL EP.DISPLAY.AN.ERROR("MONTO MAX=" + STR$(EP.RC.MAXIMP%))
        TS.IO.DATA$(6) = ""            ! anula cuenta
        EP.TCRO.CARD$ = ""
        EXIT SUB                       ! Fin      	
      ENDIF
      EP.TAX.BASE$  = STRING$(10,"0")
      EP.TAX.EPAY%  = 0
      EP.CASHBACK$  = STRING$(10,"0")
      EP.DONACION$  = STRING$(8,"0")
      EP.TAX.EFT$   = STRING$(10,"0") ! valor iva
!
!      call ep.line.print("TCRO14 VLR="+ep.valor.eft$+" tax="+ep.tax.eft$, 4100H) 
!
      EP.EFT.CLAVE$    = "50000000"
      EP.APPL$         = "08"
      IF TS.IO.KEYS(1) = 70 THEN \
        EP.ECR.FUNCTION$ = "04"  \
      ELSE \
        EP.ECR.FUNCTION$ = "02"
!
      EP.INVOICE.NBR$  = RIGHT$(STRING$(6,"0")+ STR$(SL.HD.TRANSNUM+1),6)  ! numero de terminal y tienda
      EP.CUOTAS.QTY$   = STRING$(2,"0")
      EP.COMERCIO$     = RIGHT$(STRING$(10,"0")+ TS.STORE$,10)
      EP.EPAY.TERMINAL$= RIGHT$(STRING$(8,"0")+ TS.TERMINAL$,8)
      EP.FECHA.POSTEO$ = RIGHT$(DATE$,4)
      EP.FECHA.PROC$   = DATE$ + TIME$
      EP.PROPINA$      = STRING$(8,"0")
      EP.RRN$          = RIGHT$(EP.COMERCIO$,3)+ RIGHT$(EP.EPAY.TERMINAL$,3) + \
                           RIGHT$(EP.EPAY.TRANSNUM$,6)
      EP.CAJERO$     = RIGHT$(STRING$(10,"0")+ UNPACK$(TS.OPER$),10)
      EP.FECHA.VENC$   = STRING$(4,"0")
      IF TS.IO.KEYS(1) = 70  THEN \
        EP.COD.PROC$    = "203000" \
      ELSE \
        EP.COD.PROC$    = "303000"
      IF TS.IO.MOTORKEY = 236 THEN \
        EP.TIPO.VAR$    = "41" \
      ELSE \
        EP.TIPO.VAR$    = "42"
      EP.ANUL.TRANSNUM$ = STRING$(6,"0")
      EP.ANUL.RRN$      = STRING$(12,"0") 
      EP.TIPO.LECT$     =  "01"
!
!----------------------------------------------------------------------
! Intenta validar la tarjeta CRO directamente contra el servidor
!----------------------------------------------------------------------
! Validación = -1 : Autorizada
! Validación = 0  : Negada
! Validación = 1  : No hay comunicación con el autorizador
      validaTCro% = VALIDACION.TCRO.REMOTA(EP.ALL.CARD$)
      If validaTCro% = 1 Then \
      Begin
      	! Se realiza la validación local porque no
      	! hubo comunicación con el servidor
      	validaTCro% = VALIDACION.TCRO.LOCAL(EP.ALL.CARD$)
      Endif
      If validaTCro% = -1 Then \
      Begin
      	EP.RECAUDO.OK% = -1
      Endif Else \
      Begin
        TS.IO.MOTORKEY = 0             ! Reduce proceso
        If validaTCro% = 2 Then \
        	CALL EP.DISPLAY.A.MESSAGE("PAGO RECHAZADO") \
        ELSE \
        	CALL EP.DISPLAY.A.MESSAGE("TARJETA NO EXISTE")
        TS.IO.DATA$(6) = ""            ! anula cuenta
        EP.TCRO.CARD$ = ""
        EXIT SUB                       ! Fin      	
      Endif
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
! Se inactiva el código que hacía directamente la validación local
! de la tarjeta contra el controlador
!----------------------------------------------------------------------
!      TS.TS11WERR$ = ""                                       !
!      EP.KEY$ = PACK$(EP.TIPO.VAR$) +                         \
!                PACK$(STRING$(24-LEN(TS.IO.DATA$(6)),"0") + TS.IO.DATA$(6))                               !
!      READ FORM "C13 C30";#6 KEY EP.KEY$;                    \!
!           EP.DUMMY$,EP.NOMBRE$                               !
!      IF TS.TS11WERR$ = "" THEN \                             !
!      BEGIN                         !                         !
!        READ FORM "C1 C12 C1 3I4";#11 KEY EP.KEY$;           \!
!             EP.DUMMY$,EP.DUMMY$,EP.ACCT.STATUS$,EP.DUMMY%,EP.DUMMY%,EP.DUMMY% !
!!        IF TS.TS11WERR$ = "" THEN \         !  se desactiva por consistencia con PAGO CRO
!!        BEGIN                               !  se desactiva por consistencia con PAGO CRO
!!          call ep.line.print("14 status="+unpack$(ep.acct.status$), 4100H) 
!          IF VAL(UNPACK$(EP.ACCT.STATUS$)) <> 56 THEN \
!          BEGIN
!                                                                  !
!             EP.RECAUDO.OK% = -1
!!            CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.ECR.FUNCTION$ +   \
!!              EP.EPAY.TRANSNUM$ + "20" ,                                           \ DE-1
!!              EP.ALL.CARD$,                                                        \ DE-2 
!!              EP.FECHA.POSTEO$ + EP.TIPO.LECT$ + EP.FINALIDAD$,                    \ DE-3
!!              EP.VALOR.EFT$ + EP.TAX.EFT$,                                         \ DE-4
!!              EP.FECHA.PROC$ + EP.PROPINA$,                                        \ DE-5
!!              EP.CAJERO$ + EP.INVOICE.NBR$ + EP.FECHA.VENC$)                       ! DE-6
!          ENDIF
!!        ENDIF                              !  se desactiva por consistencia con PAGO CRO
!      ENDIF       
!----------------------------------------------------------------------
    ENDIF
  ENDIF   
!              
          
  IF EP.TCRO%  OR  \                     ! Si ingresa compra con TCRO
     EP.TALP% THEN \                     ! si ingresa compra con ALP
  BEGIN   
  	! Si es una compra con ALP, el bin debe ser 899900
  	!
  	If EP.TALP% Then Begin
  		tmpFranquicia$ = getFranquicia(TS.IO.DATA$(10))
  	Endif Else Begin
  		tmpFranquicia$ = ""
  	Endif
  	!
	!If EP.TALP% And Left$(TS.IO.DATA$(10),6) <> "899900" Then \
	!Begin
	!
	If EP.TALP% And Left$(tmpFranquicia$, 7) <> "SAO ALP" Then Begin
    	DIM TS.IO.KEYS(10)            !
		DIM TS.IO.DATA$(10)            !
		TS.IO.MOTORKEY = 0             ! Reduce proceso
		CALL EP.DISPLAY.AN.ERROR("CUENTA ALP NO VALIDA" + EP.ALL.CARD$)
		!
		!EP.TCRO% = 0
		!EP.TALP% = 0
		!takeAcct% = 0
		!EP.TCRO.CARD$ = ""
		!
		Call RESET14.TCRO
		!
		EXIT SUB                       ! Fin	
	Endif
  	!
    EP.VERIFY.32%  = -1
    EP.MSR.DATA$   = ""
	
	Call printDebug("apitcro.TS.IO.DATA$(9)=" + TS.IO.DATA$(9) + " TCOSEQ.expireDate$=" + TCOSEQ.expireDate$)
	
	If TS.IO.DATA$(9) <> "" And TCOSEQ.expireDate$ <> "" Then Begin	    
	    EP.FECHA.VENC$ =  TCOSEQ.expireDate$
        EP.FECHA.VENC$ = LEFT$(EP.FECHA.VENC$ + STRING$(4," "),4)
        EP.FECHA.VENC.L$ = EP.FECHA.VENC$
		EP.FECHA.OK% = -1
		!TCOSEQ.expireDate$ = ""
		!
		EP.TIPO.LECT$ = "50"
		EP.ALL.CARD$ = TS.IO.DATA$(9)
		EP.TCRO.CARD$ = EP.ALL.CARD$
		takeAcct% = 0
	Endif


    IF TS.IO.KEYS(10) = 80 Or EP.TCRO.CARD$ <> "" THEN \
    BEGIN
      
	  If EP.TCRO.CARD$ <> "" Then Begin
	     If EP.TCRO% Then Begin
			EP.TIPO.PROD$ = "01"
		 Endif Else Begin
			EP.TIPO.PROD$ = "11"
		 Endif
		 Goto accountTaken
	  Endif
	  
	  IF (TS.IO.STATE = 10 or    \
          TS.IO.STATE = 20 ) and \
         takeAcct%  THEN \     ! esta digitando la cuenta
      BEGIN 
        IF EP.TCRO% THEN \
        BEGIN
          EP.TIPO.PROD$ = "01" 
        ENDIF \
        ELSE  \
        BEGIN 
          EP.TIPO.PROD$ = "11"
        ENDIF 
!        
!  modificaciones para control de digitacion de mas de 7 digitos
!

!        call ep.line.print("LEN="+STR$(LEN(TS.IO.DATA$(10)))+ \
!              "DEVICE="+STR$(TS.IO.DEVICE)+"TCRO="+STR$(EP.TCRO%), 4100H) 
        IF TS.IO.DEVICE = 1 AND  \  ! Si entro por teclado
           EP.TCRO%         THEN \  ! es tarjeta CRO
        BEGIN
          !---------------------------------------------------------------
          ! jsv.change.2008-10-14
          ! Se inhabilita por completo la entrada de tarjeta TCRO
          ! por teclado
!          IF LEN(TS.IO.DATA$(10)) > 7 THEN  \
!          BEGIN 
!            DIM TS.IO.KEYS(10)             ! anula secuencia de entrada
!            DIM TS.IO.DATA$(10)            !
!            TS.IO.MOTORKEY = 0             ! Reduce proceso
!            CALL EP.DISPLAY.AN.ERROR("NO PUEDE DIGITAR MASDE 7 DIGITOS")
!            EP.TCRO% = 0
!            EP.TALP% = 0
!            takeAcct% = 0
!            TO.USEREXIT(14) = -1
!            EXIT SUB
!          ENDIF     
          DIM TS.IO.KEYS(10)             ! anula secuencia de entrada
          DIM TS.IO.DATA$(10)            !
          TS.IO.MOTORKEY = 0             ! Reduce proceso
          CALL EP.DISPLAY.AN.ERROR("NO PUEDE DIGITAR    CUENTA TCRO")
          !
          !EP.TCRO% = 0
          !EP.TALP% = 0
          !takeAcct% = 0
          !EP.TCRO.CARD$ = ""
          !
          Call RESET14.TCRO
          !
          TO.USEREXIT(14) = -1
          EXIT SUB
          !---------------------------------------------------------------
        ENDIF                     
!
!    fin de modificaciones 
!
        IF TS.IO.DEVICE = 1 AND  \  ! Si entro por teclado
           EP.TCRO%         THEN \  ! es tarjeta CRO
        BEGIN                       
          CALL EP.SAVE.KEYS
          TO.USEREXIT(14) = 0
          IF NOT INGRESO.CLAVE THEN \ 
          BEGIN 
            DIM TS.IO.KEYS(10)             ! anula secuencia de entrada
            DIM TS.IO.DATA$(10)            !
            TS.IO.MOTORKEY = 0             ! Reduce proceso
            CALL EP.DISPLAY.AN.ERROR("NO PUEDE DIGITAR    CUENTA TCRO")
            !
            !EP.TCRO% = 0
            !EP.TALP% = 0
            !takeAcct% = 0
            !EP.TCRO.CARD$ = ""
            !
            Call RESET14.TCRO
            !
            TO.USEREXIT(14) = -1
            EXIT SUB
          ENDIF     
          CALL EP.RESTORE.KEYS                  ! Fin
          TO.USEREXIT(14) = -1
!          
          IF LEN(TS.IO.HDR$) > 11 THEN \
            TS.IO.HDR$ = LEFT$(TS.IO.HDR$,10) + "0" + \    ! se suprime llave de supervisor
                 MID$(TS.IO.HDR$,12,LEN(TS.IO.HDR$) - 11) \
          ELSE \
            TS.IO.HDR$ = LEFT$(TS.IO.HDR$,10) + "0" 
!
        ENDIF 
!      
!      IF LEN(TS.IO.DATA$(10)) = 0 THEN \ ! Si no hay numero de cuenta
!      BEGIN                            ! Si hay error
!        DIM TS.IO.KEYS(10)            !
!        DIM TS.IO.DATA$(10)            !
!        TS.IO.MOTORKEY = 0             ! Reduce proceso
!        CALL EP.DISPLAY.AN.ERROR("FALTA NUMERO CUENTA")
!        EP.TCRO% = 0
!        EP.TALP% = 0
!        EXIT SUB                       ! Fin
!      ENDIF \
!      ELSE  \
!      BEGIN 
!        CALL EP.LINE.PRINT("INGRESA"+TS.IO.DATA$(10),4100H) 

        IF LEN(TS.IO.DATA$(10)) <= 7 THEN  \
        BEGIN
          IF EP.TIPO.PROD$ = "01" THEN \
          BEGIN 
            TS.IO.DATA$(10) = "8998009" + RIGHT$(STRING$(7,"0") + TS.IO.DATA$(10), 7) + "0" 
          ENDIF \
          ELSE \
          BEGIN
            TS.IO.DATA$(10) = "8999009" + RIGHT$(STRING$(7,"0") + TS.IO.DATA$(10), 7) + "0"
          ENDIF
          TS.IO.DATA$(10)   = TS.IO.DATA$(10) + EP.CHECK.DIGIT(TS.IO.DATA$(10))
        ENDIF
!        
        If Len(TS.IO.DATA$(10)) > 16 Then TS.IO.DATA$(10) = Left$(TS.IO.DATA$(10), 16)
        
        IF LEN(TS.IO.DATA$(10)) <> 16 AND  \ ! Si la longitud no es 16 bytes
           TS.IO.STATE = 10 THEN \     ! esta digitando la cuenta 
        BEGIN                            
          DIM TS.IO.KEYS(10)            !
          DIM TS.IO.DATA$(10)            !
          TS.IO.MOTORKEY = 0             ! Reduce proceso
          CALL EP.DISPLAY.AN.ERROR("NUMERO CUENTA ERRADOLONGITUD INVALIDA " + Str$(LEN(TS.IO.DATA$(10))))
          !
          !EP.TCRO% = 0
          !EP.TALP% = 0
          !takeAcct% = 0
          !EP.TCRO.CARD$ = ""
          !
          Call RESET14.TCRO
          !
          EXIT SUB                       ! Fin
        ENDIF                                        
!      
!        IF EP.CHECK.DIGIT(LEFT$(TS.IO.DATA$(10),15))  <> RIGHT$(TS.IO.DATA$(10),1) THEN  ! Si digito de chequeo no coincide
				IF NOT valida.digito.chequeo(TS.IO.DATA$(10)) THEN \
        BEGIN                            ! Si hay error
          DIM TS.IO.KEYS(10)            !
          DIM TS.IO.DATA$(10)            !
          TS.IO.MOTORKEY = 0             ! Reduce proceso
          CALL EP.DISPLAY.AN.ERROR("NUMERO CUENTA ERRADOERROR DIGITO CHEQUEO")
          !
          !EP.TCRO% = 0
          !EP.TALP% = 0
          !takeAcct% = 0
          !EP.TCRO.CARD$ = ""
          !
          Call RESET14.TCRO
          !
          EXIT SUB                       ! Fin
        ENDIF                             
!        
        EP.ALL.CARD$ = TS.IO.DATA$(10)
        EP.TCRO.CARD$ = EP.ALL.CARD$
                
        takeAcct% = 0
!
        Call printDebug("Antes de pedir fecha")
        Call debugKeys
        !
        tmpState% = TS.IO.STATE
        tmpState1% = TS.IO.NEXTSTATE
        tmpDevice% = TS.IO.DEVICE
        !
        CALL EP.SAVE.KEYS
        TO.USEREXIT(14) = 0
        If TCOSEQ.expireDate$ <> "" Then Begin
			EP.KEYB.DATA$ = TCOSEQ.expireDate$
			TCOSEQ.expireDate$ = ""
			EP.FECHA.OK% = -1
		Endif Else Begin
			EP.KEYB.DATA$ = ""
			EP.FECHA.OK%  = 0
		Endif
        WHILE NOT EP.FECHA.OK% 
          CALL EP.GET.KBDATA("FECHA VENCE: mmaa??  ","0101" , "1299", \
            EP.KEYB.DATA$)
  !         call ep.line.print("LEN="+STR$(LEN(EP.KEYB.DATA$))+"DATA="+EP.KEYB.DATA$,4100H) 

          IF LEN(EP.KEYB.DATA$) = 4            AND   \
             VAL(LEFT$(EP.KEYB.DATA$,2)) >= 1  AND   \
             VAL(LEFT$(EP.KEYB.DATA$,2)) <= 12 AND   \
             VAL(MID$(EP.KEYB.DATA$,3,2)) >= 1 AND   \
             VAL(MID$(EP.KEYB.DATA$,3,2)) <= 99 THEN \
          BEGIN
            EP.FECHA.OK% = -1
          ENDIF
        WEND
        EP.FECHA.VENC$ =  EP.KEYB.DATA$  ! MID$(EP.KEYB.DATA$,3,2) + LEFT$(EP.KEYB.DATA$,2)
        EP.FECHA.VENC$ = LEFT$(EP.FECHA.VENC$ + STRING$(4," "),4)
        EP.FECHA.VENC.L$ = EP.FECHA.VENC$
!
        CALL EP.RESTORE.KEYS
        !
        TS.IO.STATE = tmpState%
        TS.IO.NEXTSTATE = tmpState1%
        TS.IO.DEVICE = tmpDevice%
        !
        Call printDebug("Despues de pedir fecha")
        Call debugKeys
        !
        TO.USEREXIT(14) = -1
!        
        IF TS.IO.DEVICE = 1 THEN \
          EP.TIPO.LECT$ = "01"   \
        ELSE \
          EP.TIPO.LECT$ = "50" 
!
        IF EP.ECR.FUNCTION$ = "51" OR \
           EP.ECR.FUNCTION$ = "52" THEN \
        BEGIN 
          CALL EP.SAVE.KEYS
          TO.USEREXIT(14) = 0
          EP.KEYB.DATA$ = ""
          CALL EP.GET.KBDATA("Digite Id Cliente:","1" , "99999999999", \
              EP.KEYB.DATA$)

!         call ep.line.print("LEN="+STR$(LEN(EP.KEYB.DATA$))+"DATA="+EP.KEYB.DATA$,4100H) 

          IF TS.IO.MOTORKEY = 73 THEN \
          BEGIN
            DIM TS.IO.KEYS(10)            !
            DIM TS.IO.DATA$(10)            !
            TS.IO.MOTORKEY = 0             ! Reduce proceso
            !
            !EP.TCRO% = 0
            !EP.TALP% = 0
            !takeAcct% = 0
            !EP.TCRO.CARD$ = ""
            !
            Call RESET14.TCRO
            !
            TO.USEREXIT(14) = -1
            EXIT SUB                       ! Fin
          ENDIF
!          
          EP.IDCTE$ = LEFT$(EP.KEYB.DATA$ + STRING$(11," "),11)
          CALL EP.RESTORE.KEYS
          TO.USEREXIT(14) = -1
!
        ENDIF
      ENDIF
!   
!
!   Invocacion de rutina del API
!
!
	  accountTaken:
	  
	  IF EP.TCRO.CARD$ <> "" THEN \
	  BEGIN
	      EP.TCRO% = 0             ! evita reproceso de tarjetas
	      EP.TALP% = 0             ! evita reproceso de tarjetas
	      EP.TCRO.CARD$ = ""
	!
	      EP.FINALIDAD$    = "0"
	      EP.TCRO.OK%      = -1
	      TS.GUIDANCE      = 0             !
	      EP.TRX.STATUS$   = "0"           !  communications OK
	      EP.AMJ.STATUS$   = "0"           !  sin iniciar transaccion
	      EP.EFT.OK%       = 0
	      EP.APPL$         = "08"
	      EP.CARD.NUMBER$  = RIGHT$(EP.ALL.CARD$,4)
	      EP.USER.DATA$    = EP.TAX.BASE$ + EP.CASHBACK$
	!
	      EP.SAVE.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
	      EP.EPAY.TRANSNUM$ = EP.NEW.TRANSNUM
	      EP.ECR.TRANSNUM$  = EP.EPAY.TRANSNUM$  !  returns transaction number used
	    
	    !
	    previousData3% = ts.io.keys(3)  
	    ! Revisa si hay un tipo variedad asignado
        ! al bin
        tvBin$ = getTipoVariedadTCRO(EP.ALL.CARD$)
        If tvBin$ = "00" Then Begin
        	tvBin$ = ""
        	call TRANSLATE.TV.CODE(LEFT$(EP.ALL.CARD$,6),tvBin$)
        Endif
        if tvBin$ <> "" and tvBin$ <> "00" and tvBin$ <> EP.TIPO.VAR$ Then \
        Begin
        	EP.TIPO.VAR$ = tvBin$
        	EP.TCRO.ALT.TV$ = tvBin$
        	TS.IO.KEYS(7)  = 90 + VAL(LEFT$(EP.TIPO.VAR$,1))
			If TCOSEQ.expireDate$ = "" Then Begin
				!TS.IO.DATA$(7) = Str$(Abs(SL.TE.AMTTENDE))
				!If SL.TE.AMTTENDE < 0 Then Begin
				!	TS.IO.KEYS(1) = 70
				!Endif
				!
				Call printDebug("*SL.TE.TENDTYPE=" + Str$(SL.TE.TENDTYPE) + "  SL.TE.TENDVAR=" + Str$(SL.TE.TENDVAR))
				!
				SL.TE.TENDTYPE = Int%(Val(Left$(tvBin$, 1)))
				SL.TE.TENDVAR = Int%(Val(Right$(tvBin$, 1)))
				!
				Call printDebug(">SL.TE.TENDTYPE=" + Str$(SL.TE.TENDTYPE) + "  SL.TE.TENDVAR=" + Str$(SL.TE.TENDVAR))
			Endif Else Begin
				TCOSEQ.expireDate$ = ""
			Endif
        	TS.IO.DATA$(3) = RIGHT$(EP.TIPO.VAR$,1)
        	!TS.IO.MOTORKEY = TS.IO.KEYS(7)
        	ts.io.keys(3) = 78
        	previousData3% = -1000
        	!
        	!Call printDebug("*SL.TE.TENDTYPE=" + Str$(SL.TE.TENDTYPE) + "  SL.TE.TENDVAR=" + Str$(SL.TE.TENDVAR))
        	!!
        	!SL.TE.TENDTYPE = Int%(Val(Left$(tvBin$, 1)))
        	!SL.TE.TENDVAR = Int%(Val(Right$(tvBin$, 1)))
        	!!
        	!Call printDebug(">SL.TE.TENDTYPE=" + Str$(SL.TE.TENDTYPE) + "  SL.TE.TENDVAR=" + Str$(SL.TE.TENDVAR))
        Endif
        ! Si el tipo de pago pide institucion
        ! va a pasar automaticamente despues
        ! por la rutina de validacion de medios de pago
        ! para descuentos. Si no pide institucion,
        ! es necesario invocar manualmente dicha
        ! rutina de validacion
        IF isTVEntity(EP.TIPO.VAR$) = 0 THEN \
        BEGIN
        	ts.io.keys(3) = 78
        	TS.IO.KEYS(7)  = 90 + VAL(LEFT$(EP.TIPO.VAR$,1))
        	TS.IO.DATA$(3) = RIGHT$(EP.TIPO.VAR$,1)
        	!call DSCPAG14B
        ENDIF
		!
		If TCOSEQ.entity% > 0 Then Begin
			Call menpag.ignoreTender
			TCOSEQ.tendType% = Int%(Val(EP.TIPO.VAR$))
		Endif
        !
        IF EP.ALL.CARD$ = "" THEN \
        BEGIN
          EP.TCRO% = 0
          EP.TALP% = 0
          takeAcct% = 0
          EP.TCRO.CARD$ = ""
          EXIT SUB                       ! Fin
        ENDIF
        if previousData3% <> -1000 then \
	    	ts.io.keys(3) = previousData3%    
	    	
	      Call printDebug("Calling EP.TARJ.CRED.14 from TCRO14")
	      Call printDebug("EP.VALOR.EFT$=" + EP.VALOR.EFT$ + \
	      		" SL.TE.AMTTENDE="+Str$(SL.TE.AMTTENDE) + \
	      		" TS.IO.KEYS(1)="+Str$(TS.IO.KEYS(1)) + \
	      		" TS.IO.DATA$(7)="+TS.IO.DATA$(7) + \
	      		" EP.ECR.FUNCTION$="+EP.ECR.FUNCTION$)
	      
	      SL.TE.AMTTENDE = Int%(Abs(Val(EP.VALOR.EFT$)))
	      If isVoidFunction( EP.ECR.FUNCTION$ ) Then Begin
	      	SL.TE.AMTTENDE = -SL.TE.AMTTENDE
	      	TS.IO.KEYS(1) = 70
	      Endif
	      !
	      !----------------------------------------------------------------------------------
	      ! 2021-11-30 jsv
	      ! En algunos casos, en la secuencia de TCRO se pierde el valor de TS.IO.KEYS(1)
	      ! Por esta razón, es mejor no guiarse por este criterio para determinar si la
	      ! operación es una anulación o un pago
	      !----------------------------------------------------------------------------------
	      !If TS.IO.KEYS(1) = 70 Then Begin
	      !	TS.IO.DATA$(7) = Str$(Abs(Val(EP.VALOR.EFT$)))
	      !Endif Else Begin
	      !	TS.IO.DATA$(7) = Str$(SL.TE.AMTTENDE)
	      !Endif
	      !
	      TS.IO.DATA$(7) = Str$(Abs(Val(EP.VALOR.EFT$)))
	      !
	      !----------------------------------------------------------------------------------
	      !
	      CALL EP.TARJ.CRED.14(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
	         EP.INVOICE.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.CUOTAS.QTY$, \
	         EP.APPROV.CODE$, EP.APPROV.DESC$, \
	         EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$, EP.USER.DATA$)        
	! 
	      IF EP.AMJ.STATUS$ <> "2"  THEN \ !    evaluate HOST command reponse
	      BEGIN
	        CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
	        CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
	        TS.IO.MOTORKEY = 0
	        !
	        !EP.TCRO% = 0
            !EP.TALP% = 0
            !takeAcct% = 0
            !EP.TCRO.CARD$ = ""
            !
            Call RESET14.TCRO
            !
            EXIT SUB                       ! Fin
	      ENDIF \
	      ELSE \
	      IF EP.TRX.STATUS$ <> "0" THEN \  !    whether communication error found
	      BEGIN
	        CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
	        CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
	        TS.IO.MOTORKEY = 0
	        !
	        !EP.TCRO% = 0
            !EP.TALP% = 0
            !takeAcct% = 0
            !EP.TCRO.CARD$ = ""
            !
            Call RESET14.TCRO
            !
            EXIT SUB                       ! Fin
	      ENDIF \
	      ELSE \
	      BEGIN 
	        EP.EFT.OK%     = -1
	!            TS.ACNUM$      = EP.ALL.CARD$
	!            TS.IO.DATA$(9) = EP.ALL.CARD$
	!            TS.IO.KEYS(9)  = 90
	        IF TS.PROCEDURE = 2 THEN \
	          TO.XCHGLIM = 0             ! no pedir llave de supervisor para intercambio de pagos
	! jsv.add.2008/03/27
	! Se guardan los valores de eft
	! para tarjeta cro
	        EP.TCRO.VALOR.EFT$ = EP.VALOR.EFT$
	        EP.TCRO.TAX.EFT$   = EP.TAX.EFT$
	        EP.TCRO.TAX.BASE$  = EP.TAX.BASE$
	!
	        !
	        !--------------------------------------------------------------------------------
	        ! 2021-09-06 jsv
	        ! Asignación de valor a la variable de número de tarjeta o número de cuenta y 
	        ! al puntero de inicio de voucher
	        !--------------------------------------------------------------------------------
	        EP.REV.ACCOUNT$ = EP.ALL.CARD$
	        EP.REV.VOUCHER.START% = EP.POINTER1%
	        !--------------------------------------------------------------------------------
	        !
	        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.ECR.FUNCTION$ +    \
	           EP.EPAY.TRANSNUM$ + "01" ,                                           \ DE-1
	           EP.ALL.CARD$,                                                        \ DE-2 
	           EP.FECHA.POSTEO$ + EP.TIPO.LECT$ + EP.FINALIDAD$,                    \ DE-3
	           EP.VALOR.EFT$ + EP.TAX.EFT$,                                         \ DE-4
	           EP.FECHA.PROC$ + EP.PROPINA$,                                        \ DE-5
	           EP.CAJERO$ + EP.INVOICE.NBR$ + EP.FECHA.VENC$)                       ! DE-6
	        EP.INICIO.FOUND% = -1
	        !
	        !---------------------------------------------------------------------
	        ! 2018-05-12 jsv
	        !---------------------------------------------------------------------
	        ! Se almacena índice correspondiente al data entry 01
	        !---------------------------------------------------------------------
	        EP.DATA1.INDEX% = SL.END
	        !
	        Call traceApplication("EP.DATA1.INDEX%=" + Str$(EP.DATA1.INDEX%), "APITCRO")
	        !---------------------------------------------------------------------
	      ENDIF 
	  ENDIF
    ENDIF
  ENDIF  
!
!  IF EP.TCRO% OR   \
!     EP.TALP% THEN \                   
!  BEGIN 
!    IF TS.IO.MOTORKEY = 73 THEN        \
!    BEGIN
!      call ep.line.print("TCRO14 BORRAR="+STR$(TS.IO.MOTORKEY), 4100H) 
!      EP.TCRO% = 0
!      EP.TALP% = 0
!    ENDIF 
!  ENDIF
!
!  call ep.line.print("DATA 7 ="+TS.IO.DATA$(7)+ " DUE="+STR$(TS.BALDUE(0)) , 4100H) 
   !
   If EP.TCRO% = 0 And EP.TALP% = 0 Then Begin
      Call APITCRO.identifyTender
   Endif
!
!
END SUB

SUB UETCRO20 PUBLIC
!*********************************************************************
!                         (UETCRO20.TPV)
!                           (Ver 1.00)
!
! This routine performs the following
! - Add data entry for CRO and ALP payment entries
!
!------------------------------------------------------------------
!
String nuevoSaldo$
Integer*1 isPluRecaudo
!
IF TS.LINETYPE = 18 AND \
	validacionRecaudosTCRO% AND \
	NOT TS.TRAINING  THEN \	
BEGIN
	IF ts.io.keys(1) <> 70 AND ts.io.keys(6) <> 81 THEN \
		CALL VALIDACION.ENVIO.RECAUDOS.TCRO
	validacionRecaudosTCRO% = 0
ENDIF
!
!  if ts.linetype = 1 and                        \ ! si es linea item
!     ts.linedata < 2 and                        \ ! si es linea item
!     right$(ts.prtbuf$,6) <> string$(6," ") then \ ! con dato de valor
!    call ep.line.print("20rec ok=" + str$(EP.RECAUDO.OK%) + " item=" + sl.it.itemcode$, 4100H) 
  IF TS.LINETYPE = 1 AND                        \ ! Si es linea item
     RIGHT$(TS.PRTBUF$,6) <> STRING$(6," ") AND \ ! con dato de Valor
     EP.RECAUDO.OK%  AND                        \ ! ha ingresado secuencia de recaudo
     isMiscPlu(SL.IT.ITEMCODE$) = -1  Then      \ ! ha ingresado PLU de recaudo valido
  BEGIN
    IF LEFT$(SL.IT.ITEMCODE$,6) = "999902" THEN \
      EP.TIPO.PROD$ = "11" \
    ELSE \
      EP.TIPO.PROD$ = "01" 
!
    EP.VALOR.EFT$ = RIGHT$(STRING$(10,"0")+STR$(ABS(SL.IT.XPRICE)),10)
    !
!	IF INT%(VAL(EP.VALOR.EFT$)) > EP.RC.MAXIMP% THEN \
!	BEGIN
!		CALL EP.DISPLAY.AN.ERROR("MONTO MAX=" + STR$(EP.RC.MAXIMP%))
!		EP.TCRO.CARD$ = ""
!		EP.RECAUDO.OK% = 0
!		EXIT SUB                       ! Fin      	
!	ENDIF
	!
	EP.SAVE.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
    EP.EPAY.TRANSNUM$ = EP.NEW.TRANSNUM
    EP.ECR.TRANSNUM$  = EP.EPAY.TRANSNUM$  !  returns transaction number used
	!
	If EP.VALOR.SALDO$ <> "" And EP.VALOR.SALDO$ <> "-000000001" Then Begin
		nuevoSaldo$ = Right$(String$(10,"0") + Str$(Int%(Val(EP.VALOR.EFT$)) + Int%(Val(EP.VALOR.SALDO$))),10)
	Endif Else \
		nuevoSaldo$ = String$(10,"0")
	!
    CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.ECR.FUNCTION$ +   \
      EP.EPAY.TRANSNUM$ + "21" ,                                           \ !DE-1
      EP.ALL.CARD$,                                                        \ !DE-2 
      EP.FECHA.POSTEO$ + EP.TIPO.LECT$ + EP.FINALIDAD$ + EP.TIPO.PROD$,    \ !DE-3
      EP.VALOR.EFT$ + nuevoSaldo$,                                         \ !DE-4
      EP.FECHA.PROC$ + EP.SW.VERSION$ ,                                    \ !DE-5
      EP.CAJERO$ + EP.INVOICE.NBR$ + EP.FECHA.VENC$)                       ! DE-6
    EP.RECAUDO.OK% = 0
    validacionRecaudosTCRO% = -1
  ENDIF
!  
  IF TS.LINETYPE = 2 THEN \
  BEGIN
    debugTcro% = 0
    takeAcct%  = 0
    IF EP.TCRO% OR EP.TALP% THEN \
    BEGIN 
      EP.TCRO% = 0
      EP.TALP% = 0
    ENDIF
  ENDIF
!  
END SUB
!*********************************************************************
!                         (UETCRO32.TPV)
!                           (Ver 1.00)
!
! This routine performs the following
! - Add data entry for CRO and ALP payment entries
!*********************************************************************
!
                                 
SUB UETCRO32 PUBLIC
	!
	! Variables a tener en cuenta en caso de un posible reverso
	String tmpRevCroAppl$, tmpRevCroFunct$, tmpRevCroCons$, tmpRevCroInv$, tmpRevCroDate$
	!
	tmpRevCroAppl$ = ""
	tmpRevCroFunct$ = ""
	tmpRevCroCons$ = ""
	tmpRevCroInv$ = ""
	tmpRevCroDate$ = ""
	!
!
  EP.APPROV.CODE$  = ""             !  sin aprobacion 
!
  IF EP.TCRO.OK% THEN \  
  BEGIN
    EP.TCRO.OK% = 0                    !  no inicia de nuevo
!    call ep.line.print("risk="+str$(SL.TE.STATUS), 4100H)
    EP.CARD.NUMBER$  = RIGHT$(EP.ALL.CARD$,4)
    EP.TCRO.CARD$ = ""
    EP.FECHA.PROC$   = DATE$ + TIME$
    EP.TRX.STATUS$   = "0"
    EP.AGENCIA$      = "0"
    EP.MINIPAGARE$   = STRING$(7,"0")
    EP.DEFUSER1$     = "0"
    EP.USER1$        = "0"
    EP.USER2$        = "0"
    EP.USER3$        = "0"
!    
    IF EP.EFT.ACTIVO%  THEN \             !  activo en transacciones en linea  
    BEGIN
      IF EP.EFT.OK% THEN \                !  viene medio de pago TEF
      BEGIN
        EP.EFT.OK% = 0                    !  no inicia de nuevo
        EP.TRX.STATUS$   = "1"            !  sin communications 
        EP.AMJ.STATUS$   = "0"            !  sin iniciar transaccion

!      
!        call ep.line.print("ant32Apl=" + ep.appl$ + "fnc=" + ep.ecr.function$ + "ast=" + ep.amj.status$ + \
!            "tst=" + ep.trx.status$ + "tr=" + ep.ecr.transnum$ , 4100H)
!
! jsv.add.2008/03/27
! Se restauran los valores de eft
! para tarjeta cro
        EP.VALOR.EFT$ = EP.TCRO.VALOR.EFT$
        EP.TAX.EFT$   = EP.TCRO.TAX.EFT$
        EP.TAX.BASE$  = EP.TCRO.TAX.BASE$
        EP.USER.DATA$ = EP.TAX.BASE$ + EP.CASHBACK$
!
	!
	! Se guarda la información necesaria para un eventual reverso
	tmpRevCroAppl$ = EP.APPL$
	tmpRevCroFunct$ = EP.ECR.FUNCTION$
	tmpRevCroCons$ = EP.ECR.TRANSNUM$
	tmpRevCroInv$ = EP.INVOICE.NBR$
	tmpRevCroDate$ = DATE$ + TIME$ 
	!
		PROCE% = 1
        CALL EP.TARJ.CRED.32(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
            EP.INVOICE.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.CUOTAS.QTY$,  \
            EP.APPROV.CODE$, EP.APPROV.DESC$, \
            EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$, EP.USER.DATA$)             
!
	!
	tmpRevCroDate$ = EP.ECR.DATETIME$
	!
!
!        call ep.line.print("dsp32Apl=" + ep.appl$ + "fnc=" + ep.ecr.function$ + "ast=" + ep.amj.status$ + \
!            "tst=" + ep.trx.status$ + "tr=" + ep.ecr.transnum$ , 4100H)
!        call ep.line.print("cod=" + ep.approv.code$+"aut="+ep.auth.number$+"dsc="+ep.approv.desc$ , 4100H)
!

        EP.TRIES.COUNT% = EP.TRIES.COUNT% + 1
        
        IF EP.AMJ.STATUS$ <> "2" OR \       ! Si falla en Appl Manager 
           EP.TRX.STATUS$ <> "0" THEN \     ! Si falla comunicacion
        BEGIN                               ! inicia autorizacion local
          IF EP.ECR.FUNCTION$ = "51" OR \
             EP.ECR.FUNCTION$ = "52" THEN \
          BEGIN
            TS.USER.RETURN = 99        ! medio de pago rechazado por falta de linea
            !EP.EFT.TRX%    = 0
            EP.EFT.DETAIL% = 0
            EP.EFT.SMA%    = 0
            PROCE% = 0                 ! no generar voucher de autorizacion local
          ENDIF \
          ELSE \
          BEGIN 
            IF EP.AMJ.STATUS$ <> "2"  THEN \  ! evaluate HOST command reponse
            BEGIN
              CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
              CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
            ENDIF \
            ELSE \
            IF EP.TRX.STATUS$ <> "0" THEN \   !    whether communication error found
            BEGIN
              CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
              CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
            ENDIF          
            EP.TIPO.AUTH$ = "3"
            EP.AUTH.NUMBER$  = STRING$(6,"0")
            EP.RRN$  = RIGHT$(TS.STORE$,3)+RIGHT$(TS.TERMINAL$,3)+ EP.EPAY.TRANSNUM$
            EP.COMERCIO$     = RIGHT$(STRING$(10,"0")+ TS.STORE$,10)
            EP.EPAY.TERMINAL$= RIGHT$(STRING$(8,"0")+ TS.TERMINAL$,8)
            EP.FECHA.POSTEO$ = RIGHT$(DATE$,4)
            EP.FECHA.PROC$ = DATE$ + TIME$
!
            IF MATCH(EP.TRX.STATUS$,EP.NO.TRANSMITION$,1) <> 0 THEN \   !    whether communication error found
            BEGIN
              EP.APPROV.CODE$ = "10"    ! rechazado por tarjeta invalida
              !EP.EFT.TRX%    = 0
              EP.EFT.DETAIL% = 0
              EP.EFT.SMA%    = 0
              PROCE% = 0                 ! no generar voucher de autorizacion local
            ENDIF \
            ELSE  \
            BEGIN 
              IF SL.TE.STATUS = 0  THEN \
              BEGIN 
                EP.APPROV.CODE$ = "11"  ! rechazado por codigo de estado invalido en SL.TE.STATUS
                !EP.EFT.TRX%    = 0
                EP.EFT.DETAIL% = 0
                EP.EFT.SMA%    = 0
                PROCE% = 0                 ! no generar voucher de autorizacion local
              ENDIF \
              ELSE \
              IF SL.TE.STATUS = 80 AND (EP.TRX.STATUS$ = "2" OR EP.TRX.STATUS$ = "3") AND EP.TIPO.VAR$ = EP.TV.TJCR$ THEN \
              BEGIN 
                EP.APPROV.CODE$ = "00"   ! aprobado localmente 
              ENDIF \
              ELSE \
              BEGIN 
                EP.APPROV.CODE$ = "12"   ! medio de pago rechazado localmente
                !EP.EFT.TRX%    = 0
                EP.EFT.DETAIL% = 0
                EP.EFT.SMA%    = 0
                IF SL.TE.STATUS = 80 THEN SL.TE.STATUS = 83
                PROCE% = 0                 ! no generar voucher de autorizacion local
              ENDIF 
            ENDIF
            CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.ECR.FUNCTION$ +   \
              EP.EPAY.TRANSNUM$ + "02" ,                                           \ DE-1
              EP.COMERCIO$ + EP.EPAY.TERMINAL$ + EP.APPROV.CODE$,                  \ DE-2 
              EP.FECHA.POSTEO$ + EP.COD.PROC.L$ + EP.TIPO.PROD$ + EP.TIPO.AUTH$+     \
              "0" + EP.AUTH.NUMBER$,                                               \ DE-3
              EP.VALOR.EFT$ + EP.TAX.EFT$,                                         \ DE-4
              EP.FECHA.PROC$,                                                      \ DE-5
              EP.RRN$ + EP.CUOTAS.QTY$ + EP.TIPO.VAR$ + EP.TRX.STATUS$ +"000")     ! DE-6
  !
            IF EP.APPROV.CODE$ = "00" THEN \
            BEGIN
              CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.ECR.FUNCTION$ + \
                EP.EPAY.TRANSNUM$ + "03" ,                                         \ DE-1
                EP.TAX.BASE$ + EP.CASHBACK$,                                       \ DE-2 
                EP.FECHA.PROC$ + EP.SW.VERSION$,                                      \ DE-3
                EP.ANUL.TRANSNUM$ + EP.AGENCIA$ + EP.MINIPAGARE$ + EP.DEFUSER1$ +  \
                EP.USER1$ + EP.USER2$ + EP.USER3$ ,                                \ DE-4
                DATE$ + TIME$ + EP.CARD.BIN.L$,                                      \ DE-5
                EP.ANUL.RRN$ + EP.FECHA.VENC.L$)                                     ! DE-6
            ENDIF
            IF EP.APPROV.CODE$ = "10" THEN \   !   tarjeta invalida
            BEGIN
              TS.USER.RETURN = 99    ! rechazado por tarjeta invalida
              PROCE% = 0             ! evita que salga voucher local
            ENDIF \
            ELSE  \
            IF EP.APPROV.CODE$ = "11" Or EP.APPROV.CODE$ = "12"  THEN \  ! valor invalido en el estado de la cuenta
            BEGIN 
              TS.USER.RETURN = 99  ! rechazado por codigo de estado invalido en SL.TE.STATUS  
              PROCE% = 0             ! evita que salga voucher local
            ENDIF \
            ELSE \
              TS.USER.RETURN  = 0     ! retoma estado de aprobacion o rechazo local
          ENDIF    
        ENDIF \       ! fin de autorizacion local 
        ELSE \       ! respuesta correcta de servidor de autorizacion 
        IF EP.APPROV.CODE$ = "00" THEN \         ! transaccion aprobada en linea 
        BEGIN
!      
!    approval OK. download transaction data
!     
!      CALL EP.DISPLAY.AN.ERROR("T32="+STR$(SL.TE.TENDTYPE)+" V="+STR$(SL.TE.TENDVAR))  
!      CALL EP.LINE.PRINT("TS.NUMTNDRS=",4100H) 
!      FOR EP.I% = 1 TO TO.NUMTNDR
!        CALL EP.LINE.PRINT(STR$(EP.I%)+"="+STR$(TS.NUMTNDRS(EP.I%)),4100H)
!      NEXT EP.I%
!      CALL EP.LINE.PRINT("TS.TENDVAMT=",4100H) 
!      FOR EP.I% = 1 TO TO.NUMTNDR
!        CALL EP.LINE.PRINT(STR$(EP.I%)+"="+STR$(TS.TENDVAMT(EP.I%)),4100H)
!      NEXT EP.I%
!     IF EP.TV.POS% <> TS.TDR.INDEX THEN \
!      BEGIN
!        TS.NUMTNDRS(EP.TV.POS%) = TS.NUMTNDRS(TS.TDR.INDEX)
!        TS.TENDVAMT(EP.TV.POS%) = TS.TENDVAMT(TS.TDR.INDEX)
!        TS.NUMTNDRS(TS.TDR.INDEX) = 0
!        TS.TENDVAMT(TS.TDR.INDEX) = 0
!        TS.TDR.INDEX    = EP.TV.POS%
!      ENDIF
!      EP.CARD.BIN$    = EP.USER.DATA$
!      SL.TE.TENDTYPE  = VAL(LEFT$(EP.TIPO.VAR$,1))
!      SL.TE.TENDVAR   = VAL(MID$(EP.TIPO.VAR$,2,1))
!      TS.ACNUM$       = EP.CARD.BIN$
          EP.TIPO.AUTH$ = "2"
          TS.USER.RETURN  = 80     !  aprobado en linea para supermarket application
          EP.EFT.TRX%     = -1
          EP.EFT.DETAIL%  = -1
          EP.EFT.SMA%     = -1
          PROCE% = 0                 ! no generar voucher de autorizacion local
          IF TS.PROCEDURE = 2 THEN \
            TO.XCHGLIM    = 0
        ENDIF \
        ELSE \     ! transaccion no aprobada
        BEGIN 
          IF EP.APPROV.DESC$ = STRING$(20," ") THEN \ 
            CALL TRANSLATE.ISO.CODE(EP.APPROV.CODE$,EP.APPROV.DESC$)
          CALL EP.DISPLAY.AN.ERROR(EP.APPROV.DESC$)
          TS.USER.RETURN = 18        ! medio de pago rechazado en linea
          !EP.EFT.TRX%    = 0
          EP.EFT.DETAIL% = 0
          EP.EFT.SMA%    = 0
          PROCE% = 0                 ! no generar voucher de autorizacion local
        ENDIF
      ENDIF     ! end trx viene medio de pago  ok
    ENDIF \     ! end of transacciones en linea
    ELSE  \     ! no esta activo transacciones en linea
    BEGIN
      EP.TIPO.AUTH$ = "3"     ! Autorizacion local
      EP.AUTH.NUMBER$  = STRING$(6,"0")

      IF SL.TE.STATUS = 0  THEN \
      BEGIN 
        EP.APPROV.CODE$ = "11"  ! rechazado por codigo de estado invalido en SL.TE.STATUS
        !EP.EFT.TRX%    = 0
        EP.EFT.DETAIL% = 0
        EP.EFT.SMA%    = 0
      ENDIF \
      ELSE \
      IF SL.TE.STATUS = 80  THEN \
      BEGIN 
        EP.APPROV.CODE$ = "00"   ! aprobado localmente 
      ENDIF \
      ELSE \
      BEGIN 
        EP.APPROV.CODE$ = "12"   ! medio de pago rechazado localmente
        !EP.EFT.TRX%    = 0
        EP.EFT.DETAIL% = 0
        EP.EFT.SMA%    = 0
      ENDIF 
!
      CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.ECR.FUNCTION$ +   \
        EP.EPAY.TRANSNUM$ + "02" ,                                           \ DE-1
        EP.COMERCIO$ + EP.EPAY.TERMINAL$ + EP.APPROV.CODE$,                  \ DE-2 
        EP.FECHA.POSTEO$ + EP.COD.PROC$ + EP.TIPO.PROD$ + EP.TIPO.AUTH$+     \
        "0" + EP.AUTH.NUMBER$,                                               \ DE-3
        EP.VALOR.EFT$ + EP.TAX.EFT$,                                         \ DE-4
        EP.FECHA.PROC$,                                                      \ DE-5
        EP.RRN$ + EP.CUOTAS.QTY$ + EP.TIPO.VAR$ + EP.TRX.STATUS$ +"000")     ! DE-6
!
      IF EP.APPROV.CODE$  = "00" THEN \
      BEGIN
        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.ECR.FUNCTION$ + \
          EP.EPAY.TRANSNUM$ + "03" ,                                         \ DE-1
          EP.TAX.BASE$ + EP.CASHBACK$,                                       \ DE-2 
          EP.FECHA.PROC$ + EP.SW.VERSION$ ,                                  \ DE-3
          EP.ANUL.TRANSNUM$+ EP.AGENCIA$ + EP.MINIPAGARE$ + EP.DEFUSER1$ +   \
          EP.USER1$ + EP.USER2$ + EP.USER3$ ,                                \ DE-4
          DATE$ + TIME$ + EP.CARD.BIN$,                                      \ DE-5
          EP.ANUL.RRN$ + EP.FECHA.VENC$)                                     ! DE-6
      ENDIF
!
      IF EP.APPROV.CODE$ = "10" THEN \   !   tarjeta invalida
      BEGIN
        TS.USER.RETURN = 99    ! rechazado por tarjeta invalida
      ENDIF \
      ELSE  \
      IF EP.APPROV.CODE$ = "11"  THEN \  ! valor invalido en el estado de la cuenta
      BEGIN 
        TS.USER.RETURN = 99  ! rechazado por codigo de estado invalido en SL.TE.STATUS  
      ENDIF \
      ELSE \
        TS.USER.RETURN  = 0     ! retoma estado de aprobacion o rechazo local
!
    ENDIF 
  ENDIF
!
!
  IF EP.VERIFY.32% THEN \
  BEGIN
    EP.VERIFY.32% = 0
    IF EP.ECR.FUNCTION$ = "51" OR \
       EP.ECR.FUNCTION$ = "52" THEN \
    BEGIN
      CALL EP.LINE.PRINT("APPROVAL CODE:"+EP.APPROV.CODE$,2100H)
      IF EP.APPROV.CODE$ = "" THEN \
      BEGIN
        CALL EP.DISPLAY.AN.ERROR("FALTA AUTORIZACION")
        CALL EP.LINE.PRINT("FALTA AUTORIZACION",2100H)
        !
        ! Envía reverso de la transacción
        Call sendReverse(tmpRevCroAppl$, tmpRevCroFunct$, tmpRevCroCons$, tmpRevCroInv$, tmpRevCroDate$, \
        		EP.REV.ACCOUNT$, EP.REV.VOUCHER.START%, EP.REV.VOUCHER.END%)
        !
        TS.USER.RETURN = 99  ! rechazado por falta de proceso autorizacion en el AS/400  
      ENDIF \
      ELSE IF EP.APPROV.CODE$ <> "00" THEN \
      BEGIN
        CALL EP.DISPLAY.AN.ERROR("TRX RECHAZADA")
        CALL EP.LINE.PRINT("TRX RECHAZADA",2100H)
        TS.USER.RETURN = 99  ! rechazado por rechazo
      ENDIF
    ENDIF
  ENDIF
!

END SUB
!
!*********************************************************************
!                         (UETCRO52.TPV)
!                           (Ver 1.00)
!
! This routine performs the following
! - Mask account for CRO and ALP payment entries from MSR
!*********************************************************************
!
SUB UETCRO52 PUBLIC
!
  IF EP.TCRO% OR \
     EP.TALP% THEN \                    
  BEGIN 

!  if debugTcro% then \
!  begin
!    call ep.line.print("rutina52"+str$(takeAcct%),4100h) 
!    call ep.line.print("key="+str$(ts.io.motorkey),4100h) 
!    call ep.line.print("state="+str$(ts.io.state),4100h) 
!    call ep.line.print("data0="+ts.io.data$(0),4100h) 
!    call ep.line.print("data1="+ts.io.data$(1),4100h) 
!    call ep.line.print("data2="+ts.io.data$(2),4100h) 
!    call ep.line.print("data3="+ts.io.data$(3),4100h) 
!    call ep.line.print("data4="+ts.io.data$(4),4100h) 
!    call ep.line.print("data5="+ts.io.data$(5),4100h) 
!    call ep.line.print("data6="+ts.io.data$(6),4100h) 
!    call ep.line.print("data7="+ts.io.data$(7),4100h) 
!    call ep.line.print("data8="+ts.io.data$(8),4100h) 
!    call ep.line.print("data9="+ts.io.data$(9),4100h) 
!    call ep.line.print("data10="+ts.io.data$(10),4100h) 
!    call ep.line.print("motorkey="+str$(ts.io.motorkey),4100h)
!    call ep.line.print("tcro="+str$(ep.tcro%)+" talp="+str$(ep.talp%),4100h)
!     
!  endif

  ENDIF
!                        
  IF EP.TCRO% OR \
     EP.TALP% THEN \                    
  BEGIN 
   	! Si es una compra con ALP, el bin debe ser 899900
	If EP.TALP% And Left$(TS.IO.DATA$(9),6) <> "899900" Then \
	Begin
    	!DIM TS.IO.KEYS(10)            !
		!DIM TS.IO.DATA$(10)            !
		!TS.IO.MOTORKEY = 0             ! Reduce proceso
		CALL EP.DISPLAY.AN.ERROR("CUENTA ALP NO VALIDA" + EP.ALL.CARD$)
		!EP.TCRO% = 0
		!EP.TALP% = 0
		!takeAcct% = 0
		!EP.TCRO.CARD$ = ""
		!TS.IO.DATA$(9) = String$(16,"0")
		TS.IO.DATA$(9) = ""
		EXIT SUB                       ! Fin	
	Endif
	!
!    CALL EP.LINE.PRINT("TCRO52" + TS.IO.DATA$(9),4100H) 
    EP.VERIFY.32% = -1
    EP.MSR.DATA$   = ""
!
    IF EP.TCRO% THEN \
    BEGIN
      EP.TIPO.PROD$ = "01" 
    ENDIF \
    ELSE \
    BEGIN 
      EP.TIPO.PROD$ = "11"
    ENDIF 
    IF TS.IO.KEYS(9) = 90 THEN \
    BEGIN
      IF LEN(TS.IO.DATA$(9)) = 0 THEN \ ! Si no hay numero de cuenta
      BEGIN                            ! Si hay error
        !DIM TS.IO.DATA$(10)            !
        !DIM TS.IO.KEYS(10)            !
!        TS.GUIDANCE = 1006             ! Avisa al cajero
        !TS.IO.MOTORKEY = 0             ! Reduce proceso
!        TS.IO.DATA$(9) = ""            ! anula cuenta
        CALL EP.DISPLAY.AN.ERROR("ERROR LECTURA TARJ")
        !EP.TCRO% = 0             ! evita reproceso de tarjetas
        !EP.TALP% = 0             ! evita reproceso de tarjetas
        !TS.IO.DATA$(9) = String$(16,"0")
        EXIT SUB                       ! Fin
      ENDIF \
      ELSE \
      IF LEN(TS.IO.DATA$(9)) <= 7 THEN \
      BEGIN
        IF EP.TCRO% THEN \
          TS.IO.DATA$(9) = "8998009" + RIGHT$(STRING$(7,"0") + TS.IO.DATA$(9), 7) + "0" \
        ELSE \
          TS.IO.DATA$(9) = "8999009" + RIGHT$(STRING$(7,"0") + TS.IO.DATA$(9), 7) + "0"
          
        TS.IO.DATA$(9) = TS.IO.DATA$(9) + EP.CHECK.DIGIT(TS.IO.DATA$(9))
!        CALL EP.LINE.PRINT(TS.IO.DATA$(9),4100H)
      ENDIF
!      
      IF LEN(TS.IO.DATA$(9)) <> 16 THEN \ ! Si la longitud no es 16 bytes
      BEGIN                            ! Si hay error
        !DIM TS.IO.DATA$(10)            !
        !DIM TS.IO.KEYS(10)            !
!        TS.GUIDANCE = 1006             ! Avisa al cajero
        !TS.IO.MOTORKEY = 0             ! Reduce proceso
!        TS.IO.DATA$(9) = ""            ! anula cuenta
        CALL EP.DISPLAY.AN.ERROR("NUMERO CUENTA ERRADO")
        !EP.TCRO% = 0             ! evita reproceso de tarjetas
        !EP.TALP% = 0             ! evita reproceso de tarjetas
        !takeAcct% = 0
        !TS.IO.DATA$(9) = String$(16,"0")
        TS.IO.DATA$(9) = ""
        EXIT SUB                       ! Fin
      ENDIF                             
!      
!      IF EP.CHECK.DIGIT(LEFT$(TS.IO.DATA$(9),15))  <> RIGHT$(TS.IO.DATA$(9),1) THEN  ! Si digito de chequeo no coincide
			IF NOT valida.digito.chequeo(TS.IO.DATA$(9)) THEN \
      BEGIN                            ! Si hay error
        !DIM TS.IO.DATA$(10)            !
        !DIM TS.IO.KEYS(10)            !
!        TS.GUIDANCE = 1006             ! Avisa al cajero
        !TS.IO.MOTORKEY = 0             ! Reduce proceso
!        TS.IO.DATA$(9) = ""            ! anula cuenta
        CALL EP.DISPLAY.AN.ERROR("NUMERO CUENTA ERRADO")
        !EP.TCRO% = 0             ! evita reproceso de tarjetas
        !EP.TALP% = 0             ! evita reproceso de tarjetas
        !takeAcct% = 0
        !TS.IO.DATA$(9) = String$(16,"0")
        TS.IO.DATA$(9) = ""
        EXIT SUB                       ! Fin
      ENDIF                             
!
      EP.ALL.CARD$ = TS.IO.DATA$(9)
      EP.TCRO.CARD$ = EP.ALL.CARD$
      takeAcct% = 0
!      
      EP.CARD.BIN.L$ = LEFT$(EP.ALL.CARD$,6)
!      call ep.line.print("5="+TS.IO.DATA$(5)+"|",4100H)
      EP.FECHA.VENC$ = LEFT$(TS.IO.DATA$(5)+ STRING$(4," "),4)
      EP.FECHA.OK% = -1
      EP.MSR.DATA$  = TS.TEMP1$ 

!      IF VAL(LEFT$(EP.FECHA.VENC$,2)) >= 1  AND   \
!         VAL(LEFT$(EP.FECHA.VENC$,2)) <= 12 AND   \
!         VAL(MID$(EP.FECHA.VENC$,3,2)) >= 1 AND   \
!         VAL(MID$(EP.FECHA.VENC$,3,2)) <= 99 THEN \
!      BEGIN
!        EP.FECHA.OK% = -1
!      ENDIF
!      IF NOT EP.FECHA.OK% THEN \
!      BEGIN 
!        CALL EP.SAVE.KEYS
!        EP.KEYB.DATA$ = ""
!        EP.FECHA.OK%  = 0
!        WHILE NOT EP.FECHA.OK% 
!          CALL EP.GET.KBDATA("FECHA VENCE: mmaa?  ","0101" , "1299", \
!            EP.KEYB.DATA$)
!         call ep.line.print("LEN="+STR$(LEN(EP.KEYB.DATA$))+"DATA="+EP.KEYB.DATA$,4100H) 
!
!          IF LEN(EP.KEYB.DATA$) = 4            AND   \
!             VAL(LEFT$(EP.KEYB.DATA$,2)) >= 1  AND   \
!             VAL(LEFT$(EP.KEYB.DATA$,2)) <= 99 AND   \
!             VAL(MID$(EP.KEYB.DATA$,3,2)) >= 1 AND   \
!             VAL(MID$(EP.KEYB.DATA$,3,2)) <= 99 THEN \
!          BEGIN
!            EP.FECHA.OK% = -1
!          ENDIF
!        WEND
!        EP.FECHA.VENC$ = LEFT$(EP.KEYB.DATA$ + STRING$(4," "),4)
!        CALL EP.RESTORE.KEYS
!      ENDIF  
!      
      IF EP.ECR.FUNCTION$ = "51" OR \
         EP.ECR.FUNCTION$ = "52" THEN \
      BEGIN 
        CALL EP.SAVE.KEYS
        EP.KEYB.DATA$ = ""
        CALL EP.GET.KBDATA("Digite Id Cliente:","1" , "99999999999", \
            EP.KEYB.DATA$)

!        call ep.line.print("LEN="+STR$(LEN(EP.KEYB.DATA$))+"DATA="+EP.KEYB.DATA$,4100H) 
        IF TS.IO.MOTORKEY = 73 THEN \
        BEGIN
          DIM TS.IO.KEYS(10)            !
          DIM TS.IO.DATA$(10)            !
          TS.IO.MOTORKEY = 0             ! Reduce proceso
          !
          !EP.TCRO% = 0
          !EP.TALP% = 0
          !takeAcct% = 0
          !EP.TCRO.CARD$ = ""
          !
          Call RESET14.TCRO
          !
          TS.IO.DATA$(9) = String$(16,"0")
          EXIT SUB                       ! Fin
        ENDIF

        EP.IDCTE$ = LEFT$(EP.KEYB.DATA$ + STRING$(11," "),11)
        CALL EP.RESTORE.KEYS

      ENDIF
!
!   Invocacion de rutina del API
!
	  IF EP.TCRO.CARD$ <> "" THEN \
	  BEGIN
	      EP.TCRO% = 0             ! evita reproceso de tarjetas
	      EP.TALP% = 0             ! evita reproceso de tarjetas
	      EP.TCRO.CARD$ = ""
	      EP.TIPO.LECT$    = "90"
	      EP.FINALIDAD$    = "0" 
	      EP.TCRO.OK%      = -1
	      TS.GUIDANCE      = 0             !
	      EP.TRX.STATUS$   = "0"           !  communications OK
	      EP.AMJ.STATUS$   = "0"           !  sin iniciar transaccion
	      EP.EFT.OK%       = 0
	      EP.CARD.NUMBER$  = RIGHT$(EP.ALL.CARD$,4)
	      EP.USER.DATA$    = EP.TAX.BASE$ + EP.CASHBACK$
	!
	      EP.SAVE.TRANSNUM$ = EP.EPAY.TRANSNUM$ 
	      EP.EPAY.TRANSNUM$ = EP.NEW.TRANSNUM
	      EP.ECR.TRANSNUM$  = EP.EPAY.TRANSNUM$  !  returns transaction number used
	      
	    ! Revisa si hay un tipo variedad asignado
        ! al bin
        tvBin$ = getTipoVariedadTCRO(EP.ALL.CARD$)
        If tvBin$ = "00" Then Begin
        	tvBin$ = ""
        	call TRANSLATE.TV.CODE(LEFT$(EP.ALL.CARD$,6),tvBin$)
        Endif
        if tvBin$ <> "" and tvBin$ <> "00" and tvBin$ <> EP.TIPO.VAR$ Then \
        Begin
        	EP.TIPO.VAR$ = tvBin$
        	EP.TCRO.ALT.TV$ = tvBin$
        	TS.IO.KEYS(7)  = 90 + VAL(LEFT$(EP.TIPO.VAR$,1))
        	!TS.IO.DATA$(7) = Str$(Abs(SL.TE.AMTTENDE))
        	!If SL.TE.AMTTENDE < 0 Then Begin
        	!	TS.IO.KEYS(1) = 70
        	!Endif
        	TS.IO.DATA$(3) = RIGHT$(EP.TIPO.VAR$,1)
        	TS.IO.MOTORKEY = TS.IO.KEYS(7)
        	!
        	Call printDebug("**SL.TE.TENDTYPE=" + Str$(SL.TE.TENDTYPE) + "  SL.TE.TENDVAR=" + Str$(SL.TE.TENDVAR))
        	!
        	SL.TE.TENDTYPE = Int%(Val(Left$(tvBin$, 1)))
        	SL.TE.TENDVAR = Int%(Val(Right$(tvBin$, 1)))
        	!
        	Call printDebug(">>SL.TE.TENDTYPE=" + Str$(SL.TE.TENDTYPE) + "  SL.TE.TENDVAR=" + Str$(SL.TE.TENDVAR))
        	!
        	call MENPAG14
        Endif
        !call DSCPAG14B
        IF EP.ALL.CARD$ = "" THEN \
        BEGIN
          EP.TCRO% = 0
          EP.TALP% = 0
          takeAcct% = 0
          EP.TCRO.CARD$ = ""
          TS.IO.DATA$(9) = String$(16,"0")
          EXIT SUB                       ! Fin
        ENDIF
	      
	      Call printDebug("Calling EP.TARJ.CRED.14 from TCRO52")
	      Call printDebug("EP.VALOR.EFT$=" + EP.VALOR.EFT$ + \
	      		" SL.TE.AMTTENDE="+Str$(SL.TE.AMTTENDE) + \
	      		" TS.IO.KEYS(1)="+Str$(TS.IO.KEYS(1)) + \
	      		" TS.IO.DATA$(7)="+TS.IO.DATA$(7) + \
	      		" EP.ECR.FUNCTION$="+EP.ECR.FUNCTION$)
	      
	      SL.TE.AMTTENDE = Int%(Abs(Val(EP.VALOR.EFT$)))
	      If isVoidFunction( EP.ECR.FUNCTION$ ) Then Begin
	      	SL.TE.AMTTENDE = -SL.TE.AMTTENDE
	      Endif
	      If TS.IO.KEYS(1) = 70 Then Begin
	      	TS.IO.DATA$(7) = Str$(Abs(Val(EP.VALOR.EFT$)))
	      Endif Else Begin
	      	TS.IO.DATA$(7) = Str$(SL.TE.AMTTENDE)
	      Endif
	      
	      CALL EP.TARJ.CRED.14(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
	         EP.INVOICE.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.CUOTAS.QTY$, \
	         EP.APPROV.CODE$, EP.APPROV.DESC$, \
	         EP.CARD.NUMBER$, EP.AUTH.NUMBER$, EP.TIPO.VAR$, EP.USER.DATA$)        
	
	! 
	      IF EP.AMJ.STATUS$ <> "2"  THEN \ !    evaluate HOST command reponse
	      BEGIN
	        CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
	        CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
	        TS.IO.MOTORKEY = 0
	        !
	        !EP.TCRO% = 0
            !EP.TALP% = 0
            !takeAcct% = 0
            !EP.TCRO.CARD$ = ""
            !
            Call RESET14.TCRO
            !
            TS.IO.DATA$(9) = String$(16,"0")
            EXIT SUB                       ! Fin
	      ENDIF \
	      ELSE \
	      IF EP.TRX.STATUS$ <> "0" THEN \  !    whether communication error found
	      BEGIN
	        CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
	        CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
	        TS.IO.MOTORKEY = 0
	        !
	        !EP.TCRO% = 0
            !EP.TALP% = 0
            !takeAcct% = 0
            !EP.TCRO.CARD$ = ""
            !
            Call RESET14.TCRO
            !
            TS.IO.DATA$(9) = String$(16,"0")
            EXIT SUB                       ! Fin
	      ENDIF \
	      ELSE \
	      BEGIN 
	        EP.EFT.OK%     = -1
	!          TS.ACNUM$      = EP.ALL.CARD$
	!          TS.IO.DATA$(9) = EP.ALL.CARD$
	!	       TS.IO.KEYS(9)  = 90
	        IF TS.PROCEDURE = 2 THEN \
	          TO.XCHGLIM = 0             ! no pedir llave de supervisor para intercambio de pagos
	! jsv.add.2008/03/27
	! Se guardan los valores de eft
	! para tarjeta cro
	        EP.TCRO.VALOR.EFT$ = EP.VALOR.EFT$
	        EP.TCRO.TAX.EFT$   = EP.TAX.EFT$
	        EP.TCRO.TAX.BASE$  = EP.TAX.BASE$
	!
	        !
	        !--------------------------------------------------------------------------------
	        ! 2021-09-06 jsv
	        ! Asignación de valor a la variable de número de tarjeta o número de cuenta y 
	        ! al puntero de inicio de voucher
	        !--------------------------------------------------------------------------------
	        EP.REV.ACCOUNT$ = EP.ALL.CARD$
	        EP.REV.VOUCHER.START% = EP.POINTER1%
	        !--------------------------------------------------------------------------------
	        !
	        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.ECR.FUNCTION$ +    \
	           EP.EPAY.TRANSNUM$ + "01" ,                                           \ DE-1
	           EP.ALL.CARD$,                                                        \ DE-2 
	           EP.FECHA.POSTEO$ + EP.TIPO.LECT$ + EP.FINALIDAD$,                    \ DE-3
	           EP.VALOR.EFT$ + EP.TAX.EFT$,                                         \ DE-4
	           EP.FECHA.PROC$ + EP.PROPINA$,                                        \ DE-5
	           EP.CAJERO$ + EP.INVOICE.NBR$ + EP.FECHA.VENC$)                       ! DE-6
	        EP.INICIO.FOUND% = -1
	        !
	        !---------------------------------------------------------------------
	        ! 2018-05-12 jsv
	        !---------------------------------------------------------------------
	        ! Se almacena índice correspondiente al data entry 01
	        !---------------------------------------------------------------------
	        EP.DATA1.INDEX% = SL.END
	        !
	        Call traceApplication("EP.DATA1.INDEX%=" + Str$(EP.DATA1.INDEX%), "APITCRO")
	        !---------------------------------------------------------------------
	      ENDIF 
	  ENDIF
    ENDIF
  ENDIF  
      
END SUB
!
Sub UETCRO43 Public
	Integer*4 realValue%
	If \
			(EP.RECAUDO.OK%) And \
			ts.io.keys(1) <> 70 And \
			(isMiscPlu(SL.IT.ITEMCODE$)) Then Begin
		If \
				SL.IE.QTYORWGT > 1 And \
				Abs(Int%(Val(TS.IO.DATA$(5)))) > 0 Then Begin
			If (SL.IT.INDICAT2 AND 4) > 0 Then \
				SL.IT.INDICAT2 = SL.IT.INDICAT2 XOR 4
			TS.IO.DATA$(6) = "1"
			SL.IE.QTYORWGT = 1
			realValue% = Abs(Int%(Val(TS.IO.DATA$(5))))
			SL.IT.XPRICE = realValue%
		Endif
		If SL.IT.XPRICE > EP.RC.MAXIMP% Then \
			SL.IT.XPRICE = EP.RC.MAXIMP%
	Endif
End Sub
!
Sub UETCRO53 Public
	Integer*2 sepIndex1%, sepIndex2%
	String tmpData1$
	!
	If TS.TEMP1I2 = 11 Then Begin
		! Registro Data Entry
		sepIndex1% = Match(Chr$(3AH), SL.STR.ENTRY$, 1)
		If sepIndex1% > 0 Then Begin
			sepIndex2% = Match(Chr$(3AH), SL.STR.ENTRY$, sepIndex1% + 1)
			If sepIndex2% > 0 Then Begin
				tmpData1$ = Unpack$(Mid$(SL.STR.ENTRY$, sepIndex1% + 1, sepIndex2% - sepIndex1% - 1))
				If isStringRecaudo21(tmpData1$) Then Begin
					! Si se encuentra un registro de recaudos CRO, se debe prender el flag de recaudos
					! para garantizar que la confirmación del recaudo viaje al AS400
					validacionRecaudosTCRO% = -1
				Endif
			Endif 
		Endif
	Endif
End Sub
!
END      
