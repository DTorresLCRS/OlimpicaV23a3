%ENVIRON T
!
! Supermarket application global data
!
%INCLUDE EAMTSWKG.J86

%INCLUDE EAMTOPTS.J86

%INCLUDE EAMTRANS.J86

	String launchApplFunc$

	Integer*1 						\
		EP.BALDUE.CHG%,			\
		EP.EFT.ENTRAD%
	!
	Integer*4 						\
		EP.TAX.EPAY%,				\
		EP.TAX.BASE%
	!
	String 								\
  	EP.TAX.BASE$
  !
  String Global 				\
  	EP.defaultApp$

  INTEGER*1 GLOBAL       \
     TS.RETV.IN.PROGRESS
  
  String Global SERIAL.REQUEST$
  
  !Integer*1 temporalParamFileLoaded%
  
  ! Arreglo de descriptores para interacción con sistema de descuentos
  String EP.DISC.MSG$(1)

!
! tipo de transaccion TEF preseleccionada
String EP.FORCE.TRXTYPE$,EP.FORCE.TENDVAR$
!
! Java calling parameters
!
  STRING                \
    EP.RETURNVALUE$,    \
    EP.EXCEPTION$

Integer*1 tmpAnulacion%
!
! error evaluation data
!
  Integer*1             \
    EP.TV.CHG%,         \
    EP.EFT.OK%,         \
    EP.EFT.RESPBLANCO%, \ ! Para manejar respuestas en blanco CACS 
    EP.EFT.RECUP.TRX%,  \
    EP.XCHGLIM%,        \
    EP.checkTrxActive%, \
    EP.validarPan%


  INTEGER*4             \
    EP.ERRNCODE%,       \
    EP.MAX.TV.SAV%,     \
    EP.EFT.RECUP.VAL%,  \
    EP.CHG.TV.SAV%

  STRING      \ 
    EP.ESPECIAL.AVANCE$,\
    EP.ESPECIAL.COMPRA$,\
    EP.numeroReciboAnular$, \ ! Recibo introducido por usuario para anular
    tefTrxArray$(2)	! Arreglo con datos de transacciones tef aprobadas
!
  Integer*1 tefTrxCounter%
!
!
  Integer*1 \
  	EP.EFT.ACTIVO%
  
  INTEGER*1           \
    EP.EFT.TRX%,      \
    EP.EFT.SMA%,      \
    EP.EFT.DETAIL%

  INTEGER*2 GLOBAL    \
    EP.SIZE.TV%,      \
    EP.VERIF.TV.SAV%, \
    EP.IOPARM%,       \
    EP.TV.POS%

  Integer*2           \
    EP.TIPO.TRX%,     \
    EP.SWITCH.PROC%

  String \
  	EP.TV.CHEQ$,         \
  	EP.TV.TJDB$,         \
  	EP.TV.TJCR$,         \
  	EP.TV.TJAV$
  
  Integer*2 \
  	EP.EN.CHEQ%,         \ ! Entidad por defecto para cheques
  	EP.EN.TJDB%,         \ ! Entidad por defecto para tarjetas debito
  	EP.EN.TJCR%,         \ ! Entidad por defecto para tarjetas credito
  	EP.EN.TJAV%            ! Entidad por defecto para avances
  
  String EP.DEFAULT.TV$
  Integer*2 EP.DEFAULT.ENT%
  	
  STRING GLOBAL          \
    EP.GUIDE.MESSAGE$(1),\
    EP.TV.CODE$(1),      \
    EP.EPAY.TRANSNUM$ 

!
! tpv application interface module data
!
  INTEGER*1              \
    EP.GOOD.END%,        \
    EP.EOT.SENT%,        \
    EP.DATA.RECEIVED%,   \
    EP.DATA.ERROR%,      \
    EP.DE1102.FOUND%,    \
    EP.DE1103.FOUND%,    \
    EP.DE1203.FOUND%,    \
    EP.TRX.RESP%,     \
    EP.TRX.APPROV%,   \
    EP.TRX.VOUCHER%,  \
    EP.TRX.TOHOST1%,  \ 
    EP.TRX.TOHOST2%,  \
    EP.TRX.REVERSO%,  \
    EP.TRX.ANUL%,     \   
    EP.TRX.PEND%,     \
    EP.EFT.FOUND%,       \
    EP.INICIO.FOUND%,    \
    EP.RESP.FOUND%,      \
    EP.APPROV.FOUND%,    \
    EP.VOUCHER.FOUND%,   \
    EP.TOHOST.FOUND%,    \
    EP.REPRINT%,         \
    EP.INTERCHG%,        \
    EP.MASK%,            \
    EP.STATE%


  INTEGER*2              \
    EP.POS%,             \
    EP.LINEA%,           \
    EP.TRY.COUNT%,       \
    EP.M.LEN%
    
  INTEGER*2  GLOBAL      \
    EP.J%,               \
    EP.MAX.TRX%

  String Global 		 \
  	EP.A$,               \
    EP.B$
  	
  String                 \
  	EP.FUNCTION$,        \
  	EP.ECR.DATETIME$,    \
  	EP.COMERCIO$,        \
  	EP.EPAY.TERMINAL$,   \
  	EP.VALOR.EPAY$,      \
    EP.TAX.EPAY$,        \
    EP.PROP.EPAY$,       \
    EP.CARD.BIN$,        \       
    EP.CUOTAS.QTY$,      \
    EP.FECHA.POSTEO$,    \
    EP.FECHA.INIC$,      \
    EP.FECHA.PROC$,      \ 
    EP.COD.PROC$,        \
    EP.FECHA.VENC$,      \
    EP.CENTRAL.CHEQUE$,  \
    EP.TIPO.ID$,         \
    EP.ID.GIRADOR$,      \
    EP.TEL.GIRADOR$,     \
    EP.COD.BANCO$,       \
    EP.CTA.CTE$,         \
    EP.NRO.CHEQUE$,      \
    EP.RRN$,             \
    EP.FRANQUICIA$,      \
    EP.BANCO$ ,          \
    EP.RECIBO$,          \
	EP.RECIBO2$,          \  !Para impresi¢n en cambio de medio de pago CACS  
    EP.LAST.RECIBO$,     \
    EP.LAST.TV$,         \
    EP.LAST.APPL$,       \
    EP.LAST.FUNCTION$,   \
    EP.PRODUCTO$,        \
    EP.PREPRODUCTO$,     \
    EP.IVA.BASE$,        \
    EP.DONACION$,        \
    EP.CAJERO$,          \
    EP.CASHBACK$,        \
    EP.FILLER4$,         \
    EP.FILLER8$,         \
    EP.FILLER11$,        \
    EP.FILLER12$,        \
    EP.DISP.MESSAGE$,    \
    EP.DISP.PARAM$,      \
    EP.DISP.OPER$,       \
    EP.DISP.CUST$,       \
    EP.MSR.DATA$,        \
    EP.KEYB.DATA$,       \
    EP.PRT.MESSAGE$,     \  
    EP.PRT.PARAM$,       \
    EP.PRT.CUST$,        \
    EP.PRT.VOUC$,        \
    EP.PRT.CUT$,         \
    EP.INI.RANGE$,       \    
    EP.END.RANGE$,       \ 
    EP.FECHA.PEND$,      \
    EP.REV.APROB$,       \
    EP.TRX.TRANSNUM$,    \
    EP.TRX.FUNCTION$,    \
    EP.TRX.FECHA$,       \
    EP.DE.APPL$,         \
    EP.DE.FUNCTION$,     \
    EP.DE.TIPO$,         \
    EP.DE.TRANSNUM$,     \
    EP.DE.TRX.STATUS$,   \
    EP.DE.AMJ.STATUS$,   \
    EP.DE.ANULTRANSNUM$, \
    EP.DE1.DATA$,        \
    EP.DE2.DATA$,        \
    EP.DE3.DATA$,        \
    EP.DE4.DATA$,        \
    EP.DE5.DATA$,        \
    EP.DE6.DATA$,        \
    EP.DE.ISOCOD$,       \
    EP.DE.CODPRO$,       \
    EP.DE.AUTH$,         \
    EP.DE.POSTEO$,       \
    EP.DE.CARD$,         \
    EP.DE.CUOTAS$,       \
    EP.DE.RRN$,          \
    EP.DE.AMOUNT$,       \
    EP.DE.IVA$,          \
    EP.DE.IVA.BASE$,     \
    EP.DE.CASHBACK$,     \
    EP.DE.PROPINA$,      \
    EP.DE.DONACION$,     \
    EP.DE.TIPOVAR$,      \
    EP.DE.CARD.BIN$,     \
    EP.DE.FECHA.VENC$,   \
    EP.DE.CTA.CTE$,      \
    EP.DE.CENTRAL.CHEQUE$, \
    EP.DE.NRO.CHEQUE$,   \
    EP.DE.FECHA.PROC$,   \
    EP.ANUL.TRANSNUM$,   \
    EP.ANUL.RRN$,        \
    EP.ANUL.AUTH$,       \
    EP.ANUL.CODPRO$,     \
    EP.ANUL.CUOTAS$,     \
    EP.ANUL.POSTEO$,     \
    EP.ANUL.CARD$,       \
    EP.MESSAGE.SENT$,    \
    EP.MESSAGE$,         \
    EP.MSGLEN$,          \
    EP.AMJ.MESSAGE$,     \
    EP.MESSAGE.RECEIVED$,\
    EP.INTERF.MESSAGE$,  \
    EP.MESSAGE.DATA$,    \
    EP.VOUCHER$(1),      \
    EP.CUT.VOUCHER$(1),  \
    EP.EFT.LINE$(1),     \
    EP.SAV.HD1$,         \
    EP.SAV.HD2$,         \
    EP.SAV.TR1$,         \
    EP.SAV.TR2$,         \
    EP.REV.FUNCTION$,    \
    EP.REV.APPL$,        \
    EP.REV.STATUS$,      \
    EP.REVTRX.STATUS$,   \
    EP.MASK$,            \
    EP.MSG.DATA$,        \
    EP.KEY$,             \
    EP.EFT.TRXTYPE$  

  String \
  	EP.IVADEV$
  
  STRING                 \
    EP.APPL.STATUS$,     \
    EP.H.AUTH.NUMBER$,   \
    EP.H.APPROV.CODE$,   \
    EP.H.CARD.NUMBER$,   \
    EP.H.TIPO.ID$,       \
    EP.H.CUST.ID$,       \
    EP.H.APPROV.DESC$,   \
    EP.H.TIPO.VAR$,      \
    EP.H.USER.DATA$,     \
    EP.CUST.NAME$,       \
    EP.CUST.LEVEL$,      \
    EP.CUST.OPTS$,       \
    EP.DISC.GRP$,        \
    EP.DB.STATUS$,       \
    EP.LAST.DATETIME$,   \
    EP.LAST.REDEEM$,     \
    EP.ACUM.POINTS$,     \
    EP.ACUM.REDEEM$,     \
    EP.ACUM.AMTPUR$,     \
    EP.ACUM.QTYPUR$,     \
    EP.PROMO.ID$(1),     \
    EP.PROMO.STATUS$(1), \
    EP.PROMO.POINTS$(1), \ 
    EP.PROMO.REDEEM$(1), \
    EP.PROMO.AMTPUR$(1), \
    EP.PROMO.QTYPUR$(1), \
    EP.TRX.PEND$         

  STRING               \
    EP.APPL$,          \
    EP.ECR.FUNCTION$,  \
    EP.TRX.STATUS$,    \
    EP.AMJ.STATUS$,    \
    EP.ECR.NBR$,       \
    EP.ECR.TRANSNUM$,  \
    EP.ECR.TRANSNUM2$, \
    EP.VALOR.EFT$,    \
    EP.TAX.EFT$,       \
    EP.PROPINA$,       \
    EP.APPROV.CODE$,   \
    EP.APPROV.DESC$,   \
    EP.CARD.NUMBER$,   \
    EP.CARD.NUMBER2$,   \
    EP.AUTH.NUMBER$,   \
    EP.AUTH.NUMBER2$,   \
    EP.TIPO.VAR$,      \
    EP.TV.FORCE$,      \
    EP.USER.DATA$,     \
    EP.MESSAGE.DESC$,  \
    EP.SAVE.KEY$
      
!
! Parameters data
!
  Integer*1 \
  	EP.controlAnuladoTotal%
  
  Integer*2 						\
  	EP.initialTefKey%,	\
  	EP.otherTefCount%,  \
  	EP.otherTefKeys%(2),\
  	EP.LEN.CLAVE%,      \
  	EP.NETCNT%,					\
  	EP.ACTUALNET%,			\
  	EP.tendMenuKey%
  
  Integer*2 Global 			\
  	EP.settlementKey%
  
  Integer*2 \
  	EP.SERIALPARAM%(2)
  String \
  	EP.PROMPT$,			\
  	EP.SLOGAN$(1),		\
  	EP.NETDESC$(1),		\
  	EP.CLAVEANUL$(1),	\
  	EP.HORACIERRE$(1),	\
  	EP.ACTUALCLAVEANUL$,\
  	EP.SERIALPARAM$(2)
  
  INTEGER*2  GLOBAL      \
    EP.MAX.TRIES%,       \ 
    EP.COM%,             \
    EP.EIAP%,            \
    EP.SPEED%,           \
    EP.DATA.BITS%,       \
    EP.STOP.BITS%

  String \
  	EP.EFT.CLAVE$
  
  STRING  GLOBAL         \
    EP.PARITY$,          \
    EP.SERIAL.MISC$,     \
    EP.ACTUALNETDESC$
 
  INTEGER*4 GLOBAL       \
    EP.CHARS.WAIT%,      \
    EP.LONG.WAIT%,       \
    EP.SHORT.WAIT%,      \
    EP.CB.IMP%,          \
    EP.MAX.TV%,          \
    EP.CHG.TV% 
!
! Variables para manejo de log TEFII
String logMask$
Integer*2 logRecordSize%
!
! Nombre de la aplicación TEFII
String TEFII.applName$
!
!
! end of data definitions
!------------------------------------------------------------------------
!
!  Standard supermarket routines
!
! print routine
!
SUB TSPREC01 EXTERNAL
END SUB
!
! DATA ENTRY routine
!
SUB TSTPEC01 EXTERNAL
END SUB
!
! Display an error wait for clear
!
SUB TSCSEC08 EXTERNAL
END SUB
!
SUB TSTDEC01 EXTERNAL
END SUB
!
!---------------------------------------------------------------------------
!
Sub TEFII.FUNCT.setApplyLastPayment(pValue%) External
	Integer*1 pValue%
End Sub
!
Sub asignarVariableGlobal(pName$, pValue$) External
	String pName$, pValue$
End Sub
!
Sub setRepeatLastSequence(pValue%) External
	Integer*1 pValue%
End Sub
!
Function TEFII.FORMAT.RTRIM$(VALUE$) External
	String VALUE$, TEFII.FORMAT.RTRIM$
End Function
!
Function TEFII.FORMAT.LTRIM$(VALUE$) External
	String VALUE$, TEFII.FORMAT.LTRIM$
End Function
!
Function TEFII.FORMAT.GETFIELD$(INDEX%) External
	Integer*2 INDEX%
	String TEFII.FORMAT.GETFIELD$
End Function
!
Function getJavaProperty(pProperty$, pFileName$, pFileSession%) External
	String getJavaProperty, pProperty$, pFileName$
	Integer*2 pFileSession%
End Function
!
Function EP.invocarApplManager0(epAppl$,epFunction$,parteVariable$,usaConsecutivo%, pNoMsg%) External
	String EP.invocarApplManager0, epAppl$, epFunction$, parteVariable$
	Integer*1 usaConsecutivo%, pNoMsg%
End Function
!
Sub EP.LINE.PRINT(LINEA$,ESTACION%) External
	String LINEA$
	Integer*2 ESTACION%
End Sub
!
Sub EP.SAVE.PRINT External
End Sub
!
Sub EP.RESTORE.PRINT External
End Sub
!
Sub EP.DISPLAY.AN.ERROR(UE.DISP.MESSAGE$) External
	String UE.DISP.MESSAGE$
End Sub
!
Sub EP.DISPLAY.A.MESSAGE(UE.DISP.MESSAGE$) External
	String UE.DISP.MESSAGE$
End Sub
!
Sub EP.GET.KBDATA(UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, UE.KEYB.DATA$) External
	String UE.REQ.MESSAGE$, UE.INI.RANGE$, UE.END.RANGE$, UE.KEYB.DATA$
End Sub
!
Sub EP.SAVE.KEYS External
End Sub
!
Sub EP.RESTORE.KEYS External
End Sub
!
Sub EP.BUSCA.LIMITE.TV(UE.TIPO.VAR$) External
	String UE.TIPO.VAR$
End Sub
!
Sub EP.ADD.DATA.ENTRY(UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6) External
	String UE.DATA1,UE.DATA2,UE.DATA3,UE.DATA4,UE.DATA5,UE.DATA6
End Sub
!
Sub TRANSLATE.TV.CODE(UE.GROUP.DESC$,UE.CODE$) External
	String UE.CODE$,UE.GROUP.DESC$
End Sub
!
Sub TRANSLATE.ISO.CODE(UE.CODE$,UE.CODE.DESC$) External
	String UE.CODE$,UE.CODE.DESC$
End Sub
!
Function TIISELEC.getTefApp(pOptionalData$, pIndex%) External
	Integer*2 pIndex%
	String TIISELEC.getTefApp, pOptionalData$
End Function
!
Sub TENDSALE.validateTender(pTendType%, pTendVar%, pEntity%, pBin$, pReturn%, pMessage$) External
	String pBin$, pMessage$
	Integer*2 pTendType%, pTendVar%, pEntity%, pReturn%
End Sub
!
Function tef.getDefaultTV Public
	String tef.getDefaultTV
	!
	tef.getDefaultTV = EP.TV.TJDB$
End Function
!
Function tef.getDefaultEntity Public
	Integer*2 tef.getDefaultEntity
	!
	tef.getDefaultEntity = EP.EN.TJDB%
End Function
!
! Recibe un String que debe tener la siguiente estructura:
! kkk:aa,kkk:aa,kkk:aa,etc
! donde kkk = número de tecla y ff es número de aplicación
Sub fillOtherTefKeys(pOtherKeysInfo$)
	String pOtherKeysInfo$, tmpInfo$
	Integer*2 tmpLen%, tmpIndex%, tmpIndex1%, tmpIndex2%
	!
	tmpLen% = Len(pOtherKeysInfo$)
	EP.otherTefCount% = 0
	!
	If tmpLen% > 0 Then Begin
		! Si el parámetro recibido no termina en "," se le agrega una "," al final
		If Right$(pOtherKeysInfo$, 1) <> "," Then Begin
			pOtherKeysInfo$ = pOtherKeysInfo$ + ","
		Endif
		!
		! Busca el número de veces que aparece el separador ","
		tmpIndex1% = 0
		While tmpIndex1% < tmpLen%
			tmpIndex1% = Match(",", pOtherKeysInfo$, tmpIndex1% + 1)
			EP.otherTefCount% = EP.otherTefCount% + 1
		Wend
		!
		! Dimensiona el arreglo donde se almacenará la información
		Dim EP.otherTefKeys%(EP.otherTefCount% - 1, 2)
		!
		! Almacena la información correspondiente
		tmpIndex1% = 0
		tmpIndex% = 0
		While tmpIndex1% < tmpLen%
			tmpIndex2% = Match(",", pOtherKeysInfo$, tmpIndex1% + 1)
			tmpInfo$ = Mid$(pOtherKeysInfo$, tmpIndex1% + 1, tmpIndex2% - tmpIndex1% - 1)
			tmpIndex1% = tmpIndex2%
			tmpIndex2% = Match(":", tmpInfo$, 1)
			If tmpIndex2% > 0 Then Begin
				If Len(tmpInfo$) - tmpIndex2% > 2 Then Begin
					If Ucase$(Right$(tmpInfo$, 1)) = "J" Then Begin
						EP.otherTefKeys%(tmpIndex%, 2) = 1
					Endif
					tmpInfo$ = Left$(tmpInfo$, Len(tmpInfo$) - 1)
				Endif
				!
				EP.otherTefKeys%(tmpIndex%, 0) = Int%(Val(Left$(tmpInfo$, tmpIndex2% - 1)))
				EP.otherTefKeys%(tmpIndex%, 1) = Int%(Val(Right$(tmpInfo$, Len(tmpInfo$) - tmpIndex2%)))
			Endif Else Begin
				EP.otherTefKeys%(tmpIndex%, 0) = Int%(Val(tmpInfo$))
				EP.otherTefKeys%(tmpIndex%, 1) = 85 ! Aplicación por default
			Endif
			tmpIndex% = tmpIndex% + 1
		Wend
	Endif
End Sub
!
Sub EP.PARSE.DISPLAY.REQUEST
    EP.DISP.MESSAGE$ = ""
    EP.DISP.PARAM$   = ""
    EP.DISP.OPER$    = ""
    EP.DISP.CUST$    = ""
    EP.APPL.STATUS$  = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = Mid$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      EP.DISP.MESSAGE$ = Mid$(EP.AMJ.MESSAGE$,12,40)
    If EP.M.LEN% >= 52 Then \
      EP.DISP.PARAM$ = Mid$(EP.AMJ.MESSAGE$,52,1)
    If EP.M.LEN% >= 53 Then \
      EP.DISP.OPER$ = Mid$(EP.AMJ.MESSAGE$,53,1)
    If EP.M.LEN% >= 54 Then \
      EP.DISP.CUST$ = Mid$(EP.AMJ.MESSAGE$,54,1)

End Sub
!
Sub EP.PARSE.PRT.HEADER
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUST$    = ""
    EP.PRT.VOUC$    = ""
    EP.PRT.CUT$     = ""
    EP.APPL.STATUS$ = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = Mid$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      EP.PRT.MESSAGE$ = Mid$(EP.AMJ.MESSAGE$,12,38)
    If EP.M.LEN% >= 50 Then \
      EP.PRT.PARAM$ = Mid$(EP.AMJ.MESSAGE$,50,1)
    If EP.M.LEN% >= 51 Then \
      EP.PRT.CUST$ = Mid$(EP.AMJ.MESSAGE$,51,1)
    If EP.M.LEN% >= 52 Then \
      EP.PRT.VOUC$ = Mid$(EP.AMJ.MESSAGE$,52,1)
    If EP.M.LEN% >= 53 Then \
      EP.PRT.CUT$ = Mid$(EP.AMJ.MESSAGE$,53,1)

End Sub
!
Sub EP.PARSE.PRT.LINE
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUST$    = ""
    EP.PRT.VOUC$    = ""
    EP.APPL.STATUS$ = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = Mid$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      EP.PRT.MESSAGE$ = Mid$(EP.AMJ.MESSAGE$,12,38)
    If EP.M.LEN% >= 50 Then \
      EP.PRT.PARAM$ = Mid$(EP.AMJ.MESSAGE$,50,1)
    If EP.M.LEN% >= 51 Then \
      EP.PRT.CUST$ = Mid$(EP.AMJ.MESSAGE$,51,1)
    If EP.M.LEN% >= 52 Then \
      EP.PRT.VOUC$ = Mid$(EP.AMJ.MESSAGE$,52,1)

End Sub
!
Sub EP.PARSE.PRT.CLOSE
    EP.PRT.MESSAGE$ = ""
    EP.PRT.PARAM$   = ""
    EP.PRT.CUT$     = ""
    EP.APPL.STATUS$ = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = Mid$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      EP.PRT.MESSAGE$ = Mid$(EP.AMJ.MESSAGE$,12,38)
    If EP.M.LEN% >= 50 Then \
      EP.PRT.PARAM$ = Mid$(EP.AMJ.MESSAGE$,50,1)
    If EP.M.LEN% >= 51 Then \
      EP.PRT.CUT$ = Mid$(EP.AMJ.MESSAGE$,51,1)

End Sub
!
Sub EP.PARSE.DATA.REQUEST
    EP.DISP.MESSAGE$  = ""
    EP.INI.RANGE$     = ""
    EP.End.RANGE$     = ""
    EP.APPL.STATUS$   = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = Mid$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      EP.DISP.MESSAGE$ = Mid$(EP.AMJ.MESSAGE$,12,40)
    If EP.M.LEN% >= 52 Then \
      EP.INI.RANGE$ = Mid$(EP.AMJ.MESSAGE$,52,20)
    If EP.M.LEN% >= 72 Then \
      EP.End.RANGE$ = Mid$(EP.AMJ.MESSAGE$,72,20)

End Sub
!
Sub EP.PARSE.DENTRY.REQUEST
    EP.DE1.DATA$ = ""
    EP.DE2.DATA$ = ""
    EP.DE3.DATA$ = ""
    EP.DE4.DATA$ = ""
    EP.DE5.DATA$ = ""
    EP.DE6.DATA$ = ""
    EP.APPL.STATUS$ = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = Mid$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      EP.DE1.DATA$ = Mid$(EP.AMJ.MESSAGE$,12,12)
    If EP.M.LEN% >= 24 Then \
      EP.DE2.DATA$ = Mid$(EP.AMJ.MESSAGE$,24,20)
    If EP.M.LEN% >= 44 Then \
      EP.DE3.DATA$ = Mid$(EP.AMJ.MESSAGE$,44,20)
    If EP.M.LEN% >= 64 Then \
      EP.DE4.DATA$ = Mid$(EP.AMJ.MESSAGE$,64,20)
    If EP.M.LEN% >= 84 Then \
      EP.DE5.DATA$ = Mid$(EP.AMJ.MESSAGE$,84,20)
    If EP.M.LEN% >= 104 Then \
      EP.DE6.DATA$ = Mid$(EP.AMJ.MESSAGE$,104,20)

End Sub
!
Sub EP.GETUNPK External
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
Sub EP.STORE.VOUCHER(UE.PRINT.LINE$,UE.PRT.CUT$) External
	String UE.PRINT.LINE$, UE.PRT.CUT$
End Sub
!
Sub EP.STORE.EFTLINE(UE.PRINT.LINE$) External
	String UE.PRINT.LINE$
End Sub
!
!
!  TEF II BASIC MODULE
!
FUNCTION TEFII.FUNCT.APPLKERNEL$(TRAMA$) EXTERNAL
	STRING TEFII.FUNCT.APPLKERNEL$
	STRING TRAMA$
END FUNCTION
!
Function TEFII.FUNCT.GETHOR External
	String TEFII.FUNCT.GETHOR
End Function
!
SUB EP.VALIDATE.DCTOS(pAnswer$,pForceTV$,pAmt$,pTax$,pBase$,pAnul%) Public
	STRING pAnswer$,pForceTV$,pAmt$,pTax$,pBase$
	Integer*1 pAnul%
	!
END SUB
!
SUB TEFII.FORMAT.RESETFIELDS EXTERNAL
END SUB
!
Function rRemove$(line$,charCode%) External
	String rRemove$,line$
End Function
!
Sub JavaCall.Initialize.Request(ClassName,MethodName,TheRequest) External
	String ClassName, MethodName, TheRequest
End Sub
!
Sub JavaCall.AddParameter.String(TheRequest,TheParameter) External
	String TheRequest, TheParameter
End Sub
!
Sub JavaCall.InvokeMethod.ReturnString(TheRequest,ReturnValue, Exception) External
	String TheRequest, ReturnValue, Exception
End Sub
!
Sub JavaCall.InvokeMethod.ReturnVoid(TheRequest,Exception) External
	String TheRequest, Exception
End Sub
!
Function disc.validateTender(pAmount%, pTenderType$, pEntity%, pAccount$) External
	Integer*4 pAmount%
	Integer*1 disc.validateTender
	String pTenderType$, pAccount$
	Integer*2 pEntity%
End Function
!
Sub disc.ignoreAccount External
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
Function getInstitucion External
	Integer*2 getInstitucion
End Function
!
Sub printDebug(pMsg$) External
	String pMsg$
End Sub
!
Sub traceApplication(pMsg$, pAppl$) External
	String pMsg$, pAppl$
End Sub
!
Function calculateTaxBase(pAmount%, pDev$) External
	Integer*4 calculateTaxBase, pAmount%
	String pDev$
End Function
!
Function calculateTaxValue(pAmount%) External
	Integer*4 calculateTaxValue, pAmount%
End Function
!
Function calculateIACValue(pAmount%) External
	Integer*4 calculateIACValue, pAmount%
End Function
!
Sub menpag.ignoreTender External
End Sub
!
Sub TEFII.FORMAT.INITIALIZEPARAM External
End Sub
!
Sub TEFII.FUNCT.INITIALIZEPARAM(pFileName$) External
	String pFileName$
End Sub
!
Function TEFII.FUNCT.ISSYNCBYDEMAND External
	Integer*1 TEFII.FUNCT.ISSYNCBYDEMAND
End Function
!
Function TEFII.FUNCT.GETAMT External
	String TEFII.FUNCT.GETAMT
End Function
!
Sub recaudot.disableTV External
End Sub
!
Function recaudot.validateTender(pTv$, pValue%, pIgnoreTv$, pIgnoreValue%) External
	Integer*1 recaudot.validateTender
	String pTv$, pIgnoreTv$
	Integer*4 pValue%, pIgnoreValue%
End Function
!
Function tef.isTefTrx Public
	Integer*1 tef.isTefTrx
	!
	tef.isTefTrx = EP.EFT.SMA%
End Function
!
Function tef.getBin Public
	String tef.getBin
	!
	tef.getBin = EP.CARD.BIN$
End Function
!
Function tef.getActualNet Public
	Integer*2 tef.getActualNet
	!
	tef.getActualNet = EP.ACTUALNET%
End Function
!
Function tef.getTaxValue Public
	Integer*4 tef.getTaxValue
	!
	If tef.isTefTrx Then \
		tef.getTaxValue = EP.TAX.EPAY% \
	Else \
		tef.getTaxValue = 0
End Function
!
Sub tef.initializeTender
	String tmpClassName$, tmpMethod$, tmpRequest$, tmpException$
	!
	Call asignarVariableGlobal("CURRENT_NET", Str$(EP.ACTUALNET%))
	!
	tmpClassName$ = "com.lacadena.tef.controller.TefTenderManager"
	tmpMethod$ = "initializeData"
	tmpRequest$ = "initializeData"
	tmpException$ = ""
	!
	Call Javacall.Initialize.Request(tmpClassName$, tmpMethod$, tmpRequest$)
	Call JavaCall.AddParameter.String(tmpRequest$, Str$(EP.ACTUALNET%))
	Call JavaCall.InvokeMethod.ReturnVoid(tmpRequest$, tmpException$)
End Sub
!
Sub EP.switchNet Public
	Integer*2 pNet%
	String tmpNet$
	!
	If EP.NETCNT% > 1 Then Begin
		If EP.NETCNT% = 2 Then Begin
			If EP.ACTUALNET% = 1 Then \
				pNet% = 2 \
			Else \
				pNet% = 1
		Endif Else Begin
			Call EP.SAVE.KEYS
			Call EP.GET.KBDATA("Ingrese nueva red","1" , "99", tmpNet$)
			If TS.IO.MOTORKEY = 73 Then Begin
				Call EP.RESTORE.KEYS
				Exit Sub
			Endif
			Call EP.RESTORE.KEYS
			pNet% = Int%(Val(tmpNet$))
		Endif
		!
		If pNet% > 0 And pNet% <= EP.NETCNT% Then Begin
			EP.ACTUALNET% = pNet%
			Call tef.initializeTender
			EP.ACTUALNETDESC$ = EP.NETDESC$(EP.ACTUALNET%)
			EP.ACTUALCLAVEANUL$ = Right$(String$(4, "0") + EP.CLAVEANUL$(EP.ACTUALNET%), 4)
			EP.COM% = EP.SERIALPARAM%(EP.ACTUALNET%, 1)
			EP.SPEED% = EP.SERIALPARAM%(EP.ACTUALNET%, 2)
			EP.DATA.BITS% = EP.SERIALPARAM%(EP.ACTUALNET%, 3)
			EP.STOP.BITS% = EP.SERIALPARAM%(EP.ACTUALNET%, 4)
			EP.PARITY$ = EP.SERIALPARAM$(EP.ACTUALNET%, 1)
			EP.SERIAL.MISC$ = EP.SERIALPARAM$(EP.ACTUALNET%, 2)
			Call TEFII.FUNCT.INITIALIZEPARAM("")
			Call TEFII.FORMAT.INITIALIZEPARAM
			Call EP.DISPLAY.AN.ERROR("Nueva red           " + EP.ACTUALNETDESC$)
		Endif Else Begin
			Call EP.DISPLAY.AN.ERROR("Ingrese valor entre 1 y " + Str$(EP.NETCNT%))
		Endif
	Endif Else Begin
		Call EP.DISPLAY.AN.ERROR("Solo existe una red")
	Endif
End Sub
!
Function isAfterCloseHour
	Integer*1 isAfterCloseHour
	String actualHour$
	!
	actualHour$ = TEFII.FUNCT.GETHOR
	If actualHour$ <> "" Then Begin
		If Val(actualHour$) > Val(EP.HORACIERRE$(EP.ACTUALNET%)) Then \
			isAfterCloseHour = -1 \
		Else \
			isAfterCloseHour = 0
	Endif Else \
		isAfterCloseHour = 0
End Function
!
Sub tef.validateDisc(pData$, pAccount$, pValidate%, pAnswer%, tmpTenderType$, tmpEntity%)
	Integer*1 pAnswer%, pValidate%
	String pData$, pAccount$, tmpTenderType$, virtualTenderType$
	Integer*2 tmpEntity%, virtualEntity%, tipoPagoOriginal%, tipoPagoNuevo%
	!
	virtualTenderType$ = Mid$(pData$, 13, 2)
	virtualEntity% = Int%(Val(Mid$(pData$, 15, 5)))
	If Len(pData$) >= 26 Then Begin
		If isAfterCloseHour Then Begin
			tmpTenderType$ = Mid$(pData$, 20, 2)
			tmpEntity% = Int%(Val(Mid$(pData$, 22, 5)))
		Endif Else Begin
			tmpTenderType$ = virtualTenderType$
			tmpEntity% = virtualEntity%		
		Endif
	Endif Else Begin
		tmpTenderType$ = virtualTenderType$
		tmpEntity% = virtualEntity%
	Endif
	Call printDebug("tmpTenderType$="+tmpTenderType$+"/"+virtualTenderType$)
	Call printDebug("tmpEntity%="+str$(tmpEntity%)+"/"+str$(virtualEntity%))
	!
	If pValidate% And TS.PROCEDURE < 1 Then \
		pAnswer% = disc.validateTender(SL.TE.AMTTENDE, virtualTenderType$, virtualEntity%, pAccount$) \
	Else \
		pAnswer% = 1
	Call printDebug("pAnswer%="+Str$(pAnswer%))
End Sub
!
Function tef.validateTender(pData$, pAccount$, pValidateDisc%)
	Integer*1 tef.validateTender, pValidateDisc%, tmpAnswer%
	String pData$, pAccount$, tmpTenderType$
	Integer*2 tmpEntity%, tipoPagoOriginal%, tipoPagoNuevo%
	!
	Call printDebug("EP.TIPO.VAR$="+EP.TIPO.VAR$)
	!
	Call tef.validateDisc(pData$, pAccount$, pValidateDisc%, tmpAnswer%, tmpTenderType$, tmpEntity%)
	! Cuando la respuesta es 2, quiere decir que no se otorgaron descuentos
	! y el presente medio de pago no tiene derecho a descuentos, o sea que está OK
	! Para este caso, no hacemos diferencia, luego la respuesta se asimila a 1
	If tmpAnswer% = 2 Then tmpAnswer% = 1
	!
	If EP.TIPO.VAR$ <> tmpTenderType$ Then Begin
		tipoPagoOriginal% = Val(Left$(EP.TIPO.VAR$,1))
		EP.TIPO.VAR$ = tmpTenderType$
		tipoPagoNuevo% = Val(Left$(EP.TIPO.VAR$,1))
		Call EP.BUSCA.LIMITE.TV(EP.TIPO.VAR$)
		If VAL(EP.VALOR.EPAY$) > EP.MAX.TV% Then \
			tmpAnswer% = 0 \
		Else If TS.PROCEDURE < 1 And (VAL(EP.VALOR.EPAY$) - Abs(TS.BALDUE(0))) > EP.CHG.TV% Then \
			tmpAnswer% = 0 \
		Else Begin
			! Ajusta los totales
			SL.TE.TENDTYPE  = VAL(LEFT$(EP.TIPO.VAR$,1))
			SL.TE.TENDVAR   = VAL(MID$(EP.TIPO.VAR$,2,1))
			TS.IO.DATA$(3) = STR$(SL.TE.TENDVAR)
			TS.IO.KEYS(7) = SL.TE.TENDTYPE + 90
			!
			If TS.NUMTNDRS(TS.TDR.INDEX) > 0 Then Begin
				TS.NUMTNDRS(EP.TV.POS%) = TS.NUMTNDRS(EP.TV.POS%) + 1
				TS.NUMTNDRS(TS.TDR.INDEX) = TS.NUMTNDRS(TS.TDR.INDEX) - 1
				TS.TENDERED(tipoPagoOriginal%) = TS.TENDERED(tipoPagoOriginal%) - VAL(EP.VALOR.EPAY$)
				TS.TENDERED(tipoPagoNuevo%) = TS.TENDERED(tipoPagoNuevo%) + VAL(EP.VALOR.EPAY$)
			Endif
			!
			TS.TENDVAMT(EP.TV.POS%) = TS.TENDVAMT(EP.TV.POS%) + VAL(EP.VALOR.EPAY$)
			TS.TENDVAMT(TS.TDR.INDEX) = TS.TENDVAMT(TS.TDR.INDEX) - VAL(EP.VALOR.EPAY$)
			!
			TS.TDR.INDEX = EP.TV.POS%
		Endif			
	Endif
	Call reasignEntity(Int%(Val(tmpTenderType$)), tmpEntity%)
	!
	tef.validateTender = tmpAnswer%
End Function
!
!-------------------------------------------------------------------------------------
! Despliega una advertencia y retorna lo siguiente:
! 1 : El usuario oprime PLU ante la advertencia (decide continuar)
! 2 : El usuario oprime BORRAR ante la advertencia (cancela la secuencia)
!-------------------------------------------------------------------------------------
Function displayWarning(pMsg$)
	Integer*1 displayWarning
	String pMsg$, tmpAnswer$
	!
	Call EP.SAVE.KEYS
	Call EP.GET.KBDATA(pMsg$, "0", "0", tmpAnswer$)
	Call EP.RESTORE.KEYS
	If tmpAnswer$ = "0" Then Begin
		displayWarning = 1
	Endif Else Begin
		displayWarning = 2
	Endif
End Function
!
Function depurateTenderOptions(pOptions$)
	String depurateTenderOptions, pOptions$, tmpAnswer$, tmpParcial$
	Integer*2 tmpIndex1%, tmpIndex2%, tmpLen%
	!
	tmpAnswer$ = ""
	tmpIndex1% = 0
	tmpLen% = Len(pOptions$)
	!
	While tmpIndex1% < tmpLen%
		tmpIndex2% = Match(":", pOptions$, tmpIndex1% + 1)
		If tmpIndex2% = 0 Then Begin
			pOptions$ = pOptions$ + ":"
			tmpIndex2% = tmpLen% + 1
		Endif
		tmpParcial$ = Mid$(pOptions$, tmpIndex1% + 1, tmpIndex2% - tmpIndex1%)
		If Match(Mid$(tmpParcial$, 13, 14), tmpAnswer$, 1) = 0 Then Begin
			tmpAnswer$ = tmpAnswer$ + tmpParcial$
		Endif
		tmpIndex1% = tmpIndex2%
	Wend		
	!
	depurateTenderOptions = tmpAnswer$
End Function
!
!--------------------------------------------------------------------------------------
! Arma el request para que Java despliegue menú de opciones de tipo de tarjeta
!--------------------------------------------------------------------------------------
Function getTefOptionsRequest(pOptions$)
	String getTefOptionsRequest, pOptions$, tmpAnswer$
	Integer*2 tmpIndex%, tmpLen%
	!
	tmpLen% = Len(pOptions$)
	tmpIndex% = 0
	tmpAnswer$ = ""
	While tmpIndex% < tmpLen%
		tmpAnswer$ = tmpAnswer$ + Mid$(pOptions$, tmpIndex% + 1, 10) + ","
		tmpAnswer$ = tmpAnswer$ + Mid$(pOptions$, tmpIndex% + 11, 2) + ","
		tmpIndex% = Match(":", pOptions$, tmpIndex% + 1)
		If tmpIndex% = 0 Then Begin
			tmpIndex% = tmpLen% + 1
		Endif
	Wend
	!
	getTefOptionsRequest = tmpAnswer$
End Function
!
!--------------------------------------------------------------------------------------
! Recibe un bin de tarjeta y reasigna, si es el caso,
! el medio de pago y la entidad correspondiente a dicho bin
! También valida que el nuevo medio de pago sea consistente
! con algún posible descuento otorgado por medio de pago
! Puede retornar los siguientes valores:
! 1 : Tarjeta correcta, se puede seguir adelante sin restricciones
! 2 : Tarjeta potencialmente correcta, hay mensaje de advertencia y el cajero abandona
! 0 : Tarjeta incorrecta, se rechaza
!
!--------------------------------------------------------------------------------------
Function tef.notifyAccount(pBin$) Public
	Integer*1 tef.notifyAccount, tmpAnswer%, tmpDiscAnswer%, tmpRecAnswer%, tmpRecAnswer1%, tmpTendSaleAnswer1%
	String pBin$, tmpClassName$, tmpMethod$, tmpRequest$, tmpReturn$, tmpException$, tmpDiscDummy1$, tmpDiscFranq$, tmpDiscData$, tmpTendSaleMsg$, tmpTendSaleMsg1$, tmpResponse$
	Integer*2 discAnswer%(1), tmpDiscCounter%, tmpDiscIndex0%, tmpDiscIndex1%, tmpDiscIndex2%, tmpDiscLen%, tmpDiscDummy2%, tmpTendSaleAnswer%, tmpSelectedOption%
	String tmpValid$
	!
	tmpValid$ = ""
	tmpRecAnswer1% = -1
	tmpTendSaleAnswer1% = -1
	tmpTendSaleMsg1$ = ""
	!
	! Remueve espacios y asteriscos a la derecha, si hay
	pBin$ = rRemove$(pBin$, 32)
	pBin$ = rRemove$(pBin$, Asc("*"))
	pBin$ = Left$(pBin$ + String$(6, "0"), 6)
	!
	tmpClassName$ = "com.lacadena.tef.controller.TefTenderManager"
	!tmpMethod$ = "getTenderParamStr"
	tmpMethod$ = "getPossibleTenderParam"
	tmpRequest$ = "notifyAccount"
	tmpReturn$ = ""
	tmpException$ = ""
	!
	Call Javacall.Initialize.Request(tmpClassName$, tmpMethod$, tmpRequest$)
	Call JavaCall.AddParameter.String(tmpRequest$, pBin$)
	!Call JavaCall.AddParameter.String(tmpRequest$, Left$(EP.PREPRODUCTO$, 2))
	Call JavaCall.AddParameter.String(tmpRequest$, EP.APPL$ + EP.FUNCTION$)
	Call JavaCall.AddParameter.String(tmpRequest$, Str$(EP.ACTUALNET%))
	Call JavaCall.InvokeMethod.ReturnString(tmpRequest$, tmpReturn$, tmpException$)
	!
	If tmpException$ <> "" Or tmpReturn$ = "" Then Begin
		tmpAnswer% = 1
	Endif Else Begin
		!
		! La variable tmpReturn$ puede contener una lista de los posibles medios
		! de pago. Hay que validar todos estos medios de pago contra el sistema
		! de promociones
		!
		Call printDebug("tender options=" + tmpReturn$)
		!
		! Se depura tmpReturn$ para evitar procesos repetitivos
		tmpReturn$ = depurateTenderOptions(tmpReturn$)
		!
		Call printDebug("depurt options=" + tmpReturn$)
		!
		Dim discAnswer%(3)
		tmpDiscCounter% = 0
		tmpDiscIndex1% = 0
		tmpDiscIndex2% = Match(":", tmpReturn$, tmpDiscIndex1% + 1)
		tmpDiscIndex0% = tmpDiscIndex2%
		tmpDiscLen% = Len(tmpReturn$)
		tmpDiscFranq$ = ""
		While tmpDiscIndex2% > 0 And tmpDiscIndex1% < tmpDiscLen%
			tmpDiscData$ = Mid$(tmpReturn$, tmpDiscIndex1% + 1, tmpDiscIndex2% - tmpDiscIndex1% - 1)
			Call tef.validateDisc(tmpDiscData$, pBin$, -1, tmpDiscAnswer%, tmpDiscDummy1$, tmpDiscDummy2%)
			Call printDebug("tmpDiscAnswer%="+Str$(tmpDiscAnswer%))
			tmpRecAnswer% = recaudot.validateTender(Mid$(tmpDiscData$, 13, 2), Abs(SL.TE.AMTTENDE), Str$(SL.TE.TENDTYPE * 10 + SL.TE.TENDVAR), Abs(SL.TE.AMTTENDE))
			Call printDebug("tmpRecAnswer%="+Str$(tmpRecAnswer%))
			If tmpRecAnswer% = 0 Then Begin
				tmpDiscAnswer% = 0
				tmpRecAnswer1% = 0
			Endif
			!
			! Validación por productos
			Call TENDSALE.validateTender(Int%(Val(Mid$(tmpDiscData$, 13, 1))), Int%(Val(Mid$(tmpDiscData$, 14, 1))), Int%(Val(Mid$(tmpDiscData$, 15, 5))), pBin$, tmpTendSaleAnswer%, tmpTendSaleMsg$)
			!
			Call printDebug("tmpTendSaleAnswer%=" + Str$(tmpTendSaleAnswer%))
			!
			If tmpTendSaleAnswer% = 0 Then Begin
				tmpDiscAnswer% = 0
				tmpTendSaleAnswer1% = 0
				tmpTendSaleMsg1$ = tmpTendSaleMsg$
			Endif
			!
			Call printDebug("Final tmpDiscAnswer%=" + Str$(tmpDiscAnswer%))
			!
			If tmpDiscAnswer% > 0 Then Begin
				! Esta opción cumplió todas las validaciones
				tmpValid$ = tmpValid$ + "0"
			Endif Else If tmpRecAnswer% = 0 Then Begin
				! Esta opción no cumplió por recaudos
				tmpValid$ = tmpValid$ + "3"
			Endif Else If tmpTendSaleAnswer% = 0 Then Begin
				! Esta opción no cumplió por restricción de artículos
				tmpValid$ = tmpValid$ + "4"
			Endif Else Begin
				! Esta opción no cumplió por temas relativos a descuentos
				If tmpDiscAnswer% > -2 Then Begin
					tmpValid$ = tmpValid$ + Str$(tmpDiscAnswer% + 2)
				Endif Else Begin
					tmpValid$ = tmpValid$ + "2"
				Endif
			Endif
			!
			If tmpDiscAnswer% >= -1 And tmpDiscAnswer% <= 2 Then \
 				discAnswer%(tmpDiscAnswer% + 1) = discAnswer%(tmpDiscAnswer% + 1) + 1
 			If tmpDiscAnswer% = -1 And tmpDiscFranq$ = "" Then tmpDiscFranq$ = Mid$(tmpReturn$, tmpDiscIndex1% + 1, 12)
 			tmpDiscCounter% = tmpDiscCounter% + 1
 			tmpDiscIndex1% = tmpDiscIndex2%
 			If tmpDiscIndex1% < tmpDiscLen% Then \
 				tmpDiscIndex2% = Match(":", tmpReturn$, tmpDiscIndex1% + 1)
		Wend
		!
		Call printDebug("tmpDiscCounter%="+Str$(tmpDiscCounter%))
		Call printDebug("discAnswer%()="+Str$(discAnswer%(0))+"/"+Str$(discAnswer%(1))+"/"+Str$(discAnswer%(2))+"/"+Str$(discAnswer%(3)))
		!
		If discAnswer%(2) = tmpDiscCounter% Or discAnswer%(3) = tmpDiscCounter% Then Begin
			! Todos los medios de pago coinciden con el descuento o
			! no se otorgaron descuentos y los medios de pago son consistentes con eso
			tmpAnswer% = 1
		Endif Else If discAnswer%(0) = tmpDiscCounter% Then Begin
			! No se otorgaron descuentos y todos los medios de pago
			! son merecedores de descuento
			tmpAnswer% = 0
			Call ep.display.an.error(EP.DISC.MSG$(0))
		Endif Else If discAnswer%(1) = tmpDiscCounter% Then Begin
			! Ninguno de los medios de pago coincide con el descuento otorgado
			tmpAnswer% = 0
			If tmpRecAnswer1% = 0 Then Begin
				! Se trata de un recaudo a terceros y ninguno de los medios de pago
				! son aceptados
				Call ep.display.an.error("Medio de pago no    valido para recaudo")
			Endif Else If tmpTendSaleAnswer1% = 0 Then Begin
				! Ninguno de los medios de pago posibles son aceptados por presencia
				! de artículos restringidos
				Call ep.display.an.error(tmpTendSaleMsg1$)
			Endif Else Begin
				Call ep.display.an.error(EP.DISC.MSG$(1))
			Endif		
		!Endif Else If discAnswer%(2) > 0 Then Begin
		!	! Por lo menos un medio de pago coincide con el descuento otorgado
		!	!Call ep.display.an.error("Revise que la tarj  coincida con el dcto")
		!	!tmpAnswer% = displayWarning("PLU=Confirma el pagoBORR=Cancela")
		!	If EP.DISC.MSG$(2) = "" Then \
		!		tmpAnswer% = 1 \
		!	Else \
		!		tmpAnswer% = displayWarning(EP.DISC.MSG$(2) + "Intro=Continuar")
		!Endif Else If discAnswer%(3) > 0 Then Begin
		!	! Por lo menos un medio de pago otorga descuentos que no se ofrecieron
		!	!Call ep.display.an.error("Con " + tmpDiscFranq$ + "    puede obtener dctos")
		!	!tmpAnswer% = displayWarning("PLU=Sigue sin dctos BORR=Cancela")
		!	If tmpRecAnswer1% = 0 Then Begin
		!		! Por lo menos un medio de pago no es aceptado para recaudo
		!		tmpAnswer% = displayWarning("Tarjeta puede no ser" + "aceptada Intro=Sigue")
		!	Endif Else If tmpTendSaleAnswer1% = 0 Then Begin
		!		! Por lo menos un medio de pago posible no es aceptado por presencia
		!		! de artículos restringidos
		!		tmpAnswer% = displayWarning("Tarjeta puede no ser" + "aceptada Intro=Sigue")
		!	Endif Else Begin
		!		If EP.DISC.MSG$(3) = "" Then \
		!			tmpAnswer% = 1 \
		!		Else \
		!			tmpAnswer% = displayWarning(Left$(EP.DISC.MSG$(3) + tmpDiscFranq$ + String$(20, " "), 20) + "Intro=Sigue sin dcto")
		!	Endif
		Endif Else Begin
			!-------------------------------------------------------------
			! 2018-03-08 jsv
			! En caso de que algunas opciones cumplan las condiciones de
			! descuentos, recaudo o artículos, y otras no, se despliega
			! menú para que el cajero seleccione el tipo de tarjeta
			!-------------------------------------------------------------
			tmpSelectedOption% = 0
			tmpRequest$ = getTefOptionsRequest(tmpReturn$)
			tmpResponse$ = EP.invocarApplManager0("85", "00", tmpRequest$, 0, -1)
			tmpAnswer% = 0
			If Len(tmpResponse$) > 0 Then Begin
				! El usuario seleccionó una opción de menú. Se debe verificar si dicha opción
				! cumple las validaciones
				If Int%(Val(tmpResponse$)) > 0 And Int%(Val(tmpResponse$)) <= Len(tmpValid$) Then Begin
					tmpAnswer% = Int%(Val(Mid$(tmpValid$, Int%(Val(tmpResponse$)), 1)))
				Endif
				If tmpAnswer% = 0 Then Begin
					! Este medio de pago cumple con todas las validaciones
					tmpAnswer% = 1
				Endif Else Begin
					tmpAnswer% = 0
					If tmpAnswer% = 1 Then Begin
						! No se otorgaron descuentos y este medio de pago
						! tiene derecho a descuentos
						Call ep.display.an.error(EP.DISC.MSG$(0))
					Endif Else If tmpAnswer% = 2 Then Begin
						! Este medio de pago no coincide con el descuento
						! otorgado
						Call ep.display.an.error(EP.DISC.MSG$(1))
					Endif Else If tmpAnswer% = 3 Then Begin
						! Este medio de pago no cumple por recaudos
						Call ep.display.an.error("Medio de pago no    valido para recaudo")
					Endif Else Begin
						If Len(tmpTendSaleMsg1$) > 0 Then Begin
							Call ep.display.an.error(tmpTendSaleMsg1$)
						Endif Else Begin
							Call ep.display.an.error("Medio de pago no    valido")
						Endif
					Endif
				Endif
			Endif
		Endif
		!
		If tmpAnswer% = 1 Then Begin
			tmpReturn$ = Left$(tmpReturn$, tmpDiscIndex0% - 1)
			If Len(TEFII.FORMAT.RTRIM$(tmpReturn$)) > 26 Then Begin
				! Hay un archivo de parámetros asociado al bin
				! debe cargarlo en forma temporal
				!temporalParamFileLoaded% = -1
				Call TEFII.FUNCT.INITIALIZEPARAM("R::ADX_IDT1:" + Mid$(tmpReturn$, 27, 12))
			Endif
!			Endif Else If temporalParamFileLoaded% Then Begin
!				temporalParamFileLoaded% = 0
!				Call TEFII.FUNCT.INITIALIZEPARAM("0")
!			Endif
		Endif
	Endif
	tef.notifyAccount = tmpAnswer%
End Function
!
! Recibe un bin de tarjeta y reasigna, si es el caso,
! el medio de pago y la entidad correspondiente a dicho bin
! También valida que el nuevo medio de pago sea consistente
! con algún posible descuento otorgado por medio de pago
Function tef.notifyResponse(pFranquicia$, pBin$) Public
	Integer*1 tef.notifyResponse
	String pFranquicia$, pBin$, tmpClassName$, tmpMethod$, tmpRequest$, tmpReturn$, tmpException$
	!
	Call printDebug("tef.notifyResponse")
	!
	tmpClassName$ = "com.lacadena.tef.controller.TefTenderManager"
	tmpMethod$ = "getTenderParamFrq"
	tmpRequest$ = "notifyResponse"
	tmpReturn$ = ""
	tmpException$ = ""
	!
	If Len(launchApplFunc$) > 0 Then Begin
		tmpReturn$ = launchApplFunc$
		launchApplFunc$ = ""
	Endif Else Begin
		Call Javacall.Initialize.Request(tmpClassName$, tmpMethod$, tmpRequest$)
		Call JavaCall.AddParameter.String(tmpRequest$, pFranquicia$)
		Call JavaCall.AddParameter.String(tmpRequest$, Left$(EP.PREPRODUCTO$, 2))
		Call JavaCall.AddParameter.String(tmpRequest$, pBin$)
		Call JavaCall.AddParameter.String(tmpRequest$, EP.APPL$ + EP.FUNCTION$)
		Call JavaCall.AddParameter.String(tmpRequest$, Str$(EP.ACTUALNET%))
		Call JavaCall.InvokeMethod.ReturnString(tmpRequest$, tmpReturn$, tmpException$)
	Endif
	!
	Call printDebug("tmpReturn$="+tmpReturn$)
	Call printDebug("tmpException$="+tmpException$)
	If tmpException$ <> "" Or tmpReturn$ = "" Then Begin
		tmpReturn$ = String$(12, "0") + EP.DEFAULT.TV$ + Right$(String$(5, "0") + Str$(EP.DEFAULT.ENT%), 5)
	Endif
	tef.notifyResponse = tef.validateTender(tmpReturn$, pBin$, -1)
End Function
!
! Recibe una franquicia y un bin de tarjeta y retorna el medio de pago
! y la entidad correspondiente
Sub tef.getTender(pFranquicia$, pBin$, pApplFunc$, pProduct$, pTender$, pEntity%) Public
	String pFranquicia$, pBin$, pApplFunc$, pProduct$, pTender$
	Integer*2 pEntity%
	String tmpClassName$, tmpMethod$, tmpRequest$, tmpReturn$, tmpException$
	String virtualTenderType$
	Integer*2 virtualEntity%
	!
	Call printDebug("tef.getTender")
	!
	tmpClassName$ = "com.lacadena.tef.controller.TefTenderManager"
	tmpMethod$ = "getTenderParamFrq"
	tmpRequest$ = "getTender"
	tmpReturn$ = ""
	tmpException$ = ""
	!
	Call Javacall.Initialize.Request(tmpClassName$, tmpMethod$, tmpRequest$)
	Call JavaCall.AddParameter.String(tmpRequest$, pFranquicia$)
	Call JavaCall.AddParameter.String(tmpRequest$, Left$(pProduct$, 2))
	Call JavaCall.AddParameter.String(tmpRequest$, pBin$)
	Call JavaCall.AddParameter.String(tmpRequest$, pApplFunc$)
	Call JavaCall.AddParameter.String(tmpRequest$, Str$(EP.ACTUALNET%))
	Call JavaCall.InvokeMethod.ReturnString(tmpRequest$, tmpReturn$, tmpException$)
	!
	Call printDebug("tmpReturn$="+tmpReturn$)
	Call printDebug("tmpException$="+tmpException$)
	If tmpException$ <> "" Or tmpReturn$ = "" Then Begin
		tmpReturn$ = String$(12, "0") + EP.DEFAULT.TV$ + Right$(String$(5, "0") + Str$(EP.DEFAULT.ENT%), 5)
	Endif
	!
	virtualTenderType$ = Mid$(tmpReturn$, 13, 2)
	virtualEntity% = Int%(Val(Mid$(tmpReturn$, 15, 5)))
	If Len(tmpReturn$) >= 26 Then Begin
		If isAfterCloseHour Then Begin
			pTender$ = Mid$(tmpReturn$, 20, 2)
			pEntity% = Int%(Val(Mid$(tmpReturn$, 22, 5)))
		Endif Else Begin
			pTender$ = virtualTenderType$
			pEntity% = virtualEntity%		
		Endif
	Endif Else Begin
		pTender$ = virtualTenderType$
		pEntity% = virtualEntity%
	Endif
End Sub
!
Function isApprovCode(pCode$) Public
	Integer*1 isApprovCode
	String pCode$
	!
	If 								\
			pCode$ = "00" Or 		\
			pCode$ = "08" Or 		\
			pCode$ = "11" Or 		\
			pCode$ = "76" Or 		\
			pCode$ = "77" Or 		\
			pCode$ = "78" Or 		\
			pCode$ = "79" Or 		\
			pCode$ = "80" Or 		\
			pCode$ = "81" Then 		\
	isApprovCode = -1 				\
Else 								\
	isApprovCode = 0
End Function
!
!---------------------------------------------------------------------------
!  User defined routines
!
!
Sub initializeTefArray public
	dim tefTrxArray$(50,15)	! Arreglo con datos de transacciones tef aprobadas
	tefTrxCounter% = 0
end sub
!
Function searchTefArray(pRecibo$)
	Integer*1 searchTefArray,searchIndex%,searchAnswer%
	String pRecibo$
	!
	pRecibo$ = Right$(String$(6,"0") + pRecibo$,6)
	!
	searchAnswer% = -1
	searchIndex% = 0
	while searchAnswer% = -1 And searchIndex% < tefTrxCounter%
		searchIndex% = searchIndex% + 1
		if tefTrxArray$(searchIndex%,1) = pRecibo$ Then \
			searchAnswer% = searchIndex%
	wend
	searchTefArray = searchAnswer%
End Function
!
!SUB PRINT.EFT.HEADER.DF PUBLIC
!!
!  CALL EP.SAVE.PRINT
!  TS.LINETYPE = 29
!  TS.SAVPRT$ = STRING$(38," ")
!  TS.SAVPRT.OPT = 4100H
!  CALL TSPREC01
!!  
!  TO.HEADERLINE1$  = LEFT$(EP.GUIDE.MESSAGE$(18),30)
!  TO.HEADERLINE2$  = STRING$(38," ")
!  TS.LINETYPE = 18 
!  TS.LINEDATA = 99
!  CALL TSPREC01
!  TO.HEADERLINE1$ = EP.SAV.HD1$
!  TO.HEADERLINE2$ = EP.SAV.HD2$
!
!  CALL EP.RESTORE.PRINT
!END SUB
!
!Sub PRINT.EFT.HEADER.DF1 Public
!!
!	CALL EP.LINE.PRINT(String$(38," "),4100H)
!	CALL EP.LINE.PRINT(Left$(EP.GUIDE.MESSAGE$(18),30),4100H)
!!
!End Sub
!
!
SUB PRINT.EFT.DETAIL.DF(UE.VALOR.EPAY$, UE.TAX.EPAY$) PUBLIC
  INTEGER*4 UE.INT4%
  INTEGER*2 UE.COUNT%
  INTEGER*1 UE.INT1%
  STRING    UE.VALOR.EPAY$, UE.TAX.EPAY$
! 
  CALL EP.SAVE.PRINT
  IF EP.FUNCTION$ = "11" OR \
     EP.FUNCTION$ = "12" OR \
     EP.Function$ = "51" OR \
     EP.Function$ = "52" OR \
     EP.FUNCTION$ = "14" OR \
     EP.FUNCTION$ = "19" THEN \
  BEGIN
    !Call EP.LINE.PRINT(Left$(EP.GUIDE.MESSAGE$(18),30),6100H)
    Call EP.LINE.PRINT(Left$(EP.SLOGAN$(EP.ACTUALNET%), 30), 6100H)
    CALL EP.LINE.PRINT("AUTORIZA="+EP.AUTH.NUMBER$+ "  TARJ="+EP.CARD.NUMBER$,6100H)  
    CALL EP.LINE.PRINT("RECIBO  ="+EP.RECIBO$,6100H) 
    CALL EP.LINE.PRINT(STRING$(38," "),6100H) 
  ENDIF
  CALL EP.RESTORE.PRINT
END SUB

Sub PRINT.EFT.DETAIL.DF2(UE.VALOR.EPAY$, UE.TAX.EPAY$) Public
  Integer*4 UE.INT4%
  Integer*2 UE.COUNT%
  Integer*1 UE.INT1%
  String    UE.VALOR.EPAY$, UE.TAX.EPAY$ 
  Call EP.SAVE.PRINT
! Call EP.LINE.PRINT("FUNCION 2->"+EP.Function$,6100H)  
  If EP.Function$ = "11" OR \
     EP.Function$ = "12" OR \
     EP.Function$ = "51" OR \
     EP.Function$ = "52" OR \
     EP.Function$ = "14" OR \
     EP.Function$ = "19" Then \
  Begin
    !Call EP.LINE.PRINT(Left$(EP.GUIDE.MESSAGE$(18),30),6100H)
    Call EP.LINE.PRINT(Left$(EP.SLOGAN$(EP.ACTUALNET%), 30), 6100H)
    Call EP.LINE.PRINT("AUTORIZA="+EP.AUTH.NUMBER2$+ "  TARJ="+EP.CARD.NUMBER2$,6100H)  
    Call EP.LINE.PRINT("RECIBO  ="+EP.RECIBO2$,6100H) 
    Call EP.LINE.PRINT(String$(38," "),6100H) 
  EndIf
  Call EP.RESTORE.PRINT
End Sub
!
Sub asignarSlogan(pValue$)
	String pValue$
	Integer*2 tmpCounter%, lastI%, actualI%
	!
	pValue$ = pValue$ + ","
	lastI% = 0
	For tmpCounter% = 1 To EP.NETCNT%
		If lastI% < Len(pValue$) Then Begin
			actualI% = Match(",", pValue$, lastI% + 1)
			EP.SLOGAN$(tmpCounter%) = Mid$(pValue$, lastI% + 1, actualI% - lastI% - 1)
			lastI% = actualI%
		Endif Else Begin
			EP.SLOGAN$(tmpCounter%) = EP.SLOGAN$(tmpCounter% - 1)
		Endif
	Next tmpCounter%
End Sub
!
Sub asignarNetDesc(pValue$)
	String pValue$
	Integer*2 tmpCounter%, lastI%, actualI%
	!
	pValue$ = pValue$ + ","
	lastI% = 0
	For tmpCounter% = 1 To EP.NETCNT%
		If lastI% < Len(pValue$) Then Begin
			actualI% = Match(",", pValue$, lastI% + 1)
			EP.NETDESC$(tmpCounter%) = Mid$(pValue$, lastI% + 1, actualI% - lastI% - 1)
			lastI% = actualI%
		Endif Else Begin
			EP.NETDESC$(tmpCounter%) = EP.NETDESC$(tmpCounter% - 1)
		Endif
	Next tmpCounter%
End Sub
!
Sub asignarHoraCierre(pValue$)
	String pValue$
	Integer*2 tmpCounter%, lastI%, actualI%
	!
	pValue$ = pValue$ + ","
	lastI% = 0
	For tmpCounter% = 1 To EP.NETCNT%
		If lastI% < Len(pValue$) Then Begin
			actualI% = Match(",", pValue$, lastI% + 1)
			EP.HORACIERRE$(tmpCounter%) = Mid$(pValue$, lastI% + 1, actualI% - lastI% - 1)
			lastI% = actualI%
		Endif Else Begin
			EP.HORACIERRE$(tmpCounter%) = EP.HORACIERRE$(tmpCounter% - 1)
		Endif
	Next tmpCounter%
End Sub

Sub asignarAnulPass(pValue$)
	String pValue$
	Integer*2 tmpCounter%, lastI%, actualI%
	!
	pValue$ = pValue$ + ","
	lastI% = 0
	For tmpCounter% = 1 To EP.NETCNT%
		If lastI% < Len(pValue$) Then Begin
			actualI% = Match(",", pValue$, lastI% + 1)
			EP.CLAVEANUL$(tmpCounter%) = Mid$(pValue$, lastI% + 1, actualI% - lastI% - 1)
			lastI% = actualI%
		Endif Else Begin
			EP.CLAVEANUL$(tmpCounter%) = EP.CLAVEANUL$(tmpCounter% - 1)
		Endif
	Next tmpCounter%
End Sub
!
Sub asignarParamSerialesStr(pValue$, pIndex%)
	String pValue$
	Integer*2 pIndex%, tmpCounter%, lastI%, actualI%
	!
	pValue$ = pValue$ + ","
	lastI% = 0
	For tmpCounter% = 1 To EP.NETCNT%
		If lastI% < Len(pValue$) Then Begin
			actualI% = Match(",", pValue$, lastI% + 1)
			EP.SERIALPARAM$(tmpCounter%, pIndex%) = Mid$(pValue$, lastI% + 1, actualI% - lastI% - 1)
			lastI% = actualI%
		Endif Else Begin
			EP.SERIALPARAM$(tmpCounter%, pIndex%) = EP.SERIALPARAM$(tmpCounter% - 1, pIndex%)
		Endif
	Next tmpCounter%
End Sub
!
Sub asignarParamSerialesNum(pValue$, pIndex%)
	String pValue$
	Integer*2 pIndex%, tmpCounter%, lastI%, actualI%
	!
	pValue$ = pValue$ + ","
	lastI% = 0
	For tmpCounter% = 1 To EP.NETCNT%
		If lastI% < Len(pValue$) Then Begin
			actualI% = Match(",", pValue$, lastI% + 1)
			EP.SERIALPARAM%(tmpCounter%, pIndex%) = Int%(Val(Mid$(pValue$, lastI% + 1, actualI% - lastI% - 1)))
			lastI% = actualI%
		Endif Else Begin
			EP.SERIALPARAM%(tmpCounter%, pIndex%) = EP.SERIALPARAM%(tmpCounter% - 1, pIndex%)
		Endif
	Next tmpCounter%
End Sub
!
Sub assignDefaultTV(pSource$, pTV$, pEntity%)
	String pSource$, pTV$
	Integer*2 pEntity%, tmpIndex%
	!
	On Error Goto assignDefaultTV.err
	!
	tmpIndex% = Match("@", pSource$, 1)
	If tmpIndex% > 0 And tmpIndex% < Len(pSource$) Then Begin
		pTV$ = Left$(pSource$, tmpIndex% - 1)
		pEntity% = Int%(Val(Right$(pSource$, Len(pSource$) - tmpIndex%)))
	Endif Else Begin
		pTV$ = pSource$
		pEntity% = 0
	Endif
	!
	Goto assignDefaultTV.end
	!
	assignDefaultTV.err:
		pEntity% = 0
		Resume assignDefaultTV.end
	assignDefaultTV.end:
		pTV$ = Right$(String$(2, "0") + pTV$, 2)
End Sub
!
SUB EPAY.INITIALIZATION
  STRING UE.VAL1$, UE.VAL2$, EP.LAST$,especialMP$, tmpNet$
  INTEGER*2 UE.VAL2%, EP.I%, EP.PARM%, UE.Q%
!
  !
  logRecordSize% = 38
  logMask$ = "C12 C10 C6 C10"
  !
  EP.defaultApp$ = "85"
  !
  SERIAL.REQUEST$ = "C$"
  call initializeTefArray
  !
  Dim EP.DISC.MSG$(3)
  !
  ! Identificación de la red a la cual está asociada la terminal
  tmpNet$ = getJavaProperty("RED_ASOCIADA", "R::ADX_IDT1:TERPARMT." + Right$(String$(3, "0") + TS.TERMINAL$, 3), EP.IOPARM%)
  If tmpNet$ = "" Then tmpNet$ = "1"
  EP.ACTUALNET% = Int%(Val(tmpNet$))
  Call tef.initializeTender
  !
!---------------------------------------------------------
  EP.TRX.FUNCTION$  = ""
  EP.TRX.TRANSNUM$  = ""
  EP.TRX.RESP%      = 0
  EP.TRX.APPROV%    = 0
  EP.TRX.VOUCHER%   = 0
  EP.TRX.TOHOST1%   = 0 
  EP.TRX.TOHOST2%   = 0
  EP.TRX.ANUL%      = 0
  EP.TRX.REVERSO%   = 0
  EP.TRX.PEND%      = 0   
  EP.TRX.FECHA$     = ""
  EP.LAST.RECIBO$   = STRING$(6,"0")   
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
  !IF EP.EFT.CLAVE$ = "" THEN \
  !BEGIN 
    TS.ER.RETURN = -1
    OPEN "R::ADX_IDT1:TEFPARM.DAT" AS EP.IOPARM% NOWRITE NODEL
    IF NOT TS.ER.RETURN  THEN \
    BEGIN
      CALL EP.DISPLAY.AN.ERROR("Imposible abrir TEFPARM")
      GOTO NOMORE.MESSAGES
    ENDIF
    IF END # EP.IOPARM% THEN  CLOSE.MESSAGES
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.EFT.ACTIVO% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    If Left$(UE.VAL1$, 28) = "Parametro activacion cheques" Then Begin
    	EP.checkTrxActive% = Val(UE.VAL2$)
    	Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Endif Else Begin
    	EP.checkTrxActive% = -1
    Endif
    EP.EFT.CLAVE$ = LEFT$(UE.VAL2$ + STRING$(8,"0"),8)
    EP.LEN.CLAVE% = LEN(EP.EFT.CLAVE$)
    
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    If Left$(UE.VAL1$,27) = "No sesion debug controlador" Then Begin
    	! Este parámetro no se usa en la aplicación de terminal
    	READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Endif
    
    IF LEFT$(UE.VAL1$,17) = "EP.initialTefKey%" THEN \
    BEGIN
    	EP.initialTefKey% = INT%(VAL(UE.VAL2$))
    	READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    	!
    	! Tecla para cierre integrado
    	If Left$(UE.VAL1$, 16) = "EP.settlementKey" Then Begin
    		EP.settlementKey% = Int%(Val(UE.VAL2$))
    		Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
    	Endif Else Begin
    		EP.settlementKey% = 0
    	Endif
    	!
    	! Tecla para contactless y otros medios de pago alternativos
    	If Left$(UE.VAL1$, 14) = "EP.tendMenuKey" Then Begin
    		EP.tendMenuKey% = Int%(Val(UE.VAL2$))
    		Read # EP.IOPARM% ; UE.VAL1$, UE.VAL2$
    	Endif Else Begin
    		EP.tendMenuKey% = 0
    	Endif
    	!
    	! Otras teclas asociadas a TEF
    	If Left$(UE.VAL1$, 15) = "EP.otherTefKeys" Then Begin
    		Call fillOtherTefKeys(UE.VAL2$)
    		Read # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    	Endif
    	!
    	EP.controlAnuladoTotal% = INT%(VAL(UE.VAL2$))
    	READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    ENDIF \
    ELSE \
    BEGIN
    	EP.initialTefKey% = 176
    	EP.controlAnuladoTotal% = 0
    ENDIF
    
    If Left$(UE.VAL1$, 15) = "EP.SWITCH.PROC%" Then Begin
    	EP.SWITCH.PROC% = Int%(Val(UE.VAL2$))
    	READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Endif Else Begin
    	EP.SWITCH.PROC% = 0
    Endif

    !EP.TV.CHEQ$ = RIGHT$(STRING$(2,"0") + UE.VAL2$,2)
    Call assignDefaultTV(UE.VAL2$, EP.TV.CHEQ$, EP.EN.CHEQ%)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    !EP.TV.TJDB$ = RIGHT$(STRING$(2,"0") + UE.VAL2$,2)
    Call assignDefaultTV(UE.VAL2$, EP.TV.TJDB$, EP.EN.TJDB%)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    !EP.TV.TJCR$ = RIGHT$(STRING$(2,"0") + UE.VAL2$,2)
    Call assignDefaultTV(UE.VAL2$, EP.TV.TJCR$, EP.EN.TJCR%)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    !EP.TV.TJAV$ = RIGHT$(STRING$(2,"0") + UE.VAL2$,2)
    Call assignDefaultTV(UE.VAL2$, EP.TV.TJAV$, EP.EN.TJAV%)
	!
	especialMP$ = EP.TV.TJCR$ + EP.TV.TJAV$
	If len(especialMP$) >= 4 Then Begin
		EP.ESPECIAL.COMPRA$ = Left$(especialMP$,2)
		EP.ESPECIAL.AVANCE$ = Mid$(especialMP$,3,2)
	Endif
	!
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.IVADEV$ = "%" + UE.VAL2$ + "%"
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.EIAP% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.PROMPT$ = UE.VAL2$
    !
    ! Descriptores para interacción con sistema de descuentos
    Read # EP.IOPARM% ; UE.VAL1$, EP.DISC.MSG$(0)
    Read # EP.IOPARM% ; UE.VAL1$, EP.DISC.MSG$(1)
    Read # EP.IOPARM% ; UE.VAL1$, EP.DISC.MSG$(2)
    EP.DISC.MSG$(2) = Left$(EP.DISC.MSG$(2) + String$(20, " "), 20)
    Read # EP.IOPARM% ; UE.VAL1$, EP.DISC.MSG$(3)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.NETCNT% = VAL(UE.VAL2$)
    !
    If EP.NETCNT% < 1 Then EP.NETCNT% = 1
    Dim EP.SERIALPARAM%(EP.NETCNT%, 4)
    Dim EP.SERIALPARAM$(EP.NETCNT%, 2)
    Dim EP.SLOGAN$(EP.NETCNT%)
    Dim EP.NETDESC$(EP.NETCNT%)
    Dim EP.CLAVEANUL$(EP.NETCNT%)
    Dim EP.HORACIERRE$(EP.NETCNT%)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Call asignarSlogan(UE.VAL2$)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Call asignarNetDesc(UE.VAL2$)
    EP.ACTUALNETDESC$ = EP.NETDESC$(EP.ACTUALNET%)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Call asignarAnulPass(UE.VAL2$)
    EP.ACTUALCLAVEANUL$ = Right$(String$(4, "0") + EP.CLAVEANUL$(EP.ACTUALNET%), 4)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Call asignarHoraCierre(UE.VAL2$)
    !
    For EP.I% = 1 To 4
	    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
	    Call asignarParamSerialesNum(UE.VAL2$, EP.I%)
	Next EP.I%
	!
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Call asignarParamSerialesStr(UE.VAL2$, 1)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    Call asignarParamSerialesStr(UE.VAL2$, 2)
    !
    EP.COM% = EP.SERIALPARAM%(EP.ACTUALNET%, 1)
    EP.SPEED% = EP.SERIALPARAM%(EP.ACTUALNET%, 2)
    EP.DATA.BITS% = EP.SERIALPARAM%(EP.ACTUALNET%, 3)
    EP.STOP.BITS% = EP.SERIALPARAM%(EP.ACTUALNET%, 4)
    EP.PARITY$ = EP.SERIALPARAM$(EP.ACTUALNET%, 1)
    EP.SERIAL.MISC$ = EP.SERIALPARAM$(EP.ACTUALNET%, 2)
    !
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.MAX.TRIES% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.CHARS.WAIT% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.SHORT.WAIT% = VAL(UE.VAL2$)
    READ # EP.IOPARM% ; UE.VAL1$,UE.VAL2$
    EP.LONG.WAIT% = VAL(UE.VAL2$)
CLOSE.MESSAGES:    
    CLOSE EP.IOPARM%
  !ENDIF
NOMORE.MESSAGES:
!temporalParamFileLoaded% = 0
Call TEFII.FUNCT.INITIALIZEPARAM("")
Call TEFII.FORMAT.INITIALIZEPARAM
!
END SUB
!
Sub EP.SEnd.MSG.TO.AMJ Public
!
    EP.EXCEPTION$=""
    EP.AMJ.MESSAGE$ = ""
!
    ! Call tef ii serial basic module
    EP.RETURNVALUE$ = TEFII.FUNCT.APPLKERNEL$(EP.MESSAGE$)
!
    If Len(EP.EXCEPTION$) > 0 Then Begin
      Call EP.LINE.PRINT("Error en el envio",4100H)
      Call EP.LINE.PRINT(Left$(EP.EXCEPTION$,38),4100H)
      EP.ERRNCODE% = 1
      EP.GOOD.End% = 0
    EndIf Else Begin
      EP.ERRNCODE% = 0
      EP.GOOD.End% = -1
      EP.AMJ.MESSAGE$ = EP.RETURNVALUE$
    EndIf
End Sub
!
!
Sub EP.EXECUTE.SPECIALSUB(pAnswer$) PUBLIC
!
	String pAnswer$,tmpParam$
	Integer*2 subIndex%
	!
    EP.APPL.STATUS$   = ""
!
    If EP.M.LEN% >= 10 Then \
      EP.APPL.STATUS$ = Mid$(EP.AMJ.MESSAGE$,10,2)
    If EP.M.LEN% >= 12 Then \
      subIndex% = Val(Mid$(EP.AMJ.MESSAGE$,12,2))
    If EP.M.LEN% >= 14 Then \
    	tmpParam$ = Mid$(EP.AMJ.MESSAGE$,14,EP.M.LEN% - 13)
    If Left$(tmpParam$,2) = "XX" Then tmpParam$ = ""
    !
    If subIndex% = 0 Then \
    Begin
    	If Not tmpAnulacion% Then \
    		Call EP.VALIDATE.DCTOS(pAnswer$,tmpParam$,EP.VALOR.EPAY$,EP.TAX.EPAY$,EP.IVA.BASE$,tmpAnulacion%) \
    	Else \
    		pAnswer$ = "0"
    Endif
!      
End Sub
!
!
SUB EP.PARSE.DBCR.RESPONSE.DF
!
  EP.H.AUTH.NUMBER$ = ""
  EP.H.APPROV.CODE$ = ""
  EP.H.APPROV.DESC$ = ""
  EP.H.CARD.NUMBER$ = ""
  EP.COMERCIO$    = ""
  EP.EPAY.TERMINAL$= ""
  EP.CUOTAS.QTY$  = ""
  EP.FECHA.POSTEO$= ""
  EP.FECHA.PROC$  = ""
  EP.COD.PROC$    = ""
  EP.RRN$         = ""
  EP.H.TIPO.VAR$  = ""
  EP.FRANQUICIA$  = ""
  EP.RECIBO$      = ""
  EP.PRODUCTO$    = ""
  EP.H.USER.DATA$ = ""
  EP.APPL.STATUS$ = ""
  EP.FECHA.VENC$  = ""
  EP.CARD.BIN$    = ""
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
      EP.H.CARD.NUMBER$ = MID$(EP.AMJ.MESSAGE$,40,4)
    IF EP.M.LEN% >= 44 THEN  \
      EP.COMERCIO$ = MID$(EP.AMJ.MESSAGE$,44,10)
    IF EP.M.LEN% >= 54 THEN  \
      EP.EPAY.TERMINAL$ = MID$(EP.AMJ.MESSAGE$,54,8)
    IF EP.M.LEN% >= 62 THEN  \
      EP.CUOTAS.QTY$ = MID$(EP.AMJ.MESSAGE$,62,2)
    IF EP.M.LEN% >= 64 THEN  \
      EP.FECHA.POSTEO$ = MID$(EP.AMJ.MESSAGE$,64,4)
    IF EP.M.LEN% >= 68 THEN  \
      EP.FECHA.PROC$ = MID$(EP.AMJ.MESSAGE$,68,12)
    IF EP.M.LEN% >= 80 THEN  \
      EP.COD.PROC$ = MID$(EP.AMJ.MESSAGE$,80,6)
    IF EP.M.LEN% >= 86 THEN  \
      EP.RRN$ = MID$(EP.AMJ.MESSAGE$,86,12)
    IF EP.M.LEN% >= 98 THEN  \
      EP.H.TIPO.VAR$ = MID$(EP.AMJ.MESSAGE$,98,2)
    IF EP.M.LEN% >= 100 THEN  \
      EP.FRANQUICIA$ = MID$(EP.AMJ.MESSAGE$,100,10)
    IF EP.M.LEN% >= 110 THEN  \
      EP.RECIBO$  = MID$(EP.AMJ.MESSAGE$,110,10)
    IF EP.M.LEN% >= 120 THEN BEGIN
      EP.PRODUCTO$ = MID$(EP.AMJ.MESSAGE$,120,10)
      EP.PREPRODUCTO$ = EP.PRODUCTO$
    ENDIF
    EP.H.USER.DATA$ = EP.FRANQUICIA$ + EP.RECIBO$ + EP.PRODUCTO$
    IF EP.M.LEN% >= 130 THEN \
      EP.FECHA.VENC$ = MID$(EP.AMJ.MESSAGE$,130,4)
    IF EP.M.LEN% >= 134 THEN \
      EP.CARD.BIN$ = MID$(EP.AMJ.MESSAGE$,134,6)    
END SUB
!
SUB EP.PARSE.CHEQUE.RESPONSE.DF
!
  EP.H.AUTH.NUMBER$ = ""
  EP.H.APPROV.CODE$ = ""
  EP.H.APPROV.DESC$ = ""
  EP.COMERCIO$      = ""
  EP.EPAY.TERMINAL$ = ""
  EP.FECHA.POSTEO$  = ""
  EP.FECHA.PROC$    = ""
  EP.COD.PROC$      = ""
  EP.RRN$           = ""
  EP.H.TIPO.VAR$    = ""
  EP.FRANQUICIA$    = ""
  EP.BANCO$         = ""
  EP.RECIBO$        = ""
  EP.PRODUCTO$      = ""
  EP.H.USER.DATA$   = ""
  EP.APPL.STATUS$   = ""
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
      EP.H.TIPO.VAR$ = MID$(EP.AMJ.MESSAGE$,92,2)
    IF EP.M.LEN% >= 94 THEN  \
      EP.FRANQUICIA$ = MID$(EP.AMJ.MESSAGE$,94,10)
    IF EP.M.LEN% >= 104 THEN  \
      EP.RECIBO$ = MID$(EP.AMJ.MESSAGE$,104,10)
    IF EP.M.LEN% >= 114 THEN \
      EP.PRODUCTO$ = MID$(EP.AMJ.MESSAGE$,114,10)
    ! Se agrega No. cuenta, No. cheque y telefono girador
    If EP.M.LEN% >= 124 Then \
      EP.CTA.CTE$ = Mid$(EP.AMJ.MESSAGE$,124,13)
    If EP.M.LEN% >= 137 Then \
      EP.NRO.CHEQUE$ = Mid$(EP.AMJ.MESSAGE$,137,10)
    If EP.M.LEN% >= 147 Then \
      EP.TEL.GIRADOR$ = Mid$(EP.AMJ.MESSAGE$,147,7)      
    !    
    EP.H.USER.DATA$ = EP.FRANQUICIA$  + EP.RECIBO$ + EP.PRODUCTO$
END SUB
!
Sub getMontoReciboAnterior(pRecibo$, pMonto%, pBin$, pOp$)
	Integer*4 pMonto%, tmpAnswer%
	String pRecibo$, pBin$, pOp$, tmpKey$, tmpField1$, tmpField2$, tmpField3$, tmpField4$
	Integer*1 tmpOpen%
	!
	tmpField3$ = ""
	Call printDebug("Busca monto para recibo " + pRecibo$)
	tmpOpen% = 0
	tmpAnswer% = -1
	tmpKey$ = Right$(String$(6, "0") + TS.TERMINAL$, 6) + Right$(String$(6, "0") + pRecibo$, 6)
	!
	On Error Goto getMontoReciboAnterior.err
	Open "R::ADX_IDT4:TEFLOG.DAT" Keyed Recl logRecordSize% As EP.IOPARM% Nowrite Nodel
	tmpOpen% = -1
	Read Form logMask$ ; #EP.IOPARM% Key tmpKey$ ; tmpField1$, tmpField2$, tmpField3$, tmpField4$
	tmpAnswer% = Int%(Val(tmpField2$))
	!
	Goto getMontoReciboAnterior.end
	!
	getMontoReciboAnterior.err:
		tmpAnswer% = -1
		Resume getMontoReciboAnterior.end
	getMontoReciboAnterior.end:
	If tmpOpen% Then Close EP.IOPARM% Else Call printDebug("No abrio archivo log")
	pBin$ = tmpField3$
	pMonto% = tmpAnswer%
	pOp$ = Right$(String$(12, "0") + tmpField4$, 12)
	!
	Call printDebug("Monto:" + Str$(pMonto%) + " Bin:" + pBin$)
End Sub
!
SUB INIT.EFT.API(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.PROPINA$, \
  EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$, \
  EP.TIPO.VAR$, EP.USER.DATA$) PUBLIC

  Integer*1 tsCounter%
  Integer*2 tsProcedureActual%, tmpEntity%
  Integer*4 tmpMonto%
!
! tpv application interface data
!
  STRING EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,             \
    EP.ECR.NBR$, EP.TAX.EFT$, EP.VALOR.EFT$, EP.ECR.TRANSNUM$, EP.APPROV.CODE$, \
    EP.PROPINA$, EP.USER.DATA$, EP.AUTH.NUMBER$, EP.APPROV.DESC$,                \
    EP.CARD.NUMBER$, EP.TIPO.VAR$ 
!
  Integer*1 tmpIndex%, tmpAnulacion%, negTrx%, tmpValidate%
  String tmpBinAnular$
! Transmision control data
!
  !
  !Call printDebug("INIT.EFT.API(" + EP.APPL$ + ", " + EP.ECR.FUNCTION$ + ",... ")
  !
  negTrx% = 0
  !
  If TS.BALDUE(0) < 0 Then Begin
  	negTrx% = -1
  	If Val(ts.io.data$(7)) <> 0 Then \
  		ts.io.data$(7) = "-" + ts.io.data$(7) \
  	Else \
  		ts.io.data$(7) = Str$(TS.BALDUE(0))
  	EP.ECR.FUNCTION$ = "10"
  Endif
  !
  If EP.ECR.FUNCTION$ = "10" Then \
  	tmpAnulacion% = -1 \
  Else \
  	tmpAnulacion% = 0
  !
  Call TEFII.FORMAT.RESETFIELDS
  !
  IF EP.TRX.PEND$ <> "" THEN \
  BEGIN
    IF VAL(EP.VALOR.EFT$) <> VAL(EP.VALOR.EPAY$) THEN \
    BEGIN
      Call printDebug(EP.VALOR.EFT$ + " <> " + EP.VALOR.EPAY$)
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "M"
    ENDIF 
    EP.TIPO.VAR$ = EP.H.TIPO.VAR$
  ENDIF
!  
  EP.AMJ.STATUS$  = "2"
  EP.TRX.STATUS$  = "0"
  EP.APPL.STATUS$ = "00"
  EP.VALOR.EPAY$  = EP.VALOR.EFT$  
  EP.EFT.RECUP.VAL% = INT%(VAL(EP.VALOR.EFT$))
  EP.TAX.EPAY$    = EP.TAX.EFT$
  EP.PROP.EPAY$   = EP.PROPINA$
  EP.IVA.BASE$    = RIGHT$(STRING$(12,"0") + LEFT$(EP.USER.DATA$,12),12)
  EP.CASHBACK$    = RIGHT$(STRING$(12,"0") + MID$(EP.USER.DATA$,13,12),12)
  EP.FILLER4$     = STRING$(12,"0")
  EP.FILLER8$     = STRING$(12,"0")
  EP.FILLER11$    = STRING$(12,"0")
  EP.FILLER12$    = STRING$(12,"0")
  EP.CAJERO$      = RIGHT$(STRING$(12,"0") + UNPACK$(TS.OPER$),12)
!
!  IF EP.TRX.PEND$ <> "" THEN \
!    EXIT SUB
! 
  EP.FECHA.INIC$  = DATE$ + TIME$
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
  EP.RECIBO$      = ""
  EP.PRODUCTO$    = ""
  EP.USER.DATA$   = ""
  EP.GOOD.END%    = 0
  EP.STATE%       = 1
  EP.EXCEPTION$   = ""
  EP.MSR.DATA$    = ""
  EP.KEYB.DATA$   = ""
  EP.ANUL.TRANSNUM$ = STRING$(6,"0")
  EP.ANUL.AUTH$     = STRING$(6,"0")
  EP.ANUL.CODPRO$   = STRING$(6,"0")
  EP.ANUL.RRN$      = STRING$(12,"0")
  EP.ANUL.CUOTAS$   = STRING$(2,"0")
  EP.ANUL.POSTEO$   = STRING$(4,"0")
  EP.ANUL.CARD$     = STRING$(4,"0")
!
	If EP.FORCE.TRXTYPE$ <> "" Then Begin
		EP.KEYB.DATA$ = EP.FORCE.TRXTYPE$
		EP.FORCE.TRXTYPE$ = ""
	Endif Else If tmpAnulacion% Then Begin
		EP.KEYB.DATA$ = "1"
	Endif Else If Not EP.checkTrxActive% Then Begin
		EP.KEYB.DATA$ = "1"
	EndIf Else Begin
	  CALL EP.SAVE.KEYS
	  CALL EP.GET.KBDATA(EP.PROMPT$,"1" , "3", \
	    EP.KEYB.DATA$)
	  IF TS.IO.MOTORKEY = 73 THEN \
	  BEGIN
	    EP.AMJ.STATUS$ = "2"
	    EP.TRX.STATUS$ = "K"
	    CALL EP.RESTORE.KEYS
	    EXIT SUB
	  ENDIF
	  !EP.EFT.TRXTYPE$ = EP.KEYB.DATA$
	  !
	  CALL EP.RESTORE.KEYS
	EndIf
	EP.EFT.TRXTYPE$ = EP.KEYB.DATA$
	!
	If EP.KEYB.DATA$ = "1" Then \
		EP.PREPRODUCTO$ = "DB        " \
	Else \
		EP.PREPRODUCTO$ = "CR        "
	!
  IF EP.ECR.FUNCTION$ = "09" THEN \
  BEGIN
    IF EP.KEYB.DATA$ = "1" OR \
       EP.KEYB.DATA$ = "2"  THEN \
    Begin
    	If TS.PROCEDURE = 1 Then \
    		EP.Function$   = "51"    \
    	Else \
      	EP.Function$   = "11"
    Endif ELSE \
      EP.FUNCTION$   = "21"
  ENDIF
!
  IF EP.ECR.FUNCTION$ = "10" THEN \  
  BEGIN
    IF EP.KEYB.DATA$ = "1" OR \
       EP.KEYB.DATA$ = "2" THEN \
    BEGIN 
    	If TS.PROCEDURE = 1 Then \
    		EP.Function$ = "52" \
      Else IF TS.PROCEDURE < 1 THEN \
      BEGIN 
        EP.FUNCTION$ = "12" 
      ENDIF \
      ELSE \
      BEGIN
        EP.FUNCTION$ = "14" 
      ENDIF
    ENDIF \
    ELSE  \
    BEGIN     
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "R"
      EXIT SUB
    ENDIF
  ENDIF
!  IF EP.ECR.FUNCTION$ = "49" THEN \  
!  BEGIN
!     EP.FUNCTION$ = "49"
!  ENDIF
!
  EP.TIPO.TRX% = VAL(EP.KEYB.DATA$)
  EP.TV.FORCE$ = ""
  tmpEntity% = 0
	If EP.FORCE.TENDVAR$ <> "" Then Begin
		EP.TIPO.VAR$ = EP.FORCE.TENDVAR$
		EP.TV.FORCE$ = EP.FORCE.TENDVAR$
		EP.FORCE.TENDVAR$ = ""
	EndIf Else Begin
	  If EP.Function$ = "51" Then Begin
	    EP.TIPO.VAR$ = EP.ESPECIAL.AVANCE$
	    tmpEntity% = EP.EN.TJAV%
	  Endif Else If EP.TIPO.TRX% = 1 Then Begin
	    EP.TIPO.VAR$ = EP.TV.TJDB$
	    tmpEntity% = EP.EN.TJDB%
	  Endif Else If EP.TIPO.TRX% = 2 Then Begin
	    EP.TIPO.VAR$ = EP.TV.TJCR$
	    tmpEntity% = EP.EN.TJCR%
	  Endif Else If EP.TIPO.TRX% = 3 Then Begin
        EP.TIPO.VAR$ = EP.TV.CHEQ$
        tmpEntity% = EP.EN.CHEQ%
      Endif
	EndIf
	SL.TE.TENDTYPE  = VAL(LEFT$(EP.TIPO.VAR$,1))
    SL.TE.TENDVAR   = VAL(MID$(EP.TIPO.VAR$,2,1))
    !
    EP.DEFAULT.TV$ = EP.TIPO.VAR$
    EP.DEFAULT.ENT% = tmpEntity%
	
!	
  If Not tmpAnulacion% Then \
  	CALL EP.BUSCA.LIMITE.TV(EP.TIPO.VAR$)

  If EP.Function$ = "12" Or EP.Function$ = "14" Then \
  Begin
  	If EP.ECR.TRANSNUM2$ = "" Then \
  	Begin
			tsProcedureActual% = ts.procedure
			ts.procedure = 26
			Call EP.SAVE.KEYS
			EP.KEYB.DATA$ = ""
			Call EP.GET.KBDATA(EP.GUIDE.MESSAGE$(23),"1" , "999999", \
			EP.KEYB.DATA$)
			If TS.IO.MOTORKEY = 73 Then \
			Begin
				EP.AMJ.STATUS$ = "2"
				EP.TRX.STATUS$ = "K"
				Call EP.RESTORE.KEYS
				ts.procedure = tsProcedureActual%
				EXIT Sub
			EndIf
			Call EP.RESTORE.KEYS
			EP.numeroReciboAnular$ = Right$(String$(6,"0") + EP.KEYB.DATA$ ,6)
			ts.procedure = tsProcedureActual%
  	Endif \
  	Else \
	  	EP.numeroReciboAnular$ = Right$(String$(6,"0") + EP.ECR.TRANSNUM2$ ,6)
  Endif
  !
  If EP.Function$ = "12" Then \
  Begin
    !If Len(EP.ECR.TRANSNUM$) > 0 Then \
    !Begin
    !  If VAL(EP.ECR.TRANSNUM$) = 0 Then \     ! No viene trx para anular
    !  Begin
    !    EP.AMJ.STATUS$ = "2"
    !    EP.TRX.STATUS$ = "N"
    !    EXIT Sub
    !  EndIf    
    !EndIf
!
    EP.DE1102.FOUND% = 0
    EP.APPROV.FOUND% = 0
    EP.DE1103.FOUND% = 0
    EP.DE1203.FOUND% = 0
    !
    tmpIndex% = searchTefArray(EP.numeroReciboAnular$)
    If tmpIndex% > 0 Then \
    Begin
    	EP.DE1102.FOUND% = -1
		EP.DE1103.FOUND% = -1
    	If tefTrxArray$(tmpIndex%,0) = "0" Then \
    	Begin
			EP.DE.ISOCOD$ = tefTrxArray$(tmpIndex%, 2)
			EP.DE.POSTEO$ = tefTrxArray$(tmpIndex%, 3)
			EP.DE.CODPRO$ = tefTrxArray$(tmpIndex%, 4)
			EP.DE.CARD$ = tefTrxArray$(tmpIndex%, 5)
			EP.DE.AUTH$ = tefTrxArray$(tmpIndex%, 6)
			EP.DE.AMOUNT$ = tefTrxArray$(tmpIndex%, 7)
			EP.DE.IVA$ = tefTrxArray$(tmpIndex%, 8)
			EP.DE.RRN$ = tefTrxArray$(tmpIndex%, 9)
			EP.DE.CUOTAS$ = tefTrxArray$(tmpIndex%,10)
			EP.DE.TIPOVAR$ = tefTrxArray$(tmpIndex%,11)
			EP.DE.IVA.BASE$ = tefTrxArray$(tmpIndex%,12)
			EP.DE.CASHBACK$ = tefTrxArray$(tmpIndex%,13)
			EP.PREPRODUCTO$ = tefTrxArray$(tmpIndex%,14)
			If Left$(EP.PREPRODUCTO$, 2) = "DB" Then \
				EP.KEYB.DATA$ = "1" \
			Else \
				EP.KEYB.DATA$ = "2"
			EP.TIPO.TRX% = VAL(EP.KEYB.DATA$)
			!
			EP.APPROV.FOUND% = isApprovCode(EP.DE.ISOCOD$)
		Endif Else Begin
			EP.DE1203.FOUND% = -1
			EP.APPROV.FOUND% = -1
		Endif
    Endif
!
    If EP.DE1102.FOUND% Then Begin           ! Encontró transacción en arreglo de transacciones aprobadas
	    If NOT EP.APPROV.FOUND% Then \
	    Begin
	      EP.AMJ.STATUS$ = "2"
	      EP.TRX.STATUS$ = "T"
	      EXIT Sub
	    EndIf    
	    If NOT EP.DE1103.FOUND% Then \         ! no se termino transaccion voucher
	    Begin
	      EP.AMJ.STATUS$ = "2"
	      EP.TRX.STATUS$ = "U"
	      EXIT Sub
	    EndIf
	    If EP.DE1203.FOUND% Then \         ! Ya existe anulacion anterior
	    Begin
	      EP.AMJ.STATUS$ = "2"
	      EP.TRX.STATUS$ = "L"
	      EXIT Sub
	    EndIf    
	    If VAL(EP.VALOR.EPAY$) <> VAL(EP.DE.AMOUNT$) Then \ ! No coincide valor de transaccion
	    Begin
	      Call printDebug(EP.VALOR.EPAY$ + " vs " + EP.DE.AMOUNT$)
	      EP.AMJ.STATUS$ = "2"
	      EP.TRX.STATUS$ = "M"
	      EXIT Sub
	    EndIf   
	    !
		EP.ECR.TRANSNUM$ = EP.numeroReciboAnular$
		EP.ECR.TRANSNUM2$ = EP.numeroReciboAnular$
	    !
	    EP.ANUL.TRANSNUM$ = EP.ECR.TRANSNUM$
	    EP.ANUL.POSTEO$   = EP.DE.POSTEO$
	    EP.ANUL.CODPRO$   = EP.DE.CODPRO$
	    EP.ANUL.CARD$     = EP.DE.CARD$
	    EP.ANUL.AUTH$     = EP.DE.AUTH$
	    EP.ANUL.RRN$      = EP.DE.RRN$
	    EP.ANUL.CUOTAS$   = EP.DE.CUOTAS$ 
	    EP.TAX.EPAY$      = Right$(String$(12,"0") + EP.DE.IVA$, 12)
	    EP.IVA.BASE$      = Right$(String$(12,"0") + EP.DE.IVA.BASE$,12)
	    EP.PROP.EPAY$     = Right$(String$(12,"0") + EP.DE.PROPINA$,12)
	    EP.CASHBACK$      = Right$(String$(12,"0") + EP.DE.CASHBACK$,12)
	    EP.CAJERO$        = Right$(String$(12,"0") + UNPACK$(TS.OPER$),12)
	    EP.TIPO.VAR$      = EP.DE.TIPOVAR$
	Endif Else Begin
		! No encontró transacción en arreglo. La busca en archivo de log para comparar el monto
		Call getMontoReciboAnterior(EP.numeroReciboAnular$, tmpMonto%, tmpBinAnular$, EP.CAJERO$)
		If tmpMonto% = -1 Then Begin
			EP.AMJ.STATUS$ = "2"
			EP.TRX.STATUS$ = "O"
			Exit Sub
		Endif Else If tmpMonto% <> Val(EP.VALOR.EPAY$) Then Begin
			Call printDebug(Str$(tmpMonto%) + " <> " + EP.VALOR.EPAY$)
			EP.AMJ.STATUS$ = "2"
			EP.TRX.STATUS$ = "M"
			Exit Sub
		Endif Else If negTrx% Then Begin
			tmpValidate% = tef.notifyAccount(tmpBinAnular$)
			If tmpValidate% = 0 Then Begin
				EP.AMJ.STATUS$ = "2"
				EP.TRX.STATUS$ = "x"
				Exit Sub
			Endif Else If tmpValidate% <> 1 Then Begin
				EP.AMJ.STATUS$ = "2"
				EP.TRX.STATUS$ = "K"
				Exit Sub
			Endif
		Endif
		!
		EP.TIPO.VAR$ = EP.TV.TJDB$
		EP.KEYB.DATA$ = "1"
		EP.PREPRODUCTO$ = "DB        "
	Endif
  EndIf                                  ! fin de transaccion de anulacion dentro de misma transaccion 

  IF EP.FUNCTION$ = "14" Or EP.Function$ = "52" THEN \          ! anulacion de transaccion de compra anterior 
  BEGIN
    If Len(EP.numeroReciboAnular$) > 0 Then \
    Begin
      If VAL(EP.numeroReciboAnular$) = 0 Then \     ! No viene trx para anular
      Begin
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "N"
        EXIT Sub
      EndIf    
    EndIf
    EP.DE1102.FOUND% = 0
    EP.DE1103.FOUND% = 0
    EP.DE1203.FOUND% = 0
    EP.APPROV.FOUND% = 0
    EP.EFT.FOUND% = 0
    !
	Call getMontoReciboAnterior(EP.numeroReciboAnular$, tmpMonto%, tmpBinAnular$, EP.CAJERO$)
	If tmpMonto% = -1 Then Begin
		EP.AMJ.STATUS$ = "2"
		EP.TRX.STATUS$ = "O"
		Exit Sub
	Endif Else If tmpMonto% <> Val(EP.VALOR.EPAY$) Then Begin
		Call printDebug(Str$(tmpMonto%) + " <> " + EP.VALOR.EPAY$)
		EP.AMJ.STATUS$ = "2"
		EP.TRX.STATUS$ = "M"
		Exit Sub
	Endif
	!
	EP.TIPO.VAR$ = EP.TV.TJDB$
	EP.KEYB.DATA$ = "1"
	EP.PREPRODUCTO$ = "DB        "
    !
    !TS.ER.RETURN = -1
    !OPEN "R::EFTTRX" KEYED RECL 158 AS EP.IOPARM% NOWRITE NODEL
    !IF NOT TS.ER.RETURN  THEN \   ! si no abre
    !BEGIN 
    !  EP.AMJ.STATUS$ = "2"
    !  EP.TRX.STATUS$ = "V"
    !  EXIT SUB
    !ENDIF
    !If EP.Function$ = "14" Then \
	!    EP.KEY$ = Right$(String$(4,"0")+TS.TERMINAL$,4) + \
	!       EP.APPL$ + "11"+ EP.ECR.TRANSNUM$ + "02" \
	!  Else \
	!    EP.KEY$ = Right$(String$(4,"0")+TS.TERMINAL$,4) + \
	!       EP.APPL$ + "51"+ EP.ECR.TRANSNUM$ + "02"
	!
    !TS.ER.RETURN = -1 
    !READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    !  KEY EP.KEY$ ;                                     \
    !  EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,  \
    !  EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$,         \
    !  EP.A$, EP.A$
    !
    !If NOT TS.ER.RETURN Then \  ! si no existe
    !Begin
    !  ! La busca como avance en caso de que se haya
    !  ! buscado como compra
    !  If EP.Function$ = "14" Then \
    !  Begin
    !  	EP.Function$ = "52"
  	!    EP.KEY$ = Right$(String$(4,"0")+TS.TERMINAL$,4) + \
	!       EP.APPL$ + "51"+ EP.ECR.TRANSNUM$ + "02"
	!    TS.ER.RETURN = -1 
	!    READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
	!      KEY EP.KEY$ ;                                     \
	!      EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,  \
	!      EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$,         \
	!      EP.A$, EP.A$      	
    !  Endif
    !Endif
    !
    !IF NOT TS.ER.RETURN THEN \  ! si no existe
    !BEGIN 
    !  EP.AMJ.STATUS$ = "2"
    !  EP.TRX.STATUS$ = "O"
    !  CLOSE EP.IOPARM%
    !  EXIT SUB
    !ENDIF \
    !ELSE \
    !BEGIN
    !  EP.ECR.TRANSNUM$ = EP.numeroReciboAnular$
    !  EP.ECR.TRANSNUM2$ = EP.numeroReciboAnular$
    !  !
    !  EP.DE1102.FOUND% = -1
    !  EP.DE.ISOCOD$  = MID$(EP.DE2.DATA$,19,2)
    !  EP.DE.POSTEO$  = LEFT$(EP.DE3.DATA$,4)
    !  EP.DE.CODPRO$  = MID$(EP.DE3.DATA$,5,6)
    !  EP.DE.CARD$    = MID$(EP.DE3.DATA$,11,4)
    !  EP.DE.AUTH$    = MID$(EP.DE3.DATA$,15,6)
    !  EP.DE.AMOUNT$  = LEFT$(EP.DE4.DATA$,10)
    !  EP.DE.IVA$     = MID$(EP.DE4.DATA$,11,10)
    !  EP.DE.PROPINA$ = "00" + MID$(EP.DE5.DATA$,13,8)
    !  EP.DE.RRN$     = LEFT$(EP.DE6.DATA$,12)
    !  EP.DE.CUOTAS$  = MID$(EP.DE6.DATA$,13,2)  
    !  EP.DE.TIPOVAR$ = MID$(EP.DE6.DATA$,15,2)
    !  !
    !  If isApprovCode(EP.DE.ISOCOD$) Then Begin
    !    EP.APPROV.FOUND% = -1
    !  Endif Else Begin
    !    EP.AMJ.STATUS$ = "2"
    !    EP.TRX.STATUS$ = "T" 
    !    CLOSE EP.IOPARM%
    !    EXIT SUB
    !  Endif
    !ENDIF
    !
    !If EP.Function$ = "14" Then \
	!    EP.KEY$ = Right$(String$(4,"0")+TS.TERMINAL$,4) + \
	!       EP.APPL$ + "11"+ EP.ECR.TRANSNUM$ + "03" \
	!  Else \
	!    EP.KEY$ = Right$(String$(4,"0")+TS.TERMINAL$,4) + \
	!       EP.APPL$ + "51"+ EP.ECR.TRANSNUM$ + "03"
	!
    !TS.ER.RETURN = -1 
    !READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    !  KEY EP.KEY$ ;                                     \
    !  EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,  \
    !  EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$,         \
    !  EP.A$, EP.A$  
    !IF NOT TS.ER.RETURN THEN \  ! si no existe
    !BEGIN 
    !  EP.AMJ.STATUS$ = "2"
    !  EP.TRX.STATUS$ = "U" 
    !  CLOSE EP.IOPARM%
    !  EXIT SUB
    !ENDIF
    !EP.DE.IVA.BASE$ = LEFT$(EP.DE2.DATA$,10)
    !EP.DE.CASHBACK$ = MID$(EP.DE2.DATA$,11,10)
    !       
    !EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
    !   EP.APPL$ + "10"+ EP.ECR.TRANSNUM$ + "03"
    !
    !TS.ER.RETURN = -1 
    !READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
    !  KEY EP.KEY$ ;                                     \
    !  EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,  \
    !  EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$,         \
    !  EP.A$, EP.A$
    !
    !IF TS.ER.RETURN THEN \           ! si existe
    !BEGIN 
    !  EP.AMJ.STATUS$ = "2"
    !  EP.TRX.STATUS$ = "L"
    !  CLOSE EP.IOPARM%
    !  EXIT SUB
    !ENDIF
    !CLOSE EP.IOPARM%
    !    
    !IF VAL(EP.VALOR.EPAY$) <> VAL(EP.DE.AMOUNT$) THEN \ ! No coincide valor de transaccion
    !BEGIN
    !  EP.AMJ.STATUS$ = "2"
    !  EP.TRX.STATUS$ = "M"
    !  EXIT SUB
    !ENDIF   
    !EP.ANUL.TRANSNUM$ = EP.ECR.TRANSNUM$
    !EP.ANUL.POSTEO$   = EP.DE.POSTEO$
    !EP.ANUL.CODPRO$   = EP.DE.CODPRO$
    !EP.ANUL.CARD$     = EP.DE.CARD$
    !EP.ANUL.AUTH$     = EP.DE.AUTH$
    !EP.ANUL.RRN$      = EP.DE.RRN$
    !EP.ANUL.CUOTAS$   = EP.DE.CUOTAS$ 
    !EP.TAX.EPAY$      = RIGHT$(STRING$(12,"0") + EP.DE.IVA$,12)
    !EP.IVA.BASE$      = RIGHT$(STRING$(12,"0") + EP.DE.IVA.BASE$,12)
    !EP.PROP.EPAY$     = RIGHT$(STRING$(12,"0") + EP.DE.PROPINA$,12)
    !EP.CASHBACK$      = RIGHT$(STRING$(12,"0") + EP.DE.CASHBACK$,12)
    !EP.CAJERO$        = RIGHT$(STRING$(12,"0") + UNPACK$(TS.OPER$),12)
    !EP.TIPO.VAR$      = EP.DE.TIPOVAR$
    !    
  ENDIF                   ! fin de transaccion de anulacion de transaccion anterior 
!     
  !
  If tmpAnulacion% Then Begin
  	Call EP.BUSCA.LIMITE.TV(EP.TIPO.VAR$)
  Endif
  !
  IF EP.FUNCTION$ = "11"  OR  \
     EP.Function$ = "51"  OR  \ ! avance
     EP.FUNCTION$ = "21" THEN  \   ! trasaccion de compra normal
  BEGIN
    EP.ANUL.TRANSNUM$ = STRING$(6,"0")
    EP.ANUL.POSTEO$   = STRING$(4,"0")
    EP.ANUL.CODPRO$   = STRING$(6,"0")
    EP.ANUL.CARD$     = STRING$(4,"0")
    EP.ANUL.AUTH$     = STRING$(6,"0")
    EP.ANUL.CUOTAS$   = STRING$(2,"0")
    EP.ANUL.RRN$      = STRING$(12,"0") 
!     
    IF VAL(EP.VALOR.EPAY$) > EP.MAX.TV% THEN \
    BEGIN
      EP.AMJ.STATUS$ = "2"
      EP.TRX.STATUS$ = "P"
      EXIT SUB
    ENDIF   
!      
    IF TS.PROCEDURE < 1 THEN BEGIN
      IF (VAL(EP.VALOR.EPAY$) - Abs(TS.BALDUE(0))) > EP.CHG.TV% THEN \
      BEGIN
        EP.AMJ.STATUS$ = "2"
        EP.TRX.STATUS$ = "Q"
        EXIT SUB
      ENDIF
    ENDIF  
  ENDIF
END SUB
!
Function autoAnul(pAppl$, pAmount%, pDoc$)
	Integer*1 autoAnul, tmpExit%
	Integer*4 pAmount%
	String pAppl$, pDoc$, anulMsg$, anulLen$, anulResponse$, tmpStatus$, trxStatus$, applStatus$, responseCode$, tmpTime$
	!
	tmpTime$ = DATE$ + TIME$
	anulMsg$ = Right$(String$(6,"0")+ TS.TERMINAL$,6) + \
		Right$(String$(6,"0")+Str$(SL.HD.TRANSNUM+1),6) + tmpTime$ + \
		"00" + Right$(String$(12, "0") + Str$(pAmount%), 12) + Right$(String$(6, "0") + pDoc$, 6) + EP.CAJERO$ + EP.ACTUALCLAVEANUL$ + STRING$(8," ") + \
		EP.FILLER8$
	anulLen$ = Right$(String$(3,"0") + Str$(Len(anulMsg$)),3)
	anulMsg$ = pAppl$ + "12" + anulLen$ + anulMsg$
	!
	tmpExit% = 0
	anulResponse$ = TEFII.FUNCT.APPLKERNEL$(anulMsg$)
	While Not tmpExit%
		If Len(anulResponse$) < 8 Then Begin
			tmpExit% = 1
		Endif Else Begin
			tmpStatus$ = Mid$(anulResponse$, 8, 1)
			If tmpStatus$ = "0" Or tmpStatus$ = "1" Then Begin
				tmpExit% = 1
			Endif Else Begin
				trxStatus$ = Mid$(anulResponse$, 9, 1)
				applStatus$ = Mid$(anulResponse$, 10, 2)
				If tmpStatus$ = "2" Then Begin
					responseCode$ = Mid$(anulResponse$, 18, 2)
					If isApprovCode(responseCode$) Then \
						tmpExit% = -1 \
					Else \
						tmpExit% = 1
				Endif Else If tmpStatus$ = "3" Then Begin
					Call EP.DISPLAY.A.MESSAGE(Mid$(anulResponse$, 12, 40))
				Endif
			Endif
		Endif
		If Not tmpExit% Then Begin
			If applStatus$ = "99" Then Begin
				tmpExit% = 1
			Endif Else Begin
				anulMsg$ = Right$(String$(6,"0")+ TS.TERMINAL$,6) + \
					Right$(String$(6,"0") + Str$(SL.HD.TRANSNUM+1),6) + tmpTime$ + \
					applStatus$
				anulLen$ = Right$(String$(3,"0") + Str$(Len(anulMsg$)),3)
				anulMsg$ = pAppl$ + "12" + anulLen$ + anulMsg$
				anulResponse$ = TEFII.FUNCT.APPLKERNEL$(anulMsg$)
			Endif
		Endif
	Wend
	!
	autoAnul = tmpExit%
End Function
!
SUB END.EFT.API(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$, \
  EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.PROPINA$,  \
  EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$,       \
  EP.TIPO.VAR$, EP.USER.DATA$) PUBLIC   
!
! tpv application interface data
!
  STRING EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,             \
    EP.ECR.NBR$, EP.TAX.EFT$, EP.VALOR.EFT$, EP.ECR.TRANSNUM$, EP.APPROV.CODE$, \
    EP.PROPINA$, EP.USER.DATA$, EP.AUTH.NUMBER$, EP.APPROV.DESC$,                \
    EP.CARD.NUMBER$, EP.TIPO.VAR$
!
  Integer*1 tmpIndex%,tipoPagoOriginal%,tipoPagoNuevo%,needForVoid%,tmpResponse%,voidTries%
  Integer*4 valorOriginal%
  !
  needForVoid% = 0
  !
  EP.EPAY.TRANSNUM$ = EP.ECR.TRANSNUM$ 
  EP.ECR.DATETIME$  = DATE$ + TIME$
  EP.USER.DATA$     = ""
  tmpAnulacion% = 0
  valorOriginal% = Val(EP.VALOR.EPAY$)
  tipoPagoOriginal% = Val(Left$(EP.TIPO.VAR$,1))
!
  If (EP.Function$ = "11" or EP.Function$ = "51") AND EP.ECR.Function$ NE "49" Then \        ! Transaccion tarjeta
  BEGIN
    EP.USER.DATA$ = ""
    EP.MESSAGE$ = RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.ECR.TRANSNUM$ + EP.ECR.DATETIME$ + \
      EP.APPL.STATUS$ + EP.VALOR.EPAY$ + EP.TAX.EPAY$ + EP.IVA.BASE$ + EP.CAJERO$ + EP.PROP.EPAY$ +\
      EP.CASHBACK$ + EP.FILLER4$
    EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
    EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
    CALL EP.SEND.MSG.TO.AMJ
  ENDIF
  IF (EP.FUNCTION$ = "12"  OR  \
     EP.FUNCTION$ = "14" or EP.Function$ = "52") AND EP.ECR.Function$ NE "49" THEN \        ! Transaccion tarjeta
  BEGIN
    tmpAnulacion% = -1
    EP.MESSAGE$ = RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + \
      RIGHT$(STRING$(6,"0")+STR$(SL.HD.TRANSNUM+1),6) + EP.ECR.DATETIME$ + \
      EP.APPL.STATUS$ + EP.VALOR.EPAY$ + EP.ECR.TRANSNUM$ + EP.CAJERO$ + EP.ACTUALCLAVEANUL$ + STRING$(8," ") + \
      EP.FILLER8$
    EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
    EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
    CALL EP.SEND.MSG.TO.AMJ
  ENDIF
!
  IF EP.FUNCTION$ = "21" AND EP.ECR.Function$ NE "49" THEN \   ! Transaccion cheque 
  BEGIN
    EP.MESSAGE$ = RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + EP.EPAY.TRANSNUM$ + EP.ECR.DATETIME$ + \
      EP.APPL.STATUS$ + EP.VALOR.EPAY$ + EP.CAJERO$ + EP.FILLER11$ + EP.FILLER12$  
    EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
    EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
    CALL EP.SEND.MSG.TO.AMJ
  ENDIF
!
  IF EP.ECR.FUNCTION$ = "49" THEN \   ! Transaccion cheque 
  BEGIN
    EP.FUNCTION$ = EP.LAST.FUNCTION$
    EP.APPL.STATUS$ = "00"
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
    EP.RECIBO$      = ""
    EP.PRODUCTO$    = ""
    EP.USER.DATA$   = ""
    EP.GOOD.END%    = 0
    EP.STATE%       = 1
    EP.EXCEPTION$   = ""
    EP.MSR.DATA$    = ""
    EP.KEYB.DATA$   = ""
!
!    call ep.line.print("apst="+ep.appl.status$+" vr="+ep.valor.epay$+" tx="+ep.tax.epay$,4100H)
!    call ep.line.print("base="+ep.iva.base$+" cj="+ep.cajero$,4100H)
!    call ep.line.print("pro="+ep.prop.epay$+" cashb="+ep.cashback$,4100H)
!    call ep.line.print("fi4="+ep.filler4$+" fi11="+ep.filler11$,4100H)
!    call ep.line.print("fi12="+ep.filler12$+" lf="+ep.last.function$+" lr="+ep.last.recibo$,4100H)
    EP.MESSAGE$ = RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) +                                      \
      RIGHT$(STRING$(6,"0")+STR$(SL.HD.TRANSNUM+1),6) + EP.ECR.DATETIME$ +                      \
      EP.APPL.STATUS$ + EP.VALOR.EPAY$ + EP.TAX.EPAY$ + EP.IVA.BASE$ + EP.CAJERO$ + EP.PROP.EPAY$ +\
      EP.CASHBACK$ + EP.FILLER4$ + EP.FILLER11$ + EP.FILLER12$ +                                \
      EP.LAST.FUNCTION$
    IF EP.LAST.Function$ = "12" OR EP.LAST.Function$ = "14" THEN \
    	EP.MESSAGE$ = EP.MESSAGE$ + EP.numeroReciboAnular$ \
    ELSE \
    	EP.MESSAGE$ = EP.MESSAGE$ + EP.LAST.RECIBO$ 
    EP.MSGLEN$ = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
    EP.MESSAGE$ = EP.APPL$ + EP.ECR.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
    CALL EP.SEND.MSG.TO.AMJ
  ENDIF
!
  EP.INICIO.FOUND%  = 0
  EP.RESP.FOUND%    = 0
  EP.APPROV.FOUND%  = 0
  EP.VOUCHER.FOUND% = 0
  EP.TOHOST.FOUND%  = 0
  EP.APPROV.CODE$   = "99"
!  
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
    IF EP.AMJ.STATUS$ = "2" THEN \        ! Transaccion con respuesta de host concluida
    BEGIN
      IF EP.M.LEN% < 9 THEN \
      BEGIN
        EP.TRX.STATUS$ = "1" 
        EXIT SUB
      ENDIF
      EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
      EP.RESP.FOUND% = -1
      IF EP.FUNCTION$ = "21" OR  \
         EP.FUNCTION$ = "22" OR  \
         EP.FUNCTION$ = "23" THEN \
      BEGIN
        CALL EP.PARSE.CHEQUE.RESPONSE.DF
        IF EP.TRX.STATUS$ <> "0" THEN \
        BEGIN
          EP.STATE% = 4
          EXIT SUB        
        ENDIF
        EP.AUTH.NUMBER$ = EP.H.AUTH.NUMBER$
        EP.APPROV.CODE$ = EP.H.APPROV.CODE$
        EP.TIPO.ID$     = "0"
        EP.RECIBO$      = LEFT$(EP.RECIBO$,6)
        EP.LAST.RECIBO$ = EP.RECIBO$
!        
        TS.USER6$ = EP.NRO.CHEQUE$
        TS.USER7$ = EP.TEL.GIRADOR$
!
        IF EP.ECR.FUNCTION$ = "49" THEN  \
        BEGIN 
          TS.ER.RETURN = -1
          OPEN "R::EFTTRX" KEYED RECL 158 AS EP.IOPARM% NOWRITE NODEL
          IF NOT TS.ER.RETURN  THEN \   ! si no abre
          BEGIN 
            EXIT SUB
          ENDIF
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
          EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) +   \ ! venta anterior
            EP.APPL$ + "11" + EP.ECR.TRANSNUM$ + "02"
          ! "8511"+ EP.ECR.TRANSNUM$ + "02"
          !------------------------------------------------------------------------
          EP.SAVE.KEY$ = EP.KEY$
          TS.ER.RETURN = -1 
          READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
            KEY EP.KEY$ ;                                         \
            EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
            EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
          IF TS.ER.RETURN THEN \  ! si existe
          BEGIN
            EP.TRX.PEND$ = ""
            EP.TRX.PEND% = 0
            CLOSE EP.IOPARM%
            EXIT SUB
          ENDIF
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
          EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) +   \
            EP.APPL$ + "12" + EP.ECR.TRANSNUM$ + "02"
          ! "8512"+ EP.ECR.TRANSNUM$ + "02"
          !------------------------------------------------------------------------
          EP.SAVE.KEY$ = EP.KEY$
          TS.ER.RETURN = -1 
          READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
            KEY EP.KEY$ ;                                         \
            EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
            EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
          IF TS.ER.RETURN THEN \  ! si existe
          BEGIN  
            EP.TRX.PEND$ = ""
            EP.TRX.PEND% = 0
            CLOSE EP.IOPARM%
            EXIT SUB
          ENDIF
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
          EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) +      \
            EP.APPL$ + "14" + EP.ECR.TRANSNUM$ + "02"
          ! "8514"+ EP.ECR.TRANSNUM$ + "02"
          !------------------------------------------------------------------------
          EP.SAVE.KEY$ = EP.KEY$
          TS.ER.RETURN = -1 
          READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
            KEY EP.KEY$ ;                                         \
            EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
            EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
          IF TS.ER.RETURN THEN \  ! si existe
          BEGIN  
            EP.TRX.PEND$ = ""
            EP.TRX.PEND% = 0
            CLOSE EP.IOPARM%
            EXIT SUB
          ENDIF
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
          EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
             EP.APPL$ + "21" + EP.ECR.TRANSNUM$ + "02" 
          !  "8521"+ EP.ECR.TRANSNUM$ + "02" 
          !------------------------------------------------------------------------
          EP.SAVE.KEY$ = EP.KEY$
          TS.ER.RETURN = -1 
          READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
            KEY EP.KEY$ ;                                         \
            EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
            EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
          IF TS.ER.RETURN THEN \  ! si existe
          BEGIN
            EP.TRX.PEND$ = ""
            EP.TRX.PEND% = 0
            CLOSE EP.IOPARM%
            EXIT SUB
          ENDIF
          CLOSE EP.IOPARM%
        ENDIF  
!           
        If EP.TV.POS% <> TS.TDR.INDEX Then \
        Begin
          If EP.Function$ = "11" OR \
             EP.Function$  ="51" OR \
             EP.Function$ = "21" Then \
          Begin   
            TS.NUMTNDRS(EP.TV.POS%) = TS.NUMTNDRS(EP.TV.POS%) + 1 
            TS.TEndVAMT(EP.TV.POS%) = TS.TEndVAMT(EP.TV.POS%) + VAL(EP.VALOR.EPAY$)
            TS.NUMTNDRS(TS.TDR.INDEX) = TS.NUMTNDRS(TS.TDR.INDEX) - 1 
            TS.TEndVAMT(TS.TDR.INDEX) = TS.TEndVAMT(TS.TDR.INDEX) - VAL(EP.VALOR.EPAY$)
            TS.TDR.INDEX = EP.TV.POS%
          EndIf \
          Else \
          If EP.Function$ = "12" OR \
             EP.Function$ = "52" OR \
             EP.Function$ = "14" Then \
          Begin
            TS.NUMTNDRS(EP.TV.POS%) = TS.NUMTNDRS(EP.TV.POS%) - 1 
            TS.TEndVAMT(EP.TV.POS%) = TS.TEndVAMT(EP.TV.POS%) - VAL(EP.VALOR.EPAY$)
            TS.NUMTNDRS(TS.TDR.INDEX) = TS.NUMTNDRS(TS.TDR.INDEX) + 1 
            TS.TEndVAMT(TS.TDR.INDEX) = TS.TEndVAMT(TS.TDR.INDEX) + VAL(EP.VALOR.EPAY$)
            TS.TDR.INDEX = EP.TV.POS%
          EndIf 
        EndIf

        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.defaultApp$ + EP.FUNCTION$ + EP.RECIBO$ + "02" , \ DE-1
             EP.COMERCIO$ + EP.EPAY.TERMINAL$ + EP.APPROV.CODE$,                             \ DE-2 
             EP.FECHA.POSTEO$ + EP.COD.PROC$ + EP.CENTRAL.CHEQUE$ + EP.TIPO.ID$ + EP.COD.BANCO$ + EP.AUTH.NUMBER$ , \ DE-3
             RIGHT$(EP.VALOR.EPAY$,10) + EP.NRO.CHEQUE$ + "0",                                          \ DE-4
             EP.TEL.GIRADOR$ + EP.CTA.CTE$,                                                  \ DE-5
             EP.RRN$ + "00" + EP.TIPO.VAR$ + EP.TRX.STATUS$ + "000" )                        ! DE-6
        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.defaultApp$ + EP.FUNCTION$  + EP.RECIBO$ + "03", \ DE-1
             EP.FECHA.INIC$,                     \ DE-2
             EP.FECHA.PROC$,                     \ DE-3
             EP.ANUL.TRANSNUM$,                  \ DE-4
             DATE$ + TIME$ + EP.CARD.BIN$,       \ DE-5
             EP.ANUL.RRN$ + EP.FECHA.VENC$)      ! DE-6 
        EP.VOUCHER.FOUND% = -1
!        TS.USERDATA$= EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$ + EP.RECIBO$ + "02" + ":" + \
!        EP.FRANQUICIA$ + ":" + EP.BANCO$ + ":" + EP.PRODUCTO$ + ":" + EP.VALOR.EPAY$ + ":" + EP.TAX.EPAY$
!        TS.TEMP1I1 = 99                  
!        CALL TSTPEC01
      ENDIF \
      ELSE \
      BEGIN 
        CALL EP.PARSE.DBCR.RESPONSE.DF
        IF EP.TRX.STATUS$ <> "0" THEN \
        BEGIN
          EP.STATE% = 4
          EXIT SUB        
        ENDIF                             
        EP.AUTH.NUMBER$ = EP.H.AUTH.NUMBER$
        EP.APPROV.CODE$ = EP.H.APPROV.CODE$
        EP.CARD.NUMBER$ = EP.H.CARD.NUMBER$
        EP.USER.DATA$   = EP.CARD.BIN$
        EP.RECIBO$      = LEFT$(EP.RECIBO$,6)
        EP.LAST.RECIBO$ = EP.RECIBO$
! 
!        CALL EP.DISPLAY.AN.ERROR("BIN="+EP.CARD.BIN$)

        IF EP.ECR.FUNCTION$ = "49" THEN \
        BEGIN 
!          CALL EP.LINE.PRINT("entró a 49",4100H)
          TS.ER.RETURN = -1
          OPEN "R::EFTTRX" KEYED RECL 158 AS EP.IOPARM% NOWRITE NODEL
          IF NOT TS.ER.RETURN  THEN \   ! si no abre
          BEGIN 
            EXIT SUB
          ENDIF
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
          EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) +   \ ! venta anterior
            EP.APPL$ + "11" + EP.ECR.TRANSNUM$ + "02"
          ! "8511"+ EP.ECR.TRANSNUM$ + "02"
          !------------------------------------------------------------------------
          EP.SAVE.KEY$ = EP.KEY$
          TS.ER.RETURN = -1 
          READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
            KEY EP.KEY$ ;                                         \
            EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
            EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
          IF TS.ER.RETURN THEN \  ! si existe
          BEGIN
            EP.TRX.PEND$ = ""
            EP.TRX.PEND% = 0
            CLOSE EP.IOPARM%
            EXIT SUB
          ENDIF
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
          EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) +   \
            EP.APPL$ + "12" + EP.ECR.TRANSNUM$ + "02"
          ! "8512"+ EP.ECR.TRANSNUM$ + "02"
          !------------------------------------------------------------------------
          EP.SAVE.KEY$ = EP.KEY$
          TS.ER.RETURN = -1 
          READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
            KEY EP.KEY$ ;                                         \
            EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
            EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
          IF TS.ER.RETURN THEN \  ! si existe
          BEGIN  
            EP.TRX.PEND$ = ""
            EP.TRX.PEND% = 0
            CLOSE EP.IOPARM%
            EXIT SUB
          ENDIF
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
          EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) +      \
            EP.APPL$ + "14" + EP.ECR.TRANSNUM$ + "02"
          ! "8514"+ EP.ECR.TRANSNUM$ + "02"
          !------------------------------------------------------------------------
          EP.SAVE.KEY$ = EP.KEY$
          TS.ER.RETURN = -1 
          READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
            KEY EP.KEY$ ;                                         \
            EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
            EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
          IF TS.ER.RETURN THEN \  ! si existe
          BEGIN  
            EP.TRX.PEND$ = ""
            EP.TRX.PEND% = 0
            CLOSE EP.IOPARM%
            EXIT SUB
          ENDIF
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
          EP.KEY$ = RIGHT$(STRING$(4,"0")+TS.TERMINAL$,4) + \
             EP.APPL$ + "21" + EP.ECR.TRANSNUM$ + "02" 
          !  "8521"+ EP.ECR.TRANSNUM$ + "02" 
          !------------------------------------------------------------------------
          EP.SAVE.KEY$ = EP.KEY$
          TS.ER.RETURN = -1 
          READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
            KEY EP.KEY$ ;                                         \
            EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
            EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
          IF TS.ER.RETURN THEN \  ! si existe
          BEGIN
            EP.TRX.PEND$ = ""
            EP.TRX.PEND% = 0
            CLOSE EP.IOPARM%
            EXIT SUB
          ENDIF
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
	        EP.KEY$ = Right$(String$(4,"0")+TS.TERMINAL$,4) +      \
	          EP.APPL$ + "51" + EP.ECR.TRANSNUM$ + "02"
	        ! "8551"+ EP.ECR.TRANSNUM$ + "02"
          !------------------------------------------------------------------------
	        EP.SAVE.KEY$ = EP.KEY$
	        TS.ER.RETURN = -1 
	        READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
	          KEY EP.KEY$ ;                                         \
	          EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
	          EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
	        If TS.ER.RETURN Then \  ! si existe
	        Begin 
	          EP.TRX.PEnd$ = ""
	          EP.TRX.PEnd% = 0
	          CLOSE EP.IOPARM%
	          EXIT Sub
	        EndIf
          !
          !------------------------------------------------------------------------
          ! 2016-08-22 jsv
          ! En vez de especificar aplicación 85, se debe especificar el contenido
          ! de la variable EP.APPL$
          !------------------------------------------------------------------------
	        EP.KEY$ = Right$(String$(4,"0")+TS.TERMINAL$,4) +      \
	          EP.APPL$ + "52" + EP.ECR.TRANSNUM$ + "02"
	        ! "8552"+ EP.ECR.TRANSNUM$ + "02"
          !------------------------------------------------------------------------
	        EP.SAVE.KEY$ = EP.KEY$
	        TS.ER.RETURN = -1 
	        READ FORM "C4 C12 5C20 C40 C2"; #EP.IOPARM%             \
	          KEY EP.KEY$ ;                                         \
	          EP.A$, EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$,      \
	          EP.DE4.DATA$, EP.DE5.DATA$, EP.DE6.DATA$, EP.A$, EP.A$
	        If TS.ER.RETURN Then \  ! si existe
	        Begin 
	          EP.TRX.PEnd$ = ""
	          EP.TRX.PEnd% = 0
	          CLOSE EP.IOPARM%
	          EXIT Sub
	        EndIf
          CLOSE EP.IOPARM%
        ENDIF  
!
!           
        If isApprovCode(EP.APPROV.CODE$) Then Begin
	        If EP.TV.FORCE$ <> "" And EP.TV.FORCE$ <> "00" Then Begin
	        	EP.H.TIPO.VAR$ = EP.TV.FORCE$
	        	!EP.TV.FORCE$ = ""
	        	!Call llenarArregloRespuesta("")
	        Endif Else Begin
	        	! Reporta la franquicia y el tipo de producto
	        	! para validar resultados finales
	        	tmpResponse% = tef.notifyResponse(EP.FRANQUICIA$, EP.CARD.BIN$)
        		If tmpResponse% <> 1 Then Begin
        			If tmpResponse% = 0 Then Begin
        				!Se otorgó un descuento que no correspondía
        				!al medio de pago en el cual finalmente se va a
        				!contabilizar la transacción.
        				!Es necesario anular el descuento
        				needForVoid% = -2
        			Endif Else Begin
        				!No se ofreció un descuento al cual se tiene
        				!derecho por el medio de pago ingresado.
        				!Es necesario anular la transacción TEF
        				EP.APPROV.CODE$ = "97"
        				needForVoid% = -1
        				Call clearEntity
        			Endif
        		Endif
		    Endif
		Endif Else Begin
			Call clearEntity
		Endif
		!
		If needForVoid% = -2 Then Begin
			Call EP.DISPLAY.AN.ERROR("Tarj no corresponde Se anula descuento")
			Call disc.voidDiscountExt
		Endif Else If needForVoid% = -1 Then Begin
			Call EP.DISPLAY.AN.ERROR("Hay dcto disponible Anule en datafono")
			needForVoid% = 0
			voidTries% = 0
			While voidTries% < 3 And needForVoid% <> -1
				needForVoid% = autoAnul(EP.APPL$, Int%(Val(EP.VALOR.EPAY$)), EP.RECIBO$)
				voidTries% = voidTries% + 1
			Wend
			If needForVoid% <> -1 Then Begin
				Call EP.DISPLAY.AN.ERROR("Anul auto no dispnblAnule contingencia")
				EP.APPROV.CODE$ = "98"
				needForVoid% = -1
			Endif
		Endif
		!
        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.defaultApp$ + EP.FUNCTION$ + EP.RECIBO$ + "02" , \ DE-1
             EP.COMERCIO$ + EP.EPAY.TERMINAL$ + EP.APPROV.CODE$,                       \ DE-2 
             EP.FECHA.POSTEO$ + EP.COD.PROC$ + EP.CARD.NUMBER$ + EP.AUTH.NUMBER$,      \ DE-3
             RIGHT$(EP.VALOR.EPAY$,10) + RIGHT$(EP.TAX.EPAY$,10),                      \ DE-4
             EP.FECHA.PROC$ + RIGHT$(EP.PROP.EPAY$,8) ,                                  \ DE-5
             EP.RRN$ + EP.CUOTAS.QTY$ + EP.TIPO.VAR$ + "0000")                         ! DE-6
        !
        If isApprovCode(EP.APPROV.CODE$) Then Begin
        	CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.defaultApp$ + EP.FUNCTION$  + EP.RECIBO$ + "03", \ DE-1
	             RIGHT$(EP.IVA.BASE$,10) + RIGHT$(EP.CASHBACK$,10), \ DE-2
	             EP.FECHA.PROC$ + RIGHT$(EP.DONACION$,8),           \ DE-3
	             EP.ANUL.TRANSNUM$ + RIGHT$(EP.FILLER4$,10),        \ DE-4
	             DATE$ + TIME$ + EP.CARD.BIN$,                      \ DE-5
	             EP.ANUL.RRN$ + EP.FECHA.VENC$)                     ! DE-6 
			!
			If EP.Function$ = "11" And tefTrxCounter% < 50 Then \
			Begin
				Call printDebug("Almacena " + EP.RECIBO$ + "," + Right$(EP.VALOR.EPAY$,10))
				tefTrxCounter% = tefTrxCounter% + 1
				tefTrxArray$(tefTrxCounter%, 0) = "0"	! Estado 0 = Normal  1 = Anulada
				tefTrxArray$(tefTrxCounter%, 1) = EP.RECIBO$
				tefTrxArray$(tefTrxCounter%, 2) = EP.APPROV.CODE$
				tefTrxArray$(tefTrxCounter%, 3) = EP.FECHA.POSTEO$
				tefTrxArray$(tefTrxCounter%, 4) = EP.COD.PROC$
				tefTrxArray$(tefTrxCounter%, 5) = EP.CARD.NUMBER$
				tefTrxArray$(tefTrxCounter%, 6) = EP.AUTH.NUMBER$
				tefTrxArray$(tefTrxCounter%, 7) = Right$(EP.VALOR.EPAY$,10)
				tefTrxArray$(tefTrxCounter%, 8) = Right$(EP.TAX.EPAY$,10)
				tefTrxArray$(tefTrxCounter%, 9) = EP.RRN$
				tefTrxArray$(tefTrxCounter%,10) = EP.CUOTAS.QTY$
				tefTrxArray$(tefTrxCounter%,11) = EP.TIPO.VAR$
				tefTrxArray$(tefTrxCounter%,12) = Right$(EP.IVA.BASE$,10)
				tefTrxArray$(tefTrxCounter%,13) = Right$(EP.CASHBACK$,10)
				tefTrxArray$(tefTrxCounter%,14) = EP.PRODUCTO$
			Endif
			!
	        EP.VOUCHER.FOUND% = -1
	        If EP.TV.FORCE$ = "" Then Begin
		        TS.USERDATA$= Pack$(EP.EFT.CLAVE$ + EP.defaultApp$ + EP.FUNCTION$ + EP.RECIBO$ + "02") + ":" + \
		        	EP.CARD.BIN$ + String$(6, "*") + EP.CARD.NUMBER$ + ":" + \
		        	EP.FRANQUICIA$ + ":" + \
		        	EP.PRODUCTO$ + ":" + \
		        	EP.VALOR.EPAY$ + ":" + \
		        	EP.TIPO.VAR$ + ":" + \
		        	Str$(getInstitucion) + ":" + \
		        	EP.AUTH.NUMBER$ + ":" + \
		        	EP.RECIBO$
		        TS.TEMP1I1 = 99                  
		        CALL TSTPEC01
		      Endif Else \
		      	EP.TV.FORCE$ = ""
	    Endif
	 ENDIF
        
     If isApprovCode(EP.APPROV.CODE$) Then Begin
        If EP.Function$ = "12" Then Begin
			tmpIndex% = searchTefArray(EP.RECIBO$)
			If tmpIndex% > 0 Then \
				tefTrxArray$(tmpIndex%,0) = "1"
		Endif
        EP.STATE% = 4
        CALL TRANSLATE.ISO.CODE(EP.APPROV.CODE$,EP.APPROV.DESC$)
        CALL EP.DISPLAY.A.MESSAGE(EP.APPROV.DESC$)
        EP.APPROV.FOUND% = -1
        If EP.APPROV.CODE$ = "  " Then  \
        BEGIN
        	EP.EFT.RESPBLANCO% = -1
        ENDIF \ 	
        Else \
        BEGIN
         	EP.EFT.RESPBLANCO% = 0
        ENDIF 	
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
!        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.APPL$ + EP.FUNCTION$  + EP.RECIBO$ + "03", \ DE-1
!             RIGHT$(EP.IVA.BASE$,10) + RIGHT$(EP.CASHBACK$,10), \ DE-2
!             EP.FECHA.PROC$ + RIGHT$(EP.DONACION$,8)            \ DE-3
!             EP.ANUL.TRANSNUM$ + RIGHT$(EP.FILLER4$,10),        \ DE-4
!             DATE$ + TIME$ + EP.CARD.BIN$,                      \ DE-5
!             EP.ANUL.RRN$ + EP.FECHA.VENC$)                     ! DE-6 
        EP.VOUCHER.FOUND% = -1
!
      ENDIF   
      IF EP.AMJ.STATUS$ = "7" THEN \
      BEGIN 
        CALL EP.PARSE.DATA.REQUEST
        CALL EP.SAVE.KEYS
        CALL EP.GET.KBDATA(EP.DISP.MESSAGE$, EP.INI.RANGE$, EP.END.RANGE$, \
          EP.KEYB.DATA$)
        CALL EP.RESTORE.KEYS
      ENDIF
      IF EP.AMJ.STATUS$ = "8" THEN \
      BEGIN
        CALL EP.PARSE.DENTRY.REQUEST
        EP.INICIO.FOUND% = -1
        EP.LAST.FUNCTION$ = EP.FUNCTION$
        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
             EP.DE5.DATA$, EP.DE6.DATA$)
      ENDIF   
      IF EP.AMJ.STATUS$ = "9" THEN \
      BEGIN
        CALL EP.PARSE.DENTRY.REQUEST
        EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
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
        EP.TRX.STATUS$ = MID$(EP.AMJ.MESSAGE$,9,1)
        CALL EP.ADD.DATA.ENTRY(EP.EFT.CLAVE$ + EP.DE1.DATA$, EP.DE2.DATA$, EP.DE3.DATA$, EP.DE4.DATA$, \
             EP.DE5.DATA$, EP.DE6.DATA$)
        EP.STATE% = 4                       !   Fin de ciclo  
        IF EP.TRX.STATUS$ = "0" THEN  \
        BEGIN
          EP.TOHOST.FOUND% = -1
        ENDIF  
      ENDIF   
      If EP.AMJ.STATUS$ = "K" Then \
      Begin
      	CALL EP.EXECUTE.SPECIALSUB(EP.KEYB.DATA$)
      	EP.AMJ.STATUS$ = "7"
      Endif
    ENDIF
    IF  (EP.STATE% < 4) AND (EP.EXCEPTION$ = "") THEN \   ! Si se continua el ciclo
    BEGIN    
      EP.MESSAGE$ = RIGHT$(STRING$(6,"0")+ TS.TERMINAL$,6) + \
      RIGHT$(STRING$(6,"0")+STR$(SL.HD.TRANSNUM+1),6) + EP.ECR.DATETIME$ + \
        EP.APPL.STATUS$
      IF EP.AMJ.STATUS$ = "7" THEN \
        EP.MESSAGE$ = EP.MESSAGE$ + EP.KEYB.DATA$
      EP.MSGLEN$  = RIGHT$(STRING$(3,"0") + STR$(LEN(EP.MESSAGE$)),3)
      IF EP.ECR.FUNCTION$ = "49" THEN \
        EP.MESSAGE$ = EP.APPL$ + EP.ECR.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$ \
      ELSE \
        EP.MESSAGE$ = EP.APPL$ + EP.FUNCTION$ + EP.MSGLEN$ + EP.MESSAGE$
!
      CALL EP.SEND.MSG.TO.AMJ  
    ENDIF
!
  WEND
  EP.TRX.PEND$     = ""
  EP.REV.FUNCTION$ = ""
  EP.FECHA.PEND$   = ""
  EP.REV.APROB$    = ""
!  CALL EP.LINE.PRINT("func="+EP.FUNCTION$+" ini="+str$(EP.INICIO.FOUND%) + " res="+str$(ep.resp.found%) ,4100H)
!
  IF EP.FUNCTION$ = "11"  OR  \        ! Transaccion tarjeta
     EP.Function$ = "51"  OR  \
     EP.Function$ = "52"  OR  \
     EP.FUNCTION$ = "12"  OR  \
     EP.FUNCTION$ = "14"  OR  \
     EP.FUNCTION$ = "21" THEN \        ! Transaccion cheque
  BEGIN
    IF EP.INICIO.FOUND%      AND   \
       NOT EP.TOHOST.FOUND% THEN   \
    BEGIN
      IF EP.RESP.FOUND% THEN       \
      BEGIN
        IF EP.APPROV.FOUND% THEN   \
        BEGIN
          If NOT EP.VOUCHER.FOUND% OR EP.EFT.RESPBLANCO% Then \
          BEGIN
            EP.TRX.PEND$ = EP.EPAY.TRANSNUM$ 
            EP.FECHA.PEND$ = EP.ECR.DATETIME$
            EP.LAST.FUNCTION$ = EP.FUNCTION$
            EP.LAST.APPL$ = EP.APPL$
          ENDIF
        ENDIF 
      ENDIF \
      ELSE  \
      BEGIN
        EP.TRX.PEND$ = EP.EPAY.TRANSNUM$
        EP.FECHA.PEND$ = EP.ECR.DATETIME$
        EP.LAST.FUNCTION$ = EP.FUNCTION$
        EP.LAST.APPL$ = EP.APPL$
      ENDIF
    ENDIF
  ENDIF
!
END SUB
!
!-----------------------------------------------------------
!  	recover routines
!
SUB EPAY.START.RECOVER.EFT
!
  Call initializeTefArray
  EP.LINEA% = 0
  DIM EP.VOUCHER$(300)
  DIM EP.CUT.VOUCHER$(300)
  DIM EP.EFT.LINE$(100)
  EP.TRX.FUNCTION$ = ""
  EP.TRX.TRANSNUM$ = ""
  EP.TRX.RESP%     = 0
  EP.TRX.APPROV%   = 0
  EP.TRX.VOUCHER%  = 0
  EP.TRX.TOHOST1%  = 0 
  EP.TRX.TOHOST2%  = 0
  EP.TRX.ANUL%     = 0   
  EP.TRX.REVERSO%  = 0 
  EP.TRX.PEND%     = 0
  EP.TRX.FECHA$    = ""
  EP.LAST.FUNCTION$= "00"
  EP.LAST.APPL$ = "00"
  EP.LAST.TV$      = "00"
  EP.LAST.RECIBO$  = STRING$(6,"0")
  EP.TRX.PEND$     = ""
  EP.ECR.DATETIME$ = STRING$(12,"0")
  EP.VALOR.EPAY$   = STRING$(12,"0")  
  EP.TAX.EPAY$     = STRING$(12,"0")
  EP.PROP.EPAY$    = STRING$(12,"0")
  EP.IVA.BASE$     = STRING$(12,"0")
  EP.CASHBACK$     = STRING$(12,"0")
  EP.FILLER4$      = STRING$(12,"0")
  EP.FILLER8$      = STRING$(12,"0")
  EP.FILLER11$     = STRING$(12,"0")
  EP.FILLER12$     = STRING$(12,"0")
  EP.CAJERO$       = RIGHT$(STRING$(12,"0") + UNPACK$(TS.OPER$),12)
  DIM EP.PROMO.ID$(10)
  DIM EP.PROMO.STATUS$(10)
  DIM EP.PROMO.POINTS$(10) 
  DIM EP.PROMO.REDEEM$(10)
  DIM EP.PROMO.AMTPUR$(10)
  DIM EP.PROMO.QTYPUR$(10)     
!
END SUB
!
!
SUB EPAY.RECOVER.EFT 
!
  Integer*2 tmpIndex%
  !
  IF TS.TEMP1I2 = 11 THEN \
  BEGIN 
    EP.J% = 3
    EP.B$ = SL.STR.ENTRY$ + ":"
    CALL EP.GETUNPK
    IF LEN(EP.A$) > EP.LEN.CLAVE% THEN  \
    BEGIN
      IF LEFT$(EP.A$,EP.LEN.CLAVE%) = EP.EFT.CLAVE$ THEN \
      BEGIN
        EP.DE.APPL$     = MID$(EP.A$,EP.LEN.CLAVE% + 1,2)
        EP.DE.FUNCTION$ = MID$(EP.A$,EP.LEN.CLAVE% + 3,2)
        EP.DE.TRANSNUM$ = MID$(EP.A$,EP.LEN.CLAVE% + 5,6)
        EP.DE.TIPO$     = MID$(EP.A$,EP.LEN.CLAVE% + 11,2)
        IF EP.DE.FUNCTION$ = "11" OR   \   ! transacciones eft
           EP.DE.Function$ = "51" OR   \
           EP.DE.Function$ = "52" OR   \
           EP.DE.FUNCTION$ = "12" OR   \   ! transacciones eft
           EP.DE.FUNCTION$ = "14" OR   \   ! transacciones eft
           EP.DE.FUNCTION$ = "21" THEN \   ! transacciones eft
        BEGIN
          IF EP.DE.TIPO$ = "01" THEN    \
          BEGIN
            EP.LINEA% = EP.LINEA% + 1
            EP.TRX.TRANSNUM$  = EP.DE.TRANSNUM$
            EP.EPAY.TRANSNUM$ = EP.DE.TRANSNUM$ 
            EP.TRX.FUNCTION$  = EP.DE.FUNCTION$
            EP.LAST.FUNCTION$ = EP.DE.FUNCTION$
            EP.LAST.APPL$     = EP.DE.APPL$
            EP.LAST.RECIBO$   = STRING$(6,"0")
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            EP.DE.AMOUNT$     = LEFT$(EP.A$,10)
            EP.DE.IVA$        = MID$(EP.A$,11,10)
            CALL EP.GETUNPK
            EP.TRX.FECHA$     = LEFT$(EP.A$,12)
            EP.DE.PROPINA$    = MID$(EP.A$,13,8)
            EP.TRX.RESP%      = 0
            EP.APPROV.FOUND%  = 0
            EP.VOUCHER.FOUND% = 0
            EP.TRX.APPROV%    = 0
            EP.TRX.VOUCHER%   = 0
            EP.TRX.TOHOST1%   = 0
            EP.TRX.TOHOST2%   = 0
            EP.TRX.REVERSO%   = 0
          ENDIF                          
!                      
          IF EP.DE.TIPO$ = "02" THEN    \
          BEGIN
            EP.TRX.RESP%     = -1
            CALL EP.GETUNPK
            EP.DE2.DATA$     = EP.A$
            EP.DE.ISOCOD$    = MID$(EP.A$,19,2)
            !
            If isApprovCode(EP.DE.ISOCOD$) Then Begin
              EP.TRX.APPROV% = -1
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
            IF EP.DE.FUNCTION$ = "11"  OR \
            	 EP.DE.Function$ = "51"  OR \
            	 EP.DE.Function$ = "52"  OR \
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
              EP.DE.PROPINA$ = MID$(EP.DE5.DATA$,13,8)
              EP.DE.RRN$    = LEFT$(EP.DE6.DATA$,12)
              EP.DE.CUOTAS$ = MID$(EP.DE6.DATA$,13,2)
              EP.DE.TIPOVAR$ = MID$(EP.DE6.DATA$,15,2)
              EP.H.TIPO.VAR$  =  EP.DE.TIPOVAR$
              !EP.AUTH.NUMBER$ = EP.H.AUTH.NUMBER$
              EP.AUTH.NUMBER$ = EP.DE.AUTH$
              !EP.APPROV.CODE$ = EP.H.APPROV.CODE$
              EP.APPROV.CODE$ = EP.DE.ISOCOD$
              !EP.CARD.NUMBER$ = EP.H.CARD.NUMBER$
              EP.CARD.NUMBER$ = EP.DE.CARD$
              EP.RECIBO$      = EP.DE.TRANSNUM$
              EP.LAST.RECIBO$ = EP.RECIBO$
            ENDIF \
            ELSE \
            BEGIN   
              EP.DE.ISOCOD$     = MID$(EP.DE2.DATA$,19,2)
              EP.DE.CENTRAL.CHEQUE$ = MID$(EP.DE3.DATA$,11,1)
              EP.DE.AUTH$       = MID$(EP.DE3.DATA$,15,6)
              EP.DE.AMOUNT$     = LEFT$(EP.DE4.DATA$,10)
              EP.DE.NRO.CHEQUE$ = MID$(EP.DE4.DATA$,11,9)
              EP.DE.CTA.CTE$    = MID$(EP.DE5.DATA$,8,13)
              EP.DE.RRN$        = LEFT$(EP.DE6.DATA$,12)
              EP.DE.TIPOVAR$    = MID$(EP.DE6.DATA$,15,2)
              EP.H.TIPO.VAR$      =  EP.DE.TIPOVAR$
              !EP.AUTH.NUMBER$ = EP.H.AUTH.NUMBER$
              EP.AUTH.NUMBER$ = EP.DE.AUTH$
              !EP.APPROV.CODE$ = EP.H.APPROV.CODE$
              EP.APPROV.CODE$ = EP.DE.ISOCOD$
              EP.RECIBO$      = EP.DE.TRANSNUM$
              EP.LAST.RECIBO$ = EP.RECIBO$
            ENDIF
          ENDIF  
!                                              
          IF EP.DE.TIPO$ = "03" THEN \
          BEGIN
            EP.TRX.VOUCHER%   = -1
            EP.VOUCHER.FOUND% = -1
            EP.LAST.RECIBO$   = EP.DE.TRANSNUM$
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
            EP.DE.IVA.BASE$  = LEFT$(EP.DE2.DATA$,10)
            EP.DE.CASHBACK$  = MID$(EP.DE2.DATA$,11,10)
            EP.DE.FECHA.PROC$ = LEFT$(EP.DE3.DATA$,12)
            EP.DE.ANULTRANSNUM$ = LEFT$(EP.DE4.DATA$,6) 
            EP.DE.CARD.BIN$   = MID$(EP.DE5.DATA$,13,6)
            EP.DE.FECHA.VENC$ = MID$(EP.DE6.DATA$,13,4)
          ENDIF  
          IF EP.APPROV.FOUND% AND EP.VOUCHER.FOUND% AND \
             NOT TS.RETV.IN.PROGRESS THEN \
          BEGIN  
            IF EP.DE.FUNCTION$ = "11"  OR \
               EP.DE.Function$ = "51" OR   \
            	 EP.DE.Function$ = "52" OR   \
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
            EP.CAJERO$         = RIGHT$(STRING$(12,"0") + UNPACK$(TS.OPER$),12)
            EP.EFT.TRX%        = -1
            EP.EFT.DETAIL%     = -1
            EP.EFT.SMA%        = -1
!            EP.DE.TRX.STATUS$  = ""
!            EP.DE.AMJ.STATUS$  = ""
!            EP.DE.APPROV.CODE$ = ""
!            EP.DE.APPROV.DESC$ = ""
!            EP.DE.USER.DATA$   = ""  
!            CALL REPRINT.VOUCHER(EP.DE.APPL$, EP.FUNCTION$, EP.DE.TRX.STATUS$, EP.DE.AMJ.STATUS$, \
!               EP.DE.TRANSNUM$, EP.DE.APPROV.CODE$, EP.DE.APPROV.DESC$, EP.DE.USER.DATA$)  
          ENDIF     
!---------------------------------------------------------
!          IF NOT EP.NO.TRACE% THEN \
!          BEGIN
!            TS.LINETYPE = 29
!            TS.SAVPRT$ = "TRX="+ EP.TRX.TRANSNUM$(EP.LINEA%)+ \
!                 " RESP=" + STR$(EP.TRX.RESP%(EP.LINEA%)) +   \
!                 " APRO=" + STR$(EP.TRX.APPROV%(EP.LINEA%)) + \
!                 " VOUC=" + STR$(EP.TRX.VOUCHER%(EP.LINEA%))
!            TS.SAVPRT.OPT = 4100H
!            CALL TSPREC01
!          ENDIF
!---------------------------------------------------------
        ENDIF                                 ! fin de trx eft 
!                
        IF EP.DE.FUNCTION$ = "12"    AND  \   !  anula en la misma transaccion
           EP.DE.TIPO$     = "03"    THEN \
        BEGIN
          EP.EFT.FOUND% = 0
          EP.POS% = 1
          WHILE NOT EP.EFT.FOUND%  AND EP.POS% <= EP.MAX.TRX% 
            IF EP.TRX.TRANSNUM$ = EP.DE.ANULTRANSNUM$ THEN \
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
            EP.TRX.ANUL% = -1 
          ENDIF
!---------------------------------------------------------
!          IF NOT EP.NO.TRACE% THEN \
!          BEGIN
!            TS.LINETYPE = 29
!            TS.SAVPRT$ = "TRX="+ EP.TRX.TRANSNUM$(EP.POS%)+ \
!                 " ANUL=" + STR$(EP.TRX.ANUL%(EP.POS%))
!            TS.SAVPRT.OPT = 4100H
!            CALL TSPREC01
!          ENDIF
!---------------------------------------------------------
        ENDIF
!                          
        IF EP.DE.FUNCTION$ = "11" OR   \    ! transacciones eft
        	 EP.DE.Function$ = "51" OR   \
        	 EP.DE.Function$ = "52" OR   \
           EP.DE.FUNCTION$ = "12" OR   \    
           EP.DE.FUNCTION$ = "13" OR   \    
           EP.DE.FUNCTION$ = "14" OR   \    
           EP.DE.FUNCTION$ = "15" THEN \    
        BEGIN
          IF EP.DE.TIPO$ = "04"  OR \       ! respuesta TO de appl manager
             EP.DE.TIPO$ = "05" THEN \      ! reversos
          BEGIN
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            CALL EP.GETUNPK
            IF MID$(EP.A$,17,1) = "0" THEN \       !  finaliza normalmente
            BEGIN    
              EP.TRX.REVERSO% = 0
              IF EP.DE.TIPO$ = "04" THEN \
              BEGIN 
                EP.TRX.TOHOST1% = -1
              ENDIF \
              ELSE \
              BEGIN
                EP.TRX.TOHOST2% = -1
              ENDIF
            ENDIF \
            ELSE  \
            BEGIN
              EP.TRX.REVERSO% = -1
            ENDIF
          ENDIF                                 ! end tipo 04 o 05         
        ENDIF                                   ! end reversos    
      ENDIF                                     ! end clave de eft 
    ENDIF
  ENDIF Else If TS.TEMP1I2 = 99 Then Begin
  	EP.J% = 3
    EP.B$ = SL.STR.ENTRY$ + ":"
    Call EP.GETUNPK
    Call printDebug("Recupera 99: " + EP.A$)
    If Len(EP.A$) > EP.LEN.CLAVE% Then Begin
    	If Left$(EP.A$, EP.LEN.CLAVE%) = EP.EFT.CLAVE$ Then Begin
    		If EP.DE.FUNCTION$ = "11" And tefTrxCounter% < 50 Then Begin
	    		Call EP.GETUNPK
	    		Call EP.GETUNPK
	    		Call EP.GETUNPK
	    		EP.PRODUCTO$ = Pack$(EP.A$)
	    		Call printDebug("Recupera " + EP.RECIBO$ + "," + Right$(EP.DE.AMOUNT$,10) + "," + EP.PRODUCTO$)
				tefTrxCounter% = tefTrxCounter% + 1
				tefTrxArray$(tefTrxCounter%, 0) = "0"	! Estado 0 = Normal  1 = Anulada
				tefTrxArray$(tefTrxCounter%, 1) = EP.RECIBO$
				tefTrxArray$(tefTrxCounter%, 2) = EP.APPROV.CODE$
				tefTrxArray$(tefTrxCounter%, 3) = EP.DE.POSTEO$
				tefTrxArray$(tefTrxCounter%, 4) = EP.DE.CODPRO$
				tefTrxArray$(tefTrxCounter%, 5) = EP.DE.CARD$
				tefTrxArray$(tefTrxCounter%, 6) = EP.DE.AUTH$
				tefTrxArray$(tefTrxCounter%, 7) = Right$(EP.DE.AMOUNT$,10)
				tefTrxArray$(tefTrxCounter%, 8) = Right$(EP.DE.IVA$,10)
				tefTrxArray$(tefTrxCounter%, 9) = EP.DE.RRN$
				tefTrxArray$(tefTrxCounter%,10) = EP.DE.CUOTAS$
				tefTrxArray$(tefTrxCounter%,11) = EP.DE.TIPOVAR$
				tefTrxArray$(tefTrxCounter%,12) = Right$(EP.DE.IVA.BASE$,10)
				tefTrxArray$(tefTrxCounter%,13) = Right$(EP.DE.CASHBACK$,10)
				tefTrxArray$(tefTrxCounter%,14) = EP.PRODUCTO$
	    	Endif Else If EP.DE.FUNCTION$ = "12" Then Begin
				tmpIndex% = searchTefArray(EP.RECIBO$)
				If tmpIndex% > 0 Then \
					tefTrxArray$(tmpIndex%,0) = "1"
	    	Endif
    	Endif
  	Endif
  Endif
  IF TS.TEMP1I2 = 05  OR \
     TS.TEMP1I2 = 06  THEN \
  BEGIN 
    IF EP.EFT.SMA% THEN \
      EP.EFT.SMA% = 0
  ENDIF
END SUB
!
!
SUB EPAY.END.RECOVER.EFT
!
  Integer*1 autoRecup%
!
  autoRecup% = 0
!
  EP.TRX.PEND%     = 0 
  EP.TRX.PEND$     = ""
  EP.FECHA.PEND$   = ""
  EP.REV.FUNCTION$ = "00"
  EP.LAST.RECIBO$  = STRING$(6,"0")
  EP.ECR.TRANSNUM$ = EP.TRX.TRANSNUM$ 
  EP.IVA.BASE$     = STRING$(12,"0")
  EP.CAJERO$       = RIGHT$(STRING$(12,"0") + UNPACK$(TS.OPER$),12)
  EP.PROP.EPAY$    = STRING$(12,"0")
  EP.CASHBACK$     = STRING$(12,"0")
  EP.FILLER4$      = STRING$(12,"0")
  EP.FILLER11$     = STRING$(12,"0")
  EP.FILLER12$     = STRING$(12,"0")

!  call ep.line.print("antesvr="+ep.de.amount$+"iv="+ep.de.iva$+"f="+ep.de.function$,4100H)
  IF EP.TRX.TRANSNUM$ <> "" AND  \    ! si hubo transaccion 
     NOT EP.TRX.TOHOST1%    AND  \    ! si no hay reverso appl manager
     NOT EP.TRX.TOHOST2%    THEN \    ! si no hay reverso host
  BEGIN
    IF EP.TRX.RESP% THEN  \           ! si obtuvo respuesta
    BEGIN
      IF EP.TRX.APPROV% THEN \        ! si fue aprobada
      BEGIN 
        IF NOT EP.TRX.VOUCHER% THEN \ ! si no obtuvo voucher
        BEGIN
          EP.TRX.PEND%      = -1      ! se marca como pendiente
          EP.TRX.PEND$      = EP.TRX.TRANSNUM$
          EP.FECHA.PEND$    = EP.TRX.FECHA$
          EP.ECR.DATETIME$  = EP.TRX.FECHA$
          EP.REV.FUNCTION$  = "15"
          EP.VALOR.EPAY$    = RIGHT$(STRING$(12,"0") + EP.DE.AMOUNT$, 12)
          EP.TAX.EPAY$      = RIGHT$(STRING$(12,"0") + EP.DE.IVA$, 12)
          EP.PROP.EPAY$     = RIGHT$(STRING$(12,"0") + EP.DE.PROPINA$,12)
        ENDIF  
      ENDIF  
    ENDIF \
    ELSE \                                   ! si no obtuvo respuesta                                  
    BEGIN 
      EP.TRX.PEND%      = -1
      EP.TRX.PEND$      = EP.TRX.TRANSNUM$
      EP.FECHA.PEND$    = EP.TRX.FECHA$
      EP.ECR.DATETIME$  = EP.TRX.FECHA$
      EP.REV.FUNCTION$  = "13"
      EP.VALOR.EPAY$    = RIGHT$(STRING$(12,"0") + EP.DE.AMOUNT$, 12)
      EP.TAX.EPAY$      = RIGHT$(STRING$(12,"0") + EP.DE.IVA$, 12)
      EP.PROP.EPAY$     = RIGHT$(STRING$(12,"0") + EP.DE.PROPINA$,12)
    ENDIF                     ! end si no obtuvo respuesta
!
    If autoRecup% = 0 AND EP.TRX.PEnd$ <> "" Then \
    Begin
      EP.EFT.RECUP.VAL% = INT%(VAL(EP.VALOR.EPAY$))
      Call EP.DISPLAY.AN.ERROR("REINTENTE PAGO POR  $" + STR$(EP.EFT.RECUP.VAL%))
    Endif
!
    If EP.TRX.PEnd$ <> "" And autoRecup% <> 0 Then  \
    BEGIN 
      EP.TRY.COUNT% = 1
      EP.EFT.OK% = 0 
      WHILE EP.TRX.PEND$ <> "" AND \   ! There is a pending trx
            EP.TRY.COUNT%  <= 3        ! only three times
        EP.USER.DATA$    = ""          !  no user data
  !        
        CALL EP.DISPLAY.AN.ERROR("TRANS. NO TERMINADA!")
        If TEFII.FUNCT.ISSYNCBYDEMAND Then \
        	CALL EP.DISPLAY.AN.ERROR("DIGITE FUNCION DE   RECUPERA. EN DATAFON")
        !
        !
        !------------------------------------------------------------------------
        ! 2016-08-22 jsv
        ! En vez de especificar aplicación 85, se debe especificar el contenido
        ! de la variable EP.LAST.APPL$
        !------------------------------------------------------------------------
        EP.APPL$ = EP.LAST.APPL$
        !EP.APPL$ = "85"
        !------------------------------------------------------------------------
        EP.ECR.FUNCTION$ = "49"
        EP.TRX.STATUS$   = "1"             !  sin communications 
        EP.AMJ.STATUS$   = "0"             !  sin iniciar transaccion
        CALL END.EFT.API(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,\
          EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EPAY$, EP.TAX.EPAY$, EP.PROPINA$,  \
          EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$,       \
          EP.TIPO.VAR$, EP.USER.DATA$)
!          
!        call ep.line.print("Rcst=" + EP.AMJ.STATUS$ + \
!              " trxst=" + EP.TRX.STATUS$ + \
!              " cod=" + ep.approv.code$  + \
!              " tv=" +  ep.tipo.var$, 4100H)
!        call ep.line.print("vou=" + ep.ecr.transnum$ + \
!              " lst=" + ep.last.recibo$,4100H)
!
        IF EP.AMJ.STATUS$ <> "2"  THEN \   !    evaluate HOST command reponse
        BEGIN
          CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
          CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
        ENDIF \
        ELSE \
        IF EP.TRX.STATUS$ <> "0" THEN \    !    whether communication error found
        BEGIN
          CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
          CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
          IF EP.TRX.STATUS$ = "O" THEN \      !  Trx no existe
            EP.TRX.PEND$ = "" 
        ENDIF\
        Else If isApprovCode(EP.APPROV.CODE$) Then Begin
          CALL EP.SAVE.KEYS
          DIM TS.IO.KEYS(10)                         ! Clear input
          EP.CARD.BIN$    = EP.USER.DATA$
          IF EP.DE.FUNCTION$ = "12" OR \
             EP.DE.FUNCTION$ = "14" THEN  \
            TS.IO.KEYS(1) = 70
          TS.IO.KEYS(7)   = 90 + VAL(LEFT$(EP.TIPO.VAR$,1))
          TS.IO.KEYS(3)   = 78  
          TS.IO.DATA$(3)  = MID$(EP.TIPO.VAR$,2,1)
          !
          !TS.IO.DATA$(9)  = EP.CARD.BIN$
          !
          TS.IO.DATA$(9)  = EP.AUTH.NUMBER$
          TS.IO.DATA$(7)  = EP.VALOR.EPAY$
          EP.EFT.TRX%     = -1
          EP.EFT.DETAIL%  = -1
          EP.EFT.SMA%     = -1
          EP.TRX.PEND$ = "" 
          EP.TRX.PEND% = 0
          CALL TSTDEC01
          CALL EP.RESTORE.KEYS
          TS.TEMP1$ = "TRX CONTABILIZADA!!!"
          TS.TEMP2$ = STRING$(20," ") 
          TS.LINETYPE = 12
          CALL TSCSEC08
          EP.EFT.RECUP.TRX% = -1
        ENDIF \
        ELSE \
        BEGIN
!          CALL TRANSLATE.ISO.CODE(EP.APPROV.CODE$,EP.MESSAGE.DESC$)
          CALL EP.DISPLAY.AN.ERROR(EP.APPROV.DESC$)
          EP.TRX.PEND$ = "" 
        ENDIF 
        EP.TRY.COUNT% = EP.TRY.COUNT% + 1
      WEND
      EP.TRX.PEND$ = ""                 ! Anula recuperacion
    ENDIF                              ! end trx pendiente 
!    
  ENDIF                       ! end si hubo transaccion 
!
  If EP.TRX.TRANSNUM$ <> "" AND  \    ! si ULTIMA TRANSACCION 
       NOT EP.TRX.TOHOST1%  AND  \    ! si no hay reverso appl manager
       NOT EP.TRX.TOHOST2%  AND  \    ! si no hay reverso host
       autoRecup% <> 0      Then \    ! si está activa función de autorecuperación
  BEGIN
    IF EP.TRX.RESP%       AND \          ! si obtuvo respuesta
       EP.TRX.APPROV%     AND \          ! si fue aprobada
       EP.TRX.VOUCHER%    AND \          ! si obtuvo voucher
       EP.EFT.SMA% THEN \                        ! no ha entrado a Supermarket Application
    BEGIN
!      CALL EP.SAVE.KEYS
      CALL EP.DISPLAY.AN.ERROR("TRANS. RECUPERADA!! ")
      DIM TS.IO.KEYS(10)                         ! Clear input
      IF EP.DE.FUNCTION$ = "12" OR \
         EP.DE.FUNCTION$ = "14" THEN  \
        TS.IO.KEYS(1) = 70
      TS.IO.KEYS(3)  = 78                        ! Asterisk key
      TS.IO.DATA$(3) = RIGHT$(EP.DE.TIPOVAR$,1)  ! Tender variety
      TS.IO.KEYS(7)  = VAL(LEFT$(EP.DE.TIPOVAR$,1)) + 90 ! Tender key
      TS.IO.DATA$(7) = EP.DE.AMOUNT$             ! Tender amount
      !
      !TS.IO.DATA$(9) = EP.DE.CARD$ + EP.DE.AUTH$ + \
      !    EP.TRX.TRANSNUM$
      !
      TS.IO.DATA$(9)  = EP.DE.AUTH$
      !
      TS.IO.KEYS(9)  = 90
      EP.EFT.SMA% = -1
!      IF SL.TE.TENDTYPE = 3 THEN \ Food stamps so
!        TS.TRX.STATUS = -1 ! set F.S. bal due
      CALL TSTDEC01
      CALL EP.RESTORE.KEYS
      TS.TEMP1$ = "TRX CONTABILIZADA!!!"
      TS.TEMP2$ = STRING$(20," ") 
      TS.LINETYPE = 12
      CALL TSCSEC08
      EP.EFT.RECUP.TRX% = -1
    ENDIF
  ENDIF 

END SUB
!
Sub resetVariablesTef
	Call initializeTefArray
	If TO.USEROPTS(7) <> 0 And \  ! user option 7 = Y
			EP.EFT.ACTIVO%  Then Begin
		EP.EFT.TRX% = 0
		TO.XCHGLIM = EP.XCHGLIM%
		EP.EFT.DETAIL% = 0
		EP.TRX.PEND$   = ""
	Endif
End Sub
!
!---------------------------------------------------------------------------------------------------
! User Exits
!---------------------------------------------------------------------------------------------------
Sub UE.TEF.01 Public
	Call resetVariablesTef
End Sub
!
Sub UE.TEF.04 Public
	Call resetVariablesTef
End Sub
!
Sub UE.TEF.06 Public
	If EP.SWITCH.PROC% > 0 And TS.PROCEDURE = EP.SWITCH.PROC% Then Begin
		Call EP.switchNet
		TS.USER.RETURN = -1
	Endif
End Sub
!
Sub UE.TEF.07 Public
	launchApplFunc$ = ""
	Call setRepeatLastSequence( 0 )
	Call TEFII.FUNCT.setApplyLastPayment( 0 )
	!
	EP.ERRNCODE% = 0
	EP.XCHGLIM% = TO.XCHGLIM
	TEFII.applName$ = "APITEFII"
	Call EPAY.INITIALIZATION
End Sub
!
Sub UE.TEF.13 Public
	If TO.USEROPTS(7)  And  \      ! user option 7 = Y
			EP.EFT.ACTIVO%  Then Begin
		If Not TS.ER.RETURN And EP.EFT.OK% Then Begin	!IF RECORD NOT READ OK AND TENDER MUST GET DATA
			If TS.TS11WERR$ NE "EF" Then Begin					!NOT RECORD NOT FOUND ERROR
				TS.TS11WERR$ = "EF"												!SET AS RECORD NOT FOUND ERROR
			Endif
		Endif
	Endif
End Sub
!
Sub UE.TEF.14 Public
	String tmpAmount$
	Integer*1 tmpTefKey%
	Integer*2 tmpI%
	!
	! CONTROLA QUE NO SE HAGA ANULADO TOTAL SIN ANTES DEVOLVER TRANSACCIONES
	If EP.controlAnuladoTotal% And 			\! Control de anulado total activo
			TS.IO.KEYS(1) = 70 And 					\! Anulado
			TS.IO.KEYS(6) = 81 And 					\! Total
			TS.TEMP1I2 = 2 And 							\! Input Key count
			TS.PROCEDURE < 1 And 						\! Transaccion de venta
			TS.TENDERED(0) <> 0 Then Begin 	 ! Medios de pago entrados
		DIM TS.IO.KEYS(10)
		DIM TS.IO.DATA$(10)
		TS.IO.MOTORKEY = 0
		CALL EP.DISPLAY.AN.ERROR("DEVUELVA MEDIOS PAGO")
		GOTO endUEAPPL14
	Endif
	!
	If TS.IO.KEYS(5) = 61 And \
			TS.IO.DATA$(5) = "2" Then Begin
		! Intercambio de medio de pago
		EP.AUTH.NUMBER2$ = ""
		EP.CARD.NUMBER2$ = ""
		EP.RECIBO2$ = ""
		EP.AUTH.NUMBER$ = ""
		EP.CARD.NUMBER$ = ""
		EP.RECIBO$ = ""
		EP.EFT.ENTRAD% = 0
	Endif
	!
	!Call printDebug("TS.IO.KEYS(7)=" + Str$(TS.IO.KEYS(7)))
	!Call printDebug("EP.initialTefKey%=" + Str$(EP.initialTefKey%))
	!Call printDebug("TO.USEROPTS(7)=" + Str$(TO.USEROPTS(7)))
	!Call printDebug("EP.EFT.ACTIVO%=" + Str$(EP.EFT.ACTIVO%))
	!
	tmpTefKey% = 0
	If EP.tendMenuKey% > 0 And TS.IO.MOTORKEY = EP.tendMenuKey% Then Begin
		Call EP.SAVE.KEYS
		EP.APPL$ = TIISELEC.getTefApp("", EP.ACTUALNET%)
		!
		Call traceApplication("TEF Application: " + EP.APPL$, TEFII.applName$)
		!
		Call EP.RESTORE.KEYS
		If EP.APPL$ = "" Then Begin
			TS.IO.MOTORKEY = 0
			DIM TS.IO.KEYS(10)
			DIM TS.IO.DATA$(10)
		Endif Else Begin
			tmpTefKey% = -1
		Endif
	Endif Else If TS.IO.MOTORKEY = EP.initialTefKey% Then Begin
		tmpTefKey% = -1
		EP.APPL$ = EP.defaultApp$
	Endif Else Begin
		tmpI% = 0
		While tmpTefKey% = 0 And tmpI% < EP.otherTefCount%
			If TS.IO.MOTORKEY = EP.otherTefKeys%(tmpI%, 0) Then Begin
				tmpTefKey% = -1
				EP.APPL$ = Right$(String$(2, "0") + Str$(EP.otherTefKeys%(tmpI%, 1)), 2)
			Endif Else Begin
				tmpI% = tmpI% + 1
			Endif
		Wend
	Endif
	!
	If tmpTefKey% Then Begin
		If TO.USEROPTS(7) And \    ! user option 7 = Y
				EP.EFT.ACTIVO%  Then Begin
			EP.validarPan% = 0
			!EP.APPL$ = "85"                    !  set to TEF application services
			EP.TRX.STATUS$   = "1"             !  sin communications 
			EP.AMJ.STATUS$   = "0"             !  sin iniciar transaccion
			EP.EFT.OK% = 0
			!
			If TS.IO.KEYS(1) = 70 Or TS.BALDUE(0) < 0 Then Begin
				EP.ECR.FUNCTION$ = "10"          !  void trx
				If TS.IO.KEYS(3) = 78 Then Begin
					EP.ECR.TRANSNUM$ = RIGHT$(STRING$(6,"0")+TS.IO.DATA$(3),6)  ! Numero de transaccion por corregir
					EP.ECR.TRANSNUM2$ = RIGHT$(STRING$(6,"0")+TS.IO.DATA$(3),6)  ! Numero de transaccion por corregir
				Endif Else Begin
					CALL EP.SAVE.KEYS
					EP.ECR.TRANSNUM$ = ""
					CALL EP.GET.KBDATA("Digite recibo anular", "1", "999999",EP.ECR.TRANSNUM$)
					CALL EP.RESTORE.KEYS
					If EP.ECR.TRANSNUM$ = "" Then Begin
						TS.IO.MOTORKEY = 0
						DIM TS.IO.KEYS(10)
						DIM TS.IO.DATA$(10)
						goto endUEAPPL14
					Endif Else Begin
						EP.ECR.TRANSNUM$ = RIGHT$(STRING$(6,"0")+EP.ECR.TRANSNUM$,6)  ! Numero de transaccion por corregir
						EP.ECR.TRANSNUM2$ = EP.ECR.TRANSNUM$  ! Numero de transaccion por corregir          
					Endif
				Endif
			Endif Else Begin
				EP.ECR.FUNCTION$ = "09"           ! init new trx
				EP.ECR.TRANSNUM$ = RIGHT$(STRING$(6,"0")+STR$(SL.HD.TRANSNUM+1),6)
			Endif
			!
			If TS.PROCEDURE < 1 And TS.INTRX Then Begin ! transaccion de venta normal
				If Not TS.BAL.TAKEN Then Begin ! OJO  VERIFICAR
					TS.GUIDANCE = 1020
					TS.IO.MOTORKEY = 0
				Endif
				If TS.IO.DATA$(7) = "" Then Begin
					If EP.TRX.PEND$ <> "" Then Begin		 ! CAB SI FUN 14 TOMA VALOR ANTERIOR
						TS.IO.DATA$(7) = STR$(VAL(TEFII.FORMAT.RTRIM$(TEFII.FORMAT.LTRIM$(TEFII.FUNCT.GETAMT))))
					Endif Else Begin
						TS.IO.DATA$(7) = STR$(TS.BALDUE(0))
						EP.BALDUE.CHG% = -1
					Endif
				Endif Else Begin
					If EP.TRX.PEND$ <> "" And \ 
							TS.IO.KEYS(1) NE 70 And \
							TS.IO.KEYS(3) NE 78 Then Begin
						tmpAmount$ = STR$(VAL(TEFII.FORMAT.RTRIM$(TEFII.FORMAT.LTRIM$(TEFII.FUNCT.GETAMT))))
						If Val(tmpAmount$) <> 0 Then TS.IO.DATA$(7) = tmpAmount$
					Endif
				Endif
				EP.BALDUE.CHG% = 0
				EP.VALOR.EFT$ = RIGHT$(STRING$(12,"0")+TS.IO.DATA$(7),12) ! valor digitado
				!
				If ABS(VAL(EP.VALOR.EFT$)) <= ABS(TS.BALDUE(0)) Then Begin ! calculo del IVA
					EP.TAX.EPAY% = calculateTaxValue(VAL(EP.VALOR.EFT$))
					EP.TAX.BASE% = calculateTaxBase(VAL(EP.VALOR.EFT$), EP.IVADEV$)
					EP.CASHBACK$ = Right$(String$(12, "0") + \
							Str$(calculateIACValue(VAL(EP.VALOR.EFT$))), 12)
				Endif Else Begin
					EP.TAX.EPAY% = calculateTaxValue(TS.BALDUE(0))
					EP.TAX.BASE% = calculateTaxBase(TS.BALDUE(0), EP.IVADEV$)
					EP.CASHBACK$ = Right$(String$(12, "0") + \
							Str$(calculateIACValue(TS.BALDUE(0))), 12)
				Endif
				!EP.CASHBACK$ = STRING$(12,"0")
				!
				EP.TAX.EFT$ = RIGHT$(STRING$(12,"0")+STR$(EP.TAX.EPAY%),12) ! valor iva
				EP.TAX.BASE$ = RIGHT$(STRING$(12,"0")+STR$(EP.TAX.BASE%),12) ! valor iva
				EP.FILLER4$  = STRING$(12,"0")
			Endif Else If TS.PROCEDURE = 1 Then Begin  ! Tender Cashing
				If TS.IO.DATA$(7) = "" Then Begin
					TS.GUIDANCE = 1003
					TS.IO.MOTORKEY = 0
				Endif Else Begin
					EP.VALOR.EFT$ = RIGHT$(STRING$(12,"0")+TS.IO.DATA$(7),12) ! valor digitado
					EP.TAX.EPAY% = 0
					EP.TAX.BASE% = 0
					EP.TAX.EFT$ = RIGHT$(STRING$(12,"0")+STR$(EP.TAX.EPAY%),12) ! valor iva
					EP.TAX.BASE$ = RIGHT$(STRING$(12,"0")+STR$(EP.TAX.BASE%),12) ! valor iva
				Endif
			Endif Else If TS.PROCEDURE = 2 Then Begin  ! Transaccion de intercambio de medio de pago
				If TS.IO.DATA$(7) = "" Then Begin
					If EP.ECR.FUNCTION$ = "10" Then Begin
						TS.GUIDANCE = 1003
						TS.IO.MOTORKEY = 0
					Endif Else Begin
						TS.IO.DATA$(7) = EP.VALOR.EFT$
					Endif
				Endif Else Begin
					If EP.ECR.FUNCTION$ = "10" Then Begin
						EP.VALOR.EFT$ = RIGHT$(STRING$(12,"0") + TS.IO.DATA$(7),12) ! valor digitado
					Endif Else Begin
						If VAL(EP.VALOR.EFT$) <> VAL(TS.IO.DATA$(7)) Then Begin
							TS.GUIDANCE = 1041
							TS.IO.MOTORKEY = 0 
						Endif
					Endif
				Endif
				If EP.ECR.FUNCTION$ = "09" And Not EP.EFT.TRX% Then Begin
					TS.GUIDANCE = 1003
					TS.IO.MOTORKEY = 0
				Endif
			Endif Else Begin ! Transaccion no de venta diferente a TEF
				TS.GUIDANCE = 1003
				TS.IO.MOTORKEY = 0
			Endif
			!
			If TS.GUIDANCE = 0 Then Begin ! trx ok
				If EP.TRX.PEND$ <> "" Then Begin
					EP.ECR.FUNCTION$ = "49"
				Endif
				EP.PROPINA$  = STRING$(12,"0")
				EP.ECR.NBR$  = RIGHT$(STRING$(6,"0")+TS.TERMINAL$,6) + \
						RIGHT$(STRING$(4,"0")+TS.STORE$,4) ! numero de terminal y tienda
				EP.APPROV.CODE$  = ""        !  no ISO8583 message
				EP.CARD.NUMBER$  = ""        !  no card number
				EP.AUTH.NUMBER$  = ""        !  no autorization number
				EP.USER.DATA$    = EP.TAX.BASE$ + EP.CASHBACK$   !  user data
				!
				Call INIT.EFT.API(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,    \
				EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.PROPINA$, \
				EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$,       \
				EP.TIPO.VAR$, EP.USER.DATA$)
				!
				If EP.AMJ.STATUS$ <> "2" Then Begin !    evaluate HOST command reponse
					CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
					CALL EP.DISPLAY.A.MESSAGE(EP.MESSAGE.DESC$)
					TS.IO.MOTORKEY = 0
				Endif Else If EP.TRX.STATUS$ <> "0" Then Begin ! whether communication error found
					Call printDebug("TRANSLATE.COMM.CODE(" + EP.TRX.STATUS$ + ")")
					CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
					CALL EP.DISPLAY.A.MESSAGE(EP.MESSAGE.DESC$)
					TS.IO.MOTORKEY = 0
				Endif Else Begin
					TS.IO.KEYS(7)  = 90 + VAL(LEFT$(EP.TIPO.VAR$,1))
					TS.IO.KEYS(3)  = 78  
					TS.IO.DATA$(3) = RIGHT$(EP.TIPO.VAR$,1)
					TS.IO.MOTORKEY = TS.IO.KEYS(7)
					!
					!TS.IO.DATA$(9) = RIGHT$(STRING$(4,"0")+ EP.CARD.NUMBER$,4)+ \
					!		RIGHT$(STRING$(6,"0")+EP.AUTH.NUMBER$,6)+ \
					!		RIGHT$(STRING$(6,"0")+EP.ECR.TRANSNUM$,6)
					!
					TS.IO.DATA$(9)  = "123456"  ! Cuenta Dummy
					TS.IO.KEYS(9)  = 90
					TS.IO.DEVICE   = 1
					EP.EFT.OK%     = -1
					Call menpag.ignoreTender
					Call recaudot.disableTV
					!U.ECR.Keyed    = -1
					If TS.PROCEDURE = 2 Then TO.XCHGLIM = 0
				Endif
			Endif ! end trx ok
		Endif Else Begin
			TS.IO.MOTORKEY = 0    ! evita que la secuencia aborte el programa
			TS.IO.KEYS(7) = 0     ! borra los datos de medio de pago
			TS.GUIDANCE = 1003    ! muestra el mensaje de secuencia invalida
		Endif
		!
		If TS.IO.KEYS(1) = 82 And 		\! Suspender
				TS.IO.KEYS(2) = 80 And  	\! Enter
				EP.EFT.TRX% Then Begin 		 ! trx EFT activa
			EP.EFT.TRX%    = 0
			TO.XCHGLIM     = EP.XCHGLIM%
			EP.EFT.DETAIL% = 0
		Endif
	Endif
	!
	endUEAPPL14:
End Sub
!
Sub UE.TEF.20 Public
!	If TO.USEROPTS(7) And \ ! user option 7 = Y
!			EP.EFT.ACTIVO% Then Begin
!		If TS.LINETYPE = 2 And \
!				EP.EFT.DETAIL% And \
!				ts.procedure <> 2 Then Begin
!			Call PRINT.EFT.HEADER.DF
!		Endif
!	Endif
End Sub
!
Sub UE.TEF.32 Public
	If TO.USEROPTS(7) And \ ! user option 7 = Y
			EP.EFT.ACTIVO% Then Begin
		If EP.TRX.PEND$ <> "" Then Begin
			EP.TRY.COUNT% = 1
			EP.EFT.OK% = 0 
			While EP.TRX.PEND$ <> "" And \	! There is a pending trx
					EP.TRY.COUNT% <= 3 					! only three times
				EP.APPROV.CODE$  = "" 				!  no ISO8583 message
				EP.CARD.NUMBER$  = "" 				!  no card number
				EP.AUTH.NUMBER$  = "" 				!  no autorization number
				EP.USER.DATA$    = "" 				!  no user data
				!
				If TEFII.FUNCT.ISSYNCBYDEMAND Then \
					Call EP.DISPLAY.AN.ERROR("DIGITE FUNCION DE   RECUPERA. EN DATAFON")
				!
				!----------------------------------------------------------------------------
				!2016-08-22 jsv
				! No se especifica código de función puesto que lo debe tomar de la última
				! operación realizada
				!----------------------------------------------------------------------------
				!EP.APPL$ = "85"
				!----------------------------------------------------------------------------
				!
				EP.ECR.FUNCTION$ = "49"
				EP.TRX.STATUS$   = "1"             !  sin communications 
				EP.AMJ.STATUS$   = "0"             !  sin iniciar transaccion
				!
				Call END.EFT.API(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,\
						EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.PROPINA$,  \
						EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$,       \
						EP.TIPO.VAR$, EP.USER.DATA$)
				!
				If EP.AMJ.STATUS$ <> "2" Then Begin !    evaluate HOST command reponse
					CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
					CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
					TS.IO.MOTORKEY = 0
					TS.USER.RETURN = 99
					EP.TRX.PEND$ = "" 
				Endif Else If EP.TRX.STATUS$ <> "0" Then Begin !    whether communication error found
					CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
					CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
					TS.IO.MOTORKEY = 0
					TS.USER.RETURN = 99
					If EP.TRX.STATUS$ = "O" Then EP.TRX.PEND$ = "" !  Trx no existe
				Endif Else If isApprovCode(EP.APPROV.CODE$) Then Begin
					EP.CARD.BIN$    = EP.USER.DATA$
					TS.USER.RETURN  = 0
					TS.IO.KEYS(7)   = 90 + VAL(LEFT$(EP.TIPO.VAR$,1))
					TS.IO.KEYS(3)   = 78  
					TS.IO.DATA$(3)  = MID$(EP.TIPO.VAR$,2,1)
					TS.IO.MOTORKEY  = TS.IO.KEYS(7)
					SL.TE.TENDTYPE  = VAL(LEFT$(EP.TIPO.VAR$,1))
					SL.TE.TENDVAR   = VAL(MID$(EP.TIPO.VAR$,2,1))
					!
					!TS.IO.DATA$(9)  = EP.CARD.BIN$
					!
					TS.IO.DATA$(9)  = EP.AUTH.NUMBER$
					TS.ACNUM$       = TS.IO.DATA$(9)
					EP.EFT.TRX%     = -1
					EP.EFT.DETAIL%  = -1
					EP.EFT.SMA%     = -1
					!
					If TS.PROCEDURE = 2 Then Begin
						If EP.ECR.FUNCTION$ = "10" Then Begin
							EP.AUTH.NUMBER2$ = EP.AUTH.NUMBER$
							EP.CARD.NUMBER2$ = EP.CARD.NUMBER$
							EP.RECIBO2$ = EP.RECIBO$
						Endif
						TO.XCHGLIM = 0
					Endif
					EP.TRX.PEND$ = ""
				Endif Else Begin
					CALL TRANSLATE.ISO.CODE(EP.APPROV.CODE$,EP.MESSAGE.DESC$)
					CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
					TS.IO.MOTORKEY = 0
					TS.USER.RETURN = 55
					EP.TRX.PEND$ = "" 
				Endif
				EP.TRY.COUNT% = EP.TRY.COUNT% + 1
			Wend
			EP.TRX.PEND$ = "" ! Anula recuperacion
		Endif Else If EP.EFT.OK% Then Begin  !  medio de pago TEF
			EP.EFT.OK% = 0                    ! no inicia de nuevo
			EP.TRX.STATUS$   = "1"            !  sin communications 
			EP.AMJ.STATUS$   = "0"            !  sin iniciar transaccion
			EP.APPROV.CODE$   = ""            !  sin aprobacion
			!
			Call END.EFT.API(EP.APPL$, EP.ECR.FUNCTION$, EP.TRX.STATUS$, EP.AMJ.STATUS$,  \
					EP.ECR.NBR$, EP.ECR.TRANSNUM$, EP.VALOR.EFT$, EP.TAX.EFT$, EP.PROPINA$,    \
					EP.APPROV.CODE$, EP.APPROV.DESC$, EP.CARD.NUMBER$, EP.AUTH.NUMBER$,         \
					EP.TIPO.VAR$, EP.USER.DATA$)
			!
			If EP.AMJ.STATUS$ <> "2" Then Begin !    evaluate HOST command reponse
				CALL TRANSLATE.APPL.CODE(EP.AMJ.STATUS$,EP.MESSAGE.DESC$)
				CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
				TS.IO.MOTORKEY = 0
				TS.USER.RETURN = 99
			Endif Else If EP.TRX.STATUS$ <> "0" Then Begin !    whether communication error found
				CALL TRANSLATE.COMM.CODE(EP.TRX.STATUS$,EP.MESSAGE.DESC$)
				CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
				TS.IO.MOTORKEY = 0
				TS.USER.RETURN = 99
			Endif Else If isApprovCode(EP.APPROV.CODE$) Then Begin
				EP.CARD.BIN$    = EP.USER.DATA$
				TS.USER.RETURN  = 0
				TS.IO.KEYS(7)   = 90 + VAL(LEFT$(EP.TIPO.VAR$,1))
				TS.IO.KEYS(3)   = 78  
				TS.IO.DATA$(3)  = MID$(EP.TIPO.VAR$,2,1)
				TS.IO.MOTORKEY  = TS.IO.KEYS(7)
				SL.TE.TENDTYPE  = VAL(LEFT$(EP.TIPO.VAR$,1))
				SL.TE.TENDVAR   = VAL(MID$(EP.TIPO.VAR$,2,1))
				!
				!TS.IO.DATA$(9)  = EP.CARD.BIN$
				!
				TS.IO.DATA$(9)  = EP.AUTH.NUMBER$
				TS.ACNUM$       = TS.IO.DATA$(9)
				EP.EFT.TRX%     = -1
				EP.EFT.SMA%     = -1
				EP.EFT.DETAIL%  = -1
				!
				If TS.PROCEDURE = 2 Then Begin
					TO.XCHGLIM = 0
					If EP.ECR.FUNCTION$ = "10" Then Begin
						EP.AUTH.NUMBER2$ = EP.AUTH.NUMBER$  
						EP.CARD.NUMBER2$ = EP.CARD.NUMBER$
						EP.RECIBO2$ = EP.RECIBO$
					Endif
					Call PRINT.EFT.DETAIL.DF(EP.VALOR.EFT$, EP.TAX.EFT$)
				Endif
			Endif Else  Begin
				CALL TRANSLATE.ISO.CODE(EP.APPROV.CODE$,EP.MESSAGE.DESC$)
				CALL EP.DISPLAY.AN.ERROR(EP.MESSAGE.DESC$)
				TS.IO.MOTORKEY = 0
				TS.USER.RETURN = 99
			Endif
		Endif     ! end trx ok
	Endif
End Sub
!
Sub UE.TEF.33 Public
	If TO.USEROPTS(7) And \
			EP.EFT.ACTIVO% Then Begin
		Call EPAY.END.RECOVER.EFT
	Endif
End Sub
!
Sub UE.TEF.53 Public
	If TO.USEROPTS(7) And \
			EP.EFT.ACTIVO% Then Begin
		Call EPAY.RECOVER.EFT
	Endif
End Sub
!
Sub UE.TEF.57 Public
	If TO.USEROPTS(7) And \
			EP.EFT.ACTIVO% Then Begin
		Call EPAY.START.RECOVER.EFT
	Endif
End Sub
!
Sub UE.TEF.60 Public
	If TO.USEROPTS(7)  And \ ! user option 7 = Y
			EP.EFT.ACTIVO% Then Begin
		If TS.LINETYPE = 2 And EP.EFT.SMA% Then Begin
			If TS.PROCEDURE <> 2 Then Begin
				EP.EFT.SMA% = 0           ! ya esta recibida por supermarket application
				If EP.TV.CHG% Then Begin
					TO.TENDLIMITS(EP.TV.POS%,0) = EP.MAX.TV.SAV% 
					TO.TENDLIMITS(EP.TV.POS%,1) = EP.CHG.TV.SAV% 
					EP.TV.CHG% = 0
				Endif
				If EP.VERIF.TV.SAV% <> 99 Then Begin
					TO.TENDOPTS(EP.TV.POS%,7) = EP.VERIF.TV.SAV%
				Endif
				TO.USEREXIT(60) = 0
				Call PRINT.EFT.DETAIL.DF(EP.VALOR.EFT$, EP.TAX.EFT$)
				If TS.PROCEDURE < 1 Then EP.EFT.DETAIL% = 0
				If TS.PROCEDURE <> 2 Then TO.USEREXIT(60) = -1
			Endif Else Begin
				EP.EFT.ENTRAD%=EP.EFT.ENTRAD%+1		  
				If EP.AUTH.NUMBER2$ <> "" AND EP.EFT.ENTRAD% = 1 Then Begin
					Call PRINT.EFT.DETAIL.DF2(EP.VALOR.EFT$, EP.TAX.EFT$)
				Endif Else Begin
					If EP.EFT.ENTRAD% = 2 And EP.AUTH.NUMBER2$ <> EP.AUTH.NUMBER$ Then Begin
						Call PRINT.EFT.DETAIL.DF(EP.VALOR.EFT$, EP.TAX.EFT$)    
						EP.EFT.SMA% = 0
					Endif
				Endif
				!
				If EP.EFT.ENTRAD% = 2 Then Begin
					EP.AUTH.NUMBER2$=""
					EP.EFT.SMA% = 0
					EP.EFT.ENTRAD%=0
				Endif
			Endif
		Endif
	Endif
End Sub
!
Function getNetCount Public
	Integer*2 getNetCount
	!
	getNetCount = EP.NETCNT%
End Function
!
Sub launchAutoTefTrx(pAmount%, pApplFunc$) Public
	Integer*4 pAmount%
	String pApplFunc$
	!
	Dim TS.IO.KEYS(10)
	Dim TS.IO.DATA$(10)
	TS.IO.DEVICE = 1
	!
	!Call setRepeatLastSequence( -1 )
	Call TEFII.FUNCT.setApplyLastPayment( -1 )
	launchApplFunc$ = pApplFunc$
	TS.IO.MOTORKEY = EP.initialTefKey%
	TS.IO.KEYS(7) = EP.initialTefKey%
	TS.IO.DATA$(7) = Str$( pAmount% )
End Sub
!
Sub RESET14.TEFII Public
	EP.EFT.OK% = 0
End Sub
!
Function RESET14.isTEFII Public
	Integer*1 RESET14.isTEFII
	!
	If EP.EFT.OK% Then Begin
		RESET14.isTEFII = -1
	Endif Else Begin
		RESET14.isTEFII = 0
	Endif
End Function
!
END
