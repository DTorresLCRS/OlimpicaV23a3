%ENVIRON T
!
!-------------------------------------------------------------------------------------------------------
! Acción 0 = Control consulta de precio: No permite una consulta de precio para este artículo
! Acción 1 = Control tarjeta plata: Sólo permite registrar este articulo si se ha ingresado tarjeta plata
! Acción 2 = Control Franqueo a Productos:  No franqueo de productos relacionados
! Acción 3 = Control lectura: Sólo permite registrar este artículo si fue leido por escáner
! Acción 4 = Control pesable: Le remueve el atributo de pesable a los artículos seleccionados
! Acción 5 = Productos cuya venta está sujeta a disponibilidad por existencia
! Acción 6 = Productos de operaciones terceros
!-------------------------------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------------------------------
! Variables de SMA
!-------------------------------------------------------------------------------------------------------
String Global			\
	TS.IO.DATA$(1),	\
	IR.DEPARTME$,		\
	ue.clf.cliente$,\
	IR.SALEPRIC$, 	\
	ITEMCODE$,			\
	SL.STR.ENTRY$
!
Integer*1 Global	\
	IR.INDICAT0
Integer*2 Global	\
	TS.IO.KEYS(1),	\
	TS.IO.MOTORKEY,	\
	TS.PROCEDURE,		\
	TS.GUIDANCE,		\
	TS.IO.DEVICE,		\
	TS.TEMP1I2
Integer Global		\
	FAST.EXIT
Integer*1 				\
	consultaPrecio%
!
!-------------------------------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------------------------------
! Variables del módulo
!-------------------------------------------------------------------------------------------------------
String plusToControl$(2), deptToControl$(2)
Integer*2 paramPlu%(2), paramDept%(2)
Integer*2 plusToControl%(1), deptToControl%(1), maxAction%, maxPlu%, maxDept%, realDevice%
Integer*2 currentIndex%
Integer*1 transactionType% 		! Tipo de transacción: -1=Indefinida 0=Venta regular 1=Operación terceros
Integer*1 thirdPartyInProg% 	! Indica que está en proceso el registro de un producto de terceros
!-------------------------------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------------------------------
! Rutinas externas de usuario
!-------------------------------------------------------------------------------------------------------
Sub EP.DISPLAY.AN.ERROR(UE.DISP.MESSAGE$) External
	String UE.DISP.MESSAGE$
End Sub
!
Sub EP.LINE.PRINT(LINEA$,ESTACION%) External
	String LINEA$
	Integer*2 ESTACION%
End Sub
!
Sub CTRINV.validateInventory(pFamily%, pReturn%, pMessage$) External
	Integer*2 pFamily%, pReturn%
	String pMessage$
End Sub
!
Sub CTRINV.validateRegularItem(pReturn%, pMessage$) External
	Integer*2 pReturn%
	String pMessage$
End Sub
!
Sub printDebug(pMsg$) External
	String pMsg$
End Sub
!
Sub getNextField(pData$, pLastIndex%, pSeparator$, pNextField$) External
	String pNextField$, pData$, pSeparator$
	Integer*2 pLastIndex%
End Sub
!
Function readEamitemrRecursive(itemToSearch$) External
	String readEamitemrRecursive, itemToSearch$
End Function
!
Sub DISC.notifyThirdPartyTrx(pFlag%) External
	Integer*1 pFlag%
End Sub
!
!-------------------------------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------------------------------
! Rutinas del módulo
!-------------------------------------------------------------------------------------------------------
!
Function isCustomerTrx
	Integer*1 isCustomerTrx
	!
	If ue.clf.cliente$ <> "" Then \
		isCustomerTrx = -1 \
	Else \
		isCustomerTrx = 0
End Function
!
Sub lTrim(pStr$, pChar%)
	String pStr$
	Integer*2 pChar%
	!
	While Asc(Left$(pStr$, 1)) = pChar%
		pStr$ = Right$(pStr$, Len(pStr$) - 1)
	Wend	
End Sub
!
Sub rTrim(pStr$, pChar%)
	String pStr$
	Integer*2 pChar%
	!
	While Asc(Right$(pStr$, 1)) = pChar%
		pStr$ = Left$(pStr$, Len(pStr$) - 1)
	Wend
End Sub
!
Sub agregarDept(pDept$)
	String pDept$, tmpParam$, tmpParam1$
	Integer*2 tmpAction%, tmpIndex%
	!
	tmpParam1$ = "0"
	Call rTrim(pDept$, 32)
	tmpIndex% = Match(",", pDept$, 1)
	If tmpIndex% > 0 Then Begin
		tmpParam$ = Right$(pDept$, Len(pDept$) - tmpIndex%)
		pDept$ = Left$(pDept$, tmpIndex% - 1)
		tmpIndex% = Match(",", tmpParam$, 1)
		If tmpIndex% > 0 Then Begin
			tmpParam1$ = Right$(tmpParam$, Len(tmpParam$) - tmpIndex%)
			tmpParam$ = Left$(tmpParam$, tmpIndex% - 1)
		Endif
		tmpAction% = Int%(Val(tmpParam$))
		If tmpAction% < 0 Then \
			tmpAction% = 0 \
		Else If tmpAction% > maxAction% Then \
			tmpAction% = maxAction%
	Endif Else Begin
		tmpAction% = 0
	Endif
	!
	Call rTrim(pDept$, 32)
	Call lTrim(pDept$, 32)
	Call lTrim(pDept$, 48)
	!
	If deptToControl%(tmpAction%) < maxDept% Then Begin
		deptToControl%(tmpAction%) = deptToControl%(tmpAction%) + 1
		deptToControl$(deptToControl%(tmpAction%), tmpAction%) = pDept$
		paramDept%(deptToControl%(tmpAction%), tmpAction%) = Int%(Val(tmpParam1$))
	Endif
End Sub
!
Sub agregarPlu(pPlu$)
	String pPlu$, tmpParam$, tmpParam1$
	Integer*2 tmpAction%, tmpIndex%
	!
	Call rTrim(pPlu$, 32)
	tmpIndex% = Match(",", pPlu$, 1)
	If tmpIndex% > 0 Then Begin
		tmpParam$ = Right$(pPlu$, Len(pPlu$) - tmpIndex%)
		pPlu$ = Left$(pPlu$, tmpIndex% - 1)
		tmpIndex% = Match(",", tmpParam$, 1)
		If tmpIndex% > 0 Then Begin
			tmpParam1$ = Right$(tmpParam$, Len(tmpParam$) - tmpIndex%)
			tmpParam$ = Left$(tmpParam$, tmpIndex% - 1)
		Endif
		tmpAction% = Int%(Val(tmpParam$))
		If tmpAction% < 0 Then \
			tmpAction% = 0 \
		Else If tmpAction% > maxAction% Then \
			tmpAction% = maxAction%
	Endif Else Begin
		tmpAction% = 0
	Endif
	!
	Call rTrim(pPlu$, 32)
	Call lTrim(pPlu$, 32)
	Call lTrim(pPlu$, 48)
	!
	If plusToControl%(tmpAction%) < maxPlu% Then Begin
		plusToControl%(tmpAction%) = plusToControl%(tmpAction%) + 1
		plusToControl$(plusToControl%(tmpAction%), tmpAction%) = pPlu$
		paramPlu%(plusToControl%(tmpAction%), tmpAction%) = Int%(Val(tmpParam1$))
	Endif
End Sub

Sub pluControl.leerPlus
	Integer*2 paramSession%,tmpIndex1%,tmpIndex2%
	Integer*1 fileOpen%,endParam%
	String tmpLine$
	!
	paramSession% = 95
	fileOpen% = 0
	endParam% = 0
	!
	on error goto errParam
	!
	open "R::ADX_UDT1:PLUCTR.DAT" as paramSession% nowrite nodel
	fileOpen% = -1
	!
	If End # paramSession% Then endParam
	!
	While Not endParam%
		READ #paramSession% ; LINE tmpLine$
		If tmpLine$ <> "" Then Begin
			If Left$(tmpLine$, 1) <> "#" And Left$(tmpLine$, 1) <> "!" Then Begin
				If Ucase$(Left$(tmpLine$,3)) = "DEP" Then \
					Call agregarDept(Right$(tmpLine$,Len(tmpLine$) - 3)) \
				Else \
					Call agregarPlu(tmpLine$)
			Endif
		Endif
	Wend
	!
	Goto endParam
	!
	errParam:
		Resume endParam
	endParam:
	If fileOpen% Then Close paramSession%
End Sub
!
Function isPluToControl(plu$, pAction%)
	Integer*1 isPluToControl,pluFound%
	String plu$,plu1$
	Integer*2 pAction%, pluCounter%
	!
	Call printDebug("isPluToControl(" + plu$ + "," + Str$(pAction%) + ")")
	plu1$ = Str$(Val(plu$))
	If Len(plu1$) > 12 Then plu1$ = Left$(plu1$,12)
	!
	pluCounter% = 1
	pluFound% = 0
	While pluCounter% <= plusToControl%(pAction%) And pluFound% = 0
		If plusToControl$(pluCounter%, pAction%) = plu1$ Then Begin
			pluFound% = -1
			currentIndex% = pluCounter%
		Endif
		pluCounter% = pluCounter% + 1
	Wend
	Call printDebug("Ans=" + Str$(pluFound%))
	isPluToControl = pluFound%
End Function
!
Function isPluToVoidFrank(plu$) Public
	Integer*1 isPluToVoidFrank
	String plu$
	!
	isPluToVoidFrank = isPluToControl(plu$, 2)
End Function
!
Function isDeptToControl(pDept$, pAction%)
	Integer*1 isDeptToControl,deptFound%
	String pDept$,dept1$
	Integer*2 pAction%, deptCounter%
	!
	dept1$ = Str$(Val(pDept$))
	!
	deptCounter% = 1
	deptFound% = 0
	While deptCounter% <= deptToControl%(pAction%) And deptFound% = 0
		If deptToControl$(deptCounter%, pAction%) = dept1$ Then Begin
			deptFound% = -1
			currentIndex% = deptCounter%
		Endif
		deptCounter% = deptCounter% + 1
	Wend
	isDeptToControl = deptFound%
End Function
!
Function isConsultaPrecio
	Integer*1 isConsultaPrecio
	!
	If TS.IO.KEYS(5) = 74 Then \
		isConsultaPrecio = -1 \
	Else \
		isConsultaPrecio = 0
End Function
!
Function pluControl.getInvFamily(pDepartment$, pItemCode$) Public
	Integer*2 pluControl.getInvFamily, tmpAnswer%
	String pDepartment$, pItemCode$
	!
	tmpAnswer% = 0
	If isDeptToControl(pDepartment$, 5) Then Begin
		tmpAnswer% = paramDept%(currentIndex%, 5)
	Endif Else If isPluToControl(pItemCode$, 5) Then Begin
		tmpAnswer% = paramPlu%(currentIndex%, 5)
	Endif
	pluControl.getInvFamily = tmpAnswer%
End Function
!
Sub recoverTransactionType(pData$)
	String pData$, tmpField$, tmpItemCode$, tmpData$, tmpDepartment$
	Integer*2 tmpIndex%
	Integer*1 tmpThirdPartyTrx%
	!
	If transactionType% = -1 Then Begin
		tmpIndex% = Match(":", pData$, 1)
		Call getNextField(pData$, tmpIndex%, ":", tmpField$) ! itemcode
		tmpItemCode$ = Unpack$( tmpField$ )
		tmpData$ = readEamitemrRecursive( tmpItemCode$ )
		If Len(tmpData$) > 0 Then Begin
			tmpItemCode$ = Left$(tmpData$, 6)
		Endif
		Call getNextField(pData$, tmpIndex%, ":", tmpField$) ! xprice
		Call getNextField(pData$, tmpIndex%, ":", tmpField$) ! department
		tmpDepartment$ = Unpack$( tmpField$ )
		!
		If isDeptToControl(tmpDepartment$, 6) Then Begin
			tmpThirdPartyTrx% = -1
		Endif Else If isPluToControl(tmpItemCode$, 6) Then Begin
			tmpThirdPartyTrx% = -1
		Endif Else Begin
			tmpThirdPartyTrx% = 0
		Endif
		!
		If tmpThirdPartyTrx% Then Begin
			transactionType% = 1
		Endif Else Begin
			transactionType% = 0
		Endif
	Endif	
End Sub
!
Function UEPLUCTR.isThirdPartyTrx Public
	Integer*1 UEPLUCTR.isThirdPartyTrx
	!
	If transactionType% = 1 Then Begin
		UEPLUCTR.isThirdPartyTrx = -1
	Endif Else Begin
		UEPLUCTR.isThirdPartyTrx = 0
	Endif
End Function
!
!-------------------------------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------------------------------
! User exits
!-------------------------------------------------------------------------------------------------------
!
Sub pluControl.02 Public
	transactionType% = -1
End Sub
!
Sub pluControl.05 Public
	transactionType% = -1
	thirdPartyInProg% = -1
End Sub
!
Sub pluControl.07 Public
	Call EP.LINE.PRINT("UEPLUCTR v.2.0 Feb-2022", 2100h)
	maxAction% = 6
	maxPlu% = 1500
	maxDept% = 100
	Dim plusToControl%(maxAction%)
	Dim deptToControl%(maxAction%)
	Dim plusToControl$(maxPlu%, maxAction%)
	Dim paramPlu%(maxPlu%, maxAction%)
	Dim deptToControl$(maxDept%, maxAction%)
	Dim paramDept%(maxDept%, maxAction%)
	Call pluControl.leerPlus
	transactionType% = -1
	thirdPartyInProg% = -1
End Sub
!
Sub pluControl.08 Public
	Integer*1 tmpRefused%, tmpThirdPartyTrx%
	Integer*2 tmpCtrInvFamily%, tmpReturn%
	String tmpMessage$
	!
	tmpRefused% = 0
	thirdPartyInProg% = -1
	!
	Call printDebug("pluControl.08 itemcode=" + Unpack$(ITEMCODE$))
	!
	If TS.PROCEDURE < 1 Then 				\ ! procedimiento de ventas
	Begin
		If consultaPrecio% = -1 Then Begin
			If isDeptToControl(UNPACK$(IR.DEPARTME$), 0) Then Begin
				consultaPrecio% = 0
				DIM TS.IO.KEYS(10)
				DIM TS.IO.DATA$(10)
				TS.GUIDANCE = 1003
				TS.IO.MOTORKEY = 0
				FAST.EXIT = -1
				tmpRefused% = -1
			Endif Else Begin
				If IR.INDICAT0 And 40H Then Begin
					If TS.IO.KEYS(6) = 0 Then Begin
						TS.IO.KEYS(6) = 72
						TS.IO.DATA$(6) = "1000"
					Endif
				Endif
			Endif
		Endif Else If Not isCustomerTrx Then Begin
			If isDeptToControl(UNPACK$(IR.DEPARTME$), 1) Then Begin
				Call ep.display.an.error("Articulo requiere   tarjeta plata")
				DIM TS.IO.KEYS(10)
				DIM TS.IO.DATA$(10)
				TS.IO.MOTORKEY = 0
				FAST.EXIT = -1
				tmpRefused% = -1
			Endif Else If isPluToControl(Unpack$(ITEMCODE$), 1) Then Begin
				Call ep.display.an.error("Articulo requiere   tarjeta plata")
				DIM TS.IO.KEYS(10)
				DIM TS.IO.DATA$(10)
				TS.IO.MOTORKEY = 0
				FAST.EXIT = -1
				tmpRefused% = -1
			Endif
		Endif
		!
		If tmpRefused% = 0 And realDevice% <> 3 Then Begin
			If isDeptToControl(UNPACK$(IR.DEPARTME$), 3) Then Begin
				Call ep.display.an.error("Ingreso de articulo solo por escaner")
				DIM TS.IO.KEYS(10)
				DIM TS.IO.DATA$(10)
				TS.IO.MOTORKEY = 0
				tmpRefused% = -1
			Endif Else If isPluToControl(Unpack$(ITEMCODE$), 3) Then Begin
				Call ep.display.an.error("Ingreso de articulo solo por escaner")
				DIM TS.IO.KEYS(10)
				DIM TS.IO.DATA$(10)
				TS.IO.MOTORKEY = 0
				tmpRefused% = -1
			Endif
		Endif
		!
		! Remoción de atributo de pesable
		If tmpRefused% = 0 And (IR.INDICAT0 And 40H) <> 0 Then Begin
			If isDeptToControl(UNPACK$(IR.DEPARTME$), 4) Then Begin
				tmpRefused% = -1
			Endif Else If isPluToControl(Unpack$(ITEMCODE$), 4) Then Begin
				tmpRefused% = -1
			Endif
			If tmpRefused% Then Begin
				Call printDebug("plucontrol.IR.SALEPRIC$="+Unpack$(IR.SALEPRIC$) + " IR.INDICAT0="+Str$(IR.INDICAT0))
				IR.INDICAT0 = IR.INDICAT0 Xor 40H
				If (IR.INDICAT0 And 10H) <> 0 Then Begin
					IR.INDICAT0 = IR.INDICAT0 Xor 10H
				Endif
				If Val(Unpack$(IR.SALEPRIC$)) = 0 Then Begin
					IR.SALEPRIC$ = Pack$("0000000001")
				Endif
				tmpRefused% = 0
			Endif
		Endif
		!
		! Control de venta sujeta a disponibilidad por inventario
		If 														\
				tmpRefused% = 0 					\! No ha sido rechazada la secuencia
		Then Begin
			tmpCtrInvFamily% = pluControl.getInvFamily(UNPACK$(IR.DEPARTME$), Unpack$(ITEMCODE$))
			tmpReturn% = 0
			If tmpCtrInvFamily% > 0 Then Begin
				Call CTRINV.validateInventory(tmpCtrInvFamily%, tmpReturn%, tmpMessage$)
			Endif Else Begin
				! Artículo de venta regular
				If 														\!
						TS.IO.KEYS(1) <> 70 And 	\! No tecla void
						TS.IO.KEYS(8) <> 67 			\! No tecla refund
				Then Begin
					Call CTRINV.validateRegularItem(tmpReturn%, tmpMessage$)
				Endif Else Begin
					tmpReturn% = -1
				Endif
			Endif
			If Not tmpReturn% Then Begin
				If Len(tmpMessage$) > 0 Then Begin
					Call ep.display.an.error(tmpMessage$)
				Endif
				Dim TS.IO.KEYS(10)
				Dim TS.IO.DATA$(10)
				TS.IO.MOTORKEY = 0
				FAST.EXIT = -1
				tmpRefused% = -1
			Endif			
		Endif
		!
		If tmpRefused% = 0 Then Begin
			tmpMessage$ = ""
			If isDeptToControl(UNPACK$(IR.DEPARTME$), 6) Then Begin
				tmpThirdPartyTrx% = -1
			Endif Else If isPluToControl(Unpack$(ITEMCODE$), 6) Then Begin
				tmpThirdPartyTrx% = -1
			Endif Else Begin
				tmpThirdPartyTrx% = 0
			Endif
			If transactionType% = -1 Then Begin
				If tmpThirdPartyTrx% Then Begin
					If isCustomerTrx Then Begin
						tmpRefused% = -1
						tmpMessage$ = "ARTICULO TERCEROS   NO PERMITIDO CON TP"
					Endif Else Begin
						!----------------------------------------------------------------
						! 2023-03-18 jsv
						! Se espera hasta la ue 67 para marcar la transacción y notificar
						!----------------------------------------------------------------
						!transactionType% = 1
						!! Notifica que se ha marcado la transacción como 
						!! operación de terceros
						!Call DISC.notifyThirdPartyTrx(2)
						!
						thirdPartyInProg% = 1
						!
						!----------------------------------------------------------------
					Endif
				Endif Else Begin
					!----------------------------------------------------------------
					! 2023-03-18 jsv
					! Se espera hasta la ue 67 para marcar la transacción
					!----------------------------------------------------------------
					!transactionType% = 0
					!
					thirdPartyInProg% = 0
					!
					!----------------------------------------------------------------
				Endif
			Endif Else If transactionType% = 0 Then Begin
				If tmpThirdPartyTrx% Then Begin
					tmpRefused% = -1
					tmpMessage$ = "ARTICULO TERCEROS   NO PERMITIDO"
				Endif
			Endif Else If transactionType% = 1 Then Begin
				If Not tmpThirdPartyTrx% Then Begin
					tmpRefused% = -1
					tmpMessage$ = "ARTICULO VTA REGULARNO PERMITIDO"
				Endif
			Endif
			If tmpRefused% Then Begin
				Call ep.display.an.error(tmpMessage$)
				Dim TS.IO.KEYS(10)
				Dim TS.IO.DATA$(10)
				TS.IO.MOTORKEY = 0
				FAST.EXIT = -1
			Endif
		Endif
	Endif
End Sub
!
Sub pluControl.14 Public
	Integer*1 tmpRefused%
	!
	tmpRefused% = 0
	If TS.PROCEDURE < 1 And 				\ ! procedimiento de ventas
			TS.IO.KEYS(2) = 80  And 		\ ! entrada PLU
			TS.IO.DATA$(2) <> "" Then 	\ ! algun PLU entrado
	Begin
		realDevice% = TS.IO.DEVICE
		consultaPrecio% = isConsultaPrecio
		If consultaPrecio% = -1 Then Begin
			If isPluToControl(TS.IO.DATA$(2), 0) Then Begin
				DIM TS.IO.KEYS(10)
				DIM TS.IO.DATA$(10)
				TS.GUIDANCE = 1003
				TS.IO.MOTORKEY = 0
				tmpRefused% = -1
			Endif
		Endif Else If Not isCustomerTrx Then Begin
			If isPluToControl(TS.IO.DATA$(2), 1) Then Begin
				Call ep.display.an.error("Articulo requiere   tarjeta plata")
				DIM TS.IO.KEYS(10)
				DIM TS.IO.DATA$(10)
				TS.IO.MOTORKEY = 0
				tmpRefused% = -1
			Endif
		Endif
		!
		If tmpRefused% = 0 And realDevice% <> 3 Then Begin
			If isPluToControl(TS.IO.DATA$(2), 3) Then Begin
				Call ep.display.an.error("Ingreso de articulo solo por escaner")
				DIM TS.IO.KEYS(10)
				DIM TS.IO.DATA$(10)
				TS.IO.MOTORKEY = 0
				tmpRefused% = -1				
			Endif
		Endif
		!
	Endif
End Sub
!
Sub pluControl.53 Public
	If TS.TEMP1I2 = 1 Then Begin
		Call recoverTransactionType(SL.STR.ENTRY$)
	Endif
End Sub
!
Sub pluControl.67 Public
	If thirdPartyInProg% > -1 Then Begin
		transactionType% = thirdPartyInProg%
		thirdPartyInProg% = -1
		If transactionType% = 1 Then Begin
			! Notifica que se ha marcado la transacción como 
			! operación de terceros
			Call DISC.notifyThirdPartyTrx(2)
		Endif
	Endif
End Sub
!
!-------------------------------------------------------------------------------------------------------
