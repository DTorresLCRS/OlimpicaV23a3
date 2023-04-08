! Rutina escrita para cancelar eventuales procesos
! que se hayan iniciado en la user exit 14
!
!
%ENVIRON T
!
SUB RESET14.TCRO EXTERNAL
END SUB
!
Function RESET14.isTCRO External
	Integer*1 RESET14.isTCRO
End Function
!
SUB RESET14.COMPENSAR EXTERNAL
END SUB
!
Function RESET14.isCOMPENSAR External
	Integer*1 RESET14.isCOMPENSAR
End Function
!
SUB RESET14.TEFII EXTERNAL
END SUB
!
Function RESET14.isTEFII External
	Integer*1 RESET14.isTEFII
End Function
!
SUB RESET14 PUBLIC
	CALL RESET14.TCRO
	CALL RESET14.COMPENSAR
	CALL RESET14.TEFII
END SUB
!
!-------------------------------------------------------------
! Función para determinar si está en curso una operación TEF
! de los siguientes tipos:
! * Tarjeta monedero
! * Tarjeta compensar
! * Datáfonos
!-------------------------------------------------------------
Function RESET14.isTef Public
	Integer*1 RESET14.isTef
	!
	If 																\!
			RESET14.isTCRO <> 0 Or 				\! Tarjeta monedero
			RESET14.isCOMPENSAR <> 0 Or 	\! Cajas de compensación o derivados
			RESET14.isTEFII <> 0 					\! Datáfonos
	Then Begin
		RESET14.isTef = -1
	Endif Else Begin
		RESET14.isTef = 0
	Endif
End Function
!
