! Clave de user data TEF Genérico
GENERIC_TEF_UDKEY = 20201101

! Lista de aplicaciones TEF
! Nombre,aplicación,tipoVariedad,entidad,permiteCompra,permiteAnulación,permiteRetiro,pideTipoYNumDoc,requiereConsulta
TENDER_MENU_1 = COMPENSAR      ,09,00,000,1,1,1,0
TENDER_MENU_2 = BILLETERA COBRE,21,XX@36:51@35,462,1,1,1,0
TENDER_MENU_3 = SODEXO         ,46,XX@62,410,1,1,0,0,1

! Medios de pago asociados a convenio y bolsillo
! TENDER_DEF_aplicacion_funcion_convenio_bolsillo = TVEEEEE

! Descriptores para solicitar cuenta
INPUT_ACCOUNT_DESC_09 = LEA TARJ/ENTRE TOKEN
INPUT_ACCOUNT_DESC_20 = @EMPTY
INPUT_ACCOUNT_DESC_21 = DIGITE NRO. CELULAR

! Mensajes para solicitar tipo y número de documento
ID_REQUEST_MESSAGE = Digite nro documento
IDTYPE_REQUEST_MESSAGE = Seleccione tipo doc

! Medios de pago asociados a convenio y bolsillo
! TENDER_DEF_aplicacion_funcion_convenio_bolsillo = TVEEEEE
TENDER_DEF_21_XX_8904800237_XX = 36462
TENDER_DEF_21_51_8904800237_XX = 35462
TENDER_DEF_21_XX_8901020022_XX = 36466
TENDER_DEF_21_51_8901020022_XX = 35466
TENDER_DEF_21_XX_8903030935_XX = 36497
TENDER_DEF_21_51_8903030935_XX = 35497
TENDER_DEF_21_XX_9011830296_XX = 36464
TENDER_DEF_21_51_9011830296_XX = 26464
TENDER_DEF_21_XX_8914800001_XX = 364000
TENDER_DEF_21_51_8914800001_XX = 264000

! Agente externo para complementar información de medios de pago
PARAM_AGENT_CLASSNAME = com.olimpica.tenders.controller.RemoteTenderManager

! Configuración de medios de pago definidos centralmente
SOCKET_IP = 192.168.0.210
SOCKET_PORT = 5545
RETRIES_LIMIT = 3
DEFAULT_TIME_BETWEEN_TRIES = 1000
