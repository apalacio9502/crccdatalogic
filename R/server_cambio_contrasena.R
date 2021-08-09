#' Registrar nueva contraseña
#'
#' Esta función registra la nueva contraseña de un usuario en la tabla ''
#' @param conexion clase formal. Conexión base de datos
#' @param fecha_hora_evento clase datetime
#' @param usuario clase character
#' @param contrasena clase character
#' @export

actualizar_contrasena_usuario <- function(conexion,fecha_hora_evento,usuario,contrasena){

  #Se crea el query MOD_CC_actualizar_contrasena
  query <- glue("UPDATE ADM_USUARIOS SET CONTRASENA='{contrasena}',
                  FECHA_HORA_MODIFICACION_CONTRASENA='{fecha_hora_evento}',
                  MODIFICAR_CONTRASENA='FALSE'
                  WHERE USUARIO ='{usuario}'")

  #Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  #Se almacena el estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  #Se retorna el resultado de la funcion
  return(resultado)
}
