#' Registrar nueva cookie
#'
#' Esta función registra la nueva cookie de un usuario en la tabla ''
#' @param conexion clase formal. Conexión base de datos
#' @param datos clase data.frame. Los datos de la nueva cookie
#' @export

registrar_cookie_usuario <- function(conexion,datos){

  # Se escribe el nuevo registro 'cookie' en la base de datos cookies en AWS
  resultado_query <- dbWriteTable(conexion,name = "ADM_COOKIES",datos,append=TRUE,row.names=FALSE)
  # Se almacena el estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  # Se retorna el resultado de la funcion
  return(resultado)

}
