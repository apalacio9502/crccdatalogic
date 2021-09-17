#' Actualizar control de modificación datos miembros
#'
#' Esta función actualiza el control de modificación datos miembros
#' @param datos clase dataframe. Datos miembros
#' @export

actualizar_control_modificacion_datos_miembros <- function(datos){

  # Se crea el data.frame resultado
  resultado <- data.frame(TABLA="ADM_MIEMBROS",
                            FECHA_HORA_MODIFICACION=max(datos$FECHA_HORA_CREACION,
                                                        datos$FECHA_HORA_MODIFICACION,na.rm = TRUE),
                            FILAS=nrow(datos))

  # Se retorna el resultado de la funcion
  return(resultado)
}

#' Registrar nuevo miembro
#'
#' Esta función registra un nuevo miembro en la tabla adm_miembros
#' @param conexion clase formal. Conexión base de datos
#' @param datos clase data.frame. Los datos del miembro
#' @export

registrar_nuevo_miembro <- function(conexion,datos){

  #Se escribe el nuevo registro 'miembro' en la tabla ADM_MIEMBROS
  resultado_query <- dbWriteTable(conexion,name = "ADM_MIEMBROS",datos,append=TRUE,row.names=FALSE)
  #Se almacena el estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  #Se verifica el estado de la operacion
  if(resultado$ESTADO==TRUE){
    #Se lee el data frame ADM_MIEMBROS de AWS
    dataframe <- dbReadTable(conexion,"ADM_MIEMBROS")
    #Se almacena el data frame ADM_MIEMBROS en la variable resultado
    resultado <- list(ESTADO=resultado_query,DATAFRAME=dataframe)
  }
  #Se retorna el resultado de la funcion
  return(resultado)

}

#' Registrar modificación miembro
#'
#' Esta función registra la nueva información del miembro modificado en la tabla ADM_MIEMBROS
#' @param conexion clase formal. Conexión base de datos
#' @param fecha_hora_evento clase datetime
#' @param usuario clase character
#' @param correo clase character
#' @param perfil clase character
#' @export

registrar_modificacion_miembro <- function(conexion,fecha_hora_evento,id,nombre,nombre_abreviacion,nit,tipo,clase,estado,segmentos){

  # Se crea el query registrar_modificacion_miembro
  query <- glue("UPDATE ADM_MIEMBROS SET NOMBRE='{nombre}', NOMBRE_ABREVIACION='{nombre_abreviacion}',
                  NIT='{nit}', TIPO='{tipo}', CLASE='{clase}', ESTADO={estado}, SEGMENTOS='{segmentos}',
                  FECHA_HORA_MODIFICACION='{fecha_hora_evento}' WHERE ID ='{id}'")

  # Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  # Se almacena el estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  # Se verifica el estado de la operacion
  if(resultado$ESTADO==TRUE){
    # Se lee el data frame ADM_MIEMBROS de AWS
    dataframe <- dbReadTable(conexion,"ADM_MIEMBROS")
    # Se almacena el data frame ADM_MIEMBROS en la variable resultado
    resultado <- list(ESTADO=resultado_query,DATAFRAME=dataframe)
  }
  #Se retorna el resultado de la funcion
  return(resultado)
}

#' Registrar eliminación miembro
#'
#' Esta función registra registra la eliminación del miembro (elimina al miembro) en la tabla adm_miembros
#' @param conexion clase formal. Conexión base de datos
#' @param id clase character
#' @export

registrar_eliminacion_miembro <- function(conexion,id){

  # Se crea el query registrar_eliminacion_miembro
  query <- glue("DELETE FROM ADM_MIEMBROS WHERE ID ='{id}'")

  # Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  # Se almacena estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  # Se verifica el estado de la operacion
  if(resultado$ESTADO==TRUE){
    # Se lee el data frame ADM_MIEMBROS de AWS
    dataframe <- dbReadTable(conexion,"ADM_MIEMBROS")
    # Se almacena el data frame ADM_MIEMBROS en la variable resultado
    resultado <- list(ESTADO=resultado_query,DATAFRAME=dataframe)
  }
  #Se retorna el resultado de la funcion
  return(resultado)

}


