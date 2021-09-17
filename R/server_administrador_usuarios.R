#' Actualizar control de modificación datos usuarios
#'
#' Esta función actualiza el control de modificación datos usuarios
#' @param datos clase dataframe. Datos usuarios
#' @export

actualizar_control_modificacion_datos_usuarios <- function(datos){

  # Se crea el data.frame resultado
  resultado <- data.frame(TABLA="ADM_USUARIOS",
                            FECHA_HORA_MODIFICACION=max(datos$FECHA_HORA_MODIFICACION_CONTRASENA,
                                                        datos$FECHA_HORA_ULTIMA_CONEXION,
                                                        datos$FECHA_HORA_ULTIMA_DESCONEXION,
                                                        datos$FECHA_HORA_ULTIMA_CONEXION_FALLIDA,
                                                        datos$FECHA_HORA_CREACION,
                                                        datos$FECHA_HORA_MODIFICACION,na.rm = TRUE),
                            FILAS=nrow(datos))

  # Se retorna el resultado de la funcion
  return(resultado)
}

#' Registrar nuevo usuario
#'
#' Esta función registra un nuevo usuario en la tabla adm_usuarios
#' @param conexion clase formal. Conexión base de datos
#' @param datos clase data.frame. Los datos del usuario
#' @export

registrar_nuevo_usuario <- function(conexion,datos){

  #Se escribe el nuevo registro 'usuario' en la tabla ADM_USUARIOS
  resultado_query <- dbWriteTable(conexion,name = "ADM_USUARIOS",datos,append=TRUE,row.names=FALSE)
  #Se almacena el estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  #Se verifica el estado de la operacion
  if(resultado$ESTADO==TRUE){
    #Se lee el data frame ADM_USUARIOS de AWS
    dataframe <- dbReadTable(conexion,"ADM_USUARIOS")
    #Se almacena el data frame ADM_USUARIOS en la variable resultado
    resultado <- list(ESTADO=resultado_query,DATAFRAME=dataframe)
  }
  #Se retorna el resultado de la funcion
  return(resultado)

}

#' Registrar modificación usuario
#'
#' Esta función registra la nueva información del usuario modificado en la tabla adm_usuarios
#' @param conexion clase formal. Conexión base de datos
#' @param fecha_hora_evento clase datetime
#' @param usuario clase character
#' @param correo clase character
#' @param perfil clase character
#' @export

registrar_modificacion_usuario <- function(conexion,fecha_hora_evento,usuario,correo,perfil){

  # Se crea el query registrar_modificacion_usuario
  query <- glue("UPDATE ADM_USUARIOS SET CORREO='{correo}',PERFIL='{perfil}',
                  FECHA_HORA_MODIFICACION='{fecha_hora_evento}' WHERE USUARIO ='{usuario}'")

  # Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  # Se almacena el estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  # Se verifica el estado de la operacion
  if(resultado$ESTADO==TRUE){
    # Se lee el data frame ADM_USUARIOS de AWS
    dataframe <- dbReadTable(conexion,"ADM_USUARIOS")
    # Se almacena el data frame ADM_USUARIOS en la variable resultado
    resultado <- list(ESTADO=resultado_query,DATAFRAME=dataframe)
  }
  #Se retorna el resultado de la funcion
  return(resultado)
}

#' Registrar nueva contraseña usuario
#'
#' Esta función registra la nueva contraseña del usuario modificado en la tabla adm_usuarios
#' @param conexion clase formal. Conexión base de datos
#' @param fecha_hora_evento clase datetime
#' @param usuario clase character
#' @param contrasena clase character
#' @export

registrar_nueva_contrasena_usuario <- function(conexion,fecha_hora_evento,usuario,contrasena){

  #Se crea el query registrar_nueva_contrasena_usuario
  query <- glue("UPDATE ADM_USUARIOS SET CONTRASENA='{encriptar_contrasena(contrasena)}',
                  MODIFICAR_CONTRASENA=1, FECHA_HORA_MODIFICACION='{fecha_hora_evento}'
                  WHERE USUARIO ='{usuario}'")

  #Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  #Se almacena el estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  #Se verifica el estado de la operacion
  if(resultado$ESTADO==TRUE){
    #Se lee el data frame ADM_USUARIOS de AWS
    dataframe <- dbReadTable(conexion,"ADM_USUARIOS")
    #Se almacena el data frame ADM_USUARIOS en la variable resultado
    resultado <- list(ESTADO=resultado_query,DATAFRAME=dataframe)
  }
  #Se retorna el resultado de la funcion
  return(resultado)
}

#' Registrar desbloqueo usuario
#'
#' Esta función registra el desbloqueo del usuario modificado en la tabla adm_usuarios
#' @param conexion clase formal. Conexión base de datos
#' @param fecha_hora_evento clase datetime
#' @param usuario clase character
#' @export

registrar_desbloqueo_usuario <- function(conexion,fecha_hora_evento,usuario){

  #Se crea el query registrar_desbloqueo_usuario
  query <- glue("UPDATE ADM_USUARIOS SET ESTADO=1, INTENTOS_FALLIDOS_CONEXION=0,
                  FECHA_HORA_MODIFICACION='{fecha_hora_evento}' WHERE Usuario ='{usuario}'")

  #Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  #Se almacena estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  #Se verifica el estado de la operacion
  if(resultado$ESTADO==TRUE){
    #Se lee el data frame ADM_USUARIOS de AWS
    dataframe <- dbReadTable(conexion,"ADM_USUARIOS")
    #Se almacena el data frame ADM_USUARIOS en la variable resultado
    resultado <- list(ESTADO=resultado_query,DATAFRAME=dataframe)
  }
  #Se retorna el resultado de la funcion
  return(resultado)

}

#' Registrar bloqueo usuario
#'
#' Esta función registra el bloqueo del usuario modificado en la tabla adm_usuarios
#' @param conexion clase formal. Conexión base de datos
#' @param fecha_hora_evento clase datetime
#' @param usuario clase character
#' @export

registrar_bloqueo_usuario <- function(conexion,fecha_hora_evento,usuario){

  # Se crea el query registrar_bloqueo_usuario
  query <- glue("UPDATE ADM_USUARIOS SET ESTADO=0,
                  FECHA_HORA_MODIFICACION='{fecha_hora_evento}' WHERE USUARIO ='{usuario}'")

  # Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  # Se almacena estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  # Se verifica el estado de la operacion
  if(resultado$ESTADO==TRUE){
    # Se lee el data frame ADM_USUARIOS de AWS
    dataframe <- dbReadTable(conexion,"ADM_USUARIOS")
    # Se almacena el data frame ADM_USUARIOS en la variable resultado
    resultado <- list(ESTADO=resultado_query,DATAFRAME=dataframe)
  }
  #Se retorna el resultado de la funcion
  return(resultado)

}

#' Registrar eliminación usuario
#'
#' Esta función registra registra la eliminación del usuario (elimina al usuario) en la tabla adm_usuarios
#' @param conexion clase formal. Conexión base de datos
#' @param usuario clase character
#' @export

registrar_eliminacion_usuario <- function(conexion,usuario){

  # Se crea el query registrar_eliminacion_usuario
  query <- glue("DELETE FROM ADM_USUARIOS WHERE USUARIO ='{usuario}'")

  # Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  # Se almacena estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  # Se verifica el estado de la operacion
  if(resultado$ESTADO==TRUE){
    # Se lee el data frame ADM_USUARIOS de AWS
    dataframe <- dbReadTable(conexion,"ADM_USUARIOS")
    # Se almacena el data frame ADM_USUARIOS en la variable resultado
    resultado <- list(ESTADO=resultado_query,DATAFRAME=dataframe)
  }
  #Se retorna el resultado de la funcion
  return(resultado)

}

#' Enviar correo de bienvenida
#'
#' Esta función envia correo el correo de bienvenida al nuevo usuario
#' @param usuario clase character
#' @param nombre clase character
#' @param correo clase character
#' @param contrasena clase character
#' @export

enviar_correo_bienvenida <- function(usuario,nombre,correo,contrasena) {

  # Se crea la variable asunto
  asunto <- "Bienvenido(a) a CRCC Analytics"

  # Se crea la variable body
  body <- glue("<p>Estimado(a) {nombre},</p><p>La Cámara de Riesgo le quiere dar una bienvenida a
                            <a href='https://camaraderiesgo.shinyapps.io/CRCC_ANALYTICS/'>CRCC Analytics</a>.</p>
                            <p>Sus credenciales de <a href='https://camaraderiesgo.shinyapps.io/CRCC_ANALYTICS/'>CRCC Analytics</a> son:</p>
                            <ul style='list-style-type:square;'>
                              <li>Usuario: <b>{usuario}</b></li>
                              <li>Contraseña: <b>{contrasena}</b></li>
                            </ul>
                            <p>Ante cualquier inquietud puede ponerse en contacto con <a href='mailto:cs.analytics@camaraderiesgo.com.co'>soporte tecnico</a></p>
                            <img src='https://www.camaraderiesgo.com/wp-content/uploads/2021/07/Logo-CRCC-Analytics.png' height='120' width='140'>")


  # Se compone el correo
  email <- compose_email(body = md(body),footer = paste("Correo enviado el ",format(Sys.time(),usetz = FALSE, tz="America/Bogota")))

  #Se envia correo
  for(i in 1:3){
    resultado <-  tryCatch(
      smtp_send(email = email,to = c(as.character(correo)),
                from =  "crcc.analytics@camaraderiesgo.com.co",subject = asunto,
                credentials = creds_file("mail/office365"),verbose = FALSE),
      error = function(e) {return(FALSE)})

    if(is.null(resultado)||resultado==TRUE){
      resultado <- list(ESTADO=TRUE)
      break
    }
  }
  resultado <- list(ESTADO=TRUE)
  return(resultado)
}

#' Enviar correo de bienvenida
#'
#' Esta función envia correo el correo de bienvenida al nuevo usuario
#' @param usuario clase character
#' @param nombre clase character
#' @param correo clase character
#' @param contrasena clase character
#' @export

enviar_nueva_contrasena <- function(usuario,nombre,correo,contrasena) {

  # Se crea la variable asunto
  asunto <- "Reinicio Credenciales CRCC Analytics"

  # Se crea la variable body
  body <- glue("<p>Estimado(a) {nombre},</p><p>Acorde a su solicitud se ha reiniciado su contraseña de
                            <a href='https://camaraderiesgo.shinyapps.io/CRCC_ANALYTICS/'>CRCC Analytics</a>.</p>
                            <p>Sus nuevas credenciales son:</p>
                            <ul style='list-style-type:square;'>
                              <li>Usuario: <b>{usuario}</b></li>
                              <li>Contraseña: <b>{contrasena}</b></li>
                            </ul>
                            <p>Ante cualquier inquietud puede ponerse en contacto con <a href='mailto:cs.analytics@camaraderiesgo.com.co'>soporte tecnico</a></p>
                            <img src='https://www.camaraderiesgo.com/wp-content/uploads/2021/07/Logo-CRCC-Analytics.png height='100' width='140'>")

  # Se compone el correo
  email <- compose_email(body = md(body),footer = paste("Correo enviado el ",format(Sys.time(),usetz = FALSE, tz="America/Bogota")))

  #Se envia correo
  for(i in 1:3){
    resultado <-  tryCatch(
      smtp_send(email = email,to = c(as.character(correo)),
                from =  "crcc.analytics@camaraderiesgo.com.co",subject = asunto,
                credentials = creds_file("mail/office365"),verbose = FALSE),
      error = function(e) {return(FALSE)})

    if(is.null(resultado)||resultado==TRUE){
      resultado <- list(ESTADO=TRUE)
      break
    }
  }
  resultado <- list(ESTADO=TRUE)
  return(resultado)
}
