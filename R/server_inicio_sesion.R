#' Buscar usuario
#'
#' Esta función busca y extra la información de un usuario en la tabla ''
#' @param conexion clase formal. Conexión base de datos
#' @param usuario clase character
#' @export

buscar_usuario <- function(conexion,usuario){

  #Se crea el query buscar_usuario
  query <- glue("SELECT * FROM ADM_USUARIOS WHERE USUARIO='{usuario}'")

  #Se envia el query a AWS
  resultado_query <- dbGetQuery(conexion,query)

  #Se retorna el resultado de la funcion
  return(resultado_query)

}

#' Buscar cookies usuario
#'
#' Esta función busca y extra las cookies de un usuario en la tabla ''
#' @param conexion clase formal. Conexión base de datos
#' @param usuario clase character
#' @export

buscar_cookies_usuario <- function(conexion,usuario){

  #Se crea el query MOD_IS_buscar_cookies
  query <- glue("SELECT * FROM ADM_COOKIES WHERE USUARIO='{usuario}'")

  #Se envia el query a AWS
  resultado_query <- dbGetQuery(conexion,query)

  #Se retorna el resultado de la funcion
  return(resultado_query)

}

#' Buscar permisos perfil
#'
#' Esta función busca y extra los permisos de un perfil en la tabla ''
#' @param conexion clase formal. Conexión base de datos
#' @param perfil clase character
#' @export

buscar_permisos_perfil <- function(conexion,perfil){

  #Se crea el query MOD_IS_buscar_permisos_perfil
  query <- glue("SELECT * FROM ADM_PERFILES WHERE NOMBRE='{perfil}'")

  #Se envia el query a AWS
  resultado_query <- dbGetQuery(conexion,query)

  #Se retorna el resultado de la funcion
  return(resultado_query)

}

#' Registrar conexión exitosa usuario
#'
#' Esta función registra la conexión exitosa de un usuario en la tabla ''
#' @param conexion clase formal. Conexión base de datos
#' @param fecha_hora_evento clase datetime
#' @param datos clase data.frame. Los datos del usuario deben ser los generados por la función
#'\code{\link{buscar_usuario}} o tener una estructura igual a dichos datos
#' @export

registrar_conexion_exitosa_usuario <- function(conexion,fecha_hora_evento,datos){

  #Se crea el query MOD_IS_login_usuario
  query <- glue("UPDATE ADM_USUARIOS SET INTENTOS_FALLIDOS_CONEXION=0,
                  FECHA_HORA_ULTIMA_CONEXION='{fecha_hora_evento}',
                  CONECTADO=TRUE WHERE
                  USUARIO ='{datos$USUARIO}' AND
                  CORREO ='{datos$CORREO}' AND
                  PERFIL ='{datos$PERFIL}' AND
                  CONTRASENA ='{datos$CONTRASENA}' AND
                  ESTADO ='{datos$ESTADO}' AND
                  CONECTADO=FALSE ")

  #Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  #Se almacena el estado de la operacion
  resultado <- list(ESTADO=resultado_query)
  #Se retorna el resultado de la funcion
  return(resultado)

}

#' Registrar conexión fallida usuario
#'
#' Esta función registra la conexión fallida de un usuario en la tabla ''
#' @param conexion clase formal. Conexión base de datos
#' @param fecha_hora_evento clase datetime
#' @param usuario clase character
#' @export

registrar_conexion_fallida_usuario <- function(conexion,fecha_hora_evento,usuario){

  #Se crea el query MOD_IS_login_error_usuario
  query <- glue("UPDATE ADM_USUARIOS SET INTENTOS_FALLIDOS_CONEXION=INTENTOS_FALLIDOS_CONEXION+1,
                  ESTADO=IF(INTENTOS_FALLIDOS_CONEXION = 3, 0, 1),
                  FECHA_HORA_ULTIMA_CONEXION_FALLIDA='{fecha_hora_evento}'
                  WHERE USUARIO ='{usuario}'")

  #Se envia el query a AWS
  resultado_query <- dbExecute(conexion,query)
  #Se almacena el estado de la operacion en la variable resultado
  resultado <- list(ESTADO=resultado_query)
  #Se retorna el resultado de la funcion
  return(resultado)

}

#' Enviar correo con el código de verificación
#'
#' Esta función envia correo con el código de verificación al usuario
#' @param nombre clase character
#' @param correo clase character
#' @param codigo clase character
#' @export

enviar_correo_codigo_verificacion <- function(nombre,correo,codigo) {

  # Se crea la variable asunto
  asunto <- "Código Verificación CRCC Analytics"

  # Se crea la variable body
  body <- glue("<p>Estimado(a) {nombre} </p><p>Su código de verificación es <b> {codigo} </b></p>ç
               <p>Ante cualquier inquietud puede ponerse en contacto con
               <a href='mailto:crcc.analytics@camaraderiesgo.com.co'>soporte tecnico</a></p>
               <img src='https://www.camaraderiesgo.com/wp-content/uploads/2021/07/Logo-CRCC-Analytics.png' height='100' width='140'>")


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
