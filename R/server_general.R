#' Inserta o activa ui tabPanel
#'
#' Esta función inserta o activa la ui de uno o varios tabPanels al navbarPage
#' @param session clase session. Variable que almacena la sesión del usuario
#' @param tabpanels_activos clase vector character. Vector de los id's de los tabPanels activos
#' @param tabpanels_nuevos clase list/vector character. Lista que contiene las ui de los tabPanels que se insertaran o vector
#' los id's de los tabPanels que se desean activar.
#' @param remover clase boolean. TRUE si se desea remover los tabPanels activos o FALSE si se desea
#' esconder los tabPanels activos Por defecto FALSE
#' @export

insertar_activar_ui_tabpanels <- function(session,tabpanels_activos=NA,tabpanels_nuevos=NA,remover=FALSE){

  # Se verifica si se deben remover los tabPanels activos
  if (remover==TRUE) {
    # Se recorre el vector de los tabpanels_activos
    for (i in c(1:length(tabpanels_activos))) {
      # Se remueve el tabPanel
      removeTab(inputId="APP", target = tabpanels_activos[i], session =session)
    }
  }else{
    # Se recorre el vector de los tabpanels_activos
    for (i in c(1:length(tabpanels_activos))) {
      # Se esconde el tabPanel
      hideTab(inputId="APP", target = tabpanels_activos[i], session =session)
    }
  }

  # Se verifica si es una lista la variable tabpanels_nuevos
  if (is.list(tabpanels_nuevos)) {
    # Se recorre la lista de los tabpanels_nuevos
    for (i in c(1:length(tabpanels_nuevos))) {
      # Se verifica si es el primer tabPanel a insertar
      if (i==1) {
        # Se inserta el tabPanel
        appendTab(inputId="APP", tab = tabpanels_nuevos[[i]], session =session,select = TRUE)
      }else{
        # Se inserta el tabPanel
        appendTab(inputId="APP", tab = tabpanels_nuevos[[i]], session =session,select = FALSE)
      }
    }

    # Se actualiza tabpanels_activos
    tabpanels_activos <- names(tabpanels_nuevos)
  }else{

    # Se recorre el vector de los tabpanels_nuevos
    for (i in c(1:length(tabpanels_nuevos))) {
      # Se verifica si es el primer tabPanel a activar
      if (i==1) {
        # Se activa el tabPanel
        showTab(inputId="APP", target = tabpanels_nuevos[i], session =session,select = TRUE)
      }else{
        # Se activa el tabPanel
        showTab(inputId="APP", target = tabpanels_nuevos[i], session =session,select = FALSE)
      }
    }
    # Se actualiza tabpanels_activos
    tabpanels_activos <- tabpanels_nuevos
  }

  return(tabpanels_activos)
}

#' Elimniar caracteres especiales
#'
#' Esta función elimina los caracteres especiales de una cadena de texto
#' @param texto clase character.
#' @export

elimnar_caracteres_especiales <- function(texto) {
  gsub("`|\\'", "", iconv(texto, to="ASCII//TRANSLIT"))
}

#' Verifica el formato de una fecha es correcto
#'
#' Esta función verifica si el formato de una fecha es correcto (YYYY-MM-DD)
#' @param fecha clase date/character
#' @export

verificar_formato_fecha <- function(fecha){
  grepl("[0-9]{4}[-]{1}([0]{1}[1-9]{1}|[1]{1}[0-2]{1})[-]{1}([0]{1}[1-9]{1}|[1-2]{1}[0-9]{1}|[3]{1}[0-1]{1})", as.character(fecha))
}

#' Colapsar columnas data.frame en una cadena de texto
#'
#' Esta función colapsa las columnas de un data.frame en una cadena de texto
#' @param datos clase data.frame
#' @export

colapsar_columnas_dataframe <- function(datos){

  # Se crea vector de los nombres de las columnas de los datos
  columnas <- colnames(datos)
  # Se inicializa la lista resultado
  resultado <- list()
  # Se recorre el vector columnas
  for (i in 1:length(columnas)) {
    # Se agrega el la información de la columna a la lista reusltado
    resultado <- append(resultado,list(paste0(columnas[i],"=",datos[i][[1]])))
  }

  # Se colapsca la lista resultado en un character
  resultado <- paste0(resultado,collapse = ";")

  return(resultado)
}

#' Encriptar contraseña
#'
#' Esta función encripta la contraseña por medio de un key, nonce y la semilla del computador
#' @param contrasena clase character
#' @export

encriptar_contrasena <- function(contrasena){
  bin2hex(data_encrypt(serialize(contrasena, NULL), key = hex2bin(Sys.getenv("key")),nonce = hex2bin(Sys.getenv("nonce"))))
}

#' Desencriptar contraseña
#'
#' Esta función desencripta la contraseña por medio de un key y un nonce
#' @param contrasena clase character
#' @export

desencriptar_contrasena <- function(contrasena){
  unserialize(data_decrypt(hex2bin(contrasena), key = hex2bin(Sys.getenv("key")),nonce = hex2bin(Sys.getenv("nonce"))))
}

#' Agregar log
#'
#' Esta función agrega un nuevo log a data.frame logs
#' @param logs clase data.frame
#' @param usuario clase character
#' @param modulo clase character
#' @param fecha_hora_evento clase character
#' @param evento clase character
#' @param tipo clase character
#' @param resultado clase numeric. 1 si el evento fue exitoso, 0 de lo contrario.
#' @param resumen clase character
#' @param detalle clase character
#' @export

escribir_log <- function(logs,usuario=NA_character_,modulo=NA_character_,fecha_hora_evento=NA_character_,
                         evento,tipo=NA_character_,resultado=1,resumen=NA_character_,detalle=NA_character_){

  # Se verifica si se la fecha_hora_evento es NA
  if(is.na(fecha_hora_evento)){
    # Se define la fecha_hora_evento del evento
    fecha_hora_evento <-  ymd_hms(now(tzone = "America/Bogota"),tz="America/Bogota")
  }

  # Se verifica si el evento es cerrar modal
  if (evento=="Cerrar modal" && is.na(tipo)) {
    tipo <- last(logs$TIPO)
  }

  #Se regista la información del nuevo log en el data.frame logs
  provisional <- logs %>%
    bind_rows(data.frame(USUARIO=usuario,MODULO=modulo,
                         FECHA_HORA_EVENTO=fecha_hora_evento,EVENTO=evento,
                         TIPO=ifelse(is.na(tipo),evento,tipo),RESULTADO=resultado,Resumen=resumen,
                         DETALLE=detalle))

  #Se retorna el resultado de la funcion
  return(provisional)

}


#' Retroalimentación evento
#'
#' Esta función genera la retroalimentación de un evento, generando el log
#' y notificandondole al usuario el resultado.
#' @param input clase input. Variable que almacena la input. del usuario
#' @param output clase output. Variable que almacena la output del usuario
#' @param session clase session. Variable que almacena la sesión del usuario
#' @param logs clase data.frame
#' @param estado_evento clase list
#' @param boton clase character
#' @param modal clase numeric
#' @export

retroalimientacion_evento <- function(input,output,session,logs,estado_evento,boton,modal=1){

  # Se regista la información del nuevo log
  logs <-escribir_log(logs=logs,fecha_hora_evento=estado_evento$FECHA_HORA_EVENTO,evento=estado_evento$EVENTO,
                       resultado=estado_evento$RESULTADO,resumen=estado_evento$RESUMEN,
                       detalle=ifelse(estado_evento$RESULTADO==1,estado_evento$DETALLE,NA_character_))

  # Se verifica el resultado de evento
  if (estado_evento$RESULTADO==0) {
    if (modal!=1) {
      if (modal==2) {
        # Se remueve el modal
        removeModal(session = session)
      }
      # Se notifica al usuario el estado de la operacion
      showNotification(ui=estado_evento$MENSAJE,type = "error",session = session,duration = 15)
    }else{
      # Se notifica al usuario el estado de la operacion
      output$mensaje_modal <-renderText({paste0(estado_evento$MENSAJE)})
      # Se habilita el boton
      shinyjs::enable(boton)
    }
  }else{
    if (modal!=0) {
      if (modal==1) {
        # Se limpia el output MOD_DIV_SUB_ADM_mensaje_modal
        output$mensaje_modal <- renderText({})
      }
      # Se remueve el modal
      removeModal(session = session)
      # Se regista la información del nuevo log en la base de datos logs en Shiny
      logs <- escribir_log(logs=logs,evento="Cerrar modal")
    }
    # Se notifica al usuario el estado de la operacion
    showNotification(ui=estado_evento$MENSAJE,type = "message",session = session,duration = 15)
  }

  return(logs)
}


#' Crear lista de botones generales
#'
#' Esta función crea la lista de los botones generales con base en el modulo activo
#' y en si el usuario es administrador
#' @param modulo_activo clase character
#' @param administrador clase boolean
#' @export

botones_generales <- function(modulo_activo=NULL,administrador=FALSE){
  # Se crea e inicializa la lista de botones
  botones <- list(actionButton(inputId ="APP_logout",label = NULL,icon = icon("sign-out-alt")))

  if (!modulo_activo %in% c("VU","CC")) {
    if (modulo_activo!="HM") {
      botones <- append(botones,list(actionButton(inputId ="APP_home",label = NULL,icon = icon("home"))))
    }

    if (modulo_activo!="ADM" & administrador==TRUE) {
      botones <- append(botones,list(actionButton(inputId ="APP_administrador",label = NULL,icon = icon("users-cog"))))
    }
    botones <- append(botones,list(actionButton(inputId ="APP_usuario",label = NULL,icon = icon("user"))))
  }
  return(botones)
}
