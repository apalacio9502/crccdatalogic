#' Inserta ui tabPanel
#'
#' Esta función inserta o activa la ui de uno o varios tabPanels al navbarPage
#' @param session clase session. Variable que almacena la sesión del usuario
#' @param tabpanels clase vector character. Vector de los id's de los tabPanels activos
#' @export

insertar_tabpanels <- function(session,tabpanels=NA){

  # Se verifica si es una lista la variable tabpanels
  if (is.list(tabpanels)) {
    # Se recorre la lista de los tabpanels
    for (i in c(1:length(tabpanels))) {
      # Se verifica si es el primer tabPanel a insertar
      if (i==1) {
        # Se inserta el tabPanel
        appendTab(inputId="APP", tab = tabpanels[[i]], session =session,select = TRUE)
      }else{
        # Se inserta el tabPanel
        appendTab(inputId="APP", tab = tabpanels[[i]], session =session,select = FALSE)
      }
    }
  }
}


#' Elimniar caracteres especiales
#'
#' Esta función elimina los caracteres especiales de una cadena de texto
#' @param texto clase character.
#' @export

elimnar_caracteres_especiales <- function(texto) {
  gsub("`|\\'", "", iconv(texto, to="ASCII//TRANSLIT"))
}

#' Escribir log
#'
#' Esta función ecribe un nuevo log a data.frame logs
#' @param logs clase data.frame
#' @param id clase number
#' @param usuario clase character
#' @param perfil clase character
#' @param aplicacion clase character
#' @param fecha_hora clase character
#' @param evento clase character
#' @param descripcion clase character
#' @param resultado clase numeric. 1 si el evento fue exitoso, 0 de lo contrario.
#' @param resumen clase character
#' @param detalle clase character
#' @export

escribir_log <- function(logs,id=NA_real_,usuario=NA_character_,perfil=NA_character_,aplicacion=NA_character_,fecha_hora=NA_character_,
                         evento=NA_character_,descripcion=NA_character_,resultado=1,resumen=NA_character_,
                         detalle=NA_character_){

  # Se verifica si se la fecha_hora es NA
  if(is.na(fecha_hora)){
    # Se define la fecha_hora del evento
    fecha_hora <-  ymd_hms(now(tzone = "America/Bogota"),tz="America/Bogota")
  }

  # Se verifica si el evento es cerrar modal
  if (evento=="Cerrar modal" && is.na(tipo)) {
    tipo <- last(logs$TIPO)
  }

  #Se regista la información del nuevo log en el data.frame logs
  provisional <- logs %>%
    bind_rows(data.frame(ID=id,
                         USUARIO=usuario,
                         PERFIL=perfil,
                         APLICACION=aplicacion,
                         FECHA_HORA=fecha_hora,
                         EVENTO=evento,
                         DESCRIPCION=descripcion,
                         RESULTADO=resultado,
                         RESUMEN=resumen,
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


