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


