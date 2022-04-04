#' Agrega columna botones a data.table (DT)
#'
#' Esta función agrega la columna botones a una tabla tipo data.table (DT)
#' @param llave clase character. Llave de los botones
#' @param ns clase character. Namespace al que pertenece el boton
#' @param botones clase vector character. Vector de los nombres de los botones
#' @export

agregar_botones_tabla <- function(llave,ns,botones){

  # Se inicializa la variable resultado
  resultado <- paste()
  for (i in botones) {

    # Se crean las caracteristicas del boton
    caracteristica_boton <- case_when(i=="info"~c("btn-dark","info-circle"),
                                      i=="modificar"~c("btn-dark","pencil-alt"),
                                      i=="reiniciar_contrasena"~c("btn-dark","key"),
                                      i=="bloquear"~c("btn-dark","lock"),
                                      i=="desbloquear"~c("btn-dark","unlock-alt"),
                                      i=="eliminar"~c("btn-dark","trash"))

    # Se crea el boton y se agrega a la variable resultado
    resultado <- paste(resultado,as.character(
      actionButton(inputId = llave,label = NULL,
                   class =caracteristica_boton[1],
                   icon = icon(caracteristica_boton[2]),style="padding:6px",
                   onclick = paste0("Shiny.setInputValue(\'",ns,"-",i,"\', this.id, {priority: 'event'})"))))
  }

  return(resultado)
}

#' Crea boton reactivo
#'
#' Esta función crea el boton reactivo de una lista de botones
#' @param botones clase lista. Lista de botones
#' @param inputId clase character. Id del boton
#' @param icon clase character. Icono del boton
#' @param status clase character. Status del boton
#' @export

boton_reactivo <- function(botones=NULL, inputId = NULL, icon = "plus", status = "default") {
  icon <- icon(icon)
  args <- botones
  for (i in seq_along(args)) {
    if (!is.null(args[[i]]$attribs$class)) {
      args[[i]]$attribs$class <- paste(args[[i]]$attribs$class, "buttons-fab")
    } else {
      args[[i]]$attribs$class <- "buttons-fab"
    }
  }
  tagList(
    tags$nav(
      class = "container-fab", args,
      tags$button(
        class = paste0("btn btn-", status, " buttons-fab btn-fab-main action-button"),
        id = inputId, icon
      )
    )
  )
}


#' Agrega contenido html a una tarjeta
#'
#' Esta función agrega el contenido html a una tarjeta (card)
#' @param title clase character. Título de la tarjeta
#' @param width clase integer. Ancho de la tarjeta minimo 1 y maximo 12
#' @param ... clase html. Contenido de la tarjeta
#' @export

tarjeta <- function(title,width,...) {
  div(class = paste0("col-sm-",width, " mb-4"),
      div(class = "card", style="padding: 0px;margin-bottom: -1rem; min-height: 180px;",
          div(class = "card-header",style="background-color:transparent;border-color: #FFFFFF; padding: 0px", title),
          do.call(div,foreach(i=list(...))%do%{
            div(class = "card-body d-flex justify-content-center",style="padding: 7px",i)
          })
      )
  )
}

#' Crear pie de pagina
#'
#' Esta función crea el pie de pagina
#' @param posicion clase character. Posición del pie de pagina ("fixed", "absolute"). Por defecto "fixed"
#' @export

pie_pagina <-function(posicion="fixed"){
  div(class="fixed-bottom", style = paste0("position:",posicion,";z-index:-100"),br(),
      div(class="row",
          div(class="col-xs-12 col-sm-6 col-md-6 col-lg-6",
              p(style="text-align:center;",
                img(src="logos/crcc.png", style="height:55px;",class="center")
              )
          ),
          div(class="col-xs-12 col-sm-6 col-md-6 col-lg-6",
              p(style="text-align:center;",
                img(src="logos/crcc_analytics_version.png", style="height:55px;",class="center")
              )
          )
      )
  )
}

#' Grilla Tarjetas
#'
#' Renderiza el contenido en una grilla de tarjetas
#' @param content clase data.frame. Requiere contener las siguientes columnas "guid," "url", "title".
#' y las siguientes columnsa de forma opcional "owner_username", "description" y "updated_time".
#' @export
rsc_card <- function(content) {
  # content should have at least one row
  if (missing(content) || !is.data.frame(content) || nrow(content) == 0) {
    stop("rsc_card() expects a data frame with at least one record.")
  }

  cols <- colnames(content)
  evaluate_widget_input(
    "rsc_card()",
    cols,
    c("guid", "title", "url")
  )
  warning_widget_input(
    "rsc_card()",
    cols,
    c("owner_username", "description", "updated_time")
  )
  warning_large_content(content)

  component <- reactR::component(
    "CardView",
    list(
      data = content
    )
  )

  dependencies <- crosstalk::crosstalkLibs()
  dependencies[["rsc_card-theme"]] <- resolve_theme_dependency("rsc_card")

  # create widget
  htmlwidgets::createWidget(
    name = "rsc_card",
    reactR::reactMarkup(component),
    width = "auto",
    height = "auto",
    package = "crccdatalogic",
    dependencies = dependencies
  )
}

# nolint start
#' Called by HTMLWidgets to produce the widget's root element.
#' @noRd
widget_html.rsc_card <- function(id, style, class, ...) {
  htmltools::tagList(
    # Necessary for RStudio viewer version < 1.2
    reactR::html_dependency_corejs(),
    reactR::html_dependency_react(),
    reactR::html_dependency_reacttools(),
    htmltools::tags$div(id = id, class = class, style = style)
  )
}

#' Shiny bindings for rsc_card
#'
#' Output and render functions for using rsc_card within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a rsc_card
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name rsc_card-shiny
#'
#' @export
rsccardOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'rsc_card', width, height, package = 'crccdatalogic')
}

#' @rdname rsc_card-shiny
#' @export
renderRsccard <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, rsccardOutput, env, quoted = TRUE)
}
# nolint end
