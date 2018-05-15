## manipular datos
genera_gauss <- function(n = 2){
  rnorm(n)
}
## prepara json_data
#' Manipula json de eventos y estructura del curso
#'
#'@import purrr
#'@import tidyr
#'@import lubridate
#'@import tibble
#'@import dplyr
#'
#' @param json_data A string
#'
#' @return la longitud de la lista
#' @export
#'
#' @examples
#' preparar_json({{"a":3,"nombre":"Mathieu"} })
preparar_json <- function(json_data){
  tt <- jsonlite::fromJSON(json_data)
  dim(tt)
}
