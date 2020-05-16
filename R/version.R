#' @title Version of Cirq
#' @description Get the current version of Cirq
#' @return prints the version.
#' @export
cirq_version <- function() {
  cirq$`__version__`
}
