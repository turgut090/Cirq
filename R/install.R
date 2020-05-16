#' @title Install Cirq
#'
#' @description This function is used to install the Cirq python module

#' @param version for specific version of Cirq, e.g. "0.8.0"
#' @param ... other arguments passed to [reticulate::py_install()].
#' @param restart_session Restart R session after installing (note this will only occur within RStudio).
#' @param from_git install the recent GitHub version of Cirq
#' @return a python module cirq
#' @importFrom reticulate py_config py_install use_python
#' @importFrom crayon red black
#' @export
install_cirq <- function(version = NULL, ..., restart_session = TRUE, from_git = FALSE) {

  if (is.null(version) & !from_git) {
    module_string <- paste0("cirq==", '0.8.0')
  } else if (!is.null(version)) {
    module_string <- paste0("cirq==", version)
  } else if (isTRUE(from_git)) {
    module_string <- paste0("git+https://github.com/quantumlib/Cirq.git")
  }

  invisible(py_config())
  py_path = Sys.which('python') %>% as.character()
  py_install(packages = paste(module_string), pip = TRUE, ...)

  fun <- function() {
    py_path = gsub(py_path, replacement = '/',pattern = '\\', fixed=TRUE)
    error <- red $ bold
    error2 <- black $ bold
    cat(error2("Cirq is installed here:"), paste('"',error(py_path),'"',sep = ''))
  }

  fun()

  if (restart_session && rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

}
