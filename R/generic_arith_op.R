# ------------------------------------------------------------ Arithmetic OPS ----------------------------------------

# ------------------------------------------------------------ POW

#' @importFrom reticulate py_has_attr
#' @export
"^.cirq.ops.pauli_string.SingleQubitPauliStringGateOperation" <- function(a, b) {
  if (py_has_attr(cirq, "mul"))
    cirq$pow(a, b)
}

#' @importFrom reticulate py_has_attr
#' @export
"^.cirq.ops.pauli_gates._PauliX" <- function(a, b) {
  if (py_has_attr(cirq, "mul"))
    cirq$pow(a, b)
}

#' @importFrom reticulate py_has_attr
#' @export
"^.cirq.ops.swap_gates.ISwapPowGate" <- function(a, b) {
  if (py_has_attr(cirq, "mul"))
    cirq$pow(a, b)
}

# ------------------------------------------------------------ MUL

#' @importFrom reticulate py_has_attr
#' @export
"*.cirq.ops.pauli_string.SingleQubitPauliStringGateOperation" <- function(a, b) {
  if (py_has_attr(cirq, "mul"))
    cirq$mul(a, b)
}


# ------------------------------------------------------------ ADD

#' @importFrom reticulate py_has_attr
#' @export
"+.cirq.ops.pauli_string.SingleQubitPauliStringGateOperation" <- function(a, b) {
  if (py_has_attr(cirq, "PauliSum") & py_has_attr(cirq, "LinearDict"))
    a$`__add__`(b)
}

#' @importFrom reticulate py_has_attr
#' @export
"+.cirq.devices.line_qubit.LineQubit" <- function(a, b) {
  a$`__add__`(b)
}
