# ------------------------------------------------------------ Print

#' @export
"print.cirq.ops.linear_combinations.PauliSum" <- function(a) {
  cat(a$`__repr__`())
}

#' @export
"print.cirq.ops.pauli_string.SingleQubitPauliStringGateOperation" <- function(a) {
  cat(a$`__repr__`())
}

#' @export
"print.cirq.ops.pauli_string.PauliString" <- function(a) {
  cat(a$`__repr__`())
}

#' @export
"print.cirq.ops.common_gates.XPowGate" <- function(a) {
  cat(a$`__repr__`())
}

#' @export
"print.cirq.ops.swap_gates.ISwapPowGate" <- function(a) {
  cat(a$`__repr__`())
}

#' @export
"print.cirq.ops.common_gates.YPowGate" <- function(a) {
  cat(a$`__repr__`())
}

#' @export
"print.cirq.ops.gate_operation.GateOperation" <- function(a) {
  cat(a$`__repr__`())
}

#' @export
"print.cirq.circuits.circuit.Circuit" <- function(a) {
  cat(a$`__repr__`())
}

#' @export
"str.cirq.circuits.circuit.Circuit" <- function(a) {
  cat(a$`__str__`())
}

# ------------------------------------------------------------ Arithmetic OPS ----------------------------------------

# ------------------------------------------------------------ POW

#' @export
"^.cirq.ops.pauli_string.SingleQubitPauliStringGateOperation" <- function(a, b) {
  if (py_has_attr(cirq, "mul"))
    cirq$pow(a, b)
}

#' @export
"^.cirq.ops.pauli_gates._PauliX" <- function(a, b) {
  if (py_has_attr(cirq, "mul"))
    cirq$pow(a, b)
}

#' @export
"^.cirq.ops.swap_gates.ISwapPowGate" <- function(a, b) {
  if (py_has_attr(cirq, "mul"))
    cirq$pow(a, b)
}

# ------------------------------------------------------------ MUL

#' @export
"*.cirq.ops.pauli_string.SingleQubitPauliStringGateOperation" <- function(a, b) {
  if (py_has_attr(cirq, "mul"))
    cirq$mul(a, b)
}


# ------------------------------------------------------------ ADD

#' @export
"+.cirq.ops.pauli_string.SingleQubitPauliStringGateOperation" <- function(a, b) {
  if (py_has_attr(cirq, "PauliSum") & py_has_attr(cirq, "LinearDict"))
    a$`__add__`(b)
}

#' @export
"+.cirq.devices.line_qubit.LineQubit" <- function(a, b) {
    a$`__add__`(b)
}

# ------------------------------------------------------------ Gate OPS ----------------------------------------







