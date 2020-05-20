


# ------------------------------------------------------------ Print ----------------------------------------

#' @title GridQubit
#' @method print cirq.devices.grid_qubit.GridQubit
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.devices.grid_qubit.GridQubit" <- function(x, ...) {
  cat(x$`__repr__`())
}

#' @title LineQubit
#' @method print cirq.devices.line_qubit.LineQubit
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.devices.line_qubit.LineQubit" <- function(x, ...) {
  cat(x$`__repr__`())
}

#' @title PauliSum
#' @method print cirq.ops.linear_combinations.PauliSum
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.ops.linear_combinations.PauliSum" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title _PauliZ
#' @method print cirq.ops.pauli_gates._PauliZ
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.ops.pauli_gates._PauliZ" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title SingleQubitPauliStringGateOperation
#' @method print cirq.ops.pauli_string.SingleQubitPauliStringGateOperation
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.ops.pauli_string.SingleQubitPauliStringGateOperation" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title PauliString
#' @method print cirq.ops.pauli_string.PauliString
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.ops.pauli_string.PauliString" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title XPowGate
#' @method print cirq.ops.common_gates.XPowGate
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.ops.common_gates.XPowGate" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title CXPowGate
#' @method print cirq.ops.common_gates.CXPowGate
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.ops.common_gates.CXPowGate" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title ISwapPowGate
#' @method print cirq.ops.swap_gates.ISwapPowGate
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.ops.swap_gates.ISwapPowGate" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title YPowGate
#' @method print cirq.ops.common_gates.YPowGate
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.ops.common_gates.YPowGate" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title GateOperation
#' @method print cirq.ops.gate_operation.GateOperation
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.ops.gate_operation.GateOperation" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title Circuit
#' @method print cirq.circuits.circuit.Circuit
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
"print.cirq.circuits.circuit.Circuit" <- function(x,...) {
  cat(x$`__repr__`())
}

#' @title Circuit
#' @method str cirq.circuits.circuit.Circuit
#' @param object any R object about which you want to have some information.
#' @param ... further arguments passed to or from other methods.
#' @export
"str.cirq.circuits.circuit.Circuit" <- function(object, ...) {
  cat(object$`__str__`())
}










