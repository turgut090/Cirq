

#' @title Hadamard
#' @family Unitary gates and operations
#' @description the controlled Hadamard gate.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_h <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$H
  else
    do.call(cirq$H, args)
}

#' @title Identity
#' @family Unitary gates and operations
#' @description the one qubit identity gate
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_i <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$I
  else
    do.call(cirq$I, args)
}

#' @title Clifford S
#' @family Unitary gates and operations
#' @description the Clifford S gate.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_s <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$S
  else
    do.call(cirq$S, args)
}


#' @title Non-Clifford T
#' @family Unitary gates and operations
#' @description the non-Clifford T gate.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_t <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$T
  else
    do.call(cirq$T, args)
}



#' @title Pauli X
#' @family Unitary gates and operations
#' @description the Pauli X gate.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_x <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$X
  else
    do.call(cirq$X, args)
}


#' @title Pauli Y
#' @family Unitary gates and operations
#' @description the Pauli Y gate.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_y <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$Y
  else
    do.call(cirq$Y, args)
}



#' @title Pauli Z
#' @family Unitary gates and operations
#' @description the Pauli Z gate.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_z <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$Z
  else
    do.call(cirq$Z, args)
}

#####################################################################################

#' @title CX
#' @family Unitary gates and operations
#' @description the controlled NOT gate. The exponent=1 instance of gate_cx_pow.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_cx <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CX
  else
    do.call(cirq$CX, args)
}

#' @title CZ
#' @family Unitary gates and operations
#' @description the controlled Z gate. The exponent=1 instance of gate_cz_pow.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_cz <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CZ
  else
    do.call(cirq$CZ, args)
}


#' @title XX
#' @family Unitary gates and operations
#' @description the tensor product of two X gates. The exponent=1 instance of gate_xx_pow.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_xx <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$XX
  else
    do.call(cirq$XX, args)
}

#' @title YY
#' @family Unitary gates and operations
#' @description the tensor product of two Y gates. The exponent=1 instance of gate_yy_pow.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_yy <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$YY
  else
    do.call(cirq$YY, args)
}


#' @title ZZ
#' @family Unitary gates and operations
#' @description the tensor product of two Z gates. The exponent=1 instance of gate_zz_pow.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_zz <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$ZZ
  else
    do.call(cirq$ZZ, args)
}

#####################################################################################


#' @title RX
#' @family Unitary gates and operations
#' @description the controlled RX gate.
#' @return Returns a gate with the matrix e^{-i X rads / 2}.
#'
#'
#' @param rads rads
#'
#' @export
gate_rx <- function(rads) {
  cirq$rx(
    rads = rads
  )
}


#' @title RY
#' @family Unitary gates and operations
#' @description the controlled RY gate.
#' @return Returns a gate with the matrix e^{-i Y rads / 2}.
#'
#'
#' @param rads rads
#'
#' @export
gate_ry <- function(rads) {
  cirq$ry(
    rads = rads
  )
}


#' @title RZ
#' @family Unitary gates and operations
#' @description the controlled RZ gate.
#' @return Returns a gate with the matrix e^{-i Z rads / 2}.
#'
#'
#' @param rads rads
#'
#' @export
gate_ry <- function(rads) {
  cirq$rz(
    rads = rads
  )
}

#####################################################################################

#' @title TOFFOLI
#' @family Unitary gates and operations
#' @description the TOFFOLI (doubly-controlled-NOT) gate that can be raised to a power.
#' The exponent=1 instance of gate_ccx_pow.
#'
#' @param ... qubits to pass. Expects 3 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_ccnot <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CCNOT
  else
    do.call(cirq$CCNOT, args)
}


#' @title CNOT
#' @family Unitary gates and operations
#' @description the controlled NOT gate.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_cnot <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CNOT
  else
    do.call(cirq$CNOT, args)
}


#' @title CCX
#' @family Unitary gates and operations
#' @description the CCX (doubly-controlled-NOT) gate that can be raised to a power.
#' The exponent=1 instance of gate_ccx_pow.
#'
#' @param ... qubits to pass. Expects 3 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_ccx <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CCX
  else
    do.call(cirq$CCX, args)
}


#' @title CCZ
#' @family Unitary gates and operations
#' @description the CCZ (doubly-controlled-Z) gate that can be raised to a power.
#' The exponent=1 instance of gate_ccx_pow.
#'
#' @param ... qubits to pass. Expects 3 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_ccz <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CCZ
  else
    do.call(cirq$CCZ, args)
}



