

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
#' @return a gate with the matrix e^{-i Z rads / 2}.
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

#####################################################################################

#' @title FREDKIN
#' @family Unitary gates and operations
#' @description the Controlled Swap gate. The Fredkin gate. An instance of gate_cs_wap.
#'
#' @param ... qubits to pass. Expects 3 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_fredkin <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$FREDKIN
  else
    do.call(cirq$FREDKIN, args)
}



#' @title Iswap
#' @family Unitary gates and operations
#' @description the iswap gate.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_iswap <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$ISWAP
  else
    do.call(cirq$ISWAP, args)
}


#' @title QFT
#' @family Unitary gates and operations.
#' @description The quantum Fourier transform. Transforms a qubit
#' register from the computational basis to the frequency basis.
#' The inverse quantum Fourier transform is `cirq.QFT(*qubits)**-1`
#' or equivalently `cirq.inverse(cirq.QFT(*qubits))`.
#'
#'
#' @param qubits – The qubits to apply the QFT to.
#'
#' @param without_reverse When set, swap gates at the end of the
#' QFT are omitted. This reverses the qubit order relative to the
#' standard QFT effect, but makes the gate cheaper to apply.
#'
#' @param inverse If set, the inverse QFT is performed instead
#' of the QFT. Equivalent to calling cirq.inverse on the result,
#' or raising it to the -1.
#' @return A cirq.Operation applying the QFT to the given qubits.
#' @export
gate_qft <- function(qubits, without_reverse = FALSE, inverse = FALSE) {
  args = list(qubits = qubits,
              without_reverse = without_reverse,
              inverse = inverse)

  do.call(cirq$QFT, args)
}



#' @title SWAP
#' @family Unitary gates and operations
#' @description the swap gate.
#'
#' @param ... qubits to pass. Expects 2 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_swap <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$SWAP
  else
    do.call(cirq$SWAP, args)
}


#' @title TOFFOLI
#' @family Unitary gates and operations
#' @description the tofolli gate. The exponent=1 instance of gate_ccx_pow.
#'
#' @param ... qubits to pass. Expects 3 qubits.
#' @return applied effects to a set of qubits.
#' @export
gate_toffoli <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$TOFFOLI
  else
    do.call(cirq$TOFFOLI, args)
}


#' @title Givens
#' @family Unitary gates and operations
#' @description a gate with matrix exp(-i angle_rads (Y⊗X - X⊗Y) / 2).
#'
#' @details In numerical linear algebra Givens rotation is any linear transformation
#' with matrix equal to the identity except for a 2x2 orthogonal submatrix
#' [[cos(a), -sin(a)],
#' [sin(a), cos(a)]] which performs a 2D rotation on a
#' subspace spanned by two basis vectors. In quantum computational chemistry
#' the term is used to refer to the two-qubit gate defined as
#' givens(a) ≡ exp(-i a (Y⊗X - X⊗Y) / 2) with the matrix
#' [[1, 0, 0, 0],
#' [0, c, -s, 0],
#' [0, s, c, 0],
#' [0, 0, 0, 1]] where
#' c = cos(a), s = sin(a). The matrix is a Givens rotation in the numerical
#' linear algebra sense acting on the subspace spanned by the |01⟩ and |10⟩ states.
#' The gate is also equivalent to the ISWAP conjugated by T^-1 ⊗ T.
#'
#' @param angle_rads The rotation angle in radians.
#'
#' @return A phased iswap gate for the given rotation.
#'
#' @export
gate_givens <- function(angle_rads) {

  args <- list(
    angle_rads = angle_rads
  )

  do.call(cirq$givens, args)
}

#' @title Identity_each
#' @family Unitary gates and operations
#' @description a single IdentityGate applied to all the given qubits.
#' @param ... parameters to pass into Identity_each. Requires and instance of Qid.
#'
#'
#'
#' @note ValueError if the qubits are not instances of Qid.
#' @return An identity operation on the given qubits.
#' @export
gate_identity_each <- function(...) {

  args = list(...)
  do.call(cirq$identity_each, args)
}


#' @title Riswap
#' @family Unitary gates and operations
#'
#' @param rads rads
#' @return a gate with matrix exp(+i angle_rads (X⊗X + Y⊗Y) / 2).
#' @export
gate_riswap <- function(rads) {
  args = list(rads = rads)

  do.call(cirq$riswap, args)
}

#' @title CCNotPowGate
#' @family Unitary gates and operations
#'
#' @param ... parameters to pass
#' @return None
#' @export
gate_ccnot_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CCNotPowGate
  else
    do.call(cirq$CCNotPowGate, args)
}

#' @title CCXPowGate
#' @family Unitary gates and operations
#' @description A Toffoli (doubly-controlled-NOT) that can be
#' raised to a power.
#' The matrix of `CCX**t` is an 8x8 identity except the bottom
#' right 2x2 area is the matrix of `X**t`.
#' @param ... parameters to pass
#' @return None
#' @export
gate_ccx_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CCXPowGate
  else
    do.call(cirq$CCXPowGate, args)
}


#' @title CCZPowGate
#' @family Unitary gates and operations
#' @description A doubly-controlled-Z that can be raised to a power.
#' The matrix of `CCZ**t` is `diag(1, 1, 1, 1, 1, 1, 1, exp(i pi t))`.
#' @param ... parameters to pass
#' @return None
#' @export
gate_ccz_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CCZPowGate
  else
    do.call(cirq$CCZPowGate, args)
}



#' @title CNotPowGate
#' @family Unitary gates and operations
#' @param ... parameters to pass
#' @return None
#' @export
gate_cnot_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CNotPowGate
  else
    do.call(cirq$CNotPowGate, args)
}


#' @title CSwapGate
#' @family Unitary gates and operations
#' @description A controlled swap gate. The Fredkin gate.
#' @param ... parameters to pass
#' @return None
#' @export
gate_cs_wap <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CSwapGate
  else
    do.call(cirq$CSwapGate, args)
}


#' @title CXPowGate
#' @family Unitary gates and operations
#' @description A gate that applies a controlled power of an X gate.
#' @details When applying CNOT (controlled-not) to qubits, you can either use
#' positional arguments `CNOT(q1, q2)`, where q2 is toggled when q1 is on,
#' or named arguments `CNOT(control=q1, target=q2)`.
#' (Mixing the two is not permitted.)
#' @param ... parameters to pass
#' @return None
#' @export
gate_cx_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CXPowGate
  else
    do.call(cirq$CXPowGate, args)
}




#' @title CZPowGate
#' @family Unitary gates and operations
#' @description A gate that applies a phase to the |11⟩ state of two qubits.
#'
#' @param ... parameters to pass
#' @return None
#' @export
gate_cz_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$CZPowGate
  else
    do.call(cirq$CZPowGate, args)
}



#' @title Controlled Gate
#' @family Unitary gates and operations
#' @description Augments existing gates to have one or more
#' control qubits. This object is typically created via `gate_controlled()`.
#' @details Initializes the controlled gate. If no arguments are specified
#' for the controls, defaults to a single qubit control.
#' @param ... parameters to pass
#' @return None
#' @export
gate_controlled <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$ControlledGate
  else
    do.call(cirq$ControlledGate, args)
}



#' @title Eigen Gate
#' @family Unitary gates and operations
#' @description A gate with a known eigendecomposition.
#' @details EigenGate is particularly useful when one wishes for different parts of
#' the same eigenspace to be extrapolated differently. For example, if a gate
#' has a 2-dimensional eigenspace with eigenvalue -1, but one wishes for the
#' square root of the gate to split this eigenspace into a part with
#' eigenvalue i and a part with eigenvalue -i, then EigenGate allows this
#' functionality to be unambiguously specified via the _eigen_components method.
#' @param ... parameters to pass
#' @return None
#' @export
gate_eigen <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$EigenGate
  else
    do.call(cirq$EigenGate, args)
}


#' @title FSimGate
#' @family Unitary gates and operations
#' @description Fermionic simulation gate.
#' @details Contains all two qubit interactions that preserve excitations, up to
#' single-qubit rotations and global phase.
#' @param ... parameters to pass
#' @return None
#' @export
gate_fsim <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$FSimGate
  else
    do.call(cirq$FSimGate, args)
}


#' @title Gate
#' @family Unitary gates and operations
#' @description An operation type that can be applied to a collection of qubits.
#' @details Gates can be applied to qubits by calling their on() method with
#' the qubits to be applied to supplied, or, alternatively, by simply
#' calling the gate on the qubits. In other words calling `MyGate.on(q1, q2)`
#' to create an Operation on q1 and q2 is equivalent to `MyGate(q1,q2)`.
#' Gates operate on a certain number of qubits. All implementations of gate
#' must implement the num_qubits method declaring how many qubits they
#' act on. The gate feature classes SingleQubitGate and TwoQubitGate
#' can be used to avoid writing this boilerplate.
#' Linear combinations of gates can be created by adding gates together and
#' multiplying them by scalars.
#' @param ... parameters to pass
#' @return None
#' @export
gate_Gate <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$Gate
  else
    do.call(cirq$Gate, args)
}


#' @title Global Phase Operation
#' @family Unitary gates and operations
#' @description An operation type that can be applied to a collection of qubits.
#' @param ... parameters to pass
#' @return None
#' @export
gate_global_phase_operation <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$GlobalPhaseOperation
  else
    do.call(cirq$GlobalPhaseOperation, args)
}


#' @title HPow Gate
#' @family Unitary gates and operations
#' @description A Gate that performs a rotation around the X+Z axis of the Bloch sphere.
#' @details The unitary matrix of HPowGate(exponent=t) is:
#' `[[g·(c-i·s/sqrt(2)), -i·g·s/sqrt(2)],`
#' `[-i·g·s/sqrt(2)], g·(c+i·s/sqrt(2))]]`
#' where
#' c = cos(π·t/2)
#' s = sin(π·t/2)
#' g = exp(i·π·t/2).
#' Note in particular that for t=1, this gives the Hadamard matrix.
#' gate_h, the Hadamard gate, is an instance of this gate at exponent=1.
#' @param ... parameters to pass
#' @return None
#' @export
gate_hpow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$HPowGate
  else
    do.call(cirq$HPowGate, args)
}





#' @title ISwap Pow Gate
#' @family Unitary gates and operations
#' @description Rotates the |01⟩ vs |10⟩ subspace of two qubits around its Bloch X-axis.
#' @details When exponent=1, swaps the two qubits and phases |01⟩ and |10⟩ by i.
#' @param ... parameters to pass
#' @return None
#' @export
gate_is_wap_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$ISwapPowGate
  else
    do.call(cirq$ISwapPowGate, args)
}



#' @title Identity Gate
#' @family Unitary gates and operations
#' @description A Gate that perform no operation on qubits.
#' @details The unitary matrix of this gate is a diagonal matrix with all 1s on the
#' diagonal and all 0s off the diagonal in any basis.
#' @param ... parameters to pass
#' @return None
#' @export
gate_identity <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$IdentityGate
  else
    do.call(cirq$IdentityGate, args)
}




#' @title Matrix Gate
#' @family Unitary gates and operations
#' @description A unitary qubit or qudit gate defined entirely by its matrix.
#' @param ... parameters to pass
#' @return None
#' @export
gate_matrix <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$MatrixGate
  else
    do.call(cirq$MatrixGate, args)
}


#' @title Operation Gate
#' @family Unitary gates and operations
#' @description An effect applied to a collection of qubits.
#' @param ... parameters to pass
#' @return None
#' @export
gate_operation <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$Operation
  else
    do.call(cirq$Operation, args)
}




#' @title Phase Gradient Gate
#' @family Unitary gates and operations
#' @description An effect applied to a collection of qubits.
#' @param ... parameters to pass
#' @return None
#' @export
gate_phase_gradient <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$PhaseGradientGate
  else
    do.call(cirq$PhaseGradientGate, args)
}


#' @title Phased ISwap Pow Gate
#' @family Unitary gates and operations
#' @description Fractional ISWAP conjugated by Z rotations.
#' @details PhasedISwapPowGate with phase_exponent p and exponent t is equivalent to
#' the composition.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_phased_is_wap_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$PhasedISwapPowGate
  else
    do.call(cirq$PhasedISwapPowGate, args)
}



#' @title Phased XZ Gate
#' @family Unitary gates and operations
#' @description A single qubit operation expressed as $Z^z Z^a X^x Z^{-a}$.
#' @details The above expression is a matrix multiplication with time going to the left.
#' In quantum circuit notation, this operation decomposes into this circuit:
#' ───Z^(-a)──X^x──Z^a────Z^z───$
#' The axis phase exponent (a) decides which axis in the XY plane to rotate
#' around. The amount of rotation around that axis is decided by the x
#' exponent (x). Then the z exponent (z) decides how much to phase the qubit.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_phased_xz <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$PhasedXZGate
  else
    do.call(cirq$PhasedXZGate, args)
}


#' @title Quantum Fourier Transform Gate
#' @family Unitary gates and operations
#' @description Switches from the computational basis to the frequency basis.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_quantum_fourier_transform <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$QuantumFourierTransformGate
  else
    do.call(cirq$QuantumFourierTransformGate, args)
}


#' @title Single Qubit Gate
#' @family Unitary gates and operations
#' @description A gate that must be applied to exactly one qubit.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_single_qubit <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$SingleQubitGate
  else
    do.call(cirq$SingleQubitGate, args)
}



#' @title Swap Pow Gate
#' @family Unitary gates and operations
#' @description The SWAP gate, possibly raised to a power. Exchanges qubits.
#' @details SwapPowGate()**t = SwapPowGate(exponent=t) and acts on two qubits
#' in the computational basis as the matrix.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_swap_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$SwapPowGate
  else
    do.call(cirq$SwapPowGate, args)
}


#' @title Tagged Operation
#' @family Unitary gates and operations
#' @description A specific operation instance that has been
#' identified with a set of Tags for special processing. This
#' can be initialized with Using Operation.with_tags(tag) or
#' by TaggedOperation(op, tag).
#' @details Tags added can be of any type, but they should be Hashable in order
#' to allow equality checking. If you wish to serialize operations into
#' JSON, you should restrict yourself to only use objects that have a JSON
#' serialization.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_tagged_opertaion <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$TaggedOperation
  else
    do.call(cirq$TaggedOperation, args)
}



#' @title Three Qubit Diagonal Gate
#' @family Unitary gates and operations
#' @description A gate given by a diagonal 8x8 matrix.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_three_qubit_diagonal <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$ThreeQubitDiagonalGate
  else
    do.call(cirq$ThreeQubitDiagonalGate, args)
}


#' @title Three Qubit Gate
#' @family Unitary gates and operations
#' @description A gate that must be applied to exactly three qubits.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_three_qubit <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$ThreeQubitDiagonalGate
  else
    do.call(cirq$ThreeQubitDiagonalGate, args)
}


#' @title Two Qubit Gate
#' @family Unitary gates and operations
#' @description A gate that must be applied to exactly two qubits.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_three_qubit <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$TwoQubitGate
  else
    do.call(cirq$TwoQubitGate, args)
}

#' @title Two Qubit Gate
#' @family Unitary gates and operations
#' @description A single-qubit idle gate that represents waiting.
#'
#' @details In non-noisy simulators, this gate is just an identity gate. But noisy
#' simulators and noise models may insert more error for longer waits.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_wait <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$WaitGate
  else
    do.call(cirq$WaitGate, args)
}


#' @title X Pow Gate
#' @family Unitary gates and operations
#' @description A gate that rotates around the X axis of the Bloch sphere.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_x_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$XPowGate
  else
    do.call(cirq$XPowGate, args)
}


#' @title XX Pow Gate
#' @family Unitary gates and operations
#' @description The X-parity gate, possibly raised to a power.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_xx_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$XXPowGate
  else
    do.call(cirq$XXPowGate, args)
}



#' @title Y Pow Gate
#' @family Unitary gates and operations
#' @description A gate that rotates around the Y axis of the Bloch sphere.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_y_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$YPowGate
  else
    do.call(cirq$YPowGate, args)
}


#' @title YY Pow Gate
#' @family Unitary gates and operations
#' @description The Y-parity gate, possibly raised to a power.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_yy_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$XXPowGate
  else
    do.call(cirq$XXPowGate, args)
}




#' @title Z Pow Gate
#' @family Unitary gates and operations
#' @description A gate that rotates around the Z axis of the Bloch sphere.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_z_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$ZPowGate
  else
    do.call(cirq$ZPowGate, args)
}


#' @title ZZ Pow Gate
#' @family Unitary gates and operations
#' @description The Z-parity gate, possibly raised to a power.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_zz_pow <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$ZZPowGate
  else
    do.call(cirq$ZZPowGate, args)
}


