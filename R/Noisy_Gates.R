#' @title Noise model like
#' @family Noisy gates and operations
#' @description A cirq.NoiseModel or a value that can be
#' trivially converted into one.
#' None is a NOISE_MODEL_LIKE. It will be replaced by the
#' cirq.NO_NOISE noise model.
#' A single qubit gate is a NOISE_MODEL_LIKE. It will be
#' wrapped inside of a cirq.ConstantQubitNoiseModel.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_noise_model_like <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$NOISE_MODEL_LIKE
  else
    do.call(cirq$NOISE_MODEL_LIKE, args)
}



#' @title No noise
#' @family Noisy gates and operations
#' @description The trivial noise model with no effects.
#' This is the noise model used when a NOISE_MODEL_LIKE noise
#' parameter is set to None.
#' @param ... parameters to pass.
#' @return None
#' @export
gate_no_noise <- function(...) {
  args = list(...)

  if (length(args) == 0)
    cirq$NO_NOISE
  else
    do.call(cirq$NO_NOISE, args)
}



#' @title Amplitude damp
#' @family Noisy gates and operations
#'
#' @description This channel evolves a density matrix via:
#' $$ \\rho \\rightarrow M_0 \\rho M_0^\\dagger + M_1 \\rho M_1^\\dagger $$
#' With: $$ \\begin{aligned} M_0 =& \\begin{bmatrix} 1 & 0 \\ 0 & \\sqrt{1 - \\gamma} \\end{bmatrix} \\ M_1 =& \\begin{bmatrix} 0 & \\sqrt{\\gamma} \\ 0 & 0 \\end{bmatrix} \\end{aligned} $$
#'
#' @param gamma the probability of the interaction being dissipative.
#'
#' @section Raises:
#' ValueError: if gamma is not a valid probability.
#' @return an AmplitudeDampingChannel with the given probability gamma.
#' @export
gate_amplitude_damp <- function(gamma) {

  args <- list(
    gamma = gamma
  )

  do.call(cirq$amplitude_damp, args)

}



#' @title Asymmetric depolarize
#' @family Noisy gates and operations
#'
#' @description This channel evolves a density matrix via
#' $$ \\rho \\rightarrow (1 - p_x - p_y - p_z) \\rho + p_x X \\rho X + p_y Y \\rho Y + p_z Z \\rho Z $$
#'
#' @param p_x The probability that a Pauli X and no other gate occurs.
#' @param p_y The probability that a Pauli Y and no other gate occurs.
#' @param p_z The probability that a Pauli Z and no other gate occurs.
#'
#' @section Raises:
#' ValueError: if the args or the sum of the args are not probabilities.
#' @return a AsymmetricDepolarizingChannel with given parameter.
#' @export
gate_asymmetric_depolarize <- function(p_x, p_y, p_z) {

  cirq$asymmetric_depolarize(
    p_x = p_x,
    p_y = p_y,
    p_z = p_z
  )

}




#' @title Bit flip
#' @family Noisy gates and operations
#'
#' @description with probability of a flip given by p. If p is NULL, return
#' a guaranteed flip in the form of an X operation. This channel
#' evolves a density matrix via $$ \\rho \\rightarrow M_0 \\rho M_0^\\dagger + M_1 \\rho M_1^\\dagger $$ With $$
#'  \\begin{aligned} M_0 =& \\sqrt{p} \\begin{bmatrix} 1 & 0 \\ 0 & 1 \\end{bmatrix} \\ M_1 =& \\sqrt{1-p} \\begin{bmatrix} 0 & 1 \\ 1 & -0 \\end{bmatrix} \\end{aligned} $$
#'
#' @param p the probability of a bit flip.
#'
#' @section Raises:
#' ValueError: if p is not a valid probability.
#' @return Construct a BitFlipChannel that flips a qubit state.
#' @export
gate_bit_flip <- function(p = NULL) {

  cirq$bit_flip(
    p = p
  )

}




#' @title Depolarize
#' @family Noisy gates and operations
#'
#' @description This channel applies one of four disjoint possibilities: nothing (the
#' identity channel) or one of the three pauli gates. The disjoint
#' probabilities of the three gates are all the same, p / 3, and the
#' identity is done with probability 1 - p. The supplied probability
#' must be a valid probability or else this constructor will raise a
#' ValueError. This channel evolves a density matrix via
#' $$ \\rho \\rightarrow (1 - p) \\rho + (p / 3) X \\rho X + (p / 3) Y \\rho Y + (p / 3) Z \\rho Z $$
#'
#' @param p The probability that one of the Pauli gates is applied.
#' Each of the Pauli gates is applied independently with probability p / 3.
#'
#' @section This channel applies one of four disjoint possibilities: nothing (the:
#' identity channel) or one of the three pauli gates. The disjoint probabilities of
#' the three gates are all the same, p / 3, and the identity is done with probability
#' 1 - p. The supplied probability must be a valid probability or else this constructor
#' will raise a ValueError.
#'
#' @section Raises:
#' ValueError: if p is not a valid probability.
#' @return a DepolarizingChannel with given probability of error.
#' @export
gate_depolarize <- function(p) {

  cirq$depolarize(
    p = p
  )

}



#' @title Generalized amplitude damp
#' @family Noisy gates and operations
#'
#' @description probabilities gamma and p. This channel evolves a density matrix via:
#' $$ \\rho \\rightarrow M_0 \\rho M_0^\\dagger + M_1 \\rho M_1^\\dagger + M_2 \\rho M_2^\\dagger + M_3 \\rho M_3^\\dagger $$
#' With: $$ \\begin{aligned} M_0 =& \\sqrt{p} \\begin{bmatrix} 1 & 0 \\ 0 & \\sqrt{1 - \\gamma} \\end{bmatrix} \\ M_1 =& \\sqrt{p} \\begin{bmatrix} 0 & \\sqrt{\\gamma} \\ 0 & 0 \\end{bmatrix} \\ M_2 =& \\sqrt{1-p} \\begin{bmatrix} \\sqrt{1-\\gamma} & 0 \\ 0 & 1 \\end{bmatrix} \\ M_3 =& \\sqrt{1-p} \\begin{bmatrix} 0 & 0 \\ \\sqrt{\\gamma} & 0 \\end{bmatrix} \\end{aligned} $$
#'
#' @param p the probability of the qubit and environment exchanging energy.
#' @param gamma the probability of the interaction being dissipative.
#'
#' @section Raises:
#' ValueError: gamma or p is not a valid probability.
#' @return a GeneralizedAmplitudeDampingChannel with the given
#' @export
gate_generalized_amplitude_damp <- function(p, gamma) {

  cirq$generalized_amplitude_damp(
    p = p,
    gamma = gamma
  )

}



#' @title Phase damp
#' @family Noisy gates and operations
#'
#' @description This channel evolves a density matrix via:
#' $$ \\rho \\rightarrow M_0 \\rho M_0^\\dagger + M_1 \\rho M_1^\\dagger $$
#' With: $$ \\begin{aligned} M_0 =& \\begin{bmatrix} 1 & 0 \\ 0 & \\sqrt{1 - \\gamma} \\end{bmatrix} \\ M_1 =& \\begin{bmatrix} 0 & 0 \\ 0 & \\sqrt{\\gamma} \\end{bmatrix} \\end{aligned} $$
#'
#' @param gamma The damping constant.
#'
#' @section Raises:
#' ValueError: is gamma is not a valid probability.
#' @return Creates a PhaseDampingChannel with damping constant gamma.
#' @export
gate_phase_damp <- function(gamma) {

  cirq$phase_damp(
    gamma = gamma
  )

}



#' @title Phase flip
#' @family Noisy gates and operations
#'
#' @description if p is NULL, return a guaranteed phase flip in
#' the form of a Z operation. This channel evolves a density matrix
#' via: $$ \\rho \\rightarrow M_0 \\rho M_0^\\dagger + M_1 \\rho M_1^\\dagger $$
#' With: $$ \\begin{aligned} M_0 =& \\sqrt{p} \\begin{bmatrix} 1 & 0 \\ 0 & 1 \\end{bmatrix} \\ M_1 =& \\sqrt{1-p} \\begin{bmatrix} 1 & 0 \\ 0 & -1 \\end{bmatrix} \\end{aligned} $$
#'
#' @param p the probability of a phase flip.
#'
#' @section Raises:
#' ValueError: if p is not a valid probability.
#' @return a phase flip channel that flips a qubit's phase with probability p
#' @export
gate_phase_flip <- function(p = NULL) {

  cirq$phase_flip(
    p = p
  )

}



#' @title Reset
#' @family Noisy gates and operations
#'
#' @param qubit qubit
#' @return a `ResetChannel` on the given qubit.
#' @export
gate_reset <- function(qubit) {

  cirq$reset(
    qubit = qubit
  )

}


#' @title Amplitude Damping Channel
#' @family Noisy gates and operations
#'
#' @description This channel models the effect of energy dissipation to the
#' surrounding environment.
#'
#' @param gamma gamma
#' @return Dampen qubit amplitudes through dissipation.
#' @export
gate_amplitude_damping_channel <- function(gamma) {

  if(missing(gamma))
    cirq$AmplitudeDampingChannel
  else
    cirq$AmplitudeDampingChannel(gamma = gamma)

}



#' @title Asymmetric Depolarizing Channel
#' @family Noisy gates and operations
#' @description A channel that depolarizes asymmetrically along different directions.
#'
#'
#' @param p_x p_x
#' @param p_y p_y
#' @param p_z p_z
#' @return The asymmetric depolarizing channel object
#' @export
gate_asymmetric_depolarizing_channel <- function(p_x, p_y, p_z) {

  if(missing(p_x) & missing(p_y) & missing(p_z))
    cirq$AsymmetricDepolarizingChannel
  else
    cirq$AsymmetricDepolarizingChannel(p_x = p_x,
                                 p_y = p_y,
                                 p_z = p_z)
}




#' @title Bit Flip Channel
#' @family Noisy gates and operations
#' @description Probabilistically flip a qubit from 1 to 0 state or vice versa.
#'
#'
#' @param p probability
#' @return Construct a channel that flips a qubit with probability p.
#' @export
gate_bit_flip_channel <- function(p) {

  if(missing(p))
    cirq$BitFlipChannel
  else
    cirq$BitFlipChannel(p = p)

}




#' @title Depolarizing Channel
#' @family Noisy gates and operations
#' @description A channel that depolarizes a qubit.
#'
#'
#' @param p probability
#' @return The symmetric depolarizing channel object
#' @export
gate_depolarizing_channel <- function(p) {

  if(missing(p))
    cirq$DepolarizingChannel
  else
    cirq$DepolarizingChannel(p = p)
}



#' @title Generalized Amplitude Damping Channel
#' @family Noisy gates and operations
#' @description Dampen qubit amplitudes through non ideal dissipation.
#'
#' @details This channel models the effect of energy dissipation into the environment
#' as well as the environment depositing energy into the system.
#'
#' @param p probability
#' @param gamma gamma
#' @return The generalized amplitude damping channel object.
#' @export
gate_generalized_amplitude_damping_channel <- function(p, gamma) {

  if(missing(p) & missing(gamma))
    cirq$GeneralizedAmplitudeDampingChannel
  else
    cirq$GeneralizedAmplitudeDampingChannel(p = p,
                                            gamma = gamma)

}



#' @title Noise Model
#' @family Noisy gates and operations
#' @description Replaces operations and moments with noisy counterparts.
#'
#' @details A child class must override at least one of the following three methods:
#' - noisy_moments
#' - noisy_moment
#' - noisy_operation
#' The methods that are not overridden will be implemented in terms of the ones
#' that are.
#' Simulators told to use a noise model will use these methods in order to
#' dynamically rewrite the program they are simulating.
#' @param ... parameters to pass.
#' @return The generalized amplitude damping channel object.
#' @export
gate_noise_model <- function(...) {

  args = list(...)

  if(length(args)==0)
    cirq$NoiseModel
  else
    do.call(cirq$cirq$NoiseModel, args)

}




#' @title Phase Damping Channel
#' @family Noisy gates and operations
#' @description Dampen qubit phase.
#'
#' @details This channel models phase damping which is the loss of quantum
#' information without the loss of energy.
#'
#' @param gamma gamma
#' @return The phase damping channel channel object.
#' @export
gate_phase_damping_channel <- function(gamma) {

  if(missing(gamma))
    cirq$PhaseDampingChannel
  else
    cirq$PhaseDampingChannel(gamma = gamma)
}



#' @title Phase Flip Channel
#' @family Noisy gates and operations
#' @description Probabilistically flip the sign of the phase of a qubit.
#'
#'
#' @param p probability
#' @return The phase flip channel channel object.
#' @export
gate_phase_flip_channel <- function(p) {

  if(missing(p))
    cirq$PhaseFlipChannel
  else
    cirq$PhaseFlipChannel(gamma = gamma)

}




#' @title ResetChannel
#' @family Noisy gates and operations
#' @description Reset a qubit back to its |0⟩ state.
#'
#' @details The reset channel is equivalent to performing an unobserved measurement
#' which then controls a bit flip onto the targeted qubit.
#'
#' @param dimension dimension
#' @return The phase flip channel channel object.
#' @export
gate_reset_channel <- function(dimension = 2) {

  if(missing(dimension))
    cirq$ResetChannel
  else
    cirq$ResetChannel(dimension = as.integer(dimension))

}










