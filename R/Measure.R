#' @title Measure
#' @family Measurement
#' @description a single MeasurementGate applied to all the given qubits.
#' The qubits are measured in the computational basis.
#' @return An operation targeting the given qubits with a measurement.
#'
#' @note ValueError if the qubits are not instances of Qid.
#' @param ... parameters to pass:
#' target – The qubits that the measurement gate should measure.
#'
#' key – The string key of the measurement. If this is NULL, it
#' defaults to a comma-separated list of the target qubits’ str values.
#'
#' invert_mask – A list of Truthy or Falsey values indicating
#' whether the corresponding qubits should be flipped. NULL
#' indicates no inverting should be done.
#' @export
measure <- function(...){
  args = list(...)
  if(length(args)==0)
    cirq$measure
  else
    do.call(cirq$measure,args)
}


#' @title Measure Each
#' @family Measurement
#' @description The qubits are measured in the computational basis.
#' @return a list of operations individually measuring the given qubits.
#'
#' @param ... parameters to pass:
#' *qubits – The qubits to measure.
#' key_func – Determines the key of the measurements of each qubit.
#' Takes the qubit and returns the key for that qubit. Defaults to str.
#' @export
measure_each <- function(...){
  args = list(...)
  if(length(args)==0)
    cirq$measure_each
  else
    do.call(cirq$measure_each,args)
}



#' @title MeasurementGate
#' @family Measurement
#' @description A gate that measures qubits in the computational basis.
#'
#' @details The measurement gate contains a key that is used to identify results
#' of measurements.
#'
#' @param num_qubits The number of qubits to act upon.
#' @param key The string key of the measurement.
#' @param invert_mask A list of values indicating whether the corresponding
#' qubits should be flipped. The list’s length must not be longer than the
#' number of qubits, but it is permitted to be shorter. Qubits with indices
#' past the end of the mask are not flipped.
#' @param qid_shape Specifies the dimension of each qid the measurement
#' applies to. The default is 2 for every qubit.
#'
#' @note If the length of invert_mask is greater than num_qubits. Or if
#' the length of qid_shape doesn’t equal num_qubits.
#' @return a list of operations individually measuring the given qubits.
#' @export
measurement_gate <- function(num_qubits = NULL, key = "str", invert_mask, qid_shape = NULL) {

  if(missing(invert_mask))
    cirq$MeasurementGate
  else
    if(!is.null(num_qubits))
      num_qubits = as.integer(num_qubits)
    cirq$MeasurementGate(
      num_qubits = num_qubits,
      key = key,
      invert_mask = invert_mask,
      qid_shape = qid_shape)

}





