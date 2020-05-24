#' @title Quirk json to circuit
#' @family Importing and Exporting
#' @description Constructs a Cirq circuit from Quirk's JSON format.
#'
#'
#' @param data Data parsed from quirk's JSON representation.
#' @param qubits Qubits to use in the circuit. See quirk_url_to_circuit.
#'
#' @param extra_cell_makers Non-standard Quirk cells to accept.
#' See quirk_url_to_circuit.
#'
#' @param quirk_url If given, the original URL from which the
#' JSON was parsed, as described in quirk_url_to_circuit.
#'
#' @param max_operation_count If the number of operations
#' in the circuit would exceed this value, the method raises
#' a ValueError instead of attempting to construct the circuit.
#' This is important to specify for servers parsing unknown input,
#' because Quirk’s format allows for a billion laughs attack in
#' the form of nested custom gates.
#'
#' @return The parsed circuit.
#'
#'
#' @section Raises:
#' ValueError: Invalid circuit URL, or circuit would be larger than `max_operations_count`.
#'
#' @export
quirk_json_to_circuit <- function(data, qubits, extra_cell_makers, quirk_url,
                                  max_operation_count) {

  args <- list(
    data = data,
    qubits = qubits,
    extra_cell_makers = extra_cell_makers,
    quirk_url = quirk_url,
    max_operation_count = as.integer(max_operation_count)
  )

  do.call(cirq$quirk_json_to_circuit, args)

}





#' @title Quirk url to circuit
#' @family Importing and Exporting
#' @description Parses a Cirq circuit out of a Quirk URL.
#'
#'
#' @param quirk_url The URL of a bookmarked Quirk circuit. It is not
#' required that the domain be "algassert.com/quirk". The only important
#' part of the URL is the fragment (the part after the #).
#'
#' @param qubits Qubits to use in the circuit. The length of the list
#' must be at least the number of qubits in the Quirk circuit
#' (including unused qubits). The maximum number of qubits in a
#' Quirk circuit is 16. This argument defaults to line_qubit_range(16)
#' when not specified.
#' @param extra_cell_makers Non-standard Quirk cells to accept.
#' This can be used to parse URLs that come from a modified version of
#' Quirk that includes gates that Quirk doesn’t define. This can be
#' specified as either a list of cirq.interop.quirk.cells.CellMaker
#' instances, or for more simple cases as a dictionary from
#' a Quirk id string to a cirq Gate.
#' @param max_operation_count If the number of operations in the circuit
#' would exceed this value, the method raises a ValueError instead of
#' attempting to construct the circuit. This is important to specify
#' for servers parsing unknown input, because Quirk’s format allows
#' for a billion laughs attack in the form of nested custom gates.
#' @return The parsed circuit.
#'
#' @section Raises:
#' ValueError: Invalid circuit URL, or circuit would be larger than `max_operations_count`.
#'
#' @export
quirk_url_to_circuit <- function(quirk_url,qubits,extra_cell_makers,max_operation_count) {

  args <- list(
    qubits = qubits,
    extra_cell_makers = extra_cell_makers,
    quirk_url = quirk_url,
    max_operation_count = as.integer(max_operation_count)
  )

  do.call(cirq$quirk_url_to_circuit, args)
}



