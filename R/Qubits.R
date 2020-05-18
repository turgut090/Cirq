#' @title Named Qubit
#'
#' @description A qubit identified by name.
#'
#' @details By default, NamedQubit has a lexicographic order. However, numbers within
#' the name are handled correctly. So, for example, if you print a circuit
#' containing `qubit_named('qubit22')` and `qubit_named('qubit3')`, the
#' wire for 'qubit3' will correctly come before 'qubit22'.
#'
#' @param name the name of the op
#'
#' @export
qubit_named <- function(name) {

  args <- list(
    name = name
  )

  do.call(cirq$NamedQubit, args)

}



#' @title Line Qubit
#'
#' @description A qubit on a 1d lattice with nearest-neighbor connectivity.
#'
#' @details LineQubits have a single attribute, and integer coordinate 'x', which
#' identifies the qubits location on the line. LineQubits are ordered by
#' this integer. One can construct new LineQubits by adding or subtracting
#' integers: >>> cirq.LineQubit(1) + 3 cirq.LineQubit(4) >>> cirq.LineQubit(2) - 1 cirq.LineQubit(1)
#'
#' @param x a linear array
#'
#' @export
qubit_line <- function(x) {

  args <- list(
    x = x
  )

  do.call(cirq$LineQubit, args)

}

#' @title Line Qubit range
#'
#' @param ... the length of parameter. E.g. the same with R base
#' function `seq(1,5,1)`. It returns the range in the form of list.
#'
#' @return a range of line qubits
#' @export
qubit_line_range <- function(...) {
  args = list(
    ...
  )

  if(is.numeric(args[[1]]))
    args[[1]] <- as.integer(args[[1]])

  do.call(cirq$LineQubit$range,args)

}

#' @title Grid Qubit
#'
#' @description A qubit on a 2d square lattice.
#'
#' @details GridQubits use row-major ordering:
#' GridQubit(0, 0) < GridQubit(0, 1) < GridQubit(1, 0) < GridQubit(1, 1)
#' New GridQubits can be constructed by adding or subtracting lists
#' >>> cirq.GridQubit(2, 3) + (3, 1) cirq.GridQubit(5, 4) >>> cirq.GridQubit(2, 3) - (1, 2) cirq.GridQubit(1, 1)
#'
#' @param row row number of a rectangular lattice.
#' @param col column number of a rectangular lattice.
#'
#' @export
qubit_grid <- function(row, col) {

  args <- list(
    row = as.integer(row),
    col = as.integer(col)
  )

  do.call(cirq$GridQubit, args)

}


#' @title Grid Qubits square
#'
#' @description a square of GridQubits.
#'
#'
#' @param diameter Length of a side of the square
#' @param top Row number of the topmost row
#' @param left Column number of the leftmost row
#'
#' @return A list of GridQubits filling in a square grid
#'
#' @export
qubit_grid_square <- function(diameter, top = 0, left = 0) {

  args <- list(
    diameter = as.integer(diameter),
    top = as.integer(top),
    left = as.integer(left)
  )

  do.call(cirq$GridQubit$square, args)

}



#' @title Num qubits
#'
#'
#'
#' @param val The value to get the number of qubits from.
#' @param default Determines the fallback behavior when `val` doesn't have a number of qubits.
#' If `default` is not set, a TypeError is raised. If default is set to a value, that value is returned.
#'
#' @return If `val` has a `_num_qubits_` method and its result is not NotImplemented, that
#' result is returned. Otherwise, if `val` has a `_qid_shape_` method, the number of qubits
#' is computed from the length of the shape and returned e.g. `len(shape)`.
#' If neither method returns a value other than NotImplemented and a default value
#' was specified, the default value is returned.
#'
#' @section Raises:
#' TypeError: `val` doesn't have either a `_num_qubits_` or a `_qid_shape_` method
#' (or they returned NotImplemented) and also no default value was specified.
#' @return the number of qubits, qudits, or qids `val` operates on.
#' @export
qubit_num <- function(val, default) {

  args <- list(
    val = val,
    default = default
  )

  do.call(cirq$num_qubits, args)

}







