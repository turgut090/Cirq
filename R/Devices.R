#' @title Unconstrained device
#' @family Devices
#' @description A device with no constraints on operations or qubits.
#'
#'
#' @return None
#' @export
unconstrained_device <- function(){

  cirq$UNCONSTRAINED_DEVICE

}

#' @title Device
#' @family Devices
#' @description Hardware constraints for validating circuits.
#' @param ... parameters to pass.
#'
#' @return device with options
#' @export
unconstrained_device <- function(...){

  args = list(...)

  if(length(args)==0)
    cirq$Device
  else
    do.call(cirq$Device, args)

}


#' @title Grid Qid
#' @family Devices
#' @description A qid on a 2d square lattice
#'
#' @details GridQid uses row-major ordering:
#' GridQid(0, 0, dimension=2) < GridQid(0, 1, dimension=2)
#' < GridQid(1, 0, dimension=2) < GridQid(1, 1, dimension=2)
#' New GridQid can be constructed by adding or subtracting lists
#' grid_qid(2, 3, dimension=2) + (3, 1) grid_qid(5, 4, dimension=2)
#' grid_qid(2, 3, dimension=2) - (1, 2) grid_qid(1, 1, dimension=2)
#'
#' @param row the row coordinate
#' @param col the column coordinate
#' @param dimension The dimension of the qid, e.g. the number of quantum levels.
#' @return grid qit
#' @export
grid_qid <- function(row, col, dimension) {

  if(missing(row) & missing(col) & missing(dimension))
    cirq$GridQid
  else
    cirq$GridQid(
      row = as.integer(row),
      col = as.integer(col),
      dimension = as.integer(dimension)
    )
}


#' @title Grid Qubit
#' @family Devices
#' @description A qubit on a 2d square lattice.
#'
#' @details GridQubits use row-major ordering:
#' GridQubit(0, 0) < GridQubit(0, 1) < GridQubit(1, 0) < GridQubit(1, 1)
#' New GridQubits can be constructed by adding or subtracting lists
#' grid_qubit(2, 3) + (3, 1) grid_qubit(5, 4)
#' grid_qubit(2, 3) - (1, 2) grid_qubit(1, 1)
#'
#' @param row row
#' @param col col
#' @return grid qubit
#' @export
grid_qubit <- function(row, col) {

  if(missing(row) & missing(col))
    cirq$GridQubit
  else
    cirq$GridQubit(
      row = as.integer(row),
      col = as.integer(col)
    )
}



#' @title Line Qid
#' @family Devices
#' @description A qid on a 1d lattice with nearest-neighbor connectivity.
#'
#' @details `LineQid`s have a single attribute, and integer coordinate 'x', which
#' identifies the qids location on the line. `LineQid`s are ordered by
#' this integer. One can construct new `LineQid`s by adding or subtracting integers:
#' line_qid(1, dimension=2) + 3
#' Output: cirq.LineQid(4, dimension=2)
#' line_qid(2, dimension=3) - 1
#' Output: cirq.LineQid(1, dimension=3)
#'
#' @param x The x coordinate.
#' @param dimension The dimension of the qid, e.g. the number of quantum levels.
#' @return initialized line qid at the given x coordinate.
#' @export
line_qid <- function(x, dimension) {

  if(missing(x) & missing(dimension))
    cirq$LineQid
  else
    cirq$LineQid(
      x = as.integer(x),
      dimension = as.integer(dimension)
    )

}


#' @title Line Qubit
#' @family Devices
#' @description A qubit on a 1d lattice with nearest-neighbor connectivity.
#'
#' @details LineQubits have a single attribute, and integer coordinate 'x', which
#' identifies the qubits location on the line. LineQubits are ordered by
#' this integer. One can construct new LineQubits by adding or subtracting integers:
#' line_qubitQubit(1) + 3
#' Output: cirq.LineQubit(4)
#' line_qubit(2) - 1
#' Output: cirq.LineQubit(1)
#'
#' @param x integer coordinate
#' @return initialized line qubit at the given x coordinate.
#' @export
line_qubit <- function(x) {

  if(missing(x) )
    cirq$LineQubit
  else
    cirq$LineQubit(
      x = as.integer(x)
    )
}



#' @title Named Qubit
#' @family Devices
#' @description A qubit identified by name.
#'
#' @details By default, NamedQubit has a lexicographic order. However, numbers within
#' the name are handled correctly. So, for example, if you print a circuit
#' containing `named_qubit('qubit22')` and `named_qubit('qubit3')`, the
#' wire for 'qubit3' will correctly come before 'qubit22'.
#'
#' @param name name of the op.
#' @return named qubit.
#' @export
named_qubit <- function(name) {

  if(missing(name) )
    cirq$NamedQubit
  else
    cirq$NamedQubit(
      name = name
    )
}


#' @title Qid
#' @family Devices
#' @description Identifies a quantum object such as a qubit, qudit, resonator, etc.
#'
#' @details Child classes represent specific types of objects, such as a qubit at a
#' particular location on a chip or a qubit with a particular name.
#' The main criteria that a custom qid must satisfy is comparability. Child
#' classes meet this criteria by implementing the _comparison_key method. For
#' example, cirq.LineQubit’s _comparison_key method returns self.x. This
#' ensures that line qubits with the same x are equal, and that line qubits
#' will be sorted ascending by x. Qid implements all equality,
#' comparison, and hashing methods via _comparison_key.
#'
#' @param ... parameters to pass.
#' @return qid object.
#' @export
qid <- function(...) {

  args = list(...)

  if(length(args)==0)
    cirq$Qid
  else
    do.call(cirq$Qid, args)
}


