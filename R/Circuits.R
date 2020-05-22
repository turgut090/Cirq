#' @title Op tree
#' @family Circuits
#' @param ... parameters to pass.
#' @description An operation or nested collections of operations.
#' @return operations
#' @export
tree_op <- function(...) {
  args = list(...)

  if(length(args)==0)
    cirq$OP_TREE
  else
    do.call(cirq$OP_TREE, args)
}


#' @title Flatten op tree
#'
#' @description Performs an in-order iteration of the operations (leaves) in an OP_TREE.
#'
#' @family Circuits
#' @param root The operation or tree of operations to iterate.
#' @param preserve_moments Whether to yield Moments intact instead of flattening them.
#'
#'
#' @section Raises:
#' TypeError: root isn't a valid OP_TREE.
#' @return Operations from the tree.
#' @export
tree_flatten_op <- function(root, preserve_moments = FALSE) {

  cirq$flatten_op_tree(
    root = root,
    preserve_moments = preserve_moments
  )

}


#' @title Freeze op tree
#'
#' @description Replaces all iterables in the OP_TREE with lists.
#'
#'
#' @param root The operation or tree of operations to freeze.
#'
#' @return An OP_TREE with the same operations and branching structure,
#' but where all internal nodes are lists instead of arbitrary iterables.
#' @return An OP_TREE with the same operations and branching structure, but
#' where all internal nodes are list of multiple types instead of
#' arbitrary iterables.
#' @export
tree_freeze_op <- function(root) {

  cirq$freeze_op_tree(
    root = root
  )

}


#' @title Transform op tree
#'
#' @description Maps transformation functions onto the nodes of an OP_TREE.
#'
#'
#' @param root The operation or tree of operations to transform.
#' @param op_transformation How to transform the operations
#' (i.e. leaves).
#' @param iter_transformation How to transform the iterables
#' (i.e. internal nodes).
#' @param preserve_moments Whether to leave Moments alone. If TRUE,
#' the transformation functions will not be applied to Moments or
#' the operations within them.
#'
#' @return A transformed operation tree.
#'
#' @section Raises:
#' TypeError: root isn't a valid OP_TREE.
#'
#' @export
tree_transform_op <- function(root, op_transformation, iter_transformation, preserve_moments = FALSE) {

  cirq$transform_op_tree(
    root = root,
    op_transformation = op_transformation,
    iter_transformation = iter_transformation,
    preserve_moments = preserve_moments
  )

}


#' @title Circut
#' @family Circuits
#' @description A mutable list of groups of operations to apply to some qubits
#' @return operations.
#' @export
circuit <- function() {
  cirq$Circuit
}


#' @title Circuit Dag
#' @family Circuits
#' @description A representation of a Circuit as a directed acyclic graph.
#'
#' @details Nodes of the graph are instances of Unique containing each operation of a
#' circuit. Edges of the graph are lists of nodes. Each edge specifies a required
#' application order between two operations. The first must be applied before
#' the second. The graph is maximalist (transitive completion).
#'
#' @param can_reorder A predicate that determines if two operations may be reordered.
#' Graph edges are created for pairs of operations where this returns False.
#' The default predicate allows reordering only when the operations don’t share common qubits.
#' @param incoming_graph_data Data in initialize the graph. This can be any value
#' supported by networkx.DiGraph() e.g. an edge list or another graph.
#' @param device  Hardware that the circuit should be able to run on.
#' @return Initialized Circuit Dag.
#' @export
circuit_dag <- function(can_reorder, incoming_graph_data = NULL,
                       device = unconstrained_device()) {

  if(missing(can_reorder))
    cirq$CircuitDag
  else
    cirq$CircuitDag(
      can_reorder = can_reorder,
      incoming_graph_data = incoming_graph_data,
      device = device
    )

}

#' @title Unconstrained device
#'
#' @description A device that allows everything, infinitely fast.
#'
#'
#' @return list of ops.
#' @export
unconstrained_device <- function(){

  cirq$UNCONSTRAINED_DEVICE()

}

#' @title Gate Operation
#' @family Circuits
#' @description An application of a gate to a sequence of qubits.
#'
#'
#' @param gate the gate to apply.
#'
#' @param qubits the qubits to operate on.
#'
#' @return operation applied to the sequence of qubits.
#' @export
gate_operation_ <- function(gate, qubits){

  if(missing(gate) & missing(qubits))
    cirq$GateOperation
  else {
    cirq$GateOperation(gate = gate,
                       qubits = qubits)
  }

}


#' @title Insert Strategy
#' @family Circuits
#' @description Indicates preferences on how to add multiple
#' operations to a circuit.
#'
#'
#' @param ... parameters to pass.
#' @return added operation to a circuit
#' @export
insert_strategy <- function(...) {

  args = list(...)
  if(length(args)==0)
    cirq$InsertStrategy
  else
    do.call(cirq$InsertStrategy, args)


}



#' @title Moment
#' @family Circuits
#' @description A time-slice of operations within a circuit.
#' Grouping operations into moments is intended to be a strong suggestion to
#' whatever is scheduling operations on real hardware. Operations in the same
#' moment should execute at the same time (to the extent possible; not all
#' operations have the same duration) and it is expected that all operations
#' in a moment should be completed before beginning the next moment.
#' @details Moment can be indexed by qubit or list of qubits:
#' moment[qubit] returns the Operation in the moment which touches the
#' given qubit, or throws KeyError if there is no such operation.
#' moment[qubits] returns another Moment which consists only of those
#' operations which touch at least one of the given qubits. If there
#' are no such operations, returns an empty Moment.
#'
#' @param ... parameters to pass.
#' @return constructed moment with the given operations.
#' @export
moment <- function(...) {

  args = list(...)
  if(length(args)==0)
    cirq$Moment
  else
    do.call(cirq$Moment, args)

}


#' @title Parallel Gate Operation
#' @family Circuits
#' @description An application of several copies of a gate to a group of qubits.
#'
#'
#' @param ... parameters to pass.
#' @return applied operations to the qubits.
#' @export
gate_parallel_operation <- function(...) {

  args = list(...)
  if(length(args)==0)
    cirq$ParallelGateOperation
  else
    do.call(cirq$ParallelGateOperation, args)
}



#' @title Qubit Order
#' @family Circuits
#' @description Defines the kronecker product order of qubits.
#'
#'
#' @param ... parameters to pass.
#' @return None
#' @export
qubit_order <- function(...) {

  args = list(...)
  if(length(args)==0)
    cirq$QubitOrder
  else
    do.call(cirq$QubitOrder, args)
}




#' @title Qubit Order or List
#' @family Circuits
#' @description Defines the kronecker product order of qubits.
#'
#'
#' @param ... parameters to pass.
#' @return None
#' @export
qubit_order_or_list <- function(...) {

  args = list(...)
  if(length(args)==0)
    cirq$QubitOrderOrList
  else
    do.call(cirq$QubitOrderOrList, args)
}



#' @title Unique
#'
#' @description A wrapper for a value that doesn't compare equal
#' to other instances.
#'
#' @details For example: 5 == 5 but Unique(5) != Unique(5). Unique is used by
#' CircuitDag to wrap operations because nodes in a graph
#' are considered the same node if they compare equal to each other. X(q0)
#' in one moment of a Circuit and X(q0) in another moment of the Circuit are
#' wrapped by Unique(X(q0)) so they are distinct nodes in the graph.
#'
#' @param ... parameters to pass.
#' @return None.
#' @export
cirquit_unique <- function(...) {

  args = list(...)
  if(length(args)==0)
    cirq$Unique
  else
    do.call(cirq$Unique, args)
}




