## R interface to Cirq

The ```cirq``` package provides R wrappers to [Cirq](https://cirq.readthedocs.io/).

Cirq is a Python library for writing, manipulating, and optimizing quantum circuits and running them against quantum computers and simulators.

> [TensorFlow](https://www.tensorflow.org/quantum/concepts): Quantum computing relies on properties of quantum mechanics to compute problems that would be out of reach for classical computers. A quantum computer uses qubits. Qubits are like regular bits in a computer, but with the added ability to be put into a superposition and share entanglement with one another.


## Usage: basics

Using named qubits can be useful for abstract algorithmss well as algorithms not yet mapped onto hardware.
```
q0 = qubit_named('source')
q1 = qubit_named('target')
```

Line qubits can be created individually
```
q3 = qubit_line(3)
```

Or created in a range. This will create ```qubit_line(0)```, ```qubit_line(1)```, ```qubit_line(2)```.
```
c(q0, q1, q2) %<-% qubit_line_range(3)
```

Grid Qubits can also be referenced individually
```
q4_5 = qubit_grid(4,5)
```

Or created in bulk in a square. This will create 16 qubits from (0,0) to (3,3)
```
qubits = qubit_grid_square(4)
```


```
cirq:::cirq$google$Foxtail
```

There are also pre-packaged sets of qubits called Devices. These are qubits along with a set of rules of how they can be used. A cirq.Device can be used to apply adjacency rules and other hardware constraints to a quantum circuit. For our example, we will use the cirq.google.Foxtail device that comes with cirq. It is a 2x11 grid that mimics early hardware released by Google.

```
(0, 0)───(0, 1)───(0, 2)───(0, 3)───(0, 4)───(0, 5)───(0, 6)───(0, 7)───(0, 8)───(0, 9)───(0, 10)
│        │        │        │        │        │        │        │        │        │        │
│        │        │        │        │        │        │        │        │        │        │
(1, 0)───(1, 1)───(1, 2)───(1, 3)───(1, 4)───(1, 5)───(1, 6)───(1, 7)───(1, 8)───(1, 9)───(1, 10)
```
