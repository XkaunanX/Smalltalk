| cola |
cola := Queue new.
cola enqueue: 1.
cola enqueue: 2.
cola enqueue: 3.
Transcript show: (cola dequeue) printString; nl.  "Devuelve 1"
