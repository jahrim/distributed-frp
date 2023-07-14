# FRASP: Scala-based DSL for Distribured FRP
Functional Reactive Approach to Self-organisation Programming (FRASP) is a reactive *self-organisation* programming
language that enables the decoupling of program logic from its scheduling.

This project is aim to be used as research product, since it is still not ready for industrial applications.

The framework il publicly available on maven central, so you can include it in your preffered build system:
```gradle
dependencies {
  implementation("io.github.cric96:distributed-frp_3:0.1.3")
  implementation("org.scala-lang:scala3-library_3:3.2.2")
}
```
```sbt
libraryDependecies += "io.github.cric96" %% "distributed-frp" % "0.1.3"
```

[Here](https://github.com/AggregateComputing/experiment-2023-acsos-distributed-frp) there is a showcase in which we used this library combined with Alchemist -- a simulator for large-scale pervasive systems.

This is a fork from the original work made by [Francesco Dente](https://github.com/francescodente) with few changes to support the `share` operator.
For more details, please refer to the [Francesco thesis](https://github.com/francescodente/distributed-frp-thesis/releases/download/0.1.2%2B2023-03-16-01-57/thesis-main-0.1.2+2023-03-16-01-57.pdf)
## Main Concept

### Datatypes

The system follows the Functional Reactive Programming (FRP) paradigm to express a self-organizing collective computation as a graph of reactive sub-computations. 
Each sub-computation is referred to as a "flow" and is represented programmatically using the `Flow[T]` type, where `T` is the type of the output of the computation.
A `Flow` is essentially a function that takes a `Context` and returns a time-varying value represented as a cell of `Export`s. 
The `Flow` may depend on the exports of other `Flow`s recursively.

### Local Values

The language provides constructs for local and atomic operations that do not depend on other flows or neighbors:

- `constant(e)` returns a constant flow that always evaluates to the provided argument.
- `sensor(name)` returns the flow of values produced by the sensor with the given `name`.
- `mid()` is a shortcut for `sensor("mid")` and returns the constant flow of the device ID.

### Choice

The `mux(c,t,e)` expression allows conditional branching based on a Boolean flow `c`. 
It returns a flow that outputs the value of flow `t` when `c` is true and the value of flow `e` when `c` is false. 
This construct enables conditional behavior without resulting in partitions in the device network.

### Interaction with Neighbors

Communication with neighbors is facilitated through the `nbr(f)` construct, which handles communication in both directions at once. 
It takes a flow `f` as a parameter, and the local output of `f` is automatically sent to neighbors. 
The output of the whole `nbr(f)` expression is an object called `NeighborField[T]`, which collects the values of `f` computed by all neighbors.

A special kind of sensor, called `nbrSensor(name)`, provides a value for each neighbor. 
For example, the built-in function `nbrRange` returns a flow of `NeighborField[Double]`, 
representing the neighboring field of estimated distances to neighbors.

### Branching

The `branch(c,t,e)` expression enables computation branching based on the evaluation of the Boolean expression `c`. 
It returns the value of expression `t` when `c` is true and the value of expression `e` when `c` is false. 
Devices executing different branches won't interact with each other, 
allowing for distinct sub-computations. 
This concept is called "alignment," and devices can dynamically switch branches based on sensor values.

### Lifting

Lifting allows the combination of multiple flows together. 
The expression `lift(f1,f2,...,fN)(g)` applies function `g` to the outputs of flows `f1` to `fN`. 
Lifting can also be applied to flows of `NeighborField`s, combining values from the input `NeighborField`s neighbor-wise. 
For example, lifting `nbr(mid(),nbrRange())` with the function `(nbrId,nbrDist) => s"${nbrId} is at distance ${nbrDist} from me"` yields a flow that reports the ID and distance for each neighbor.

### Looping (State Evolution)

The `loop(init,ft)` construct
evolves a piece of state, starting from an initial value `init`, by applying function `ft` to the flow of the previous state, 
resulting in the flow of the next state. 
This construct enables stateful computations. 
The pace of state evolution depends on the implementation of the `Context`, which may provide a default throttling period. 
An alternative implementation of `loop` could accept a stream to dictate the pace explicitly.
