Introduction
============

Fast Froward is a probabilistic programming engine. It is centered around the idea of open probabilistic programming: instead of relying of a small set of building blocks (conjunctions, forall, ...), Fast Froward allows users to use an ever-expanding wide variety of constructs, such as spanning tree constraints, or cardinality formulae, provided by a set language/library designers. 

Fast Froward is based on the following paradigm: Probabilistic Programming amounts to composing and coupling objective functions over possible worlds. Crucially, each objective function comes with its own set of optimizers, and the optimizer for the composed objective itself is composed of these optimizers. This has an important consequence: the probabilistic programmer can exploit existing tailor-made inference/optimization routines for the tractable structure of its model. 

Technically Fast-Forward is relying on the idea of dual relaxation and decomposition. Here large optimization problems are split into several isolated ones that are tied together by coupling constraints on their variables. These constraints are transformed into terms of the Lagrangian. In this formulation optimizing (or finding the saddle points of) the global objective (say for MAP or Marginal inference) can be achieved by iteratively solving isolated sub-problems (which may be tractable), and passing "messages" that slightly alter the invidiual problems to coordinate global optimization. 

Installation with Maven
=======================

To compile and install fast-froward you first need to install maven (http://maven.apache.org/). Then compile and test the code

`mvn test`

If this works install fast-froward to your maven repo:

`mvn install`

Installation with SBT
=====================

To compile and run all tests

`sbt test`

To generate the fast-froward command line cd into `fast-froward-core` and call

`sbt generate-script`


Running Fast-Froward Scala Scripts
==================================

The fast-froward command line tool (`ff`) is nothing more than the Scala interpreter with the fast-froward class path injected. For convenience we recommend to add it to the `PATH` variable of your OS. The script is found under `fast-froward-core/target/scala_2.7.7` and can be used exactly like the `scala` interpreter. 

Say you have a scala script `Test.scala` that uses fast-froward, and you have some additional library in test.jar that Test.scala uses, then you can call it with

`ff -cp test.jar Test.scala`  