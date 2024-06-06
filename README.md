# Prelude

Yet another repository containing the solutions for the exercises proposed in the programming classic *Structure and Interpretation of Computer Programs* [*SICP*]. Authored by Harold Abelson and Gerald Jay Sussman [with Julie Sussman], *SICP* was first published in 1984 by MIT Press and soon became one of the most acclaimed books on programming of all time.

Rightfully known as *The Wizard Book*, *SICP* stands out for its unique approach to programming and computer science. Abelson paradoxically asseverates:

> Computer science is not a science. It's also not really very much about computers...

Rather, it is analogized with magic. A particularly memorable and amusing aphorism by Sussman perfectly captures the parallelism:

> The way you write a recursive procedure is by wishful thinking. You have to believe!

*SICP* is impregnated with with alluring ideas like this, definitely captivating the inspirited reader from first to last page.

Although sometimes quite difficult to read [the exercises don´t get any easier] it completely worths the effort and the sporadic frustration: *SICP's* ability for presenting a truly abundant and diverse set of computer science [or magic] fundamentals in such a well-structured manner is rather remarkable.

The second edition of the book [published in 1996] has kindly been made available for free and may be downloaded at <https://web.mit.edu/6.001/6.037/sicp.pdf> [see copy at *sicp.pdf*]. Additionally, a set of twenty video lectures given in July 1986 for Hewlett-Packard employees, and professionally produced by Hewlett-Packard Television may be found at <https://ocw.mit.edu/courses/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video_galleries/video-lectures/>. <br>

# Table of Contents

## 1 &nbsp; Building Abstractions with Procedures

### 1.1 &nbsp; The Elements of Programming

**[Video Lecture 01A: Overview and Introduction to Lisp]** <br>
1.1.1 &nbsp; Expressions <br>
1.1.2 &nbsp; Naming and the Environment <br>
1.1.3 &nbsp; Evaluating Combinations <br>
1.1.4 &nbsp; Compound Procedures <br>
1.1.5 &nbsp; The Substitution Model for Procedure Application <br>
1.1.6 &nbsp; Conditional Expressions and Predicates **[1.1 to 1.5]** <br>
1.1.7 &nbsp; Example: Square Roots by Newton’s Method **[1.6 to 1.8]** <br>
1.1.8 &nbsp; Procedures as Black-Box Abstractions <br>

### 1.2 &nbsp; Procedures and the Processes They Generate

**[Video Lecture 01B: Procedures and Processes; Substitution Model]** <br>
1.2.1 &nbsp; Linear Recursion and Iteration **[1.9 and 1.10]** <br>
1.2.2 &nbsp; Tree Recursion **[1.11 to 1.13]** <br>
1.2.3 &nbsp; Orders of Growth **[1.14 and 1.15]** <br>
1.2.4 &nbsp; Exponentiation **[1.16 to 1.19]** <br>
1.2.5 &nbsp; Greatest Common Divisors **[1.20]** <br>
1.2.6 &nbsp; Example: Testing for Primality **[1.21 to 1.28]** <br>

### 1.3 &nbsp; Formulating Abstractions with Higher-Order Procedures

**[Video Lecture 02A: Higher-order Procedures]** <br>
1.3.1 &nbsp; Procedures as Arguments **[1.29 to 1.33]** <br>
1.3.2 &nbsp; Constructing Procedures Using Lambda **[1.34]** <br>
1.3.3 &nbsp; Procedures as General Methods **[1.35 to 1.39]** <br>
1.3.4 &nbsp; Procedures as Returned Values **[1.40 to 1.46]** <br>

## 2 &nbsp; Building Abstractions with Data

### 2.1 &nbsp; Introduction to Data Abstraction

**[Video Lecture 02B: Compound Data]** <br>
2.1.1 &nbsp; Example: Arithmetic Operations for Rational Numbers **[2.1]** <br>
2.1.2 &nbsp; Abstraction Barriers **[2.2 and 2.3]** <br>
2.1.3 &nbsp; What Is Meant by Data? **[2.4 to 2.6]** <br>
2.1.4 &nbsp; Extended Exercise: Interval Arithmetic **[2.7 to 2.16]** <br>

### 2.2 &nbsp; Hierarchical Data and the Closure Property

**[Video Lecture 03A: Henderson Escher Example]** <br>
2.2.1 &nbsp; Representing Sequences **[2.17 to 2.23]** <br>
2.2.2 &nbsp; Hierarchical Structures **[2.24 to 2.32]** <br>
2.2.3 &nbsp; Sequences as Conventional Interfaces **[2.33 to 2.43]** <br>
2.2.4 &nbsp; Example: A Picture Language **[2.44 to 2.52]** <br>

### 2.3 &nbsp; Symbolic Data

**[Video Lecture 03B: Symbolic Differentiation; Quotation]** <br>
**[Video Lecture 04A: Pattern Matching and Rule-based Substitution]** <br>
2.3.1 &nbsp; Quotation **[2.53 to 2.55]** <br>
2.3.2 &nbsp; Example: Symbolic Differentiation **[2.56 to 2.58]** <br>
2.3.3 &nbsp; Example: Representing Sets **[2.59 to 2.66]** <br>
2.3.4 &nbsp; Example: Huffman Encoding Trees **[2.67 to 2.72]** <br>

### 2.4 &nbsp; Multiple Representations for Abstract Data

**[Video Lecture 04B: Generic Operators]** <br>
2.4.1 &nbsp; Representations for Complex Numbers <br>
2.4.2 &nbsp; Tagged data <br>
2.4.3 &nbsp; Data-Directed Programming and Additivity **[2.73 to 2.77]** <br>

### 2.5 &nbsp; Systems with Generic Operations

2.5.1 &nbsp; Generic Arithmetic Operations **[2.78 to 2.80]** <br>
2.5.2 &nbsp; Combining Data of Different Types **[2.81 to 2.86]** <br>
2.5.3 &nbsp; Example: Symbolic Algebra **[2.87 to 2.97]** <br>

## 3 &nbsp; Modularity, Objects, and State

### 3.1 &nbsp; Assignment and Local State

**[Video Lecture 05A: Assignment, State, and Side-effects]** <br>
3.1.1 &nbsp; Local State Variables **[3.1 to 3.4]** <br>
3.1.2 &nbsp; The Benefits of Introducing Assignment **[3.5 and 3.6]** <br>
3.1.3 &nbsp; The Costs of Introducing Assignment **[3.7 and 3.8]** <br>

### 3.2 &nbsp; The Environment Model of Evaluation

3.2.1 &nbsp; The Rules for Evaluation <br>
3.2.2 &nbsp; Applying Simple Procedures <br>
3.2.3 &nbsp; Frames as the Repository of Local State **[3.9 and 3.10]** <br>
3.2.4 &nbsp; Internal Definitions **[3.11]** <br>

### 3.3 &nbsp; Modeling with Mutable Data

**[Video Lecture 05B: Computational Objects]** <br>
3.3.1 &nbsp; Mutable List Structure **[3.12 to 3.20]** <br>
3.3.2 &nbsp; Representing Queues **[3.21 to 3.23]** <br>
3.3.3 &nbsp; Representing Tables **[3.24 to 3.27]** <br>
3.3.4 &nbsp; A Simulator for Digital Circuits **[3.28 to 3.32]** <br>
3.3.5 &nbsp; Propagation of Constraints **[3.33 to 3.37]** <br>

### 3.4 &nbsp; Concurrency: Time Is of the Essence

3.4.1 &nbsp; The Nature of Time in Concurrent Systems **[3.38]** <br>
3.4.2 &nbsp; Mechanisms for Controlling Concurrency **[3.39 to 3.49]** <br>

### 3.5 &nbsp; Streams

**[Video Lecture 06A: Streams, Part 1]** <br>
**[Video Lecture 06B: Streams, Part 2]** <br>
3.5.1 &nbsp; Streams Are Delayed Lists **[3.50 to 3.52]** <br>
3.5.2 &nbsp; Infinite Streams **[3.53 to 3.62]** <br>
3.5.3 &nbsp; Exploiting the Stream Paradigm **[3.63 to 3.76]** <br>
3.5.4 &nbsp; Streams and Delayed Evaluation **[3.77 to 3.82]** <br>

## 4 &nbsp; Metalinguistic Abstraction

### 4.1 &nbsp; The Metacircular Evaluator

**[Video Lecture 07A: Metacircular Evaluator, Part 1]** <br>
4.1.1 &nbsp; The Core of the Evaluator **[4.1]** <br>
4.1.2 &nbsp; Representing Expressions **[4.2 to 4.10]** <br>
4.1.3 &nbsp; Evaluator Data Structures **[4.11 to 4.13]** <br>
4.1.4 &nbsp; Running the Evaluator as a Program **[4.14]** <br>
4.1.5 &nbsp; Data as Programs **[4.15]** <br>
4.1.6 &nbsp; Internal Definitions **[4.16 to 4.21]** <br>
4.1.7 &nbsp; Separating Syntactic Analysis from Execution **[4.22 to 4.24]** <br>

### 4.2 &nbsp; Variations on a Scheme-Lazy Evaluation

**[Video Lecture 07B: Metacircular Evaluator, Part 2]** <br>
4.2.1 &nbsp; Normal Order and Applicative Order **[4.25 and 4.26]** <br>
4.2.2 &nbsp; An Interpreter with Lazy Evaluation **[4.27 to 4.31]** <br>
4.2.3 &nbsp; Streams as Lazy Lists **[4.32 to 4.34]** <br>

### 4.3 &nbsp; Variations on a Scheme-Nondeterministic Computing

4.3.1 &nbsp; Amb and Search **[4.35 to 4.37]** <br>
4.3.2 &nbsp; Examples of Nondeterministic Programs **[4.38 to 4.49]** <br>
4.3.3 &nbsp; Implementing the Amb Evaluator **[4.50 to 4.54]** <br>

### 4.4 &nbsp; Logic Programming

**[Video Lecture 08A: Logic Programming, Part 1]** <br>
**[Video Lecture 08B: Logic Programming, Part 2]** <br>
4.4.1 &nbsp; Deductive Information Retrieval **[4.55 to 4.63]** <br>
4.4.2 &nbsp; How the Query System Works <br>
4.4.3 &nbsp; Is Logic Programming Mathematical Logic? **[4.64 to 4.69]** <br>
4.4.4 &nbsp; Implementing the Query System **[4.70 to 4.79]** <br>

## 5 &nbsp; Computing with Register Machines

### 5.1 &nbsp; Designing Register Machines

**[Video Lecture 09A: Register Machines]** <br>
**[5.1]** <br>
5.1.1 &nbsp; A Language for Describing Register Machines **[5.2]** <br>
5.1.2 &nbsp; Abstraction in Machine Design **[5.3]** <br>
5.1.3 &nbsp; Subroutines <br>
5.1.4 &nbsp; Using a Stack to Implement Recursion **[5.4 to 5.6]** <br>
5.1.5 &nbsp; Instruction Summary <br>

### 5.2 &nbsp; A Register-Machine Simulator

**[Video Lecture 09B: Explicit-control Evaluator]** <br>
**[5.7]** <br>
5.2.1 &nbsp; The Machine Model <br>
5.2.2 &nbsp; The Assembler **[5.8]** <br>
5.2.3 &nbsp; Generating Execution Procedures for Instructions **[5.9 to 5.13]** <br>
5.2.4 &nbsp; Monitoring Machine Performance **[5.14 to 5.19]** <br>

### 5.3 &nbsp; Storage Allocation and Garbage Collection

**[Video Lecture 10B: Storage Allocation and Garbage Collection]** <br>
5.3.1 &nbsp; Memory as Vectors **[5.20 to 5.22]** <br>
5.3.2 &nbsp; Maintaining the Illusion of Infinite Memory <br>

### 5.4 &nbsp; The Explicit-Control Evaluator

5.4.1 &nbsp; The Core of the Explicit-Control Evaluator <br>
5.4.2 &nbsp; Sequence Evaluation and Tail Recursion <br>
5.4.3 &nbsp; Conditionals, Assignments, and Definitions **[5.23 to 5.25]** <br>
5.4.4 &nbsp; Running the Evaluator **[5.26 to 5.30]** <br>

### 5.5 &nbsp; Compilation

**[Video Lecture 10A: Compilation]** <br>
5.5.1 &nbsp; Structure of the Compiler **[5.31 and 5.32]** <br>
5.5.2 &nbsp; Compiling Expressions <br>
5.5.3 &nbsp; Compiling Combinations <br>
5.5.4 &nbsp; Combining Instruction Sequences <br>
5.5.5 &nbsp; An Example of Compiled Code **[5.33 to 5.38]** <br>
5.5.6 &nbsp; Lexical Addressing **[5.39 to 5.44]** <br>
5.5.7 &nbsp; Interfacing Compiled Code to the Evaluator **[5.45 to 5.52]** <br>
