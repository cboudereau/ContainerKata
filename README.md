# ContainerKata

Find the best solution to pack drums into containers by respecting the following rules : 

- The *container* has a *capacity* limit and drums have different *size*
- All *chemicals* in a container cannot exceed the *capacity* limit.
- *TNT* drums require *armored* *container*
- *Ammonia* drums require *ventilated* *container*
- *Biological samples* and *TNT* should be separated.
- *Water* and *Biological samples* do not require any *container feature*.

## DDD

Design matters : how it is possible to represent actual and expected behavior whithout a good design first ?

### Types

#### Domain
- Types with primitives
- Fix primitives

#### Services
- Identity Services
- Identify where to not use exceptions

## Implementation

### Unit Test in the REPL

#### Little demo

### Container specification Tests

#### Size for sand
#### TNT and Ammonia
#### Biological and TNT

## Switch to Unit Test automation

## What is the limit of our solution ?
- Is it easy to enumerate ?

Add unit test to prove the allocation space problem.
Identity through types cardinality.

- What are properties ?
- Is it easy to prove
- It is easy to verify ?