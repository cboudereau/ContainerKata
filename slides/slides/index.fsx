
(**
- title : Cargo Kata
- description : Introduction to Type Driven Development
- author : Clement Boudereau
- theme : sky
- transition : default

***

### Cargo Kata specifications

*"Find the best solution to pack drums into containers by respecting the following rules"* 

---

#### Rules 
- The **container** has a **capacity** limit and drums have different **size**
- All **chemicals** in a container cannot exceed the **capacity** limit.
- **TNT** drums require **armored** **container**
- **Ammonia** drums require **ventilated** **container**
- **Biological samples** and **TNT** should be separated.
- **Water** and **Biological samples** do not require any **container feature**.

***

### TDD

Let's start to write our first Unit Test...
*)

let expected = failwith "not yet implemented"
let actual = failwith "not yet implemented"

actual |> should equal expected

(**

---

#### How to describe types ?

*"Design matters : how it is possible to represent actual behavior without a good expected design first ?"*

---
#### Type Driven Development at the rescue!

---
#### Design in the REPL

- What is a REPL ?
- How to write test in the REPL ? 

---
#### Domain Types
*)

type Chemical = Water

type Drum = 
    { Type : Chemical
      Size : decimal }

type Container = 
    { Capacity : decimal 
      Drums : Drum list }

(**
---

#### Primitive obsession : Potential bugs !
- Does the Size be **negative** ?
- Does Size multiplied by Size matter ?
...

---
#### Make illegal states unrepresentable !
*)

type PositiveSize = private PositiveSize of decimal
    with static member (+) (PositiveSize x, PositiveSize y) = PositiveSize (x + y)
         static member Zero = PositiveSize 0m
module PositiveSize = 
    let build x = if x >= 0m then Some (PositiveSize x) else None
    let get x = 
        build x 
        |> function Some x -> x | None -> failwith "expected a positive size"

type Chemical = Water

type Drum = 
    { Type : Chemical
      Size : PositiveSize }

type Container = 
    { Capacity : PositiveSize 
      Drums : Drum list }

module Container =
    let empty capacity = { Capacity=capacity; Drums = [] } 

(**
---
#### Domain Services
*)

type ContainerSpecification = Drum -> Container -> Container option
type Pack = Drum list -> Container list -> Container list option

(**
---

#### And here is our first test!

*)

let container = 100m |> PositiveSize.get |> Container.empty

let checkSize : ContainerSpecification = 
    fun drum container ->failwith "not yet implemented"

let water =
    { Type = Water
      Size = PositiveSize.get 10m }

container |> checkSize water = Some { container with Drums = [water] }
container |> checkSize { water with Size = PositiveSize.get 11m } = None

(**
---

#### Small implementation
*)
let totalSize container = container.Drums |> List.sumBy (fun x -> x.Size)
let checkSize : ContainerSpecification = fun drum container ->
    if ((container |> totalSize) + drum.Size) > container.Capacity then None
    else Some container


(**
---
#### Biological samples and TNT should be separated 

*)

type Chemical = Water | TNT | BiologicalSample
let tnt = { Type = TNT; Size = PositiveSize.get 1m }
let biologicalSample = { Type = TNT; Size = PositiveSize.get 1m }

let checkBiologicalSpec : ContainerSpecification = failwith "not yet implemented"

{ container with Drums = [ tnt ] } 
|> checkBiologicalSpec biologicalSample = None

{ container with Drums = [ biologicalSample ] } 
|> checkBiologicalSpec tnt = None

let expected = Some { container with Drums = [ biologicalSample ] }
container 
|> checkBiologicalSpec biologicalSample = expected
    


(**
---

#### Small implementation
*)

let checkBiologicalSpec : ContainerSpecification =
    let spec x y = 
        match x, y with
        | TNT, BiologicalSample | BiologicalSample, TNT -> false
        | _ -> true
    fun drum container ->
        if container.Drums |> List.forall (fun x -> spec x.Type drum.Type) then
            Some container 
        else None

(**
---

#### Composing both checkSize and checkBiologicalSpec ?

***

### Function Composition

![lego](images/lego.png)

---

#### First version

*)

let validate : ContainerSpecification = fun drum container ->
    match checkSize drum container with
    | None -> None
    | Some candidate -> checkBiologicalSpec drum container

(**
---

#### What if adding more and more rules ?
*)

let rule3 : ContainerSpecification = 
    fun drum container -> failwith "not yet implemented"
let rule4 : ContainerSpecification = 
    fun drum container -> failwith "not yet implemented"

let validate : ContainerSpecification = fun drum container ->
    match checkSize drum container with
    | None -> None
    | Some candidate -> 
        match checkBiologicalSpec drum container with
        | None -> None
        | Some candidate -> 
            match rule3 drum candidate with
            | None -> None
            | Some candidate -> rule4 drum candidate

(**
---

#### Too much Complexity!

How to reduce complexity ?
*)

(**
---

#### "and then" aka Kleisli composition

*)

let andThen (rule1:ContainerSpecification) (rule2:ContainerSpecification) 
    : ContainerSpecification = fun drum container ->
    match rule1 drum container with
    | Some candidate -> rule2 drum candidate
    | None -> None

(**
---

#### Rewrite it again 

*)

let validate = 
    checkSize
    |> andThen checkBiologicalSpec
    |> andThen rule3
    |> andThen rule4

(**
---

#### Kleisli / "and then" FISH operator

*)

let (>=>) rule1 rule2 = andThen rule1 rule2


let validate = 
    checkSize
    >=> checkBiologicalSpec
    >=> rule3
    >=> rule4

(**
---

#### List of rules

*)
let validate rules : ContainerSpecification = 
    let zero : ContainerSpecification = fun _ container -> Some container
    rules |> List.fold andThen zero

let rules : ContainerSpecification list = 
    [ checkSize; checkBiologicalSpec; rule3; rule4 ]

let allRules : ContainerSpecification = rules |> validate

(**
---

#### SOLID PRINCIPLES

Function composition all the way down!

' S : Single responsability : one function to do one thing
' O : one function "andThen" to open for extension but rules functions are not modifiable
' L : Only one "ContainerSpecification" type and different implementations
' I : the function type is the Interface
' D : Dependency inversion by using function composition

---
### Full implementation demo

' full demo
' property based testing

*)

(**

***

### When using Property Based Testing ?
- Is it hard to enumerate ?
- What are properties ?
- Is it easy to prove ?
- It is easy to verify ?
*)