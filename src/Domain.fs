module Domain

type PositiveSize = private PositiveSize of decimal 
    with static member (+) (PositiveSize x, PositiveSize y) = PositiveSize (x + y)
         static member Zero = PositiveSize 0m
type Chemical = | TNT | Ammonia | BiologicalSamples | Sand //Order is important to pack specific chemical first
type Drum = { Size : PositiveSize; Type : Chemical }
type ContainerFeature = | ArmoredContainer | VentilatedContainer
type Container = { Capacity : PositiveSize; Contents : Drum list; Features : ContainerFeature Set }

//Services
type ContainerSpecification = Drum -> Container -> Container option
//Now, no need to use exception when the return type is a value or a business error. The business error appears clearly into the DDD service
type Pack = Drum list -> Container list -> Container list option

module PositiveSize =
    let build x = if x >= 0m then Some (PositiveSize x) else None
    let get x = build x |> function Some x -> x | None -> failwith "expected a positive size"
    let value (PositiveSize x) = x

module Container = 
    let empty capacity features = { Capacity=capacity; Features=features; Contents = [] }
    let size container = container.Contents |> List.sumBy (fun x -> x.Size)
