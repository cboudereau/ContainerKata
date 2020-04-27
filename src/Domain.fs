module Domain

type Container = { Capacity : Positive Size; Contents : Drum list; Features : ContainerFeature Set }
and Drum = { Size : Positive Size; Type : Chemical }
and Chemical = | TNT | Ammonia | BiologicalSamples | Sand //Order is important to pack specific chemical first
and ContainerFeature = | ArmoredContainer | VentilatedContainer
and Size<'a> = private Size of decimal 
    with static member (+) (Size x, Size y) = Size (x + y)
and Positive = interface end //FSharp Phantom type

//Services
type ContainerSpecification = Drum -> Container -> Container option
type PackingError = NoAnswerFound
type AddDrum = Drum -> Container -> Container option
//Now, no need to use exception when the return type is a value or a business error. The business error appears clearly into the DDD service
type Pack = Drum list -> Container list -> Result<Container list, PackingError>

module Container = 
    let empty capacity features = { Capacity=capacity; Features=features; Contents=[] }

module PositiveSize =
    let build x : Positive Size option = if x >= 0m then Some (Size x) else None
    let get x : Positive Size = build x |> function Some x -> x | None -> failwith "expected a positive size"
    let Zero = get 0m
    let value (Size x) = x
