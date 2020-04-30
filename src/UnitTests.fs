namespace ``Given drums and incompatible containers, `` 
    open Xunit
    open Domain
    open Core.Operators
    
    module Drum = 
        let ammonia = { Size=PositiveSize.get 10m; Type=Ammonia } 
        let tnt = { Size=PositiveSize.get 10m; Type=TNT } 
        let biologicalSamples = { Size=PositiveSize.get 10m; Type=BiologicalSamples }
        let sand = { Size=PositiveSize.get 10m; Type = Sand }

    module Container = 
        let big = Container.empty (PositiveSize.get 100m) Set.empty

    module ``when packing them, then expect no answer found`` =
        let [<Fact>] ``one drum larger than the container`` () = [Container.big] |> Containers.pack [ { Drum.sand with Size = PositiveSize.get 101m } ] == None
