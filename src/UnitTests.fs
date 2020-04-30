namespace DataContext
open Domain

module Drum = 
    let ammonia = { Size=PositiveSize.get 10m; Type=Ammonia } 
    let tnt = { Size=PositiveSize.get 10m; Type=TNT } 
    let biologicalSamples = { Size=PositiveSize.get 10m; Type=BiologicalSamples }
    let sand = { Size=PositiveSize.get 10m; Type = Sand }

module Container = 
    let big = Container.empty (PositiveSize.get 100m) Set.empty

namespace ``Given drums and incompatible containers, `` 

open Xunit
open Domain
open Core.Operators
open DataContext

module ``when packing them, then expect no answer found`` =
    let [<Fact>] ``no container nor drum``() = [] |> Containers.pack [] == Some []
    let [<Fact>] ``one container no drum``() = [Container.big] |> Containers.pack [] == Some [Container.big]
    let [<Fact>] ``no container but on drum`` () = [] |> Containers.pack [Drum.sand] == None
    let [<Fact>] ``one drum larger than the container`` () = [Container.big] |> Containers.pack [ { Drum.sand with Size = PositiveSize.get 101m } ] == None



namespace ``Given drums and compatible containers, `` 

open Xunit
open Domain
open Core.Operators
open DataContext

module ``when packing them, then expect then container with drums`` =
    
    let [<Fact>] ``100 container and 100 sand`` () = 
        let sand = { Drum.sand with Size = 100m |> PositiveSize.get }
        [Container.big] |> Containers.pack [ sand ] == Some [{ Container.big with Contents = [sand] }]
