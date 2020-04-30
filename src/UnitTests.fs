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
    let [<Fact>] ``one drum larger than the container`` () = [Container.big] |> Containers.pack [ { Drum.sand with Size = PositiveSize.get 101m } ] == None
    let [<Fact>] ``one incompatible container with ammonia`` ()= [Container.big] |> Containers.pack [Drum.ammonia] == None
    let [<Fact>] ``one incompatible container with tnt`` ()= [Container.big] |> Containers.pack [Drum.tnt] == None
    let [<Fact>] ``tnt and biological samples are incompatible``() = 
        [{ Container.big with Features = set [ ArmoredContainer ]}] |> Containers.pack [Drum.tnt; Drum.biologicalSamples] == None
        [{ Container.big with Features = set [ ArmoredContainer; VentilatedContainer ]}] |> Containers.pack [Drum.ammonia; Drum.tnt; Drum.biologicalSamples] == None

namespace ``Given drums and compatible containers, ``
open Xunit
open Domain
open Core.Operators
open DataContext

module ``when packing them, then expect filled containers`` =
    let test drums containers = 
        let prune = List.sort
        let filled = containers |> Option.get |> List.collect (fun x -> x.Contents)
        prune filled == prune drums

    let [<Fact>] ``empty containers, empty drums`` () = Containers.pack [] [] == (Some [])
    let [<Fact>] ``one container, empty drums`` () = Containers.pack [] [Container.big] == Some [Container.big]
    let [<Fact>] ``one container full after adding the sand`` () = 
        [Container.big] |> Containers.pack [ { Drum.sand with Size = PositiveSize.get 10m } ] == Some [{ Container.big with Contents = [ { Drum.sand with Size = PositiveSize.get 10m }] }]
    let [<Fact>] ``one ventilated for ammonia`` ()=
        [{ Container.big with Features = set [ VentilatedContainer ] }] |> Containers.pack [ Drum.ammonia ] == Some [{ Container.big with Features = set [ VentilatedContainer ]; Contents = [Drum.ammonia] }]
    let [<Fact>] ``one armored container for TNT``()=
        [{ Container.big with Features = set [ ArmoredContainer ]}] |> Containers.pack [ Drum.tnt ] == Some [{ Container.big with Features=set [ ArmoredContainer ]; Contents = [Drum.tnt]}]
    let [<Fact>] ``one armored ventilated container for ammonia and TNT``()=
        [{ Container.big with Features = set [ ArmoredContainer; VentilatedContainer ]}] |> Containers.pack [Drum.tnt;Drum.ammonia] == Some [{ Container.big with Features = set [ ArmoredContainer; VentilatedContainer ]; Contents = [Drum.ammonia; Drum.tnt] }]

    let [<Fact>] ``multiple containers and drums``()=
        let ammonia = { Drum.ammonia with Size = PositiveSize.get 100m }
        let sand = { Drum.sand with Size = PositiveSize.get 100m } 
        let tnt = { Drum.tnt with Size = PositiveSize.get 100m }
        let ventilatedContainer = { Container.big with Features = set [ VentilatedContainer ] }
        let armoredContainer = { Container.big with Features = set [ ArmoredContainer ] }
        
        let drums = [ ammonia; sand; tnt ]

        [ armoredContainer; Container.big; ventilatedContainer ]
        |> Containers.pack drums 
        |> test drums

    let [<Fact>] ``different sizes, but fit exactly the container capacity`` () =
        let nine = PositiveSize.get 9m
        let three = PositiveSize.get 3m
        
        let drums = [{ Drum.sand with Size = three }
                     { Drum.sand with Size = nine }]

        let containers = [{ Container.big with Capacity = nine }
                          { Container.big with Capacity = three }]

        Containers.pack drums containers  |> test drums

    let [<Fact>] ``different sizes than container capacity`` () =
        let nine = PositiveSize.get 9m
        let three = PositiveSize.get 3m
        let ten = PositiveSize.get 10m
        
        let drums = [{ Drum.sand with Size = three }
                     { Drum.sand with Size = nine }
                     { Drum.sand with Size = nine }
                     { Drum.sand with Size = ten } ]

        let containers = [{ Container.big with Capacity = nine + ten }
                          { Container.big with Capacity = nine + three }]

        Containers.pack drums containers |> test drums 

    //Test extracted from PropertyBasedTests
    //https://en.wikipedia.org/wiki/Knapsack_problem
    let ``2 are bigger than others`` () =
        let nine = PositiveSize.get 9m
        let ten = PositiveSize.get 10m
        let eight = PositiveSize.get 8m
        let two = PositiveSize.get 2m
        let drums = 
            [ { Drum.sand with Size = eight }
              { Drum.sand with Size = ten }
              { Drum.sand with Size = two }
              { Drum.sand with Size = nine } ]
        
        let containers = 
            [ { Container.big with Capacity = nine + eight }
              { Container.big with Capacity = ten + two } ]

        containers |> Containers.pack drums |> test drums