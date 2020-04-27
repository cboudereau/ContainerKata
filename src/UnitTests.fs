﻿namespace ``Given drums and incompatible containers, `` 
    open Xunit
    open Domain
    open Core.Operators
    
    module ``when packing them, then expect no answer found`` =
        let container = { Capacity = PositiveSize.get 100m; Contents = []; Features = set [] }
        let ammonia = { Size=PositiveSize.get 10m; Type=Ammonia } 
        let tnt = { Size=PositiveSize.get 10m; Type=TNT } 
        let biologicalSamples = { Size=PositiveSize.get 10m; Type=BiologicalSamples }
        let sand = { Size=PositiveSize.get 10m; Type = Sand }

        let [<Fact>] ``one drum larger than the container`` () = [container] |> Containers.pack [ { sand with Size = PositiveSize.get 101m } ] == None
        let [<Fact>] ``one incompatible container with ammonia`` ()= [container] |> Containers.pack [ammonia] == None
        let [<Fact>] ``one incompatible container with tnt`` ()= [container] |> Containers.pack [tnt] == None
        let [<Fact>] ``tnt and biological samples are incompatible``() = 
            [{ container with Features = set [ ArmoredContainer ]}] |> Containers.pack [tnt; biologicalSamples] == None
            [{ container with Features = set [ ArmoredContainer; VentilatedContainer ]}] |> Containers.pack [ammonia; tnt; biologicalSamples] == None

namespace ``Given drums and compatible containers, ``
    open Xunit
    open Domain
    open Core.Operators

    module ``when packing them, then expect filled containers`` =
        let container = { Capacity = PositiveSize.get 100m; Contents = []; Features = set [] }
        let ammonia = { Size=PositiveSize.get 10m; Type=Ammonia } 
        let tnt = { Size=PositiveSize.get 10m; Type=TNT } 
        let biologicalSamples = { Size=PositiveSize.get 10m; Type=BiologicalSamples }
        let sand = { Size=PositiveSize.get 10m; Type = Sand }
       
        let test drums containers = 
            let prune = List.sort
            let filled = containers |> Option.get |> List.collect (fun x -> x.Contents)
            prune filled == prune drums

        let [<Fact>] ``empty containers, empty drums`` () = Containers.pack [] [] == (Some [])
        let [<Fact>] ``one container, empty drums`` () = Containers.pack [] [container] == Some [container]
        let [<Fact>] ``one container full after adding the sand`` () = 
            [container] |> Containers.pack [ { sand with Size = PositiveSize.get 10m } ] == Some [{ container with Contents = [ { sand with Size = PositiveSize.get 10m }] }]
        let [<Fact>] ``one ventilated for ammonia`` ()=
            [{ container with Features = set [ VentilatedContainer ] }] |> Containers.pack [ ammonia ] == Some [{ container with Features = set [ VentilatedContainer ]; Contents = [ammonia] }]
        let [<Fact>] ``one armored container for TNT``()=
            [{ container with Features = set [ ArmoredContainer ]}] |> Containers.pack [ tnt ] == Some [{ container with Features=set [ ArmoredContainer ]; Contents = [tnt]}]
        let [<Fact>] ``one armored ventilated container for ammonia and TNT``()=
            [{ container with Features = set [ ArmoredContainer; VentilatedContainer ]}] |> Containers.pack [tnt;ammonia] == Some [{ container with Features = set [ ArmoredContainer; VentilatedContainer ]; Contents = [ammonia; tnt] }]

        let [<Fact>] ``multiple containers and drums``()=
            let ammonia = { ammonia with Size = PositiveSize.get 100m }
            let sand = { sand with Size = PositiveSize.get 100m } 
            let tnt = { tnt with Size = PositiveSize.get 100m }
            let ventilatedContainer = { container with Features = set [ VentilatedContainer ] }
            let armoredContainer = { container with Features = set [ ArmoredContainer ] }
            
            let drums = [ ammonia; sand; tnt ]

            [ armoredContainer; container; ventilatedContainer ]
            |> Containers.pack drums 
            |> test drums

        let [<Fact>] ``different sizes, but fit exactly the container capacity`` () =
            let nine = PositiveSize.get 9m
            let three = PositiveSize.get 3m
            
            let drums = [{ sand with Size = three }
                         { sand with Size = nine }]

            let containers = [{ container with Capacity = nine }
                              { container with Capacity = three }]

            Containers.pack drums containers  |> test drums

        let [<Fact>] ``different sizes than container capacity`` () =
            let nine = PositiveSize.get 9m
            let three = PositiveSize.get 3m
            let ten = PositiveSize.get 10m
            
            let drums = [{ sand with Size = three }
                         { sand with Size = nine }
                         { sand with Size = nine }
                         { sand with Size = ten } ]

            let containers = [{ container with Capacity = nine + ten }
                              { container with Capacity = nine + three }]

            Containers.pack drums containers |> test drums 

        //Test extracted from PropertyBasedTests
        //https://en.wikipedia.org/wiki/Knapsack_problem
        let ``2 are bigger than others`` () =
            let nine = PositiveSize.get 9m
            let ten = PositiveSize.get 10m
            let eight = PositiveSize.get 8m
            let two = PositiveSize.get 2m
            let drums = 
                [ { sand with Size = eight }
                  { sand with Size = ten }
                  { sand with Size = two }
                  { sand with Size = nine } ]
            
            let containers = 
                [ { container with Capacity = nine + eight }
                  { container with Capacity = ten + two } ]

            containers |> Containers.pack drums |> test drums