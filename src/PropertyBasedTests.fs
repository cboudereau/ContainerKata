namespace ``Space allocation properties``

open Domain
open FsCheck
open FsCheck.Xunit

module Sand = 
    type FullContainer = 
        static member Values() = 
            let fullFilledContainers count = 
                let fullFilledContainer = 
                    gen { 
                        let! drums = 
                            Gen.elements [ 1m .. 1m .. 10m ]
                            |> Gen.listOfLength 1 //Change this parameter to 2 to get the issue
                            |> Gen.map (List.map (fun x -> { Type=Sand; Size = PositiveSize.get x }))
                        let totalSize = drums |> List.fold (fun x y -> x + y.Size) PositiveSize.Zero
                        let container = Container.empty totalSize Set.empty
                        return container, drums }

                gen {
                    let! countainersCount = count
                    return! List.init countainersCount (fun _ -> fullFilledContainer) |> Gen.sequence 
                }

            gen {
                let! fullContainers = Gen.choose (1, 2) |> fullFilledContainers 
                let (containers, drums) = fullContainers |> List.fold (fun (containers, drums) (c,d) -> c::containers, List.append drums d) ([],[]) 
                let! containers = Gen.shuffle containers
                let! drums = Gen.shuffle drums
                return drums |> Array.toList, containers |> Array.toList
            } |> Arb.fromGen 

    let [<Property (Arbitrary=[| typeof<FullContainer> |])>] ``Given drums which fit exactly in containers, when packing them, then expect all containers full filled`` (drums, containers) =
        let test drums containers = 
            let filled = containers |> List.collect (fun x -> x.Contents)
            let prune x = x |> List.sort
            prune filled = prune drums

        containers |> Containers.pack drums |> Option.get |> test drums
        

