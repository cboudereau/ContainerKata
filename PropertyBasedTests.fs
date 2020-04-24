namespace ``Space allocation properties``

open Domain
open FsCheck
open FsCheck.Xunit

module Sand = 
    let randomList = 
        let one = 
            gen { 
                let! drums = 
                    Gen.elements [ 1m .. 1m .. 10m ]
                    |> Gen.listOfLength 1 //Change this parameter to 2 to get the issue
                    |> Gen.map (List.map (fun x -> { Type=Sand; Size = PositiveSize.get x }))
                let totalSize = drums |> List.fold (fun x y -> x + y.Size) PositiveSize.Zero
                let container = Container.empty totalSize Set.empty
                return container, drums }

        let list count = 
            List.init count (fun _ -> one)
            |> Gen.sequence

        gen {
            let! countainersCount = Gen.choose (1, 2)
            let! fullContainers = list countainersCount 
            let (containers, drums) = fullContainers |> List.fold (fun (containers, drums) (c,d) -> c::containers, List.append drums d) ([],[]) 
            let! containers = Gen.shuffle containers
            let! drums = Gen.shuffle drums
            return drums |> Array.toList, containers |> Array.toList
        }

    type FullContainer = static member Values() = Arb.fromGen randomList

    let [<Property (Arbitrary=[| typeof<FullContainer> |])>] ``Given drums which fit exactly in containers, when packing them, then expect all containers full of drums`` (drums, containers) =
        let test drums containers = 
            let filled = containers |> List.collect (fun x -> x.Contents)
            let prune x = x |> List.sort
            prune filled = prune drums

        containers |> Containers.pack drums |> Result.get |> test drums
        

