module Containers
open Domain

module Specifications = 
    //Container spectification spec combinator (kleisli) : spec1 AND spec2
    let andThen (spec2:ContainerSpecification) (spec1:ContainerSpecification) 
        : ContainerSpecification = fun drum container ->
        match spec1 drum container with
        | Some candidate -> spec2 drum candidate
        | None -> None 
    
    let (>=>) (spec1:ContainerSpecification) (spec2:ContainerSpecification) : ContainerSpecification = andThen spec2 spec1 
    
    //The main validate function which is a composition of specifications
    let validate : ContainerSpecification = 
        let checkSpaceSpec : ContainerSpecification = fun drum container -> 
            if Container.size container + drum.Size > container.Capacity then None
            else Some container

        let checkBiologicalSpec : ContainerSpecification =
            let spec x y = 
                match x, y with
                | TNT, BiologicalSamples | BiologicalSamples, TNT -> false
                | _ -> true
            fun drum container ->
                if container.Contents |> List.forall (fun x -> spec x.Type drum.Type) then
                    Some container 
                else None
        
        let checkFeature chemical feature : ContainerSpecification = 
            fun drum container -> 
                if drum.Type <> chemical then Some container
                elif container.Features |> Set.exists ((=)feature) then Some container
                else None
        let checkTNTSpec : ContainerSpecification = checkFeature TNT ArmoredContainer
        let checkAmmoniaSpec : ContainerSpecification = checkFeature Ammonia VentilatedContainer

        //Combine specs together with the composition operator over ContainerSpecification. Order is important to fail fast
        checkSpaceSpec 
        >=> checkBiologicalSpec 
        >=> checkTNTSpec 
        >=> checkAmmoniaSpec

//Container functions
let pack : Pack = 
    let tryAdd drum container =
        Specifications.validate drum container |> Option.map (fun c -> { c with Contents = drum :: c.Contents })

    let rec pack drums packed candidates = 
        match drums, candidates with
        | [], [] -> Some packed
        | [], candidates -> Some (List.append packed candidates)
        | _::_, [] -> None
        | drum::drums, candidate::candidates ->
            match tryAdd drum candidate with
            | None -> pack (drum::drums) (candidate::packed) candidates
            | Some candidate -> pack drums [] (List.append packed (candidate::candidates))
    
    fun drums containers ->
        let drums = drums |> List.sortBy (fun x -> x.Type)
        let containers = containers |> List.sortBy (fun x -> x.Capacity)
        pack drums [] containers
