﻿module Containers
open Domain

let size container = 
    container.Contents 
    |> List.fold (fun s x -> s + x.Size) PositiveSize.Zero

module Specifications = 
    //Container spectification spec combinator (kleisli) : spec1 AND spec2
    let (>=>) (spec1:ContainerSpecification) (spec2:ContainerSpecification) : ContainerSpecification = fun drum container -> spec1 drum container |> Option.bind (spec2 drum)
    
    //The main validate function which is a composition of specifications
    let validate : ContainerSpecification = 
        let checkSpaceSpec : ContainerSpecification = fun drum container -> 
            if size container + drum.Size > container.Capacity then None
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
let tryAdd : AddDrum = fun drum container -> 
    Specifications.validate drum container |> Option.map (fun c -> { c with Contents = drum :: c.Contents })

let pack : Pack = fun drums containers ->

    let drums = drums |> List.sortBy (fun x -> x.Type)

    let containers = containers |> List.sortBy (fun x -> x.Capacity)

    let tryAddContainer drum containers = 
        let rec tryAddContainer drum incompatibles candidates = 
            match candidates with
            | [] -> None
            | candidate::others -> 
                match tryAdd drum candidate with
                | None -> tryAddContainer drum (candidate :: incompatibles) others
                | Some compatible -> Some (List.append (List.rev (compatible :: incompatibles)) others)

        tryAddContainer drum [] containers

    let rec pack drums containers =
        match drums with
        | [] -> Ok containers
        | drum::others -> 
            match tryAddContainer drum containers with
            | Some compatibles -> pack others compatibles
            | None -> Error NoAnswerFound

    pack drums containers
