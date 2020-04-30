module Containers
open Domain

module Specifications = 
    //The main validate function which is a composition of specifications
    let validate : ContainerSpecification = 
        let checkSpaceSpec : ContainerSpecification = fun drum container -> 
            if Container.size container + drum.Size > container.Capacity then None
            else Some container

        //Combine specs together with the composition operator over ContainerSpecification. Order is important to fail fast
        checkSpaceSpec 

//Container functions
let pack : Pack = 
    fun drums containers ->
        let tryAdd drum container = 
            Specifications.validate drum container |> Option.map (fun c -> { c with Contents = drum :: c.Contents })
        
        let rec pack drums packed containers = 
            match drums, containers with
            | [], [] -> Some packed
            | [], candidates -> Some (List.append packed candidates)
            | _ :: _, [] -> None
            | drum :: drums, candidate :: candidates ->
                match tryAdd drum candidate with
                | Some candidate -> pack drums [] (List.append packed (candidate :: candidates)) 
                | None -> pack (drum::drums) (candidate::packed) candidates
            
        
        pack drums [] containers
