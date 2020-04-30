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
        failwith "not yet implemented" 
