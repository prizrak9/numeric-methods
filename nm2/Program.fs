
module List =
    let inline mapItem (i:int) func list =
        [
            yield! list |> List.take i
            yield func list.[i]
            yield! list |> List.skip (i + 1)
        ]

    let inline findAndReplace e ne list =
        [
            for l in list ->
                if l = e then ne else l
        ]



// i - machine type
// j - product type

let ``number of machine types`` = 3

let ``number of machines for each machine type`` =
    [
        5
        4
        5
    ]

let ``number of product types`` = 6

let ``number of products 1 machine can produce for each machine type`` = 
    [
        // machine type
        [
            // product type quantity
            300
            100
            350
            50
            150
            40
        ]
        [
            200
            150
            200
            40
            100
            30
        ]
        [
            100
            50
            100
            30
            200
            50
        ]
    ]

let ``plan of production for each product type`` =
    [
        300
        200
        600
        100
        200
        100
    ]

let ``revenue from selling one product for each product type`` =
    [
        20
        40
        10
        50
        30
        40
    ]



let ``is the number of machines used less than the total number of machines for machine type`` u i =
    let ui = u |> List.sumBy (List.item i)
    0 <= ui && ui <= (``number of machines for each machine type`` |> List.item i)

let ``is the number of products that can be produced more than the plan for each product type`` u j =
    let uij i = u |> List.item i |> List.item j
    let aij i = ``number of products 1 machine can produce for each machine type`` |> List.item i |> List.item j
    [for i in 0 .. ``number of machine types`` - 1  -> uij i * aij i] |> List.sum >= ``plan of production for each product type``.[j]


let ``revenue from selling all produced products`` u =
    [
        for i in 0 .. ``number of machine types`` - 1 do
            for j in 0 .. ``number of product types`` - 1 ->
                let uij = u |> List.item i |> List.item j
                let rj = ``revenue from selling one product for each product type`` |> List.item j
                let aij = ``number of products 1 machine can produce for each machine type`` |> List.item i |> List.item j
                uij * rj * aij
    ] |> List.sum

type MachineMoving =
    | Process of int list
    // Machines reached non-existing product type.
    // It means that there are no possible combinations left.
    | Finish

let ``all combinations of machine type and product type`` =
    let minPosition = -1;

    let machinePositionsForProductType i = List.init ``number of machines for each machine type``.[i] (fun _ -> minPosition)

    let moveMachines machinePositions =
        let minPos = machinePositions |> List.min
        let nextPos = minPos + 1

        if nextPos = ``number of product types`` 
        then Finish
        else
            // Pick one machine at the lowest position to move up.
            let machineToMoveIndex = machinePositions |> List.findIndex ((=) minPos)

            // Move chosen machine up.
            let newMachinePositions = machinePositions |> List.mapItem machineToMoveIndex (fun _ -> nextPos)
            
            // Reset positions of machines at the lowest position.
            let newMachinePositions = newMachinePositions |> List.map (fun a -> if a = minPos then minPosition else a)

            Process newMachinePositions

    let countMachinesForProductType machinePositions =
        let rec inFunc positionsLeft acc =
            match positionsLeft with
            | [] -> acc
            | h::t -> 
                if h < 0
                then inFunc t acc
                else inFunc t (acc |> List.mapItem h (fun a -> a + 1))
        inFunc machinePositions (List.init ``number of product types`` (fun _ -> 0))

    let rec allCombinations machinePositions acc =
        match machinePositions with
        | Process positions -> allCombinations (moveMachines positions) (positions::acc)
        | Finish -> acc
       

    let machineVariations =
        [
            for i in 0 .. ``number of machine types`` - 1 ->
                allCombinations (Process (machinePositionsForProductType i)) []
                |> List.map (fun a -> countMachinesForProductType a)
        ]

    // The number of combinations is insane. > 4e7
    let machineVariationsAllPossibleCombinations =
        seq {
            for i in machineVariations.[0] do
                for j in machineVariations.[1] do
                    for k in machineVariations.[2] ->
                        [i;j;k]
        }


    machineVariationsAllPossibleCombinations, machineVariations |> List.map List.length |> List.sum


//let rec combs lst n acc =
//    if n = 1 then [for i in lst -> i::acc]
//    else
//        [
//            for i in 0 .. List.length lst - n + 1 do
//                yield! combs lst.[i+1..] (n-1) (lst.[i]::acc)
//        ]
//let kkk = combs [1;2;3;4] 3 []

let acceptableConfigurations =
    seq {
        for u in fst ``all combinations of machine type and product type`` do
            if
                List.init ``number of product types`` (fun j -> ``is the number of products that can be produced more than the plan for each product type`` u j)
                |> List.forall id
            then
                yield u
    }


// Imperative but simple.
let mutable max:int list list * int = [],0

for u in acceptableConfigurations do
    let r = u, ``revenue from selling all produced products`` u
    if snd r > snd max
    then
        max <- r
        printfn "%A" max

printfn "\n\nComplete! The best configuration:\n%A" max


//let res1 = 
//    //``all combinations of machine type and product type``
//    res
//    //|> Seq.take 1000
//    |> Seq.map (fun u -> (u, ``revenue from selling all produced products`` u))
//    //|> List.ofSeq
//    //|> List.sortByDescending (fun (u,r) -> r)
//    |> Seq.maxBy (fun (u,r) -> r)

//printfn "%A" res1
