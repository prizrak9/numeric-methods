// Learn more about F# at http://fsharp.org

open System

module List =
    let inline mapItem (i:int) func list =
        [
            yield! list |> List.take i
            yield func list.[i]
            yield! list |> List.skip (i + 1)
        ]
    let inline replaceItem (i:int) value list =
        mapItem i (fun _ -> value) list


let printF func arr =
    printfn "%A" (func arr)
    arr

let printM matrix =
    for i in 0 .. Array2D.length1 matrix - 1 do
        for j in 0 ..  Array2D.length2 matrix - 1 do
            printf "%-8.1f" matrix.[i,j]
            //if j < Array2D.length2 matrix - 1 then printf ";"
        printfn ""
    printfn ""

type Type = Max | Min

let inline chooseType t f1 f2 =
    match t with Max -> f1 | Min -> f2

let maybe f1 f2 condition =
    if condition then f1 else f2

let gauss minr minc matrix =
    [
        for i in 0 .. Array2D.length1 matrix - 1 do
            if i = minr
            then 
                yield 
                    [for a in matrix.[i,0..] ->
                        a / matrix.[minr, minc]]
            else
                yield 
                    [for j in 0 .. Array2D.length2 matrix - 1 ->
                        matrix.[i,j] + (matrix.[i,minc] / -matrix.[minr,minc]) * matrix.[minr,j]]
    ]
    |> array2D

let ans (matrix:float [,]) basis =
    let mergedBasis = List.zip (basis) (List.ofArray matrix.[1..,0])

    List.init (Array2D.length2 matrix) (fun i -> 
        match mergedBasis |> List.tryFind (fun (index,_) -> index - 1 = i) with None -> 0. | Some (_,value) -> value
    )

let rec iterate (matrix:float [,]) basis t =
    if matrix.[0,1..] |> Array.forall (chooseType t (<=) (>=) 0.)
    then matrix, basis
    else
        printM matrix

        // Min Element Column
        let minc = 
            matrix.[0,1..] 
            |> Array.mapi (fun i a -> i + 1,a) 
            |> chooseType t Array.minBy Array.maxBy snd
            |> fst 

        // Min Element Row
        let minr =
            matrix.[1..,minc] 

            // MRT test
            |> Array.mapi (fun i a -> i + 1, a) 
            |> Array.filter (fun (_,a) -> a > 0.)
            |> Array.map (fun (i,a) -> i, matrix.[i,0] / a) 
            |> printF (Array.map snd)

            // Ignore Infinity ratios.
            |> Array.filter (snd >> Double.IsInfinity >> maybe false true)
            |> Array.minBy snd
            |> fst 

        let basis = List.replaceItem (minr - 1) minc basis

        printfn "%A\n%A\n" (ans matrix basis) (minc, minr)

        let matrix = gauss minr minc matrix
           
        iterate matrix basis t

let solve matrix t =
    let basis =  List.init (Array2D.length1 matrix - 1) (fun a -> (Array2D.length2 matrix - Array2D.length1 matrix) + a + 1)
    let matrix, basis = iterate (matrix) basis t
    matrix, (ans matrix basis) |> List.take (basis |> List.length)



// matrix
// P F F F 0 0 0
// B x x x 1 0 0
// B x x x 0 1 0
// B x x x 0 0 1

// P -> max


let matrix =
    [
        [0.; -3.; -5.; -4.; 0.; 0.; 0.] // F
        [1100.; 0.1; 0.2; 0.4; 1.; 0.; 0.] // 9
        [120.; 0.05; 0.02; 0.02; 0.; 1.; 0.] // 10
        [8000.; 3.; 1.; 2.; 0.; 0.; 1.] // 11
    ]
    |> array2D

let matrix2 =
    [
        [0;     -6000;  -4000;  -3500;  2500;   4500;   1600;   4000;   6000;   2000;   2000;   3000;    1200;  2000;   2000;   1000;   1500;   6000;   2000;   0; 0; 0; 0; 0; 0; 0; 0; 0] 
        [5;     1;      1;      1;      1;      1;      1;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      1; 0; 0; 0; 0; 0; 0; 0; 0] 
        [4;     0;      0;      0;      0;      0;      0;      1;      1;      1;      1;      1;      1;      0;      0;      0;      0;      0;      0;      0; 1; 0; 0; 0; 0; 0; 0; 0]
        [5;     0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      1;      1;      1;      1;      1;      1;      0; 0; 1; 0; 0; 0; 0; 0; 0] 
        [300;   300;    0;      0;      0;      0;      0;      200;    0;      0;      0;      0;      0;      100;    0;      0;      0;      0;      0;      0; 0; 0; 1; 0; 0; 0; 0; 0]
        [200;   0;      100;    0;      0;      0;      0;      0;      150;    0;      0;      0;      0;      0;      50;     0;      0;      0;      0;      0; 0; 0; 0; 1; 0; 0; 0; 0]
        [600;   0;      0;      350;    0;      0;      0;      0;      0;      200;    0;      0;      0;      0;      0;      100;    0;      0;      0;      0; 0; 0; 0; 0; 1; 0; 0; 0]
        [100;   0;      0;      0;      50;     0;      0;      0;      0;      0;      40;     0;      0;      0;      0;      0;      30;     0;      0;      0; 0; 0; 0; 0; 0; 1; 0; 0]
        [200;   0;      0;      0;      0;      150;    0;      0;      0;      0;      0;      100;    0;      0;      0;      0;      0;      200;    0;      0; 0; 0; 0; 0; 0; 0; 1; 0]
        [100;   0;      0;      0;      0;      0;      40;     0;      0;      0;      0;      0;      30;     0;      0;      0;      0;      0;      50;     0; 0; 0; 0; 0; 0; 0; 0; 1]
    ]
    |> List.map (List.map float)
    |> array2D

let matrix4 =
    [
        [0;     -6000;  -4000;  -3500;  2500;   4500;   1600;   4000;   6000;   2000;   2000;   3000;    1200;  2000;   2000;   1000;   1500;   6000;   2000;   0; 0; 0; 0; 0; 0; 0; 0; 0] 
        [5;     1;      1;      1;      1;      1;      1;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      1; 0; 0; 0; 0; 0; 0; 0; 0] 
        [4;     0;      0;      0;      0;      0;      0;      1;      1;      1;      1;      1;      1;      0;      0;      0;      0;      0;      0;      0; 1; 0; 0; 0; 0; 0; 0; 0]
        [5;     0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      0;      1;      1;      1;      1;      1;      1;      0; 0; 1; 0; 0; 0; 0; 0; 0] 
        [-300;  -300;   0;      0;      0;      0;      0;      -200;   0;      0;      0;      0;      0;      -100;   0;      0;      0;      0;      0;      0; 0; 0; 1; 0; 0; 0; 0; 0]
        [-200;  0;      -100;   0;      0;      0;      0;      0;      -150;   0;      0;      0;      0;      0;      -50;    0;      0;      0;      0;      0; 0; 0; 0; 1; 0; 0; 0; 0]
        [-600;  0;      0;      -350;   0;      0;      0;      0;      0;      -200;   0;      0;      0;      0;      0;      -100;   0;      0;      0;      0; 0; 0; 0; 0; 1; 0; 0; 0]
        [-100;  0;      0;      0;      -50;    0;      0;      0;      0;      0;      -40;    0;      0;      0;      0;      0;      -30;    0;      0;      0; 0; 0; 0; 0; 0; 1; 0; 0]
        [-200;  0;      0;      0;      0;      -150;   0;      0;      0;      0;      0;      -100;   0;      0;      0;      0;      0;      -200;   0;      0; 0; 0; 0; 0; 0; 0; 1; 0]
        [-100;  0;      0;      0;      0;      0;      -40;    0;      0;      0;      0;      0;      -30;    0;      0;      0;      0;      0;      -50;    0; 0; 0; 0; 0; 0; 0; 0; 1]
    ]
    |> List.map (List.map float)
    |> array2D


let matrix3 =
    [
        [0;     1000;   500;    700;    400;    1200;   500;    500;    350;    0;      0;      0;      0] 
        [14;    6;      4;      5;      3;      0;      0;      0;      0;      1;      0;      0;      0] 
        [14;    0;      0;      0;      0;      5;      6;      7;      5;      0;      1;      0;      0]
        [1;     1;      1;      1;      1;      0;      0;      0;      0;      0;      0;      1;      0] 
        [1;     0;      0;      0;      0;      1;      1;      1;      1;      0;      0;      0;      1]
    ]
    |> List.map (List.map float)
    |> array2D

let matrix6 =
    [
        [0;     1000;   500;    700;    400;    1200;   500;    500;    350;    0;      0;      0;      0;      0;      0] 
        [14;    6;      4;      5;      3;      0;      0;      0;      0;      1;      0;      0;      0;      0;      0] 
        [14;    0;      0;      0;      0;      5;      6;      7;      5;      0;      1;      0;      0;      0;      0]
        [1;     1;      0;      0;      0;      1;      0;      0;      0;      0;      0;      1;      0;      0;      0] 
        [1;     0;      1;      0;      0;      0;      1;      0;      0;      0;      0;      0;      1;      0;      0]
        [1;     0;      0;      1;      0;      0;      0;      1;      0;      0;      0;      0;      0;      1;      0]
        [1;     0;      0;      0;      1;      0;      0;      0;      1;      0;      0;      0;      0;      0;      1]
    ]
    |> List.map (List.map float)
    |> array2D

let matrix8 =
    [
        [0;     1000;   500;    700;    400;    1200;   500;    500;    350;    0;      0;      0;      0;      0;      0;      0] 
        [14;    6;      4;      5;      3;      0;      0;      0;      0;      1;      0;      0;      0;      0;      0;      0] 
        [14;    0;      0;      0;      0;      5;      6;      7;      5;      0;      1;      0;      0;      0;      0;      0]
        [1;     1;      0;      0;      0;      1;      0;      0;      0;      0;      0;      1;      0;      0;      0;      0] 
        [1;     0;      1;      0;      0;      0;      1;      0;      0;      0;      0;      0;      1;      0;      0;      0]
        [1;     0;      0;      1;      0;      0;      0;      1;      0;      0;      0;      0;      0;      1;      0;      0]
        [1;     0;      0;      0;      1;      0;      0;      0;      1;      0;      0;      0;      0;      0;      1;      0]
        [4;     1;      1;      1;      1;      1;      1;      1;      1;      0;      0;      0;      0;      0;      0;      1]
    ]
    |> List.map (List.map float)
    |> array2D
    

let matrix5 =
    [
        [0;3;     1;     3;      2;      4;      1;     3;        0;      0;      0] 
        [200;0;    1;      2;      0;      1;      3;      4;         1;      0;      0] 
        [350;0;    1;      0;      3;      2;      1;      0;         0;      1;      0]
        [400;2;     1;      1;      0;      0;      0;      0;         0;      0;      1] 
    ]
    |> List.map (List.map float)
    |> array2D

let matrix7 =
    [
        [0;-2;     -3;     0;      0;      0;    ] 
        [40;-2;    6;      1;      0;      0;   ] 
        [28;3;    2;      0;      1;      0;   ]
        [14;2;     -1;      0;      0;      1;  ] 
    ]
    |> List.map (List.map float)
    |> array2D




let m, answer = solve matrix6 Min
//let m, answer = solve matrix7 Max
//let m, ans = solve matrix2 Max

printM m
printfn "%A" answer

