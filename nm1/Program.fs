open System


module HalfDivision =
    // E - precision
    let min E func range =
        let rec inFunc left right =
            if right - left < E
            then 
                if func(left) < func(right)
                then left
                else right
            else
                let center = (left + right) / 2.

                if func(left) < func(right)
                then inFunc left center
                else inFunc center right

        inFunc (fst range) (snd range)

module RandDivision =
    // count - number of points to generate
    let min count func range =
        let start = fst range
        let size = snd range - start
        let rnd = Random()

        List.init count (ignore >> rnd.NextDouble)
        |> List.map ((*) size >> (+) start)
        |> List.map (fun x -> x, func(x))
        |> List.minBy snd
        |> fst

module GradientMethod =
    type Vector2 = 
        { x:float; y:float }
    
        static member private applyRight f a b = { x = f a b.x; y = f a b.y }
        static member private applyLeft f a b = { x = f a.x b; y = f a.y b }
        static member private applyBoth f a b = { x = f a.x b.x; y = f a.y b.y }
        static member private apply f a = { x = f a.x; y = f a.y }
    
        static member sqrMagnitude a = a.x ** 2. + a.y ** 2.
        static member magnitude a = a |> Vector2.sqrMagnitude |> sqrt
        static member norm a = 
            match Vector2.magnitude a with
            | 1. -> a
            | m -> Vector2.applyLeft (/) a m
    
        static member (*) (a,b) = Vector2.applyBoth (*) a b 
        static member (*) (a,b) = Vector2.applyLeft (*) a b
        static member (*) (a,b) = Vector2.applyRight (*) a b
        static member (+) (a,b) = Vector2.applyBoth (+) a b
        static member (/) (a,b) = Vector2.applyBoth (/) a b
        static member (/) (a,b) = Vector2.applyLeft (/) a b
        static member (-) (a,b) = Vector2.applyBoth (-) a b
        static member (~-) a = Vector2.apply (~-) a

        override this.ToString() = sprintf "(%f, %f)" this.x this.y
    
    // E - precision
    // Let given function be paraboloid.
    // Given that we won't calculate derivative on each iteration.
    // A finishing condition will be compairing values of consecutive points.
    let minGeneral E func (grad:(Vector2 -> float) list) start startStep =
        let step = 
            let grad = { x = grad.[0] start; y = grad.[1] start }
            let delta = func (start + grad) - func start

            if delta < 0. then grad else -grad
            |> Vector2.norm
            |> (*) startStep
            
        let rec inFunc current (step:Vector2) =
            // All derivatives are less than E
            if grad |> List.forall (fun g -> g(current) < E)
            then current
            else
                let next = current + step

                if func next - func current < 0.
                then inFunc next step
                else inFunc current (step * 0.5)

        inFunc start step

    let min E func grad start startStep =
        let func r = func r.x r.y
        
        let grad = grad |> List.map (fun f -> (fun r -> f r.x r.y))

        let start = { x = fst start; y = snd start }

        minGeneral E func grad start startStep


let task1() =

    let k = 2.
    let g = 8.

    let func x = x**2. + 2.*k*g*x + k
    let range = -k*g - 2. , k*g + 1.

    let E = 0.01
    printfn "Half division with E=%f: %f" E (HalfDivision.min E func range)

    let count = 100
    printfn "Random division with count=%i: %f" count (RandDivision.min count func range)

let task2() =
    let k = 2.
    let g = 8.

    let func x y = x**2. + y**2. - 2.*k*g*x + k

    //let h = func 16. 0.

    //let func x y = (x**2. - 2.*k*g*x + (k*g)**2.) + y**2. + k - (k*g)**2.
    //let func x y = (x - k*g)**2. + y**2. + k - (k*g)**2
    // (k*g)**2 - k = (x - k*g)**2. + y**2. + k
    // center = (k*g;0)
    let grad =
        [
            fun x _ -> 2.*x - 2.*k*g
            fun _ y -> 2.*y
        ]

    let start = k*g + 2. , 2.
    let startStep = 1.

    let min = GradientMethod.min 0.01 func grad start startStep 

    printfn 
        """Gradient method with start=%A, startStep=%f: %O
Value in this point: %f
Analytical min is (16, 0) with value -254""" 
        start 
        startStep 
        min
        (func min.x min.y)



task1()
task2()