open System
open System.Threading.Tasks
open System.Diagnostics

// Дополнительные сведения о F# см. на http://fsharp.org
// Дополнительную справку см. в проекте "Учебник по F#".

 
let mult (m1: int [,]) (m2: int [,]) =
    if m1.GetLength(1) <> m2.GetLength(0)
    then None
    else let l, n, m = m1.GetLength(0), m2.GetLength(1), m2.GetLength(0)
         let m3 = Array2D.create l n 0
         [ for i in 0 .. l - 1 do
                for j in 0 .. n - 1 do
                    for k in 0 .. m - 1 do
                        m3.[i,j] <- m3.[i,j] + m1.[i,k] * m2.[k,j]
         ] |> ignore
         Some m3
 
 
let asyncMult nthreads (m1: int [,]) (m2: int [,]) =
    if m1.GetLength(1) <> m2.GetLength(0)
    then None
    else let l, n, m = m1.GetLength(0), m2.GetLength(1), m2.GetLength(0)
         let m3 = Array2D.create l n 0
         [ for r in List.splitInto nthreads [0 .. l - 1] ->
            async {
            for i in r do
                for j in 0 .. n - 1 do
                    for k in 0 .. m - 1 do
                        m3.[i,j] <- m3.[i,j] + m1.[i,k] * m2.[k,j]
         } ]
         |> Async.Parallel
         |> Async.RunSynchronously
         |> ignore
         Some m3
 


let threadsMult (m1: int [,]) (m2: int [,]) =
    if m1.GetLength(1) <> m2.GetLength(0)
    then None
    else let l, n, m = m1.GetLength(0), m2.GetLength(1), m2.GetLength(0)
         let m3 = Array2D.create l n 0
         Parallel.For(0, l, (fun i->
            for j = 0 to n - 1 do
                for k = 0 to m - 1 do
                    m3.[i,j] <- m3.[i,j] + m1.[i,k] * m2.[k,j]))  
         |> ignore
         Some m3
 
         
let ex2 size (sw:Stopwatch) nthreads =
    let r = new Random()
 
    let m1 = Array2D.init size size (fun i j -> r.Next(0,1024))
    let m2 = Array2D.init size size (fun i j -> r.Next(0,1024))
   
    sw.Restart()
    asyncMult nthreads m1 m2 |> ignore
    sw.Stop()
    Console.WriteLine("Async matrices multiplication  in {0}", sw.Elapsed)
 
    sw.Restart()
    threadsMult m1 m2 |> ignore
    sw.Stop()
    Console.WriteLine("TPL matrices multiplication in {0}", sw.Elapsed)
 
    sw.Restart()
    mult m1 m2 |> ignore
    sw.Stop()
    Console.WriteLine("Sequential matrices multiplication in {0}", sw.Elapsed)

[<EntryPoint>]
let main argv =
    let sw = new Stopwatch()
    let nthreads = 4
    printfn "Ready to start"
    Console.ReadKey() |> ignore
    ex2 512 sw nthreads
    printfn "Done"
    Console.ReadKey() |> ignore
    0