open System
open System.Globalization
open System.Threading.Tasks
open System.Collections.Generic
open System.ComponentModel
open System.Linq
open System.Diagnostics
open System.Threading.Tasks

// Дополнительные сведения о F# см. на http://fsharp.org
// Дополнительную справку см. в проекте "Учебник по F#".

let rec transpose matrix = 
  match matrix with   // matrix is a list<list<int>>
  | row::rows ->      // case when the list of rows is non-empty
    match row with    // rows is a list<int>
    | col::cols ->    // case when the row is non-empty
      // Take first elements from all rows of the matrix
      let first = List.map List.head matrix
      // Take remaining elements from all rows of the matrix
      // and then transpose the resulting matrix
      let rest = transpose (List.map List.tail matrix) 
      first :: rest
    | _ -> []
  | _ -> [] 

let rec innerMult u v =
    match u, v with 
    | [x], [y] -> x*y     | u'::u, v'::v -> u'*v' + innerMult u v 

let multiply xs ys =
    [for row in xs ->
        async{
            for col in transpose ys do
                s = innerMult row col
                
        }
    ]
    |>Async.Parallel
    |>Async.RunSynchronously
    |>ignore

let generateRandomList rows cols=
    let rnd = System.Random()
    [for col in cols -> 
        List.init rows (fun _ -> rnd.Next(0,100))]

let matrix1 = generateRandomList 512 [1..512]
let matrix2 = generateRandomList 512 [1..512]

//printfn "%A" (multiply matrix2x3 matrix3x2)

//let rowMulitplier row ys = 
//    [for col in transpose ys -> innerMult row col]


//#time
//multiply matrix1 matrix2
//#time

let factorise n =
    let rec h n x a =
        if x = n then
            x::a
        elif n % x = 0 then
            h (n/x) x (x::a)
        else
            h n (x+1) a
    h n 2 []
 
let nfactors = factorise >> Seq.length
 
let ex1 from till (sw:Stopwatch) nthreads =
    let works = List.splitInto nthreads [from..till]
 
    sw.Restart()
    seq { for w in works -> async { return Seq.sum(Seq.map nfactors w) } }
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.sum
    |> Console.WriteLine
    sw.Stop()
    Console.WriteLine("Async factorisation in {0}", sw.Elapsed)
 
    let r = Array.create (till - from + 1) 0
    sw.Restart()
    Parallel.For(from, till+1, (fun i -> do r.[i-from] <- nfactors i)) |> ignore
    Console.WriteLine(Array.sum r)
    sw.Stop()
    Console.WriteLine("TPL factorisation in {0}", sw.Elapsed)
 
    sw.Restart()
    Seq.map nfactors [from..till]
    |> Seq.sum
    |> Console.WriteLine
    sw.Stop()
    Console.WriteLine("Sequential factorisation in {0}", sw.Elapsed)
 
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
    Console.WriteLine("Async matrices in {0}", sw.Elapsed)
 
    sw.Restart()
    threadsMult m1 m2 |> ignore
    sw.Stop()
    Console.WriteLine("TPL matrices in {0}", sw.Elapsed)
 
    sw.Restart()
    mult m1 m2 |> ignore
    sw.Stop()
    Console.WriteLine("Sequential matrices in {0}", sw.Elapsed)
 
[<EntryPoint>]
let main argv =
    let sw = new Stopwatch()
    let nthreads = 4
    printfn "Ready to start"
    Console.ReadKey() |> ignore
    //ex1 200000 300000 sw nthreads
    ex2 512 sw nthreads
    printfn "Done"
    Console.ReadKey() |> ignore
    0