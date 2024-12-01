open System.IO

let readFile path =
    seq { use reader = new StreamReader(File.OpenRead(path))
          while not reader.EndOfStream do
              yield reader.ReadLine() 
    }

let step1 leftSorted rightSorted = List.zip leftSorted rightSorted
                                           |> List.map (fun (l, r) -> abs (l - r))
                                           |> List.sum


let extractMatch (rightMap: System.Collections.Generic.IDictionary<'a,int>) number =
    match rightMap.TryGetValue(number) with
    | true, value -> value
    | false, _ -> 0

let step2 leftSorted rightSorted =
    let rightMap = rightSorted
                   |> List.countBy id
                   |> Map.ofList      
    leftSorted
    |> List.map(fun (number) ->
        number * (extractMatch rightMap number))
    |> List.sum


let cleanInput (input: string seq) =
                                        input
                                        |> Seq.map (fun line -> line.Split("  ")
                                                                    |> Array.map (fun word -> word.Trim())
                                                                    |> fun words -> (words.[0] |> int, words.[1] |> int))
                                        |> Seq.toList
[<EntryPoint>]
let main argv =
    let  rawList: (int * int) list = @"/Users/hugomufraggi/RiderProjects/ConsoleApp3/input.txt"
                                        |> readFile
                                        |> cleanInput
                                        
                                            
    let (left, right ) = List.unzip  rawList 
    let leftSorted: int list = left |> List.sort
    let rightSorted: int list = right |> List.sort
    //let res = step1 leftSorted rightSorted
    let res2 = step2 leftSorted rightSorted
   // printfn "%A" res
    printfn "%A" res2
    0