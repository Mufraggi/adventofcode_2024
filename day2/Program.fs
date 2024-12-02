open System.IO
let readFile path =
    seq { use reader = new StreamReader(File.OpenRead(path))
          while not reader.EndOfStream do
              yield reader.ReadLine() 
    }

let clean (lines:string seq) =
    lines
    |> Seq.map (fun line -> line.Split(" ")
                                |> Array.map(fun nb -> nb
                                                    |> int)
                     )
    |> Seq.toArray 
    
              
let checkLine (isInf: bool) (isSup: bool) (isSafe:bool) =
    match (isInf, isSup, isSafe) with
    | (true, false, true) -> true
    | (false, true, true) -> true
    | (_, _, false) -> false
    | (false, false, _) -> false
    | _ -> false
        

let isInf (line: int array) =
    line
    |> Array.windowed 2
    |> Array.map (fun [|a; b|] -> a > b)
    |> Array.forall id

let isSup(line: int array) =
     line
    |> Array.windowed 2
    |> Array.map (fun [|a; b|] -> a < b)
    |> Array.forall id

let isSafe(line: int array)=
    line
    |> Array.windowed 2
    |> Array.map (fun [|a; b|] ->
        abs (a - b) > 0 &&  abs (a - b) < 4 )
    |> Array.forall id
    
let checkValidity(lines: int array array) =
    lines
    |> Array.map(fun line->
        checkLine (isInf line) (isSup line) (isSafe line) )
    

let tryRemoveSingleLevel (line: int array) =
    [0..line.Length-1]
    |> List.exists (fun removeIndex ->
        let modifiedLine = 
            line 
            |> Array.indexed 
            |> Array.filter (fun (i, _) -> i <> removeIndex) 
            |> Array.map snd
        
        let isIncreasing = isSup modifiedLine
        let isDecreasing = isInf modifiedLine
        let isAdjacentDiffValid = isSafe modifiedLine
        (isIncreasing || isDecreasing) && isAdjacentDiffValid
    )

let checkLineDampener (line: int array) =
    let originalSafe = 
        let isIncreasing = isSup line
        let isDecreasing = isInf line
        let isAdjacentDiffValid = isSafe line

        (isIncreasing || isDecreasing) && isAdjacentDiffValid

    let problemDampenerSafe = 
        if originalSafe then 
            true 
        else 
            tryRemoveSingleLevel line

    problemDampenerSafe

let checkValidityDampener(lines: int array array) =
    lines
    |> Array.map checkLineDampener
    
[<EntryPoint>]
let main argv =
    let res = @"/Users/hugomufraggi/RiderProjects/ConsoleApp3/day2/input.txt"
            |> readFile
            |> clean
            |> checkValidity
            |> Array.filter (fun x -> x) |> Array.length
    
    let resPart2 = @"/Users/hugomufraggi/RiderProjects/ConsoleApp3/day2/input.txt"
                    |> readFile
                    |> clean
                    |> checkValidityDampener
                    |> Array.filter (fun x -> x) |> Array.length
            
    printfn "Le nombre de true est : %d" res
    
    
    printfn "Part 2 - Number of safe reports with Problem Dampener: %d" resPart2

            
    0