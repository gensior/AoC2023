// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO

// replace all word instances of digits with digits in string
let replace (text:string) =
    text.Replace("zero", "z0o")
        .Replace("eleven", "e11n")
        .Replace("twelve", "t12e")
        .Replace("thirteen", "t13n")
        .Replace("fourteen", "f14n")
        .Replace("fifteen", "f15n")
        .Replace("sixteen", "s16n")
        .Replace("seventeen", "s17n")
        .Replace("eighteen", "e18n")
        .Replace("nineteen", "n19n")
        .Replace("twenty", "t20y")
        .Replace("one", "o1e")
        .Replace("two", "t2o")
        .Replace("three", "t3e")
        .Replace("four", "f4r")
        .Replace("five", "f5e")
        .Replace("six", "s6x")
        .Replace("seven", "s7n")
        .Replace("eight", "e8y")
        .Replace("nine", "n9e")
        .Replace("ten", "t10n")

// find all digits in a string
let digits text = text |> replace |> Seq.filter Char.IsDigit |> Seq.toList

// combine first and last chars in a sequence to a string
let combine text =
    match text with
    | [] -> ""
    | [x] -> x.ToString() + x.ToString() |> string
    | x::xs -> x.ToString() + xs.[xs.Length - 1].ToString()

// convert string to int
let toInt text = text |> int

// find first and last digit in a string and convert them to an int
let firstAndLast text = text |> digits |> combine |> toInt

// open a file and read its contents line by line (lazy)
let lines = File.ReadLines("inputs.txt")

// add all digits parsed from the lines
let sum = lines |> Seq.map digits |> Seq.map combine |> Seq.map toInt |> Seq.sum

printfn $"%A{sum}"