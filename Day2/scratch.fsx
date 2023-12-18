open System.IO

type Color = Red | Green | Blue

type ColorNumber = {
    color: Color
    number: int
}

type Hand = {
    Blue: int option
    Green: int option
    Red: int option
}

type Game = {
    id: int
    Blue: int
    Green: int
    Red: int
}

let parseLine (line:string) =
    
    /// <summary>
    /// Parses a hand from a string
    ///
    /// like "1 red, 1 green, 1 blue"
    /// </summary>
    /// <param name="text">The string to parse</param>
    let parseHand (text:string) : Hand =
        let hand = { Blue = None; Green = None; Red = None }
        
        /// <summary>
        /// Parses a color from a string
        ///
        /// like "1 red"
        /// </summary>
        /// <param name="text">The string to parse</param>
        let parseColor (text:string) =
            let parts = text.Split(' ')
            let number = int parts.[0]
            let color = match parts.[1] with
                        | "red" -> Red
                        | "green" -> Green
                        | "blue" -> Blue
                        | x -> failwith $"Unknown color: {x}"
            { color = color; number = number }
            
        text.Split(", ")
            |> Array.map parseColor
            |> Array.fold (fun hand colorNumber ->
                match colorNumber.color with
                | Red ->
                    match hand.Red with
                    | Some x when x > colorNumber.number -> { hand with Red = Some colorNumber.number }
                    | _ -> hand
                | Green ->
                    match hand.Green with
                    | Some x when x > colorNumber.number -> { hand with Green = Some colorNumber.number }
                    | _ -> hand
                | Blue ->
                    match hand.Blue with
                    | Some x when x > colorNumber.number -> { hand with Blue = Some colorNumber.number }
                    | _ -> hand
            ) hand
            
    let parts = line.Split(": ")
    let game = parts.[0].Split(" ")[1] |> int
    parts.[1].Split("; ")
               |> Array.map parseHand
               // add the highest of red, green, and blue colors to the game object
               |> Array.fold (fun game hand ->
                    // highest red so far
                    let red = match hand.Red with
                              | Some x when x > game.Red -> x
                              | _ -> game.Red
                    let green = match hand.Green with
                                | Some x when x > game.Green -> x
                                | _ -> game.Green
                    let blue = match hand.Blue with
                                 | Some x when x > game.Blue -> x
                                 | _ -> game.Blue
                    { game with Blue = blue; Green = green; Red = red }
               ) { id = game; Blue = 0; Green = 0; Red = 0 }
    
let isPossible (game:Game) =
    let blue = 14
    let red = 12
    let green = 13
    game.Blue <= blue && game.Red <= red && game.Green <= green
    
let findAnswer (file:string)=
    let lines = File.ReadLines($"Day2/{file}.txt")
    lines
    |> Seq.map parseLine
    |> Seq.filter isPossible
    |> Seq.fold (fun acc game -> acc + game.id) 0
    
findAnswer "sample" |> printfn "%A"