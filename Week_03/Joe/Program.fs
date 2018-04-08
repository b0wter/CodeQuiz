// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic

/// <summary>
/// Erstellt aus dem übergebenen Argument ein Histogramm.
/// </summary>
let histogram = 
    Seq.fold (fun acc key ->
        if Map.containsKey key acc
        then Map.add key (acc.[key] + 1) acc
        else Map.add key 1 acc
    ) Map.empty
    >> Seq.sortBy (fun keyValuePair -> keyValuePair.Value)

/// <summary>
/// Wandelt ein KeyValuePair in ein Tuple um.
/// </summary>
let keyValuePairToTuple (input: seq<KeyValuePair<char, int>>) =
    input |> Seq.map (fun x -> (x.Key, x.Value))

/// <summary>
/// Einfacher Weg den Median zu berechnen. Weniger performant, aber besser lesbar.
/// </summary
let simpleMedian xs =
    let length = Seq.length xs
    let sortedList = xs |> Seq.sort |> Seq.toArray
    // bei Arrays mit gerade Zahl ist uns egal ob wir Floor oder Ceiling nehmen
    sortedList.[length/2]

/// <summary>
/// Berechnet den Median einer Liste. Liefert ein Tuple zurück, dessen Durchschnitt der Meridian ist.
/// </summary>
let median xs =
    // Teile die Liste in drei Teile auf: weniger als, gleich und mehr als
    /// x:    aktueller Pivot-Punkt
    /// xs:   Aufzuteilende Unterliste
    /// cont: continuation function
    let rec partition x xs cont =
        match xs with
        | [] ->
            cont [] 0 [x] 1 [] 0
        | y::ys ->
            if y < x then
                partition x ys (fun lts n1 eqs n2 gts n3 ->
                    cont (y::lts) (n1+1) eqs n2 gts n3)
            elif y = x then
                partition y ys (fun lts n1 eqs n2 gts n3 ->
                    cont lts n1 (x::eqs) (n2+1) gts n3)
            else // y > x
                partition x ys (fun lts n1 eqs n2 gts n3 ->
                    cont lts n1 eqs n2 (y::gts) (n3+1))
    let rec loop before xs after =
        match xs with
        | [] -> failwith "Median einer leeren Liste!"
        | x::xs ->
            partition x xs (fun lts numlt eqs numeq gts numgt ->
                if before + numlt > numeq + numgt + after then
                    loop before lts (after + numeq + numgt)
                elif before + numlt = numeq + numgt + after then
                    (List.max lts, x)
                elif before + numlt + numeq > numgt + after then
                    (x, x)
                elif before + numlt + numeq = numgt + after then
                    (x, List.min gts)
                else
                    loop (before + numlt + numeq) gts after)
    loop 0 xs 0

/// <summary>
/// Summiert die Differencen der einzelnen Histogrammelemente mit dem Median.
/// </summary>
let differenceToMedian (median: int) (items: seq<KeyValuePair<char, int>>) =
    items |> Seq.sumBy(fun x -> Math.Abs(x.Value - median))

let elementsWithoutMedianValue (median: int) (items: seq<KeyValuePair<char, int>>) =
    items |> Seq.sumBy(fun x -> 
        if x.Value = median then 0
        else x.Value
    )

[<EntryPoint>]
let main _ =
    let input = Console.ReadLine()
    let bins = input |> histogram //|> Seq.iter (printf "%A") 
    let median = bins |> keyValuePairToTuple |> Seq.map (snd) |> Seq.toList |> median
    let difference = differenceToMedian (fst median) bins

    let result = 
        if difference <= 1 then "JA"
        elif (elementsWithoutMedianValue (fst median) bins) = 1 then "JA"
        else "NEIN"

    Console.WriteLine(result)
    0 // return an integer exit code