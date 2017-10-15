/// Stuff that should be built into the language, but I couldn't find.

module Misc

/// Remove occurences of chars from s
//  http://rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#F.23
let stripChars (chars:string) (s:string)  =
    Array.fold (fun (s:string) c -> s.Replace(c.ToString(),""))
                s (chars.ToCharArray())

/// Does list a start with list b?
let rec startsWith<'X when 'X: equality> (a :'X list) (b :'X list) =
    match b with
        | [] -> true
        | B::Bs -> match a with
                   | A::As -> (A=B) && (startsWith As Bs)
                   | _ -> false

// Is this actually useful anywhere? Can I use this in practice?
type NonEmptyList<'t> =
    | One of 't
    | More of first:'t * rest:NonEmptyList<'t>

// Collapse functions. Basically collapses a list of tuples to a dictionary. I have chosen to return a list of tuples in the format (key*values[]) instead.
module List =
    let collapse (kv:('a*'b) List) =
        let data1, data2 = kv |> List.unzip
        let keys = data1 |> Seq.distinct |> List.ofSeq

        keys
        |> List.map (fun x -> (x, kv 
                                  |> List.filter (fun (k,v) -> k=x) 
                                  |> List.map snd))

module Array =
    let collapse(kv:('a*'b)[]) =
        let data1, data2 = kv |> Array.unzip
        let keys = data1 |> Seq.distinct |> Array.ofSeq //there's a natice Array.distinct in newer versions of F#, but this project is targeting an older version

        keys
        |> Array.map (fun x -> (x, kv 
                                  |> Array.filter (fun (k,v) -> k=x) 
                                  |> Array.map snd))