module Tilde.Export
open Tilde.Tree
open System

//-------------------------------------------------------------------------------------------------

/// display a string in a given color
let printStringColor s c = 
    System.Console.ForegroundColor <- c
    printf "%s" s

/// display a list of element, each with a given color
let rec printStringListColor l = 
    match l with 
    | [] -> ()
    | (c,s)::q -> printStringListColor q ; printStringColor s c

//-----

let labelForAllOfTest test variableId = sprintf "∀t∈t%d, %s(t) ?" variableId test.name

let labelExistOfTest test variableId = sprintf "∃t%d, %s(t%d) ?" variableId test.name variableId

//-----
let rec printTreeRec tree knownVars previously =
   match tree with 
   | Leaf cat -> 
      Console.ForegroundColor <- ConsoleColor.White
      printf " %A" cat
   | Node node ->
      let newKnownVars, label =
        match Map.containsKey node.variableId knownVars with
        | true -> 
            knownVars,
            labelForAllOfTest node.test node.variableId
        | false -> 
            Map.add node.variableId true knownVars,
            labelExistOfTest node.test node.variableId
      printStringColor label ConsoleColor.Blue
      printf "\n"
      printStringListColor <| (ConsoleColor.Red,"├─No──")::previously
      printTreeRec node.no newKnownVars <| (ConsoleColor.Red,"│     ")::previously
      printf "\n"
      printStringListColor  <| (ConsoleColor.Green,"└─Yes─")::previously
      printTreeRec node.yes newKnownVars <| (ConsoleColor.Green,"      ")::previously

//-----

/// display a tree in ascii
let printTree arbre = 
    printTreeRec arbre Map.empty []
    System.Console.ForegroundColor <- ConsoleColor.White
    printf "\n"