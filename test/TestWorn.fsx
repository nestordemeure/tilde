#r "../build/Tilde.dll"

open Tilde
open Tilde.Tree

type Category =  Ok | Fix | Sendback

//-------------------------------------------------------------------------------------------------
// EXAMPLES

type UsedPart = Gear | Chain | Engine | Wheel

type Car = UsedPart list

//-----

let examples : Examples<Category,UsedPart> = 
   [
      Fix, [Gear; Chain]
      Sendback, [Engine; Chain]
      Sendback, [Wheel]
      Ok, []
   ]

//-------------------------------------------------------------------------------------------------
// LITTERAUX

let replaceable part = 
   match part with 
   | Gear | Chain -> true
   | _ -> false

let worn part = true

//-----

let litteraux = [check "Replaceable" replaceable ; check "Worn" worn]

//-------------------------------------------------------------------------------------------------
// KNOWN SOLUTION

let solution =
   Node { 
      test = check "Worn" worn
      variableId = 0
      no = Leaf Ok
      yes = Node {
                     test = check "Replaceable" replaceable
                     variableId = 0
                     no = Leaf Sendback
                     yes = Leaf Fix
                  }
      }

Export.printTree solution

examples |> List.map (snd >> classify solution)

//-------------------------------------------------------------------------------------------------
// NEW SOLUTION

let mySolution = Tree.create examples litteraux

Export.printTree mySolution

examples |> List.map (snd >> classify mySolution)

(* 
identical to the official solution :
∃t0, Worn(t0) ?
├─No── Ok
└─Yes─∀t∈t0, Replaceable(t) ?
      ├─No── Sendback
      └─Yes─ Fix
*)