
#r "../build/Tilde.dll"

open Tilde
open Tilde.Tree

//-------------------------------------------------------------------------------------------------
// EXAMPLES (new set from Muggleton train generator)

type Category =  East | West

type Charge = Circle | Triangle | Rectangle | Diamond | Hexagone | NoCharge
type Roof = NoRoof | Flat | Peaked | Jagged
type Side = Simple | Double
type Length = Long | Short
type Container = Rectangular | Bucket | Ellipse | Hexagonal | UShaped

type Wagon = 
    {
        charge : Charge ; chargeNumber : int 
        container : Container ; sides : Side ; roof : Roof
        length : Length ; whellNumber : int
    }
    
type Train = Wagon list

//-----

let wagon = {charge=Rectangle ; chargeNumber=1 ; roof=NoRoof ; sides = Simple ; whellNumber = 2 ; container = Rectangular ; length = Short}

let t1 : Train =
   [
       {wagon with length = Long ; roof = Jagged ; whellNumber = 3}
       {wagon with charge = Circle ; container = Hexagonal}
       {wagon with charge = Triangle}
       {wagon with charge = NoCharge ; length = Long ; roof = Jagged}
   ]
let t2 : Train =
   [
       {wagon with charge = Hexagone ; length = Long}
       wagon
       {wagon with charge = Triangle ; roof = Flat}
   ]
let t3 : Train =
    [
        {wagon with roof = Peaked}
        {wagon with container = Bucket}
        {wagon with charge = Circle ; length = Long}
        wagon
    ]
let t4 : Train =
    [
        {wagon with length = Long ; chargeNumber = 3}
        {wagon with charge = Circle}
        {wagon with charge = Hexagone ; roof = Jagged ; length = Long ; whellNumber = 3}
        {wagon with charge = Triangle ; container = UShaped}
    ]
let t5 : Train =
    [
        {wagon with charge = Triangle ; container = Bucket}
        {wagon with charge = Circle ; container = UShaped}
        {wagon with charge = Triangle}
        {wagon with charge = Triangle}
    ]

let east = [t1;t2;t3;t4;t5] |> List.map (fun t -> East,t)

//-----

let t6 : Train =
    [
        {wagon with container = Bucket}
        {wagon with charge = Triangle ; length = Long}
    ]
let t7 : Train = 
    [
        {wagon with charge = Hexagone ; length = Long}
        {wagon with charge = Circle}
        {wagon with charge = Circle ; sides = Double}
        {wagon with length = Long ; chargeNumber = 3}
    ]
let t8 : Train =
    [
        {wagon with charge = Triangle ; container = UShaped}
        {wagon with length = Long ; chargeNumber = 3 ; whellNumber = 3}
    ]
let t9 : Train =
    [
        {wagon with length = Long ; chargeNumber = 3 ; whellNumber = 3 ; roof = Flat}
        {wagon with length = Long ; chargeNumber = 3 ; roof = Flat}
        {wagon with charge = NoCharge ; length = Long}
        {wagon with charge = Triangle ; container = UShaped}
    ]
let t10 : Train =
    [
        {wagon with charge = Hexagone ; roof = Flat ; length = Long ; whellNumber = 3}
        {wagon with charge = Triangle ; container = UShaped}
    ]

let west = [t6;t7;t8;t9;t10] |> List.map (fun t -> West,t)

//------

let examples = east @ west

//-------------------------------------------------------------------------------------------------
// LITTERAUX
let chargeIs charge = 
    match charge with 
    | NoCharge -> check "has_no_charge" (fun wagon -> wagon.charge = charge)
    | _ -> check (sprintf "charge_is_a_%A" charge) (fun wagon -> wagon.charge = charge)
let roofIs roof = 
    match roof with 
    | NoRoof -> check "has_no_roof" (fun wagon -> wagon.roof = roof)
    | _ -> check (sprintf "roof_is_%A" roof) (fun wagon -> wagon.roof = roof)
let sideIs side = 
    check (sprintf "sideType_is_%A" side) (fun wagon -> wagon.sides = side)
let chargeNumberIs n = 
    check (sprintf "has_%d_charges" n) (fun wagon -> wagon.chargeNumber = n)
let whellNumberIs n = 
    check (sprintf "has_%d_whells" n) (fun wagon -> wagon.whellNumber = n)
let containerIs container = 
    check (sprintf "container_is_%A" container) (fun wagon -> wagon.container = container)
let lengthIs length = 
    check (sprintf "is_%A" length) (fun wagon -> wagon.length = length)

//-----

let litteraux =
    [
        for charge in [Circle; Triangle; Rectangle; Hexagone; NoCharge] do 
            yield chargeIs charge
        
        for roof in [NoRoof; Flat; Peaked; Jagged] do
            yield roofIs roof

        for container in [Rectangular; Bucket; Ellipse; Hexagonal; UShaped] do 
            yield containerIs container

        yield sideIs Simple
        yield sideIs Double

        yield lengthIs Short
        yield lengthIs Long

        yield chargeNumberIs 3
        yield chargeNumberIs 1

        yield whellNumberIs 2
        yield whellNumberIs 3
    ]

//-------------------------------------------------------------------------------------------------
// SOLUTION

let solution = Tree.create examples litteraux

Export.printTree solution

east |> List.map (snd >> classify solution) |> printfn "east : %A"
west |> List.map (snd >> classify solution) |> printfn "west : %A"

(*
almost degrades into a list : the representation is probably too poor for the problem at hand
(adding properties for the whole train, and not just single wagons belonging to the train, would probably help)
∃t0, roof_is_Jagged(t0) ?
├─No──∃t1, roof_is_Peaked(t1) ?
│     ├─No──∃t2, is_Long(t2) ?
│     │     ├─No── East
│     │     └─Yes─∀t∈t2, charge_is_a_Triangle(t) ?
│     │           ├─No──∀t∈t2, charge_is_a_Rectangle(t) ?
│     │           │     ├─No──∀t∈t2, roof_is_Flat(t) ?
│     │           │     │     ├─No──∀t∈t2, charge_is_a_Hexagone(t) ?
│     │           │     │     │     ├─No── West
│     │           │     │     │     └─Yes─ East
│     │           │     │     └─Yes─ West
│     │           │     └─Yes─ West
│     │           └─Yes─ West
│     └─Yes─ East
└─Yes─ East
*)
