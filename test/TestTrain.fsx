
#r "../build/Tilde.dll"

open Tilde
open Tilde.Tree

type Category =  East | West

//-------------------------------------------------------------------------------------------------
// EXAMPLES (Michalski's original set of trains)

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

let wagon = {charge=Circle ; chargeNumber=1 ; roof=NoRoof ; sides = Simple ; whellNumber = 2 ; container = Rectangular ; length = Short}

let t1 : Train =
   [
      wagon
      {wagon with length = Long ; whellNumber = 3}
      {wagon with charge = Triangle ; roof = Peaked}
      {wagon with length = Long ; charge = Rectangle ; chargeNumber=3}
   ]
let t2 : Train =
   [
      {wagon with chargeNumber=2 ; roof = Flat}
      {wagon with charge = Rectangle ; container = Bucket}
      {wagon with charge = Triangle ; container = UShaped}
   ]
let t3 : Train =
    [
        {wagon with charge = Triangle ; length = Long ; whellNumber = 3}
        {wagon with charge = Triangle ; container = Hexagonal ; roof=Flat}
        wagon
    ]
let t4 : Train =
    [
        {wagon with charge = Rectangle}
        {wagon with charge = Diamond; container = Ellipse}
        {wagon with charge = Triangle; sides = Double}
        {wagon with charge = Triangle; container = Bucket}
    ]
let t5 : Train =
    [
        {wagon with roof = Flat}
        {wagon with roof = Flat; length = Long ; whellNumber = 3 ; charge = Rectangle}
        {wagon with charge = Triangle ; sides = Double}
    ]

let east = [t1;t2;t3;t4;t5] |> List.map (fun t -> East,t)

//-----

let t6 : Train =
    [
        {wagon with charge = Triangle}
        {wagon with roof = Flat ; chargeNumber = 3 ; length = Long}
    ]
let t7 : Train = 
    [
        {wagon with length = Long ; roof = Jagged ; charge = NoCharge ; chargeNumber=0}
        {wagon with charge = Triangle ; container = UShaped}
        {wagon with sides = Double}
    ]
let t8 : Train =
    [
        {wagon with container = UShaped}
        {wagon with roof = Flat ; whellNumber = 3 ; charge=Rectangle}
    ]
let t9 : Train =
    [
        {wagon with container = Bucket}
        {wagon with charge = Rectangle}
        {wagon with roof = Jagged; whellNumber = 3 ; charge=Rectangle}
        {wagon with container = Bucket}
    ]
let t10 : Train =
    [
        {wagon with charge = Rectangle ; chargeNumber = 2 ; length = Long}
        {wagon with charge = Rectangle ; container = UShaped}
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
        for charge in [Circle; Triangle; Rectangle; Diamond; NoCharge] do 
            yield chargeIs charge
        
        for roof in [NoRoof; Flat; Peaked; Jagged] do
            yield roofIs roof

        for container in [Rectangular; Bucket; Ellipse; Hexagonal; UShaped] do 
            yield containerIs container

        for chargeNumber in 1..3 do
            yield chargeNumberIs chargeNumber

        yield sideIs Simple
        yield sideIs Double

        yield lengthIs Short
        yield lengthIs Long

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
∃t0, charge_is_a_Triangle(t0) ?
├─No── West
└─Yes─∃t1, has_no_charge(t1) ?
      ├─No──∀t∈t0, roof_is_Peaked(t) ?
      │     ├─No──∃t2, has_3_charges(t2) ?
      │     │     ├─No── East
      │     │     └─Yes─ West
      │     └─Yes─ East
      └─Yes─ West
*)
