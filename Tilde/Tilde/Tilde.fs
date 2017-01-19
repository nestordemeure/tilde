
module Tilde.Tree
open System.Collections.Generic

type 'Element Propriete = 'Element -> bool

[<CustomEquality;NoComparison>]
type 'Element Litteral = 
    { propriete : 'Element Propriete ; name : string }
    // implement a name based equality (an int should be used for efficience)
    interface System.IEquatable<'Element Litteral> with
        member this.Equals(other : 'Element Litteral) =
            other.name = this.name

/// build a litteral
let check name prop = { propriete = prop ; name = name }
/// produce the negation of a litteral
let negate lit = { name = "not " + lit.name ; propriete = lit.propriete >> not }

type Node<'Element,'Category> = 
    { 
        test : 'Element Litteral
        variableId : int
        yes : Tilde<'Element,'Category>
        no : Tilde<'Element,'Category> 
    }

and Tilde<'Element,'Category> =
    | Leaf of 'Category
    | Node of Node<'Element,'Category>

//-----

type 'Element Sample = 'Element list

type Examples<'Category,'Element> = ('Category * ('Element Sample)) list

//-------------------------------------------------------------------------------------------------

/// given a property aplied to a variable, return the branch in which to go and an updated table with variable names
let chooseBranch propriete variableId namedVariablesDict sample =
    match Map.tryFind variableId namedVariablesDict with 
    | Some variables -> // existing variable, forall ?
        match List.forall propriete variables with 
        | false -> false, namedVariablesDict
        | true -> true, namedVariablesDict
    | None -> // new variable, exists ?
        match List.filter propriete sample with 
        | [] -> false, namedVariablesDict
        | variables -> let newNamesVariablesDict = Map.add variableId variables namedVariablesDict
                       true, newNamesVariablesDict

//-----

/// gives a category to a sample using a tree and a dictionnary that associate a variableId with potential instances
let rec classifyRec tree namedVariablesDict sample =
    match tree with 
    | Leaf category -> category
    | Node node -> 
        let branch, newNamesVariablesDict = chooseBranch node.test.propriete node.variableId namedVariablesDict sample
        match branch with 
        | true -> classifyRec node.yes newNamesVariablesDict sample
        | false -> classifyRec node.no newNamesVariablesDict sample

//-----

/// gives a category to a sample using a tree
let classify tree sample = classifyRec tree Map.empty sample

//-------------------------------------------------------------------------------------------------

module List =
    /// removes a value from a list
    let rec minus x l =
        match l with 
        | [] -> l 
        | t::q -> if t = x then q else t::(minus x q)

module Map =
    /// removes a value from a key, remove the key if possible
    let cleanPool poolId value map =
        let pool = Map.find poolId map
        let newPool = List.minus value pool
        match newPool with 
        | [] -> Map.remove poolId map
        | _ -> Map.add poolId newPool map

    /// removes a given value from all key 
    let cleanAllPool value map =
        Map.map (fun key l -> List.minus value l) map

//-------------------------------------------------------------------------------------------------

/// given a variable and a propriety, splits a set of examples into two branches : yes and no
let rec splitByBranche variableId propriete examples =
    match examples with 
    | [] -> [], []
    | (cat,x,di)::q -> 
        let yes,no = splitByBranche variableId propriete q
        let isYes, newDi = chooseBranch propriete variableId di x
        match isYes with 
        | true -> (cat,x,newDi)::yes, no
        | false -> yes, (cat,x,newDi)::no

/// given a (non empty) branch, compute the split information or entropy as described in "Efficient C4.5" (the smaller the better)
let evaluateBranch branch =
    let length = List.length branch |> float
    let c45 catLength = 
        let ratio = (float catLength)/length
        - ratio * System.Math.Log(ratio)
    let gini catLength = 
        let ratio = (float catLength)/length
        ratio * (1. - ratio)
    branch
    |> List.countBy (fun (cat,x,di) -> cat)
    |> List.sumBy (snd >> c45)
    //|> List.sumBy (snd >> gini)

//-----

let rec justOneCategoryLeft l =
    match l with 
    | [] | [_] -> true
    | (cat1,x1,di1)::(cat2,x2,di2)::q -> 
        (cat1 = cat2) && (justOneCategoryLeft <| (cat2,x2,di2)::q )

/// return a (litteral,variableId,yes,no) option with the best split according to the split information
let pickTest (litsPerVariable : Map<int,'Element Litteral list>) examples =
    let mutable result = None
    if not <| justOneCategoryLeft examples then 
        let mutable bestEvaluation = System.Double.PositiveInfinity
        // build a branch, evaluate it, keep it if it is better than the current best
        let saveBest variableId lit =
            let yes,no = splitByBranche variableId lit.propriete examples
            match yes, no with 
            | [], _ | _, [] -> () // the split brings no information to the table
            | _ ->  let evaluation = (evaluateBranch yes) + (evaluateBranch no)
                    if evaluation < bestEvaluation then 
                        bestEvaluation <- evaluation
                        result <- Some (lit,variableId,yes,no)
        // loop on all posible variable/litteral possible
        for kv in litsPerVariable do 
            List.iter (saveBest kv.Key) kv.Value
    result

//-----

/// recursively builds a decision tree
let rec createRec litsPerVariable newVariableId examples = 
    let split = pickTest litsPerVariable examples
    match split with 
    | None -> // no more good splitting
        let mainCategory = examples |> List.countBy (fun (cat,x,di) -> cat) |> List.maxBy snd |> fst
        Leaf mainCategory
    | Some (lit,variableId,yes,no) when variableId = newVariableId -> 
        let newlitsPerVariable = litsPerVariable |> Map.add (newVariableId+1) litsPerVariable.[newVariableId]
        let yeslitsPerVariable = 
            newlitsPerVariable 
            |> Map.cleanAllPool lit |> Map.cleanPool variableId (negate lit)
        let nolitsPerVariable = 
            newlitsPerVariable 
            |> Map.remove newVariableId // this variable name will never be used in the noBranch
            |> Map.cleanAllPool lit |> Map.cleanAllPool (negate lit)
        let yesNode = createRec yeslitsPerVariable (newVariableId+1) yes
        let noNode = createRec nolitsPerVariable (newVariableId+1) no
        Node { test = lit ; variableId = variableId ; yes = yesNode ; no = noNode }
    | Some (lit,variableId,yes,no) ->
        let yeslitsPerVariable = 
            litsPerVariable 
            |> Map.cleanPool variableId lit |> Map.cleanPool variableId (negate lit)
        let nolitsPerVariable = 
            litsPerVariable 
            |> Map.cleanPool variableId lit
        let yesNode = createRec yeslitsPerVariable newVariableId yes
        let noNode = createRec nolitsPerVariable newVariableId no
        Node { test = lit ; variableId = variableId ; yes = yesNode ; no = noNode }

//-----
let create (examples : Examples<'Category,'Element'>) litteraux =
    let litteraux = List.map negate litteraux |> List.append litteraux // building the negations
    let litsPerVariable = Map.add 0 litteraux Map.empty
    let examples = List.map (fun (cat,x) -> cat,x,Map.empty) examples
    createRec litsPerVariable 0 examples

//-------------------------------------------------------------------------------------------------

// PSEUDOCODE :

// liste de variables, chacune associé a une liste de fonction applicable (inclus les fonctions mais aussi leur négation !)
// numéros de la prochaine variable
// liste d'exemple avec leur categorie et leur dictionnaire

// si il ne reste qu'une catégorie alors feuille 

// on évalut toute les combinaison de variable et fonction
// on choisit la meilleur en fonction d'un critère

// quand on a choisit une fonction+variable
// si c'est sur une variable existante, 
//      oui : on retire f et !f de son pool
//      non : on retire f de son pool
// si c'est sur une variable nouvelle, 
//(si la vrariable=0 alors nouvelle variable, on construit une nouvelle variable identique qu'on manipulera)
//      non : on retire f et !f de tout le monde
//      oui : on retire f de tout le monde, !f de son pool
