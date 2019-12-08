module Propositions =
    type Environment = Map<string, Constant>
    and Constant = bool

    type Proposition =
        | Variable of string
        | Not of Proposition
        | Or of Proposition * Proposition
        | And of Proposition * Proposition
        | Implies of Proposition * Proposition
        | Equals of Proposition * Proposition
        
    let rec Evaluate (env: Environment) (prop: Proposition) : Constant =
        match prop with
        | Variable v -> Map.find v env
        | Not p -> not (Evaluate env p)
        | Or (p1, p2) -> if (Evaluate env p1 = Evaluate env p2 = false) then false else true
        | And (p1, p2) -> if (Evaluate env p1 = Evaluate env p2 = true) then true else false
        | Implies (p1, p2) -> if (Evaluate env p1 = true && Evaluate env p2 = false ) then false else true
        | Equals (p1, p2) -> if (Evaluate env p1 = Evaluate env p2) then true else false
        
    let rec ToString (prop: Proposition) : string =
        match prop with
        | Variable v -> v
        | Not p -> sprintf "not %s" (ToString p)
        | Or (p1, p2) -> sprintf "%s or %s" (ToString p1) (ToString p2)
        | And (p1, p2) -> sprintf "%s and %s" (ToString p1) (ToString p2)
        | Implies (p1, p2) -> sprintf "%s => %s" (ToString p1) (ToString p2)
        | Equals (p1, p2) -> sprintf "%s <=> %s" (ToString p1) (ToString p2)
        
    let rec Verify (env: Environment) (prop: Proposition) : Constant =
        match prop with
        | Variable v -> env.ContainsKey v
        | Not p -> Verify env p
        | Or (p1, p2) | And (p1, p2) | Implies (p1, p2) | Equals (p1, p2) -> (Verify env p1 = true) && (Verify env p2 = true)
            
    let rec NamesUsed (prop: Proposition) : string list =
        match prop with
        | Variable v -> [v]
        | Not p -> NamesUsed p
        | Or (p1, p2) | And (p1, p2) | Implies (p1, p2) | Equals (p1, p2) -> NamesUsed p1 @ NamesUsed p2