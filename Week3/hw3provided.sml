(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


fun only_capitals sL = List.filter (fn s => Char.isUpper(String.sub(s,0))) sL;

fun longest_string1 sL = List.foldl (fn (s,i) => if String.size s > String.size i then s else i) "" sL

fun longest_string2 sL = List.foldl (fn (s,i) => if String.size s >= String.size i then s else i) "" sL

fun longest_string_helper f sL = List.foldl (fn(x,y) => if f(String.size x,String.size y) then x else y) "" sL

val longest_string3 = longest_string_helper (fn(x,y) => x > y)

val longest_string4 = longest_string_helper (fn(x,y) => x >= y)

val longest_capitalized = longest_string3 o only_capitals;

val rev_string = implode o rev o explode;

fun first_answer f l = 
    case l of
    [] =>  raise NoAnswer
    | x::xs => case f x of
        SOME y => y
        | NONE => first_answer f xs

fun all_answers f l = 
    let fun loop (l', acc) = 
        case l' of
        [] => SOME acc
        | SOME(x)::xs => loop (xs, acc @ x)
        | NONE::xs => NONE
    in
    loop(map f l, [])
    end

fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn x => String.size x) p

fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat(p) = 
    let fun convert_to_strings (Variable x) = [x]
            | convert_to_strings(TupleP ps) = List.foldl(fn(p', acc) => acc @ convert_to_strings(p')) [] ps
            | convert_to_strings x = []
        fun all_unique [] = true
            | all_unique(x::xs) = not (List.exists(fn y => x = y) xs) orelse all_unique xs 
    in 
        (all_unique o convert_to_strings) p
    end 

fun match (v, p) = 
    case (p, v) of
        (Wildcard, _) => SOME []
        | (Variable v', _) => SOME [(v', v)]
        | (UnitP, Unit) => SOME []
        | (ConstP cp, Const cv) => if cp = cv then SOME [] else NONE
        | (TupleP ps, Tuple vs) => if List.length ps = List.length vs then
                                    all_answers(fn(vs',ps') => match(vs',ps')) (ListPair.zip(vs, ps))
                                    else NONE
        | (ConstructorP(s1, p1), Constructor(s2,p2)) => if s1 = s2 then match(p2,p1) else NONE
        | _ => NONE

fun first_match v lP =
    ( SOME(first_answer (fn p => match(v,p)) lP) ) handle NoAnswer => NONE