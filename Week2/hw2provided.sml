(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* Start of problem one stuff *)

fun all_except_option (_, []) = NONE
    | all_except_option (s, x::xs) = 
        if same_string(s,x) then SOME xs else case all_except_option(s,xs) of
            NONE => NONE
            | SOME y => SOME(x :: y)

fun get_substitutions1([], _) = []
    | get_substitutions1(x::xs,s) =
    case all_except_option(s,x) of
        NONE => get_substitutions1(xs,s)
        | SOME y => y @ get_substitutions1(xs,s)


fun get_substitutions2(sLL, s) =
    let fun loop([], acc) = acc
    | loop(x::xs, acc) =
        case all_except_option(s,x) of
            NONE => loop(xs, acc)
            | SOME y => loop (xs, acc @ y)
    in
        loop(sLL, [])
    end


fun similar_names(sLL, {first: string, middle: string, last: string}) =
    let fun make_full_names(sL, acc) = case sL of [] => acc | x::xs => make_full_names(xs, acc @ [{first = x, middle = middle, last = last}] )
    in
    make_full_names(first :: get_substitutions2(sLL, first), [])
    end

(* End of problem one stuff *)




(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* Start of problem two stuff *)


fun card_color(Diamonds, _) = Red
  | card_color(Hearts, _) = Red
  | card_color(_, _) = Black

fun card_value(_, Ace) = 11
  | card_value(_, Num v) = v
  | card_value(_, _) = 10

fun remove_card(cs,c,e) = case cs of
    [] => raise e
    | x::xs => if c = x then xs else x::remove_card(xs,c,e)

fun all_same_color ([]) = true
  | all_same_color (_ :: []) = true
  | all_same_color (c1 :: c2 :: cs) =
      card_color c1 = card_color c2 andalso all_same_color(c2 :: cs)

fun sum_cards(cs) = 
    let fun loop(cs, acc) = case cs of [] => acc | x::xs => loop(xs, acc + card_value(x)) 
    in
    loop(cs, 0)
    end

fun score(cs, g) = 
    let val total = sum_cards(cs)
    val divisor = if all_same_color(cs) then 2 else 1
    in
    if (total > g) then ((total - g) * 3) div divisor else (g - total) div divisor
    end

fun officiate(cards, moves, goal) = 
    let val held_cards = []
    in
        let fun gameLoop(card_list, moves, goal, held_cards) = case moves of
            [] => score(held_cards, goal)
            | this_move::other_moves => case this_move of
                Discard c => gameLoop(card_list, other_moves, goal, remove_card(held_cards, c, IllegalMove))
                | Draw => case card_list of
                    [] => score(held_cards, goal)
                    | c::[] => score(c :: held_cards, goal)
                    | c::cs => gameLoop(cs, other_moves, goal, c :: held_cards);
        in
            gameLoop(cards, moves, goal, held_cards)
        end
    end





(* End of problem two stuff *)