use "hw2provided.sml";


(* The Names Part *)

(* 1 *)
val all_except_option_ans = all_except_option("www",["www"]) = SOME [];
val all_except_option_ans = all_except_option("www",["qqq","eee","www","rrr"]) = SOME ["qqq","eee","rrr"];


(* 2 *)
val get_substitutions1_1 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"];
val get_substitutions1_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick","Freddie","F"];

(* 3 *)
val get_substitutions2_1 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"];

val get_substitutions2_2 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick","Freddie","F"];

(* 4 *)
val similar_names_ans = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
        [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
         {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}];

(* The Solitaire Part *)

val card_colour_1 = card_color((Clubs, Num 2)) = Black;
val card_colour_2 = card_color((Hearts, King)) = Red;
val card_colour_3 = card_color((Spades, Num 7)) = Black;
val card_colour_4 = card_color((Diamonds, Ace)) = Red;

val card_value_1 = card_value((Clubs, Num 4)) = 4;
val card_value_2 = card_value((Hearts, Num 9)) = 9;
val card_value_3 = card_value((Diamonds, Ace)) = 11;
val card_value_4 = card_value((Spades, King)) = 10;

val remove_card_1 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [];
val remove_card_2 = remove_card([(Hearts, Ace),(Diamonds, Ace)], (Diamonds, Ace), IllegalMove) = [(Hearts, Ace)];
val remove_card_3 = remove_card([(Hearts, Ace),(Hearts, Num 9),(Hearts, Num 4)], (Hearts, Num 4), IllegalMove) = [(Hearts, Ace),(Hearts, Num 9)];


val all_same_colour_1 = all_same_color([(Hearts, Num 1),(Hearts, Num 9),(Diamonds, Num 10)]) = true;
val all_same_colour_2 = all_same_color([(Hearts, Ace),(Hearts, Num 8),(Spades, Num 4)]) = false;
val all_same_colour_3 = all_same_color([(Spades, King),(Clubs, Num 2),(Clubs, King)]) = true;
val all_same_colour_4 = all_same_color([(Hearts, Num 5),(Clubs, Num 1),(Hearts, Ace)]) = false;



val sum_cards_1 = sum_cards([(Hearts, Num 1),(Hearts, Num 9),(Diamonds, Num 10)]) = 20;
val sum_cards_2 = sum_cards([(Hearts, Ace),(Diamonds, Ace)]) = 22;
val sum_cards_3 = sum_cards([(Hearts, Ace),(Hearts, Num 9),(Hearts, Num 4)]) = 24;

val score_1 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4;
val score_2 = score([(Hearts, King),(Diamonds, Num 3)],10) = 9 div 2;
val score_3 = score([(Spades, Num 9),(Clubs, Num 2),(Clubs, Num 1),(Clubs, Queen)],25) = 3 div 2;
val score_4 = score([(Hearts, Ace),(Clubs, Jack),(Hearts, Num 4)],7) = 54;


val officiate_1 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6;

val officiate_2 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3;

val officiate_3 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true);
