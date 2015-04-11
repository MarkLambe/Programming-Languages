(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3provided.sml";

val test1a = only_capitals ["A","B","C"] = ["A","B","C"]

val test1b = only_capitals ["A","b","C"] = ["A","C"]

val test1c = only_capitals ["A","B","Cat"] = ["A","B","Cat"]

val test2a = longest_string1 ["A","bc","C"] = "bc"

val test2b = longest_string1 ["About","bc","C"] = "About"

val test2c = longest_string1 ["A","bc","Ca"] = "bc"

val test3a = longest_string2 ["A","bc","C"] = "bc"

val test3b= longest_string2 ["About","bc","C"] = "About"

val test3c = longest_string2 ["A","bc","Ca"] = "Ca"

val test4a = longest_string1 ["A","bc","C"] = "bc"

val test4b = longest_string1 ["About","bc","C"] = "About"

val test4c = longest_string1 ["A","bc","Ca"] = "bc"

val test4d = longest_string2 ["A","bc","C"] = "bc"

val test4e = longest_string2 ["About","bc","C"] = "About"

val test4f = longest_string2 ["A","bc","Ca"] = "Ca"

val test5a = longest_capitalized ["A","bc","C"] = "A";

val test5b = longest_capitalized ["about","bC","C"] = "C";

val test5c = longest_capitalized ["About", "Abour", "abouts"] = "About";

val test6a = rev_string "abc" = "cba";

val test6b = rev_string "" = "";

val test6c = rev_string "aBbc" = "cbBa";

val test7a = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4;

val test7b = first_answer (fn x => if x < 13 then SOME x else NONE) [21,32,3,4,5] = 3;

val test7c = first_answer (fn x => if x = 5 then SOME x else NONE) [1,2,3,4,5] = 5;

val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE;

val test8b = all_answers (fn x => if x > ~1 then SOME [x] else NONE) [~2,3,4,~5,6,7] = NONE;

val test8c = all_answers (fn x => if x > 0 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7];

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1;

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME [] 


