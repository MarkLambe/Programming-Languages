(* problem 1 *)
(* is date 1 older than date 2 *)
(* return bool *)
fun is_older(d1: int*int*int, d2: int*int*int) =
  (365*(#1 d1)+30*(#2 d1)+(#3 d1))<(365*(#1 d2)+30*(#2 d2)+(#3 d2)) 

(* problem 2 *)
(* how many times does m appear in list *)
(* return int *)
fun number_in_month(L: (int*int*int) list, m: int) =
  if null L
  then 0
  else 
    if #2 (hd L) = m 
    then 1 + number_in_month(tl L, m)
    else 0 + number_in_month(tl L, m)

(* problem 3 *)
fun number_in_months(L: (int*int*int) list, M: int list)=
  if null M
  then 0
  else number_in_month(L, hd M) + number_in_months(L, tl M)

(* problem 4 *) 
fun dates_in_month(L: (int*int*int) list, m: int) =
  if null L
  then []
  else 
    if #2 (hd L) = m 
    then hd L :: dates_in_month(tl L, m)
    else dates_in_month(tl L, m)

(* problem 5 *)
fun dates_in_months(L: (int*int*int) list, M: int list) =
  if null M
  then []
  else dates_in_month(L, hd M) @ dates_in_months(L, tl M)

(* problem 6 *)
fun get_nth(L: string list, n: int) = 
  if n = 1 then hd L else get_nth(tl L, n-1) 

(*  problem 7 *)
val monthList = ["January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"]
fun date_to_string(date: (int*int*int)) = 
  get_nth(monthList, (#2 date)) ^" "^ Int.toString(#3 date) ^", "^ Int.toString(#1 date)

(* problem 8*)
fun number_before_reaching_sum(n: int, L: int list) =
  let
    fun helper(index: int, runningSum: int, valList: int list) =
      if runningSum + hd valList >= n then index
      else helper(index+1, runningSum + hd valList, tl valList)
  in 
    helper(0,0, L)
  end

(* problem 9 *)
val daysMonths = [31,28,31,30,31,30,31,31,30,31,30,31]
fun what_month(d: int) =
  number_before_reaching_sum(d, daysMonths)+1

(* problem 10 *)
fun month_range(d1: int, d2: int) =
  if d1 > d2
  then []
  else what_month(d1) :: month_range(d1+1,d2)





