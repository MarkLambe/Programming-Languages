(* compare two dates which one is older *)
fun is_older (date1 : int*int*int, date2: int*int*int) =
    if #1 date1 > #1 date2
    then false
    else if #1 date1 < #1 date2
    then true
    else if #2 date1 > #2 date2
    then false
    else if #2 date1 < #2 date2
    then true
    else if #3 date1 >= #3 date2
    then false
    else true

(* count number of dates in list in a given month *)
fun number_in_month (date : (int*int*int) list, month : int) =
    if null date
    then 0
    else 
	if #2 (hd date) = month
	then 1 + number_in_month(tl date, month)
	else number_in_month(tl date, month)

(* count number of dates in list in given month in list *)
fun number_in_months(date : (int*int*int) list, month : int list) =
    if null month
    then 0
    else number_in_month(date, hd month) + number_in_months(date, tl month)

(* return list of dates in a given month *)
fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else 
	if #2 (hd dates) = month
	then (hd dates) :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

(* return list of dates in given month list *)
fun dates_in_months(dates : (int*int*int) list, month : int list) =
    if null month
    then []
    else dates_in_month(dates, hd month) @ dates_in_months(dates, tl month)

(* return the nth element of the string list *)
fun get_nth(str : string list, index : int) =
    let
	fun list_loop(strList: string list, i : int) =
	    if null strList
	    then ""
	    else if i = index
	    then hd strList
	    else list_loop(tl strList, i + 1)
    in 
	list_loop(str, 1)
    end

(* covert a date into a formatted string *)
fun date_to_string(date : int*int*int) =
    let
	val month = ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
    in
	get_nth(month, #2 date) ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* return the position which is the first element whose sum is less than target *)
fun number_before_reaching_sum(sum : int, numbers : int list) =
    let 
	fun find_num (prev_sum : int, numbers : int list, last_idx : int) =
	    if null numbers
	    then 0
	    else if hd numbers + prev_sum >= sum
	    then last_idx
	    else find_num(prev_sum + hd numbers, tl numbers, last_idx + 1)
    in
	if null numbers
	then 0
	else find_num(0, numbers, 0)
    end

(* return the month of the day  *)
fun what_month(day : int) =
    let
	val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, days_in_months) + 1
    end

(* return the month of the day between the range *)
fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* return the oldest date *)
fun oldest(date : (int*int*int) list) =
    let 
	fun find_oldest(oldest_temp : int*int*int, date_list : (int*int*int) list) =
	    let
		val result = is_older(oldest_temp, hd date_list)
	    in
		if null (tl date_list)
		then	    
		    if result
		    then SOME oldest_temp
		    else SOME (hd date_list)
		else 
		    if result
		    then find_oldest(oldest_temp, tl date_list)
		    else find_oldest(hd date_list, tl date_list)
	    end
    in
	if null date
	then NONE
	else find_oldest(hd date, tl date)
    end

(* search for element in the set *)
fun find_in_set(element : int, temp_set : int list) =
    if null temp_set
    then false (* can't find it in set *)
    else if element = hd temp_set
    then true
    else find_in_set(element, tl temp_set)

(* create non-duplicate set *)
fun create_set(all_months : int list, month_set : int list) =
    if null all_months
    then month_set
    else if find_in_set(hd all_months, month_set)
    then create_set(tl all_months, month_set)
    else create_set(tl all_months, hd all_months :: month_set)

(* count number of dates in list in given month in list *)
fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
    number_in_months(dates, create_set(months, []))

(* return list of dates in given month list *)
fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, create_set(months, []))

(* check if date is valid *)
fun reasonable_date(date : int*int*int) =
    let
	val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 

	(* test if it is leap year *)
	fun is_leap_year(year : int) =
	    if (year mod 400 = 0)
	    then true
	    else if (year mod 100 <> 0) andalso (year mod 4 = 0)
	    then true
	    else false

	(* return max days in a month *)
	fun get_days(month : int, days : int list) =
	    if null days
	    then 0
	    else if month = 1
	    then hd days
	    else get_days(month - 1, tl days)

	(* test if day of the month is valid *)
	fun is_day_valid(test_date : int*int*int) =
	    if #3 test_date < 1 orelse #3 test_date > 31
	    then false
	    else if (#2 test_date = 2) andalso is_leap_year(#1 test_date)
	    then
		(* check Feburary in leap year *)
		if (get_days(#2 test_date, days_in_month) + 1) >= #3 test_date
		then true
		else false
	    else if get_days(#2 test_date, days_in_month) >= #3 test_date
	    then true
	    else false

	(* test if year is valid *)
	fun is_year_valid(year : int) =
	    if year > 0
	    then true
	    else false

	(* test if month is valid *)
	fun is_month_valid(month : int) =
	    if month > 0 andalso month < 13
	    then true
	    else false
    in
	is_year_valid(#1 date) andalso is_month_valid(#2 date) andalso is_day_valid(date)
    end
