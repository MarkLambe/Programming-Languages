fun is_older (d1 : int*int*int, d2 : int*int*int) =
    #1 d1 < #1 d2 orelse (#1 d1 = #1 d2 andalso #2 d1 < #2 d2) orelse (#1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2)

fun number_in_month (dates : (int*int*int) list, month: int) =
    if null dates
    then 0
    else 
        if #2 (hd dates) = month
        then 1 + number_in_month (tl dates, month)
        else number_in_month (tl dates, month)

fun number_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else
        let val num = number_in_month (dates, hd months)
        in  
        if num > 0
        then num + number_in_months (dates, tl months)
        else number_in_months (dates, tl months)
        end

fun dates_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else 
        if #2 (hd dates) = month
        then hd dates :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else 
        let val temp_d = dates_in_month (dates, hd months)
        in
        if null temp_d
        then dates_in_months(dates, tl months)
        else temp_d @ dates_in_months(dates, tl months)
        end 

fun get_nth (l : string list, i: int ) =
    if i = 1
    then hd l
    else get_nth(tl l, i - 1)

fun date_to_string (d : (int*int*int)) =
    let val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
    in
    get_nth(months, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
    end

fun number_before_reaching_sum(sum: int, l: int list) =
    if sum <= hd l
    then 0
    else
    1 + number_before_reaching_sum(sum - hd l, tl l)

fun what_month(day: int) =
    let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 
    number_before_reaching_sum(day, months) + 1
    end

fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else
        if day1 = day2
        then [what_month(day1)]
        else
        what_month(day1) :: month_range (day1 + 1,day2)

fun oldest(l: (int*int*int) list) =
    if null l
    then NONE
    else
        let val ans = oldest(tl l)
        in if isSome ans andalso is_older(valOf ans,hd l)
            then ans
            else SOME (hd l)
        end