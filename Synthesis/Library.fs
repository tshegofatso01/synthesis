module Synthesis
open System

let abelar x = (x > 12 && x < 3097 && x%12=0 )

let area b h =
   match (b >= 0.0) && (h >= 0.0) with
   | true -> 0.5*b*h
   | false -> failwith("Can't handle negative values")
    

let zollo a =
   match a > 0 with
   | true -> a*2
   | false -> a*(-1)


let min a b =
   match a > b with
   |true -> b
   |false -> a


let max a b =
    match a < b with
    |true -> b
    |false -> a

let ofTime h m s = h*60*60+m*60+s
 
let toTime s = 
   match s > 0 with
   | true -> ((s/3600),((s-((s/3600)*3600))/60),(s-(s-(s-(((s-((s/3600)*3600))/60))*60)%60))) 
   | false -> (0,0,0)
   
let digits n =
   let rec count n digitnr = 
      match n<>0 with
      | true -> count (n/10) (digitnr+1)
      | false -> max digitnr 1     
   count n 0 
 

let minmax (a,b,c,d) = (min (min a b)(min c d) , max(max a b)(max c d))


let isLeap y =
   match y < 1582 with
   | true -> failwith "Can't be less than year leap was implemented"
   | false -> match ((y % 4 = 0) && ((y % 100 <> 0) || (y % 400 = 0) )) with
              | true -> true
              | false -> false
   
   
  
let month m =
   match m with
   | 1 -> ("January",31)
   | 2 -> ("February",28)
   | 3 -> ("March",31)
   | 4 -> ("April",30)
   | 5 -> ("May",31)
   | 6 -> ("June",30)
   | 7 -> ("July",31)
   | 8 -> ("August",31)
   | 9 -> ("September",30)
   | 10 -> ("October",31)
   | 11 -> ("November",30)
   | 12 -> ("December",31)
   | _ -> failwith "Not a valid int"

let toBinary i =
   match i < 0 with
   |true -> failwith "NOPE"
   |false -> match i=0 with
             | true -> "0"
             | false -> let rec binary i =
                           match (i=0) with
                           | true -> ""
                           | false -> match i%2=0 with
                                      |true -> binary(i/2)+"0"
                                      |false ->binary(i/2)+"1"
                        binary i 


let bizFuzz n =
   let rec fizzing (m,c3s,c5s,both) = 
      match (m <0 || m=1) with
      | true -> (c3s,c5s,both)
      | false -> match m%3=0 with
                 | true -> match m%5=0 with
                           | true -> fizzing (m-1,c3s+1,c5s+1,both+1)
                           | false -> fizzing(m-1,c3s+1,c5s,both)
                 | false -> match m%5=0 with
                            | true -> fizzing(m-1,c3s,c5s+1,both)
                            | false -> fizzing(m-1,c3s,c5s,both)
   
   fizzing (n,0,0,0) 
   

let monthDay day y =
  let getMonthName index =
    let (monthName, _) = month index
    monthName

  let rec subtract index days _list =
    match _list with 
    | head::tail -> 
      match days>head with 
      | true -> subtract (index+1) (days-head) tail
      | false -> index
    | _ -> failwith ""

  let isInvalidCase = day<1 || y<1582
  let monthIndex = 
    match isInvalidCase, (isLeap y) with 
    | true, _ -> failwith ""
    | _, true -> subtract 1 day [31;29;31;30;31;30;31;31;30;31;30;31]
    | _, false -> subtract 1 day [31;28;31;30;31;30;31;31;30;31;30;31]

  getMonthName monthIndex

let sqrt n =
    let rec calculate guess i =
        match i with
        | 10 -> guess
        | _ ->
            let g = (guess + n/guess) / 2.0
            calculate g (i+1)
    match n <= 0.0 with
    | true -> failwith "Impossibru!"
    | _ ->
        calculate (n/2.0) 0


let coord x =
   let distance p2 =
      match x,p2 with |((x1,y1),(x2,y2)) -> sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
