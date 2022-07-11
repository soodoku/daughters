clear
set more off

capture log close
log using tab1t4.log, replace


use genvote/congress/finalprograms/basic

gen srvlngsq=srvlng*srvlng
gen agesq=age*age
qui tab totchi, gen(chid)
qui tab rgroup, gen(reld)
qui tab statalph, gen(sdum)
qui tab region, gen(regd)
replace rtl = 100-rtl

*table 1
*sample means 
summ anygirl ngirls totchi   white female age srvlng  demvote nowtot aauw rtl reld* if totchi~=. & congress==105
tab totchi if congress==105
sort repub
by repub: summ anygirl ngirls totchi   white female age srvlng  demvote nowtot aauw rtl reld* if totchi~=. & congress==105
by repub: tab totchi if congress==105

*table 2/appendix table 3
*column1 (table2)
reg nowtot ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==105
*column 2 (table2), column1 (at3)
reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==105
*column 3 (table2), column3 (at3)
reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==106
*column 4 (table2), column5 (at3)
reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==107
*column 5 (table2), column7 (at3)
reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid12 regd* demvote  if congress==108

*reamining columns of appendix table 3
*column 2 
reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 regd* demvote  if congress==105
*column 4
reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5  regd* demvote  if congress==106
*column 6 
reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5  regd* demvote  if congress==107
*column 8 
reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 regd* demvote  if congress==108

*table 3
sort female
by female: reg nowtot ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==105
by female: reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==105
by female: reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==106
by female: reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==107
by female: reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid12 regd* demvote  if congress==108

sort repub
by repub: reg nowtot ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==105
by repub: reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==105
by repub: reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==106
by repub: reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid11 regd* demvote  if congress==107
by repub: reg aauw ngirls white female repub  age agesq srvlng srvlngsq reld1 reld3-reld5 chid2-chid12 regd* demvote  if congress==108


*table 4
capture program drop _all
program define decomp
gen check=0
local i=1
while `i'<=20{
reg now`i' ngirls white female repub  age agesq srvlng srvlngsq reld* demvote chid* regd* 
gen b`i' =_b[ngirls] 
gen se`i'=_se[ngirls]
replace check = check + b`i' 
local i= `i'+1
}
*replace check  = check*5

end
decomp
summ b1-se20 check
exit
drop b1-se20 check








