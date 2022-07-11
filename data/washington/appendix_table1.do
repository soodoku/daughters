clear
set more off

capture log close
log using appendixtable1.log, replace

use genvote/congress/finalprograms/genold108
sort statenam dist name
save genvote/congress/finalprograms/genold108, replace

use genvote/congress/finalprograms/basic
keep if congress==108
sort statenam dist name
merge statenam dist name using genvote/congress/finalprograms/genold108
tab _merge
drop _merge

gen srvlngsq=srvlng*srvlng
gen agesq=age*age
qui tab totchi, gen(chid)
qui tab rgroup, gen(reld)
qui tab region, gen(regd)

*create a variable a dummy for whether oldes is a girl or a boy
gen df =1 if genold=="G"
replace df=0 if genold=="B"


*full congress, columns 1 and 2
reg ngirls df  white female repub   age agesq srvlng srvlngsq  reld* chid* regd* if totchi~=0
reg totchi df white female repub   age agesq srvlng srvlngsq  reld* regd* if totchi~=0

*by party
sort repub
*columns 3 and 5
by repub: reg ngirls df  white female repub  age agesq srvlng srvlngsq  reld* chid* regd* if totchi~=0
*columns 4 and 6
by repub: reg totchi df white female repub  age agesq srvlng srvlngsq  reld*  regd* if totchi~=0

exit
