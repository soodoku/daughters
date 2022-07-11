*test for amount of selection
clear
set more off

capture log close
log using appendixtable2.log, replace
*state dummies as main spec


use genvote/congress/finalprograms/basic, replace
gen srvlngsq=srvlng*srvlng
gen agesq=age*age
qui tab region, gen(regd)

replace propgirls=. if totchi==0 
replace medinc=medinc/1000

reg propgirls demvote  medinc  perw perf percol perur dr1per dr2per dr3per dr4per alabort moredef  morecrime moreserv protgay   regd*  if congress==105  
test dr1per dr2per dr3per dr4per 
reg propgirls demvote  medinc  perw perf percol perur dr1per dr2per dr3per dr4per alabort moredef  morecrime moreserv protgay   regd*  if congress==106  
test dr1per dr2per dr3per dr4per 
reg propgirls demvote  medinc  perw perf percol perur dr1per dr2per dr3per dr4per alabort moredef  morecrime moreserv protgay   regd*  if congress==107  
test dr1per dr2per dr3per dr4per 
reg propgirls demvote  medinc  perw perf percol perur dr1per dr2per dr3per dr4per alabort moredef  morecrime moreserv protgay   regd*  if congress==108  
test dr1per dr2per dr3per dr4per 

exit

