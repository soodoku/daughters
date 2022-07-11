clear
set more off
set mem 500m
set matsize 1200
set maxvar 20000

capture log close
log using table5t6.log, replace

*first, run regression for each vote (by congress) and save coefficient, se and t stats
*reshape so each vote is an observation


use genvote/congress/finalprograms/allvotes5_8
keep if congress==105

gen srvlngsq=srvlng*srvlng
gen agesq=age*age
qui tab totchi, gen(chid)
qui tab rgroup, gen(reld)
qui tab region, gen(regd)

capture program drop _all
program define doit
foreach var of varlist voteyesv1-voteyesv1166{
qui reg `var' ngirls white female repub  age agesq srvlng srvlngsq demvote reld* chid* regd* 
gen gc`var'=_b[ngirls]
gen gse`var'=_se[ngirls]
gen gt`var'=abs(gc`var'/gse`var')
}
end
doit

keep gcvoteyesv1-gtvoteyesv1166 congress
keep if _n==1
reshape long gcvoteyesv gsevoteyesv gtvoteyesv, i(congress) j(voteno)
save 105temp, replace



clear
use genvote/congress/finalprograms/allvotes5_8
keep if congress==106

gen srvlngsq=srvlng*srvlng
gen agesq=age*age
qui tab totchi, gen(chid)
qui tab rgroup, gen(reld)
qui tab region, gen(regd)

capture program drop _all
program define doit
foreach var of varlist voteyesv1-voteyesv1209{
qui reg `var' ngirls white female repub  age agesq srvlng srvlngsq demvote reld* chid* regd* 
qui gen gc`var'=_b[ngirls]
qui gen gse`var'=_se[ngirls]
qui gen gt`var'=abs(gc`var'/gse`var')
}
end
doit

keep gcvoteyesv1-gtvoteyesv1209 congress
keep if _n==1
reshape long  gcvoteyesv  gsevoteyesv  gtvoteyesv, i(congress) j(voteno)
save 106temp, replace


clear
use genvote/congress/finalprograms/allvotes5_8
keep if congress==107

gen srvlngsq=srvlng*srvlng
gen agesq=age*age
qui tab totchi, gen(chid)
qui tab rgroup, gen(reld)
qui tab region, gen(regd)

capture program drop _all
program define doit
foreach var of varlist  voteyesv1-voteyesv990{
qui reg `var' ngirls white female repub  age agesq srvlng srvlngsq demvote reld* chid* regd* 
qui gen gc`var'=_b[ngirls]
qui gen gse`var'=_se[ngirls]
qui gen gt`var'=abs(gc`var'/gse`var')
}
end
doit

keep gcvoteyesv1-gtvoteyesv990 congress
keep if _n==1
reshape long  gcvoteyesv gsevoteyesv gtvoteyesv, i(congress) j(voteno)
save 107temp, replace


clear
use genvote/congress/finalprograms/allvotes5_8
keep if congress==108

gen srvlngsq=srvlng*srvlng
gen agesq=age*age
qui tab totchi, gen(chid)
qui tab rgroup, gen(reld)
qui tab region, gen(regd)

capture program drop _all
program define doit
foreach var of varlist voteyesv1-voteyesv1218{
qui reg `var' ngirls white female repub age agesq srvlng srvlngsq demvote reld* chid* regd* 
qui gen gc`var'=_b[ngirls]
qui gen gse`var'=_se[ngirls]
qui gen gt`var'=abs(gc`var'/gse`var')
}
end
doit

keep gcvoteyesv1-gtvoteyesv1218 congress
keep if _n==1
reshape long gcvoteyesv  gsevoteyesv gtvoteyesv, i(congress) j(voteno)
save 108temp, replace


*now merge votes with Rohde's information on votes (unanimous, etc) and my coding of abortion votes


clear
use genvote/congress/finalprograms/rollcallcode
sort congress voteno
save genvote/congress/finalprograms/rollcallcode, replace

clear
use genvote/congress/finalprograms/abortionvotes
sort congress session cqvoteno
save genvote/congress/finalprograms/abortionvotes, replace



use 108temp
append using 107temp
append using 106temp
append using 105temp
sort congress voteno
merge congress voteno using genvote/congress/finalprograms/rollcallcode
tab _merge
drop _merge



sort congress session cqvoteno
merge congress session cqvoteno using genvote/congress/finalprograms/abortionvotes
tab _merge
gen abortion =1 if _merge==3
drop _merge descrip



*finally, collapse Rohde's categories

*all categories
*smbolic, internal, procedural
gen cat =0 if issue>=0 & issue<=99 | issue==261

*defense
replace cat =1 if (issue>=300 & issue<=399) | (issue>=110 & issue<=119) | issue==156 | (issue>=190 & issue<=199) | (issue>=240 & issue<=259)

*foreign policy
replace cat=2 if (issue>=400 & issue<=499) | (issue>=101 & issue<=102) | (issue>=140 & issue<=149)

*economic, taxes, budget
replace cat=3 if (issue>=500 & issue<=599) | issue==104

*energy
replace cat=4 if (issue>=600 & issue<=649) | (issue>=130 & issue<=139) | issue==161

*environment
replace cat=5 if issue>=650 & issue<=699 | issue==155 | issue==162 | issue==163

*government operations/civil rights/aid to localities
replace cat=6 if (issue>=900 & issue<=909) | (issue>=720 & issue<=739) | issue==108 | issue==176 | (issue>=700 & issue<=719) | (issue>=760 & issue<=779) | (issue>=105 & issue<=107) | (issue>=120 & issue<=129) | (issue>=180 & issue<=189) | (issue>=220 & issue<=229)

*campaigns and elections
replace cat=7 if issue>=740 & issue<=749

*crime
replace cat=8 if (issue>=750 & issue<=759) 

*social services
replace cat=9 if (issue>=800 & issue<=819) | (issue>=830 & issue<=869) | issue==151 | issue==154 | issue==173 | issue==174

*health
replace cat=10 if (issue>=820 & issue<=829) | (issue>=940 & issue<=949) | issue==172 | issue==175 

*agriculture: note one vote on FS is in here 205
replace cat=11 if (issue>=910 & issue<=919) | (issue>=200 & issue<=209)

*transportation
replace cat=12 if (issue>=920 & issue<=929) | (issue>=210 & issue<=219)

*labor
replace cat=13 if (issue>=960 & issue<=969) | issue==171

*mis domestic: includes consumer issue, arts and public, NASA
*only two votes classified as women's issues, they are in there
replace cat=14 if (issue>=950 & issue<=959) | (issue>=970 & issue<=999) | issue==152 |issue==164  | issue==939

*mis appopropriations: use when doesn't fit any categories above or fits multiple categories
replace cat=15 if (issue>=270 & issue<=299) | issue==100 | issue==109 | issue==150 | issue==159 | issue==160 | issue==170 | issue==169 | issue==179

*now do abortion
replace cat=16 if issue ==103 | issue==121 | issue==948  
replace cat=16 if abortion==1 


* dummies for significance level
gen yesv10s =1 if gtvoteyesv>1.64
replace yesv10s=0 if yesv10s==.

*redo dummies to only indicate significant and daughters in liberal direction
gen votelib=1 if nearunam==0 & partyunity==1 & gcvoteyes>0 & demvoteyes>demvoteno
replace votelib=1 if nearunam==0 & partyunity==1 & gcvoteyes<0 & demvoteno>demvoteyes
replace votelib=0 if nearunam==0 & partyunity==1 & gcvoteyes>=0 & demvoteno>demvoteyes
replace votelib=0 if nearunam==0 & partyunity==1 & gcvoteyes<=0 & demvoteyes>demvoteno
gen yesvl10s =1 if gtvoteyesv>1.64 & votelib==1
replace yesvl10s=0 if yesvl10s==. 
 

*table 5 columns 1a and 1b
sort cat
by cat: ttest yesvl10s=.10 if nearunam==0 & partyunity==1

*figure 2
by cat: ci yesvl10s if nearunam==0 & partyunity==1


*column table 5 1c
capture program drop _all
program define testeq
local i=0
while `i'<=15{
sort cat
ttest (yesvl10s) if nearunam==0 & partyunity==1 & (cat==16 | cat==`i'), by (cat)
local i=`i'+1
}
end
testeq


*table 6 columns 1a-b, 2a-b, 3a-b and 4a-b
sort congress cat
by congress cat: ttest yesvl10s=.10 if nearunam==0 & partyunity==1

*table 6 columns c
capture program drop _all
program define testeq
local j=105
while `j'<=108{
local i=0
while `i'<=15{
sort cat
ttest (yesvl10s) if nearunam==0 & partyunity==1 & (cat==16 | cat==`i') & congress==`j', by (cat)
local i=`i'+1
}
local j=`j'+1
}
end
testeq

erase 105temp.dta
erase 106temp.dta
erase 107temp.dta
erase 108temp.dta
