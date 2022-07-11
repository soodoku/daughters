clear
set more off

capture log close
log using figure1.log, replace

use genvote/congress/finalprograms/basic

*top panel
*left
ci nowtot if congress==105 & totchi==2 & ngirls==0
ci nowtot if congress==105 & totchi==2 & ngirls==1
ci nowtot if congress==105 & totchi==2 & ngirls==2

*middle
ci nowtot if congress==105 & repub==0 & totchi==2 & ngirls==0
ci nowtot if congress==105 & repub==0 & totchi==2 & ngirls==1
ci nowtot if congress==105 & repub==0 & totchi==2 & ngirls==2

*right
ci nowtot if congress==105 & repub==1 & totchi==2 & ngirls==0
ci nowtot if congress==105 & repub==1 & totchi==2 & ngirls==1
ci nowtot if congress==105 & repub==1 & totchi==2 & ngirls==2

*bottom panel
*left
ci nowtot if congress==105  & totchi==3 & ngirls==0
ci nowtot if congress==105  & totchi==3 & ngirls==1
ci nowtot if congress==105  & totchi==3 & ngirls==2
ci nowtot if congress==105 & totchi==3 & ngirls==3

*middle
ci nowtot if congress==105 & repub==0 & totchi==3 & ngirls==0
ci nowtot if congress==105 & repub==0 & totchi==3 & ngirls==1
ci nowtot if congress==105 & repub==0 & totchi==3 & ngirls==2
ci nowtot if congress==105 & repub==0 & totchi==3 & ngirls==3

*right
ci nowtot if congress==105 & repub==1 & totchi==3 & ngirls==0
ci nowtot if congress==105 & repub==1 & totchi==3 & ngirls==1
ci nowtot if congress==105 & repub==1 & totchi==3 & ngirls==2
ci nowtot if congress==105 & repub==1 & totchi==3 & ngirls==3

