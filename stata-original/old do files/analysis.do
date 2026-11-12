use "N:\Private\SPPA-2002\sppa-2002.dta", clear
do "N:\Private\SPPA-2002\sppa2002-ren.do"
recode peeduc 31/28=0 39=1 40/42=2 43=3 44/46=4,g(educ)
cap drop netdist 
cap drop netint
gen netdist=0
gen netint=0
foreach var of varlist classicalbin-amusparkbin {
	qui entropy perace pesex educ agecat if `var'==1
	replace netdist=netdist+(`r(Hdist)'*`var')
	replace netint=netint+(`r(Hint)'*`var')
	}
keep if pulk==1
compress
save "N:\Private\network-activation\netactiv.dta",replace


drop if musicpref<1
proxmat3 classicalbin-amusparkbin,svd cos1
mat C=r(distances)
keep if pulk==1
egen omnivore=rsum2(classicalbin-amusparkbin),allmiss

reshape long pulkm,i(caseid)
drop if pulkm==-1
tab pulkm
gen netactiv=0
gen netactivwk=0
gen netactivstr=0
for var pulkm*:replace netactiv=1 if X==1|X==4
for var pulkm*:replace netactivwk=1 if X==1
for var pulkm*:replace netactivstr=1 if X==4
gen possibties=(omnivore*(omnivore-1))/2	

foreach name in cultties wcultties stregnth id altdens propred red effecsize {
	cap drop `name'
	}
cap gen cultties=.
cap gen wcultties=.
cap gen strength=.
cap gen id=_n
qui levelsof id,l(levels)
local t=0
local c=0
local w=0
foreach l of local levels {
	local i=1
	foreach xi of varlist classicalbin-amusparkbin {
		local j=1
		qui sum `xi' in `l',meanonly
		local mi=r(mean)
		foreach xj of varlist classicalbin-amusparkbin {
			qui sum `xj' in `l',meanonly
			local mj=r(mean)
			if `mi'==1 & `mj'==1 &`i'~=`j' {
				local t=`t'+(0.5*C[`i',`j'])
				local c=`c'+1
				}
			local j=`j'+1
			}
		local i=`i'+1
		qui replace wcultties=`t' in `l'
		qui replace cultties=`c' in `l'
		}
	local t=0
	local c=0
	local w=0
	disp "Case: `l'"
	}
gen altdens=wcultties/possibties
gen red=(2*wcultties)/omnivore
gen effecsize=omnivore-red
gen propred=red/omnivore
gen omnixprop=omnivore*propred

xi3:biprobit ( netactivwk=omnivore propred omnixprop) (netactivstr=omnivore propred omnixprop),cluster(caseid)






lab def netactiv 1 "Used a Network Strategy" 0 "Did not Use a network Strategy"
lab val netactiv netactiv

graph bar omnivore,over( netactiv) ytitle("Mean Number of Cultural Activities")

global mod race1 race2 race4 hisp1 sex2 agecat peeduca 

est drop _all
qui logit netactiv omnivore,cluster(caseid)
est store mod1
qui logit netactiv omnivore $mod,cluster(caseid)
est store mod2
qui logit netactivwk omnivore,cluster(caseid) 
est store mod3
qui logit netactivstr omnivore,cluster(caseid) 
est store mod4
estout mod*, cells(b(star fmt(%9.4f)) t(par fmt(%9.2f))) ///
stats(chi2 bic N,fmt(%9.2f %9.1f %9.0f) star(chi2)) ///
starlevels(* 0.05) ///
label  varwidth(10)  style(fixed) legend unstack

qui logit netactivwk omnivore
postgr3 omnivore,nodraw gen(wkpred)
qui logit netactivstr omnivore
postgr3 omnivore,nodraw gen(strpred)


line wkpred strpred omnivore,sort ///
ylab(0(.05).2,nogrid) xsize(3) ysize(4) ///
ytitle("Predicted probability") ///
xtitle("Number of Cultural Forms") ///
legend(pos(6) label(1 "Contacted Employer Directly") ///
label(2 "Friends and Family"))

