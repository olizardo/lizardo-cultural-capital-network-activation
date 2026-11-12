do "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\dataproc.do"
cap gen mean1=.
cap gen mean2=.
cap gen t=.
cap gen p=.
foreach j of numlist 6 {
	forval i=1/7 {
		qui xi:regress omnivore i.learnjob_meth`i'  if wrkstat<3
		testparm _Ilearnjob*
		mat b=e(b)
		mat v=e(V)
		quietly {
			replace mean1=b[1,2] in `i'
			replace mean2=b[1,2]+b[1,1] in `i'
			replace t=b[1,1]/sqrt(v[1,1]) in `i'
			replace p=r(p) in `i'
			format mean* t %9.2f
			format p %9.3f
			}
		}
	list mean* t p in 1/7,clean
	listtex names mean* t p in 1/7,end(\\) rstyle(tabular) type
	}

global cultlist sports30 music30 art30 tvmov30 health30 games30 humor30 sci30 hobby30 
cap drop *pref
gen numwkpref=0 if fin30!=.
gen numstrpref=0 if fin30!=.
foreach x of varlist $cultlist {
	replace numwkpref=numwkpref+1 if `x'==2
	replace numstrpref=numstrpref+1 if `x'==3|`x'==4
	}

*/adjusting for socio-demographic factors*/
est drop _all
forval i=1/7 {
	qui eststo: xi:probit learnjob_meth`i' omnivore educ prestg80 age i.sex i.black i.married childs black_hisp logsize if wrkstat<3
	}
lab var omnivore "Cultural Variety Scale"
lab var age "Respondent's Age"
lab var age_sq "Age Squared"
lab var _Isex_2 "Gender (Women=1)"
lab var childs "N. of Children"
lab var _Iblack_his_1 "Race/Eth. (Black or Hispanic=1)"
lab var prestg80 "Occ. Prestige (Current Job)"
lab var educ "N. of Years of Education"
lab var _Imarried_1 "Mar. Status (Married=1)"
lab var logsize "City Size in 1000s (logged)"
esttab,stats(chi2 N,fmt(%9.2f %9.0f)) nogaps nolines compress b(%9.2f) t(%9.2f) par lab varw(25) drop(_cons) unstack plain tex 

lab var seemovie "Movies"
lab var popmusic "Pop. Music"
lab var readfict "Reading"
lab var visitart "Galleries/Museums"
lab var gomusic "Symphony/Opera"
lab var dance "Dance"
lab var drama "Theater"

est drop _all
cap mat drop learn3
cap mat drop learn6
foreach i of numlist 3 6 {
	local j=1
	foreach x of varlist seemovie popmusic readfict visitart gomusic dance drama {
		qui eststo: xi:probit learnjob_meth`i' `x' educ prestg80 age i.sex i.black i.married childs black_hisp logsize if wrkstat<3
		qui mfx compute,nose
		if `j'==1 {
			mat learn`i'=e(Xmfx_dydx)'
			}
		else if `j'>1 {
			mat learn`i'=learn`i',e(Xmfx_dydx)'
			}
		local j=`j'+1
		}
	}
mat A=learn3[1,1..7]'
mat A=A,learn6[1,1..7]'
mat li A


gen graduate=degree==4
gen college=degree==3|degree==4
mat B=J(7,4,0)
local j=1
foreach w of varlist educ age prestg80 college {
	local i=1
	foreach x of varlist seemovie popmusic readfict visitart gomusic dance drama {
		qui sum `w' if `x'==1 
		mat B[`i',`j']=r(mean)
		local i=`i'+1
		}
	local j=`j'+1
	}
	