/*table 1 Response Memo*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
est drop _all
quietly {
	qui logit tie_vs_notie omnivore ///
	age agesq women black hisp married 
	est store mod1
	qui logit tie_vs_notie omnivore ///
	age agesq women black hisp married ///
	deg2 deg3 deg4 deg5,robust
	est store mod2
	}
esttab mod*,stats(N ll chi2) lab nogaps nolines

/*table 3 Response Memo*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
est drop _all
quietly {	
	qui nbreg numcntct omnivore ///
	age agesq women black hisp married deg2 deg3 deg4 deg5
	est store mod1
	}
esttab mod*,stats(N ll chi2) lab nogaps nolines 

/*table 4 Response Memo*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
est drop _all
quietly {	
	qui mlogit strong_and_acqboss_vs_notie omnivore c.numcntct##c.numcntct ///
	age agesq women black hisp married deg2 deg3 deg4 deg5,
	est store mod2
	}
esttab mod*,stats(N ll chi2) lab nogaps nolines unstack


/*table 2 statistical significance*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
est drop _all
quietly {
	logit tie_vs_notie omnivore ///
	age agesq women black hisp married ///
	deg2 deg3 deg4 deg5,robust
	est store mod1
	mlogit strong_and_acqboss_vs_notie omnivore ///
	age agesq women black hisp married ///
	deg2 deg3 deg4 deg5,robust
	est store mod2
	}
esttab mod*,stats(N ll chi2) lab unstack nolines nogaps


/*figure 1 substantive significance*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
mlogit strong_and_acqboss_vs_notie omnivore ///
age agesq women black hisp married ///
deg2 deg3 deg4 deg5,robust

margins,at(omnivore=(0(1)7)) predict(pr out(2))
marginsplot, ///
	recast(scatter) recastci(rspike) ///
	ysize(6) xsize(4) ylab(0(.05).4,labs(small)) xlab(0(1)7) ///
	plotopts(mc(black) mfc(black) msize(large)) ///
	ytitle("Predicted Probability of Using a Weak Tie") ///
	title("",size(small)) ///
	yline(.2322 ) text(.228 6.5 "Sample Mean",size(vsmall)) ///
	ciopts(lc(gs10) lw(vthin)) name(gr1,replace)




/*figure 2*/
preserve
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
keep seemovie popmusic readfict visitart gomusic drama dance college acq_vs_othe boss_vs_other
cap gen acq_b=.
cap gen boss_b=.
cap gen percollege=.
cap gen lab=""
replace lab="Movies" in 1
replace lab="Pop. Music" in 2
replace lab="Read Fiction" in 3
replace lab="Go to the Museum" in 4
replace lab="Classical Music Perf." in 5
replace lab="Theater Perf." in 6
replace lab="Dance Perf." in 7
encode lab,g(enc_lab)

local i=1 
foreach x of varlist seemovie popmusic readfict visitart gomusic drama dance {	
qui sum college if `x'==1,meanonly
	replace percollege=r(mean) in `i'
	local i=`i'+1
	}

est drop _all
local i=1 
foreach y of varlist seemovie popmusic readfict visitart gomusic drama dance {
	qui logit `y' acq_vs_other
	qui margins,dydx(acq_vs_other)
	matrix A=r(b)
	qui replace acq_b=A[1,1] in `i'
	local i=`i'+1
	}

est drop _all
local i=1 
foreach y of varlist seemovie popmusic readfict visitart gomusic drama dance {
	qui logit `y' boss_vs_other
	qui margins,dydx(boss_vs_other)
	matrix A=r(b)
	qui replace boss_b=A[1,1] in `i'
	local i=`i'+1
	}
list lab acq_b boss_b percollege in 1/7,clean
restore

/*figure 3: testing the hypothesis that those who use weak ties have larger networks*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
recode omnivore 0=. 1/3=1 4/5=2 6/7=3,g(omnicat)
lab def omnicat 1 "1-3 Activities" 2  "4-5 Activities" 3 "6+ Activities"
lab val omnicat omnicat

est drop _all
mlogit strong_and_acqboss_vs_notie omnivore c.numcntct##c.numcntct ///
age agesq women black hisp married ///
deg2 deg3 deg4 deg5,robust
est store mod
esttab mod*,stats(N ll chi2) lab nogaps nolines unstack


margins,at(numcntct=(0(10)100)) predict(pr out(2)) 
marginsplot, ///
	ysize(6) xsize(6)  ///
	ylab(.2(.1).3) xlab(,labs(vsmall)) ///
	xtitle("N. of Contacts",size(small)) ///
	ytitle("Predicted Probability of Using a Weak Tie",size(small)) ///
	title("",size(small)) 
	noci
	 
