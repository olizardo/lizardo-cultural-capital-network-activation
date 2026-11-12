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

/*table 2 Response Memo*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
est drop _all
quietly {
	qui mlogit strong_and_acqboss_vs_notie omnivore ///
	age agesq women black hisp married  ///
	deg2 deg3 deg4 deg5,robust
	est store mod1
	}
esttab mod*,stats(N ll chi2) lab nogaps nolines unstack


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


/*table 5 Response Memo*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
capture {
	recode omnivore 0=. 1/3=1 4/5=2 6/7=3,g(omnicat)
	lab def omnicat 1 "1-3 Activities" 2  "4-5 Activities" 3 "6+ Activities"
	lab val omnicat omnicat
	}
est drop _all
quietly {	
	qui logit acqboss_vs_strong c.numcntct##c.numcntct##i.omnicat ///
    age agesq women black hisp married deg2 deg3 deg4 deg5
	est store mod1
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
	logit strong_vs_other omnivore ///
	age agesq women black hisp married ///
	deg2 deg3 deg4 deg5,robust
	est store mod2
	logit acqboss_vs_other omnivore ///
	age agesq women black hisp married ///
	deg2 deg3 deg4 deg5,robust
	est store mod3
	}
esttab mod*,stats(N ll chi2) lab

/*figure 1 substantive significance*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
logit tie_vs_notie omnivore ///
age agesq women black hisp married ///
deg2 deg3 deg4 deg5,robust
margins,at(omnivore=(0(1)7))
marginsplot, ///
	recast(scatter) recastci(rspike) ///
	ysize(6) xsize(4) ylab(.3(.05).7,labs(small)) xlab(0(1)7) ///
	plotopts(mc(black) mfc(black) msize(large)) ///
	ytitle("Predicted Probability of Using Strong or Weak Tie") ///
	title("All Respondents",size(small)) ///
	yline(.5346821) text(.52 5.7 "Sample Mean",size(vsmall)) ///
	ciopts(lc(gs10) lw(vthin)) nodraw name(gr1,replace)
logit strong_vs_other omnivore ///
age agesq women black hisp married ///
deg2 deg3 deg4 deg5,robust
margins,at(omnivore=(0(1)7))
marginsplot, ///
	recast(scatter) recastci(rspike) ///
	ysize(6) xsize(4) ylab(.3(.05).7,labs(small)) xlab(0(1)7) ///
	plotopts(mc(black) mfc(black) msize(large)) ///
	ytitle("Predicted Probability of Using A Strong Tie") ///
	title("Excluding Those who used a Weak Tie",size(small)) ///
	yline(.3939774 ) text(.38 5.7 "Sample Mean",size(vsmall)) ///
	ciopts(lc(gs10) lw(vthin)) nodraw name(gr2,replace)
	
logit acqboss_vs_other omnivore ///
age agesq women black hisp married ///
deg2 deg3 deg4 deg5,robust
margins,at(omnivore=(0(1)7))
marginsplot, ///
	recast(scatter) recastci(rspike) ///
	ysize(6) xsize(4) ylab(.3(.05).7,labs(small)) xlab(0(1)7) ///
	plotopts(mc(black) mfc(black) msize(large)) ///
	ytitle("Predicted Probability of Using A Weak Tie") ///
	title("Excluding Those who Used a Strong Tie",size(small)) ///
	yline(.3328729) text(.32 5.7 "Sample Mean",size(vsmall)) ///
	ciopts(lc(gs10) lw(vthin)) nodraw name(gr3,replace)
graph combine gr1 gr3,rows(1) ysize(5) xsize(6.5) 


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
qui sum college if `x' == 1,meanonly
	replace percollege = r(mean) in `i'
	local i = `i'+1
	}

est drop _all
local i=1 
foreach y of varlist seemovie popmusic readfict visitart gomusic drama dance {
	qui logit `y' acq_vs_other
	qui margins,dydx(acq_vs_other)
	matrix A = r(b)
	qui replace acq_b = A[1,1] in `i'
	local i = `i'+1
	}

est drop _all
local i=1 
foreach y of varlist seemovie popmusic readfict visitart gomusic drama dance {
	qui logit `y' boss_vs_other
	qui margins,dydx(boss_vs_other)
	matrix A = r(b)
	qui replace boss_b = A[1,1] in `i'
	local i = `i'+1
	}
list lab acq_b boss_b percollege in 1/7,clean
restore

/*figure 3: testing the hypothesis that those who use weak ties have larger networks*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
capture {
	drop omnicat
	cap recode omnivore 0=. 1/3=1 4/5=2 6/7=3,g(omnicat)
	lab def omnicat 1 "1-3 Activities" 2  "4-5 Activities" 3 "6+ Activities"
	lab val omnicat omnicat
	}
qui logit acqboss_vs_strong c.numcntct##c.numcntct##i.omnicat ///
age agesq women black hisp married deg2 deg3 deg4 deg5
margins,at(numcntct=(0(10)100)) predict(pr) over(omnicat)
marginsplot, ///
	ysize(6) xsize(6)  ///
	xlab(,labs(vsmall)) ///
	xtitle("N. of Contacts",size(small)) ///
	ytitle("Predicted Probability of Using a Weak Tie",size(small)) ///
	title("Respondents Who  Used a Social Contact",size(small)) ///
	noci 
