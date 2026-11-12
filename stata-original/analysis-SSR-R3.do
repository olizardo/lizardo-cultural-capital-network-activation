/*table 2 statistical significance*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
est drop _all
quietly {
	mlogit strong_and_acqboss_vs_notie omnivore ///
	age agesq women black hisp married ///
	deg2 deg3 deg4 deg5,robust
	est store mod1
	}
esttab mod*,stats(N ll chi2) lab unstack nolines nogaps 


/*figure 1 substantive significance*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
gen working = wrkstat < 4

qui mlogit strong_and_acqboss_vs_notie ///
c.omnivore##i.working ///
age agesq women black hisp married ///
deg2 deg3 deg4 deg5,robust
margins,at(omnivore=(0(1)7)) predict(pr out(2))
marginsplot, ///
	ciopts(lc(gs15) lw(vthin)) ///
	plotopts(msym(diamond) mc(white) msiz(medium) mfc(gs2) lc(gs2) lw(vthin)) ///
	ysize(6) xsize(4) ylab(0(.05).4,labs(small) nogrid) xlab(0(1)7) ///
	ytitle("Predicted Probability of Using a Weak Tie") ///
	title("",size(small)) ///
	yline(.2322, lp(dash) lc(gs10)) text(.228 6.5 "Sample Mean",size(vsmall)) ///
	name(gr1,replace)
qui margins,at(omnivore=(0(1)7)) predict(pr out(2)) within(working)
marginsplot, ///
	ciopts(recast(rspike) lc(gs13) lw(vthin)) ///
	plot1opts(recast(scatter) ///
	msym(diamond) mc(white) mfc(gs8) msize(large)) ///
	plot2opts(recast(scatter) ///
	msym(square) mc(white) mfc(gs1) msize(large)) ///
	ysize(6) xsize(4) ylab(0(.05).5,labs(small) nogrid) xlab(0(1)7) ///
	ytitle("Predicted Probability of Using a Weak Tie") ///
	title("",size(small)) ///
	yline(.2322 ) text(.228 6.5 "Sample Mean",size(vsmall)) ///
	legend(label(1 "Currently Employed") label(2 "Not Employed")) ///
	name(gr2,replace)


/*figure 1 substantive significance (by yearsjob)*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
gen working = wrkstat < 4
recode yearsjob 0/1 = 1 2/50 = 2,g(yearsjob1)
lab def yearsjob1 1 "Less than an Year on Job" 2 "More than a Year on Job"
lab val yearsjob1 yearsjob1
qui mlogit strong_and_acqboss_vs_notie ///
c.omnivore##i.yearsjob1 ///
age agesq women black hisp married ///
deg2 deg3 deg4 deg5 if working == 1,robust
qui margins,at(omnivore=(0(1)7)) predict(pr out(2)) within(yearsjob1)
marginsplot, ///
	ciopts(recast(rspike) lc(gs13) lw(vthin)) ///
	plot1opts(msym(diamond) mc(white) mfc(gs8) lw(vthin) lc(gs8)) ///
	plot2opts(msym(square) mc(white) mfc(gs1) lw(vthin) lc(gs1)) ///
	ylab(0(.05).4,labs(small) nogrid) xlab(0(1)7) ///
	ytitle("Predicted Probability of Using a Weak Tie") ///
	title("",size(small)) ///
	yline(.256  ) ///
	text(.252 6.5 "Sample Mean", size(vsmall)) ///
	name(gr1,replace) ///
	ysize(6) xsize(4) ///
	legend(pos(6))
	
	



/*figure 2*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
cd "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation"

do figure2.do
preserve
keep acq_b boss_b percollege enc_lab
keep in 1 / 7
sort percollege
twoway bar acq_b enc_lab || ///
bar boss_b enc_lab || ///
connected percollege enc_lab, yaxis(2) ///
xlab(1(1)7,val angle(45) labs(small))
restore




/*table 1 response memo*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
est drop _all
quietly {
	mlogit strong_and_acqboss_vs_notie omnivore yearsjob ///
	age agesq women black hisp married ///
	deg2 deg3 deg4 deg5 if wrkstat==1,robust
	est store mod1
	}
esttab mod*,stats(N ll chi2) lab unstack nolines nogaps 

/*table 2 response memo*/
use "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",clear
foreach x of varlist hi pop {
	sum `x'
	replace `x' = (`x' - r(min)) / (r(max) - r(min))
	}
sum hi pop
est drop _all
quietly {
	mlogit strong_vs_acq_vs_boss_vs_other hi pop  ///
	age agesq women black hisp married ///
	deg2 deg3 deg4 deg5 if yearsjob,robust
	est store mod1
	}
esttab mod*,stats(N ll chi2) lab unstack nolines nogaps 
margins,at(hi = (0(.1)1)) pr(out(2))
margins,at(pop = (0(.1)1)) pr(out(2))
margins,at(hi = (0(.1)1)) pr(out(3))
margins,at(pop = (0(.1)1)) pr(out(3))
