

*/mutinomial logit model*/
xi:mlogit learnjob2 omnivore educ prestg80 age i.sex i.black_hisp ,b(7)
test omnivore
cap drop *_hat
postgr3 omnivore,out(2) nodraw gen(closefr_hat)
postgr3 omnivore,out(3) nodraw gen(acquaint_hat)
postgr3 omnivore,out(6) nodraw gen(emp_hat)
postgr3 omnivore,out(7) nodraw gen(self_hat)
twoway line closefr_hat acquaint_hat emp_hat self_hat omnivore, ///
sort ylab(0(.1).25,nogrid)  ///
legend(label(1 "Close Friend") label(2 "Acquaintance") label(3 "Directly From Employer") label(4 "Called/Asked Myself")) ///
xtitle("Cultural Diversity Scale",size(small)) xlab(0(1)7) ysize(6) xsize(7) ///
ytitle("Predicted Probability",size(small)) 


	
	
global adjvars age i.sex i.black_hisp educ prestg80
qui xi3:mlogit learnjob2 omnivore $adjvars if yearsjob<6,b(8)
test omnivore
qui xi3:mlogit learnjob2 i.omnivore2 $adjvars if yearsjob<6,b(8)

testparm _Iomnivore*

cap drop *_hat
postgr3 omnivore,out(1) nodraw gen(relativ_hat)
postgr3 omnivore,out(2) nodraw gen(closefr_hat)
postgr3 omnivore,out(3) nodraw gen(acquaint_hat)
postgr3 omnivore,out(4) nodraw gen(empserv_hat)
postgr3 omnivore,out(5) nodraw gen(ad_hat)
postgr3 omnivore,out(6) nodraw gen(direct_hat)
postgr3 omnivore,out(7) nodraw gen(called_hat)
line  relativ_hat closefr_hat acquaint_hat omnivore, ///
sort ylab(0(.05).3,nogrid)  ///
legend(label(1 "Relative") label(2 "Close Friend") label(3 "Acquaintance")) ///
xtitle("Cultural Diversity Scale") xlab(1(1)7) ysize(6) xsize(7) ///
ytitle("Predicted Probability") ///
name(gr1,replace) nodraw

line empserv_hat ad_hat direct_hat called_hat omnivore2, ///
sort ylab(0(.05).3,nogrid)  ///
legend(label(1 "Emp. Service") label(2 "Ad/Sign") label(3 "Directly Contacted") label(4 "Called Myself")) ///
xtitle("Cultural Diversity Scale") xlab(1(1)3) ysize(6) xsize(7) ///
name(gr2,replace) nodraw

graph combine gr1 gr2,ysize(8) xsize(6) rows(2)





use "N:\Private\MISC DATA SOURCES\SPPA-2002\sppa-2002.dta", clear
do "N:\Private\MISC DATA SOURCES\SPPA-2002\sppa2002-ren.do"
egen omnivore=rsum2(classicalbin-amusparkbin),allmiss
keep perace prorigin pesex agecat prtage peeduca pareduc pel* pul* omnivore prestg80 caseid tot_educ tot_earn tsei prmarsta prnmchld
gen hisp=prorigin<8
keep if pulk==1|pulk==2
reshape long pulkm,i(caseid)
gen selectvar=pulkm!=-1
recode pulkm -1=.
recode pulkm 2/3=3 5=3 4=2 7=3 6=4 8=5 10=5 9=6 11=6 13=6,g(jobsearch)
lab def jobsearch 1 "Direct contact with Emp." 2 "Friends or relatives" 3 "Employment Agency/Service" 4 "Send Resume/Filled App." 5 "Placed/Answered/Looked at Ads" 6 "Other"
lab val jobsearch jobsearch
tab jobsearch,g(searchjob_meth)

gen friends_rel=pulkm==4
gen direct_emp=pulkm==1
gen emp_org=pulkm==2|pulkm==3|pulkm==5|pulkm==7
gen ad_resume=pulkm==6|pulkm==10

cap gen mean1=.
cap gen mean2=.
cap gen t=.
cap gen p=.
forval i=1/6 {
	qui xi:regress omnivore i.searchjob_meth`i' 
	testparm _Isearchjob*
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
	listtex mean* t p in 1/7,end(\\) rstyle(tabular) type



*/selection models*/
est drop _all
forval i=1/6 {
	qui xi:heckprob searchjob_meth`i' omnivore i.pesex i.perace i.hisp prtage peeduca,sel(selectvar=peeduca i.pesex i.perace i.married prestg80) cluster(caseid)
	est store mod`i'
	}
lab var omnivore "Cultural Variety Scale"
lab var prtage "Respondent's Age"
lab var _Ipesex_2 "Gender (Women=1)"
lab var _Iperace_2 "Race/Eth. (Black or Afr. Amer=1)"
lab var _Iperace_3 "Race/Eth. (Nat. Amer.=1)"
lab var _Iperace_2 "Race/Eth. (Asian/Pac. Isl.=1)"
lab var _Ihisp_1 "Race/Eth (Hispanic=1)"
lab var prestg80 "Occ. Prestige (Current Job)"
lab var peeduca "N. of Years of Education"
lab var prnmchld "N. of Children"
lab var _Imarried_1 "Mar. Status (Married=1)"
esttab mod*,stats(chi2 N,fmt(%9.2f %9.0f)) nogaps nolines compress b(%9.2f) t(%9.2f) par lab varw(25) drop(_cons selectvar:) unstack plain tex

xi:heckprob 

lab def netactiv 1 "Used a Network Strategy" 0 "Did not Use a network Strategy"
lab val netactiv netactiv
lab def netactivdet 1 "Did not Use a network Strategy" 2 "Contacted Friends and Family" 3 "Contacted the Employer Directly"

replace netactivdet=2 if netactivstr==1
replace netactivdet=3 if netactivwk==1
replace netactivdet=1 if netactiv==0
lab val netactivdet netactivdet



global mod i.pesex i.perace hisp prtage peeduca prestg80
for var pesex  perace hisp prtage peeduca prestg80 :drop if X==.

est drop _all
quietly {
	xi:eststo:logit netactiv omnivore if pulkm~=9,cluster(caseid)
	xi:eststo:logit netactiv omnivore $mod  if pulkm~=9,cluster(caseid)
	xi:eststo:logit netactivwk omnivore  if pulkm~=9,cluster(caseid) 
	xi:eststo:logit netactivstr omnivore  if pulkm~=9,cluster(caseid)
	}	
esttab,stats(chi2 N,star(chi2)) starlevels(* 0.05) nogaps


qui logit netactivwk omnivore
postgr3 omnivore,nodraw gen(wkpred)
qui logit netactivstr omnivore
postgr3 omnivore,nodraw gen(strpred)
twoway connected wkpred strpred omnivore,sort ///
ylab(0(.05).2,glc(gs14)) xlab(0(4)32) ///
 xsize(6) ysize(8) ///
ytitle("Predicted probability") ///
xtitle("Number of Cultural Forms") ///
legend(pos(6) label(1 "Contacted Employer Directly") ///
label(2 "Friends and Family")) 


graph box omnivore,over(netactiv) ytitle("Mean Number of Cultural Activities")

graph hbox omnivore, ///
over(netactivdet) ///
ytitle("Number of Cultural Activities") ///
ylab(0(2)32,glc(gs14))

use "N:\Private\NETWORKS AND CULTURE\cultural-capital-economic-capital\sppa-2002.dta", clear
do "N:\Private\SOCIOLOGY OF THE ARTS\SPPA-2002\sppa2002-ren.do"

keep if pelkdur~=.
egen omnivore=rsum2(classicalbin-amusparkbin),allmiss
keep perace prorigin pesex agecat prtage peeduca pareduc pelkdur  omnivore prestg80 tot_educ tot_earn tsei pulkm*
gen hisp=prorigin<8
center omnivore prestg80 peeduca
global mod i.pesex  i.perace hisp prtage c_peeduca c_prestg80
for var pesex  perace hisp prtage peeduca prestg80 :drop if X==.
est drop _all
quietly {
	xi:eststo:ztp  pelkdur omnivore $mod 
	}
esttab,stats(chi2 N,star(chi2)) starlevels(* 0.05) nogaps
gen omnixprest=omnivore*c_prestg80

est drop _all
quietly {
	xi:eststo:ztp pelkdur omnivore
	xi:eststo:ztp pelkdur omnivore $mod
	xi:eststo:ztp pelkdur omnivore $mod omnixprest
	}
esttab,stats(chi2 N,star(chi2)) starlevels(* 0.05) nogaps