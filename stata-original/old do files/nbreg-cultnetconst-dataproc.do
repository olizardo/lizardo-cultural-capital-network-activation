use "N:\Private\SPPA\sppa_2002.dta", clear
do "N:\Private\SPPA\sppa2002-ren.do"
for var peeduc paeduc maeduc:recode X 31/38=0 39=1 40/42=2 43=3 44/46=4,g(Xrec)
egen pareducrec=rsum2(maeduc paeduc),allmiss
egen cultnet=rsum2(classicalbin- amusparkbin),anymiss
qui tab prhspnon,g(hisp)
qui tab perace,g(race)
qui tab pesex,g(sex)
qui tab agecat,g(age)
gen prtagesq=prtage^2
gen college=peeducarec>=3
gen married=prmarsta==1
gen kids=prchld~=0
gnbreg  cultnet pareducrec peeducarec sex1 race1 race2 prtage prtagesq married kids gecmsasz,lnalpha(pareducrec peeducarec sex1 race1 race2 prtage gecmsasz )
predict netconst,lnalpha

keep if pulk==1
reshape long pulkm,i(caseid)
drop if pulkm==-1
quietly {
	gen netactiv=pulkm==1|pulkm==4
	gen netactivwk=pulkm==1
	gen netactivstr=pulkm==4
	gen orgactiv=(pulkm>1&pulkm<4)|pulkm==5
	tab prcow1,g(prcowloc)
	tab prmjind1,g(prmjindus)
	}

est drop _all
quietly {
	xi3:logit netactivstr cultnet netconst   prmjindus2 prmjindus4 prmjindus5 prmjindus6 prmjindus7 prmjindus8 prmjindus10 prmjindus11 prmjindus12 prmjindus14 prmjindus16  prmjindus18 prmjindus19 prmjindus20 prmjindus21 prmjindus22  prcowloc1 prcowloc5 prcowloc6,cluster(caseid)
	est store mod1
	xi3:logit netactivwk cultnet netconst  prmjindus2 prmjindus3 prmjindus4 prmjindus5 prmjindus6 prmjindus7 prmjindus8 prmjindus10 prmjindus11 prmjindus12 prmjindus14 prmjindus15 prmjindus16 prmjindus17 prmjindus18 prmjindus19 prmjindus20 prmjindus21  prmjindus23 prcowloc5 prcowloc6 prcowloc1 prcowloc2 prcowloc3,cluster(caseid)
	est store mod2
	xi3:logit orgactiv cultnet netconst  prmjindus2 prmjindus4 prmjindus5 prmjindus6 prmjindus7 prmjindus8 prmjindus9 prmjindus10 prmjindus11 prmjindus12 prmjindus13 prmjindus14 prmjindus15  prmjindus18 prmjindus19 prmjindus20 prmjindus21 prmjindus23  prcowloc2 prcowloc3 prcowloc5 ,cluster(caseid)
	est store mod3
	}
	
use "N:\Private\gss\2002.dta", clear
gen college=degree>=3&degree<=4
recode educ 98=.
recode numcntct 998=.
recode numprobs 96=75 98=.
gen white=racecen1==1
gen black=racecen1==2
gen female=sex==2
recode rincom98 24/98=.
gen married=marital==1
gen agesq=age^2
gen kids=childs~=0 
gen cultnet=0 if popmusic~=.
for var popmusic seemovie visitart readfict drama dance gomusic:replace cultnet=cultnet+1 if X==1&X~=.

gnbreg  numprobs educ female white black age agesq married kids xnor,lnalpha(educ female white black age agesq married kids xnor)
predict persnetconst,alpha

gnbreg  cultnet educ female white black age agesq married kids xnor,lnalpha(educ female white black age agesq married kids xnor)
predict cultnetconst,lnalpha