use  "N:\Private\MISC DATA SOURCES\GSS\2002.dta",clear
drop if learnjob==.
keep learnjob yearsjob soc* seemovie popmusic readfict drama dance visitart gomusic ///
educ degree age sex racecen1 wrkstat prestg80 marital numcntct numprobs paeduc maeduc ///
padeg madeg
gen black=racecen1==2
gen hisp=racecen1==16
gen married=marital==1
gen college=degree>2
gen women=sex==2
gen agesq=age^2/100
recode numcntct 998=.
recode numprobs 98=.
gen numwk=numcntct-numprobs
tab degree,g(deg)

recode learnjob 1=. 2/3=1 4=2 5=3 6/8=4 9=5 10=6 11=7 98=.,g(learnjob2)
lab def learnjob2 1 "relatives" 2 "close friend" 3 "acquaintance" ///
4 "emp. service/school" 5 "advertisement/sign" ///
6 "directly from employer" 7 "called/asked myself" 
lab val learnjob2 learnjob2
gen acq=learnjob2==3
gen boss=learnjob2==6

recode learnjob2 1/3=1 4/5=0 6=1 7=0,g(tie_vs_notie)
recode learnjob2 1/2=0 3=1 4/5=. 6=1 7=.,g(acqboss_vs_strong)
recode learnjob2 1/2=0 3=1 4/5=. 6=. 7=.,g(acq_vs_strong)
recode learnjob2 1/2=0 3=. 4/5=. 6=1 7=.,g(boss_vs_strong)
recode learnjob2 1/2=. 3=1 4/5=0 6=0 7=0,g(acq_vs_other)
recode learnjob2 1/2=. 3=. 4/5=0 6=1 7=0,g(boss_vs_other)
recode learnjob2 1/2=. 3=1 4/5=0 6=. 7=0,g(acqboss_vs_other)
recode learnjob2 1/2=1 3=. 4/5=0 6=. 7=0,g(strong_vs_other)
recode learnjob2 1/2=1 3=2 4/5=0 6=3 7=0,g(strong_vs_acq_vs_boss_vs_other)
recode learnjob2 1/2=1 3=2 4/5=0 6=2 7=0,g(strong_and_acqboss_vs_notie)



for var seemovie popmusic readfict drama dance visitart gomusic:recode X 2/9=0
lab def plusminus 1 + 0 -
for var seemovie popmusic readfict drama dance visitart gomusic: lab val X plusminus
egen omnivore=rsum2(seemovie popmusic readfict drama dance visitart gomusic),anymiss
for var seemovie popmusic drama dance visitart gomusic readfict ///
educ degree age sex racecen1 wrkstat prestg80 marital:drop if X==.

lab var omnivore "Cultural Variety"
lab var age "Age"
lab var agesq "Age Squared"
lab var women "Women"
lab var black "Black/Afr. Amer."
lab var hisp "Hispanic"
lab var married "Married"
lab var college "College Degree"
lab var prestg80 "Occ. Prestige"
lab var deg2 "High School"
lab var deg3 "Some College"
lab var deg4 "College"
lab var deg5 "Graduate/Prof."
lab var numcntct "N. of Contacts."

factor seemovie-gomusic,pcf
rot,pro
predict hi pop
save "N:\Private\SOCIOLOGY OF CULTURE\cultural-capital-network-activation\using.dta",replace




