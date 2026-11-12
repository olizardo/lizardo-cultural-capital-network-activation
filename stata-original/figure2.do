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
	local i=`i'+1
	}

est drop _all
local i=1 
foreach x of varlist seemovie popmusic readfict visitart gomusic drama dance {
	qui mlogit strong_vs_acq_vs_boss_vs_other `x' ///
	age agesq women black hisp married ///
	deg2 deg3 deg4 deg5,robust
	qui margins,dydx(`x') predict(out(2))
	matrix A = r(b)
	qui replace acq_b = A[1,1] in `i'
	qui margins,dydx(`x') predict(out(3))
	matrix A = r(b)
	qui replace boss_b = A[1,1] in `i'
	local i = `i'+1
	}

list lab acq_b boss_b percollege in 1/7,clean
