
clear

erase "estimates_HIV.dta"
set scheme s1mono
cap program drop specchart
program specchart
syntax varlist, [replace] spec(string)
	* save current data
	tempfile temp
	save "`temp'",replace
	*dataset to store estimates
	if "`replace'"!=""{
			clear
			gen beta=.
			gen se=.
			gen spec_id=.
			gen u95=.
			gen u90=.
			gen l95=.
			gen l90=.
			save "estimates_HIV.dta",replace
	}	
	else{
		* load dataset
		use "estimates_HIV.dta",clear
	}

	* add observation
	local obs=_N+1
	set obs `obs'
	replace spec_id=`obs' if _n==`obs'
	* store estimates
	replace beta =_b[`varlist'] if  spec_id==`obs'
	replace se=_se[`varlist']   if  spec_id==`obs'
	replace u95=beta+invt(e(df_r),0.975)*se if  spec_id==`obs'
	replace u90=beta+invt(e(df_r),0.95)*se if  spec_id==`obs'
	replace l95=beta-invt(e(df_r),0.975)*se  if  spec_id==`obs'
	replace l90=beta-invt(e(df_r),0.95)*se  if  spec_id==`obs'
	* store specification
	foreach s in `spec'{
		cap gen `s'=1 			if  spec_id==`obs'
		cap replace `s'=1 		 if  spec_id==`obs'
	}
		save "estimates_HIV.dta",replace
	* restore dataset
	use `temp',clear
end

local xvars Hpct BLpct
local policyvars prescriptionlimit tamper idrequirement doctorshopping physicianexam pharma pdmp painclinic paraph samaritan
global region r20081-r20164  //region-year fixed effects
global stateyear s20081-s201651


* run regressions 
	use "./Data/torun.dta", clear

* main spec
reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_nevertreat==1 & sample_drop==0 & year>2007, absorb(countycode year) cluster(countycode)
  
specchart syringeineff,spec(main samplenevertreat covars4 short cfe yfe sbyyfe drop) replace


* ALTERNATIVE SPECS
* loop over sample size
foreach j in nevertreat all clinics border {
foreach d in drop {
	clear
	use "./Data/torun.dta"
	
	drop s20*
forvalues m=2003/2016{
forvalues i=1/51{
	gen year`m'=(year==`m')
	gen stategroup`i'=(stategroup==`i')
	gen s`m'`i'=year`m'*stategroup`i'
	drop year`m'
	drop stategroup`i'
} 
}

global stateyearl s20031-s201651

	g timel=year-2003
	g timel2=timel^2

	* main spec for other comparison groups
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyearl if sample_`j'==1 & sample_drop==0, absorb(countycode year) cluster(countycode)
  specchart syringeineff,spec(covars4 linear sample`j' noshort cfe yfe sbyyfe drop) 

	* no covars 
	reghdfe HIV_ratenl syringeineff if sample_`j'==1  & sample_drop==0, absorb(countycode year i.countycode##c.timel) cluster(countycode)
	specchart  syringeineff,spec(covars0 linear sample`j' noshort cfe yfe drop) 

	* econ covars 
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel) cluster(countycode)
	specchart  syringeineff,spec(covars2 linear sample`j' noshort cfe yfe drop) 

	*just demo
	reghdfe HIV_ratenl syringeineff `xvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel) cluster(countycode)
	specchart  syringeineff,spec(covars3 linear sample`j' noshort cfe yfe drop) 

	* econ + demo covars 
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel) cluster(countycode)
	specchart  syringeineff,spec(covars4 linear sample`j' noshort cfe yfe drop) 

	*only policy demo
	reghdfe HIV_ratenl syringeineff `policyvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel) cluster(countycode)
	specchart  syringeineff,spec(covars5 linear sample`j' noshort cfe yfe drop) 


	* all covars 
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel) cluster(countycode)
	specchart  syringeineff,spec(covars6 linear sample`j' noshort cfe yfe drop) 

	
	* no covars + quadratric
	reghdfe HIV_ratenl syringeineff if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel i.countycode##c.timel2) cluster(countycode)
	specchart  syringeineff,spec(covars0 quad sample`j' noshort cfe yfe drop) 
	
	* econ covars + quadratic
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel i.countycode##c.timel2) cluster(countycode)
	specchart  syringeineff,spec(covars2 quad sample`j' noshort cfe yfe drop) 

	*just demo + quadratic
	reghdfe HIV_ratenl syringeineff `xvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel i.countycode##c.timel2) cluster(countycode)
	specchart  syringeineff,spec(covars3 quad sample`j' noshort cfe yfe drop) 

	* econ + demo covars + quadratic
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel i.countycode##c.timel2) cluster(countycode)
	specchart  syringeineff,spec(covars4 quad sample`j' noshort cfe yfe drop) 

	*only policy demo + quadratic
	reghdfe HIV_ratenl syringeineff `policyvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year i.countycode##c.timel i.countycode##c.timel2) cluster(countycode)
	specchart  syringeineff,spec(covars5 quad sample`j' noshort cfe yfe drop) 

	
	* all covars + quadratric
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' if sample_`j'==1 & sample_drop==0, absorb(countycode 		year i.countycode##c.timel i.countycode##c.timel2) cluster(countycode)
	specchart  syringeineff,spec(covars6 quad sample`j' noshort cfe yfe drop) 
	
	* all covars + quadratric + stateyear
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyearl if sample_`j'==1 & sample_drop==0, absorb(countycode 		year i.countycode##c.timel i.countycode##c.timel2) cluster(countycode)
	specchart  syringeineff,spec(covars4 quad sample`j' noshort cfe yfe sbyyfe drop) 
	
	****NO TRENDS

	* no covars + notrend
	reghdfe HIV_ratenl syringeineff if sample_`j'==1 & sample_drop==0, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars0 notrend sample`j' noshort cfe yfe  drop) 
	
	* econ covars + notrend
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate if sample_`j'==1 & sample_drop==0, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars2 notrend sample`j' noshort cfe yfe drop) 

	*just demo + notrend
	reghdfe HIV_ratenl syringeineff `xvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars3 notrend sample`j' noshort cfe yfe drop) 

	* econ + demo covars + notrend
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars4 notrend sample`j' noshort cfe yfe drop) 

	*only policy demo + notrend
	reghdfe HIV_ratenl syringeineff `policyvars' if sample_`j'==1 & sample_drop==0, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars5 notrend sample`j' noshort cfe yfe drop) 

	
	* all covars + notrend
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' if sample_`j'==1 & sample_drop==0, absorb(countycode 		year) cluster(countycode)
	specchart  syringeineff,spec(covars6 notrend sample`j' noshort cfe yfe drop) 
	
	* all covars + notrend + stateyear
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyearl if sample_`j'==1 & sample_drop==0, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars4 notrend sample`j' noshort cfe yfe sbyyfe drop) 
	
		
	
	
	*****************
	**SHORTER PANEL**
	*****************
 replace time=year-2008
 replace time2=time^2
 
	* main spec for other comparison groups
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1 & sample_drop==0 & year > 2007, absorb(countycode year) cluster(countycode)
  specchart syringeineff,spec(covars4 sample`j' linear short cfe yfe sbyyfe drop) 
  
	
	* no covars 
	reghdfe HIV_ratenl syringeineff if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars0 linear sample`j' short cfe yfe drop) 

	* econ covars 
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars2 linear sample`j' short cfe yfe drop) 

	*just demo
	reghdfe HIV_ratenl syringeineff `xvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars3 linear sample`j' short cfe yfe drop) 

	* econ + demo covars 
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars4 linear sample`j' short cfe yfe drop) 

	*only policy demo
	reghdfe HIV_ratenl syringeineff `policyvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars5 linear sample`j' short cfe yfe drop) 


	* all covars 
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars6 linear sample`j' short cfe yfe drop) 
	
	
	***************************
	**NOT DROPPING THE ZEROES**
	***************************

 
	* main spec for other comparison groups
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1 & year > 2007, absorb(countycode year) cluster(countycode)
  specchart syringeineff,spec(covars4 sample`j' linear short cfe yfe sbyyfe nodrop) 
  
	
	* no covars 
	reghdfe HIV_ratenl syringeineff if sample_`j'==1  & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars0 linear sample`j' short cfe yfe nodrop) 

	* econ covars 
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate if sample_`j'==1  & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars2 linear sample`j' short cfe yfe nodrop) 

	*just demo
	reghdfe HIV_ratenl syringeineff `xvars' if sample_`j'==1  & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars3 linear sample`j' short cfe yfe nodrop) 

	* econ + demo covars 
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' if sample_`j'==1  & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars4 linear sample`j' short cfe yfe nodrop) 

	*only policy demo
	reghdfe HIV_ratenl syringeineff `policyvars' if sample_`j'==1  & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars5 linear sample`j' short cfe yfe nodrop) 


	* all covars 
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' if sample_`j'==1  & year > 2007, absorb(countycode year i.countycode##c.time) cluster(countycode)
	specchart  syringeineff,spec(covars6 linear sample`j' short cfe yfe nodrop) 

	*************
	**Quadratic**
	*************
	
	* no covars + quadratric
	reghdfe HIV_ratenl syringeineff if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time i.countycode##c.time2) cluster(countycode)
	specchart  syringeineff,spec(covars0 quad sample`j' short cfe yfe drop) 
	
	* econ covars + quadratic
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time i.countycode##c.time2) cluster(countycode)
	specchart  syringeineff,spec(covars2 quad sample`j' short cfe yfe drop) 

	*just demo + quadratic
	reghdfe HIV_ratenl syringeineff `xvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time i.countycode##c.time2) cluster(countycode)
	specchart  syringeineff,spec(covars3 quad sample`j' short cfe yfe drop) 

	* econ + demo covars + quadratic
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time i.countycode##c.time2) cluster(countycode)
	specchart  syringeineff,spec(covars4 quad sample`j' short cfe yfe drop) 

	*only policy demo + quadratic
	reghdfe HIV_ratenl syringeineff `policyvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year i.countycode##c.time i.countycode##c.time2) cluster(countycode)
	specchart  syringeineff,spec(covars5 quad sample`j' short cfe yfe drop) 

	
	* all covars + quadratric
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode 		year i.countycode##c.time i.countycode##c.time2) cluster(countycode)
	specchart  syringeineff,spec(covars6 quad sample`j' short cfe yfe drop) 
	
	* all covars + quadratric + stateyear
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1  & sample_drop==0 & year>2007, absorb(countycode 		year i.countycode##c.time i.countycode##c.time2) cluster(countycode)
	specchart  syringeineff,spec(covars4 quad sample`j' short cfe yfe sbyyfe drop) 
	
****NO TRENDS

	* no covars + notrend
	reghdfe HIV_ratenl syringeineff if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars0 notrend sample`j' short cfe yfe nodrop) 
	
	* econ covars + notrend
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars2 notrend sample`j' short cfe yfe nodrop) 

	*just demo + notrend
	reghdfe HIV_ratenl syringeineff `xvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars3 notrend sample`j' short cfe yfe nodrop) 

	* econ + demo covars + notrend
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars4 notrend sample`j' short cfe yfe nodrop) 

	*only policy demo + notrend
	reghdfe HIV_ratenl syringeineff `policyvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode year) cluster(countycode)
	specchart  syringeineff,spec(covars5 notrend sample`j' short cfe yfe nodrop) 

	
	* all covars + notrend
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode 		year) cluster(countycode)
	specchart  syringeineff,spec(covars6 notrend sample`j' short cfe yfe nodrop) 
	
	* all covars + notrend + stateyear
	reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1  & sample_drop==0 & year > 2007, absorb(countycode 	year) cluster(countycode)
	specchart  syringeineff,spec(covars4 notrend sample`j' short cfe yfe sbyyfe nodrop) 
	
		}	
}

* create chart
use "estimates_HIV.dta",clear
* drop duplicates 
* drop duplicates 
duplicates drop linear covars* quad sample* short noshort cfe yfe sbyyfe notrend drop nodrop, force
drop if quad==1 | sampleclinics==1

/* sort specification by category */
gsort -short -noshort -covars0 -covars2 -covars3 -covars4 -covars5 -covars6 -cfe -yfe -sbyyfe -notrend -linear -samplenevertreat -sampleall -sampleborder -nodrop -drop /// 
	, mfirst
/* sort estimates by coefificent size, uncomment to activate sort by category */
sort beta
* rank
gen rank=_n
* gen indicators and scatters
	local scoff=" "
	local scon=" "
	local ind=-3.9
	foreach var in short noshort  {
	   cap gen i_`var'=`ind'
	   local ind=`ind'-0.8
	   local scoff="`scoff' (scatter i_`var' rank,msize(vsmall) msymbol(D) mcolor(gs10))" 
	   local scon="`scon' (scatter i_`var' rank if `var'==1,msize(vsmall) msymbol(D) mcolor(black))" 
	}
	
	local ind=`ind'-1.0
	foreach var in covars0 covars2 covars3 covars4 covars5 covars6 {
	cap gen i_`var'=`ind'
		local ind=`ind'-0.8
		local scoff="`scoff' (scatter i_`var' rank,msize(vsmall) msymbol(D) mcolor(gs10))" 
	   local scon="`scon' (scatter i_`var' rank if `var'==1,msize(vsmall) msymbol(D) mcolor(black))" 
	}

		local ind=`ind'-1.0
	foreach var in cfe yfe sbyyfe notrend linear  { 
	cap gen i_`var'=`ind'
		local ind=`ind'-0.8
		local scoff="`scoff' (scatter i_`var' rank,msize(vsmall) msymbol(D) mcolor(gs10))" 
	   local scon="`scon' (scatter i_`var' rank if `var'==1,msize(vsmall) msymbol(D) mcolor(black))" 
	}
	
			local ind=`ind'-1.0
	foreach var in samplenevertreat sampleall sampleclinics sampleborder {
	cap gen i_`var'=`ind'
		local ind=`ind'-0.8
		local scoff="`scoff' (scatter i_`var' rank,msize(vsmall) msymbol(D) mcolor(gs10))" 
	   local scon="`scon' (scatter i_`var' rank if `var'==1,msize(vsmall) msymbol(D) mcolor(black))" 
	}
	
		local ind=`ind'-1.0
	foreach var in drop nodrop {
	cap gen i_`var'=`ind'
		local ind=`ind'-0.8
		local scoff="`scoff' (scatter i_`var' rank,msize(vsmall) msymbol(D) mcolor(gs10))" 
	   local scon="`scon' (scatter i_`var' rank if `var'==1,msize(vsmall) msymbol(D) mcolor(black))" 
	}
	
	
* plot
tw  (scatter beta rank if main==1, mcolor(blue) msymbol(O) mfcolor(white) msize(small)) ///  main spec 
   (rbar u95 l95 rank, fcolor(gs12) lcolor(gs12) lwidth(none)) /// 95% CI
   (rbar u90 l90 rank, fcolor(gs6) lcolor(gs16) lwidth(none)) /// 90% CI
   (scatter beta rank, mcolor(black) msymbol(D) msize(small)) ///  point estimates
   `scoff' `scon' /// indicators for spec
  (scatter beta rank if main==1, mcolor(blue) msymbol(O) mfcolor(white)  msize(small)) ///  main spec 
  (scatter i_linear rank if main==1,msize(vsmall) msymbol(O) mfcolor(white) mcolor(blue))  ///
    (scatter i_samplenevertreat rank if main==1,msize(vsmall) mfcolor(white) msymbol(O) mcolor(blue))  ///
	(scatter i_covars6 rank if main==1,msize(vsmall) msymbol(O) mfcolor(white) mcolor(blue))  ///
	 (scatter i_short rank if main==1,msize(vsmall) msymbol(O) mfcolor(white) mcolor(blue))  ///
	 (scatter i_cfe rank if main==1,msize(vsmall) msymbol(O) mfcolor(white) mcolor(blue))  ///
	(scatter i_yfe rank if main==1,msize(vsmall)  msymbol(O) mfcolor(white) mcolor(blue))  ///
	(scatter i_sbyyfe rank if main==1,msize(vsmall) msymbol(O) mfcolor(white) mcolor(blue))  ///
	(scatter i_drop rank if main==1,msize(vsmall) msymbol(O) mfcolor(white) mcolor(blue))  ///
   ,legend (order(1 "Main spec. (Beta = - 0.96)" 4 "Point estimate" 2 "95% CI" 3 "90% CI") region(lcolor(white)) ///
	pos(12) ring(1) rows(1) size(vsmall) symysize(small) symxsize(small)) ///
   xtitle(" ") ytitle(" ") ///
   yscale(noline) xscale(noline) ylab(-2(2)6,noticks nogrid angle(horizontal)) xlab("", noticks)  ///
   graphregion (fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) 
 *  four marks between individual listings, 5 between new headings
* now add stuff to the y axis  
*gr_edit .yaxis1.add_ticks -1. `"Specification             "', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -3.1 `"{bf:Panel Length}          "', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -3.9 `"2008-2016"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -4.7 `"2003-2016"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -5.7 `"{bf:Covariates}             "', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -6.5 `"None"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -7.3 `"Econ. Only"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -8.1 `"Demo. Only"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -8.9 `"Econ. + Demo."', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -9.7 `"Policy Only"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -10.5 `"All"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -11.5 `"{bf:FEs and Trends}      "', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -12.3 `"County FEs"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -13.1 `"Year FEs"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -13.9 `"State-by-Year FEs"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -14.7 `"No Time Trends"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -15.5 `"Linear Time Trends"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -16.5 `"{bf:Alt. Control Groups}  "', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -17.3 `"Never Treated"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -18.1 `"All Counties"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -18.9 `"Counties W/ SEPs"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -19.7 `"Border Counties"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -20.7 `"{bf:Sample Restriction}"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -21.5 `"Drop Zero Death Counties"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )
gr_edit .yaxis1.add_ticks -22.3 `"Keep All Counties"', custom tickset(major) editstyle(tickstyle(textstyle(size(vsmall))) )


gr_edit .yaxis1.add_ticks 9 `"Coefficient"', custom tickset(major) editstyle(tickstyle(textstyle(size(small))) )
graph export "./Figures/robustness_HIV_noquad.png", replace
