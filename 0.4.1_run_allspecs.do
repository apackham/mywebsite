
if c(username)=="packham1" {
*cd "C:\Users\packham1\Dropbox\Opioid Project"
cd "/Users/packham1/Dropbox/Opioid Project"
}

clear all
set maxvar 15000

*
use ./Data/county_mort0016_withcontrols.dta, clear
set more off
set matsize 11000


local begyear=2008

*states I don't have any HIV data for: AK, AR, DC, DE, GA, ME, NE, NJ, NM, ND, OK, RI, SD, WV, WY
g sample_censored=0
replace sample_censored=1 if statecode==2 | statecode==5 | statecode==10 | statecode==11 | statecode==13 | statecode==23 | statecode==31 | statecode==34 | statecode==35 | statecode==38 
replace sample_censored=1 if statecode==40 | statecode==44 | statecode==46 | statecode==56 | statecode==54
g sample_noncensored=1-sample_censored

bys countycode: egen maxHIVcases=max(HIVcases)
*suppressing missing counties: can omit, can recode, robustness included in footnote
replace HIVcases=0 if HIVcases==. & year>2007

drop if countycode=="01045" 
replace clinics=0 if countycode=="48485" /*tx clinic, only gives naloxone*/
*drop if countycode=="18143" /*scott county, IN; run with and without for robustness*/

*drop state with missing HIV data for all years
replace HIVcases=. if statecode==46


*generate rates with percent interpretation
g opioid_rate=ln(((opioid_death)/pop)*100000)
g HIV_rate=ln(((HIVcases)/CDCpop)*100000)

g opioid_ratenl=((opioid_death)/pop)*100000
g drugod_ratenl=((drugod)/pop)*100000

g HIV_ratenl=((HIVcases)/pop)*100000
g HIV_prevratenl=((HIVprev_cases/pop))*100000
g AIDS_ratenl=((new_HIV_cases)/pop)*100000
g HIV_rateCDCnl=((HIVcases/CDCpop))*1000

g chlaratenl =((chlacases/CDCpop))*100000
g gonratenl =((goncases/pop))*100000

		g opioidod_3039=opioidod_3034+opioidod_3539
		g opioidod_2029=opioidod_2024+opioidod_2529
		g opioidod_4049=opioidod_4044+opioidod_4549
		g opioidod_5059=opioidod_5054+opioidod_5559
		g drugod_2029=drugod_2024+drugod_2529
		g drugod_3039=drugod_3034+drugod_3539
		g drugod_4049=drugod_4044+drugod_4549
		g drugod_5059=drugod_5054+drugod_5559
		
foreach i in opioid drug{
foreach j in 2029 3039 4049 5059 1014 1519 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 over69 male hsless white black {
g `i'od_`j'_ratenl=((`i'od_`j')/pop)*100000
g `i'od_`j'_rate=((`i'od_`j'+1)/pop)*100000
}
}
 
g drugabuse_ratenl=((drugabuse)/pop)*100000
g drugsale_opioid_ratenl=((drugsale_opioid)/pop)*100000
g drugposs_opiumnl=((drugposs_opium)/pop)*100000
g stealingnl=((stealing)/pop)*100000

foreach x in anyop opioid drug pois tot tot_veh tot_alc meth heroin {
replace `x'=0 if `x'==.
}

*TEDS outocomes
bys countycode: egen minadm=min(TOT_ADM)
replace TOT_ADM=0 if minadm<. & minadm>0 & TOT_ADM==.
replace HEROIN=0 if minadm<. & minadm>0 & HEROIN==.
replace ANYOP=0 if minadm<. & minadm>0 & ANYOP==.
replace CJPK=0 if minadm<. & minadm>0 & CJPK==.

g adm_ratenl=(TOT_ADM/pop)*100000
g pkadm_ratenl=(PK/pop)*100000
g heroinadm_ratenl=(HEROIN/pop)*100000
g anyopadm_ratenl=(ANYOP/pop)*100000
g noncrimadm_ratenl=(CJPK/pop)*100000

*ruhm adjusted outcomes
g anyop_ratenl=((anyop/pop)*100000)
g ruhmopioid_ratenl=((opioid/pop)*100000)
g tot_ratenl=((tot/pop)*100000)
g veh_ratenl=((tot_veh/pop)*100000)
g alc_ratenl=((tot_alc/pop)*100000)
g drug_ratenl=((drug/pop)*100000)
g meth_ratenl =((meth/pop)*100000)
g heroin_ratenl = ((heroin/pop)*100000)
g heronly_ratenl= ((heronly/pop)*100000)
g op_ratenl=(((heronly+opioid)/pop)*100000)
g synop_ratenl=(synop/pop)*100000 /*most likely measure of fentanyl*/
g cocaine_ratenl=(cocaine/pop)*100000
g illicit_ratenl=((synop+heronly)/pop)*100000
g oponly_ratenl=((oponly/pop)*100000)

g aanyop_ratenl=((aanyop/pop)*100000)
g aillicit_ratenl=((aillicit/pop)*100000)
g anatop_ratenl=((anatop/pop)*100000)
g aheroin_ratenl=((aheroin/pop)*100000)
g asynop_ratenl=((asynop/pop)*100000)

g anyop_rate=ln(anyop_ratenl)
g heroin_rate=ln(heroin_ratenl)
g heronly_rate=ln(heronly_ratenl)
g op_rate=ln(op_ratenl)
g drug_rate=ln(drug_ratenl)
g meth_rate=ln(meth_ratenl)
g synop_rate=ln(synop_ratenl) /*most likely measure of fentanyl*/
g cocaine_rate=ln(cocaine_ratenl)
g illicit_rate=ln(illicit_ratenl)
g aanyop_rate=ln(aanyop_ratenl)

g hospin_ratenl= (hospin/pop)*100000
g hospout_ratenl= (hospout/pop)*100000
g hospdoa_ratenl= (hospdoa/pop)*100000
g autopsy_ratenl=(autopsy/pop)*100000
g home_ratenl=(home/pop)*100000

g tot_nodrug=tot-drug
g tot_nodrug_ratenl= ((tot_nodrug/pop)*100000)
g lpop=ln(pop)



foreach i in aanyop aheroin asynop aillicit {
foreach j in 20 30 40 50 female white black hispanic male {
g `i'_`j'_ratenl=((`i'_`j')/pop)*100000
}
}

*generate rates with a smaller population, ages more likely to use injection drugs
g opioid_ratesp=ln(((opioid_death+1)/pop)*100000)
g HIV_ratesp=ln(((HIVcases+1)/CDCpop)*100000)


ihstrans HIVcases aanyop drug aillicit aheroin aanyop_ratenl HIV_ratenl, prefix(ihs_)

g treat=0
replace treat=1 if effyear09<. | effyear10<. | effyear11<. | effyear12<. | effyear13<. | effyear14<. | effyear15<. |  effyear16<. | effyear17<.


*create variable with (earliest) treatment year
g effyear=.
foreach i in 17 16 15 14 13 12 11 10 09 08 {
replace effyear=20`i' if effyear`i'==20`i'
}

foreach i in 09 10 11 12 13 14 15 16 17 {
replace effyear`i'=effyear`i'/20`i'
}


*define post*treat variable
g syringeineff=0
replace syringeineff=1 if treat==1 & year>=effyear

replace treat=0 if effyear==2008
replace syringeineff=0 if effyear==2008
replace clinics=0 if effyear==2008

replace syringeineff=0 if effyear==2017 
replace treat=0 if effyear==2017 
replace clinics=0 if effyear==2017

replace clinics=0 if statecode==48
replace treat=0 if statecode==48 /*one clinic, only has Naloxone*/

destring urbanicity, replace
bys countycode: egen urbancode=min(urbanicity)
bys countycode: egen maxurban=max(urbancode)
drop if maxurban==.

*note: drops some small alaskan counties
**note: shannon county, SD merges with another county 2015
g urban=0
replace urban=1 if urbancode==1 | urbancode==2 | urbancode==3  //keeping cities

g sample_all=1

g sample_urban=0
replace sample_urban=1 if urban==1

g sample_reduced=1
replace sample_reduced=0 if yearopened==.

g sample_clinics=0
replace sample_clinics=1 if clinics>0 | treat==1
replace sample_clinics=1 if clinics_adj>0 | treat==1 /*alternative treatment measure, using diff. data scraping (not used for main analysis)*/

g sample_rural=0
replace sample_rural=1 if urban==0

g sample_ohio=0
replace sample_ohio=1 if statecode==39

g sample_midwest=0
replace sample_midwest=1 if statecode==18 | statecode==17 | statecode ==26 | statecode==39 | statecode==55 | statecode ==19 | statecode==20 | statecode==27 | statecode ==29 | statecode==31 | statecode==38 | statecode ==46 

g sample_south=0
replace sample_south=1 if statecode==10 | statecode==11 | statecode ==12 | statecode==13 | statecode==24 | statecode ==37 | statecode==45 | statecode==51 | statecode ==54 | statecode==1 | statecode==21 | statecode ==28 | statecode==47 | statecode==5 | statecode==22 | statecode==40 | statecode==48 

g sample_northeast=0
replace sample_northeast=1 if statecode==9 | statecode==23 | statecode ==25 | statecode==33 | statecode==44 | statecode ==50 | statecode==34 | statecode==36 | statecode ==42 

g sample_west=0
replace sample_west=1 if statecode==4 | statecode==8 | statecode ==16 | statecode==35 | statecode==30 | statecode ==49 | statecode==32 | statecode==56 | statecode ==2 | statecode==6 | statecode==15 | statecode ==41 | statecode==53

g sample_restrict=1
replace sample_restrict=0 if state_abbrev=="AR" | state_abbrev=="NE" | state_abbrev=="TX" | state_abbrev=="SC" | state_abbrev=="FL" | state_abbrev=="MO" | state_abbrev=="SD" | state_abbrev=="ID" | state_abbrev=="OK" | state_abbrev=="FL" | state_abbrev=="IA" | state_abbrev=="GA" | state_abbrev=="WY" | state_abbrev=="AL" | state_abbrev=="KS" | state_abbrev=="MS"

g sample_nevertreat=0
replace sample_nevertreat=1 if treat==1 | clinics==0

egen avgpop=mean(pop), by(countycode)
sum avgpop if sample_nevertreat==1 & year==2016, detail
gen sample_hipop=avgpop>r(p50)
gen sample_lopop=avgpop<=r(p50)

bys statecode: egen stclinics=sum(clinics)
g sample_stclinics=0
replace sample_stclinics=1 if stclinics>0

g sample_border=0
replace sample_border=1 if treat==1 

forvalues j=1(1)15 {
levelsof adj_countycode`j', local(adjacent`j') clean

foreach x in `adjacent`j'' {
replace sample_border=1 if countycode=="`x'"
}
}


** generate region-by-year fixed effects
forvalues j=2008/2016{
	gen year`j'=(year==`j')
	gen r`j'1=year`j'*sample_northeast
	gen r`j'2=year`j'*sample_midwest
	gen r`j'3=year`j'*sample_south
	gen r`j'4=year`j'*sample_west
	drop year`j'
}

*generating state-by-year fixed effects
sort statecode
egen stategroup=group(statecode)
forvalues j=2008/2016{
forvalues i=1/51{
	gen year`j'=(year==`j')
	gen stategroup`i'=(stategroup==`i')
	gen s`j'`i'=year`j'*stategroup`i'
	drop year`j'
	drop stategroup`i'
} 
}



g lag=0
replace lag=1 if year==effyear 
g lag2=0
replace lag2=1 if year==effyear+1
g lag3=0
replace lag3=1 if year==effyear+2
g lag4=0
replace lag4=1 if year>=effyear+3 & effyear<.

g lead=0
replace lead=1 if year==effyear-1
g lead2=0
replace lead2=1 if year==effyear-2
g lead3=0
replace lead3=1 if year==effyear-3
g lead4=0
replace lead4=1 if year<=effyear-4 & effyear<.

*for HCUP data
bys statecode: egen minopenstate=min(effyear)
g sttreat=0
replace sttreat=1 if minopenstate<.
g stlag=0
replace stlag=1 if year==minopenstate
g stlag2=0
replace stlag2=1 if year==minopenstate+1
g stlag3=0
replace stlag3=1 if year==minopenstate+2
g stlag4=0
replace stlag4=1 if year>=minopenstate+3
g stlead=0
replace stlead=1 if year==minopenstate-1
g stlead2=0
replace stlead2=1 if year==minopenstate-2
g stlead3=0
replace stlead3=1 if year==minopenstate-3
g stlead4=0
replace stlead4=1 if year<=minopenstate-4 & minopenstate<.
g stsyringeineff=0
	replace stsyringeineff=1 if year>=minopenstate & sttreat==1

***generate variable for states that outlaw drug paraphernalia with no SEP related exceptions
g paraph=0 
replace paraph=1 if paraphernalia==1 & poparaexyn_Yesneedlessyringe==0 & poparaexyn_Yesinjectionorinj==0 & syringeexclusion==0 & poexcptlst_Forparticipantsins==0 & poparaseppartyn_Possessionthrou==0

rename tamperresistantprescriptionform tamper
rename prescriptiondrugmonitoringprogra pdmp
rename pharmacistverification pharma


destring countycode, replace
xtset countycode year

** generate county population weight
bys countycode: egen popwt=mean(pop) 

g Hpct=Hpop/pop
g BLpct=BLpop/pop


bys countycode: egen sat_flag=max(sat_tot)
replace sat_flag=1 if sat_flag>0 & sat_flag<.

gen time=year-`begyear'



		g time2=time^2

	/*bys countycode: egen max_op=max(aanyop)
	bys countycode: egen max_drug=max(drug)
		
	bys countycode: egen mindrug=min(drug)

	drop if mindrug==0
	drop if year<2003
	save "./Data/torun_long.dta", replace /*I'm doing this for my robustness short/long table only*/
	drop max_op max_dru mindrug
	*/

	
	*keep if year>2007	/*for robustness short/long table only*/
	g year_flag=0 
	replace year_flag=1 if year>2007
	bys countycode year_flag: egen max_op=max(aanyop)
	bys countycode year_flag: egen max_drug=max(drug)
		
	bys countycode year_flag: egen mindrug=min(drug)

	g sample_drop=0
	replace sample_drop=1 if mindrug==0 & year_flag==1
	
	keep if year>2002
		save "./Data/torun.dta", replace


use "./Data/torun.dta", clear
	drop if sample_drop==1
	keep if year>2007
	
local xvars Hpct BLpct
local policyvars_resid prescriptionlimit_resid tamper_resid idrequirement_resid doctorshopping_resid physicianexam_resid pharma_resid pdmp_resid painclinic_resid paraph_resid samaritan_resid

local policyvars prescriptionlimit tamper idrequirement doctorshopping physicianexam pharma pdmp painclinic paraph samaritan
global region r20081-r20164  //region-year fixed effects
global stateyear s20081-s201651 //state by year fixed effects

**creating tables and graphs
loc REG_estout "substitute(_ \_ { $\left\{\text{ } }\right\}$) la collabels(none) eqlabels(none) posthead("") mlabels(none) prefoot("") postfoot("") varwidth(16) modelwidth(12) style(tex) starl(* .10 ** 0.05 *** 0.01)"

local policyvars prescriptionlimit tamper idrequirement doctorshopping physicianexam pharma pdmp painclinic paraph samaritan


***************
***Sum Stats***
***************
preserve
svyset [pweight=BLpct]
svy: mean prescriptionlimit tamper idrequirement doctorshopping physicianexam pharma pdmp
estat sd
estadd mat sd= r(sd)
label var prescriptionlimit "Child is First Born"
label var tamper "Mother Married"
label var idrequirement "Mother Education: < High School"
label var doctorshopping "Mother Education: Some College"
label var physicianexam "Mother Insured by Medicaid"
label var pharma "Mother Uninsured"
label var pdmp "Mother's Age"
esttab using "./tables/sumstats_esttest.tex", `REG_estout' cell("b (fmt(3)) sd (fmt(2))") replace nolines prehead("") noobs nomtitle fragment

mean anyop_ratenl [pw=BLpct] if year==2010
scalar meanmean = round(e(b)[1,1], 0.001)
di meanmean
eststo l1: reg aanyop_ratenl syringeineff prescriptionlimit [pweight=BLpct], r
estadd scalar ymean=meanmean


mean anyop_rate [pw=BLpct]
scalar meanmean = round(e(b)[1,1], 0.001)
di meanmean
eststo l2: reg aanyop_ratenl syringeineff prescriptionlimit Hpct [pweight=BLpct], r
estadd scalar ymean=meanmean


	estout l1 l2 using "./tables/test.tex", c(b(star fmt(4)) se(par fmt(4))) stats(ymean, la("mean")) keep(syringeineff) replace

	
*******************************
*regressions*******************
*******************************

reghdfe aanyop_ratenl syringeineff, absorb(countycode year) cluster(countycode)
sum aanyop_ratenl
scalar meanaanyop_ratenl = round(r(mean),0.001)
di meanaanyop_ratenl
estadd scalar xmean= meanaanyop_ratenl
eststo l1

estout l1  using "./Tables/`x'_`j'_OLS_trends.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(syringe* lead) order(syringeineff lead) `REG_estout' replace  

	
	******************
	***MAIN RESULTS***
	******************
	preserve

foreach x in aanyop_ratenl drug_ratenl aheroin_ratenl aillicit_ratenl HIV_ratenl aanyop_rate HIV_rate ihs_HIVcases ihs_aanyop drugabuse_ratenl drugsale_opioid_ratenl drugposs_opiumnl stealingnl gonratenl chlaratenl hospin_ratenl hospout_ratenl hospdoa_ratenl home_ratenl {
foreach j in nevertreat  { 

eststo clear
drop if year<2008
reghdfe `x' syringeineff if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'

eststo l1
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'

eststo l2
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' `policyvars' if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l3
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l4
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear if sample_`j'==1, absorb(countycode year i.countycode##c.time) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l5
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear lead if sample_`j'==1, absorb(countycode year i.countycode##c.time) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l6
	
		label var syringeineff "Average Effect of SEP"
		label var lead "One-Year Lead"
		
estout l1 l2 l3 l4 l5 l6  using "./Tables/`x'_`j'_OLS_trends.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(syringe* lead) order(syringeineff lead) `REG_estout' replace   

estout l1 l2 l3 l4 l5 using "./Tables/`x'_`j'_OLS_trends_nolead.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(syringe*) order(syringeineff) `REG_estout' replace     

estout l1 l2 l3 l4 using "./Tables/`x'_`j'_OLS_noleadnotrends.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(syringe*) order(syringeineff) `REG_estout' replace     

		}
		}
		restore
	
	**********************
	**Medicaid expansion**
	**********************
	g sample_medicaid=0
	g sample_nomedicaid=0
	replace sample_medicaid=1 if statecode==2 | statecode==4 | statecode==5 | statecode==6 | statecode==9 | statecode==10 | statecode==15 | statecode==17 | statecode==19 | statecode==21
	replace sample_medicaid=1 if statecode==24 | statecode==25 | statecode==27 | statecode==32 | statecode==34 | statecode==35 | statecode==36 | statecode==38 | statecode==39 | statecode==41
	replace sample_medicaid=1 if statecode==44 | statecode==50 | statecode==53 | statecode==54 | statecode==55 | statecode==26 | statecode==42 | statecode==33 | statecode==42 | statecode==18 
	*AZ, AR, CA, CO, CT, DE, HI, IL, IA, KY, 
	*MD, MA, MN,NV, NJ, NM, NY, ND, OH, OR,
	*RI, VT, WA, WV, WI, MI, NH,PA, IN, and AK
	
	replace sample_nomedicaid=1 if sample_medicaid==0


foreach j in medicaid nomedicaid { 

eststo clear
drop if year<2008
reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear if sample_`j'==1 & sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum HIV_ratenl if sample_`j'==1 & sample_nevertreat==1
scalar meanHIV_ratenl = round(r(mean),0.001)
di meanHIV_ratenl
estadd scalar xmean= meanHIV_ratenl
eststo l1
reghdfe drug_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear if sample_`j'==1 & sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum drug_ratenl if sample_`j'==1 & sample_nevertreat==1
scalar meandrug_ratenl = round(r(mean),0.001)
di meandrug_ratenl
estadd scalar xmean= meandrug_ratenl
eststo l2
reghdfe aanyop_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear if sample_`j'==1 & sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum aanyop_ratenl if sample_`j'==1 & sample_nevertreat==1
scalar meanaanyop_ratenl = round(r(mean),0.001)
di meanaanyop_ratenl
estadd scalar xmean= meanaanyop_ratenl
eststo l3
preserve
collapse (mean) stsyringeineff stlag* stlead* `policyvars' unemploy_rate pct_poverty opioidED* opioidIP* sample_medicaid sample_nomedicaid (sum) pop CDCpop Hpop BLpop, by(statecode year)
g pctHpopst=Hpop/pop
g pctBLpopst=BLpop/pop
g syringeineff=stsyringeineff
reghdfe opioidED_rate syringeineff pct_poverty unemploy_rate pctHpopst pctBLpopst `policyvars' if sample_`j'==1, absorb(statecode year) cluster(statecode)
sum opioidED_rate if sample_`j'==1
scalar meanopioidED_rate = round(r(mean),0.001)
di meanopioidED_rate
estadd scalar xmean= meanopioidED_rate
eststo l4

reghdfe opioidIP_rate syringeineff pct_poverty unemploy_rate `policyvars' pctHpopst pctBLpopst if sample_`j'==1, absorb(statecode year) cluster(statecode)
sum opioidIP_rate if sample_`j'==1
scalar meanopioidIP_rate = round(r(mean),0.001)
di meanopioidIP_rate
estadd scalar xmean= meanopioidIP_rate
eststo l5
restore

reghdfe drugsale_opioid_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear if sample_`j'==1 & sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum drugsale_opioid_ratenl if sample_`j'==1
scalar meandrugsale_opioid_ratenl = round(r(mean),0.001)
di meandrugsale_opioid_ratenl
estadd scalar xmean= meandrugsale_opioid_ratenl
eststo l6

reghdfe drugposs_opiumnl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear if sample_`j'==1 & sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum drugposs_opiumnl if sample_`j'==1
scalar meandrugposs_opiumnl = round(r(mean),0.001)
di meandrugposs_opiumnl
estadd scalar xmean= meandrugposs_opiumnl
eststo l7




label var syringeineff "Average Effect of SEP"

		
estout l1 l2 l3 l4 l5  using "./Tables/`x'_`j'_OLS_notrends.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N , fmt(2 0) la("Mean" "Observations")) keep(syringe*) `REG_estout' replace     
		}
		

	**********************
	**Policy Vars**
	**********************
g naloxone=0
replace naloxone=1 if statecode==25 & year>=2012
replace naloxone=1 if (statecode==11 | statecode==34 | statecode==37 | statecode==40 | statecode==41 | statecode==50) & year>=2013
replace naloxone=1 if (statecode==6 | statecode==10 | statecode==13 | statecode==26 | statecode==27 | statecode==36 | statecode==39 | statecode==42 | statecode==44 | statecode==27 | statecode==49 | statecode==55) & year>=2014
replace naloxone=1 if (statecode==1 | statecode==5  | statecode==8 | statecode==9 | statecode==12 | statecode==16 | statecode==17 | statecode==18 | statecode==21 | statecode==23 | statecode==24 | statecode==28 | statecode==31 | statecode==32 | statecode==33 | statecode==38 | statecode==45 | statecode==48 | statecode==51 | statecode==53 | statecode==54) & year>=2015
replace naloxone=1 if statecode==22 & year>=2016
replace naloxone=1 if statecode==35 & year>=2001


preserve

foreach x in aanyop_ratenl aheroin_ratenl asynop_ratenl aillicit_ratenl HIV_ratenl {
foreach j in nevertreat  { 

eststo clear
drop if year<2008

reghdfe `x'  syringeineff pct_poverty unemploy_rate `xvars' if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l1

reghdfe `x'  syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l2

reghdfe `x'  syringeineff  pct_poverty unemploy_rate `xvars' prescriptionlimit tamper idrequirement if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l3

reghdfe `x'  syringeineff pct_poverty unemploy_rate `xvars' prescriptionlimit tamper idrequirement physicianexam pharma if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l4

reghdfe `x'  syringeineff pct_poverty unemploy_rate `xvars' prescriptionlimit tamper idrequirement doctorshopping physicianexam pharma if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l5

reghdfe `x'  syringeineff pct_poverty unemploy_rate `xvars' prescriptionlimit tamper idrequirement doctorshopping physicianexam pharma painclinic if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l6

reghdfe `x'  syringeineff pct_poverty unemploy_rate `xvars' prescriptionlimit tamper idrequirement doctorshopping physicianexam pharma pdmp painclinic if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l7

reghdfe `x'  syringeineff pct_poverty unemploy_rate `xvars' prescriptionlimit tamper idrequirement doctorshopping physicianexam pharma pdmp painclinic paraph samaritan if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l8

reghdfe `x'  syringeineff pct_poverty unemploy_rate `xvars' prescriptionlimit tamper idrequirement doctorshopping physicianexam pharma pdmp painclinic paraph samaritan naloxone if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l9

label var syringeineff "Average Effect of SEP"
		
estout l1 l2 l3 l4 l5 l6 l7 l8 l9 using "./Tables/`x'_policyvars_`j'_OLS_notrends.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N , fmt(2 0) la("Mean" "Observations")) keep(syringeineff) order(syringeineff) `REG_estout' replace     
		}
		}
		restore

	
		*
	******************
	***TEDS RESULTS***
	******************
	preserve
foreach j in nevertreat  { 

eststo clear
drop if year<2008

reghdfe adm_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum adm_ratenl if sample_`j'==1
scalar meanadm_ratenl = round(r(mean),0.001)
di meanadm_ratenl
estadd scalar xmean= meanadm_ratenl
eststo l1
reghdfe anyopadm_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum anyopadm_ratenl if sample_`j'==1
scalar meananyopadm_ratenl = round(r(mean),0.001)
di meananyopadm_ratenl
estadd scalar xmean= meananyopadm_ratenl
eststo l2
reghdfe heroinadm_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum heroinadm_ratenl if sample_`j'==1
scalar meanheroinadm_ratenl = round(r(mean),0.001)
di meanheroinadm_ratenl
estadd scalar xmean= meanheroinadm_ratenl
eststo l3
reghdfe pkadm_ratenl syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum pkadm_ratenl if sample_`j'==1
scalar meanpkadm_ratenl = round(r(mean),0.001)
di meanpkadm_ratenl
estadd scalar xmean= meanpkadm_ratenl
eststo l4
	
		label var syringeineff "Average Effect of SEP"
		
estout l1 l2 l3 l4  using "./Tables/TEDSoutcomes_`j'_OLS.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(syringe*) order(syringeineff lead) `REG_estout' replace   
		}
		
		restore


	******************
	***LEADS TABLE***
	******************
	preserve
foreach j in nevertreat { 
   
eststo clear
drop if year<2008
drop if mindrug==0
reghdfe HIV_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear lead if sample_`j'==1, absorb(countycode year i.countycode##c.time) cluster(countycode)
sum HIV_ratenl if sample_`j'==1
scalar meanHIV_ratenl = round(r(mean),0.001)
di meanHIV_ratenl
estadd scalar xmean= meanHIV_ratenl
eststo l1
reghdfe drug_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear lead if sample_`j'==1, absorb(countycode year i.countycode##c.time) cluster(countycode)
sum drug_ratenl if sample_`j'==1
scalar meandrug_ratenl = round(r(mean),0.001)
di meandrug_ratenl
estadd scalar xmean= meandrug_ratenl
eststo l2
reghdfe aanyop_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear lead if sample_`j'==1, absorb(countycode year i.countycode##c.time) cluster(countycode)
sum aanyop_ratenl if sample_`j'==1
scalar meanaanyop_ratenl = round(r(mean),0.001)
di meanaanyop_ratenl
estadd scalar xmean= meanaanyop_ratenl
eststo l3
reghdfe aillicit_ratenl syringeineff pct_poverty unemploy_rate `xvars' `policyvars' $stateyear lead if sample_`j'==1, absorb(countycode year i.countycode##c.time) cluster(countycode)
sum aillicit_ratenl if sample_`j'==1
scalar meanaillicit_ratenl = round(r(mean),0.001)
di meanaillicit_ratenl
estadd scalar xmean= meanaillicit_ratenl
eststo l4

	
		label var syringeineff "Average Effect of SEP"
	label var lead "One-Year Lead"
		
estout l1 l2 l3 l4  using "./Tables/mainleads_`j'_OLS_trends.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(syringeineff lead) `REG_estout' replace     
		}
		
		restore
	

		*******************
		**CONTROLS CHECK***
		*******************
preserve
foreach j in nevertreat { 
   
eststo clear
drop if year<2008
drop if mindrug==0
reghdfe pct_poverty syringeineff $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum pct_poverty if sample_`j'==1
scalar meanpct_poverty = round(r(mean),0.001)
di meanpct_poverty
estadd scalar xmean= meanpct_poverty
eststo l1
reghdfe unemploy_rate syringeineff $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum unemploy_rate if sample_`j'==1
scalar meanunemploy_rate = round(r(mean),0.001)
di meanunemploy_rate
estadd scalar xmean= meanunemploy_rate
eststo l2
reghdfe Hpct syringeineff $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum Hpct if sample_`j'==1
scalar meanHpct = round(r(mean),0.001)
di meanHpct
estadd scalar xmean= meanHpct
eststo l3
reghdfe BLpct syringeineff $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum BLpct if sample_`j'==1
scalar meanBLpct = round(r(mean),0.001)
di meanBLpct
estadd scalar xmean= meanBLpct
eststo l4
reghdfe pop syringeineff $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum pop if sample_`j'==1
scalar meanpop = round(r(mean),0.001)
di meanpop
estadd scalar xmean= meanpop
eststo l5
	
		label var syringeineff "Average Effect of SEP"

		
estout l1 l2 l3 l4 l5 using "./Tables/controls_`j'_OLS_withpop.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(syringeineff) `REG_estout' replace     
		}
		
		restore
		

		*********************************
			**HOSPITAL ADMISSIONS **
		*********************************
	foreach i in 09 10 11 12 13 14 15 16 {
	g effyearfl`i'=0
	replace effyearfl`i'=1 if effyear`i'<.
	}
	
		foreach x in opioidIP_rate opioidED_rate {
			preserve
eststo clear
drop if year<2008
g openings=effyearfl09+effyearfl10+effyearfl11+effyearfl12+effyearfl13+effyearfl14+effyearfl15+effyearfl16

collapse (mean) stsyringeineff stlag* stlead* `policyvars' unemploy_rate pct_poverty opioidED* opioidIP* (sum) pop CDCpop Hpop BLpop clinics effyearfl* openings, by(statecode year)
g pctHpopst=Hpop/pop
g pctBLpopst=BLpop/pop
g stsep_clinics=stsyringeineff*openings
g time=year-2008

reghdfe `x' stsep_clinics, absorb(statecode year) cluster(statecode)
sum `x' 
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l1
reghdfe `x' stsep_clinics pct_poverty unemploy_rate pctHpopst pctBLpopst, absorb(statecode year) cluster(statecode)
sum `x' 
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l2
reghdfe `x' stsep_clinics pct_poverty unemploy_rate pctHpopst pctBLpopst `policyvars', absorb(statecode year) cluster(statecode)
sum `x'
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l3

reghdfe `x' stsep_clinics pct_poverty unemploy_rate pctHpopst pctBLpopst `policyvars', absorb(statecode year i.statecode##c.time) cluster(statecode)
sum `x'
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l4

reghdfe `x' stsep_clinics pct_poverty unemploy_rate pctHpopst pctBLpopst `policyvars' stlead, absorb(statecode year) cluster(statecode)
sum `x'
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l5

		label var stsep_clinics "Average Effect of SEP* Number of SEPs"
		
		label var stlead "One-Year Lead"

		
estout l1 l2 l3 l5 using "./Tables/`x'_OLS_lead_intensity.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(stsep_clinics stlead) order(stsep_clinics stlead) `REG_estout' replace     
		restore
		}		
		
		
		
foreach x in opioidIP_rate opioidED_rate {
	preserve
eststo clear
drop if year<2008
collapse (mean) stsyringeineff stlag* stlead* `policyvars' unemploy_rate pct_poverty opioidED* opioidIP* (sum) pop CDCpop Hpop BLpop, by(statecode year)
g pctHpopst=Hpop/pop
g pctBLpopst=BLpop/pop
g time=year-2008

reghdfe `x' stsyringeineff, absorb(statecode year) cluster(statecode)
sum `x' 
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l1
reghdfe `x' stsyringeineff pct_poverty unemploy_rate pctHpopst pctBLpopst, absorb(statecode year) cluster(statecode)
sum `x' 
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l2
reghdfe `x' stsyringeineff pct_poverty unemploy_rate pctHpopst pctBLpopst `policyvars', absorb(statecode year) cluster(statecode)
sum `x'
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l3
reghdfe `x' stsyringeineff pct_poverty unemploy_rate pctHpopst pctBLpopst `policyvars', absorb(statecode year i.statecode##c.time) cluster(statecode)
sum `x'
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l4
reghdfe `x' stsyringeineff pct_poverty unemploy_rate pctHpopst pctBLpopst `policyvars' stlead, absorb(statecode year) cluster(statecode)
sum `x'
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l5

		label var stsyringeineff "Average Effect of SEP"
		
		label var stlead "One-Year Lead"

		
estout l1 l2 l3 l5 using "./Tables/`x'_OLS_lead.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(stsyr* stlead) order(stsyringeineff stlead) `REG_estout' replace     
		restore
		}
		

	**************************
	**Testing comparison groups**
	**************************
		preserve
foreach x in aanyop_ratenl  HIV_ratenl   {
eststo clear
drop if year<2008
reghdfe `x' syringeineff pct_poverty unemploy_rate `policyvars' `xvars' $stateyear if sample_nevertreat==1 , absorb(countycode year) cluster(countycode)
sum `x' if sample_nevertreat==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l1

reghdfe `x' syringeineff pct_poverty unemploy_rate `policyvars' `xvars' $stateyear if sample_all==1 , absorb(countycode year) cluster(countycode)
sum `x' if sample_all==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l2

reghdfe `x' syringeineff pct_poverty unemploy_rate `policyvars' `xvars' $stateyear if sample_clinics==1 , absorb(countycode year) cluster(countycode)
sum `x' if sample_clinics==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l3

reghdfe `x' syringeineff pct_poverty unemploy_rate `policyvars' `xvars' $stateyear if sample_stclinics==1 , absorb(countycode year) cluster(countycode)
sum `x' if sample_stclinics==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l4

reghdfe `x' syringeineff pct_poverty unemploy_rate `policyvars' `xvars' $stateyear if sample_border==0 | treat==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_border==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l5

reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' $stateyear if (sample_censored==0 & sample_nevertreat==1), absorb(countycode year) cluster(countycode)
sum `x' if (sample_censored==0 & sample_nevertreat==1) 
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l6



		label var syringeineff "Average Effect of SEP"
	
		
estout l1 l2 l3 l4 l5 l6 using "./Tables/`x'_comparisongroups_OLS_notrends_nonborder.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N , fmt(2 0) la("Mean" "Observations")) keep(syring*) order(syringeineff) `REG_estout' replace     
		}
		
		restore

		
	***************
	***subgroups***
	***************
egen avgpov=mean(pct_poverty), by(countycode)
sum avgpov if treat==1 & year==2016, detail
gen sample_hipov=avgpov>r(p50)
gen sample_lopov=avgpov<=r(p50)


preserve
foreach x in aanyop_ratenl HIV_ratenl  {
eststo clear
drop if year<2008
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_nevertreat==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l1
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_clinics==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_clinics==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l2
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars'  $stateyear if sample_urban==1 & sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_urban==1 & sample_nevertreat==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l3
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars'  $stateyear if sample_rural==1 & sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_rural==1 & sample_nevertreat==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l4
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars'  $stateyear if sample_lopov==1 & sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_lopov==1 & sample_nevertreat==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l5
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars'  $stateyear if sample_hipov==1 & sample_nevertreat==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_hipov==1 & sample_nevertreat==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l6
	
	label var syringeineff "Average Effect of SEP"
		
estout l1  l3 l4 l5 l6 using "./Tables/`x'_subgroups_OLS_notrends.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N , fmt(2 0) la("Mean" "Observations")) keep(syringeineff*) `REG_estout' replace     
		}
		
		restore
	


******************************
**Main results by treat year**
******************************
	preserve
		replace treat=0 if effyear==. & treat==1
		replace treat=0 if effyear==2017

foreach x in aanyop_ratenl HIV_ratenl asynop_ratenl aheroin_ratenl { 
foreach j in nevertreat  { 


eststo clear
drop if year<2008
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1, absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'
eststo l1

reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1 & (treat==0 |  (treat==1 & effyear<2013)), absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1 & (treat==0 | effyear<2013)==1 & year<2013
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'

eststo l2
reghdfe `x' syringeineff pct_poverty unemploy_rate `xvars' $stateyear if sample_`j'==1 & ((treat==0 ) | (treat==1 & effyear>2012 & effyear<2016)), absorb(countycode year) cluster(countycode)
sum `x' if sample_`j'==1 & (treat==0 | (effyear>2012 & effyear<2017))==1 & year>=2013
scalar mean`x' = round(r(mean),0.001)
di mean`x'
estadd scalar xmean= mean`x'

eststo l3

	
		label var syringeineff "Average Effect of SEP"

	
		
estout l1 l2 l3  using "./Tables/`x'_`j'_OLS_nolead_bytreatgroup_notrends.tex", cells(b(star fmt(3)) se(par fmt(3))) stats(xmean N, fmt(2 0) la("Mean" "Observations")) keep(syringeineff*)  `REG_estout' replace     
		}
		}
		
		restore	

		


