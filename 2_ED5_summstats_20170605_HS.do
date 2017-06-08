*******************************************
*Author: Hummy Song
*Date created: January 18, 2017
*Date modified: June 5, 2017 
*Description: Create new variables and generate summary statistics for ED5
*******************************************
/*TABLE OF CONTENTS
1) Setup
2) ED5 - NYC - 3 years post pod - 3 pods
	A) Summary statistics at patient level
	B) Summary statistics at clinician level
3) Find correlations between continuous variables
4) Tenure Heatmaps for att, res, and nurse
*/

version 14
clear all 
macro drop _all
set more off
capture log close

local today 20170605
*local location "C:\Users\Melissa Valentine\Desktop\Work\Current Research\W-FiveWayPods\ED-TeamFamiliarity"
*local location "C:\Users\songheek\Dropbox\1_projects\1_team\data"
*local location "/Users/hummy/Dropbox (Penn)/Documents/Hummy SCHOOL/Papers and Research/TFPD"
local location "/Users/HS/Dropbox (Penn)/Documents/Hummy SCHOOL/Papers and Research/w Yihao/TFPD"

**********************************************************
*1) Setup
**********************************************************
cd "`location'"
local log "`location'/STATA output/Log files"
local data "`location'/Data/Processed Data"
local merge_data "`location'/Data/Processed Data/For merges"
local raw_data "`location'/Data/Raw Data"
local graph "`location'/STATA output/Graphs"
local table "`location'/STATA output/Tables"
log using "`log'/2_ED5_summstats_`today'.txt", text replace

**********************************************************
*2) ED5 - NYC - 3 years post pod - 3 pods
**********************************************************
use "`data'/ED5_cleaned_20170330.dta", clear

**********************************************************
*A) Summary statistics at patient level
**********************************************************
sort facility_id case_id

*0) Generate missing stats and implement exclusion criteria
count if eddispo==5 
*This counts LWBS
count if missing(arrivaltime)
count if missing(att1)
count if missing(nurse1)
tabmiss arrivaltime att1 nurse1
drop if missing(arrivaltime)

*1) Number of observations by ESI level
unique case_id
tab acuity_n, missing

*2) Mean and sd for continuous variables (report % missing)
*Age
summ age
count if missing(age)
*No. of attendings associated with the case
egen att_ct=rownonmiss(att1)
label var att_ct "Count of attendings associated with the case"
summ att_ct
count if missing(att_ct)
*No. of residents associated with the case
egen res_ct=rownonmiss(res1)
label var res_ct "Count of residents associated with the case"
summ res_ct
count if missing(res_ct)
*No. of nurses associated with the case
egen nurse_ct=rownonmiss(nurse1-nurse2)
label var nurse_ct "Count of nurses associated with the case"
summ nurse_ct
count if missing(nurse_ct)

*Team familiarity (using only att1, res1, and nurse1 for now)
unique att1
unique res1 
unique nurse1

foreach prov1 in att1 res1 nurse1 {
	foreach prov2 in att1 res1 nurse1 {
	
	if "`prov1'" != "`prov2'" {
		*No. previous times `prov1' worked with `prov2' since the beginning of the dataset (cumulative)
		sort `prov1' `prov2' case_id
		by `prov1' `prov2': gen tf_`prov1'`prov2'_cump=_n-1 if !missing(`prov1') & !missing(`prov2')
		label var tf_`prov1'`prov2'_cump "Cumulative number of prior cases that `prov1' and `prov2' worked together since beginning of dataset"
		summ tf_`prov1'`prov2'_cump
		*No. times `prov1' worked with `prov2' in the entire dataset (total)
		sort `prov1' `prov2' case_id
		egen tf_`prov1'`prov2'_tot=max(tf_`prov1'`prov2'_cump) if !missing(`prov1') & !missing(`prov2'), by(`prov1' `prov2')
		replace tf_`prov1'`prov2'_tot=tf_`prov1'`prov2'_tot+1
		label var tf_`prov1'`prov2'_tot "Total number of cases that `prov1' and `prov2' worked together in the entire dataset"
		summ tf_`prov1'`prov2'_tot
	}
	}
}

*No. previous times att1, res1, and nurse1 worked together since the beginning of the dataset (cumulative)
sort att1 res1 nurse1 case_id
by att1 res1 nurse1: gen tf_att1res1nurse1_cump=_n-1 if !missing(att1) & !missing(res1) & !missing(nurse1)
label var tf_att1res1nurse1_cump "Cumulative number of prior cases that att1, res1, and nurse1 worked together since beginning of dataset"
summ tf_att1res1nurse1_cump
*No. times att1, res1, and nurse1 worked together in the entire dataset (total)
sort att1 res1 nurse1 case_id
egen tf_att1res1nurse1_tot=max(tf_att1res1nurse1_cum) if !missing(att1) & !missing(res1) & !missing(nurse1), by(att1 res1 nurse1)
replace tf_att1res1nurse1_tot=tf_att1res1nurse1_tot+1
label var tf_att1res1nurse1_tot "Total number of cases that att1, res1, and nurse1 worked together in the entire dataset"
summ tf_att1res1nurse1_tot

*Create variables for No. previous times att1, res1, and nurse1 worked each and together in the past 3 and 6 months
sort case_id att1 res1 nurse1
gen tf_att1res1nurse1_3mo = .
gen tf_att1res1nurse1_6mo = .
label var tf_att1res1nurse1_3mo "Cumulative number of prior cases that att1, res1, and nurse1 worked together in the past 3 months"
label var tf_att1res1nurse1_6mo "Cumulative number of prior cases that att1, res1, and nurse1 worked together in the past 6 months"
foreach prov1 in att1 res1 nurse1 {
	gen case_`prov1'_3mo = .
	gen case_`prov1'_6mo = .
	label var case_`prov1'_3mo "Cumulative number of prior cases that `prov1' worked on in the past 3 months"
	label var case_`prov1'_6mo "Cumulative number of prior cases that `prov1' worked on in the past 6 months"

	foreach prov2 in att1 res1 nurse1 {
		if "`prov1'" != "`prov2'" {
			gen tf_`prov1'`prov2'_3mo = .
			gen tf_`prov1'`prov2'_6mo = .
			label var tf_`prov1'`prov2'_3mo "Cumulative number of prior cases that `prov1' and `prov2' worked together in past 3 months"
			label var tf_`prov1'`prov2'_6mo "Cumulative number of prior cases that `prov1' and `prov2' worked together in past 6 months"

			gen hhi_`prov1'`prov2'_3mo=.
			label var hhi_`prov1'`prov2'_3mo "HHI for `prov1' with `prov2' for the last 3 months"
			gen hhi_`prov1'`prov2'_6mo=.
			label var hhi_`prov1'`prov2'_6mo "HHI for `prov1' with `prov2' for the last 6 months"
			}
	}
}
local 3mo=90*24*60*60*1000
local 6mo=180*24*60*60*1000
local N = _N
egen teamid = group(att1 res1 nurse1)

qui forvalues i=1/`N' {
	*calculate 1 and 2 way cumulative count for the last 3 or 6 months
	foreach prov1 in att1 res1 nurse1 {
		if !missing(`prov1'[`i']){
			//individual cumulative cases
			count if `prov1' == `prov1'[`i'] & inrange(arrivaltime[`i'] - arrivaltime, 0, `3mo')
			replace case_`prov1'_3mo= r(N)-1 in `i'
			count if `prov1' == `prov1'[`i'] &  inrange(arrivaltime[`i'] - arrivaltime, 0, `6mo')
			replace case_`prov1'_6mo= r(N)-1 in `i'

			//2 way cumulative cases
			foreach prov2 in att1 res1 nurse1 {
				if !missing(`prov2'[`i']) & "`prov1'" != "`prov2'" {
					count if `prov1' == `prov1'[`i'] &  `prov2' == `prov2'[`i'] & inrange(arrivaltime[`i'] - arrivaltime, 0, `3mo')
					replace tf_`prov1'`prov2'_3mo= r(N)-1 in `i'
					count if `prov1' == `prov1'[`i'] &  `prov2' == `prov2'[`i'] & inrange(arrivaltime[`i'] - arrivaltime, 0, `6mo')
					replace tf_`prov1'`prov2'_6mo= r(N)-1 in `i'
				}
			}
		}
	}
}

qui forvalues i=1/`N' {
	*calculate 3-way cumulative count for the last 3 or 6 months
	if !missing(att1[`i']) & !missing(res1[`i']) & !missing(nurse1[`i']) {
		count if att1 == att1[`i'] &  res1 == res1[`i'] & nurse1 == nurse1[`i'] & inrange(arrivaltime[`i'] - arrivaltime, 0, `3mo')
		replace tf_att1res1nurse1_3mo = r(N)-1 in `i'
		count if att1 == att1[`i'] &  res1 == res1[`i'] & nurse1 == nurse1[`i'] & inrange(arrivaltime[`i'] - arrivaltime, 0, `6mo')
		replace tf_att1res1nurse1_6mo = r(N)-1 in `i'
	}
}

*Partner diversity measured by the Hirschman-Herfindahl Index
/*NOTES: case_att1/res1/nurse1_cump gets redefined within the subpopulation that will be used to define each HHI variable
*/

* 2-way HHI
foreach prov1 in att1 res1 nurse1 {
	foreach prov2 in att1 res1 nurse1 {

		if "`prov1'" != "`prov2'" {
			preserve
			drop if missing(`prov1') | missing(`prov2')
			
			keep `prov1' `prov2' case_id tf_`prov1'`prov2'_cump
			sort `prov1' case_id
			by `prov1': gen case_`prov1'_cump=_n-1
			label var case_`prov1'_cump "Cumulative number of prior cases that `prov1' worked on since beginning of dataset"

			*cumulative HHI
			by `prov1': gen hhi_`prov1'`prov2'=1 if _n==1
			by `prov1': replace hhi_`prov1'`prov2'=(hhi_`prov1'`prov2'[_n-1]*((case_`prov1'_cump[_n-1])^2)-(tf_`prov1'`prov2'_cump[_n-1])^2+(tf_`prov1'`prov2'_cump[_n-1]+1)^2)/(case_`prov1'_cump[_n-1]+1)^2 if hhi_`prov1'`prov2'==.
			label var hhi_`prov1'`prov2' "HHI for `prov1' with `prov2'"

			compress
			save "`merge_data'/ED5_hhi_`prov1'`prov2'_`today'.dta", replace
			restore
			merge 1:1 `prov1' `prov2' case_id using "`merge_data'/ED5_hhi_`prov1'`prov2'_`today'.dta"
			count if missing(`prov1') | missing(`prov2')
			drop _merge case_`prov1'_cump
			sort `prov1' case_id
			summ hhi_`prov1'`prov2', det
		}
	}
}

local hhi_cur_3mo=.
local hhi_cur_6mo=.
foreach prov1 in att1 res1 nurse1 {
	foreach prov2 in att1 res1 nurse1 {
		if "`prov1'" != "`prov2'" {
			keep `prov1' `prov2' case_id tf_`prov1'`prov2'_3mo case_`prov1'_3mo tf_`prov1'`prov2'_6mo case_`prov1'_6mo hhi_`prov1'`prov2'_3mo hhi_`prov1'`prov2'_6mo arrivaltime
			drop if missing(`prov1') | missing(`prov2')

			qui forvalues i=1/`N' {
				preserve
				*calculate 3 mo HHI value for each observation
				drop if !inrange(arrivaltime[`i'] - arrivaltime, 0, `3mo')
				sort `prov1' case_id
				by `prov1': gen hhi_`prov1'`prov2'_3mo=1 if _n==1
				by `prov1': replace hhi_`prov1'`prov2'_3mo=(hhi_`prov1'`prov2'_3mo[_n-1]*((case_`prov1'_3mo[_n-1])^2)-(tf_`prov1'`prov2'_3mo[_n-1])^2+(tf_`prov1'`prov2'_3mo[_n-1]+1)^2)/(case_`prov1'_3mo[_n-1]+1)^2 if hhi_`prov1'`prov2'_3mo==.
				replace hhi_cur_3mo = hhi_`prov1'`prov2'_3mo[_N]

				*calculate 6 mo HHI value for each observation
				restore, preserve
				drop if !inrange(arrivaltime[`i'] - arrivaltime, 0, `6mo')
				sort `prov1' case_id
				by `prov1': gen hhi_`prov1'`prov2'_6mo=1 if _n==1
				by `prov1': replace hhi_`prov1'`prov2'_6mo=(hhi_`prov1'`prov2'_6mo[_n-1]*((case_`prov1'_6mo[_n-1])^2)-(tf_`prov1'`prov2'_6mo[_n-1])^2+(tf_`prov1'`prov2'_6mo[_n-1]+1)^2)/(case_`prov1'_6mo[_n-1]+1)^2 if hhi_`prov1'`prov2'_6mo==.
				replace hhi_cur_6mo = hhi_`prov1'`prov2'_6mo[_N]

				restore

				replace hhi_`prov1'`prov2'_3mo[`i'] = hhi_cur_3mo
				replace hhi_`prov1'`prov2'_6mo[`i'] = hhi_cur_6mo
				}

			compress
			save "`merge_data'/ED5_hhi_`prov1'`prov2'_36mo_`today'.dta", replace
			merge 1:1 `prov1' `prov2' case_id using "`merge_data'/ED5_hhi_`prov1'`prov2'_`today'.dta"
			count if missing(`prov1') | missing(`prov2')
			drop _merge
			summ hhi_`prov1'`prov2'_3mo hhi_`prov1'`prov2'_6mo, det
		}
	}
}

* 3-way HHI
preserve
drop if missing(att1) | missing(res1) | missing(nurse1)

keep att1 res1 nurse1 case_id tf_att1res1nurse1_* case_*
foreach prov1 in att1 res1 nurse1 {
	sort `prov1' case_id
	by `prov1': gen case_`prov1'_cump=_n-1
	label var case_`prov1'_cump "Cumulative number of prior cases that `prov1' worked on since beginning of dataset"
	
	*cumulative HHI3 since beginning dataset
	by `prov1': gen hhi3_`prov1'=1 if _n==1
	by `prov1': replace hhi3_`prov1'=(hhi3_`prov1'[_n-1]*((case_`prov1'_cump[_n-1])^2)-(tf_att1res1nurse1_cump[_n-1])^2+(tf_att1res1nurse1_cump[_n-1]+1)^2)/(case_`prov1'_cump[_n-1]+1)^2 if hhi3_`prov1'==. 
	label var hhi3_`prov1' "HHI3 for `prov1'"
	
	*cumulative HHI3 in last 3 mo 
	by `prov1': gen hhi3_`prov1'_3mo=1 if _n==1
	by `prov1': replace hhi3_`prov1'_3mo=(hhi3_`prov1'_3mo[_n-1]*((case_`prov1'_3mo[_n-1])^2)-(tf_att1res1nurse1_3mo[_n-1])^2+(tf_att1res1nurse1_3mo[_n-1]+1)^2)/(case_`prov1'_3mo[_n-1]+1)^2 if hhi3_`prov1'_3mo==. 
	label var hhi3_`prov1'_3mo "HHI3 for `prov1' for last 3 months"
	
	*cumulative HHI3 in last 6 mo
	by `prov1': gen hhi3_`prov1'_6mo=1 if _n==1
	by `prov1': replace hhi3_`prov1'_6mo=(hhi3_`prov1'_6mo[_n-1]*((case_`prov1'_6mo[_n-1])^2)-(tf_att1res1nurse1_6mo[_n-1])^2+(tf_att1res1nurse1_6mo[_n-1]+1)^2)/(case_`prov1'_6mo[_n-1]+1)^2 if hhi3_`prov1'_6mo==. 
	label var hhi3_`prov1'_6mo "HHI3 for `prov1' for last 6 months"
}
compress
save "`merge_data'/ED5_hhi3_`prov1'_`today'.dta", replace
restore
merge 1:1 att1 res1 nurse1  case_id using "`merge_data'/ED5_hhi3_`prov1'_`today'.dta"
count if missing(att1) | missing(res1) | missing(nurse1)
drop _merge case_att1_cump case_res1_cump case_nurse1_cump
summ hhi3*, det
		
*Cumulative number of prior cases each provider worked on since beginning of dataset
sort att1 case_id
by att1: gen case_att1_cump=_n-1 if !missing(att1)
label var case_att1_cump "Cumulative number of prior cases that att1 worked on since beginning of dataset"
sort res1 case_id
by res1: gen case_res1_cump=_n-1 if !missing(res1)
label var case_res1_cump "Cumulative number of prior cases that res1 worked on since beginning of dataset"
sort nurse1 case_id
by nurse1: gen case_nurse1_cump=_n-1 if !missing(nurse1)
label var case_nurse1_cump "Cumulative number of prior cases that nurse1 worked on since beginning of dataset"

*generate shift identifier
gen shift_id = date(arrivaltime-hours(7))

*LOS (raw and then capping values at 0 and 24 hours, i.e., 1440 mins)
sort case_id
gen timeinEDraw=minutes(departtime-arrivaltime)
gen timetoattraw=minutes(atttime-arrivaltime)
gen timetoresraw=minutes(restime-arrivaltime)
gen timetonurseraw=minutes(nursetime-arrivaltime)
rename timearrtolabraw timetolabraw
rename timearrtoradraw timetoradraw
gen timetodisporaw=minutes(dispotime-arrivaltime)
gen timeinprocess_todepartraw=timeinEDraw - min(timetoattraw,timetoresraw,timetonurseraw)
gen timeinprocess_todisporaw=timetodisporaw - min(timetoattraw,timetoresraw,timetonurseraw)

foreach t in timeinED timetoatt timetores timetonurse timetolab timetorad timetodispo timeinprocess_todepartraw timeinprocess_todisporaw{
	sum `t'raw
	gen `t' = `t'raw
	replace `t' =. if `t'raw > 1440
	replace `t' =. if `t'raw < 0
	sum `t'raw `t', det
}

*3) No. observations in each category and percentage for binary/categorical variables (report % missing)
*Gender
tab female, missing
*Race and Ethnic Group: Deleted (see do file 1)
*Disposition
tab eddispo, missing
*Pod
tab pod, missing
*Unit
tab unit, missing
*Arrival day of week and more
gen arrivaldow=dow(dofc(arrivaltime))
label define dow_l 0 "Sunday" 1 "Monday" 2 "Tuesday" 3 "Wednesday" 4 "Thursday" 5 "Friday" 6 "Saturday", replace
label values arrivaldow dow_l
gen arrivalmo=month(dofc(arrivaltime))
gen arrivalyr=year(dofc(arrivaltime))
gen arrivalmoyr = arrivalmo + (arrivalyr - 2009)*12
label var arrivalmoyr "Months since Jan 2009"
gen arrivalwk=week(dofc(arrivaltime))
gen arrivalwkyr = arrivalwk + (arrivalyr - 2009)*52
tab arrivaldow, missing
*Arrival time of day
gen arrivalhr=hh(arrivaltime)
tab arrivalhr, missing
tab arrivalmo arrivalyr, missing
*Arrival shift
recode arrivalhr (7/14=1) (15/22=2) (0/6 23=3), gen(arrivalshift) 
label define arrivalshift_l 1 "AM shift" 2 "PM shift" 3 "Overnight shift", replace
label values arrivalshift arrivalshift_l
tab arrivalshift, missing
*9 day revisit
sort pat_id case_id
local ninedays=9*24*60*60*1000
gen revisit_9days=0
replace revisit_9days=1 if pat_id==pat_id[_n-1] & (arrivaltime-arrivaltime[_n-1])<`ninedays' 
summ revisit_9days, det
*LWBS 
gen lwbs=0
replace lwbs=1 if eddispo == 5
tab lwbs, missing

*4) Characteristics of patients with more than one att/res/nurse
gen att_ct_cat=.
replace att_ct_cat=0 if att_ct==0
replace att_ct_cat=1 if att_ct==1
replace att_ct_cat=2 if att_ct>=2 
label define attresnurse_cat 0 "None" 1 "One" 2 "Two or more", replace
label var att_ct_cat attresnurse_cat 
tab att_ct_cat, missing
bysort att_ct_cat: tab acuity_n
bysort att_ct_cat: tab arrivaldow
bysort att_ct_cat: tab arrivalhr

gen res_ct_cat=.
replace res_ct_cat=0 if res_ct==0
replace res_ct_cat=1 if res_ct==1
replace res_ct_cat=2 if res_ct>=2 
label define resresnurse_cat 0 "None" 1 "One" 2 "Two or more", replace
label var res_ct_cat resresnurse_cat 
tab res_ct_cat, missing
bysort res_ct_cat: tab acuity_n
bysort res_ct_cat: tab arrivaldow
bysort res_ct_cat: tab arrivalhr

gen nurse_ct_cat=.
replace nurse_ct_cat=0 if nurse_ct==0
replace nurse_ct_cat=1 if nurse_ct==1
replace nurse_ct_cat=2 if nurse_ct>=2 
label define nursenursenurse_cat 0 "None" 1 "One" 2 "Two or more", replace
label var nurse_ct_cat nursenursenurse_cat 
tab nurse_ct_cat, missing
bysort nurse_ct_cat: tab acuity_n
bysort nurse_ct_cat: tab arrivaldow
bysort nurse_ct_cat: tab arrivalhr

save "`data'/ED5_cleaned2_`today'.dta", replace

****************SUMMARY TABLE BY ESI LEVEL****************
*for all dataset
estpost tabstat tf_att1res1_cump tf_att1nurse1_cump tf_res1nurse1_cump tf_att1res1nurse1_cump ///
		hhi_att1res1 hhi_att1nurse1 hhi_res1att1 hhi_res1nurse1 hhi_nurse1att1 ///
		hhi_nurse1res1 hhi3_att1 hhi3_res1 hhi3_nurse1 timeinEDraw timeinED ///
		timetoattraw timetoatt timetoresraw timetores timetonurseraw timetonurse ///
		timetolabraw timetolab timetoradraw timetorad timetodisporaw timetodispo ///
		age att_ct res_ct nurse_ct, ///
		stats(mean sd n) columns(statistics)
*for ESI
foreach x in 1 2 3 4 5 {
	estpost tabstat tf_att1res1_cump tf_att1nurse1_cump tf_res1nurse1_cump tf_att1res1nurse1_cump ///
		hhi_att1res1 hhi_att1nurse1 hhi_res1att1 hhi_res1nurse1 hhi_nurse1att1 ///
		hhi_nurse1res1 hhi3_att1 hhi3_res1 hhi3_nurse1 timeinEDraw timeinED ///
		timetoattraw timetoatt timetoresraw timetores timetonurseraw timetonurse ///
		timetolabraw timetolab timetoradraw timetorad timetodisporaw timetodispo ///
		age att_ct res_ct nurse_ct ///
		if acuity_n==`x', stats(mean sd n) columns(statistics)
	est store sumstat_`x'
} 
esttab sumstat_1 sumstat_2 sumstat_3 sumstat_4 sumstat_5, main(mean) aux(sd) obslast label unstack brackets, using"`table'/ED5_sumstat_cont_`today'.txt", replace tab

*for unit
foreach x in 1 2 3 4 5 6 {
	estpost tabstat tf_att1res1_cump ///
		tf_att1nurse1_cump tf_res1nurse1_cump tf_att1res1nurse1_cump ///
		hhi_att1res1 hhi_att1nurse1 hhi_res1att1 hhi_res1nurse1 hhi_nurse1att1 ///
		hhi_nurse1res1 hhi3_att1 hhi3_res1 hhi3_nurse1 timeinEDraw timeinED ///
		timetoattraw timetoatt timetoresraw timetores timetonurseraw timetonurse ///
		timetolabraw timetolab timetoradraw timetorad timetodisporaw timetodispo ///
		age att_ct res_ct nurse_ct ///
		if unit==`x', stats(mean sd n) columns(statistics)
} 

****************SUMMARY TABLE BY ESI LEVEL AND UNIT****************
*for all dataset
foreach var in revisit_9days lwbs female acuity_n eddispo meansofarrival unit arrivaldow arrivalmoyr arrivalshift arrivalhr {
	tab `var' , missing
}

*by ESI level
foreach x in 1 2 3 4 5 {
	foreach var in revisit_9days lwbs female eddispo meansofarrival unit arrivaldow arrivalmoyr arrivalshift arrivalhr {
	disp `x'
	tab `var' if acuity_n==`x', missing
	}
}
*by unit 
foreach x in 1 2 3 4 5 6 {
	foreach var in revisit_9days lwbs female acuity_n eddispo meansofarrival arrivaldow arrivalmoyr arrivalshift arrivalhr {
	disp `x'
	tab `var' if unit==`x', missing
	}
}

/*
****************SUMMARY FIGURES BY ESI LEVEL****************
hist acuity_n, by(att_ct_cat) xlabel(1(1)5)
graph export "`graph'/ED5_att_ct_cat_acuity_`today'.png", replace width(1150) height(800)
hist arrivaldow, by(att_ct_cat) xlabel(0(1)6)
graph export "`graph'/ED5_att_ct_cat_arrivaldow_`today'.png", replace width(1150) height(800)
hist arrivalhr, by(att_ct_cat) xlabel(0(4)23)
graph export "`graph'/ED5_att_ct_cat_arrivalhr_`today'.png", replace width(1150) height(800)

hist acuity_n, by(res_ct_cat) xlabel(1(1)5)
graph export "`graph'/ED5_res_ct_cat_acuity_`today'.png", replace width(1150) height(800)
hist arrivaldow, by(res_ct_cat) xlabel(0(1)6)
graph export "`graph'/ED5_res_ct_cat_arrivaldow_`today'.png", replace width(1150) height(800)
hist arrivalhr, by(res_ct_cat) xlabel(0(4)23)
graph export "`graph'/ED5_res_ct_cat_arrivalhr_`today'.png", replace width(1150) height(800)

hist acuity_n, by(nurse_ct_cat) xlabel(1(1)5)
graph export "`graph'/ED5_nurse_ct_cat_acuity_`today'.png", replace width(1150) height(800)
hist arrivaldow, by(nurse_ct_cat) xlabel(0(1)6)
graph export "`graph'/ED5_nurse_ct_cat_arrivaldow_`today'.png", replace width(1150) height(800)
hist arrivalhr, by(nurse_ct_cat) xlabel(0(4)23)
graph export "`graph'/ED5_nurse_ct_cat_arrivalhr_`today'.png", replace width(1150) height(800)

*/
**********************************************************
*B) Summary statistics at clinician level
**********************************************************
*Currently just looking at att1, res1, nurse1

*1) Number of unique attendings, residents, and nurses
unique att1
unique res1
unique nurse1

*2) Mean and sd of experience in days
*att1
sort att1 arrivaltime
preserve
collapse (min) min_arrivaltime_temp=arrivaltime (max) max_arrivaltime_temp=arrivaltime if !missing(att1), by(att1)
gen double exp_att1_tot=hours(max_arrivaltime_temp-min_arrivaltime_temp)/24 if !missing(att1)
drop *_temp
summ exp_att1_tot, det
restore

*res1
sort res1 arrivaltime
preserve
collapse (min) min_arrivaltime_temp=arrivaltime (max) max_arrivaltime_temp=arrivaltime if !missing(res1), by(res1)
gen double exp_res1_tot=hours(max_arrivaltime_temp-min_arrivaltime_temp)/24 if !missing(res1)
drop *_temp
summ exp_res1_tot, det
restore

*nurse1
sort nurse1 arrivaltime
preserve
collapse (min) min_arrivaltime_temp=arrivaltime (max) max_arrivaltime_temp=arrivaltime if !missing(nurse1), by(nurse1)
gen double exp_nurse1_tot=hours(max_arrivaltime_temp-min_arrivaltime_temp)/24 if !missing(nurse1)
drop *_temp
summ exp_nurse1_tot, det
restore

*3) Mean and sd of cumulative # different care providers the attending/resident/nurse worked with in past 6 months (assuming a month is 30.416667 days)
local sixmonths=6*30.416667*24*60*60*1000
*TBD post discussion

*4) Mean and sd of cumulative HHI measured every six months during the study period
*TBD post discussion


**********************************************************
*3) Find correlations between continuous variables
**********************************************************
ssc install mkcorr
*mkcorr timeinEDraw timeinED timetoattraw timetoatt timetoresraw timetores timetonurseraw timetonurse timetolabraw timetolab	timetoradraw	timetorad	lwbs	tf_att1res1_cump	tf_att1nurse1_cump	tf_res1nurse1_cump	tf_att1res1nurse1_cump	hhi_att1res1	hhi_att1nurse1	hhi_res1att1	hhi_res1nurse1	hhi_nurse1att1	hhi_nurse1res1	age	att_ct	res_ct	nurse_ct female acuity_n
mkcorr timeinEDraw timeinED timetoattraw timetoatt timetoresraw timetores timetonurseraw ///
timetonurse timetolabraw timetolab timetoradraw timetorad revisit_9days lwbs tf_att1res1_cump ///
tf_att1nurse1_cump tf_res1nurse1_cump tf_att1res1nurse1_cump	hhi_att1res1 ///
hhi_att1nurse1 hhi_res1att1	hhi_res1nurse1 hhi_nurse1att1 hhi_nurse1res1 ///
age att_ct res_ct nurse_ct female, log(ED5_corr) replace

**********************************************************
*4) Tenure Heatmaps for att, res, and nurse
**********************************************************
foreach prov in att res nurse {
	use "`data'/ED5_cleaned2_`today'.dta", clear

	drop if missing(`prov'1)
	sort `prov'1
	by `prov'1: egen `prov'_first_appearance = min(arrivaltime)
	format `prov'_first_appearance %tc
	by `prov'1: egen `prov'_last_appearance = max(departtime)
	format `prov'_last_appearance %tc
	gen `prov'_tenure = hours(`prov'_last_appearance - `prov'_first_appearance)/24/365

	collapse (count) num_cases=case_id (mean) `prov'_first_appearance `prov'_last_appearance `prov'_tenure, by(`prov'1 arrivalwkyr)

	sort `prov'1 arrivalwkyr
	by `prov'1: gen temp=1 if _n==1

	gsort -`prov'_tenure 
	replace temp=sum(temp) if ~missing(temp)
	sort `prov'1
	by `prov'1: egen `prov'id = max(temp)

	twoway (contour num_cases `prov'id arrivalwkyr, interp(none) crule(int) ccuts(0(100)300) ecolor(green)  heatmap), ytitle(`prov' ID) xtitle(Patient Arrival Week)
	graph export "`graph'/ED5_`prov'_heatmap_`today'.png", replace width(1150) height(800)
	
	drop temp
	reshape wide num_cases, i(`prov'id) j(arrivalwkyr)
	egen num_cases = rowtotal(num_cases*) 
	order `prov'id `prov'1 `prov'_first_appearance `prov'_last_appearance `prov'_tenure num_cases
	export excel using "`table'/ED5_heatmap_`today'.xls", sheet("`prov'", replace) firstrow(variables) 
}

log close
*EOF
