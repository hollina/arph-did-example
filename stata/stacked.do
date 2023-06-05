import delimited "~/Documents/GitHub/arph-did-example/stata/timing_yes_growing_yes.csv", clear 

// Run simple TWFE to show issue. 
reghdfe fake_value treated, absorb(year state_pc) vce(cluster state_pc)


// Set up for stacked. 
gen no_treat = 0
replace no_treat = 1 if fake_year_treated == 2020

replace fake_year_treated = . if fake_year_treated == 2020
gen rel_time = year - fake_year_treated


summ rel_time
local relmin = abs(r(min))
local relmax = abs(r(max))

	// leads
	cap drop F_*
	forval x = 1/`relmin' {  // drop the first lead
		gen     F_`x' = rel_time == -`x'
		replace F_`x' = 0 if no_treat==1
	}

	
	//lags
	cap drop L_*
	forval x = 0/`relmax' {
		gen     L_`x' = rel_time ==  `x'
		replace L_`x' = 0 if no_treat==1
	}

ren F_1 ref  //base year
qui ds F_* L_* ref
local lead_lag_list = r(varlist)
egen id = group(state_pc)

// Run stacked on treated variable. 
preserve
stackedev fake_value treated, cohort(fake_year_treated) time(year) never_treat(no_treat) unit_fe(id) clust_unit(id)

mat results = r(table)'
svmat results
mat list results



rename  results1 b 
rename  results2 se

drop if missing(b)
outsheet b se using"~/Documents/GitHub/arph-did-example/stata/timing_yes_growing_yes_stacked_att.csv" , comma replace

restore
stackedev fake_value F_*  L_* ref, cohort(fake_year_treated) time(year) never_treat(no_treat) unit_fe(id) clust_unit(id)

exit
mat results = r(table)'
svmat results
mat list results

rename  results1 b 
rename  results2 se
gen type = ""
local i = 1
foreach v in `lead_lag_list' {
	replace type = "`v' " in `i'
	local i = `i' + 1
}
di "`lead_lag_list'"
replace type = subinstr(type, "L_", "",.) 
replace type = subinstr(type, "F_", "-",.) 
replace type = subinstr(type, "ref", "-1",.) 


keep type b se 
drop if missing(type)
destring type, replace force
sort type
order type
replace se = 0 if type == -1
outsheet type b se using"~/Documents/GitHub/arph-did-example/stata/timing_yes_growing_yes_stacked_es.csv" , comma replace

exit

event_plot, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-10(1)10) ///
		title("stackedev")) stub_lag(L_#) stub_lead(F_#) trimlag(10) trimlead(10) together 
		
		stackedev fake_value treated, cohort(fake_year_treated) time(year) never_treat(no_treat) unit_fe(id) clust_unit(id)
		
