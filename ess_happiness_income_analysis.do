********************************************************************************
* Household Income and Happiness in Italy
* European Social Survey (ESS), Round 11
*
* Academic project developed in Stata
*
* Main tasks:
* - data preparation and recoding
* - survey-weighted univariate and bivariate analysis
* - data visualisation
* - multinomial logistic regression
* - interaction analysis
*
* The descriptive analysis uses the ESS analysis weight and survey design.
* Multinomial regression models are estimated without survey weights, following
* the methodological approach used in the project.
********************************************************************************

clear all
set more off

* Update this path before running the script
use "ESS11e04_1-subset.dta", clear


********************************************************************************
* 1. DATA PREPARATION AND VARIABLE RECODING
********************************************************************************

* Happiness: Low (0-4), Medium (5-7), High (8-10)
recode happy ///
    (0/4 = 1 "Low") ///
    (5/7 = 2 "Medium") ///
    (8/10 = 3 "High"), ///
    gen(happy3)

* Household income: five ordered categories + missing category
recode hinctnta ///
    (1/2  = 1 "Very low") ///
    (3/4  = 2 "Low") ///
    (5/6  = 3 "Medium") ///
    (7/8  = 4 "High") ///
    (9/10 = 5 "Very high") ///
    (missing = 99 "Missing"), ///
    gen(income)

* Age group: retain original categories and identify missing values
replace agegroup = 99 if missing(agegroup)
label define agegroup 99 "Missing", modify
label values agegroup agegroup

* Education
recode eisced ///
    (1   = 1 "Primary") ///
    (2   = 2 "Lower Secondary") ///
    (3/5 = 3 "Upper Secondary") ///
    (6/7 = 4 "Tertiary") ///
    (55  = 99 "Missing"), ///
    gen(education)

replace education = 99 if missing(eisced)

* Main source of household income
recode hincsrca ///
    (1/3 = 1 "Work") ///
    (4   = 2 "Pensions") ///
    (5/6 = 3 "Welfare/benefits") ///
    (7/8 = 4 "Investments/Other") ///
    (missing = 99 "Missing"), ///
    gen(main_income)

* Self-reported health
recode health ///
    (4/5 = 1 "Bad") ///
    (3   = 2 "Medium") ///
    (1/2 = 3 "Good") ///
    (missing = 99 "Missing"), ///
    gen(health_cl)

* Marital status
recode maritalb ///
    (1/2 = 1 "Married") ///
    (3/6 = 2 "Not Married") ///
    (missing = 99 "Missing"), ///
    gen(maritalb_cl)

* Parenthood
gen child = chldhhe
replace child = 99 if missing(child)
label define child_lb 1 "Yes" 2 "No" 99 "Missing"
label values child child_lb

* Frequency of social meetings
recode sclmeet ///
    (1/2 = 1 "Low") ///
    (3/4 = 2 "Medium") ///
    (5/7 = 3 "High") ///
    (missing = 99 "Missing"), ///
    gen(social_meeting)

* Perceived control over life
recode ctrlife ///
    (0/3  = 1 "Low") ///
    (4/6  = 2 "Medium") ///
    (7/10 = 3 "High") ///
    (missing = 99 "Missing"), ///
    gen(control)

* Urbanisation
recode domicil ///
    (1/3 = 1 "Urban") ///
    (4/5 = 2 "Non-Urban") ///
    (missing = 99 "Missing"), ///
    gen(urban)

* Macro-region
gen region_group = .
replace region_group = 1 if inlist(region, "ITC", "ITH")
replace region_group = 2 if region == "ITI"
replace region_group = 3 if inlist(region, "ITF", "ITG")
replace region_group = 99 if missing(region_group)

label define region_lb ///
    1 "North" ///
    2 "Centre" ///
    3 "South and Islands" ///
    99 "Missing"

label values region_group region_lb

* Main activity
replace mnactic = 99 if missing(mnactic)

recode mnactic ///
    (1   = 1 "Paid Worker") ///
    (2   = 2 "Student") ///
    (3   = 3 "Unemployed") ///
    (4/8 = 4 "Inactive") ///
    (9   = 99 "Missing"), ///
    gen(main_activity)

replace main_activity = 99 if missing(main_activity)

* Combine labour-market position with contract type
replace wrkctra = 99 if missing(wrkctra)

gen main_activity2 = .

replace main_activity2 = 1  if main_activity == 1 & wrkctra == 1
replace main_activity2 = 2  if main_activity == 1 & wrkctra != 1 & wrkctra != 99
replace main_activity2 = 99 if main_activity == 1 & wrkctra == 99
replace main_activity2 = 3  if main_activity == 2
replace main_activity2 = 4  if main_activity == 3
replace main_activity2 = 5  if main_activity == 4
replace main_activity2 = 99 if main_activity == 99

label define activity_lb ///
    1 "Paid Worker - permanent" ///
    2 "Paid Worker - temporary" ///
    3 "Student" ///
    4 "Unemployed" ///
    5 "Inactive" ///
    99 "Missing"

label values main_activity2 activity_lb

* Under 35 and living with parents
egen lives_with_parents = anymatch(rshipa2-rshipa12), values(3)

gen par_home_u35 = .
replace par_home_u35 = 1 if lives_with_parents == 1 & agea < 35
replace par_home_u35 = 0 if lives_with_parents == 0 | agea >= 35

label define par_home_lb 0 "No" 1 "Yes"
label values par_home_u35 par_home_lb


********************************************************************************
* 2. SURVEY DESIGN
********************************************************************************

svyset psu [pweight=anweight], strata(stratum)


********************************************************************************
* 3. UNIVARIATE DESCRIPTIVE ANALYSIS
********************************************************************************

svy: tab income, percent
svy: tab happy3, percent
svy: tab agegroup, percent
svy: tab gndr, percent
svy: tab education, percent
svy: tab health_cl, percent
svy: tab maritalb_cl, percent
svy: tab child, percent
svy: tab main_activity2, percent
svy: tab control, percent
svy: tab urban, percent
svy: tab region_group, percent
svy: tab social_meeting, percent
svy: tab main_income, percent


********************************************************************************
* 4. BIVARIATE ANALYSIS
********************************************************************************

* Main relationship: household income and happiness
svy: tab income happy3, row percent

* Selected socioeconomic and demographic controls
svy: tab agegroup happy3, row percent
svy: tab gndr happy3, row percent
svy: tab education happy3, row percent
svy: tab main_income happy3, row percent
svy: tab health_cl happy3, row percent
svy: tab maritalb_cl happy3, row percent
svy: tab child happy3, row percent
svy: tab main_activity2 happy3, row percent
svy: tab control happy3, row percent
svy: tab urban happy3, row percent
svy: tab region_group happy3, row percent
svy: tab social_meeting happy3, row percent
svy: tab par_home_u35 happy3, row percent


********************************************************************************
* 5. DATA VISUALISATION
********************************************************************************

* Create percentage indicators for the three happiness categories
tab happy3, gen(happy_cat)

gen happy1_pct = happy_cat1 * 100
gen happy2_pct = happy_cat2 * 100
gen happy3_pct = happy_cat3 * 100


* Happiness by household income
graph bar (mean) happy1_pct happy2_pct happy3_pct [pweight=anweight] ///
    if income != 99, ///
    over(income, label(angle(0) labsize(small))) ///
    stack ///
    ytitle("Percentage of respondents", size(medsmall)) ///
    title("Distribution of happiness by household income") ///
    legend(order(1 "Low happiness" 2 "Medium happiness" 3 "High happiness") ///
           position(6) rows(1) size(small) region(lcolor(white))) ///
    ylabel(0(20)100, grid labsize(small)) ///
    blabel(bar, position(center) format(%4.1f) size(vsmall) color(black)) ///
    bar(1, color("198 233 240")) ///
    bar(2, color("102 194 214")) ///
    bar(3, color("41 128 185")) ///
    graphregion(color(white)) ///
    plotregion(color(white))

graph export "happiness_by_income.png", replace width(3000)


* Happiness by age group
graph bar (mean) happy1_pct happy2_pct happy3_pct [pweight=anweight], ///
    over(agegroup, label(angle(0) labsize(small))) ///
    stack ///
    ytitle("Percentage of respondents", size(medsmall)) ///
    title("Distribution of happiness by age group") ///
    legend(order(1 "Low happiness" 2 "Medium happiness" 3 "High happiness") ///
           position(6) rows(1) size(small) region(lcolor(white))) ///
    ylabel(0(20)100, grid labsize(small)) ///
    blabel(bar, position(center) format(%4.1f) size(vsmall) color(black)) ///
    bar(1, color("198 233 240")) ///
    bar(2, color("102 194 214")) ///
    bar(3, color("41 128 185")) ///
    graphregion(color(white)) ///
    plotregion(color(white))

graph export "happiness_by_age.png", replace width(3000)


* Happiness by education
graph bar (mean) happy1_pct happy2_pct happy3_pct [pweight=anweight], ///
    over(education, label(angle(0) labsize(small))) ///
    stack ///
    ytitle("Percentage of respondents", size(medsmall)) ///
    title("Distribution of happiness by education") ///
    legend(order(1 "Low happiness" 2 "Medium happiness" 3 "High happiness") ///
           position(6) rows(1) size(small) region(lcolor(white))) ///
    ylabel(0(20)100, grid labsize(small)) ///
    blabel(bar, position(center) format(%4.1f) size(vsmall) color(black)) ///
    bar(1, color("198 233 240")) ///
    bar(2, color("102 194 214")) ///
    bar(3, color("41 128 185")) ///
    graphregion(color(white)) ///
    plotregion(color(white))

graph export "happiness_by_education.png", replace width(3000)


* Happiness by self-reported health
graph bar (mean) happy1_pct happy2_pct happy3_pct [pweight=anweight], ///
    over(health_cl, label(angle(0) labsize(small))) ///
    stack ///
    ytitle("Percentage of respondents", size(medsmall)) ///
    title("Distribution of happiness by health status") ///
    legend(order(1 "Low happiness" 2 "Medium happiness" 3 "High happiness") ///
           position(6) rows(1) size(small) region(lcolor(white))) ///
    ylabel(0(20)100, grid labsize(small)) ///
    blabel(bar, position(center) format(%4.1f) size(vsmall) color(black)) ///
    bar(1, color("198 233 240")) ///
    bar(2, color("102 194 214")) ///
    bar(3, color("41 128 185")) ///
    graphregion(color(white)) ///
    plotregion(color(white))

graph export "happiness_by_health.png", replace width(3000)


********************************************************************************
* 6. MULTINOMIAL LOGISTIC REGRESSION
********************************************************************************

* Medium happiness (category 2) is the reference outcome.

* Baseline model: household income only
mlogit happy3 i.income, rrr baseoutcome(2)

* Full model with socioeconomic, demographic and personal controls
mlogit happy3 ///
    i.income ///
    i.agegroup ///
    i.gndr ///
    i.main_income ///
    i.health_cl ///
    i.education ///
    i.maritalb_cl ///
    i.child ///
    i.urban ///
    i.region_group ///
    i.social_meeting ///
    i.control ///
    i.main_activity2, ///
    rrr baseoutcome(2)


********************************************************************************
* 7. INTERACTION MODELS
********************************************************************************

* Gender and parental status
mlogit happy3 ///
    i.income ///
    i.agegroup ///
    i.gndr##i.child ///
    i.main_income ///
    i.health_cl ///
    i.education ///
    i.maritalb_cl ///
    i.urban ///
    i.region_group ///
    i.social_meeting ///
    i.control ///
    i.main_activity2, ///
    rrr baseoutcome(2)

* Gender and household income
mlogit happy3 ///
    i.income##i.gndr ///
    i.agegroup ///
    i.main_income ///
    i.health_cl ///
    i.education ///
    i.maritalb_cl ///
    i.child ///
    i.urban ///
    i.region_group ///
    i.social_meeting ///
    i.control ///
    i.main_activity2, ///
    rrr baseoutcome(2)


********************************************************************************
* END OF SCRIPT
********************************************************************************
