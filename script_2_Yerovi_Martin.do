/**************************************************************************************************
 Project: CP2022
 Author:  Martin Yerovi
 Purpose: Estimate the effect of cell overcrowding on perceived cell safety using a recursive mixed-process model (CMP). The endogenous regressor is cell overcrowding, instrumented with whether sentenced and pre-trial inmates are held in separate facilities.

 Notes:
   - This script is intended as a clean, reproducible version for application purposes.
   - Update the global path below before running the script.
   - The script stops after the main CMP model.
**************************************************************************************************/

clear all
set more off
version 17

*-----------------------------------------------------------------------------------------------*
* 0. User settings
*-----------------------------------------------------------------------------------------------*

* Update this path to the folder where the CP2022 dataset is stored.
global data_path "D:\Users\Martin\OneDrive\Documentos\EPN\Martin\Tesis"
use "$data_path\bdd_cp2022.dta", clear

*-----------------------------------------------------------------------------------------------*
* 1. Outcome variable: perceived safety inside the cell
*-----------------------------------------------------------------------------------------------*
* Source variable:
*   f1_s5p47: self-reported feeling of safety in the cell.
*
* Coding used in the survey:
*   1 = safe
*   2 = unsafe
*   8/9 = don't know / no response
*
* Final variable:
*   seg_bin = 1 if the respondent feels safe in the cell; 0 otherwise.

drop if inlist(f1_s5p47, 8, 9)

gen byte seg_bin = .
replace seg_bin = 1 if f1_s5p47 == 1
replace seg_bin = 0 if f1_s5p47 == 2

label define seg_bin_lbl 0 "Unsafe" 1 "Safe", replace
label values seg_bin seg_bin_lbl
label var seg_bin "Feels safe inside the cell"


*-----------------------------------------------------------------------------------------------*
* 2. Main explanatory variable: cell overcrowding
*-----------------------------------------------------------------------------------------------*
* Source variable:
*   f1_s5p01: number of people with whom the respondent shares the cell.
*
* The variable is top-coded at 40 to reduce the influence of extreme values. The logarithmic
* transformation allows for non-linear effects and keeps observations in individual cells by adding 1.

gen hacinamiento = f1_s5p01
replace hacinamiento = . if inlist(f1_s5p01, 98, 99)

replace hacinamiento = 40 if hacinamiento > 40 & hacinamiento < .
gen ln_hacin = ln(hacinamiento + 1)

label var hacinamiento "Number of people sharing the cell"
label var ln_hacin "Log number of people sharing the cell plus one"


*-----------------------------------------------------------------------------------------------*
* 3. Sociodemographic controls
*-----------------------------------------------------------------------------------------------*

gen edad     = f1_s2_p03
gen sexo     = f1_s2_p02
gen educ     = f1_s2_p17
gen conyugal = f1_s2_p31
gen etnia    = f1_s2_p11

label var edad     "Age"
label var sexo     "Sex assigned at birth"
label var educ     "Educational attainment"
label var conyugal "Marital status"
label var etnia    "Ethnic self-identification"

* Educational attainment collapsed into four categories.
recode educ ///
    (1/4  = 1) ///
    (5/6  = 2) ///
    (7/8  = 3) ///
    (9/13 = 4), gen(educ4)

label define educ4_lbl ///
    1 "Primary or less" ///
    2 "Lower secondary" ///
    3 "Upper secondary" ///
    4 "Higher education", replace

label values educ4 educ4_lbl
label var educ4 "Educational attainment, four categories"

* Marital status collapsed into three categories.
recode conyugal ///
    (1   = 1) ///
    (2/3 = 2) ///
    (4/9 = 3), gen(conyugal3)

label define conyugal3_lbl ///
    1 "Single" ///
    2 "Married or in union" ///
    3 "Separated, divorced, or widowed", replace

label values conyugal3 conyugal3_lbl
label var conyugal3 "Marital status, three categories"


*-----------------------------------------------------------------------------------------------*
* 4. Sentence and prison-exposure controls
*-----------------------------------------------------------------------------------------------*

gen sentence_years  = f1_s4p1301
gen sentence_years2 = sentence_years^2 if sentence_years < .

label var sentence_years  "Sentence length in years"
label var sentence_years2 "Sentence length in years squared"

* Effective time already spent in prison, collapsed into three categories.
recode f1_s4p01 (8 9 = .)

gen byte time_prison_cat3 = .
replace time_prison_cat3 = 1 if f1_s4p01 == 1
replace time_prison_cat3 = 2 if inlist(f1_s4p01, 2, 3)
replace time_prison_cat3 = 3 if inlist(f1_s4p01, 4, 5)

label define time_prison_lbl ///
    1 "Up to 6 months" ///
    2 "6 months to 1.5 years" ///
    3 "More than 1.5 years", replace

label values time_prison_cat3 time_prison_lbl
label var time_prison_cat3 "Effective time in prison, three categories"


*-----------------------------------------------------------------------------------------------*
* 5. Institutional controls
*-----------------------------------------------------------------------------------------------*
* Prison fixed effects are included to absorb time-invariant differences across facilities.

encode f1_s1_i12, gen(prison_fe)
label var prison_fe "Prison fixed effects"


*-----------------------------------------------------------------------------------------------*
* 6. Additional prison-experience controls
*-----------------------------------------------------------------------------------------------*

gen traslado_prev = (f1_s5p51 == 1) if inlist(f1_s5p51, 1, 2)
gen visitas       = (f1_s5p42 == 1) if inlist(f1_s5p42, 1, 2)

label var traslado_prev "Previously transferred between prisons"
label var visitas       "Received visits while in prison"


*-----------------------------------------------------------------------------------------------*
* 7. Criminal-history controls
*-----------------------------------------------------------------------------------------------*

gen recidivist = f1_s6p01
gen byte recidivist_bin = .
replace recidivist_bin = 1 if recidivist == 1
replace recidivist_bin = 0 if recidivist == 2

label var recidivist_bin "Repeat offender"

* Original crime-type dummies.
foreach i of numlist 1/19 {
    gen crime_`i' = f1_s4p16__`i'
    label var crime_`i' "Crime type `i'"
}

* Crime groups used as controls.
gen byte crime_property = 0
replace crime_property = 1 if ///
    crime_1  == 1 | ///
    crime_7  == 1 | ///
    crime_8  == 1 | ///
    crime_9  == 1 | ///
    crime_10 == 1 | ///
    crime_16 == 1

label var crime_property "Property or economic crime"

gen byte crime_illicit = 0
replace crime_illicit = 1 if ///
    crime_2 == 1 | ///
    crime_6 == 1

label var crime_illicit "Drug- or weapons-related crime"

gen byte crime_violent = 0
replace crime_violent = 1 if ///
    crime_3  == 1 | ///
    crime_4  == 1 | ///
    crime_5  == 1 | ///
    crime_11 == 1 | ///
    crime_12 == 1 | ///
    crime_15 == 1 | ///
    crime_17 == 1 | ///
    crime_18 == 1

label var crime_violent "Violent, coercive, or sexual crime"

gen byte crime_org = 0
replace crime_org = 1 if ///
    crime_13 == 1 | ///
    crime_14 == 1

label var crime_org "Organized-crime-related offense"

gen byte crime_other = 0
replace crime_other = 1 if crime_19 == 1

label var crime_other "Other reported offense"


*-----------------------------------------------------------------------------------------------*
* 8. Instrumental variable
*-----------------------------------------------------------------------------------------------*
* Source variable:
*   f1_s5p07: whether sentenced and pre-trial inmates are held in separate facilities.
*
* Instrument:
*   sep_proc_i = 1 if sentenced and pre-trial inmates are separated; 0 otherwise.

recode f1_s5p07 (8 9 = .)

gen byte sep_proc_i = .
replace sep_proc_i = 1 if f1_s5p07 == 1
replace sep_proc_i = 0 if f1_s5p07 == 2

label var sep_proc_i "Separation of sentenced and pre-trial inmates"


*-----------------------------------------------------------------------------------------------*
* 9. Estimation: recursive mixed-process model
*-----------------------------------------------------------------------------------------------*
* Equation 1: perceived cell safety, estimated as a probit model.
* Equation 2: cell overcrowding, estimated as a continuous endogenous variable.
*
* The model uses sep_proc_i as an instrument for ln_hacin and includes prison fixed effects.

capture which cmp
if _rc {
    display as error "The command 'cmp' is not installed. Install it before running this model:"
    display as error "ssc install cmp"
    exit 199
}

cmp setup

cmp ///
    (seg_bin = ln_hacin ///
        traslado_prev visitas ///
        edad sexo i.educ4 i.conyugal3 i.etnia ///
        sentence_years sentence_years2 ///
        recidivist_bin ///
        crime_property crime_illicit crime_violent crime_org crime_other ///
        i.time_prison_cat3 ///
        i.prison_fe) ///
    (ln_hacin = sep_proc_i ///
        traslado_prev visitas ///
        edad sexo i.educ4 i.conyugal3 i.etnia ///
        sentence_years sentence_years2 ///
        recidivist_bin ///
        crime_property crime_illicit crime_violent crime_org crime_other ///
        i.time_prison_cat3 ///
        i.prison_fe), ///
    indicators($cmp_probit $cmp_cont) ///
    vce(robust)

estimates store cmp_main

