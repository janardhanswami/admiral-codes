

install.packages("haven")
library(haven)
adsl1 <- read_xpt("adam//adsl.xpt")
dm1 <- read_xpt("sdtm//dm.xpt")
sv1 <- read_xpt("sdtm//sv.xpt")
ex1 <- read_xpt("sdtm//ex.xpt")
ds1 <- read_xpt("sdtm//ds.xpt")
qs1 <- read_xpt("sdtm//qs.xpt")
vs1 <- read_xpt("sdtm//vs.xpt")
sc1 <- read_xpt("sdtm//sc.xpt")
mh1 <- read_xpt("sdtm//mh.xpt")

View(adsl1)

dm2 <- dm1 %>%
  select(STUDYID,USUBJID,SUBJID,SITEID,AGE, AGEU,RACE,SEX, ETHNIC,RFSTDTC,RFENDTC)

View(dm2)
#deriving arm OR TRT related variables like TRT01P TRT01PN, TRT01A, TRT01AN

trt <- dm2 %>%
  mutate(SITEGR1=SITEID,
         TRT01P=ARM,
         TRT01A=ARM,
         TRT01AN = case_when(ARM == "Placebo" ~ 1,
                             ARM == "Xanomeline High Dose" ~2,
                             ARM == "Xanomeline Low Dose" ~3,
                             ARM == "Screen Failure" ~4),
         TRT01PN = TRT01AN)


#creating treatment start date TRTSDT variable

sv2 <- sv1 %>%
  select(USUBJID,SVSTDTC, VISITNUM)

trts <- derive_vars_merged( trt, sv2,
                            filter_add = VISITNUM == 3.0,
                            new_vars = vars(TRTSSDT = SVSTDTC),
                            by_vars = vars(USUBJID)) %>%
derive_vars_dt(new_vars_prefix = "TRTS",
                 dtc = TRTSSDT) %>%
  select(USUBJID, TRTSDT,TRT01P,TRT01A,TRT01AN,TRT01PN)



#deriving treatment end date variable (TRTENDT)

trte <-
  derive_vars_merged(dm1, ex2,
                     filter_add = !is.na(EXENDTC),
                     new_vars = vars(TRTEEDT= EXENDTC),
                     by_vars = vars(USUBJID),
                     order = vars(EXENDTC),
                     mode="last"
                     ) %>%
  derive_vars_dt(new_vars_prefix="TRTE",
                 dtc = TRTEEDT) %>%
  select(USUBJID, TRTEDT,ARM, ARMCD)


 trt <-  derive_vars_merged(trts, trte,
                     by_vars =  vars(USUBJID))


 vis1dt <- derive_vars_dt(sv1, new_vars_prefix ="VISIT1",dtc=SVSTDTC) %>%
          select(USUBJID, VISIT1DT, VISIT)

 trtvars <- derive_vars_merged(trt, vis1dt, by_vars = vars(USUBJID), filter_add = (VISIT == "SCREENING 1")) %>%
   select(-VISIT)

 #deriving treatment duration varaible (trtdurd)
 trtdurd <- derive_var_trtdurd(trtvars,
                               start_date = TRTSDT,
                               end_date = TRTEDT)



#deriving all flag variables
flgvar <- trtdurd %>%
  mutate(ITTFL = if_else(ARM != "", "Y", "N",""),
         SAFFL = if_else(ITTFL == "Y" & !is.na(TRTSDT), "Y", "N"))

#deriving EFFFL flag

EFFFL <- qs1 %>%
  filter( QSTESTCD == "CIBIC" & VISITNUM >3 ) %>%
  distinct(USUBJID) %>%
  mutate(EFFFL="Y")

derive_vars_merged(dm1, dataset_add =qs1, dataset_add = flgvar, by_vars = vars(USUBJID))

flagvar1 <- derive_vars_merged( flgvar,EFFFL, by_vars = vars(USUBJID))





  #height and weight baseline values from vs

  hgt <- vs1 %>%
filter(VSTESTCD =='HEIGHT' & VISIT =="SCREENING 1") %>%
    mutate(HEIGHTBL = VSSTRESN) %>%
    select(USUBJID, HEIGHTBL)

  wgt <- vs1 %>%
    filter(VSTESTCD == "WEIGHT" & VISIT == "BASELINE") %>%
    mutate(WEIGHTBL = VSSTRESN) %>%
    select(USUBJID, WEIGHTBL)

  #merging those vars together

htwt <- derive_vars_merged(hgt,wgt, by_vars=vars(USUBJID)) %>%
  mutate(BMIBL = WEIGHTBL / ((HEIGHTBL/100)**2),
         BMIBLGR1 = case_when(BMIBL < 25 ~ "<25",
                              BMIBL >= 25 & BMIBL < 30 ~ "25-<30",
                              BMIBL >= 30 ~ ">30"))

#merging with main  dataset

var2 <- derive_vars_merged(flagvar1, htwt, by_vars=vars(USUBJID))
#deriving EDUCLVL variable
var3 <- derive_vars_merged(var2, sc1, by_vars = vars(USUBJID), filter_add = (SCTESTCD=="EDLEVEL"),
                           new_vars = vars(EDUCLVL=SCSTRESN))


var4 <- derive_vars_merged(var3, mh1, by_vars = vars(USUBJID), filter_add = (MHCAT=='PRIMARY DIAGNOSIS'),
                           new_vars = vars(DISONSSDT= MHSTDTC)) %>%
  derive_vars_dt(new_vars_prefix = "DISONS", dtc= DISONSSDT) %>%
  select(-DISONSSDT) %>%

#deriving DURDIS variable

 derive_vars_duration( new_var = DURDIS, new_var_unit = NULL, DISONSDT, VISIT1DT,
                       in_unit = "days", out_unit="months") %>%
#deriving group1 var DURDSGR1

  mutate(DURDSGR1= case_when(DURDIS<12 ~ "<12",
                             DURDIS>=12 ~ ">=12" ))

 #Flag variables from SV domain

  cmpfls <- sv1 %>%
  derive_vars_dt(new_vars_prefix = "SVS",
                 dtc = SVSTDTC) %>%
  derive_vars_dt(new_vars_prefix = "SVE",
                 dtc = SVENDTC)


cmp8fl <- cmpfls %>%
  filter(VISIT == "WEEK 8") %>%
  mutate(COMP8FL = case_when(SVEDT >= SVSDT ~ "Y",
                             TRUE ~ "N")) %>%
  select(USUBJID, COMP8FL)

cmp16fl <- cmpfls %>%
  filter(VISIT == "WEEK 16") %>%
  mutate(COMP16FL = case_when(SVEDT >= SVSDT ~ "Y",
                              TRUE ~ "N")) %>%
  select(USUBJID, COMP16FL)

cmp24fl <- cmpfls %>%
  filter(VISIT == 'WEEK 24') %>%
  mutate(COMP24FL = case_when(SVEDT >= SVSDT ~ "Y",
                              TRUE ~ "N")) %>%
  select(USUBJID, COMP24FL)


allcmp <- derive_vars_merged(cmp8fl, cmp16fl, by_vars = vars(USUBJID))
allcmp1  <-  derive_vars_merged(cmp24fl, allcmp, by_vars = vars(USUBJID))

var5 <- derive_vars_merged(var4, allcmp1, by_vars = vars(USUBJID))


 #deriving vars from DS
 var6 <-
   derive_vars_merged(var5,ds1, by_vars=vars(USUBJID), filter_add = (DSCAT == "DISPOSITION EVENT"),
                     new_vars = vars(DCDECOD = DSDECOD)) %>%
  mutate(EOSSTT = if_else(DCDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED"),
         DCREASCD = if_else(EOSSTT == "COMPLETED", EOSSTT, DCDECOD),
         DISCONFL =  if_else(DCREASCD == "COMPLETED", "", "Y" ),
         DSRAEFL =  if_else(DCREASCD == "ADVERSE EVENT", "Y", ""))

 tem <- ds1 %>%
   filter(DSTERM == "PROTOCOL COMPLETED") %>%
   mutate(VISNUMEN = if_else(VISIT == "WEEK 26", 12, VISITNUM)) %>%
   select(USUBJID, VISNUMEN)

 var7 <- derive_vars_merged(var6,tem, by_vars=vars(USUBJID))


#derivation of MMSETOT variable
 qs3 <- qs1 %>%
   filter(QSCAT == "MINI-MENTAL STATE") %>%
   group_by(USUBJID) %>%
   mutate(MMSETOT = sum(QSSTRESN)) %>%
   distinct(USUBJID, MMSETOT) %>%
   ungroup()

 ads <- derive_vars_merged(var7, qs3, by_vars=vars(USUBJID))
 adsl <- derive_vars_merged(ads, dm2, by_vars = vars(USUBJID))

  View(adsl)

#using metacore and metatools on the final ADSL dataset

 metacore <- spec_to_metacore("metadata//specs.xlsx", where_sep_sheet = F, quiet = T)

 specs <- metacore %>%
   select_dataset("ADSL")


 adsl1 <- create_cat_var(data=adsl,
                         metacore=specs,
                         ref_var = AGE,
                         grp_var = AGEGR1,
                         num_grp_var = AGEGR1N)

 adsl3 <- order_cols(adsl1, specs)
 adsl4 <- xportr_label(adsl3, specs)
 View(adsl4)

 adsl2 <- create_var_from_codelist(data = adsl1, metacore = specs, input_var = RACE, out_var = RACEN,decode_to_code=TRUE)
 adsl2 <- create_var_from_codelist(data = adsl2, metacore = specs, input_var = ARM, out_var = ARMN
,decode_to_code=TRUE)

 View(adsl2)

 check_variables( metacore, "ADSL4")


For ARMN=0 or 1: CUMDOSE=TRT01PN*TRTDUR. --- For ARMN=2: CUMDOSE will be based on 54mg per day for the
# of days subj was in 1st dosing interval (i.e., visit4date-TRTSTDT+1 if 1st interval completed, TRTEDT-TRTSTDT+1
if subj discontinued <=visit 4 and > visit 3), 81mg per day for the # of days subj was in 2nd dosing interval
(i.e., visit12date-visit4date if 2nd interval completed, TRTEDT-visit4date
  if subj discontinued <= visit 12 and > visit 4), and 54mg per day for the
# of days subj was in 3rd dosing interval (i.e., TRTEDT - visit12date if subj continued after visit 12).










q <- qs1 %>%
  filter(QSCAT == "MINI-MENTAL STATE") %>%
  distinct(USUBJID)


View(q)

 d <- as.numeric(c)
   mutate(MMSETOT = sum(QSORRES))

 View(qs)


ds2 <- ds1 %>%
  filter(DSCAT == "DISPOSITION EVENT")
View(ds2)

  mutate(VISNUMEN = case_when(DSTERM == "PROTCOL COMPLETED" & VISITNUM == 13|13.0  ~ 12))

                              VISITNUM))



ds3 <- ds1 %>%
   mutate(VISNUMEN = if_else(DSTERM == "PROTOCOL COMPLETED" & VISIT == "WEEK 26", 12, VISITNUM ))
View(ds3)


if DS.VISITNUM=13 where DSTERM='PROTCOL COMPLETED' then VISNUMEN=12,
otherwise VISNUMEN=DS.VISITNUM where DSTERM='PROTCOL COMPLETED'












View(trte)

class(trte$TRTSSDT)


  derive_var_trtdurd(start_date = TRTSDT, end_date = TRTEDT)

View(trte)

tr <-
  derive_extreme_records(dm1,
                         ex1,
                        by_vars =vars(USUBJID),
                        order  = vars(USUBJID, VISITNUM),
                        mode = "last",
                        set_values_to = vars(
                          DTYPE = "MINIMUM"))
View(tr)


ex1 %>% dim()
ex1 %>% unique() %>% dim()
ex1 %>% group_by(USUBJID) %>% unique() %>% dim()
get_duplicates_dataset()

g

get_duplicates_dataset()

v <- derive_vars_merged(dm1,
  dataset_add = ex1,
  filter_add = !is.na(EXENDTC),
  by_vars = vars(STUDYID, USUBJID),
  new_vars = vars(TRTEDTM = EXENDTC),
  order = vars(EXENDTC),
  mode = "last"
)

View(v)

get_duplicates_dataset()

ext <- ex1 %>%
  derive_vars_dt(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    highest_imputation = "M"
  )

View(ext)

exd <- date_source(dataset_name = "ext", date = EXENDT)

ae_end <- date_source(
  dataset_name = "ex",
  date = convert_dtc_to_dt(EXENDTC, highest_imputation = "M")
)


trte <- dm1 %>%
  derive_var_extreme_dt(new_var = TRTEDT,
                        exd,
                              source_datasets = list(),
                        mode = "last",
                              subject_keys = vars(USUBJID))


View(adsl4)

adsl2 <- sv1 %>%
  filter(VISITNUM == 3.0) %>%
  mutate(TRTSDT = SVSTDTC) %>%
  select(USUBJID, TRTSDT)

adsl3 <- full_join(adsl1, adsl2, by = "USUBJID")
View(adsl3)



install.packages(c("usethis", "credentials"))

## set your user name and email:



usethis::use_git_config(user.name = "janardhanswami", user.email = "saijanardhanswami@mail.com")
#### 1. Sign up at GitHub.com ################################################

## If you do not have a GitHub account, sign up here:
## https://github.com/join

# ----------------------------------------------------------------------------

### 2. Configure git with Rstudio ############################################
install.packages(c("usethis", "credentials"))

## set your user name and email:
usethis::use_git_config(user.name = "janardhanswami", user.email = "saijanardhanswami@mail.com")

# ----------------------------------------------------------------------------

### 3. Configure github with Rstudio ############################################

## create a personal access token for authentication:
usethis::create_github_token() 




## set personal access token:
credentials::set_github_pat()


# ----------------------------------------------------------------------------

#### 4. Restart R! ###########################################################

# ----------------------------------------------------------------------------

#### 5. Verify settings ######################################################

usethis::git_sitrep()

## Your username and email should be stated correctly in the output. 
## Also, the report should contain something like:
## 'Personal access token: '<found in env var>''

## If you are still having troubles, read the output carefully.
## It might be that the PAT is still not updated in your `.Renviron` file.
## Call `usethis::edit_r_environ()` to update that file manually.

# ----------------------------------------------------------------------------

# SOURCE: https://gist.github.com/Z3tt/3dab3535007acf108391649766409421




