


vs1 <- read_xpt("sdtm//vs.xpt")
adsl1 <- read_xpt("adam//adsl.xpt")
View(vs)


#merging VS dataset with ADSL dataset

adsl_vs <- derive_vars_merged(vs1, adsl1, by_vars = vars(USUBJID, STUDYID))



#Deriving variables needed for ADVS as per spec

advs1 <- adsl_vs %>%
  mutate(ABLFL = VSBLFL,
       ADY = VSDY,
       ATPT = VSTPT,
       ABLFL = VSBLFL,
       PARAMCD = VSTESTCD,
       PARAM = VSTEST,
       ADY = VSDY,
       AVAL = VSSTRESN,
       BASETYPE = VSTPT) %>%
  mutate(ATPTN = case_when(VSTPT == "AFTER LYING DOWN FOR 5 MINUTES" ~ 1,
                         VSTPT == "AFTER STANDING FOR 1 MINUTE" ~ 2,
                         VSTPT == "AFTER STANDING FOR 1 MINUTE" ~ 3),
         AVISIT =  if_else(VISIT != "SCREENING 1" & VISIT != "SCREENING 2" &
                             VISIT != "AMBUL ECG PLACEMENT" &
                             VISIT != "UNSCHEDULED 3.1" & VISIT != "AMBUL ECG REMOVAL", VISIT, "")) %>%
  restrict_derivation(derivation = derive_var_extreme_flag,
                      args = params(
                        by_vars =  vars( USUBJID, VISITNUM),
                        order = vars(VSTEST,VSSEQ),
                        mode = "first",
                        new_var = ANL01FL),
                      filter = VISIT != "SCREENING 1" & VISIT != "SCREENING 2" &
                        VISIT != "AMBUL ECG PLACEMENT" & VISIT != "UNSCHEDULED 3.1" & VISIT != "AMBUL ECG REMOVAL") %>%


  derive_vars_dt(new_vars_prefix = "A", dtc= VSDTC ) %>%
    mutate(AVAL= VSSTRESN) %>%
  derive_var_base(
    by_vars = vars(USUBJID, VSSEQ),
    source_var = AVAL,
    new_var = BASE,
    filter = VSBLFL == "Y") %>%
  derive_var_chg() %>%
  derive_var_pchg() %>%
  select(STUDYID,SITEID,USUBJID,AGE, RACE,SEX,SAFFL,TRTSDT,TRTEDT,PARAMCD,
         PARAM, ADT, ADY,ATPTN,ATPT,AVISIT,AVAL,BASE,BASETYPE,CHG, PCHG, VISITNUM,VISIT,VSSEQ,ANL01FL,ABLFL
  )


#derive vars based on the code list
metacore <- spec_to_metacore("metadata//specs.xlsx", where_sep_sheet = F, quiet = T)

specs <- metacore %>%
  select_dataset("ADVS")

ADVS <- xportr_label(avds3,specs)

avds3 <- create_var_from_codelist(data = advs1, metacore = specs,
                                  input_var = PARAMCD_ADVS, out_var = PARAMN,
                                  decode_to_code = FALSE)



View(specs)

?create_var_from_codelist()




View(avds3)

unique(vs1$VSTPT)

#Deriving ANL01FL variable




View(anl01fl)
