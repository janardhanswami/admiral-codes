

ae1 <- read_xpt("sdtm//ae.xpt")
adsl1 <- read_xpt("adam//adsl.xpt")

View(ads_ae)
View(adsl1)

#merging with the ADSL dataset.
ads_ae1 <- derive_vars_merged(ae1, adsl1, by_vars= vars(USUBJID, STUDYID))


#deriving ASTDT and AENDT variables by applying the imputation rule

asedt <- derive_vars_dt(ads_ae1, new_vars_prefix = "AST",dtc = AESTDTC, highest_imputation = "M",
                        date_imputation = "first",flag_imputation ="auto" ) %>%

derive_vars_dt( new_vars_prefix="AEN", dtc=AEENDTC, highest_imputation = "M",
                       date_imputation = "first",flag_imputation = "auto") %>%

#deriving treatment emergent flag (trtemfl)

 derive_var_trtemfl(new_var= TRTEMFL, start_date = ASTDT, end_date = AENDT, trt_start_date = TRTSDT) %>%
  mutate(AENDY = case_when(AENDT>=TRTSDT  ~ AENDT-TRTSDT+1,
                           TRTSDT > AENDT ~ AENDT-TRTSDT),
         ASTDY = case_when(ASTDT>=TRTSDT  ~ ASTDT-TRTSDT+1,
                           TRTSDT > ASTDT ~ ASTDT-TRTSDT)) %>%
  derive_vars_duration( new_var = AEDUR, new_var_unit = AEDURU, start_date = ASTDT,
                                      end_date = AENDT, out_unit="days") %>%

  select(STUDYID, SITEID, USUBJID,  AGE,  RACE, SEX,SAFFL, RFSTDTC, RFENDTC,
        TRTSDT,TRTEDT,ASTDT,AENDT,TRTEMFL,  AESER, AEDECOD, AEBODSYS,AENDY, AESEQ, ASTDY, AEDUR, AEDURU,
        AETERM, AELLT,AELLTCD,AEPTCD,AEHLT,AEHLTCD,AEHLGT, AEHLGTCD, AESOC, AESOCCD, AESEV, AESCAN, AESCONG,
        AESDISAB, AESDTH, AESHOSP, AESLIFE, AESOD, AEREL, AEACN, AEOUT
  ) %>%

  #deriving AOCCFL flag
    restrict_derivation(
      derivation = derive_var_extreme_flag,
      args = params(
        by_vars = vars(USUBJID),
        order = vars(ASTDT, AESEQ),
        new_var = AOCCFL,
        mode = "first"
      ),
      filter = TRTEMFL == "Y"
    ) %>%

restrict_derivation(derivation = derive_var_extreme_flag,
                    args = params(
                      by_vars = vars(USUBJID, AEDECOD),
                      order = vars(AEBODSYS, AEDECOD,ASTDT, AESEQ),
                      new_var = AOCCPFL,
                      mode = "first"
                    ),
                    filter = TRTEMFL == "Y") %>%

restrict_derivation(derivation = derive_var_extreme_flag,
                    args = params(
                      by_vars = vars(USUBJID, AEBODSYS),
                      order = vars(AEDECOD, ASTDT, AESEQ),
                      new_var = AOCCSFL,
                      mode = "first"
                    ),
                    filter = TRTEMFL == "Y") %>%
  restrict_derivation(derivation = derive_var_extreme_flag,
                      args  =params(
                        by_vars = vars(USUBJID),
                        order = vars(ASTDT, AESEQ),
                        new_var = AOCCO2FL,
                        mode = "first"
                      ),
                      filter = TRTEMFL =="Y" & AESER == "Y") %>%
  restrict_derivation(derivation = derive_var_extreme_flag,
                      args = params(
                        by_vars = vars(USUBJID, AEBODSYS),
                        order = vars(ASTDT, AESEQ),
                        new_var = AOCCO3FL,
                        mode ="first"),
                      filter = TRTEMFL == "Y" & AESER == "Y"
                      ) %>%
  restrict_derivation(derivation = derive_var_extreme_flag,
                      args = params(by_vars =vars(USUBJID, AEBODSYS, AEDECOD),
                                    order = vars(ASTDT, AESEQ),
                                    new_var = AOCCO4FL,
                                    mode = "first"),
                      filter = TRTEMFL == "Y" & AESER == "Y") %>%
  mutate(CQ01NAM = "") %>%
  restrict_derivation(derivation = derive_var_extreme_flag,
                      args = params(
                      by_vars = vars(USUBJID, CQ01NAM),
                      order = vars(USUBJID, ASTDT, AESEQ),
                      new_var = AOCCO1FL,
                      mode = "first"),
                      filter = TRTEMFL == "Y" & CQ01NAM == "")



#deriving metadata using metacore and metatools package.



metacore <- spec_to_metacore("metadata//specs.xlsx", where_sep_sheet = F, quiet = T)

specs <- metacore %>%
  select_dataset("ADAE")

ADAE <- xportr_label(asedt, specs)
View(ADAE)


