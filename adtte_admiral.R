

rfendt <- ADAE %>%
  derive_vars_dt(new_vars_prefix = "RFEN",
                 dtc = RFENDTC) %>%
  mutate(ADT = if_else(!is.na(ASTDT) & ASTDT> TRTSDT, ASTDT, RFENDT),
         CNSR = if_else(TRTEMFL == "Y", 0, 1),
         EVNTDESC = if_else(CNSR == 0, "Dematologic Event Occured", "STUDY COMPLETION"),
         PARAM =  "Time to First Dermatologic Event",
         PARAMCD = "TTDE",
         SRCDOM = "ADAE",
         SRCSEQ = if_else(SRCDOM == "ADAE", AESEQ, NA_real_),
         SRCVAR = if_else(ASTDT>TRTSDT, "ASTDT", "RFENDT"))

View(rfendt)
