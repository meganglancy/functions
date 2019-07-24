# Health board name recode

hb_recode <- function(hb_in, recode_to = "long", and_choice = "and") {
  
  # Default: "long"
  # Can also use recode_to = "short"
  
  hb_in <- stringr::str_trim(toupper(hb_in), "both")
  
  if (recode_to == "short") {
    hb_out <- dplyr::case_when(
      hb_in %in% c("S08000001", "S08000015", "AA", "AYRSHIRE AND ARRAN", "AYRSHIRE & ARRAN") ~ "AA", 
      hb_in %in% c("S08000002", "S08000016", "BR", "BOR", "BORDERS") ~ "BR",
      hb_in %in% c("S08000003", "S08000017", "DG", "DUMFRIES AND GALLOWAY", "DUMFRIES & GALLOWAY") ~ "DG",
      hb_in %in% c("S08000004", "S08000018", "S08000029", "FF", "FIFE") ~ "FF",
      hb_in %in% c("S08000005", "S08000019", "FV", "FORTH VALLEY") ~ "FV",
      hb_in %in% c("S08000007", "S08000021", "S08000031", "GGC", "GC", "GGHB", "GREATER GLASGOW AND CLYDE", "GREATER GLASGOW & CLYDE") ~ "GC",
      hb_in %in% c("S08000006", "S08000020", "GR", "GRA", "GRAMPIAN") ~ "GR",
      hb_in %in% c("S08000008", "S08000022", "HG", "HIG", "HIGHLAND", "HIGHLANDS") ~ "HG",
      hb_in %in% c("S08000009", "S08000023", "S08000032", "LN", "LAN", "LANARKSHIRE") ~ "LN",
      hb_in %in% c("S08000010", "S08000024", "LO", "LOT","LOTHIAN") ~ "LO",
      hb_in %in% c("S08000011", "S08000025", "OR", "ORK","ORKNEY") ~ "OR",
      hb_in %in% c("S08000012", "S08000026", "SH", "SHT", "SHETLAND") ~ "SH",
      hb_in %in% c("S08000013", "S08000027", "S08000030", "TY", "TAY", "TAYSIDE") ~ "TY",
      hb_in %in% c("S08000014", "S08000028", "WI", "WESTERN ISLES") ~ "WI",
      TRUE ~ hb_in)
    
  }
  else {
    hb_out <- dplyr::case_when(
      hb_in %in% c("S08000001", "S08000015", "AA", "AYRSHIRE AND ARRAN", "AYRSHIRE & ARRAN") ~ "Ayrshire and Arran", 
      hb_in %in% c("S08000002", "S08000016", "BR", "BOR", "BORDERS") ~ "Borders",
      hb_in %in% c("S08000003", "S08000017", "DG", "DUMFRIES AND GALLOWAY", "DUMFRIES & GALLOWAY") ~ "Dumfries and Galloway",
      hb_in %in% c("S08000004", "S08000018", "S08000029", "FF", "FIFE") ~ "Fife",
      hb_in %in% c("S08000005", "S08000019", "FV", "FORTH VALLEY") ~ "Forth Valley",
      hb_in %in% c("S08000007", "S08000021", "S08000031", "GGC", "GC", "GGHB", "GREATER GLASGOW AND CLYDE", "GREATER GLASGOW & CLYDE") ~ "Greater Glasgow and Clyde",
      hb_in %in% c("S08000006", "S08000020", "GR", "GRA", "GRAMPIAN") ~ "Grampian",
      hb_in %in% c("S08000008", "S08000022", "HG", "HIG", "HIGHLAND", "HIGHLANDS") ~ "Highland",
      hb_in %in% c("S08000009", "S08000023", "S08000032", "LN", "LAN", "LANARKSHIRE") ~ "Lanarkshire",
      hb_in %in% c("S08000010", "S08000024", "LO", "LOT","LOTHIAN") ~ "Lothian",
      hb_in %in% c("S08000011", "S08000025", "OR", "ORK","ORKNEY") ~ "Orkney",
      hb_in %in% c("S08000012", "S08000026", "SH", "SHT", "SHETLAND") ~ "Shetland",
      hb_in %in% c("S08000013", "S08000027", "S08000030", "TY", "TAY", "TAYSIDE") ~ "Tayside",
      hb_in %in% c("S08000014", "S08000028", "WI", "WESTERN ISLES") ~ "Western Isles",
      TRUE ~ hb_in) %>% 
      stringr::str_replace_all(., "(\\&|\\band\\b)", and_choice)
  }
  
  return(hb_out)
  
}

