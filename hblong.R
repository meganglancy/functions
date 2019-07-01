hblong <- function(hbcode, and = "and") {
  hbcode <- stringr::str_trim(toupper(hbcode), "both")
  hb_long_code <- dplyr::case_when(
    hbcode %in% c("S08000001", "S08000015", "AA") ~ "Ayrshire and Arran", 
    hbcode %in% c("S08000002", "S08000016", "BR", "BOR") ~ "Borders",
    hbcode %in% c("S08000003", "S08000017", "DG") ~ "Dumfries and Galloway",
    hbcode %in% c("S08000004", "S08000018", "S08000029", "FF") ~ "Fife",
    hbcode %in% c("S08000005", "S08000019", "FV") ~ "Forth Valley",
    hbcode %in% c("S08000007", "S08000021", "S08000031", "GGC", "GC", "GGHB") ~ "Greater Glasgow and Clyde",
    hbcode %in% c("S08000006", "S08000020", "GR", "GRA") ~ "Grampian",
    hbcode %in% c("S08000008", "S08000022", "HG", "HIG") ~ "Highlands",
    hbcode %in% c("S08000009", "S08000023", "S08000032", "LN", "LAN") ~ "Lanarkshire",
    hbcode %in% c("S08000010", "S08000024", "LO", "LOT") ~ "Lothian",
    hbcode %in% c("S08000011", "S08000025", "OR", "ORK") ~ "Orkney",
    hbcode %in% c("S08000012", "S08000026", "SH", "SHT") ~ "Shetland",
    hbcode %in% c("S08000013", "S08000027", "S08000030", "TY", "TAY") ~ "Tayside",
    hbcode %in% c("S08000014", "S08000028", "WI") ~ "Western Isles",
    TRUE ~ hbcode) %>% 
    stringr::str_replace_all(., "(\\&|\\band\\b)", and)
  return(hb_long_code)
}
