#' Gets canton abbreviation or name from HID as used in Checks/Mindsteps-system
#'
#' @param hid A vector of HIDs (canton, school, class, or student)
#' @param full.name Return full name of canton or abbreviation (e.g., ZH)? Default is abbreviation
#'
#' @return A character vector of same length as `hid`.
#' 
#' @export
hid2canton <- function(hid, full.name=FALSE) {
  dd <- data.frame(hid = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13",
                           "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26",
                           "32"),
                   canton1 = c("Aargau", "Appenzell Innerrhoden", "Appenzell Ausserrhoden", "Basel-Stadt", "Basel-Landschaft", "Bern",
                               "Fribourg", "Geneva", "Glarus", "Graubünden", "Jura", "Lucerne", "Neuchâtel", "Nidwalden", "Obwalden",
                               "Schaffhausen", "Schwyz", "Solothurn", "St. Gallen", "Thurgau", "Ticino", "Uri", "Valais", "Vaud", "Zug",
                               "Zurich", "Fürstentum Liechtenstein"),
                   canton2 = c("AG", "AI", "AR", "BS", "BL", "BE", "FR", "GE", "GL", "GR", "JU", "LU", "NE",
                               "NW", "OW", "SH", "SZ", "SO", "SG", "TG", "TI", "UR", "VS", "VD", "ZG", "ZH",
                               "FL")
                   )
  hid <- substring(as.character(hid), 1, 2)
  hid <- ifelse(nchar(hid)==2, hid, paste0("0",hid))
  if(!all(hid %in% dd$hid)) message("WARNING: Invalid HIDs found!")
  sapply(hid,
         function(x, ct, hid) ifelse(x %in% hid, ct[which(hid==x)], NA_character_),
         hid = dd$hid,
         ct = if(full.name) dd$canton1 else dd$canton2,
         USE.NAMES = FALSE)
}
