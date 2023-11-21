#' Get ability-input files (input into TAM script) from IBE server
#'
#' @param check Check type, i.e. P3, P5, S2 or S3
#' @param year Year of check
#' @param domain Scale/test, e.g. "dles". "math" is vaild, but "mzuv", "mfur", & "mgfd" are *not* valid.
#' @param selection If several files are found, choose either the newest (date) or the last among sorted file names.
#' @param check_type `"P"` for primary school or `"S"` for secondary school, by default consistent with `check`
#' @param spec_regex The function constructs a regular expression to find the correct file(s). Provide your own regular expression here, to overrule.
#' @param read_data If TRUE (default), use readRDS and return file content; if FALSE, return file path.
#' @param avoid.nz Avoids Nachzügler files by excluding file names that contain "NZ"
#' @param avoid.res Avoids Forschung files by excluding file names that contain "forsch" or "rese". If FALSE, research files are preferred!
#' @param hybrid Get ability data from paper-pencil ("paper") or online ("online") test? Only relevant for Check P5 as of 2023.
#'
#' @return If `read_data` is `TRUE`, the content of the Rds-file as a data frame, else the full path to the Rds-file.
#' @export
#'
#' @examples

get_abil_input_file <- function(check, year, domain,
                                selection = c("date", "name"),
                                check_type = substr(toupper(check), 1, 1),
                                spec_regex = NULL, read_data = TRUE,
                                avoid.nz = TRUE, avoid.res = TRUE, hybrid = NULL)
{
  check <- toupper(check)
  year <- as.numeric(year)
  domain <- tolower(domain)
  is_schreib <- substr(domain, 2, 4) == "sch"
  
  # check validity of arguments
  valid_checks <- c("P3", "P5", "S2", "S3")
  if (! check %in% valid_checks) stop("Check must be one of the following:\n", paste(valid_checks, collapse=", ") )
  if (! check_type %in% c("P", "S")) stop("Only check types P and S are implemented")
  if (! year %in% 2013:2099) stop(year," is not a valid year.")
  if (is.null(hybrid)) {
    if (check_type == "P" & year >= 2023 & !grepl("sch", domain)) stop("`hybrid` must be specified for P5 Checks in 2023 or later.")
  } else {
    if (! hybrid[1] %in% c("paper", "online", "computer")) {
      stop("`hybrid` must be 'paper' or 'online'")
    } else {
      if (hybrid == "computer") hybrid <- "online"
      if (! (check == "P5" & year >= 2023)) message("Argument `hybrid` will be ignored because ",check_type," ",year," is not a hybrid check.")
    }
  }
    
  valid_domains <- c("dles", "dsif", "dsch",
                     "eles", "ehoe", "esch",
                     "fles", "fhoe", "fsch",
                     "math", "natw")
  if (! domain %in% valid_domains) stop("Domain must be one of the following abbreviations:\n", paste(valid_domains, collapse=", ") )
  
  # specify server path
  if (dir.exists("//ibe-srv01/IBE_Projekte")) srvpath <- "//ibe-srv01/" else if (dir.exists("//192.168.1.101/IBE_Projekte")) srvpath <- "//192.168.1.101/"
  
  if (check_type == "P") {
    
    # make path to abils directory
    if (check == "P5" & as.numeric(year) >= 2023) { # no subjectfolders for hybrid P5-checks, so subjectfolder==domain
      subjfolder <- ifelse(substr(domain,1,1)=="m", "math", domain)
    } else {
      subjfolder <- switch(substr(domain, 1, 1), d = "Deutsch", e = "Englisch", f = "Franzoesisch", m = c("Mathematik", "Mathe"), n = "Natw")
      if (is_schreib) subjfolder <- paste0(subjfolder, "_Schreiben")
    }
    dirpath <- paste0(srvpath,"IBE_Projekte/Checks/Checks_",year,"/Check_",check,"/Auswertung/",subjfolder,"/Daten")
    if (length(dirpath) > 1) dirpath[which(sapply(dirpath, dir.exists))]
    
    # make file name regex
    year_short <- substr(as.character(year), 3, 4)
    if (is.null(spec_regex)) {
      if (is_schreib) {
        file_rx <- paste0("((P|p)",substr(check,2,2),"_",domain,"|",domain,"_(P|p)",substr(check,2,2),")_",year,"_TAM")
      } else {
        file_rx <- paste0(domain,"_(P|p)",substr(check,2,2),"_","[[:digit:]]{2}",year_short,"_abils")
        if (year >= 2023 & hybrid == "paper" & avoid.res) file_rx <- sub("abils", "?(paper)_abils", file_rx) # select paper vs online abils input file for hybrid P5-checks
        if (year >= 2023 & hybrid == "online") file_rx <- sub("abils", "ABILS", file_rx)
      }
    } else file_rx <- spec_regex
    
  } else if (check_type == "S") {
    
    # make path to abils directory
    dirpath <- paste0(srvpath,"IBE_Projekte/Checks/Checks_",year,"/Check_S2_S3/Auswertung/Check_",check,"/",domain,"/Daten")
    
    # make file name regex
    if (is.null(spec_regex)) {
      if (is_schreib) {
        file_rx <- paste0("((S|s)",substr(check,2,2),"_",domain,"|",domain,"_(S|s)",substr(check,2,2),")_",year,"_TAM")
      } else {
        file_rx <- paste0(domain,"_",check,"_",year,"_(ABILS|abils)")
      }
    } else file_rx <- spec_regex
    
  }
  
  # read file list and select newest according to "selection"
  files <- file.info(list.files(path = dirpath, pattern = file_rx, full.names = TRUE))
  
  if (nrow(files) == 0) {
    message("ERROR: No abils file found for Check ",check," ",year," in ",domain,
            "\nPath: ",dirpath,
            "\nFile pattern: ",file_rx)
    return()
  }
  
  afile <- rownames(files)[order(switch(selection[1],
                                        date = files$mtime,
                                        name = rownames(files)),
                                 decreasing = TRUE)]
  if (avoid.nz) afile <- afile[!grepl("NZ", afile, ignore.case=FALSE)] # do not use ignore.case=TRUE here! (will catch "FraNZösisch")
  if (avoid.res) afile <- afile[!grepl("(forsch|rese)", afile, ignore.case = TRUE)]
  if (!avoid.res & any(grepl("(forsch|rese)", afile, ignore.case = TRUE))) afile <- grep("(forsch|rese)", afile, ignore.case = TRUE, value=TRUE)
  
  # if newest is not RDS, take next same-named file, if that does not work, exit with error message
  repeat {
    read <- tryCatch(readRDS(afile[1]), error=function(x) NULL)
    if (!is.null(read)) break
    topfiles <- sub("\\.[[:alpha:]]{3,5}$", "", afile[1:2])
    if (sum(topfiles[1]==topfiles[2], na.rm=TRUE)==1) { #sum avoids error in case of NA
      afile <- afile[-1]
    } else {
      message("\nERROR: File for Check ",check," ",year," in ",domain," not in valid format.",
              "\nFile:",afile[1],
              "\nPath: ",dirpath,
              "\nFile pattern: ",file_rx)
      read <- NULL
      break
    }
  }
  
  # return data or path according to read_data argument
  if (read_data) {
    message("\nReading file: ", sub("^/", "", gsub("/.+/", "", afile[1])) )
    return(read)
  } else return(afile[1])
  
}
