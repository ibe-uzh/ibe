#' Get ability-output files (input into TAM script) from IBE server
#'
#' @param check Check type, i.e. P3, P5, S2 or S3
#' @param year Year of check
#' @param domain Scale/test, e.g. "dles". "math" is *not* vaild, but "mzuv", "mfur", & "mgfd" are valid.
#' @param selection If several files are found, choose either the newest (date) or the last among sorted file names.
#' @param check_type `"P"` for primary school or `"S"` for secondary school, by default consistent with `check`
#' @param spec_regex The function constructs a regular expression to find the correct file(s). Provide your own regular expression here, to overrule.
#' @param read_data If TRUE (default), use readRDS and return file content; if FALSE, return file path.
#' @param avoid.nz Avoids Nachzügler files by excluding file names that contain "NZ"
#'
#' @return If `read_data` is `TRUE`, the content of the Rds-file as a data frame, else the full path to the Rds-file.
#' 
#' @examples
#' get_abil_output_file("P3", 2021, "mzuv", read_data = FALSE)
#' head(get_abil_output_file("P3", 2021, "mzuv"))
#' 
#' @export
get_abil_output_file <- function(check, year, domain,
                                 selection = c("date", "name"),
                                 check_type = substr(toupper(check), 1, 1),
                                 spec_regex = NULL, read_data = TRUE,
                                 avoid.nz = TRUE, kDrive = FALSE)
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
  valid_domains <- c("dles", "dsif", "dsch",
                     "eles", "ehoe", "esch",
                     "fles", "fhoe", "fsch",
                     "mzuv", "mgfd", "mfur", "natw")
  if (! domain %in% valid_domains) stop("Domain must be one of the following abbreviations:\n", paste(valid_domains, collapse=", ") )
  
  # specify server or kDrive path
  if (kDrive) {
    kdrivepath <- paste0("C:/Users/",Sys.info()[["user"]],"/kDrive/Common documents/")
    if (dir.exists(kdrivepath)) {
      srvpath <- kdrivepath
      rm(kdrivepath)
    } else stop("kDrive directory (",kdrivepath,") not found!")
  } else {
    if (dir.exists("//ibe-srv01/IBE_Projekte")) {
      srvpath <- "//ibe-srv01/"
    } else if (dir.exists("//192.168.1.101/IBE_Projekte")) {
      srvpath <- "//192.168.1.101/"
    } else stop("ibe-srv01 (192.168.1.101) not accessible!")
  }
  
  if (check_type == "P") {
    
    # make path to abils directory
    if (check == "P5" & as.numeric(year) >= 2023 ) { # no subjectfolders for hybrid P5-checks, so subjectfolder==domain
      subjfolder <- ifelse(substr(domain,1,1)=="m", "math", domain)
    } else {
      subjfolder <- switch(substr(domain, 1, 1), d = "Deutsch", e = "Englisch", f = "Franzoesisch", m = c("Mathematik", "Mathe"), n = "Natw")
      if (is_schreib) subjfolder <- paste0(subjfolder, "_Schreiben")
    }
    dirpath <- paste0(srvpath,"IBE_Projekte/Checks/Checks_",year,"/Check_",check,"/Auswertung/",subjfolder,"/Daten")
    if (length(dirpath) > 1) dirpath[which(sapply(dirpath, dir.exists))]
    
    # make file name regex
    if (is.null(spec_regex)) {
      if (subjfolder[1] %in% c("Mathematik", "math") ) {
        file_rx <- paste0("math_(P|p)",substr(check,2,2),"_",year,"_Abils_",domain)
      } else {
        file_rx <- paste0(domain,"_(P|p)",substr(check,2,2),"_",year,"_Abils")
      }
    } else file_rx <- spec_regex
    
  } else if (check_type == "S") {
    
    # make path to abils directory
    dirpath <- paste0(srvpath,"IBE_Projekte/Checks/Checks_",year,"/Check_S2_S3/Auswertung/Check_",check,"/",domain2,"/Daten")
    
    # make file name regex
    if (is.null(spec_regex)) {
      if (substring(domain, 1, 1) == "m" & year < 2024) { # slightly different naming scheme when using new abils script
        file_rx <- paste0("math_(S|s)",substr(check,2,2),"_",year,"_Abils_",domain)
      } else {
        file_rx <- paste0(domain,"_(S|s)",substr(check,2,2),"_",year,"_Abils")
      }
    } else file_rx <- spec_regex
    
  }
  
  # read file list and sort according to "selection" argument
  files <- file.info(list.files(path = dirpath, pattern = file_rx, full.names = TRUE))
  
  if (nrow(files) == 0) {
    message("\nERROR: No abils file found for Check ",check," ",year," in ",domain,
            "\nPath: ",dirpath,
            "\nFile pattern: ",file_rx)
    return()
  }
  
  afile <- rownames(files)[order(switch(selection[1],
                                        date = files$mtime,
                                        name = rownames(files)),
                                 decreasing = TRUE)]
  if (avoid.nz) afile <- afile[!grepl("NZ", rownames(files))]
  
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
  if (read_data & !is.null(read)) {
    message("\nReading file: ", sub("^/", "", gsub("/.+/", "", afile[1])) )
    return(read)
  } else return(afile[1])

}


