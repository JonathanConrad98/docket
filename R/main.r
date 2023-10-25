#' @import zip
#' @import stringr
#' @import XML
#' @import xml2

##
##Pre-importation quality check
##
check_file <- function(filename){
  valid_filetypes <- c(".doc", ".docx") #supported file types

  #Check if the file is a valid type
  if (!any(str_ends(filename, valid_filetypes))) {
    warning("File type not supported. Please confirm that file is .doc or .docx")
    return(FALSE)
  }

  #Check if it exists
  if (!file.exists(filename)) {
    warning("file not found")
    return(FALSE)
  }

  #Check if the temp file already exists
  if (file.exists(paste0(filename, "_dockettemp"))) {
    close_unzip_file(filename)
    if (file.exists(paste0(filename, "_dockettemp"))) {
      warning(paste("ERROR: Temp file for document generation already exists:", paste0(filename, "_dockettemp")))
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  return(TRUE)
}

##
##Clean data to remove any non-basic Latin characters from directly adjacent to flag
##
removeNonLatin_Leading <- function(input_data, targetChar) {
  char_check <- regmatches(input_data, gregexpr(paste0(".", targetChar), input_data)) #get all instances of start flag and immediate adjacent char
  char_check <- unique(unlist(char_check)) #unique list of flag-char combinations
  char_check <- char_check[grepl("^[\x01-\x7F]", substr(char_check, 1, 1)) == FALSE] #Issue characters
  output_data <- input_data

  if (length(char_check) > 0) {
    for (i in 1:length(char_check)){
      output_data <- str_replace_all(output_data, char_check[i], targetChar)
    }
    output_data <- removeNonLatin_Leading(output_data, targetChar)
  }
  return(output_data)
}


##
##Create temporary hold file for the data
#
unzip_file <- function(filename){
  if (check_file(filename) != TRUE) {
    stop()
    return(FALSE)
  }

  temp_dir <- paste0(filename, "_dockettemp") #Temporary directory for the unzipped content

  dir.create(path = temp_dir)

  if (!file.exists(temp_dir)) {
    stop("Unable to generate temporary holding files")
    return(FALSE)
  }

  zip::unzip(filename, exdir = temp_dir)
  return(TRUE)
}

##
##Deletes the temporary hold file
##
close_unzip_file <- function(filename) {
  temp_dir <- paste0(filename, "_dockettemp")
  unlink(temp_dir, recursive = TRUE)

  if (file.exists(temp_dir) == TRUE){
    warning(paste("Couldn't delete file at", temp_dir))
  }
}

##
##Gets the raw XML data
##
open_zipfile <- function(filename) {
  unzip_file(filename)
  temp_dir <- paste0(filename, "_dockettemp")
  temp_dir_xml <- paste0(filename, "_dockettemp/word/document.xml")

  if (!file.exists(temp_dir_xml)) {
    close_unzip_file(temp_dir)
    stop(paste0("Could not find"), temp_dir_xml)
  }

  docket_template <- XML::toString.XMLNode(XML::xmlParse(file = temp_dir_xml))

  #Can be optimized by only being called if needed
  docket_template <- removeNonLatin_Leading(docket_template, "\u00AB")
  docket_template <- removeNonLatin_Leading(docket_template, "\u00BB")
  return(docket_template)
}

##
##Gets the raw XML holding the flags in Word
##
get_docket_xml <- function(content_extract) {
  matches <- regmatches(content_extract, gregexpr("\u00AB(.*?)\u00BB", content_extract))
  extracted_flags_raw <- unlist(matches)
  return(extracted_flags_raw)
}

##
##Returns the flags after removing XML elements
##
get_flags <- function(content_extract) {
  extracted_flags_raw <- get_docket_xml(content_extract)
  cleaned_string <- gsub("<[^>]*>", "", extracted_flags_raw)
  cleaned_string <- gsub("\n", "", cleaned_string)
  cleaned_string <- gsub(" ", "", cleaned_string)
  return(cleaned_string)
}

##
##Return only the essential parts of the XML flag
##
get_flags_raw <- function(content_extract) {
  extracted_flags_raw <- get_docket_xml(content_extract)
  matches <- regmatches(extracted_flags_raw, gregexpr("<[^>]+>|\u00AB|\n", extracted_flags_raw))[[1]]
  return(matches)
}

#' Create an empty dictionary from the flags in a document template
#'
#' @description Scans the input file for strings enclosed by flag wings: « »
#'
#' @param filename The file path to the document template. Supports .doc and .docx
#' @return A two-column data frame intended for populating data into the template:
#' \itemize{
#'   \item \strong{flag}: Lists all the flags identified in the document.
#'   \item \strong{replace values}: An empty column where users can insert values to replace the corresponding flags in the template.
#' }
#' @export
getDictionary <- function(filename) {
  content_extract <- open_zipfile(filename) #open the document zip file
  flag <- get_flags(content_extract) #Get the cleaned flags
  close_unzip_file(filename) #Close the document zip file

  #Create a dataframe of the unique flags
  docket.dictionary.public <- data.frame('flag' = unique(flag),
                                         'replace values' = rep(NA,length(unique(flag))))

  return(docket.dictionary.public)
}


#' Check if dictionary meets specific requirements.
#'
#' @description Verifies that the input dictionary meets the following conditions
#' 1. It is a two-column dataframe
#' 2. Column 1 is named "flag"
#' 3. Column 1 contains flags without starting and ending wings: « »
#'
#' @param dictionary A data frame intended for feeding into the template. It should be structured according to the described conditions
#' @return Logical. Returns 'TRUE' if the dictionary meets requirements for processing. Returns false otherwise
#' @export
checkDictionary <- function(dictionary){
  if (is.data.frame(dictionary) == FALSE){
    warning("Dictionary must be dataframe")
    return(FALSE)
  }

  if (ncol(dictionary) != 2) {
    warning("Dictionary must have 2 columns")
    return(FALSE)
  }

  if (colnames(dictionary)[1] != "flag") {
    warning("Column 1 of dictionary must be named 'flag' and contain the flags found in getDictionary()")
    return(FALSE)
  }

  #Check if the left flag is present in the input dictionary as this is the minimum character necessary to function
  if (FALSE %in% c(grepl("\u00AB", dictionary[,1]))) {
    warning("Flag not found: document and dictionary should contain a flag in the format \u00ABdocument_flag\u00BB")
    return(FALSE)
  }

  if (TRUE %in% is.na(dictionary[,2])) {
    warning("NA found in dictionary... Replacing with original flags... Please use NA as chars if intended value")
    return(TRUE)
  }
  return(TRUE)
}

##
##Create a private dictionary with all the components
##
getPrivateDictionary <- function(xml_data){
  zipfile_xml <- xml_data

  flag <- get_flags(zipfile_xml) #formatted flags
  flag_xml <- get_docket_xml(zipfile_xml) #unformatted XML flag

  docket.dictionary.private <- data.frame('flag xml' = flag_xml, 'flag' = flag)
  docket.dictionary.private$'adj flag xml' <- NA

  for (i in 1:nrow(docket.dictionary.private)) {
    docket.dictionary.private$'adj flag xml'[i] <-
      regmatches(docket.dictionary.private[i,1], gregexpr("<[^>]+>|\u00AB|\n", docket.dictionary.private[i,1]))[1]

    docket.dictionary.private$'adj flag xml'[i] <- as.character(paste0(unlist(docket.dictionary.private$'adj flag xml'[i]), collapse = ""))
  }
  return(docket.dictionary.private)
}


#' Replace flags in template document with data from R environment
#
#' @description Scans the input .doc or .docx file for specified flags, as defined in the dictionary,
#' and replaces them with corresponding data. The edited content is then saved to a new document.
#'
#' @param filename The file path to the document template. Supports .doc and .docx formats
#' @param dictionary A data frame where each row represents a flag in the document
#' @param outputName The file path and name for the saved output document
#' @return Generates a new .doc or .docx file with the flags replaced by the specified data
#' @export
docket <- function(filename, dictionary, outputName) {
  if (checkDictionary(dictionary) != TRUE){
    stop()
  }

  old_wd <- getwd()

  temp_dir <- paste0(filename, "_dockettemp") #Temp directory for holding files

  zipfile_xml <- open_zipfile(filename) #Creates temp file and extracts the content

  docket.dictionary.private <- getPrivateDictionary(zipfile_xml) #Creates a dictionary of the private flags

  full_dictionary <- merge(docket.dictionary.private, dictionary, by = "flag", all.x=TRUE, all.y=FALSE) #Joins private dictionary with user dictionary

  #Replace NAs with blanks
  full_dictionary[,4] <- ifelse(is.na(full_dictionary[,4]), full_dictionary[,2], full_dictionary[,4])

  #Replace start of flag with the value in order to maintain the flag structure
  for (i in 1:nrow(full_dictionary)) {
    full_dictionary[i,3] <- gsub("\u00AB", full_dictionary[i,4], full_dictionary[i,3])
  }

  #Replace unformatted xml elements with formatted xml elements
  for (i in 1:nrow(full_dictionary)) {
    zipfile_xml <- str_replace_all(zipfile_xml, full_dictionary[i,2], as.character(full_dictionary[i,3]))
  }


  #Replace the document.xml file with the updated XML
  write_xml(as_xml_document(zipfile_xml), paste0(temp_dir, "/word/document.xml"))

  setwd(temp_dir)
  zip(zipfile = outputName, files = list.files(), recurse = TRUE)
  setwd(old_wd)
  close_unzip_file(filename)
}

