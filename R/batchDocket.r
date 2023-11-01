#' @import zip
#' @import stringr
#' @import XML
#' @import xml2
#'
#' @title Dictionary for multiple docket outputs
#
#' @description Scans the input file for strings enclosed by flag wings: « ». Creates a replacement value column for each document to be generated
#' @param filename The file path to the document template
#' @param outputFiles A list of the file names and paths for the populated templates
#' @param dictionaryLength Number of columns in the batch dictionary. Defaults to the number of output files. Cannot be shorter than the count of 'outputFiles'
#' @return Data frame for populating data into the template with row 1 containing the output file names
#' @examples
#' # Path to the sample template file included in the package
#' template_path <- system.file("batch_document", "batchTemplate.docx", package="docket")
#' output_paths <- as.list(paste0(dirname(template_path), paste0("/batch document", 1:5, ".docx")))
#'
#' # Create a dictionary by using the getDictionary function on the sample template file
#' result <- getBatchDictionary(template_path, output_paths)
#' print(result)
#' @export
getBatchDictionary <- function(filename, outputFiles, dictionaryLength = length(outputFiles)) {
  if (is.numeric(dictionaryLength) != TRUE){
    stop("dictionaryLength must be a numeric")
  }

  if (is.list(outputFiles) != TRUE){
    stop("outputFiles must be a list of the file names and paths for the populated templates")
  }

  if (dictionaryLength < length(outputFiles)) {
    stop("Dictionary Length shorter than ouptut files")
  }

  if (length(outputFiles) == 0) {
    stop("Please input the output paths for the batch documents as a list")
  }

  if (length(outputFiles) == 1) {
    warning("Batch dictionaries should only be used for the creation of multiple documents")
  }

  #Create Null file names if the dictionary length is longer than the input file length
  if (dictionaryLength > length(outputFiles)) {
    outputFiles <- paste0(c(outputFiles, rep(NA, (dictionaryLength - length(outputFiles)))))
  }

  private.batch.dictionary <- getDictionary(filename) #Create a basic dictionary

  #Add top column with batch details
  private.batch.dictionary <- rbind(c("Output File Location:", NA), private.batch.dictionary)

  #Create batch dictionary columns
  for (i in 1:dictionaryLength) {
    private.batch.dictionary[,(i+1)] <- private.batch.dictionary[,2]
    private.batch.dictionary[1,(i+1)] <- outputFiles[i]
  }

  colnames(private.batch.dictionary) <- c("flag", c(paste0("replace.value.", 1:(ncol(private.batch.dictionary)-1))))

  return(private.batch.dictionary)
}

#' @title Check that the batch dictionary is valid
#
#' @description Validates that the input batch dictionary meets the following requirements:
#' #' #' \itemize{
#'   \item \strong{1.} It is a data frame
#'   \item \strong{2.} Column 1 is named "flag"
#'   \item \strong{3.} Column 1 contains flags with starting and ending wings: « »
#'   \item \strong{4.} Row 1 contains the file names and paths of the populated output documents
#'   }
#'
#' @param batchDictionary A data frame where each row represents a flag to be replaced in the template document
#' and each column represents a final document to be generated
#' @return Logical. Returns 'TRUE' if the batch dictionary meets requirements for processing. Returns 'FALSE' otherwise
#' @examples
#' # Path to the sample template file included in the package
#' template_path <- system.file("batch_document", "batchTemplate.docx", package="docket")
#' output_paths <- as.list(paste0(dirname(template_path), paste0("/batch document", 1:5, ".docx")))
#'
#' # Create a dictionary by using the getDictionary function on the sample template file
#' result <- getBatchDictionary(template_path, output_paths)
#' result[2,2:ncol(result)] <- Sys.getenv("USERNAME") #Author name
#' result[3,2:ncol(result)] <- as.character(Sys.Date())
#' result[4,2:ncol(result)] <- 123
#' result[5,2:ncol(result)] <- 456
#' result[6,2:ncol(result)] <- 789
#' result[7,2:ncol(result)] <- sum(as.numeric(result[4:6,2]))
#'
#' # Verify that the result dictionary is valid
#' if (checkBatchDictionary(result) == TRUE) {
#'   print("Valid Batch Dictionary")
#' }
#' @export
checkBatchDictionary <- function(batchDictionary) {
  if (is.data.frame(batchDictionary) == FALSE){
    warning("Batch dictionary must be dataframe")
    return(FALSE)
  }

  if (colnames(batchDictionary)[1] != "flag") {
    warning("Column 1 of Batch dictionary must be named 'flag' and contain the flags found in getBatchDictionary()")
    return(FALSE)
  }

  #Check if the left flag is present in the input dictionary as this is the minimum character necessary to function
  if (FALSE %in% c(grepl("\u00AB", batchDictionary[2:nrow(batchDictionary),1]))) { #checks after row 1
    warning("Flag not found: document and dictionary should contain a flag in the format \u00ABdocument_flag\u00BB")
    return(FALSE)
  }

  if (!ncol(batchDictionary) > 2) {
    warning("Batch dictionary is 2 columns or less. Batch docket generation should only be for multiple documents")
    return(FALSE)
  }

  if (TRUE %in% c(is.na(batchDictionary[1,]))){
    warning("NA Found for file location")
    return(FALSE)
  }

  if (TRUE %in% c(batchDictionary[1,] == "NA")){
    warning("NA Found for file location")
    return(FALSE)
  }

  if (TRUE %in% c(batchDictionary == "NA")){
    warning("NA found in batch dictionary... Replacing with original flags... Please use NA as chars if intended value")
    return(TRUE)
  }

  return(TRUE)
}

#' @title Create Documents
#
#' @description Scans the input template file for specified flags as defined in the dictionary,
#' and replaces them with corresponding data. Repeats the process for each column, generating a new document
#' for each column which is saved as the file name and path listed in row 1
#' @param filename The file path to the document template. Supports .doc and .docx formats
#' @param batchDictionary A data frame where each row represents a flag to be replaced in the template document
#' and each column represents a final document to be generated
#' @return Generates new .doc or .docx files with the flags replaced by the specified data for that column
#' @examples
#' # Path to the sample template file included in the package
#' template_path <- system.file("batch_document", "batchTemplate.docx", package="docket")
#' output_paths <- as.list(paste0(dirname(template_path), paste0("/batch document", 1:5, ".docx")))
#'
#' # Create a dictionary by using the getDictionary function on the sample template file
#' result <- getBatchDictionary(template_path, output_paths)
#' result[2,2:ncol(result)] <- Sys.getenv("USERNAME") #Author name
#' result[3,2:ncol(result)] <- as.character(Sys.Date())
#' result[4,2:ncol(result)] <- 123
#' result[5,2:ncol(result)] <- 456
#' result[6,2:ncol(result)] <- 789
#' result[7,2:ncol(result)] <- sum(as.numeric(result[4:6,2]))
#'
#' # Verify that the result dictionary is valid
#' if (checkBatchDictionary(result) == TRUE) {
#'  batchDocket(template_path, result)
#'  for (i in 1:length(output_paths)) {
#'    if (file.exists(output_paths[[i]])) {
#'      print(paste("docket", i, "Successfully Created"))
#'    }
#'  }
#'}
#' @export
batchDocket <- function(filename, batchDictionary) {
  if (checkBatchDictionary(batchDictionary) != TRUE){
    stop()
  }

  temp_dir <- paste0(filename, "_dockettemp") #Temp directory for holding files
  zipfile_xml <- open_zipfile(filename) #Creates temp file and extracts the content
  on.exit(close_unzip_file(filename)) #Removes temp file

  docket.dictionary.private <- getPrivateDictionary(zipfile_xml) #Creates a dictionary of the private flags

  #Create a batch docket for each column
  for(i in 1:(ncol(batchDictionary)-1)) {
    createBatchDocket(filename, batchDictionary[,c(1, i+1)], zipfile_xml, docket.dictionary.private)
  }

}

createBatchDocket <- function(filename, batchDictionary, zipfile_xml, docket.dictionary.private) {
  old_wd <- getwd()
  on.exit(setwd(old_wd))

  temp_dir <- paste0(filename, "_dockettemp")
  #Join private dictionary with instance of batch dictionary
  full_dictionary <- merge(docket.dictionary.private, batchDictionary, by = "flag", all.x=TRUE, all.y=FALSE) #Joins private dictionary with user dictionary

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
  zip(zipfile = batchDictionary[1,2], files = list.files(), recurse = TRUE, include_directories = FALSE)
  setwd(old_wd)
}


