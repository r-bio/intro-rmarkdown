
##' This function renames all files consistently and moves them into their own folder.
##'
##' This function does two things. First, it finds all files that
##' match a given regular expression pattern in an input
##' directory. Once the files are identified, they are renamed such
##' that they all start with their family name, followed by the full
##' genus name.
##' @title Rename the Holothuria files
##' @param input_path The path to the directory that holds the incorrectly named files.
##' @param output_path The path to the directory where the renamed files will be copied over.
##' @param pattern A regular expression indicating the pattern used to
##' recognize the files that are going to be affected by the function.
##' @param family The family name to be used as a prefix for all files.
##' @return TRUE if all the files identified by the regular expression
##' were moved to the "output" folder, FALSE otherwise.
##' @author
##' @export
rename_holothuria <- function(input_path="data_original/",
                              output_path="data_output/split_files",
                              pattern="(^H\\.|^Hol|^holo)",
                              family="Holothuriidae") {
    ## List all the files
    oldNm <- list.files(pattern=pattern, path=input_path)
    if (length(oldNm) < 1) {
        stop("No file found.")
    }

    ## Replace the abbreviated genus names with the full one
    fullGenus <- gsub(pattern, "Holothuria", oldNm)
    newNm <- paste(family, fullGenus, sep="-")

    ## Create a 2-column object that holds the old name and the common name
    nm <- cbind(file.path(input_path, oldNm), file.path(output_path, newNm))

    ## Copy the files to the data_output/split_files directory
    testRnm <- apply(nm, 1, function(x) file.copy(x[1], x[2]))

    if(all(testRnm)) {
        message("All good!")
        TRUE
    } else {
        warning("Something went wrong.")
        FALSE
    }
}


##' This function gather in a single data frame the content of CSV files identified by a regular expression.
##'
##' All the files must contain the same column names.
##' @title Collect all files split files and put them in a single data frame.
##' @param pattern A regular expression indicating how to recognize the files that need to be collected
##' @param path The path to the directory where the individual files are listed.
##' @return The consolidated data frame.
##' @author
##' @export
collect_holothuria <- function(pattern="^Holothuriidae-", path="data_output/split_files") {

    holFiles <- list.files(path=path, pattern=pattern)

    ## create empty data.frame to store the results
    res <- data.frame()

    for (i in 1:length(holFiles)) {
        tmpFile <- read.csv(file.path(path, holFiles[i]), stringsAsFactors=FALSE)

        ## write a command that selects the following columns from tmpFile.
        ## dwc.family
        ## dwc.genus
        ## dwc.specificEpithet
        ## idigbio.uuid
        ## dwc.scientificName
        ## dwc.occurrenceID
        ## dwc.decimalLatitude
        ## dwc.decimalLongitude
        tmpFile <- .....

        ## write a command that adds these selected columns to our storage data.frame `res`
        res <- .....
    }
    res
}
