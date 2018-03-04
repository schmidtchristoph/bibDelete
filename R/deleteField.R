#' Delete all occurrences of a specified field in a bibtex file
#'
#' Deletes all occurrences of a specified field in a bibtex file and writes a
#' new processed version in the same directory.
#'
#' @param filename string denoting the filename "name.bib"
#'
#' @param fieldname string denoting the bibtex field that should be removed from
#'   all entries in the input file. This function might automatically remove
#'   non-standard field types as well.
#'
#' @param verbose logical indicating whether in addition to the deletion success
#'   indicator (TRUE/FALSE) a vector containing indices of deleted lines and the
#'   processed file content is also returned. This is mostly useful for
#'   debugging purposes. Defaults to FALSE.
#'
#' @param addCustomField character string/ vector of character strings denoting
#'   a custom bibtex field type(s) that should never be automatically removed.
#'   This option is not needed for standard bibtex file.
#'
#' @return Returns a logical indicating whether or not the field
#'   \code{fieldname} was removed from file \code{filename}. If it was removed
#'   (TRUE), then an output file 'filename_pr.bib' (filename_processed) with the
#'   field in question removed is written to disk into the current working
#'   directory.
#'
#'   If input argument \code{verbose} was set to TRUE, then a list is returned
#'   that also contains the indices of deleted lines of the input file, as well
#'   as the content of the processed input file.
#'
#' @note The underlying parser function does not rely on predefined field names
#'   but uses the bibtex syntax elements "," and "\{" and "" (empty lines) to
#'   infer where a given input field name of interest starts and where it ends,
#'   even if it consists of multiple lines and no matter where its position in a
#'   bib entry block is situated. That's nice.
#'
#'   An empty last line is appended automatically to the input bib file if it
#'   misses such an empty line at its end. A copy of the original file is stored
#'   just in case it is needed.
#'
#' @export
#'
#' @examples
#'
#' # test.bib contains one single-line annotation
#' filePath <- system.file("testdata/test.bib", package = "bibDelete")
#' file.copy(filePath, "test.bib")
#' deleteField("test.bib", "annote")
#' deleteField("test.bib", "annote", TRUE)
#' deleteField("test.bib", "month")
#' deleteField("test.bib", "month", TRUE)
#' file.remove(c("test.bib", "test_pr.bib"))
#'
#'
#'
#' filePath <- system.file("testdata/test.bib", package = "bibDelete")
#' file.copy(filePath, "test.bib")
#' deleteField("test.bib", "annote") # returns TRUE
#' deleteField("test_pr.bib", "annote") # returns FALSE
#'
#' # file 'test_pr_pr.bib' will contain neither 'annote' nor 'month' fields
#' deleteField("test_pr.bib", "month") # returns TRUE
#' file.remove(c("test.bib", "test_pr.bib", "test_pr_pr.bib"))
#'
#'
#'
#' # test2.bib contains multi-line annotations
#' filePath <- system.file("testdata/test2.bib", package = "bibDelete")
#' file.copy(filePath, "test2.bib")
#' deleteField("test2.bib", "annote")
#' deleteField("test2.bib", "author")
#' deleteField("test2.bib", "Journal") # FALSE: wrong fieldname
#' deleteField("test2.bib", "journal") # TRUE: correct fieldname
#' deleteField("test2.bib", "month")
#' deleteField("test2_pr.bib", "annote") # month and annote removed
#' file.remove(c("test2.bib", "test2_pr.bib", "test2_pr_pr.bib"))
#'
#'
#'
#' # test3.bib contains non-standard fields and non-standard empty lines
#' filePath <- system.file("testdata/test3.bib", package = "bibDelete")
#' file.copy(filePath, "test3.bib")
#' deleteField("test3.bib", "annote")
#' deleteField("test3.bib", "month")
#' deleteField("test3_pr.bib", "annote") # month and annote removed
#' file.remove(c("test3.bib", "test3_pr.bib", "test3_pr_pr.bib"))
#'
#'
#'
#' # test4.bib contains non-standard annote fields with non-standard field
#' # delimiters
#' filePath <- system.file("testdata/test4.bib", package = "bibDelete")
#' file.copy(filePath, "test4.bib")
#' deleteField("test4.bib", "annote")
#' deleteField("test4.bib", "title")
#' deleteField("test4_pr.bib", "annote") # title and annote removed
#' file.remove(c("test4.bib", "test4_pr.bib", "test4_pr_pr.bib"))
#'
#' @author Christoph Schmidt <schmidtchristoph@@users.noreply.github.com>

# 04.03.18

deleteField <- function(filename, fieldname, verbose = FALSE, addCustomField = NULL){
   # check if bib file does contain an empty last line: if not, 'readLines()' will throw a warning, as the final line is incomplete (no final EOF marker)
   contents <- readChar(filename, file.info(filename)$size)
   lastChar <- stringr::str_sub(contents, -1L, -1L)

   if( lastChar != "\n" ){
      file.copy( filename, paste0(stringr::str_sub(filename, 1, -5), "_original", ".bib") )
      writeLines(contents, filename) # writing just contents without appending any newline character should automatically add an empty new line at the end of the file
   }







   f        <- readLines(filename)
   loc      <- stringr::str_extract(f, fieldname)
   ind      <- which(is.na(loc) %in% FALSE)  # indices of lines where the search pattern starts (where each such corresponding field of interest might span multiple lines)
   linesDel <- numeric(0)


   if(length(ind) > 0){ # only if the field exists
      for(k in 1:length(ind)){
         thisind  <- ind[k]
         linesDel <- c(linesDel, thisind) # to remove entire line(s) that contain the field of interest

         # the field spans only one single line:
         # -------------------------------------
         # if (nr of '{') = (nr of '}')
         # if 0 = (nr of '{') = (nr of '}') that is the field has incorrectly no delimiter, might end with a ',' and the next line contains '= {'
         #
         # the field spans multiple lines:
         # -------------------------------------
         # if (nr of '{') != (nr of '}')
         # count the number of open '{' and closing '}' delimiters; field ends when the number of closing '}' delimiters matches the number of open ones; delete all lines until reaching the last closing one, then also delete the line of the closing delimiter
         pos       <- getPosDelimiters(f, thisind)
         posOpen   <- pos$pOpen
         posClosed <- pos$pClose


         if( length(posOpen) != length(posClosed) ){ # searched field spans multiple lines; thus, more lines have to be added to deleted lines
            allOpen    <- posOpen
            allClosed  <- posClosed

            while(TRUE){
               thisind  <- thisind + 1 # go to the next line
               linesDel <- c(linesDel, thisind) # to remove entire line(s) that contain the field of interest

               # check if the incremented new line is the last line of the field
               pos2       <- getPosDelimiters(f, thisind)
               posOpen2   <- pos2$pOpen
               posClosed2 <- pos2$pClose

               allOpen    <- c(allOpen, posOpen2)
               allClosed  <- c(allClosed, posClosed2)

               if(length(allOpen) == length(allClosed)){ # we have finally reached the last line of the searched field (all open {-delimiters are matched by same number of closed }-delimiters)
                  break
               }
            }
         }
         else { # if the searched field has only a single line, given by thisind, which is already added to the deleted lines, thus nothing to do here anymore except for checking for non-standard continuation of the field on the next line(s)
            linesDel <- checkNonstandardContinuation(f, thisind, linesDel, addCustomField)
         }


         linesDel <- checkForSubsequentEmptyLines(f, thisind, linesDel)
         f        <- removeCommaOnLineBeforeSearchedField( f, ind, k, utils::tail(linesDel, 1) )

      } # for: looping over all lines in which the searched field appears



      writeLines(f[-linesDel], paste0(stringr::str_sub(filename, 1, -5), "_pr", ".bib"))



      if(!verbose){
         return(TRUE)
      }
      else {
         return(list(success=TRUE, linesDel=linesDel, processed=f[-linesDel]))
      }


   } # if searched field exists
   else { # field does not exist in bib file
      writeLines(paste0("The field 'fieldname' (", fieldname, ") does not exist in 'filename' (", filename, ")."))
      return(FALSE)
   }
}














# for each line, gets the indices of opening ('{') delimiters and of closing ('}') delimiters
getPosDelimiters <- function(f, thisind){
   posOpen   <- stringr::str_locate_all(f[thisind], stringr::fixed("{"))
   posOpen   <- posOpen[[1]][ , 1] # indices of {-delimiter in line thisind

   posClosed <- stringr::str_locate_all(f[thisind], stringr::fixed("}"))
   posClosed <- posClosed[[1]][ , 1] # indices of }-delimiter in line thisind


   return( list( pOpen=unname(posOpen), pClose=unname(posClosed) ) )
}






# check if lines following the last line of the searched field are empty lines
# delete all such empty lines until next field withing the same bib entry is encountered (has "= {" in the first line)
checkForSubsequentEmptyLines <- function(f, thisind, linesDel){
   nextLine <- thisind + 1

   while(TRUE){
      if( f[nextLine] == "" ){
         linesDel <- c(linesDel, nextLine)
         nextLine  <- nextLine + 1
      }
      else {
         break
      }
   }

   return(linesDel)
}





# check if current bib entry ( e.g. @article{} ) ends on the next line;
# if yes, then remove comma ending the line/field directly before the start of the searched field ind[k]
removeCommaOnLineBeforeSearchedField <- function(f, ind, k, lastVisitedLine){

   if(f[lastVisitedLine+1] == "}"){ # the next line is already the end of the entire entry, as it consists of only the "}" character
      lastCharLineBefore <- stringr::str_sub( f[ind[k]-1], -1L, -1L )

      if(lastCharLineBefore == ","){
         f[ind[k]-1] <- stringr::str_sub( f[ind[k]-1], 1L, -2L ) # delete the comma from the end of the line
      }
   }

   return(f)
}






# checks whether a field without delimiter ({,}) that is supposedly a one line
# field is incorrectly continued on following lines
# e.g.:
# month = mar,
# vs.:
# month = mar
# dec
# nov,
#
# also intrinsically/ automatically deletes non-standard field types
#
#
#
# f <- c("@article{Peel:2014ul,", "author = {Peel, Leto and Clauset, Aaron},",
# "title = {{Detecting change points in the large-scale structure of evolving networks}},",
# "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
# "eprinttype = {arxiv},", "month = mar", "dec", "nov,", "annote = {{\\#} see conference proceeding papers3://publication/uuid/739AD14E-73B1-4A87-8A5B-6DBD847D0F47",
# "here, it goes on,", "", "{\\#} corresponding Python code at http://gdriv.es/letopeel/code.html",
# "", "{\\#} change-point methods based on network measures like the mean degree, clustering coeffi- cient, or mean geodesic path length performed poorly, yielding high false negative rates even for large structural changes},",
# "", "test = {testfield},", "month = mar,", "dec,", "nov,", "}")
#
# thisind <- linesDel <- 8
#
#
#
# f <- c("@article{Peel:2014ul,", "author = {Peel, Leto and Clauset, Aaron},",
# "title = {{Detecting change points in the large-scale structure of evolving networks}},",
# "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
# "eprinttype = {arxiv},", "month = mar", "dec", "nov,", "annote = {{\\#} see conference proceeding papers3://publication/uuid/739AD14E-73B1-4A87-8A5B-6DBD847D0F47",
# "here, it goes on,", "", "{\\#} corresponding Python code at http://gdriv.es/letopeel/code.html",
# "", "{\\#} change-point methods based on network measures like the mean degree, clustering coeffi- cient, or mean geodesic path length performed poorly, yielding high false negative rates even for large structural changes},",
# "", "test = {testfield},", "month = mar,", "dec,", "nov,", "}")
#
# thisind <- 19L
# linesDel <- c(8, 9, 10, 19)

checkNonstandardContinuation <- function(f, thisind, linesDel, addCustomField){
   bibFields <- getBibFields()
   if(!is.null(addCustomField)){
      bibFields <- c(bibFields, addCustomField)
   }

   nextLine  <- thisind + 1
   condWhile <- TRUE


   while(condWhile){
      if(nextLine >= length(f) || f[nextLine] == "}"){ break }

      pos <- unname( stringr::str_locate(f[nextLine], stringr::fixed("=") )[1,1])

      if(is.na(pos)){ # no "=" found on nextLine, thus nextLine can be deleted
         linesDel <- c(linesDel, nextLine)
         nextLine <- nextLine + 1
      }
      else { # "=" was found on nextLine: check if it belongs to a new field specification; if yes, stop deleting lines
         isNewField <- FALSE

         for(k in 1:length(bibFields)){
            searchPattern <- paste0(bibFields[k], " = ")
            pos2          <- unname( stringr::str_locate(f[nextLine], stringr::fixed(searchPattern) )[1,1])

            if(!is.na(pos2)){ # new field definition was found, thus nextLine cannot be deleted
               isNewField <- TRUE
               condWhile  <- FALSE
               break # break out of for loop
            }
         }


         if(!isNewField){ # even though there was a "=" on nextLine, no new *legit* field definition was encountered, thus nextLine can be deleted
            linesDel <- c(linesDel, nextLine)
            nextLine <- nextLine + 1
         }
      } # else "=" found
   }

   return(linesDel)
}






getBibFields <- function(){
   bf <- c("author", "title", "journal", "year", "eprint", "eprinttype",
           "month", "annote", "address", "booktitle", "chapter", "crossref",
           "edition", "editor", "howpublished", "institution", "key", "note",
           "number", "organization", "pages", "publisher", "school", "series",
           "type", "volume")
   return( bf )
}
