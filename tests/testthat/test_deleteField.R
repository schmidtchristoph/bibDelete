# 04.03.18
#  @author Christoph Schmidt <schmidtchristoph@@users.noreply.github.com>


library(testthat)

context("deleteField")


test_that("correct lines are deleted - pt. 1", {
   filePath <- system.file("testdata/test.bib", package = "bibDelete")




   r        <- deleteField(filePath, "annote", verbose = TRUE)
   expect_equal(r$linesDel, 35)
   filePath2 <- system.file("testdata/test_pr.bib", package = "bibDelete")
   f         <- readLines(filePath2)
   expect_true( stringr::str_sub(f[34], -1L, -1L)!="," )




   r        <- deleteField(filePath, "month", verbose = TRUE)
   expect_equal(r$linesDel, c(12, 23, 34))
   filePath2 <- system.file("testdata/test_pr.bib", package = "bibDelete")
   f         <- readLines(filePath2)
   expect_true( stringr::str_sub(f[11], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[21], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[31], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[32], -1L, -1L)!="," )




   r         <- deleteField(filePath, "month", verbose = TRUE) # month + annote field removed
   filePath2 <- system.file("testdata/test_pr.bib", package = "bibDelete")
   r         <- deleteField(filePath2, "annote", verbose = TRUE)
   expect_equal(r$linesDel, c(32))
   filePath3 <- system.file("testdata/test_pr_pr.bib", package = "bibDelete") # month + annote field removed
   f         <- readLines(filePath3)
   expect_true( stringr::str_sub(f[11], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[21], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[31], -1L, -1L)!="," )




   r         <- deleteField(filePath, "annote", verbose = TRUE) # annote + month field removed
   filePath2 <- system.file("testdata/test_pr.bib", package = "bibDelete")
   r         <- deleteField(filePath2, "month", verbose = TRUE)
   expect_equal(r$linesDel, c(12, 23, 34))
   filePath3 <- system.file("testdata/test_pr_pr.bib", package = "bibDelete") # annote + month field removed
   f         <- readLines(filePath3)
   expect_true( stringr::str_sub(f[11], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[21], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[31], -1L, -1L)!="," )




   file.remove(filePath2, filePath3)
})














test_that("correct lines are deleted - pt. 2", {
   filePath <- system.file("testdata/test2.bib", package = "bibDelete")




   r        <- deleteField(filePath, "annote", verbose = TRUE)
   expect_equal(r$linesDel, c(9, 10, 11, 22, 23, 24, 25, 26, 27, 28))
   filePath2 <- system.file("testdata/test2_pr.bib", package = "bibDelete")
   f         <- readLines(filePath2)
   expect_true( stringr::str_sub(f[8], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[18], -1L, -1L)!="," )




   r        <- deleteField(filePath, "month", verbose = TRUE)
   expect_equal(r$linesDel, c(8, 21))
   filePath2 <- system.file("testdata/test2_pr.bib", package = "bibDelete")
   f         <- readLines(filePath2)
   expect_true( stringr::str_sub(f[7], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[10], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[19], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[26], -1L, -1L)!="," )




   r         <- deleteField(filePath, "month", verbose = TRUE) # month + annote field removed
   filePath2 <- system.file("testdata/test2_pr.bib", package = "bibDelete")
   r         <- deleteField(filePath2, "annote", verbose = TRUE)
   expect_equal(r$linesDel, c(8, 9, 10, 20, 21, 22, 23, 24, 25, 26))
   filePath3 <- system.file("testdata/test2_pr_pr.bib", package = "bibDelete") # month + annote field removed
   f         <- readLines(filePath3)
   expect_true( stringr::str_sub(f[6], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[7], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[15], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[16], -1L, -1L)!="," )




   r         <- deleteField(filePath, "annote", verbose = TRUE) # annote + month field removed
   filePath2 <- system.file("testdata/test2_pr.bib", package = "bibDelete")
   r         <- deleteField(filePath2, "month", verbose = TRUE)
   expect_equal(r$linesDel, c(8, 18))
   filePath3 <- system.file("testdata/test2_pr_pr.bib", package = "bibDelete") # annote + month field removed
   f         <- readLines(filePath3)
   expect_true( stringr::str_sub(f[6], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[7], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[15], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[16], -1L, -1L)!="," )




   file.remove(filePath2, filePath3)
})
























test_that("correct lines are deleted - pt. 3", {
   filePath <- system.file("testdata/test3.bib", package = "bibDelete")
   file.copy(filePath, "test3.bib")



   r        <- deleteField("test3.bib", "annote", verbose = TRUE)
   expect_equal(r$linesDel, c(11, 12, 13, 14, 15, 16, 17))
   f         <- readLines("test3_pr.bib")
   expect_true( stringr::str_sub(f[10], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[11], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[12], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[13], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[14], -1L, -1L)=="," ) # test3.bib is not standard conform: normally there should be just a single delimiter "}"




   r        <- deleteField("test3.bib", "month", verbose = TRUE)
   expect_equal(r$linesDel, c(8, 9, 10, 19, 20, 21))
   f         <- readLines("test3_pr.bib")
   expect_true( stringr::str_sub(f[7], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[13], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[15], -1L, -1L)!="," )




   r         <- deleteField("test3.bib", "month", verbose = TRUE) # month + annote field removed
   r         <- deleteField("test3_pr.bib", "annote", verbose = TRUE)
   expect_equal(r$linesDel, c(8, 9, 10, 11, 12, 13, 14))
   f         <- readLines("test3_pr_pr.bib") # month + annote field removed
   expect_true( stringr::str_sub(f[7], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[8], -1L, -1L)!="," )




   r         <- deleteField("test3.bib", "annote", verbose = TRUE) # annote + month field removed
   expect_equal(r$linesDel, 11:17)
   r2         <- deleteField("test3_pr.bib", "month", verbose = TRUE)
   expect_equal(r2$linesDel, c(8, 9, 10, 11, 12, 13, 14))
   f         <- readLines("test3_pr_pr.bib") # annote + month field removed
   expect_true( stringr::str_sub(f[1], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[2], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[3], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[4], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[5], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[6], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[7], -1L, -1L)=="," ) # a very rare bug in 'removeCommaOnLineBeforeSearchedField()', only caused by severely non-standard bib entries (i.e. two "month" fields running over multiple lines each, separated by a non-standard "test" field type); this bug can most likely only be fixed in a general way be running over entire file, line by line and finding start and end of each bib entry; then checking the last fields ending of each bib entry for a remaining comma
   expect_true( stringr::str_sub(f[8], -1L, -1L)=="}" )





   r         <- deleteField("test3.bib", "annote", verbose = TRUE) # annote + month field removed + taking care of custom field type definition
   expect_equal(r$linesDel, 11:17)
   r2         <- deleteField("test3_pr.bib", "month", verbose = TRUE, addCustomField = "test")
   expect_equal(r2$linesDel, c(8, 9, 10, 12, 13, 14))
   f         <- readLines("test3_pr_pr.bib") # annote + month field removed
   expect_true( stringr::str_sub(f[1], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[2], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[3], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[4], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[5], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[6], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[7], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[8], -1L, -1L)!="," )
   expect_true( stringr::str_sub(f[9], -1L, -1L)=="}" )




   r        <- deleteField("test3.bib", "journal", verbose = TRUE)
   expect_equal(r$linesDel, 4)
   f         <- readLines("test3_pr.bib")
   expect_true( stringr::str_sub(f[3], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[4], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[5], -1L, -1L)=="," )
   expect_true( stringr::str_sub(f[20], -1L, -1L)=="," ) #  should not be the case, but 'test3.bib' is not standard conform and the last field type ('month') wasn't requested to be deleted




   file.remove("test3.bib", "test3_pr.bib", "test3_pr_pr.bib")
})














test_that("correct output file is generated", {
   filePath  <- system.file("testdata/test.bib", package = "bibDelete")
   file.copy(filePath, "test.bib")
   deleteField("test.bib", "annote")
   f         <- readLines("test_pr.bib")
   f_expect  <- c("%% Created using Papers on Thu, 11 Aug 2016.", "%% http://papersapp.com/papers/",
                  "", "@article{Estrada:2010ka,", "author = {Estrada, Ernesto},",
                  "title = {{Quantifying network heterogeneity}},", "journal = {Physical Review E},",
                  "year = {2010},", "volume = {82},", "number = {6},", "pages = {066102},",
                  "month = dec", "}", "", "@article{Freeman:1977kx,", "author = {Freeman, Linton C},",
                  "title = {{A set of measures of centrality based on betweenness}},",
                  "journal = {Sociometry},", "year = {1977},", "volume = {40},",
                  "number = {1},", "pages = {35},", "month = mar", "}", "", "@article{Krzywinski:2012jj,",
                  "author = {Krzywinski, Martin and Birol, Inanc and Jones, Steven J M and Marra, Marco A},",
                  "title = {{Hive plots-rational approach to visualizing networks}},",
                  "journal = {Briefings in Bioinformatics},", "year = {2012},",
                  "volume = {13},", "number = {5},", "pages = {627--644},", "month = sep",
                  "}")
   expect_equal(f, f_expect)




   filePath  <- system.file("testdata/test.bib", package = "bibDelete")
   file.copy(filePath, "test.bib")
   deleteField("test.bib", "annote")
   deleteField("test_pr.bib", "month")
   deleteField("test_pr_pr.bib", "author")
   f         <- readLines("test_pr_pr_pr.bib")
   f_expect  <- c("%% Created using Papers on Thu, 11 Aug 2016.", "%% http://papersapp.com/papers/",
                  "", "@article{Estrada:2010ka,", "title = {{Quantifying network heterogeneity}},",
                  "journal = {Physical Review E},", "year = {2010},", "volume = {82},",
                  "number = {6},", "pages = {066102}", "}", "", "@article{Freeman:1977kx,",
                  "title = {{A set of measures of centrality based on betweenness}},",
                  "journal = {Sociometry},", "year = {1977},", "volume = {40},",
                  "number = {1},", "pages = {35}", "}", "", "@article{Krzywinski:2012jj,",
                  "title = {{Hive plots-rational approach to visualizing networks}},",
                  "journal = {Briefings in Bioinformatics},", "year = {2012},",
                  "volume = {13},", "number = {5},", "pages = {627--644}", "}")
   expect_equal(f, f_expect)
   file.remove(c("test.bib", "test_pr.bib", "test_pr_pr.bib", "test_pr_pr_pr.bib"))




   filePath  <- system.file("testdata/test.bib", package = "bibDelete")
   file.copy(filePath, "test.bib")
   deleteField("test.bib", "month")
   f         <- readLines("test_pr.bib")
   f_expect  <- c("%% Created using Papers on Thu, 11 Aug 2016.", "%% http://papersapp.com/papers/",
                  "", "@article{Estrada:2010ka,", "author = {Estrada, Ernesto},",
                  "title = {{Quantifying network heterogeneity}},", "journal = {Physical Review E},",
                  "year = {2010},", "volume = {82},", "number = {6},", "pages = {066102}",
                  "}", "", "@article{Freeman:1977kx,", "author = {Freeman, Linton C},",
                  "title = {{A set of measures of centrality based on betweenness}},",
                  "journal = {Sociometry},", "year = {1977},", "volume = {40},",
                  "number = {1},", "pages = {35}", "}", "", "@article{Krzywinski:2012jj,",
                  "author = {Krzywinski, Martin and Birol, Inanc and Jones, Steven J M and Marra, Marco A},",
                  "title = {{Hive plots-rational approach to visualizing networks}},",
                  "journal = {Briefings in Bioinformatics},", "year = {2012},",
                  "volume = {13},", "number = {5},", "pages = {627--644},", "annote = {{\\#} hive plots provide visual signatures of large networks}",
                  "}")
   expect_equal(f, f_expect)




   filePath  <- system.file("testdata/test.bib", package = "bibDelete")
   file.copy(filePath, "test.bib")
   deleteField("test.bib", "title")
   f         <- readLines("test_pr.bib")
   f_expect  <- c("%% Created using Papers on Thu, 11 Aug 2016.", "%% http://papersapp.com/papers/",
                  "", "@article{Estrada:2010ka,", "author = {Estrada, Ernesto},",
                  "journal = {Physical Review E},", "year = {2010},", "volume = {82},",
                  "number = {6},", "pages = {066102},", "month = dec", "}", "",
                  "@article{Freeman:1977kx,", "author = {Freeman, Linton C},",
                  "journal = {Sociometry},", "year = {1977},", "volume = {40},",
                  "number = {1},", "pages = {35},", "month = mar", "}", "", "@article{Krzywinski:2012jj,",
                  "author = {Krzywinski, Martin and Birol, Inanc and Jones, Steven J M and Marra, Marco A},",
                  "journal = {Briefings in Bioinformatics},", "year = {2012},",
                  "volume = {13},", "number = {5},", "pages = {627--644},", "month = sep,",
                  "annote = {{\\#} hive plots provide visual signatures of large networks}",
                  "}")
   expect_equal(f, f_expect)




   file.remove(c("test.bib", "test_pr.bib"))
})





















test_that("correct output file is generated--pt2", {
   filePath  <- system.file("testdata/test2.bib", package = "bibDelete")
   file.copy(filePath, "test2.bib")
   deleteField("test2.bib", "annote")
   f         <- readLines("test2_pr.bib")
   f_expect  <- c("@article{Lancichinetti:2012kx,", "author = {Lancichinetti, Andrea and Fortunato, Santo},",
                  "title = {{Consensus clustering in complex networks}},", "journal = {Scientific Reports},",
                  "year = {2012},", "volume = {2},", "pages = {336},", "month = mar",
                  "}", "", "@article{Peel:2014ul,", "author = {Peel, Leto and Clauset, Aaron},",
                  "title = {{Detecting change points in the large-scale structure of evolving networks}},",
                  "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
                  "eprinttype = {arxiv},", "month = mar", "}")
   expect_equal(f, f_expect)




   filePath  <- system.file("testdata/test2.bib", package = "bibDelete")
   file.copy(filePath, "test2.bib")
   deleteField("test2.bib", "annote")
   deleteField("test2_pr.bib", "month")
   deleteField("test2_pr_pr.bib", "author")
   f         <- readLines("test2_pr_pr_pr.bib")
   f_expect  <- c("@article{Lancichinetti:2012kx,", "title = {{Consensus clustering in complex networks}},",
                  "journal = {Scientific Reports},", "year = {2012},", "volume = {2},",
                  "pages = {336}", "}", "", "@article{Peel:2014ul,", "title = {{Detecting change points in the large-scale structure of evolving networks}},",
                  "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
                  "eprinttype = {arxiv}", "}")
   expect_equal(f, f_expect)
   file.remove(c("test2.bib", "test2_pr.bib", "test2_pr_pr.bib", "test2_pr_pr_pr.bib"))




   filePath  <- system.file("testdata/test2.bib", package = "bibDelete")
   file.copy(filePath, "test2.bib")
   deleteField("test2.bib", "month")
   f         <- readLines("test2_pr.bib")
   f_expect  <- c("@article{Lancichinetti:2012kx,", "author = {Lancichinetti, Andrea and Fortunato, Santo},",
                  "title = {{Consensus clustering in complex networks}},", "journal = {Scientific Reports},",
                  "year = {2012},", "volume = {2},", "pages = {336},", "annote = {{\\#} module detection algorithms might be dependent on random seeds",
                  "", "{\\#} nr of runs r = nr of partitions used for the consensus matrix}",
                  "}", "", "@article{Peel:2014ul,", "author = {Peel, Leto and Clauset, Aaron},",
                  "title = {{Detecting change points in the large-scale structure of evolving networks}},",
                  "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
                  "eprinttype = {arxiv},", "annote = {{\\#} see conference proceeding papers3://publication/uuid/739AD14E-73B1-4A87-8A5B-6DBD847D0F47",
                  "", "{\\#} corresponding Python code at http://gdriv.es/letopeel/code.html",
                  "", "{\\#} we found that changes associated with two communities merging or with one of several communities losing its internal connections ({\\textquotedblleft}fragmentation{\\textquotedblright}) were more difficult to accurately detect than those associated with one community splitting in two or with many singletons connecting to form a new community ({\\textquotedblleft}formation{\\textquotedblright})",
                  "", "{\\#} change-point methods based on network measures like the mean degree, clustering coeffi- cient, or mean geodesic path length performed poorly, yielding high false negative rates even for large structural changes}",
                  "}")
   expect_equal(f, f_expect)




   filePath  <- system.file("testdata/test2.bib", package = "bibDelete")
   file.copy(filePath, "test2.bib")
   deleteField("test2.bib", "title")
   f         <- readLines("test2_pr.bib")
   f_expect  <- c("@article{Lancichinetti:2012kx,", "author = {Lancichinetti, Andrea and Fortunato, Santo},",
                  "journal = {Scientific Reports},", "year = {2012},", "volume = {2},",
                  "pages = {336},", "month = mar,", "annote = {{\\#} module detection algorithms might be dependent on random seeds",
                  "", "{\\#} nr of runs r = nr of partitions used for the consensus matrix}",
                  "}", "", "@article{Peel:2014ul,", "author = {Peel, Leto and Clauset, Aaron},",
                  "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
                  "eprinttype = {arxiv},", "month = mar,", "annote = {{\\#} see conference proceeding papers3://publication/uuid/739AD14E-73B1-4A87-8A5B-6DBD847D0F47",
                  "", "{\\#} corresponding Python code at http://gdriv.es/letopeel/code.html",
                  "", "{\\#} we found that changes associated with two communities merging or with one of several communities losing its internal connections ({\\textquotedblleft}fragmentation{\\textquotedblright}) were more difficult to accurately detect than those associated with one community splitting in two or with many singletons connecting to form a new community ({\\textquotedblleft}formation{\\textquotedblright})",
                  "", "{\\#} change-point methods based on network measures like the mean degree, clustering coeffi- cient, or mean geodesic path length performed poorly, yielding high false negative rates even for large structural changes}",
                  "}")
   expect_equal(f, f_expect)




   file.remove(c("test2.bib", "test2_pr.bib"))
})





















test_that("correct output file is generated--pt3", {
   filePath  <- system.file("testdata/test3.bib", package = "bibDelete")
   file.copy(filePath, "test3.bib", overwrite = TRUE)
   deleteField("test3.bib", "annote")
   f         <- readLines("test3_pr.bib")
   f_expect  <- c("@article{Peel:2014ul,", "author = {Peel, Leto and Clauset, Aaron},",
                  "title = {{Detecting change points in the large-scale structure of evolving networks}},",
                  "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
                  "eprinttype = {arxiv},", "month = mar", "dec", "nov,", "test = {testfield},",
                  "month = mar,", "dec,", "nov,", "}")
   expect_equal(f, f_expect)




   filePath  <- system.file("testdata/test3.bib", package = "bibDelete")
   file.copy(filePath, "test3.bib", overwrite = TRUE)
   deleteField("test3.bib", "annote", addCustomField = "test")
   deleteField("test3_pr.bib", "month", addCustomField = "test")
   deleteField("test3_pr_pr.bib", "author", addCustomField = "test")
   f         <- readLines("test3_pr_pr_pr.bib")
   f_expect  <- c("@article{Peel:2014ul,", "title = {{Detecting change points in the large-scale structure of evolving networks}},",
                  "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
                  "eprinttype = {arxiv},", "test = {testfield}", "}")
   expect_equal(f, f_expect)
   file.remove(c("test3.bib", "test3_pr.bib", "test3_pr_pr.bib", "test3_pr_pr_pr.bib"))




   filePath  <- system.file("testdata/test3.bib", package = "bibDelete")
   file.copy(filePath, "test3.bib", overwrite = TRUE)
   deleteField("test3.bib", "month")
   f         <- readLines("test3_pr.bib")
   f_expect  <- c("@article{Peel:2014ul,", "author = {Peel, Leto and Clauset, Aaron},",
                  "title = {{Detecting change points in the large-scale structure of evolving networks}},",
                  "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
                  "eprinttype = {arxiv},", "annote = {{\\#} see conference proceeding papers3://publication/uuid/739AD14E-73B1-4A87-8A5B-6DBD847D0F47",
                  "here, it goes on,", "", "{\\#} corresponding Python code at http://gdriv.es/letopeel/code.html",
                  "", "{\\#} change-point methods based on network measures like the mean degree, clustering coeffi- cient, or mean geodesic path length performed poorly, yielding high false negative rates even for large structural changes},",
                  "", "test = {testfield}", "}")
   expect_equal(f, f_expect)




   filePath  <- system.file("testdata/test3.bib", package = "bibDelete")
   file.copy(filePath, "test3.bib", overwrite = TRUE)
   deleteField("test3.bib", "title")
   f         <- readLines("test3_pr.bib")
   f_expect  <- c("@article{Peel:2014ul,", "author = {Peel, Leto and Clauset, Aaron},",
                  "journal = {arXiv},", "year = {2014},", "eprint = {1403.0989},",
                  "eprinttype = {arxiv},", "month = mar", "dec", "nov,", "annote = {{\\#} see conference proceeding papers3://publication/uuid/739AD14E-73B1-4A87-8A5B-6DBD847D0F47",
                  "here, it goes on,", "", "{\\#} corresponding Python code at http://gdriv.es/letopeel/code.html",
                  "", "{\\#} change-point methods based on network measures like the mean degree, clustering coeffi- cient, or mean geodesic path length performed poorly, yielding high false negative rates even for large structural changes},",
                  "", "test = {testfield},", "month = mar,", "dec,", "nov,", "}")
   expect_equal(f, f_expect)




   file.remove(c("test3.bib", "test3_pr.bib"))
})



















test_that("correct output file is generated--pt4", {
   filePath  <- system.file("testdata/test4.bib", package = "bibDelete")
   file.copy(filePath, "test4.bib", overwrite = TRUE)
   deleteField("test4.bib", "annote")
   f         <- readLines("test4_pr.bib")
   f_expect  <- c("@article{Nuzzo:2014bp,", "author = {Nuzzo, Regina},", "title = {{Statistical errors}},",
                  "journal = {Nature},", "year = {2014},", "volume = {506},", "number = {7487},",
                  "pages = {150--152}", "}")
   expect_equal(f, f_expect)




   filePath  <- system.file("testdata/test4.bib", package = "bibDelete")
   file.copy(filePath, "test4.bib", overwrite = TRUE)
   deleteField("test4.bib", "annote")
   deleteField("test4_pr.bib", "author")
   deleteField("test4_pr_pr.bib", "year")
   f         <- readLines("test4_pr_pr_pr.bib")
   f_expect  <- c("@article{Nuzzo:2014bp,", "title = {{Statistical errors}},",
                  "journal = {Nature},", "volume = {506},", "number = {7487},",
                  "pages = {150--152}", "}")
   expect_equal(f, f_expect)
   file.remove(c("test4_original.bib", "test4.bib", "test4_pr.bib", "test4_pr_pr.bib", "test4_pr_pr_pr.bib"))
})
