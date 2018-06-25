library(testthat)
library(dbcasereport)

gen_test_db<-function() {
  n=100
  ids=paste0(stringr::str_pad(seq_len(n), width = 3, pad = "0"), sample(LETTERS, size = n, replace=TRUE), sample(LETTERS, n, replace=TRUE), sample(0:9, n, replace=TRUE))
  lab=sample(c(haven::tagged_na("a", "c", "b", "z"), NA, 1:4), size=n, replace=TRUE)
  lab=labelled::labelled(lab, c("Agreement" = 1, "Disagreement" = 4, "First" = haven::tagged_na("c"),
                                "Refused" = haven::tagged_na("a"), "Not home" = haven::tagged_na("z"), "Just don't know"=NA))


  db<-data.table::data.table(ids=ids,
                             fac=as.factor(sample(c("lewa","środkowa","prawa"), replace=TRUE, size=n)),
                             lab=lab, num=runif(n = n),
                             int1=rpois(n=100, 10.3), int2=rpois(n=100, 15.3), int3=rpois(n=100, 20.3))

  rc<-dbcasereport::ReportClassStorage$new(db=db, casenamesvar='ids')
  rcvar<-ReportClassWithVariable$new(parent=rc, variable='lab')
  return(list(db=db, reportClass=rcvar))
}



test_check("dbcasereport")

