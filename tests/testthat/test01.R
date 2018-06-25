context("simple test")
library(dbcasereport)
library(testthat)

#source('tests/testthat/testfunctions.R')

#source('testfunctions.R')

testthat::test_that("Test", {
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

  formatter<-function(varcase_txt, context_df, nalabel, short=FALSE, ...) {
    if(nrow(context_df)>1 ) {
      have=" have "
    } else {
      have=" has "
    }
    if(short) {
      return(paste0(varcase_txt, have, "suspicious NA label ", nalabel))
    } else {
      return(paste0(varcase_txt, have, "very, very suspicious NA label ", nalabel))
    }
  }
  rap_fun<-typeReporter_factory(reportClass = rcvar, type = 'label_na', type_caption = 'NA labels of labelled',
                                formatters=formatter, flag_use_case=TRUE)

  rap_fun(case = 3, nalabel = "_label_")
  rap_fun(case = 4, nalabel = "_label_")
  rap_fun(case = db$ids[14], nalabel = "_label_")
  rap_fun(case = 14, nalabel = "_label_")

  rap_fun<-typeReporter_factory(reportClass = rcvar, type = 'label_na', type_caption = 'NA labels of labelled',
                                formatters=formatter, flag_use_case=TRUE)
  rap_fun(case = 15, nalabel = "_label_")

  rcvar$elements
  reportClass<-rc

  doc<-ReportGatherer::doc_Document$new(author = 'Ja',  title = 'test', format = 'md')
  compile_report(reportClass=rc, doc=doc, formatter_name='default')

  a<-pander::Pandoc$new()
  doc$render(a)
  ReportGatherer::save_report(a, filename = '/tmp/cos')
})


