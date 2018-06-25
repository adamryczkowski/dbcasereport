context("Basic functionality test")
library(dbcasereport)
library(testthat)

#source('tests/testthat/testfunctions.R')

#source('testfunctions.R')

testthat::test_that("Basic functionality test", {
  rcvar=gen_test_db()$reportClass
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
