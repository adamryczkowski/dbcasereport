context("extra_parameters test")
library(dbcasereport)
library(testthat)

#source('tests/testthat/testfunctions.R')

#source('testfunctions.R')

testthat::test_that("Test for extra_parameters", {
  ans=gen_test_db()
  rcvar=ans$reportClass
  db=ans$db
  formatter<-function(varcase_txt, context_df, nalabel, subset_df, short=FALSE, ...) {
    if(nrow(context_df)>1 ) {
      have=" have "
    } else {
      have=" has "
    }
    if(short) {
      ans<-paste0(varcase_txt, have, "suspicious NA label ", nalabel)
    } else {
      ans<-paste0(varcase_txt, have, "very, very suspicious NA label ", nalabel)
    }
    ans<-paste0(ans, " with ", length(unique(context_df$colour)), " distinct colours.")
    return(ans)
  }
  rap_fun<-typeReporter_factory(reportClass = rcvar, type = 'label_na', type_caption = 'NA labels of labelled', extra_parameters = list(colour=quote(expr=)),
                                formatters=formatter, flag_use_case=TRUE)

  rap_fun(case = 3, nalabel = "_label_", colour = 'blue')
  rap_fun(case = 4, nalabel = "_label_", colour = 'red')
  rap_fun(case = db$ids[14], nalabel = "_label_", colour='red')
  rap_fun(case = 14, nalabel = "_label_", colour='blue')

  rap_fun<-typeReporter_factory(reportClass = rcvar, type = 'label_na', type_caption = 'NA labels of labelled',
                                formatters=formatter, flag_use_case=TRUE)
  rap_fun(case = 15, nalabel = "_label_", colour='white')

  rcvar$elements
  #reportClass<-rcvar

  doc<-ReportGatherer::doc_Document$new(author = 'Ja',  title = 'test', format = 'md')
  compile_report(reportClass=rcvar, doc=doc, formatter_name='default')

  a<-pander::Pandoc$new()
  doc$render(a)
  ReportGatherer::save_report(a, filename = '/tmp/cos')
})
