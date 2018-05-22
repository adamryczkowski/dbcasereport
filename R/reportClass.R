#This class is used as a container that holds all the reported errors as a list, where each item
#corresponds to a single case and single column.
#
#Columns are identified by name.
#Cases are indentified by integer.
ReportClass<-R6::R6Class(
  classname='ReportClass',
  public = list(
    initialize = function() {
      #do nothing
    },
    set_case_names=function(casenames) {
      checkmate::assert_character(casenames, unique = TRUE)
      private$casenames_<-casenames
    },
    add_element=function(type, case, var, par1=character(0), par2=character(0)) {
      checkmate::assert_integer(case)
      checkmate::assert_character(var)
      checkmate::assert_string(type)
      for(scase in case) {
        for(v in var) {
          item<-list(type=type, case=scase, var=v, par1=par1, par2=par2)
          private$elements_<-c(private$elements_, list(item))
        }
      }
    }
  ),
  active = list(
    elements=function() {private$elements_},
    casenames=function() {private$casenames_}
  ),
  private = list(
    elements_=list(), #Each element is a list with members: case, var, type, par1, par2
    casenames_=list()
  )
)

general_hash_fn<-function(el) {
  return(list(hash=digest::digest(list(type=el$type, par1=el$par1, par2=el$par2)), context=list(par1=el$par1, par2=el$par2)))
}