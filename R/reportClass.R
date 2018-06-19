#This class is used as a container that holds all the reported errors as a list, where each item
#corresponds to a single case and single column.
#
#Columns are identified by name.
#Cases are indentified by string


#' Class that holds the actual data with formatters.

#' Class that holds the actual data with formatters.
ReportClassStorage<-R6::R6Class(
  classname='ReportClassStorage',
  public = list(
    initialize = function(db, casenamesvar=NULL, casenames=NULL) {
      checkmate::assert_data_frame(db)
      private$db_<-db
      private$types_<-list()

      if(!is.null(casenamesvar) && !is.null(casenames)) {
        stop(paste0("Cannot specify both casenames and casenamesvar"))
      }

      if (!is.null(casenames)) {
        checkmate::assert_character(casenames, unique = TRUE)
        private$casenames_<-casenames
      }

      if (!is.null(casenamesvar)) {
        checkmate::assert_string(casenamesvar)
        if(! casenamesvar %in% colnames(db)) {
          stop(paste0("Cannot find casename column ", casenamesvar, " among variables in the data.frame"))
        }
        private$casenames_<-as.character(db[[casenamesvar]])
      }
    },
    declare_type=function(type, caption, parlist, default_formatter_name, defualt_formatter=NULL, flag_requires_cases=TRUE) {
      checkmate::assert_string(type)
      checkmate::assert_string(caption)
      if('character' %in% class(parlist)) {
        parlist<-setNames(rep(alist(,)[1], length(parlist)),parlist )
      }
      checkmate::assert_list(parlist, names = 'unique')
      checkmate::assert_flag(flag_requires_cases)
      if(type %in% names(private$types_)) {
        #Replacing the existing type is not supported right now
        browser()
      } else {
        item<-list(parlist=parlist, requires_cases=flag_requires_cases, caption=caption, formatters=list())
        private$types_<-c(private$types_, setNames(list(item), type))
      }
      if(!missing(defualt_formatter) && !is.null(defualt_formatter)) {
        self$add_formatter(type=type, formatter_name=default_formatter_name, formatter=default_formatter)
      }

    },
    add_formatter=function(type, formatter_name, formatter) {
      #browser()
      if(!type %in% names(private$types_)) {
        #type doesn't exist. First we must define the type
        browser()
      }
      #types<-private$types_
      type_l<-private$types_[[type]]
      if(formatter_name %in% names(type_l$formatters)) {
        #We don't support re-adding the same formatter again
        browser()
      }
      #formatter must be a function that takes varcase_txt argument and a subset of paramaters declared in its type.
      #Moreover, the function must not take argument 'row', and 'col'.
      checkmate::assert_function(formatter, args = c('varcase_txt' ))
      fmls<-names(formals(formatter))
      fmls<-setdiff(fmls, c('...', 'varcase_txt', 'context_df', 'row', 'col'))
      #varcase_txt contains nicely formatted description of the row.
      #context_df is a data.table with one record for each entry to gather in the current row.
      #It contains columns "row", "col", and any other custom column declared in the type.
      if (!all(fmls %in% names(type_l$parlist))) {
        args<-fmls[which(!fmls %in% type_l$parlist)]
        browser()
        cat(paste0(paste0(args, collapse=', '), ' are missing in formatter!'))
      }
      type_l$formatters<-c(type_l$formatters, as.list(setNames(list(formatter=formatter), formatter_name)))
#      types[[type]]<-type_l
      private$types_[[type]]<-type_l
#      browser()
    },
    type_exists=function(type) {
      return(type %in% names(private$types_))
    },
    get_type=function(type) {
      if(type %in% names(private$types_)) {
        return(private$types_[[type]])
      } else {
        return(NULL)
      }
    },

    add_element=function(type, case, var, ...) {
      checkmate::assert_string(type)
      if(!type %in% names(private$types_)) {
        stop(paste0(type, " is not registered report type."))
      }
      type_entry<-private$types_[[type]]
      extra_args<-list(...)
      if(!all(names(extra_args) %in% names(type_entry$parlist))) {
        stop(paste0(paste0(setdiff(extra_args, names(type_entry$parlist)), collapse=", "), " are not registered for the report type ", type))
      }

      checkmate::assert_character(var, null.ok=FALSE)

      if(!is.null(case)) {
        for(scase in case) {
          for(v in var) {
            if(scase>length(private$casenames_) || scase<1){
              browser()
              stop("Case number outside the range. Feed more recent case numbers with $set_case_names()")
            }
            case_str<-private$casenames_[[scase]]
            item<-c(list(type=type, case=case_str, var=v), extra_args)
            private$elements_<-c(private$elements_, list(item))
          }
        }
      } else {
        for(v in var) {
          item<-list(type=type, var=v, par1=par1, par2=par2)
          private$elements_<-c(private$elements_, list(item))
        }
      }
    }
  ),
  active = list(
    elements=function() {private$elements_},
    casenames=function() {private$casenames_},
    db=function() {private$db_}
  ),
  private = list(
    elements_=list(), #Each element is a list with members: case, var, type, par1, par2
    casenames_=list(),
    db_=NULL,
    types_=list()  #Named list of all valid report types. Key is the type name. Each entry is a list with the following fields:
    # parlist - named list of parameters a formatter needs to format its string. These parameters will be required by the addelement. Value is the default argument for the parameter.
    # requires_cases - TRUE/FALSE - whether the type is about the variable as a whole, or concerns a specific cases. TRUE - cases, FALSE - variable as a whole
    # caption - textual information about the type
    # formatters - named list of formatters. First formatter from the list will be treated as default. Each formatter is a list with two elements:
    #             a) formatter - function that formats. It must accept all or subset of declared arguments, and also varcase_txt. Optioanlly can use context_df.
    #                No parameters not present in set c(names(parlist), 'varcase_txt', 'context_df').
    #                Formatter will be called once for every observed combination of its parameters.
  )
)

ReportClassWithVariable<-R6::R6Class(
  classname='ReportClassWithVariable',
  public = list(
    initialize = function(parent, variable) {
      if('ReportClassWithVariable' %in% class(parent)) {
        parent<-parent$base_class
      }
      checkmate::assert_class(parent, classes = c('ReportClassStorage'))



      checkmate::assert_string(variable)

      private$parent_<-parent
      checkmate::assert_true(variable %in% colnames(private$parent_$db))

      private$variable_<-variable

    },
    declare_type=function(type, caption, parlist, default_formatter_name, defualt_formatter, flag_requires_cases=TRUE) {
      private$parent_$declare_type(type, caption, parlist, default_formatter_name, defualt_formatter, flag_requires_cases)
    },
    add_formatter=function(type, formatter_name, formatter) {
      private$parent_$add_formatter(type, formatter_name, formatter)
    },
    type_exists=function(type) {
      private$parent_$type_exists(type)
    },
    add_element=function(type, case=NULL, ...) {
      private$parent_$add_element(type=type, case=case, var=private$variable_, ...)
    }
  ),
  active = list(
    elements=function() {private$parent_$elements},
    casenames=function() {private$parent_$casenames},
    base_class=function() {private$parent_},
    db=function() {private$parent_$db},
    variable=function() {private$variable_}
  ),
  private = list(
    parent_=NULL, #Main object that gathers the report
    variable_=NULL #Default varname
  )
)

# Fabryka wygodnych w użyciu funkcji zbierających wyjątki i je formatujących.
#
# Funkcja produkuje wygodną funkcję pozwalającą na zapisywanie wyjątków do zadanej klasy raportów.
#
# Bezpośrednio przed wyprodukowaniem wyjątkowych obserwacji należy użyć tej funkcji aby dostać funkcję produkującą.
# W argumencie należy podać wszystko, czego ta funkcja by chciała, tj. formattera (lub więcej niż jednego).
# Funkcja jest super-inteligentna i odczytuje argumenty formatterów wraz z ich wartościami domyślnymi i na ich podstawie
# produkuje sensowny wpis typu do klasy.
#
# Należy jednak pamiętać, aby każde wywołanie funkcji dla danego typu miało identyczny zakres formatterów, inaczej
# tylko pierwszy zestaw formatterów zostanie zapamiętany.
#
# Jeśli mamy tylko jeden formatter, to można go podać w argumencie formatters. A gdy więcej, to
# jako nazwaną listę w tymże argumencie
typeReporter_factory <- function(reportClass, type, type_caption, formatters, flag_use_case) {

  if('ReportClassStorage' %in% class(reportClass)) {
    checkmate::assert_r6(reportClass, classes = c('ReportClassStorage'))
  } else if ('ReportClassWithVariable' %in% class(reportClass)) {
    checkmate::assert_r6(reportClass, classes = c('ReportClassWithVariable'))
  } else {
    browser()
  }

  checkmate::assert_string(type)
  checkmate::assert_string(type_caption)
  if(reportClass$type_exists(type)) {
    browser()
    parlist=reportClass$elements
  } else {
    if('function' %in% class(formatters)) {
      formatters<-list(default=formatters)
    } else {
      checkmate::assert_list(formatters, names = 'unique')
    }

    fmls<-list()
    for(i in seq_along(formatters)) {
      formatter<-formatters[[i]]
      checkmate::assert_function(formatter, args = c('varcase_txt' ))
      tmpfmls <- names(formals(formatter))
      if(any(c('row','col')%in%tmpfmls)) {
        stop(paste0("Formatter must not take parameters 'col' and 'row'"))
      }
      fmls[[i]]<-setdiff(tmpfmls, c('...', 'varcase_txt', 'context_df', 'row', 'col'))
    }
    parnames<-unique(unlist(fmls))
    parlist<-setNames(rep(alist(,)[1], length(parnames)),parnames)
    par_set=setNames(rep(FALSE, length(parlist)), parnames)
    for(i in seq_along(formatters)) {
      formatter<-formatters[[i]]
      fmls<-formals(formatter)
      fmls<-fmls[setdiff(names(fmls), c('varcase_txt', 'context_df', 'row', 'col'))]
      for(argname in names(fmls)) {
        if(par_set[[argname]]) {
          if(parlist[[argname]]!=fmls[[argname]]) {
            parlist[[argname]]<-quote(expr=)
          }
        } else {
          par_set[[argname]]<-TRUE
          parlist[[argname]]<-fmls[[argname]]
        }
      }
    }
    rm(par_set, fmls, formatter, parnames)

    reportClass$declare_type(type=type, caption=type_caption, parlist=parlist,
                             flag_requires_cases = TRUE)
    for(i in seq_along(formatters)) {
      fn<-formatters[[i]]
      fn_name<-names(formatters)[[i]]
      reportClass$add_formatter(type=type, formatter_name=fn_name, formatter=fn)
    }
  }
  fn_tmp<-function() {} #Just to get our execution environment


  if(flag_use_case) {
    fn_body<-substitute({
      args<-mget(names(formals()),sys.frame(sys.nframe()))
      do.call(reportClass$add_element, c(list(type=mytype), args))
    }, list(mytype=type))
    if('ReportClassStorage' %in% class(reportClass)) {
      fmls<-c(list(var=quote(expr=), case=quote(expr=)), parlist)
    }else{
      fmls<-c(list(case=quote(expr=)), parlist)
    }
  } else {
    fn_body<-substitute({
      args<-mget(names(formals()),sys.frame(sys.nframe()))
      do.call(reportClass$add_element, c(list(case=NA, type=mytype), args))
    }, list(mytype=type))
    if('ReportClassStorage' %in% class(reportClass)) {
      fmls<-c(list(var=quote(expr=)), parlist)
    }else{
      fmls<-parlist
    }
  }

  fun<-as.function(c(fmls, fn_body), environment(fn_tmp))
  return(fun)
}

