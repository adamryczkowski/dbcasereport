# Function uses fn_fasher on each entry in the reportClass to get
# a set of distinct hashes.
#
# Those hashes gets enveloped in their own R6 class, together with all the
# extra information that got returned by the fn_hasher.
#
# fn_hasher(entry_as_list) is a function that returns a named list with 2 elements: the hash (string)
#    and another named list "context" of additional information to be included in the report.
#    The lists entries will be used as the additional contextual information to be included in the
#    report string. The order of those entries will get mixed, effectively disassociating them from the
#    variable names and case numbers (if you don't want that, simply extend the hash to include those pieces
#    of data, that you don't want to mix)
#    A function must always return a context list with fixed and constant names of the elements.

reserved_parameter_names<-c('varcase_txt', 'context_df', 'subset_df', 'row', 'col')

compile_report<-function(reportClass, doc, varcase_formatter=varcase_formatter_en, formatter_name='default') {
  # First stage: we are sorting all entries into separate types. Each type will get its own chapter.
  if(!checkmate::test_r6(reportClass, classes = 'ReportClassStorage')) {
    reportClass<-reportClass$base_class
  }
  checkmate::assert_r6(reportClass, classes = 'ReportClassStorage')

  elements<-reportClass$elements
  types<-purrr::map_chr(reportClass$elements, 'type')
#  browser()
  types_df<-dplyr::arrange(data.table::as.data.table(table(types)), N)
  names(types_df)<-c('type', 'count')
  for(i in seq_len(nrow(types_df))) {
    type<-types_df$type[[i]]
    pos<-which(types==type)
    type_info<-reportClass$get_type(type)
    caption<-type_info$caption
    chapter<-doc$insert_section(text = caption, tags = type)
    if(formatter_name %in% names(type_info$formatters)) {
      formatter<-type_info$formatters[[formatter_name]]
    } else {
      if('default' %in% names(type_info$formatters)) {
        formatter<-type_info$formatters$default
      } else {
        if(length(type_info$formatters)>0) {
          formatter<-type_info$formatters[[1]]
        } else {
          browser() #No formatters for this type
        }
      }
    }
    rep<-compile_report_type(elements[pos], type, parlist=names(type_info$parlist),
                             doc = chapter, varcase_formatter=varcase_formatter, formatter=formatter, reportClass=reportClass)
  }
}

#Compiles report for the given type. All elements are guaranteed to come from the same type.
compile_report_type<-function(elements, type, parlist, doc, varcase_formatter, formatter, reportClass) {

  fmt_args<-formals(formatter)
  fmt_args<-setdiff(names(fmt_args), c('varcase_txt', 'context_df', 'subset_df'))

  fn_hasher<-function(...) {
    #    browser()
    args<-list(...)[[1]]
    args<-args[fmt_args]
    args<-args[order(names(args))]
    return(digest::digest(args))
  }
  fn_contexts<-function(...) {
    #    browser()
    args<-list(...)[[1]]
    args<-args[parlist]
    args<-args[order(names(args))]
    return(args)
  }
  hashes<-purrr::map_chr(elements, fn_hasher)
  varcases_l<-purrr::map(elements, ~list(case=.$case, var=.$var))
  contexts<-purrr::map(elements, fn_contexts)
  hashes_df<-dplyr::arrange(data.table::as.data.table(table(hashes)), -N)
  names(hashes_df)<-c('hash', 'count')
  for(i in seq_len(nrow(hashes_df))) {
    hash<-hashes_df$hash[[i]]
    pos<-which(hashes==hash)
    rep<-compile_report_hash(contexts[pos], varcases=varcases_l[pos], type=type, doc=doc, varcase_formatter=varcase_formatter, formatter=formatter, reportClass=reportClass)
  }
}

#Compiles report for the given hash. The report is a list that will be fed to the formatting function.
#First it generates a list of rectangles of variables
compile_report_hash<-function(contexts, varcases, type, doc, varcase_formatter, formatter, reportClass) {
  cols_names<-unique(purrr::map_chr(varcases, 'var'))
  rows_names<-unique(purrr::map_chr(varcases, 'case'))

  rect<-array(integer(1), dim = c(length(rows_names),length(cols_names)))
  inv_rect<-array(integer(1), dim = c(length(rows_names),length(cols_names)))

  for(i in seq_along(varcases)) {
    varcase<-varcases[[i]]
    row<-which(varcase$case == rows_names)
    col<-which(varcase$var == cols_names)
    rect[row, col ]<-1L
    inv_rect[row, col]<-i
  }
  colnames(rect)<-cols_names
  rownames(rect)<-rows_names

  rects<-rectpartitions:::get_rectangles_shuffle(rect)
  #Now we have a set of rectangles. We should also compress the contexts too

  contexts_df<-lists2df::lists_to_df(contexts)
  for(rect in rects) {
    pos<-expand.grid(col=rect$cols, row=rect$rows)
#    browser()
    idx<-inv_rect[cbind(pos$row, pos$col)]
    rect_contexts_df<-contexts_df[idx,]
    rect<-list(rows=setNames(rows_names[rect$rows], names(rect$rows)), cols=rect$cols)
    compile_report_par(rect=rect, type = type, context_df=rect_contexts_df, doc=doc, varcase_formatter = varcase_formatter, formatter=formatter, reportClass=reportClass)
  }
}

varcase_formatter_en<-function(rows, cols, in_dt, case_names, language='EN') {
  if(toupper(language)=='EN') {
    col_names_gen<-itemNaming::variable_list_formatter_df_gen(df = in_dt, max_explicit_items_count = 11, number_of_elements_around_ellipsis = c(4,3))
    row_names_gen<-itemNaming::case_list_formatter_df_gen(df = in_dt, case_names = case_names, max_explicit_items_count = 11, number_of_elements_around_ellipsis = c(4,3))
    value_names_gen<-itemNaming::vector_formatter_df_gen()
  } else if (toupper(language)=='PL') {
    browser()
  } else {
    browser()
  }

  col_names_report<-col_names_gen(names(cols))
  row_names_report<-row_names_gen(names(rows))
  #    row_names<-danesurowe::format_case_list(case_names = names(rows), all_cases = rownames(in_dt))

  lengths<-((length(rows)>1)*1 + (length(cols)>1)*2)*1
  # lengths:
  # 0 - one variable, one case, 1 - one variable, two or more cases,
  # 2 - two or more variables, one case, 3 - two or more variables, two or more cases,
  values<- data.table:::subset.data.table(in_dt, case_names %in% rows, names(cols))

  if(toupper(language)=='EN') {
    text=paste0(col_names_report[[1]], " of ", row_names_report[[1]])
  } else if (toupper(language)=='PL') {
    browser()
    if(lengths==0) {
      text=paste0(col_names_report[[1]], " of ", row_names_report[[1]])
    } else if(lengths==1) {
      text=paste0(col_names_report[[1]], " of ", row_names_report[[1]])
    } else if(lengths==2) {
      text=paste0(col_names_report[[1]], " of ", row_names_report[[1]])
    } else if(lengths==3) {
      text=paste0(col_names_report[[1]], " of ", row_names_report[[1]])
    } else {
      browser()
    }
  } else {
    browser()
  }
  return(list(varcase_txt=text, values=values))
}

compile_report_par<-function(rect, type, context_df, doc, varcase_formatter, formatter, reportClass) {
  varcases<-varcase_formatter(rows=rect$rows, cols=rect$cols, in_dt=reportClass$db, case_names=reportClass$casenames)

  fmls<-setdiff(names(formals(formatter)), c(reserved_parameter_names, '...'))
  args_df<-plyr::count(context_df[,fmls])
  if(nrow(args_df)>1) {
    browser() #Something wrong with the context_df
  }
  args_df<-as.list(args_df[setdiff(names(args_df), 'freq')])

  args=c(list(varcase_txt=varcases$varcase_txt, subset_df=varcases$values, context_df=context_df), args_df)
  args<-args[intersect(names(args), names(formals(formatter)))]
  ans<-do.call(formatter,  args = args)
  doc$insert_paragraph(text = ans, tags = as.character(type))
  return(ans)
}
