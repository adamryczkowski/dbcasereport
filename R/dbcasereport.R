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

compile_report<-function(reportClass, doc, report_composer, formatter_name='default') {
  # First stage: we are sorting all entries into separate types. Each type will get its own chapter.
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
                             doc = chapter, report_composer, formatter=formatter)
  }
}

#Compiles report for the given type. All elements are guaranteed to come from the same type.
compile_report_type<-function(elements, type, doc, report_composer, formatter) {

  fmt_args<-formals(formatter)
  fmt_args<-setdiff(names(fmt_args), c('varcase_txt', 'context_df'))

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
    rep<-compile_report_hash(contexts[pos], varcases_l[pos], type, doc, report_composer, formatter)
  }
}

#Compiles report for the given hash. The report is a list that will be fed to the formatting function.
#First it generates a list of rectangles of variables
compile_report_hash<-function(contexts, varcases, type, doc, report_composer, formatter) {
  cols_names<-unique(purrr::map_chr(varcases, 'var'))
  rows_names<-unique(purrr::map_chr(varcases, 'case'))

  rect<-array(integer(1), dim = c(length(rows_nr),length(cols_names)))
  inv_rect<-array(integer(1), dim = c(length(rows_nr),length(cols_names)))

  for(i in seq_along(varcases)) {
    varcase<-varcases[[i]]
    row<-which(varcase$case == rows_nr)
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
    rect_contexts_df<-plyr::count(contexts_df[idx,])
    rect<-list(rows=setNames(rows_nr[rect$rows], names(rect$rows)), cols=rect$cols)
    compile_report_par(rect=rect, type = type, context_df=rect_contexts_df, doc=doc, report_compose = report_composer)
  }
}

compile_report_par<-function(rect, type, context_df, doc, report_compose) {
#  browser()
  txt<-report_composer(rows=rect$rows, cols=rect$cols, type=type, context_df=context_df)
  doc$insert_paragraph(text = txt, tags = as.character(type))
}
