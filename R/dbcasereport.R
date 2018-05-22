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

compile_report<-function(reportClass, fn_hasher) {
  elements<-reportClass$elements
  types<-purrr::map_chr(reportClass$elements, 'type')
  types_df<-dplyr::arrange(as.data.frame(table(types)), -Freq)
  names(types_df)<-c('type', 'count')
  for(i in seq_len(nrow(types_df))) {
    type<-types_df$type[[i]]
    pos<-which(types==type)
    rep<-compile_report_type(elements[pos], fn_hasher, type)
  }
}

#Compiles report for the given type. All elements are guaranteed to come from the same type.
compile_report_type<-function(elements, fn_hasher, type) {
  hashes_l<-purrr::map(elements, 'type')
  varcases_l<-purrr::map(elements, list(case=.$case, var=.$var))

  hashes<-purrr::map_char(hashes_l, 'hash')
  contexts<-purrr::map(hashes_l, 'context')
  hashes_df<-dplyr::arrange(as.data.frame(table(hashes)), -Freq)
  names(hashes_df)<-c('hash', 'count')
  for(i in seq_len(nrow(hashes_df))) {
    hash<-hashes_df$hash[[i]]
    pos<-which(hashes==hash)
    rep<-compile_report_hash(contexts[pos], varcases_l[pos], type, hash)
  }
}

#Compiles report for the given hash. The report is a list that will be fed to the formatting function.
#First it generates a list of rectangles of variables
compile_report_hash<-function(contexts, varcases, type, hash) {
  cols_names<-purrr::map_char(varcases, 'var')
  rows_nr<-purrr::map_char(varcases, 'case')

  rect<-array(integer(1), dim = c(length(rows_nr),length(cols_names)))

  for(varcase in varcases) {
    rect[which(varcase$case == rows_nr), which(varcase$var == cols_names) ]<-1L
  }

  rects<-rectpartitions:::get_rectangles_shuffle(rect)
  #Now we have a set of rectangles. We should also compress the contexts too

  contexts_df<-objectstorage::lists_to_df(contexts)
  #Perhaps all contexts that are contant should be treated like that. Other should
}
