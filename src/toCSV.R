# final CSV will be saved at `logs_var_name`.csv in the current directory
# (use setwd("path/to/directory") to set, and getwd() to check, the directory
# where the CSV will be saved)
logs_var_name <- 'logs_NEW_23_8654_final'
logs <- get(logs_var_name)

# PRECHECK that all logs are valid sizes
# (if this prints anything, __must__ fix those logs before converting to CSV)
# (this code block will automatically fix common issues with table sizing)
logs <- lapply(seq_along(logs), function(i) {
  log <- logs[[i]]
  
  # check for merged headers (e.g., combined Time Start and End columns yielding
  # "Start End" column rather than separate "Start" and "End" columns) and fix
  # them by splitting by space
  aircraft <- log$aircraft$extracttable_ocr
  if (ncol(aircraft) != 19) {
    cols <- aircraft %>%
      sapply(function(x) any(grepl(". [0-9]", x))) %>%
      which()
    
    for (col in cols) {
      aircraft <- separate(aircraft, col, into = c('l','r'), sep = ' ',
                           extra = 'merge', fill = 'right')
    }
  }
  
  if (ncol(aircraft) != 19) {
    print(paste(i, 'aircraft table'))
  }
  log$aircraft$extracttable_ocr <- aircraft
  
  
  
  # check all activity descriptions tables have four columns as expected
  if (!all(sapply(log$descriptions$extracttable_ocrs,
                  function(tib) { ncol(tib) == 4 }))) {
    print(paste(i, 'descriptions table'))
  }
  
  # check for 17 meta table columns, as expected
  meta <- log$meta$extracttable_ocr
  if (ncol(meta) != 17 & ncol(meta) >= 14) {
    first_row <- meta[1, 14:ncol(meta)]
    second_row <- meta[2, 14:ncol(meta)]
    
    # check if there's just missing co-pilot data as is often the case
    if (((if (is.data.frame(first_row))
      (first_row %>% apply(1, paste, collapse = ' '))
      else first_row)
      == 'Co-Pilot Ser # On Duty Off Duty')
      | (is.character(second_row) &&
         (second_row == '' | second_row == '-' | second_row == ' '))
      | (is.data.frame(second_row) &&
         (second_row %>% apply(1, paste, collapse = ' ') %>% .[[1]] 
          %>% grepl('^( ?- *| ?-? +)$', .))))
      # just fill in with extra columns
      for (col in 1:(17 - ncol(meta))) {
        meta[, paste0(col, '_temp')] = ''
      }
  }
  
  if (ncol(meta) != 17) {
    print(paste(i, 'meta table'))
  }
  log$meta$extracttable_ocr <- meta
  
  # insert fixed log
  logs[[i]] <- log
})




output_csv <- NULL

for (i in 1:length(logs)) {
  log <- logs[[i]]
  # print(i)
  
  # special checks
  if ('SOME_MISSING_ACTIVITIES_RECAP' %in% log$specials
      & !('REDACTED_ACTIVITIES_DESC' %in% log$specials)) {
    stop(paste(i, 'Some missing activities recap yet descriptions aren\'t redacted!'))
    # since this means we wouldn't be able to match some activities to their flights,
    # likely. as with the below case, we could probably write code to check this.
  }
  
  if ('REDACTED_ACTIVITIES_DESC_CONTD' %in% log$specials
      & !('REDACTED_ACTIVITIES_DESC' %in% log$specials)) {
    stop(paste(i, 'Second page of activites description redacted yet first isn\'t!'))
    # note that realistically we should just note that some activities are redacted
    # and still process those that are present. in practice it seems the redacted_contd
    # implies the redacted first page always. but if this isn't the case, we just
    # need to update the following code to handle this case.
  }
  
  
  
  # aircraft info table getting event times
  flight_times <- log$aircraft$extracttable_ocr
  
  if (ncol(flight_times) != 19) {
    stop(paste(i, 'Aircraft info table does not have 19 columns.'))
  }
  
  flight_times <- flight_times %>%
    slice(3:n()) %>%
    mutate(flight = `1`,
           f_time_start = as.integer(`2`),
           f_time_end = as.integer(`3`)) %>%
    select(flight, f_time_start, f_time_end)
  
  
  if (!('NO_ACTIVITIES_RECORDED' %in% log$specials
        | 'NO_ACTIVITIES_DESC' %in% log$specials
        | 'REDACTED_ACTIVITIES_DESC' %in% log$specials)) {
    # activities recap table getting times for activities
    activity_times <- log$recap$local_ocrs %>%
      bind_rows() %>%
      mutate(act_no = lag(word, 2),
             cur_match = grepl("^[0-9]{4}$", word),
             next_ = lead(word),
             next_match = grepl("^[0-9]{4}$", next_)) %>%
      filter(cur_match & next_match) %>%
      select(act_no, act_time_start = word, act_time_end = next_) %>%
      mutate(act_time_start = as.integer(act_time_start),
             act_time_end = as.integer(act_time_end)) %>%
      # fill in missing (invalid detected) act. numbers using entry before or after
      mutate(act_no = ifelse(is.na(suppressWarnings(as.integer(act_no))),
                             ifelse(is.na(suppressWarnings(as.integer(lag(act_no)))),
                                    ifelse(is.na(suppressWarnings(as.integer(lead(act_no)))),
                                           '<invalid>',
                                           as.character(suppressWarnings(as.integer(lead(act_no))) - 1)),
                                    as.character(suppressWarnings(as.integer(lag(act_no)) + 1))),
                             act_no))
    
    if (any(activity_times$act_no == '<invalid>')) {
      stop(paste(i, 'Invalid activity times table numbers.'))
    }
    
    # checking all events there (wait don't actually do this)
    # activity_times %>%
    #   mutate(is_ascending = as.integer(act_no) < as.integer(lag(act_no)) + 1) %>%
    #   all(.$is_ascending, na.rm = TRUE)
    
    
    
    # mutate descriptions to contain correct aircraft info log
    descriptions <- log$descriptions$extracttable_ocrs
    
    if (!all(sapply(descriptions, function(tib) { ncol(tib) == 4 }))) {
      stop(paste(i, 'Activity descriptions tables do not have 4 columns'))
    }
    
    descriptions <- descriptions %>%
      lapply(function(tib) { slice(tib, 2:n()) }) %>%
      bind_rows() %>%
      left_join(activity_times, by = c(`1` = 'act_no')) %>%
      rowwise() %>%
      mutate(flight = ifelse(length(filter(flight_times, f_time_start <= act_time_start
                                           & f_time_end >= act_time_end)$flight) != 0,
                             filter(flight_times, f_time_start <= act_time_start
                                    & f_time_end >= act_time_end)$flight,
                             '?')) %>%
      ungroup()
    names(descriptions)[1:4] <- c('act_no', 'location', 'area', 'comments')
    
    # note, this is the handling of flights used:
    #  - activities that don't have a matching flight (by start/end time) get '?' as flight # (above)
    #  - flights that aren't matched to any activities get a log entry (below)
    # often times there's only two flights and they *should* be matched to all activities,
    #   there's just sometimes weird clerical errors (I assume?) w flight/end times. so.
    #   this could be handled different to actually match it up.
    for (fl_num in setdiff(unique(flight_times$flight), unique(descriptions$flight))) {
      descriptions <- descriptions %>%
        rbind(data.frame(act_no = '', location = '', area = '',
                         comments = paste0('***no activities found to match flight ', fl_num, '***'),
                         act_time_start = '', act_time_end = '',
                         flight = as.character(fl_num)))
    }
  } else {
    if (length(log$descriptions$extracttable_ocrs) != 0) {
      stop(paste(i, 'Expected no activity description table based on specials yet there is one!'))
    }
  
    if ('NO_ACTIVITIES_RECORDED' %in% log$specials) {
      descriptions <- data.frame(act_no = '', location = '', area = '',
                                 comments = c('***no activities recorded for this log***'),
                                 act_time_start = '', act_time_end = '')
    } else if ('NO_ACTIVITIES_DESC' %in% log$specials) {
      descriptions <- data.frame(act_no = '', location = '', area = '',
                                 comments = c('***activity comments page missing for this log***'),
                                 act_time_start = '', act_time_end = '')
    } else { # REDACTED_ACTIVITIES_DESC
      descriptions <- data.frame(act_no = '', location = '', area = '',
                                 comments = c('***activity comments redacted for this log***'),
                                 act_time_start = '', act_time_end = '')
    }
    
    descriptions <- descriptions[rep(1, nrow(flight_times)), ] %>%
      cbind(select(flight_times, flight))
  }
  
  # retain original row orderings
  descriptions$order_id <- 1:nrow(descriptions)
  
  aircraft <- log$aircraft$extracttable_ocr
  # already checked columns earlier w flight_times
  
  # cutting off last 6 columns because OCR doesn't pick them up (checkboxes)
  names(aircraft) <- c('flight', 'flight_time_start', 'flight_time_end', 'hobbs_meter_start',
                       'hobbs_meter_end', 'flight_time', 'fuel_loc', 'fuel_gal',
                       'abort_code', 'abort_time', 'maint_hobbs_start', 'maint_hobbs_end',
                       'landings')
  aircraft <- aircraft[, -c(14:19)]
  
  descriptions <- merge(aircraft, descriptions, by = 'flight', all.y = TRUE)
  
  # add descriptions meta table
  if ('NO_ACTIVITIES_DESC' %in% log$specials) {
    descriptions <- data.frame('full_tfo' = '', 'full_pilot' = '', 'full_date' = '',
                               'full_watch' = '', 'full_assignment' = '') %>%
      cbind(descriptions)
  } else {
    desc_meta <- log$description_meta$local_table %>%
      data.frame()
    names(desc_meta) <- paste0('full_', tolower(names(desc_meta)))
    descriptions <- desc_meta %>%
      cbind(descriptions)
  }
  
  # add meta table
  meta <- log$meta$extracttable_ocr
  
  if (ncol(meta) != 17) {
    stop(paste(i, 'Meta table does not have 17 columns'))
  }
  
  names(meta) <- c('date', 'watch', 'assign', 'ac_num', 'w/c', 'tfo', 'tfo_ser_num', 'tfo_on_duty',
                   'tfo_off_duty', 'pilot', 'pilot_ser_num', 'pilot_on_duty', 'pilot_off_duty',
                   'co_pilot', 'co_pilot_ser_num', 'co_pilot_on_duty', 'co_pilot_off_duty')
  
  descriptions <- meta %>%
    slice(-1) %>%
    .[rep(1, nrow(descriptions)), ] %>%
    cbind(descriptions)
  
  # add scraper meta
  log_meta <- log$log_meta
  log_meta$log_number <- i
  log_meta$present_pages <- log_meta$present_pages %>%
    paste(collapse = ',')
  log_meta$num_pages <- log_meta$num_pages %>%
    paste(collapse = ',')
  log_meta <- data.frame(log_meta) %>%
    .[, c('file_start_page', 'file_end_page', 'log_number', 'present_pages',
          'num_pages', 'filename', 'file_total_pages')]
  names(log_meta) <- paste0('meta_', names(log_meta))
  
  descriptions <- descriptions %>%
    cbind(log_meta[rep(1, nrow(descriptions)), ])
  
  # sort by original ordering
  descriptions <- descriptions[order(descriptions$order_id), ]
  descriptions$order_id <- NULL
  
  # add to CSV!
  output_csv <- output_csv %>%
    rbind(descriptions)
}

write.csv(output_csv, paste0(logs_var_name, '.csv'), row.names = FALSE)
