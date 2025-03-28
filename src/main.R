library(tidyverse)

source('./extracttable_functions.R')

# !! set extracttable_api_key before running
# extracttable_api_key <- 'key_here'

# --- config options ---

# whether or not to call ExtractTable and save results
CALL_EXTRACTTABLE <- TRUE
# path to PDF to scrape
PDF_PATH <- '~/Desktop/Projects/CE/heli-testing/23-8654-Final.pdf'
# whether or not to prompt in console after each image is cropped
PROMPT_AFTER_CROP <- FALSE
# whether or not to save images of sections of PDF
SAVE_IMAGES <- FALSE
# where to save images to (ensure the folder is created)
SAVED_IMAGE_PATH <- 'test_images'

###########################
### FUNCTIONS AND SETUP ###
###########################

# https://stackoverflow.com/a/44152358 to emulate C(++) enum
Enum <- function(...) {
  values <- sapply(match.call(expand.dots = TRUE)[-1L], deparse)
  
  stopifnot(identical(unique(values), values))
  
  res <- setNames(seq_along(values), values)
  res <- as.environment(as.list(res))
  lockEnvironment(res, bindings = TRUE)
  res
}

# sections of flight log: flight metadata, aircraft info, activities recap (1st
#   page, later pages, ending table), activity descriptions (1st page, later pages)
SECT <- Enum(META, AIRCRAFT, RECAP_START, RECAP_CONTD, RECAP_END, DESC_START, DESC_CONTD)
# now sections is an "enum" w the four main sections of the pdf log

# check whether input set of words is within provided bounds of OCR'ed data;
# if not, stop the program with an error.
#   ocr_data: OCR data to check through, from local (Tesseract) OCR
#   words: array of words to check for (should be lowercase if case_sensitive
#     is FALSE or not provided)
#   top_lim: y coordinate (starting from top) to check below
#   bot_lim: y coordinate (starting from top) to check above
#   thresh: minimum number of words found to consider a success
#   err: error message to stop program with if threshold not reached
#   case_sensitive: whether to perform case-sensitive checking (default false)
# returns the list of matching words
check_for_text <- function(ocr_data, words, top_lim, bot_lim, thresh, err,
                           case_sensitive = FALSE) {
  if (case_sensitive) {
    matching <- ocr_data %>%
      filter(word %in% words & top > top_lim & bot < bot_lim)
  } else {
    matching <- ocr_data %>%
      filter(tolower(word) %in% words & top > top_lim & bot < bot_lim)
  }
  
  if (nrow(matching) < thresh) {
    stop(err)
  }
  
  return (matching)
}

# generate and print preview text for current section of PDF, and if enabled,
# save image with that same name and prompt to continue
#   image: image of currently cropped section
#   page: current page number
#   sect: current section (of type SECT)
#   meta: whether in the "meta" section of activity descriptions
# also depends on global config variables SAVE_IMAGES and PROMPT_AFTER_CROP
# returns: preview text for current section
preview <- function(image, page, sect, meta = FALSE) {
  sect <- switch(sect, 'meta', 'aircraft', 'recap start', 'recap contd',
                 'recap end', 'description start', 'description contd')
  
  if (meta)
    preview_text <- paste('Page', page, sect, 'meta')
  else
    preview_text <- paste('Page', page, sect)
  
  print(preview_text)
  
  if (SAVE_IMAGES)
    magick::image_write(image, path = file.path(SAVE_IMAGE_PATH, paste0(preview_text, '.png')))
  
  if (PROMPT_AFTER_CROP)
    readline(prompt="Press [enter] to continue")
  
  return (preview_text)
}

# crop 50px off the left (there's usually a noticeable paper edge there, from
# scanning -- this should never cut into data)
#   image: image to crop
#   height: height of image
#   y_offset: y cropping from the top, if applicable (default 0)
# returns: cropped image (Magick)
simple_crop <- function(image, height, y_offset = 0) {
  return (magick::image_crop(image, geometry = paste0(3250, 'x', height,'+50+', y_offset)))
}

# get_table: Get the OCRed table from ExtractTable in an R-readable format
# optionally, find word- and row-level redactions and include these
#   image: The ImageMagick image of the table to extract text from
#   find_redactions (default false): Whether to also find redactions within
#     the image and insert them into the returned table text. At present should
#     only be used for the activity comments section.
get_table <- function(image, find_redactions = FALSE) {
  et_resp <- image %>%
    ExtractTable(extracttable_api_key)
  api_key_usage <<- api_key_usage + 1
  
  if (find_redactions) {
    table <- get_redacted_table(et_resp$full_resp, image)
  } else {
    table <- et_resp %>%
      .$tables %>%
      .[[1]]
  }
  
  # sort cols/rows
  table <- table %>%
    select(sapply(sort(as.integer(names(.))), toString)) %>%
    .[order(as.numeric(row.names(.))),]
  # rename cols/rows to match R indices
  names(table) <- as.integer(names(table)) + 1
  rownames(table) <- as.integer(row.names(table)) + 1
  
  return (list(
    table = table,
    response = et_resp$full_resp
  ))
}

# "crop" Tesseract OCR data to within supplied coordinate bounds (i.e., return
# only the words within the given bounds in the data)
#   ocr_data: Tesseract data to crop
#   top_lim: furthest up possible y-coordinate (lowest number; starting from 0)
#   bot_lim: furthest down possible y-coordinate (highest number)
#   left_lim: furthest left possible x-coordinate (lowest number)
#   right_lim: furthest right possible x-coordinate (highest number)
# returns: the subset of ocr_data that lies within supplied bounds
crop_local_ocr <- function(ocr_data, top_lim, bot_lim, left_lim, right_lim) {
  if (missing(left_lim) | missing(right_lim)) {
    return (filter(ocr_data, top > top_lim & bot < bot_lim))
  } else {
    return (filter(ocr_data, top > top_lim & bot < bot_lim
                            & left >= left_lim & right <= right_lim))
  }
}

######################################
### WORD-LEVEL REDACTION DETECTION ###
######################################

# generate a string to represent an in-text redaction of given width, using a
# average character width of 16.32 pixels (see readme for details)
#   width: the width, in pixels, of the redaction
# returns: string in the format "[***]" where there is approximately one asterisk
#   per redacted character
create_redaction_string <- function(width) {
  return(paste0(
    "[",
    strrep("*", round(width / 16.32)),
    "]"
  ))
}

# Note: This function at present is built around ONLY the activity comments section;
# with minor changes it could be adapted to detect redactions in other sections
# 
# In particular, the row redaction detection uses above/below row activity numbers
# to describe redacted row location, and inserts info about missing rows in the
# "comments" column; the fourth column for this section. The remaining components
# are also given entire rows and denoted in the "comments" column.
#
# get_redacted_table: Process a response from ExtractTable, in tandem with the original
# image used to get that response, to output a table with word- and row-level redactions
# detected and included. (See readme for more details)
#   parsed_resp: The raw response from the ExtractTable API
#   img: The image used to get the aforementioned response
# returns: table of text data, as a dataframe, with redactions inserted
get_redacted_table <- function(parsed_resp, img) {
  # process image to only contain black squares
  f_ <- tempfile(fileext = ".png")
  img %>%
    magick::image_transparent('#000', 5) %>%
    magick::image_channel(channel = 'alpha') %>%
    magick::image_negate() %>%
    magick::image_write(f_)
  
  # detect connected components (redaction squares) -- in white
  components <- system2('magick', stdout = TRUE,
                        paste0(f_, ' ',
                               '-define connected-components:verbose=true ',
                               '-define connected-components:exclude-header=true ',
                               '-define connected-components:exclude-ids=true ',
                               '-define connected-components:sort=area ',
                               '-define connected-components:sort-order=decreasing ',
                               '-connected-components 4 ', f_)
  )
  
  # extract connected component info
  pattern <- '  ([0-9]+)x([0-9]+)\\+([0-9]+)\\+([0-9]+) ([0-9]+\\.[0-9]),([0-9]+\\.[0-9]).+gray\\((0|255)\\)'
  components <- regmatches(components, regexec(pattern, components))
  components <- data.frame(
    width = sapply(components, function(x) as.numeric(x[2])),
    height = sapply(components, function(x) as.numeric(x[3])),
    x = sapply(components, function(x) as.numeric(x[4])),
    y = sapply(components, function(x) as.numeric(x[5])),
    center_x = sapply(components, function(x) as.numeric(x[6])),
    center_y = sapply(components, function(x) as.numeric(x[7])),
    valid = sapply(components, function(x) x[8] == '255')
  ) %>%
    transform(area = x * y) %>%
    # filter out invalid (non-black areas) and too small/large of areas
    filter(valid & width > 5 & height > 5 & height < 1000)
  components$valid <- NULL
  
  # remove temp file
  if (file.exists(f_)){
    file.remove(f_)
  }
  
  # if no redactions detected, return table as is
  if (nrow(components) == 0) {
    table <- parsed_resp$Tables$TableJson %>%
      sapply(unlist) %>%
      t() %>%
      as.data.frame()
    return(table)
  }
  
  #   == insert redactions into text ==
  
  width <- magick::image_info(img)$width
  height <- magick::image_info(img)$height
  
  # calculate bounds of each "line" from ExtractTable data
  bounded_lines <- parsed_resp[["Lines"]][["LinesArray"]][[1]] %>%
    mutate(
      x_min_raw = sapply(WordsArray, function(words_arr) {
        min(sapply(words_arr$Loc, function(loc) loc[1]))
      }),
      x_max_raw = sapply(WordsArray, function(words_arr) {
        max(sapply(words_arr$Loc, function(loc) loc[3]))
      }),
      y_min_raw = sapply(WordsArray, function(words_arr) {
        min(sapply(words_arr$Loc, function(loc) loc[2]))
      }),
      y_max_raw = sapply(WordsArray, function(words_arr) {
        max(sapply(words_arr$Loc, function(loc) loc[4]))
      }),
      x_min = round(x_min_raw * width),
      x_max = round(x_max_raw * width),
      y_min = round(y_min_raw * height),
      y_max = round(y_max_raw * height)
    )
  
  # insert redactions into cells where detected redaction component overlaps
  redacted_cells <- lapply(parsed_resp[["Tables"]][["TableCoordinates"]], function(row) {
    lapply(row, function(cell) { # cell: given-to-us coords of a table cell (as proportions)
      if (length(cell[[1]]) == 0 | nrow(components) == 0)
        return(NULL)
      
      # determine which redactions are for this cell of the table
      redact_overlaps <- which(
        mapply(function(center_x, center_y, cell) {
          center_x >= cell[1] * width && center_x <= cell[3] * width &&
            center_y >= cell[2] * height && center_y <= cell[4] * height
        }, components$center_x, components$center_y, MoreArgs = list(cell = cell[[1]]))
      )
      
      if (length(redact_overlaps) == 0) {
        return(NULL)
      }
      
      # get lines that fall within cell bounds
      lines <- bounded_lines[which(mapply(function(x_min, x_max, y_min, y_max, cell) {
        x_min >= cell[1] && x_max <= cell[3] &&
          y_min >= cell[2] && y_max <= cell[4]
      }, bounded_lines$x_min_raw, bounded_lines$x_max_raw, bounded_lines$y_min_raw,
      bounded_lines$y_max_raw, MoreArgs = list(cell = cell[[1]]))), ]
      
      redactions <- components[redact_overlaps, ]
      # remove overlapping components to keep track of which components are matched
      components <<- components[-redact_overlaps, ]
      final <- "";
      
      # loop through lines and add redactions, https://stackoverflow.com/a/1699296
      by(lines, seq_len(nrow(lines)), function (line) {
        if (nrow(redactions) == 0) {
          final <<- paste(final, line$Line)
          return()
        }
        
        # get redactions before this line
        redactions_before_idx <- which(mapply(function(center_y, x, width) {
          (center_y <= line$y_max && (x + width) <= line$x_min) ||
            center_y <= line$y_min
        }, redactions$center_y, redactions$x, redactions$width))
        
        if (length(redactions_before_idx) > 0) {
          redactions_before <- redactions[redactions_before_idx, ] %>%
            arrange(x) # sort horizontally
          
          redaction_text <- redactions_before$width %>%
            lapply(create_redaction_string) %>%
            paste()
          
          final <<- paste(final, redaction_text, line$Line) %>% trimws()
          redactions <<- redactions[-redactions_before_idx, ]
        } else {
          final <<- paste(final, line$Line)
        }
      })
      
      # add remaining redactions to end of string
      if (nrow(redactions) > 0) {
        final <- paste(final, create_redaction_string(sum(redactions$width)))
      }
      
      return(trimws(final))
      
      # return(list( # useful for debugging
      #   new_text = final,
      #   cell = cell[[1]],
      #   redactions = redactions,
      #   lines = lines
      # ))
    })
  }) %>%
    lapply(function(row) {
      row[!sapply(row, is.null)] # remove non-redacted cells
    }) %>%
    .[lengths(.) > 0] # actually remove those cells
  
  # row-level redactions
  # initial idea: assuming required width of 2500:
  # row_redactions <- filter(components, width > 2500) %>% # wide enough to be a whole row
  #   arrange(y)
  # components <- filter(components, width <= 2500) # update remaining components
  # now: inserting all remaining redactions, but distinguishing those that don't
  #   seem wide enough to be a row when writing text (see "insert redacted rows"
  #   section below)
  row_redactions <- components %>% arrange(y)
  
  # get middle of each detected row's coordinates
  average_row_y <- sapply(parsed_resp[["Tables"]][["TableCoordinates"]], function(row) {
    sapply(row, function(col) mean(c(col[[1]][2], col[[1]][4]))) %>%
      mean() %>%
      `*`(height) %>%
      round() %>%
      return()
  }) %>%
    sort()
  
  # get rows above and below for each detected full-row redaction
  row_redactions$row_above <- sapply(row_redactions$center_y, function(y_center) {
    (y_center - average_row_y) %>%
      .[. > 0] %>%
      which.min() %>%
      names()
  })
  row_redactions$row_below <- sapply(row_redactions$center_y, function(y_center) {
    (average_row_y - y_center) %>%
      .[. > 0] %>%
      which.min() %>%
      names()
  })
  
  # find "Act No." for row above and row below
  row_redactions$row_above_act <- sapply(row_redactions$row_above, function(row_above) {
    if (is.null(row_above))
      return(NULL)
    parsed_resp[["Tables"]][["TableJson"]][[row_above]][["0"]]
  })
  row_redactions$row_below_act <- sapply(row_redactions$row_below, function(row_below) {
    if (is.null(row_below))
      return(NULL)
    parsed_resp[["Tables"]][["TableJson"]][[row_below]][["0"]]
  })
  
  # update response to include word-level redactions
  to_update <- parsed_resp
  
  for (row in names(redacted_cells)) {
    for (col in names(redacted_cells[[row]])) {
      if (row %in% names(to_update[["Tables"]][["TableJson"]]) &&
          col %in% names(to_update[["Tables"]][["TableJson"]][[row]])) {
        to_update[["Tables"]][["TableJson"]][[row]][[col]] <-
          redacted_cells[[row]][[col]]
      }
    }
  }
  
  # turn into dataframe, sort rows
  to_update <- to_update$Tables$TableJson %>%
    sapply(unlist) %>%
    t() %>%
    as.data.frame() %>%
    .[order(as.numeric(row.names(.))), ]
  
  # insert redacted rows into response
  by(map_df(row_redactions, rev), seq_len(nrow(row_redactions)), function(row) {
    if (row$width > 2500) {
      text <- "***redacted row(s) "  
    } else {
      text <- "***unknown redaction "
    }
    
    text <- paste0(text, row$width, "x", row$height, " pixels at (", row$x,
                   ", ", row$y, ")")
    
    some_rows <- FALSE
    
    if ("row_above" %in% colnames(row)) {
      if (startsWith(tolower(row$row_above_act), "act"))
        text <- paste(text, "below header row")
      else
        text <- paste(text, "below activity", row$row_above_act)
      some_rows <- TRUE
    }
    
    if ("row_below" %in% colnames(row)) {
      text <- paste(text, "above activity", row$row_below_act)
      some_rows <- TRUE
    }
    
    if (!some_rows) {
      print("ERROR: No rows above nor below detected redacted row. There should be.")
      print("Printing row component info:")
      print(row)
    }
    
    text <- paste0(text, "***")
    
    if ("row_below" %in% colnames(row)) {
      to_update <<- to_update %>%
        add_row("3" = text, .before = which(rownames(.) == row$row_below))
    } else {
      to_update <<- to_update %>%
        add_row("3" = text) 
    }
  })
  
  # rename rows to account for added rows
  rownames(to_update) <- as.character(0:(nrow(to_update) - 1))
  
  return(to_update)
}

#################
### MAIN CODE ###
#################

pages <- pdftools::pdf_info(PDF_PATH)$pages

api_key_usage <- 0

cur_sect <- SECT$META
page_already_setup <- FALSE
no_activities <- FALSE

logs <- list()

page <- 0
while (page < pages) {
  if (page_already_setup) {
    page_already_setup <- FALSE
  } else {
    # log page number detection metadata (adding for *previous page*)
    # -- if OCR successfully detects numbers in positions for Page x of y, save
    #    those numbers for future reference
    if (page != 0) {
      bottom_pages_text <- suppressWarnings(local_ocr %>%
                                              crop_local_ocr(2350, 2550, 1500, 1800) %>%
                                              tail(3) %>% .[[1]] %>% .[c(1,3)] %>%
                                              as.integer())
      
      if (!is.na(bottom_pages_text[1]))
        cur_log$log_meta$present_pages <- cur_log$log_meta$present_pages %>%
        c(bottom_pages_text[1])
      if (!is.na(bottom_pages_text[2]))
        cur_log$log_meta$num_pages <- cur_log$log_meta$num_pages %>%
        c(bottom_pages_text[2])
    }
    
    # setup new page
    page <- page + 1
    
    image <- PDF_PATH %>%
      magick::image_read_pdf(pages=page)
    info <- image %>%
      magick::image_info()
    
    if (info$width != 3300 | info$height != 2550) { # CONST
      stop(paste0('Unexpected image dimensions: ', info$width, 'x', info$height))
    }
    
    local_ocr <- tesseract::ocr_data(image) %>%
      separate(bbox, into = c('left', 'top', 'right', 'bot'), sep = ',', convert = TRUE)
  }
  
  
  
  
  
  # log saving/updating
  if (cur_sect == SECT$META) {
    # --- add old log
    if (page != 1) {
      cur_log$log_meta$file_end_page <- page - 1
      logs <- append(logs, list(cur_log))
    }
    
    # --- set up new log
    cur_log <- list(
      log_meta = list(
        filename = basename(PDF_PATH),
        file_total_pages = pages,
        file_start_page = page,
        present_pages = c(),
        num_pages = c()
      ),
      specials = c(),
      recap = list(images = c(), local_ocrs = list(), extracttable_ocrs = list()),
      descriptions = list(images = c(), local_ocrs = list(), extracttable_ocrs = list())
    )
    # other properties created on the fly (meta/aircraft/recap_end/description_meta)
    
    no_activities <- FALSE
  }
  
  
  
  
  
  # flight metadata
  if (cur_sect == SECT$META) {
    # count number of rows matching the following conditions:
    #   is one of Air Support Division Daily Log
    #   y pixel height is between 50 and 250
    # and if count is < 3 ERROR (description of check_for_text fxn)
    title_words <- c('air', 'support', 'division', 'daily', 'log')
    title <- check_for_text(local_ocr, title_words, 50, 250, 3,
                            'Could not find Air Support Division Daily Log header')
    
    # select bounds for info table, 100 to 250 px below above
    log_header_bot <- title$bot %>%
      mean() %>%
      round()
    table_top <- log_header_bot + 100
    table_bot <- table_top + 150
    
    # count number of table headers matching the following:
    #   is one of the table headers
    #   within selected bounds
    # if count < 15 (?? 17 total headers but i think that suffices) ERROR
    meta_table_headers <- c('date', 'watch', 'assign', 'ac', '#', 'w/c', 'tfo', 'ser', '#',
                            'ser#', 'on', 'duty', 'off', 'onduty', 'offduty', 'pilot', 'co-pilot') # CONST
    check_for_text(local_ocr, meta_table_headers, table_top, table_bot, 15,
                   'Could not find meta table headers')
    
    # crop image
    table_image <- image %>%
      simple_crop(150, table_top) # CONST (same as in table_bot line)
    
    # save metadata
    cur_log$meta$image <- preview(table_image, page, cur_sect)
    cur_log$meta$local_ocr <- crop_local_ocr(local_ocr, table_top, table_bot)
    
    # call extracttable
    if (CALL_EXTRACTTABLE)
      cur_log$meta$extracttable_ocr <- get_table(table_image)
    
    cur_sect <- SECT$AIRCRAFT
  }
  
  
  
  
  
  # aircraft information
  if (cur_sect == SECT$AIRCRAFT) {
    aircraft_words <- c('aircraft', 'information')
    
    aircraft_local <- check_for_text(local_ocr, aircraft_words, 450, 700, 2,
                                     'Could not find Aircraft Information header')
    
    header_bot <- aircraft_local$bot %>%
      mean() %>%
      round()
    
    activities_words <- c('Activities', 'Recap')
    activities_local <- check_for_text(local_ocr, activities_words, header_bot + 300, Inf, 2,
                                       'Could not find Activities Recap header', TRUE)
    
    activities_top <- activities_local$top %>%
      mean() %>%
      round()
    
    # bound between aircraft info header and activities recap header
    table_top <- header_bot + 50
    table_bot <- activities_top - 50
    
    if (table_bot - table_top < 0) {
      stop('Negative number of pixels between top and bottom of aircraft info table.')
    }
    
    aircraft_table_headers <- c('fit', 'flt', '#', 'time', 'hobbs', 'meter', 'flight',
                                'start', 'end', 'flight', 'time', 'fuel', 'abort',
                                'code', 'maint', 'landings', 'ac', 'man.', 'adaplog',
                                'fdms', 'cc', 'headsets', 'panel', 'cover')
    check_for_text(local_ocr, aircraft_table_headers, table_top, table_bot, 18,
                   'Could not find meta table headers')
    
    table_image <- image %>%
      simple_crop(table_bot - table_top, table_top)
    
    cur_log$aircraft$image <- preview(table_image, page, cur_sect)
    cur_log$aircraft$local_ocr <- crop_local_ocr(local_ocr, table_top, table_bot)
    
    if (CALL_EXTRACTTABLE)
      cur_log$aircraft$extracttable_ocr <- get_table(table_image)
    
    cur_sect <- SECT$RECAP_START
  }
  
  
  
  
  
  # beginning of activities recap
  if (cur_sect == SECT$RECAP_START) {
    if (!exists('activities_local') | !exists('activities_top')) {
      stop('No activities recap header information when there should be')
    }
    
    if (!exists('log_header_bot')) {
      stop('No top header information when there should be')
    }
    
    # rough estimate, should be low enough
    footer_top <- log_header_bot + 2100
    # bottom of page
    footer_bot <- 2550
    
    recap_footer_words <- c('comments', 'passanger(s)', 'agency(s):',
                            'approving', 'supervisor:', 'date:')
    # split entries with dashes in between into multiple entries here
    # (in case Comments - Passanger(s) - Agency(s) get combined together, so
    #  we can still detect that they're there)
    split_dashes <- local_ocr %>% separate_rows(word, sep = "-")
    footer_local <- check_for_text(split_dashes, recap_footer_words, footer_top, footer_bot, 0)
    
    table_top <- activities_top + 110
    
    no_activities_words <- c('*', 'no', 'activities', 'recorded', 'for', 'this', 'assignment')
    no_activities_local <- check_for_text(local_ocr, no_activities_words, table_top, table_top + 300, 0)
    
    # at end
    if (nrow(footer_local) > 3) {
      footer_top <- (footer_local$top %>%
        mean() %>%
        round()) - 10
      footer_bot <- (footer_local$bot %>%
        mean() %>%
        round()) + 10
      
      # 5 of 7 detected ("no" is almost a guarantee from table headers)
      if (nrow(no_activities_local) > 4) {
        print('No activities recorded!')
        
        cur_log$specials <- cur_log$specials %>%
          c('NO_ACTIVITIES_RECORDED')
        
        cur_sect <- SECT$RECAP_END
        
        no_activities <- TRUE
        footer_top <- footer_top - 200
      } else {
        # want to find last row of table for cropping;
        # we can do this by searching for the last (vertically) number left of
        # 1100 pixels and using 25px below that as the bottom boundary
        table_bot <- (local_ocr %>%
                        filter(grepl('^[0-9]+$', word) & right < 1100 & bot < footer_top) %>%
                        slice(n()) %>% # get last row matching filter
                        pull(bot)) + 25 # get "bot" property, add 25 px
        
        # after calculation, useful to set footer area higher so we can
        # potentially capture large handwriting beneath table
        footer_top <- max(footer_top - 200, table_bot)
        
        cur_sect <- SECT$RECAP_END
      }
    } else { # not at end
      # same calculation as before, but no bottom bound
      table_bot <- (local_ocr %>%
                      filter(grepl('^[0-9]+$', word) & right < 1100) %>%
                      slice(n()) %>% # get last row matching filter
                      pull(bot)) + 25 # get "bot" property, add 25 px
      
      cur_sect <- SECT$RECAP_CONTD
    }
    
    if (!no_activities) {
      table_image <- image %>%
        simple_crop(table_bot - table_top, table_top)
      
      cur_log$recap$images <- cur_log$recap$images %>%
        c(preview(table_image, page, SECT$RECAP_START))
      cur_log$recap$local_ocrs <- cur_log$recap$local_ocrs %>%
        append(list(crop_local_ocr(local_ocr, table_top, table_bot)))
      
      # not reliably extracted using ExtractTable + we have another source for
      # this data
      # if (CALL_EXTRACTTABLE)
      #   cur_log$recap$extracttable_ocrs <- cur_log$recap$extracttable_ocrs %>%
      #     append(list(get_table(table_image)))
      
      if (cur_sect == SECT$RECAP_CONTD)
        next 
    }
  }
  
  
  
  
  
  # activities recap continuing onto next page
  if (cur_sect == SECT$RECAP_CONTD) {
    # first check for footer to set lower bound
    recap_footer_words <- c('comments', 'passanger(s)', 'agency(s):',
                            'approving', 'supervisor:', 'date:')
    # see recap start split_dashes var for explanation
    split_dashes <- local_ocr %>% separate_rows(word, sep = "-")
    footer_local <- check_for_text(split_dashes, recap_footer_words, 2200, 2550, 0)
    
    if (nrow(footer_local) < 4) {
      print('Note: Activities recap missing activities...')
      
      cur_log$specials <- cur_log$specials %>%
        c('SOME_MISSING_ACTIVITIES_RECAP')
      
      page_already_setup <- TRUE
      cur_sect <- SECT$DESC_START
      next
    }
    
    # there could be a flight log with 3 pages of activities recap, but it seems unlikely
    
    footer_top <- (footer_local$top %>%
                     mean() %>%
                     round()) - 10
    footer_bot <- (footer_local$bot %>%
                     mean() %>%
                     round()) + 10
    
    table_bot <- (local_ocr %>%
      filter(grepl('^[0-9]+$', word) & right < 1100 & bot < footer_top) %>%
      slice(n()) %>% # get last row matching filter
      pull(bot)) + 25 # get "bot" property, add 25 px
    
    # same as in previous section; widening footer area
    footer_top <- max(footer_top - 200, table_bot)
    
    table_image <- image %>%
      simple_crop(table_bot)
    
    cur_log$recap$images <- cur_log$recap$images %>%
      c(preview(table_image, page, cur_sect))
    cur_log$recap$local_ocrs <- cur_log$recap$local_ocrs %>%
      append(list(crop_local_ocr(local_ocr, 0, table_bot)))

    # have spreadsheet provided to do this instead!    
    # if (CALL_EXTRACTTABLE)
    #   cur_log$recap$extracttable_ocrs <- cur_log$recap$extracttable_ocrs %>%
    #     append(list(get_table(table_image)))
    
    cur_sect <- SECT$RECAP_END
  }
  
  
  
  
  
  # footer table of recap section
  # expects footer_top and footer_bot variables already set! (they should be)
  if (cur_sect == SECT$RECAP_END) {
    # may just wish to crop to the bottom of the page if we're doing manual
    # data entry with this section
    table_image <- image %>%
      simple_crop(2550 - footer_top, footer_top) # was footer_bot - footer_top before
    
    cur_log$recap_end$image <- preview(table_image, page, cur_sect)
    cur_log$recap_end$local_ocr <- crop_local_ocr(local_ocr, footer_top, 2550)
    
    # no extracttable call; only a small footer with often handwritten or redacted
    # text, so not worth an API call
    
    cur_sect <- SECT$DESC_START
    next
  }
  
  
  
  
  
  # activities descriptions, start
  if (cur_sect == SECT$DESC_START) {
    desc_meta_words <- c('tfo', 'pilot', 'date', 'watch', 'assignment')
    meta_local <- check_for_text(local_ocr, desc_meta_words, 0, 400, 0)
    
    main_header_words <- c('air', 'support', 'division', 'daily', 'log')
    header_local <- check_for_text(local_ocr, main_header_words, 0, 300, 0)

    if (nrow(meta_local) < 3 | nrow(header_local) > 3) {
      print('No activities description page!')
      
      cur_log$specials <- cur_log$specials %>%
        c('NO_ACTIVITIES_DESC')
      
      page_already_setup <- TRUE
      cur_sect <- SECT$META
      next
    }
    
    meta_top <- (meta_local$top %>%
      mean() %>%
      round()) - 15
    meta_headers_bot <- (meta_local$bot %>%
      mean() %>%
      round()) + 15
    
    meta_bot <- meta_headers_bot + 125
    
    meta_image <- image %>%
      simple_crop(meta_bot - meta_top, meta_top)
    
    cur_log$description_meta$image <- preview(meta_image, page, cur_sect, TRUE)
    cur_log$description_meta$local_ocr <- crop_local_ocr(local_ocr, meta_top, meta_bot)
    
    # redundant data from the meta section, but names are fully spelt out.
    # not quite worth an ExtractTable call, since local OCR is quite good with
    # alphabet recognition (not so much alphanumeric)
    cur_log$description_meta$local_table <- data.frame(
      TFO = local_ocr %>%
        crop_local_ocr(meta_headers_bot, meta_bot, 0, 725) %>% # middle of TFO and Pilot
        filter(confidence > 70) %>%
        .[["word"]] %>% # only care about the words
        paste(collapse = ' '), # combine strings
      Pilot = local_ocr %>%
        crop_local_ocr(meta_headers_bot, meta_bot, 725, 1450) %>%
        filter(confidence > 70) %>%
        .[["word"]] %>%
        paste(collapse = ' '),
      Date = local_ocr %>%
        crop_local_ocr(meta_headers_bot, meta_bot, 1450, 2250) %>%
        filter(confidence > 70) %>%
        .[["word"]] %>%
        paste(collapse = ' '),
      Watch = local_ocr %>%
        crop_local_ocr(meta_headers_bot, meta_bot, 2250, 2650) %>%
        filter(confidence > 40) %>%
        .[["word"]] %>%
        paste(collapse = ' '),
      Assignment = local_ocr %>%
        crop_local_ocr(meta_headers_bot, meta_bot, 2650, 3300) %>%
        filter(confidence > 40) %>%
        .[["word"]] %>%
        paste(collapse = ' ')
    )
    
    # no ExtractTable call for desc meta!
    
    if (no_activities) {
      print('No activities descriptions because there are no activities...')
      cur_sect <- SECT$META
      next
    }
    
    desc_table_headers <- c('act', 'no.', 'location', 'area', 'comments')
    local_desc_table <- check_for_text(local_ocr, desc_table_headers, meta_bot, meta_bot + 100, 0)
    
    # should be before 'page x of y' without ever hitting bottom of table
    desc_bot <- 2550 - 150
    
    # threshold for number of words before redaction: 5 rn but could change
    if (nrow(local_desc_table) < 3 |
        nrow(filter(local_ocr, top > (local_desc_table$bot %>% mean() %>% round()) & bot < desc_bot)) < 5) {
      print('REDACTED activities descriptions')
      
      cur_log$specials <- cur_log$specials %>%
        c('REDACTED_ACTIVITIES_DESC')
      
      cur_sect <- SECT$DESC_CONTD
      next
    }
    
    table_image <- image %>%
      simple_crop(desc_bot - meta_bot, meta_bot)
    
    # CALL EXTRACTTABLE + SAVE INFO
    cur_log$descriptions$images <- cur_log$descriptions$images %>%
      c(preview(table_image, page, cur_sect))
    cur_log$descriptions$local_ocrs <- cur_log$descriptions$local_ocrs %>%
      append(list(crop_local_ocr(local_ocr, meta_bot, desc_bot)))
    
    if (CALL_EXTRACTTABLE)
      cur_log$descriptions$extracttable_ocrs <- cur_log$descriptions$extracttable_ocrs %>%
        append(list(get_table(table_image, TRUE)))
    
    cur_sect <- SECT$DESC_CONTD
    next
  }
  
  
  
  
  
  if (cur_sect == SECT$DESC_CONTD) {
    # may not actually be a next page of description
    
    desc_table_headers <- c('act', 'no.', 'location', 'area', 'comments')
    headers <- check_for_text(local_ocr, desc_table_headers, 0, 250, 0)
    
    if (nrow(headers) < 3) {
      page_already_setup <- TRUE
      cur_sect <- SECT$META
      next
    }
    
    # check whether redacted; this assumes that the headers above ^ are not redacted
    # potential, if necessary TODO: also detect large black boxes to determine
    # whether activity descriptions are redacted
    desc_bot <- 2550 - 170
    if (nrow(filter(local_ocr, top > (headers$bot %>% mean() %>% round()) & bot < desc_bot)) < 5) {
      print('REDACTED continued activities descriptions')
      cur_log$specials <- cur_log$specials %>%
        c('REDACTED_ACTIVITIES_DESC_CONTD')
      
      cur_sect <- SECT$META
      next
    }
    
    table_image <- image %>%
      simple_crop(2550 - 100)
    
    cur_log$descriptions$images <- cur_log$descriptions$images %>%
      c(preview(table_image, page, cur_sect))
    cur_log$descriptions$local_ocrs <- cur_log$descriptions$local_ocrs %>%
      append(list(crop_local_ocr(local_ocr, 100, 2550)))
    
    if (CALL_EXTRACTTABLE)
      cur_log$descriptions$extracttable_ocrs <- cur_log$descriptions$extracttable_ocrs %>%
        append(list(get_table(table_image, TRUE)))
    
    # assuming no more than 2 pages of description; next page is assumed to be
    # a new log
    cur_sect <- SECT$META
    next
  }
}

# this code adds the final page after the while loop has finished

# detect pages text (same code as beginning of while loop)
bottom_pages_text <- suppressWarnings(local_ocr %>%
                                        crop_local_ocr(2350, 2550, 1500, 1800) %>%
                                        tail(3) %>% .[[1]] %>% .[c(1,3)] %>%
                                        as.integer())

if (!is.na(bottom_pages_text[1])) {
  cur_log$log_meta$present_pages <- cur_log$log_meta$present_pages %>%
    c(bottom_pages_text[1])
}
if (!is.na(bottom_pages_text[2])) {
  cur_log$log_meta$num_pages <- cur_log$log_meta$num_pages %>%
    c(bottom_pages_text[2])
}

# add file end page and append!
cur_log$log_meta$file_end_page <- page
logs <- append(logs, list(cur_log))

print(paste("Done! Found", length(logs), "logs and called ExtractTable", api_key_usage, "times."))
