#' as_gt
#'
#' Translates a table created with tablespan to a great table (gt). See <https://gt.rstudio.com/>.
#'
#' Tablespan itself does not provide any printing of tables as HTML table. However,
#' with as_gt, tablespan can be translated to a great table which provides html and
#' LaTeX output.
#'
#' @param tbl table created with tablespan::tablespan
#' @param groupname_col Provide column names to group data. See ?gt::gt for more
#' details.
#' @param separator_style style of the vertical line that separates the row names
#' from the data.
#' @param auto_format should the table be formatted automatically?
#' @param ... additional arguments passed to gt::gt().
#' @returns gt table that can be further adapted with the gt package.
#' @import gt
#' @export
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' tbl <- tablespan(data = summarized_table,
#'                  formula = (LHS = Cylinder:cyl + Engine:vs) ~
#'                    N +
#'                    (Results = (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'                       (`Weight` = Mean:mean_wt + SD:sd_wt)))
#'
#' gt_tbl <- as_gt(tbl)
#' gt_tbl
as_gt <- function(tbl,
                  groupname_col = NULL,
                  separator_style = gt::cell_borders(sides = c("right"),
                                                     weight = gt::px(1),
                                                     color = "gray"),
                  auto_format = TRUE,
                  ...){
  if(!is.null(tbl$header$lhs)){
    data_set <- cbind(tbl$table_data$row_data,
                      tbl$table_data$col_data)
  }else{
    data_set <- tbl$table_data$col_data
  }

  gt_tbl <- gt::gt(data = data_set,
                   groupname_col = groupname_col,
                   ...)

  # add the spanners
  gt_tbl <- add_gt_spanners(gt_tbl = gt_tbl,
                            tbl = tbl)

  if(!is.null(tbl$header$lhs)){
    rowname_headers <- colnames(tbl$table_data$row_data)
    gt_tbl <- add_gt_rowname_separator(gt_tbl = gt_tbl,
                                       right_of = rowname_headers[length(rowname_headers)],
                                       separator_style = separator_style)
  }

  if(!is.null(tbl$title) | !is.null(tbl$subtitle))
    gt_tbl <- add_gt_titles(gt_tbl,
                            title = tbl$title,
                            subtitle = tbl$subtitle)
  if(!is.null(tbl$footnote))
    gt_tbl <- add_gt_footnote(gt_tbl,
                              footnote = tbl$footnote)
  if(auto_format){
    gt_tbl <- gt_tbl |>
      gt::fmt_auto() |>
      gt::sub_missing(missing_text = "")
  }

  return(gt_tbl)
}

#' flatten_table
#'
#' The table header within tables created with tablespan is represented in
#' highly nested lists. The following function "flattens" this list to simplify
#' implementing the same headers in gt.
#' @param tbl table created with tablespan::tablespan
#' @keywords internal
#' @import gt
#' @noRd
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' tbl <- tablespan(data = summarized_table,
#'                  formula = (LHS = Cylinder:cyl + Engine:vs) ~
#'                    N +
#'                    (Results = (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'                       (`Weight` = Mean:mean_wt + SD:sd_wt)))
#' str(tablespan:::flatten_table(tbl))
flatten_table <- function(tbl){
  if(!is.null(tbl$header$lhs)){
    flattened_lhs <- flatten_table_partial(tbl_partial = tbl$header$lhs)
  }else{
    flattened_lhs <- NULL
  }
  flattened_rhs <- flatten_table_partial(tbl_partial = tbl$header$rhs)

  return(list(flattened_lhs = flattened_lhs,
              flattened_rhs = flattened_rhs))
}

#' flatten_table_partial
#'
#' Called by tablespan:::flatten_table. Recursive function that flattens the
#' left hand or right hand side of the table headers.
#' @param tbl_partial partial of a table header
#' @param id unique id assigned to the current element. When creating a gt, we have
#' to ensure that spanners have different ids. This can be problematic if multiple
#' spanners have the same label. The id is created automatically and will contain
#' all parents of the spanners as well to ensure that each spanner has a unique, but
#' reproducible id.
#' @param flattened list filled recursively
#' @import gt
#' @keywords internal
#' @noRd
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' tbl <- tablespan(data = summarized_table,
#'                  formula = (LHS = Cylinder:cyl + Engine:vs) ~
#'                    N +
#'                    (Results = (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'                       (`Weight` = Mean:mean_wt + SD:sd_wt)))
#' str(tablespan:::flatten_table_partial(tbl$header$rhs))
flatten_table_partial <- function(tbl_partial, id = "", flattened = list()){
  if(!is.null(tbl_partial$entries)){
    children <- list(list(label = tbl_partial$name,
                          id = paste0(id, "_", tbl_partial$name),
                          level = tbl_partial$level,
                          children = sapply(tbl_partial$entries,
                                            function(x) x$name),
                          children_ids = sapply(tbl_partial$entries,
                                                function(x) paste0(id, "_", tbl_partial$name, "_", x$name)),
                          # For items, tablespan can store a name that is different
                          # from the actual item label to allow for renaming
                          children_items = sapply(tbl_partial$entries,
                                                  function(x) if(!is.null(x$item_name)) {x$item_name}else{x$name})))
    flattened <- c(flattened,
                   children)
    for(entry in tbl_partial$entries){
      flattened <- flatten_table_partial(tbl_partial = entry,
                                         id = paste0(id, "_", tbl_partial$name),
                                         flattened = flattened)
    }
  }
  return(flattened)
}

#' add_gt_spanners
#'
#' Adds the spanners defined in a tablespan table to a gt table.
#'
#' @param gt_tbl gt table without spanners
#' @param tbl table created with tablespan::tablespan
#' @import gt
#' @keywords internal
#' @noRd
add_gt_spanners <- function(gt_tbl, tbl){
  flattened_tbl <- flatten_table(tbl)

  if(!is.null(flattened_tbl$flattened_lhs)){
    gt_tbl <- add_gt_spanner_partial(gt_tbl = gt_tbl,
                                     tbl_partial = flattened_tbl$flattened_lhs)
  }

  gt_tbl <- add_gt_spanner_partial(gt_tbl = gt_tbl,
                                   tbl_partial = flattened_tbl$flattened_rhs)

  return(gt_tbl)
}

#' add_gt_spanner_partial
#'
#' Adds the spanners of the left hand side or right hand side of the headers
#' defined in tablespan table to a gt table.
#'
#' @param gt_tbl gt table without spanners
#' @param tbl_partial left or right hand side header of a table created with
#' tablespan::tablespan
#' @import gt
#' @importFrom dplyr all_of
#' @importFrom rlang :=
#' @keywords internal
#' @noRd
add_gt_spanner_partial <- function(gt_tbl, tbl_partial){
  # The table spanners need to be added in the correct order. All children of
  # a spanner must already be in the table, otherwise we get an error.
  # The level tells us the order; we have to start with the lowest one
  levels <- sort(unique(sapply(tbl_partial, function(x) x$level)))

  # Next, we iterate over the levels and add them to the gt:
  for(level in levels){
    for(parent_item in seq_along(tbl_partial)){
      parent_name <- tbl_partial[[parent_item]]$label
      parent <- tbl_partial[[parent_item]]

      if(parent$level == level){

        item_names <- parent$children_items[parent$children_items %in% colnames(gt_tbl$`_data`)]
        spanner_ids <- parent$children_ids[!parent$children_items %in% colnames(gt_tbl$`_data`)]

        # if we are at the base level, we do not add a spanner:
        if(parent_name != "_BASE_LEVEL_")
          gt_tbl <- gt_tbl |>
          gt::tab_spanner(label = parent_name,
                          id = parent$id,
                          columns = dplyr::all_of(item_names),
                          spanners = spanner_ids)

        # If children_items and children don't match, we also need to rename elements
        to_rename <- which(parent$children_items != parent$children)

        if(length(to_rename) > 0){
          for(t_r in to_rename){
            old_name <- parent$children_items[t_r]
            gt_tbl <- gt_tbl |>
              gt::cols_label({{old_name}} := parent$children[t_r])
          }
        }
      }
    }
  }
  return(gt_tbl)
}

#' add_gt_rowname_separator
#'
#' Adds a vertical line between the row names part and the data of the table.
#' @param gt_tbl great table
#' @param right_of name of the last data column to the right of which the actual
#' data starts
#' @param separator_style style of the vertical line that separates the row names
#' from the data.
#' @import gt
#' @keywords internal
#' @noRd
add_gt_rowname_separator <- function(gt_tbl,
                                     right_of,
                                     separator_style){
  gt_tbl <- gt_tbl |>
    gt::tab_style(style = separator_style,
                  locations = gt::cells_body(columns = all_of(right_of)))
  return(gt_tbl)
}

#' add_gt_titles
#'
#' Add a title and subtitle to a gt table
#' @param gt_tbl gt table
#' @param title title text
#' @param subtitle subtitle text
#' @return gt
#' @keywords internal
#' @importFrom gt tab_header
#' @noRd
add_gt_titles <- function(gt_tbl,
                          title,
                          subtitle){
  return(
    gt_tbl |>
      gt::tab_header(title = title,
                     subtitle = subtitle) |>
      gt::opt_align_table_header(align = c("left"))
  )
}

#' add_gt_footnote
#'
#' Add a footnote to a gt table
#' @param gt_tbl gt table
#' @param footnote footnote text
#' @returns gt
#' @keywords internal
#' @importFrom gt tab_header
#' @noRd
add_gt_footnote <- function(gt_tbl,
                            footnote){
  return(
    gt_tbl |>
      gt::tab_footnote(footnote = footnote)
  )
}
