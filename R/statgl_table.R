#' Automatic statgl table
#'
#' Concvenience function to quickly write html tables. Returns a kable with automatic alignments and formats.
#'
#' @param df A data frame
#' @param year_col Name of year column. Disregards formatiing at this num column.
#' @param ... Parameters to \code{format}
#'
#' @return
#' @export
#'
#' @examples
#' statgl_table(ggplot2::mpg, year_col = year)
statgl_table <- function(df, year_col, caption, output_type = "html_document", replace_0s = FALSE, ...){


  if(output_type == "html_document"){
    aligns <- paste0(c("l", rep("r", ncol(df))), collapse = "")

    if(!missing(year_col)){
      df <- dplyr::mutate_at(df, rlang::as_name(rlang::enquo(year_col)), as.character)
    }

    df <- dplyr::mutate_if(df, is.numeric, format, big.mark = ".", decimal.mark = ",")

    if(replace_0s){

      df <- dplyr::mutate_all(df, as.character)

      df <- dplyr::mutate_all(df, trimws)

      df <- dplyr::mutate_all(df, plyr::mapvalues, from = "0", to = "[-]{}", warn_missing = FALSE)
    }

      if(!missing(caption)){
        kableExtra::kable_styling(
          kableExtra::kable(df, align = aligns, caption = caption, ...),
            bootstrap_options = c("striped", "hover", "condensed", "responsive"))

      }
      else{
        kableExtra::kable_styling(
          kableExtra::kable(df, align = aligns, ...),
            bootstrap_options = c("striped", "hover", "condensed", "responsive"))
      }

    }
  else{
#    flextable::autofit(statgl_flex(df))
  }
}

