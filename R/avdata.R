#' @name avdata
#'
#' @aliases avdata_import
#'
#' @title Import data into the current workspace
#'
#' @description `avdata()` returns key-value tables representing the
#'     information visualized under the DATA tab, 'REFERENCE DATA' and
#'     'OTHER DATA' items.  `avdata_import()` updates (modifies or
#'     creates new, but does not delete) rows in 'REFERENCE DATA' or
#'     'OTHER DATA' tables.
#'
#' @return `avdata()` returns a tibble with five columns: `"type"`
#'     represents the origin of the data from the 'REFERENCE' or
#'     'OTHER' data menus. `"table"` is the table name in the
#'     `REFERENCE` menu, or 'workspace' for the table in the 'OTHER'
#'     menu, the key used to access the data element, the value label
#'     associated with the data element and the value (e.g., google
#'     bucket) of the element.
#'
#' @param namespace `character(1)` AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name `character(1)` AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
#'
#' @return `avdata_import()` returns, invisibly, the subset of the
#'     input table used to update the AnVIL tables.
#'
#' @examples
#' library(AnVILBase)
#' if (has_avworkspace(strict = TRUE, platform = gcp())) {
#'     ## from within AnVIL
#'     data <- avdata()
#'     data
#'     if (interactive())
#'         avdata_import(data)
#' }
NULL

# avdata ------------------------------------------------------------------

#' @rdname avdata
#'
#' @export
avdata <- function(
    namespace = avworkspace_namespace(),
    name = avworkspace_name()
) {
    stopifnot(
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    checkInstalled("AnVIL")
    name <- URLencode(name)
    response <- AnVIL::Terra()$getWorkspace(
        namespace, URLencode(name), "workspace.attributes"
    )
    avstop_for_status(response, "avworkspace_data")

    content <- content(response)[[1]][["attributes"]]

    ## a workspace DATA element may be preceeded by the 'workspace:'
    ## tag, remove it
    names(content) <- sub("^workspace:", "", names(content))
    ## remove non-DATA attributes. `description` is from the workspace
    ## landing page. The `:` seems to be used as a delimiter, e.g.,
    ## `tag:tags`
    exclude <-
        names(content) %in% "description" |
        grepl("^[a-z]+:", names(content))
    content <- content[!exclude]

    ## some elements are lists, e.g., a vector of google
    ## buckets. Translate these to their character(1) representation,
    ## so the tibble has a column of type <chr> and shows the value of
    ## the character(1) entries, rather than a column of type list
    ## showing "chr(1)" for most elements
    is_character <- vapply(content, is.character, logical(1))
    content[!is_character] <- vapply(
        content[!is_character],
        ## list-like elements usually have a key-value structure, use
        ## the value
        function(x) jsonlite::toJSON(unlist(x[["items"]], use.names = FALSE)),
        character(1)
    )

    ## create the referenceData tibble; 'referenceData' keys start
    ## with "referenceData_"
    referenceData_id <- "referenceData_"
    referenceData_regex <- "^referenceData_([^_]+)_(.*)$"
    is_referenceData <- startsWith(names(content), referenceData_id)
    referenceData <- content[is_referenceData]
    referenceData_tbl <- tibble(
        type = rep("reference", length(referenceData)),
        table = sub(referenceData_regex, "\\1", names(referenceData)),
        key = sub(referenceData_regex, "\\2", names(referenceData)),
        value = as.character(unlist(referenceData, use.names = FALSE))
    )

    ## 'other' data
    otherData <- content[!is_referenceData]
    otherData_tbl <- tibble(
        type = "other",
        table = "workspace",
        key = names(otherData),
        value = as.character(unlist(otherData, use.names = FALSE))
    )

    bind_rows(otherData_tbl, referenceData_tbl)
}

# avdata_import -----------------------------------------------------------

#' @rdname avdata
#'
#' @param .data A tibble or data.frame for import as an AnVIL table.
#'
#' @importFrom dplyr filter
#' @export
avdata_import <- function(
    .data,
    namespace = avworkspace_namespace(),
    name = avworkspace_name()
) {
    stopifnot(
        is.data.frame(.data),
        all(c("type", "table", "key", "value") %in% names(.data)),
        all(vapply(
            select(.data, "type", "table", "key", "value"),
            is.character,
            logical(1)
        )),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    .data <- filter(.data, .data$type == "other", table %in% "workspace")

    if (!nrow(.data)) {
        message(
            "'avdata_import()' has no rows of type 'other' and ",
            "table 'workspace'"
        )
        return(invisible(.data))
    }

    ## create a 'wide' table, with keys as column names and values as
    ## first row. Prefix "workspace:" to first column, for import
    keys <- paste0("workspace:", paste(.data$key, collapse = "\t"))
    values <- paste(.data$value, collapse = "\t")
    destination <- tempfile()
    writeLines(c(keys, values), destination)

    ## upload the table to AnVIL
    entities <- httr::upload_file(destination)
    checkInstalled("AnVIL")
    .avworkspace_stop_if_not_writable(namespace, name, "avdata_import")
    response <- AnVIL::Terra()$importAttributesTSV(
        namespace, URLencode(name), entities
    )
    avstop_for_status(response, "avdata_import")

    invisible(.data)
}
