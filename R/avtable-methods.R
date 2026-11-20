#' @name avtable-methods
#'
#' @aliases avtables avtable avtable_import avtable_import_set
#' @aliases avtable_delete_values
#'
#' @title Methods that work with the primary datasets in the DATA tab
#'
#' @description  Tables can be visualized under the DATA tab, TABLES
#'     item.  `avtable()` returns an AnVIL table.  `avtable_paged()`
#'     retrieves an AnVIL table by requesting the table in 'chunks',
#'     and may be appropriate for large tables. `avtable_import()`
#'     imports a data.frame to an AnVIL table.  `avtable_import_set()`
#'     imports set membership (i.e., a subset of an existing table)
#'     information to an AnVIL table.  `avtable_delete_values()`
#'     removes rows from an AnVIL table.
#'
#' @inheritParams av
#' @inheritParams avdata
#' @inheritParams avnotebooks-methods
#'
#' @return `avtables()`: A tibble with columns identifying the table,
#'     the number of records, and the column names.
#'
#' @importFrom tibble tibble
#' @importFrom AnVILBase avstop_for_status avtables avtable avtable_import_set
#' @importFrom AnVILBase avtable_delete_values
#' @importFrom BiocBaseUtils isCharacter isScalarCharacter isScalarLogical
#' @importFrom BiocBaseUtils isScalarNumber isScalarInteger checkInstalled
#'
#' @examples
#' if (interactive()) {
#'     avtables("waldronlab-terra", "Tumor_Only_CNV")
#'     avtable("participant", "waldronlab-terra", "Tumor_Only_CNV")
#'
#'     library(dplyr)
#'     ## mtcars dataset
#'     mtcars_tbl <-
#'         mtcars |>
#'         as_tibble(rownames = "model_id") |>
#'         mutate(model_id = gsub(" ", "-", model_id))
#'
#'     ## set a workspace with write access to import the table
#'     avworkspace("waldronlab-terra/mramos-wlab-bioc-0")
#'
#'     avstatus <- avtable_import(mtcars_tbl)
#'
#'     avtable_import_status(avstatus)
#'
#'     set_status <- avtable("model") |>
#'         avtable_import_set("model", "cyl", "model_id")
#'
#'     avtable_import_status(set_status)
#'
#'     ## won't be able to delete a row that is referenced in another table
#'     avtable_delete_values("model", "Mazda-RX4")
#'
#'     ## delete the set
#'     avtable_delete("model_set")
#'
#'     ## then delete the row
#'     avtable_delete_values("model", "Mazda-RX4")
#'
#'     ## recreate the set (if needed)
#'     avtable("model") |>
#'         avtable_import_set("model", "cyl", "model_id")
#'
#' }
#' @include gcp-class.R
NULL

# avtables ----------------------------------------------------------------

#' @describeIn avtable-methods `avtables()` describes tables available in a
#'     workspace
#'
#' @exportMethod avtables
setMethod("avtables", signature = c(platform = "gcp"), definition =
    function(
        namespace = avworkspace_namespace(), name = avworkspace_name(),
        ..., platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(namespace),
            isScalarCharacter(name)
        )
        checkInstalled("AnVIL")
        types <- AnVIL::Terra()$getEntityTypes(namespace, URLencode(name))
        avstop_for_status(types, "avtables")
        lst <- content(types)
        table <- names(lst)
        count <- vapply(lst, `[[`, integer(1), "count")
        colnames <- vapply(lst, function(elt) {
            value <- unlist(elt[c("idName", "attributeNames")], use.names=FALSE)
            paste(value, collapse=", ")
        }, character(1))
        tibble(table, count, colnames)
    }
)

.is_avtable <-
    function(table, namespace, name)
{
    tbls <- avtables(namespace, name)
    table %in% tbls$table
}

## helpers for avtable(), avtable_paged() avtable_import() handling
## treatment NA_character_
.avtable_na <-
    function(na)
{
    stopifnot(isCharacter(na, zchar = TRUE))
    function(x) {
        ## flatten() has changed "" (no attribute in the entity set)to
        ## NA_character_; change to ""
        x[is.na(x)] <- ""
        ## follow the rules of `na` re-assign NA_character_
        x[x %in% na] <- NA_character_
        x
    }
}

# avtable -----------------------------------------------------------------

#' @describeIn avtable-methods `avtable()` retrieves a table from an AnVIL
#'    workspace
#'
#' @param table character(1) table name as returned by, e.g., `avtables()`.
#'
#' @param na in `avtable()` and `avtable_paged()`, character() of
#'     strings to be interpreted as missing values. In
#'     `avtable_import()` character(1) value to use for representing
#'     `NA_character_`. See Details.
#'
#' @details Treatment of missing values in `avtable()`,
#'     `avtable_paged()` and `avtable_import()` are handled by the
#'     `na` parameter.
#'
#' @details `avtable()` may sometimes result in a curl error 'Error in
#'     curl::curl_fetch_memory' or an 'Internal Server Error (HTTP
#'     500)' This may be due to a server time-out when trying to read
#'     a large (more than 50,000 rows?) table; using `avtable_paged()`
#'     may address this problem.
#'
#' @details For `avtable()` and `avtable_paged()`, the default `na = c("",
#'     "NA")` treats empty cells or cells containing "NA" in a Terra
#'     data table as `NA_character_` in R. Use `na = character()` to
#'     indicate no missing values, `na = "NA"` to retain the
#'     distinction between `""` and `NA_character_`.
#'
#' @details For `avtable_import()`, the default `na = "NA"` records
#'     `NA_character_` in R as the character string `"NA"` in an AnVIL
#'     data table.
#'
#' @details The default settings (`na = "NA"` in `avtable_import()`,
#'     `na = c("", "NA")` in `avtable()`) are appropriate to
#'     'round-trip' data from R to AnVIL and back when character vectors
#'     contain only `NA_character_`. Use `na = "NA"` in both functions to
#'     round-trip data containing both `NA_character_` and `""`. Use
#'     a distinct string, e.g., `na = "__MISSING_VALUE__"`, for both
#'     arguments if the data contains a string `"NA"` as well as
#'     `NA_character_`.
#'
#' @return `avtable()`: a tibble of data corresponding to the AnVIL
#'     table `table` in the specified workspace.
#'
#' @importFrom dplyr select starts_with ends_with across where mutate
#' @importFrom AnVILBase flatten
#'
#' @exportMethod avtable
setMethod("avtable", signature = c(platform = "gcp"), definition =
    function(
        table, namespace = avworkspace_namespace(),
        name = avworkspace_name(), na = c("", "NA"),
        ..., platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(table),
            isScalarCharacter(namespace),
            isScalarCharacter(name)
            ## ,
            ##  `unknown table; use 'avtables()' for valid names` =
            ##      .is_avtable(table, namespace, name)
        )
        na_fun <- .avtable_na(na)

        checkInstalled("AnVIL")
        tryCatch({
            entities <- AnVIL::Terra()$getEntities(
                namespace, URLencode(name), table
            )
        }, error = function(err) {
            msg <- paste0(
                "'avtable()' failed, see 'Details' of `?avtable` for help\n",
                "  ", conditionMessage(err)
            )
            stop(msg, call. = FALSE)
        })
        avstop_for_status(entities, "avtable")
        tbl <-
            entities |>
            flatten()
        if (!"name" %in% names(tbl)) {
            stop(
                "table '", table, "' does not exist; see 'avtables()'\n",
                "    namespace: ", namespace, "\n",
                "    name: ", name, "\n"
            )
        }
        tbl <-
            tbl |>
            select(
                "name", starts_with("attributes"), -ends_with("entityType")
            ) |>
            mutate(across(where(is.character), na_fun))
        names(tbl) <- sub("^attributes.", "", names(tbl))
        names(tbl) <- sub(".entityName$", "", names(tbl))
        names(tbl) <- sub("^name$", paste0(table, "_id"), names(tbl))
        tbl
    }
)

# avtable_nest ------------------------------------------------------------

## one list attribute: 'items' values and 'itemsType' to an R list
.avtable_nest_items <-
    function(items, itemsType)
{
    lapply(seq_along(items), function(i) {
        value <- items[[i]]
        if (identical(itemsType[[i]], "EntityReference")) {
            if (is.data.frame(value)) {
                value <- value[["entityName"]]
            } else if (is.list(value)) {
                value <- vapply(
                    value, function(x) x[["entityName"]], character(1L)
                )
            }
            if (is.null(value))
                value <- character()
        }
        value
    })
}

#' @rdname avtable-methods
#'
#' @description `avtable_nest()` reshapes the list attributes in a
#'     table returned by `avtable()` or `avtable_paged()` into list
#'     columns named as in the AnVIL workspace DATA tab.
#'
#' @param .tbl a `tibble` as returned by `avtable()` or
#'     `avtable_paged()`.
#'
#' @details `avtable()` returns each AnVIL list attribute `<name>` as
#'     two columns, `<name>.items` and `<name>.itemsType`;
#'     `avtable_paged()` returns it as a column `<name>` containing a
#'     `data.frame` with `items` and `itemsType`. `avtable_nest()`
#'     replaces these with a single list column `<name>`, as shown in
#'     the workspace. Values of entity reference lists (e.g., set
#'     membership) are represented by their entity names. Other
#'     columns are unchanged.
#'
#' @return `avtable_nest()` returns a `tibble` with one list column for
#'     each list attribute.
#'
#' @examples
#' tbl <- tibble::tibble(
#'     sample_id = c("s1", "s2"),
#'     cohorts.itemsType = "AttributeValue",
#'     cohorts.items = list(c("A", "B"), "C")
#' )
#' avtable_nest(tbl)
#'
#' @export
avtable_nest <-
    function(.tbl)
{
    stopifnot(is.data.frame(.tbl))

    ## avtable(): '<name>.items' and '<name>.itemsType' columns
    items_cols <- grep("\\.items$", names(.tbl), value = TRUE)
    bases <- sub("\\.items$", "", items_cols)
    type_cols <- paste0(bases, ".itemsType")
    keep <- type_cols %in% names(.tbl) & !bases %in% names(.tbl)
    for (i in which(keep)) {
        .tbl[[items_cols[[i]]]] <- .avtable_nest_items(
            .tbl[[items_cols[[i]]]], .tbl[[type_cols[[i]]]]
        )
        names(.tbl)[names(.tbl) == items_cols[[i]]] <- bases[[i]]
    }
    .tbl <- .tbl[setdiff(names(.tbl), type_cols[keep])]

    ## avtable_paged(): '<name>' data.frame column of items, itemsType
    is_nested <- vapply(.tbl, function(column) {
        is.data.frame(column) &&
            all(c("items", "itemsType") %in% names(column))
    }, logical(1L))
    for (column in names(.tbl)[is_nested]) {
        .tbl[[column]] <- .avtable_nest_items(
            .tbl[[column]][["items"]], .tbl[[column]][["itemsType"]]
        )
    }

    .tbl
}

.avtable_import_set_entity <-
    function(.data, entity)
{
    oentity <- entity
    idx <- match(entity, names(.data))

    if (!startsWith(entity, "entity:"))
        entity <- paste0("entity:", entity)
    if (!endsWith(entity, "_id"))
        entity <- paste0(entity, "_id")

    if (is.na(idx)) {                   # new column, arbitrary index
        .data[[entity]] <- seq_len(nrow(.data))
    } else {                           # existing column, maybe rename
        names(.data)[idx] <- entity
    }

    stopifnot(!anyDuplicated(.data[[entity]]), !anyNA(.data[[entity]]))

    .data[c(entity, setdiff(names(.data), entity))]
}

## encode list columns as JSON arrays, which Terra's TSV import
## recognizes as list attributes; see
## https://support.terra.bio/hc/en-us/articles/6624394629147
.avtable_import_list_element <-
    function(x, column, row)
{
    if (is.factor(x))
        x <- as.character(x)
    if (!is.null(x) && !(is.atomic(x) && is.null(dim(x)))) {
        stop(
            "list column '", column, "' row ", row, " must contain an ",
            "atomic vector; nested lists and other types are not supported",
            call. = FALSE
        )
    }
    if (anyNA(x)) {
        stop(
            "list column '", column, "' row ", row, " contains NA; ",
            "AnVIL list attributes cannot contain missing values",
            call. = FALSE
        )
    }
    if (!length(x))
        return("[]")
    as.character(jsonlite::toJSON(unname(x), auto_unbox = FALSE, digits = NA))
}

.avtable_import_list_columns <-
    function(.data)
{
    is_list <- vapply(.data, is.list, logical(1L))
    for (column in names(.data)[is_list]) {
        values <- .data[[column]]
        .data[[column]] <- vapply(
            seq_along(values),
            function(row) .avtable_import_list_element(
                values[[row]], column, row
            ),
            character(1L)
        )
    }
    .data
}

.avtable_import_write_dataset <-
    function(.data, na)
{
    destination <- tempfile()
    write.table(
        .data, destination,
        quote = FALSE, sep = "\t", row.names=FALSE, na = na
    )

    destination
}

.avtable_import_pages_index <-
    function(.data, n, page, pageSize)
{
    ## import table in chunks to avoid server timeout;
    ## https://github.com/Bioconductor/AnVIL/issues/76

    ## arbitrary: use page size so that each 'chunk' is about 1M elements
    if (is.null(pageSize)) {
        N_ELEMENTS <- 1500000
        pageSize <- ifelse(
            prod(dim(.data)) > N_ELEMENTS,
            as.integer(floor(N_ELEMENTS / NCOL(.data))),
            NROW(.data)
        )
    }

    row_index <- seq_len(NROW(.data))
    ## assign all rows a page
    page_id <- (row_index - 1L) %/% pageSize + 1L
    ## exclude rows before `page`
    page_id[page_id < page] <- 0L
    ## return a maximum of `n` elements after the first non-zero element
    if (is.finite(n))
        page_id[row_index > sum(page_id == 0) + n] <- 0L

    pages <- split(row_index, page_id)
    pages <- pages[names(pages) != "0"]
    message("pageSize = ", pageSize, " rows (", length(pages), " pages)")
    pages
}

.avtable_import_chunks <-
    function(
        .data, namespace, name, delete_empty_values, na,
        n, page, pageSize
    )
{
    ## divide large tables into chunks, if necessary
    pages <- .avtable_import_pages_index(.data, n, page, pageSize)

    ## progress bar
    n_uploaded <- 0L
    progress_bar <- NULL
    if (length(pages) > 1L && interactive()) {
        progress_bar <- txtProgressBar(max = sum(lengths(pages)), style = 3L)
        on.exit(close(progress_bar))
    }

    status <- rep("Failed", length(pages))
    job_id <- rep(NA_character_, length(pages))
    ## iterate through pages
    for (chunk_index in seq_along(pages)) {
        chunk_idx <- pages[[chunk_index]]
        chunk_name <- names(pages)[[chunk_index]]
        chunk <- .data[chunk_idx, , drop = FALSE]
        job_id[[chunk_index]] <- tryCatch({
            .avtable_import(chunk, namespace, name, delete_empty_values, na)
        }, error = function(err) {
            msg <- paste(strwrap(paste0(
                "failed to import page ", chunk_name,
                "; continuing to next page"
            )), collapse = "\n")
            warning(msg, "\n", conditionMessage(err), immediate. = TRUE)
            NA_character_
        })
        n_uploaded <- n_uploaded + length(chunk_idx)
        if (!is.null(progress_bar))
            setTxtProgressBar(progress_bar, n_uploaded)
    }
    status[!is.na(job_id)] <- "Uploaded"
    tibble(
        page = seq_along(pages),
        from_row = vapply(pages, min, integer(1)),
        to_row = vapply(pages, max, integer(1)),
        job_id = job_id,
        status = status,
        message = rep(NA_character_, length(status))
    )
}

.avtable_import <-
    function(.data, namespace, name, delete_empty_values, na)
{
    destination <- .avtable_import_write_dataset(.data, na)
    entities <- httr::upload_file(destination)

    response <- AnVIL::Terra()$flexibleImportEntities(
        namespace, URLencode(name),
        async = TRUE,
        deleteEmptyValues = delete_empty_values,
        entities = entities
    )
    avstop_for_status(response, "avtable_import")
    content(response)$jobId
}

# avtable_import ----------------------------------------------------------

#' @describeIn avtable-methods upload a table to the DATA tab
#'
#' @param entity `character(1)` column name of `.data` to be used as
#'     imported table name. When the table comes from R, this is
#'     usually a column name such as `sample`. The data will be
#'     imported into AnVIL as a table `sample`, with the `sample`
#'     column included with suffix `_id`, e.g., `sample_id`. A column
#'     in `.data` with suffix `_id` can also be used, e.g., `entity =
#'     "sample_id"`, creating the table `sample` with column
#'     `sample_id` in AnVIL. Finally, a value of `entity` that is not
#'     a column in `.data`, e.g., `entity = "unknown"`, will cause a
#'     new table with name `entity` and entity values
#'     `seq_len(nrow(.data))`.
#'
#' @param delete_empty_values logical(1) when `TRUE`, remove entities
#'     not included in `.data` from the DATA table. Default: `FALSE`.
#'
#' @details `avtable_import()` tries to work around limitations in
#'     `.data` size in the AnVIL platform, using `pageSize` (number of
#'     rows) to import so that approximately 1500000 elements (rows x
#'     columns) are uploaded per chunk. For large `.data`, a progress
#'     bar summarizes progress on the import. Individual chunks may
#'     nonetheless fail to upload, with common reasons being an
#'     internal server error (HTTP error code 500) or transient
#'     authorization failure (HTTP 401). In these and other cases
#'     `avtable_import()` reports the failed page(s) as warnings. The
#'     user can attempt to import these individually using the `page`
#'     argument. If many pages fail to import, a strategy might be to
#'     provide an explicit `pageSize` less than the automatically
#'     determined size.
#'
#' @details List columns in `.data`, e.g., `tibble(id = 1, cohorts =
#'     list(c("A", "B")))`, are imported as AnVIL list attributes.
#'     Each element must be an atomic vector (character, numeric,
#'     integer, logical, or factor) without `NA` values; elements are
#'     encoded as JSON arrays (e.g., `["A","B"]`) in the uploaded TSV.
#'     `avtable()` returns list attributes as two columns,
#'     `<name>.items` (a list column of the values) and
#'     `<name>.itemsType`.
#'
#' @details Write operations (`avtable_import()`, `avtable_import_set()`,
#'     `avtable_delete()`, and `avtable_delete_values()`) first check
#'     that the user has write access (`WRITER`, `OWNER`, or
#'     `PROJECT_OWNER`) to the workspace, and stop with an informative
#'     error otherwise. A successful check is remembered for the rest
#'     of the R session.
#'
#' @details Re-importing values identical to those already in the
#'     table (no entity changes) is reported by Terra as a failed job;
#'     `avtable_import_status()` shows status `"Error"` with message
#'     'All entities failed to update. There were 0 errors in total.'
#'     The existing table is unchanged.
#'
#' @importFrom AnVILBase avtable_import
#' @exportMethod avtable_import
setMethod(
    "avtable_import",
    signature = c(platform = "gcp"),
    definition = function(
        .data,
        entity = names(.data)[[1L]],
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        delete_empty_values = FALSE,
        na = "NA",
        n = Inf,
        page = 1L,
        pageSize = NULL,
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            is.data.frame(.data),
            isScalarCharacter(entity),
            isScalarCharacter(namespace),
            isScalarCharacter(name),
            isScalarLogical(delete_empty_values),
            isScalarCharacter(na, zchar = TRUE),
            isScalarNumber(n, infinite.ok = TRUE),
            isScalarInteger(as.integer(page)),
            is.null(pageSize) || isScalarInteger(as.integer(pageSize))
        )

        ## identify the 'entity' column
        .data <- .avtable_import_set_entity(.data, entity)
        ## encode list columns as JSON arrays
        .data <- .avtable_import_list_columns(.data)
        checkInstalled("AnVIL")
        .avworkspace_stop_if_not_writable(namespace, name, "avtable_import")
        ## divide large tables into chunks, if necessary
        .avtable_import_chunks(
            .data, namespace, name, delete_empty_values, na,
            n, page, pageSize
        )
    }
)
# avtable_import_set ------------------------------------------------------

#' @describeIn avtable-methods Import set membership information to a table in
#'   the AnVIL workspace
#'
#' @param origin character(1) name of the entity (table) used to
#'     create the set e.g "sample", "participant",
#'     etc.
#'
#' @param set `character(1)` column name of `.data` identifying the
#'     set(s) to be created.
#'
#' @param member `character()` vector of entity from the avtable
#'     identified by `origin`. The values may repeat if an ID is in
#'     more than one set
#'
#' @details `avtable_import_set()` creates new rows in a table
#'     `<origin>_set`. One row will be created for each distinct value
#'     in the column identified by `set`. Each row entry has a
#'     corresponding column `<origin>` linking to one or more rows in
#'     the `<origin>` table, as given in the `member` column. The
#'     operation is somewhat like `split(member, set)`.
#'
#' @return `avtable_import_set()` returns a `character(1)` name of the
#'     imported AnVIL tibble.
#'
#' @importFrom utils write.table
#' @importFrom dplyr all_of
#'
#' @examples
#' library(AnVILBase)
#' if (has_avworkspace(platform = gcp()) && interactive()) {
#' ## editable copy of '1000G-high-coverage-2019' workspace
#' avworkspace("bioconductor-rpci-anvil/1000G-high-coverage-2019")
#' sample <-
#'     avtable("sample") |>                               # existing table
#'     mutate(set = sample(head(LETTERS), nrow(.), TRUE))  # arbitrary groups
#' sample |>                                   # new 'participant_set' table
#'     avtable_import_set("participant", "set", "participant")
#' sample |>                                   # new 'sample_set' table
#'     avtable_import_set("sample", "set", "name")
#' }
#'
#' @exportMethod avtable_import_set
setMethod("avtable_import_set", signature = c(platform = "gcp"), definition =
    function(
        .data, origin, set = names(.data)[[1]], member = names(.data)[[2]],
        namespace = avworkspace_namespace(), name = avworkspace_name(),
        delete_empty_values = FALSE, na = "NA",
        n = Inf, page = 1L, pageSize = NULL,
        ..., platform = cloud_platform()
    ) {
        stopifnot(
            is.data.frame(.data),
            isScalarCharacter(origin),
            isScalarCharacter(set),
            set %in% names(.data),
            isScalarCharacter(member),
            !identical(set, member), member %in% names(.data),
            isScalarCharacter(namespace),
            isScalarCharacter(name),
            isScalarLogical(delete_empty_values),
            isScalarCharacter(na, zchar = TRUE),
            isScalarNumber(n, infinite.ok = TRUE),
            isScalarInteger(as.integer(page)),
            is.null(pageSize) || isScalarInteger(as.integer(pageSize))
        )
        origin <- URLencode(origin)

        .data <-
            .data |>
            select(all_of(c(set, member)))
        names(.data)[[1]] <- paste0("membership:", origin, "_set_id")
        names(.data)[[2]] <- origin

        checkInstalled("AnVIL")
        .avworkspace_stop_if_not_writable(
            namespace, name, "avtable_import_set"
        )
        .avtable_import_chunks(
            .data, namespace, name, delete_empty_values, na,
            n, page, pageSize
        )
    }
)


# avtable_delete ----------------------------------------------------------

#' @describeIn avtable-methods Delete a table from the AnVIL workspace.
#'
#' @return `avtable_delete()` returns `TRUE` if the table is successfully
#'   deleted.
#'
#' @importFrom AnVILBase avtable_delete
#' @exportMethod avtable_delete
setMethod(
    "avtable_delete",
    signature = c(platform = "gcp"),
    definition = function(
        table,
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(table),
            isScalarCharacter(namespace),
            isScalarCharacter(name)
        )
        table <- URLencode(table)
        checkInstalled("AnVIL")
        .avworkspace_stop_if_not_writable(namespace, name, "avtable_delete")
        response <- AnVIL::Rawls()$deleteEntitiesOfType(
            namespace, URLencode(name), table
        )
        avstop_for_status(response, "avtable_delete")
        TRUE
    }
)

# avtable_delete_values ---------------------------------------------------

#' @describeIn avtable-methods Delete rows from a table in the AnVIL workspace
#'
#' @param values vector of values in the entity (key) column of
#'     `table` to be deleted. A table `sample` has an associated
#'     entity column with suffix `_id`, e.g., `sample_id`. Rows with
#'     entity column entries matching `values` are deleted.
#'
#' @return `avtable_delete_values()` returns a `tibble` representing
#'     deleted entities, invisibly.
#'
#' @importFrom utils capture.output
#' @importFrom httr status_code
#'
#' @exportMethod avtable_delete_values
setMethod("avtable_delete_values", signature = c(platform = "gcp"),
    definition =  function(
        table, values,
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        ..., platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(table),
            isScalarCharacter(namespace),
            isScalarCharacter(name)
        )

        body <- tibble(entityType = table, entityName = as.character(values))

        checkInstalled("AnVIL")
        .avworkspace_stop_if_not_writable(
            namespace, name, "avtable_delete_values"
        )
        response <- AnVIL::Terra()$deleteEntities(
            namespace, URLencode(name), body
        )
        if (status_code(response) == 409L) {
            tbl <-
                response |>
                flatten() |>
                capture.output() |>
                paste(collapse = "\n")
            stop(
                "\n",
                "  'values' (entityName) appear in more than one table",
                " (entityType);",
                "\n  delete entityName from leaf tables first\n\n",
                tbl
            )
        }
        avstop_for_status(response, "avtable_delete_values") # other errors

        invisible(body)
    }
)
