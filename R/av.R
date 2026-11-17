#' @importFrom utils txtProgressBar setTxtProgressBar head
.avtable_pages <-
    function(FUN, ..., n, page, pageSize)
{
    result <- NULL
    bar <- NULL
    repeat {
        response <- FUN(..., page = page, pageSize = pageSize)
        result <- bind_rows(result, response$results)
        if (is.null(bar)) {
            max <- max(min(n, response$resultMetadata$filteredCount), 1L)
            bar <- txtProgressBar(max = max, style = 3L)
            on.exit(close(bar))
        }
        setTxtProgressBar(bar, min(NROW(result), n))
        page <- response$parameters$page + 1L
        test <-
            (page > response$resultMetadata$filteredPageCount) ||
            (NROW(result) >= n)
        if (test)
            break
    }

    head(result, n)
}

#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @importFrom utils URLencode
.avtable_paged1 <-
    function(
        namespace, name, table,
        page, pageSize, sortField, sortDirection,
        filterTerms, filterOperator)
{
    response <- AnVIL::Terra()$entityQuery(
        namespace, URLencode(name), table,
        page, pageSize, sortField, sortDirection,
        filterTerms, filterOperator)
    avstop_for_status(response, "avtable_paged")

    lst <-
        response |>
        as.list()
    if (length(lst$results)) {
        results <- bind_cols(
            tibble(name = lst$results$name),
            as_tibble(lst$results$attributes)
        )
    } else {
        results <- tibble()
    }
    list(
        parameters = lst$parameters,
        resultMetadata = lst$resultMetadata,
        results = results
    )
}

#' @name av
#'
#' @title Miscellaneous functions for interacting with AnVIL tables and files
#'
#' @param n numeric(1) maximum number of rows to return
#'
#' @param page integer(1) first page of iteration
#'
#' @param pageSize integer(1) number of records per page. Generally,
#'     larger page sizes are more efficient.
#'
#' @param sortField character(1) field used to sort records when
#'     determining page order. Default is the entity field.
#'
#' @param sortDirection character(1) direction to sort entities
#'     (`"asc"`ending or `"desc"`ending) when paging.
#'
#' @param filterTerms character(1) string literal to select rows with
#'     an exact (substring) matches in column.
#'
#' @param filterOperator character(1) operator to use when multiple
#'     terms in `filterTerms=`, either `"and"` (default) or `"or"`.
#'
#' @inheritParams avtable-methods
#'
#' @return `avtable_paged()`: a tibble of data corresponding to the
#'     AnVIL table `table` in the specified workspace.
#'
#' @importFrom BiocBaseUtils isScalarNumber isScalarInteger
#' @importFrom dplyr across where mutate
#' @export
avtable_paged <-
    function(table,
        n = Inf, page = 1L, pageSize = 1000L,
        sortField = "name", sortDirection = c("asc", "desc"),
        filterTerms = character(),
        filterOperator = c("and", "or"),
        namespace = avworkspace_namespace(),
        name = avworkspace_name(), na = c("", "NA"))
{
    page <- as.integer(page)
    pageSize <- as.integer(pageSize)
    stopifnot(
        isScalarCharacter(table),
        isScalarNumber(n, infinite.ok = TRUE),
        isScalarInteger(page),
        isScalarInteger(pageSize),
        isScalarCharacter(sortField),
        length(filterTerms) == 0L || isScalarCharacter(filterTerms),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
        ## ,
        ## `unknown table; use 'avtables()' for valid names` =
        ##     .is_avtable(table, namespace, name)
    )
    sortDirection <- match.arg(sortDirection)
    filterOperator <- match.arg(filterOperator)
    name <- URLencode(name)
    na_fun <- .avtable_na(na)

    checkInstalled("AnVIL")
    tbl <- .avtable_pages(
        .avtable_paged1,
        namespace = namespace, name = name, table = table,
        sortField = sortField, sortDirection = sortDirection,
        filterTerms = filterTerms, filterOperator = filterOperator,
        n = n, page = page, pageSize = pageSize
    )
    names(tbl) <- sub("^name$", paste0(table, "_id"), names(tbl))
    tbl |>
        mutate(across(where(is.character), na_fun))
}

## Terra reports an upsert in which no entity changed (e.g., re-importing
## identical values) as status 'Error' with this message; used to add a
## hint to the warning, not to change the reported status
.avtable_import_is_noop <-
    function(message)
{
    !is.na(message) &&
        grepl("All entities failed to update", message, fixed = TRUE) &&
        grepl("There were 0 errors in total", message, fixed = TRUE)
}

#' @rdname av
#'
#' @description `avtable_import_status()` queries for the status of an
#'     'asynchronous' table import. Failed jobs generate a warning and
#'     have status `"Error"`. Terra reports re-importing values identical
#'     to the existing table as an `"Error"` ('All entities failed to
#'     update. There were 0 errors in total.'); the warning notes that the
#'     table is likely unchanged.
#'
#' @param job_status tibble() of job identifiers, returned by
#'     `avtable_import()` and `avtable_import_set()`.
#'
#' @importFrom httr content
#' @export
avtable_import_status <-
    function(job_status,
        namespace = avworkspace_namespace(), name = avworkspace_name())
{
    stopifnot(
        is.data.frame(job_status),
        c("job_id", "status") %in% colnames(job_status),
        isCharacter(job_status$job_id, na.ok = TRUE),
        isCharacter(job_status$status),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    todo <- !job_status$status %in% c("Done", "Failed", "Error")
    job_ids <- job_status$job_id[todo]
    n_jobs <- length(job_ids)
    updated_status <- rep(NA_character_, n_jobs)
    updated_message <- job_status$message
    names(updated_status) <- names(updated_message) <- job_ids

    progress_bar <- NULL
    message("checking status of ", n_jobs, " avtable import jobs")
    if (n_jobs > 1L && interactive()) {
        progress_bar <- txtProgressBar(max = n_jobs, style = 3L)
        on.exit(close(progress_bar))
    }

    checkInstalled("AnVIL")
    for (job_index in seq_len(n_jobs)) {
        job_id <- job_ids[[job_index]]
        tryCatch({
            response <- AnVIL::Terra()$importJobStatus(namespace, name, job_id)
            avstop_for_status(response, "avtable_import_status")
            content <- content(response)
            updated_status[[job_index]] <- content$status
            if ("message" %in% names(content)) {
                updated_message[[job_index]] <- gsub(
                    "[[:space:]]+", " ", content$message
                )
            } else {
                updated_message[[job_index]] <- NA_character_
            }
            if (identical(tolower(content$status), "error")) {
                hint <- if (
                    .avtable_import_is_noop(updated_message[[job_index]])
                )
                    paste0(
                        "\n  (Terra reports this when all imported values ",
                        "are identical to the existing table;\n  the ",
                        "table is likely unchanged)"
                    )
                stop(
                    "job failed with status: ",
                    updated_message[[job_index]], hint, call. = FALSE
                )
            }
        }, error = function(err) {
            err_msg <- conditionMessage(err)
            is_err_status <- grepl("job failed with status", err_msg)
            status_msg <- if (is_err_status) "error" else "failed to get"
            msg <- paste(strwrap(paste0(
                status_msg, " status of job_id '", job_id, "'; ",
                "continuing to next job"
            )), collapse = "\n")
            warning(msg, "\n", err_msg, immediate. = TRUE)
        })
        if (!is.null(progress_bar))
            setTxtProgressBar(progress_bar, job_index)
    }

    job_status$status[todo] <- updated_status
    job_status$message[todo] <- updated_message
    job_status
}

##
## workspace bucket
##

.avbucket_cache <- local({
    .key <- function(namespace, name)
        paste(namespace, name, sep = "/")
    buckets <- new.env(parent = emptyenv())

    list(exists = function(namespace, name) {
        exists(.key(namespace, name), envir = buckets)
    }, get = function(namespace, name) {
        buckets[[ .key(namespace, name) ]]
    }, set = function(namespace, name, bucket) {
        buckets[[ .key(namespace, name) ]] <- bucket
    }, keys = function() {
        names(buckets)
    }, flush = function() {
        rm(list = names(buckets), envir = buckets)
    })
})

.avbucket_path <-
    function(bucket, ...)
{
    stopifnot(GCPtools::gsutil_is_uri(bucket))

    ## get path without duplicate "/"
    args <- expand.grid(..., stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    args <- unname(as.list(args))
    bucket <- sub("/*$", "", bucket)
    args <- lapply(args, sub, pattern = "^/*", replacement = "")
    args <- lapply(args, sub, pattern = "/*$", replacement = "")
    args <- do.call(
        "mapply",
        c(list(
            FUN = paste,
            MoreArgs = list(sep = "/"),
            USE.NAMES = FALSE
        ), args)
    )
    paste0(bucket, ifelse(length(args), "/", ""), args)
}

#' @rdname av
#'
#' @description `avfiles_ls()` returns the paths of files in the
#'     workspace bucket.  `avfiles_backup()` copies files from the
#'     compute node file system to the workspace bucket.
#'     `avfiles_restore()` copies files from the workspace bucket to
#'     the compute node file system.  `avfiles_rm()` removes files or
#'     directories from the workspace bucket.
#'
#' @param path For `avfiles_ls(), the character(1) file or directory
#'     path to list. For `avfiles_rm()`, the character() (perhaps with
#'     length greater than 1) of files or directory paths to be
#'     removed. The elements of `path` can contain glob-style
#'     patterns, e.g., `vign*`.
#'
#' @param full_names logical(1) return names relative to `path`
#'     (`FALSE`, default) or root of the workspace bucket?
#'
#' @param recursive logical(1) list files recursively?
#'
#' @return `avfiles_ls()` returns a character vector of files in the
#'     workspace bucket.
#'
#' @examples
#' library(AnVILBase)
#' if (has_avworkspace(platform = gcp()) && interactive())
#'     avfiles_ls()
#'
#' @export
avfiles_ls <-
    function(
        path = "",
        full_names = FALSE,
        recursive = FALSE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name())
{
    stopifnot(
        isScalarCharacter(path, zchar = TRUE),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    bucket <- avstorage(namespace, name)
    source <- .avbucket_path(bucket, path)
    result <- avlist(source, recursive = recursive)
    if (full_names) {
        sub(paste0(bucket, "/*"), "", result)
    } else {
        sub(paste0(source, "/*"), "", result)
    }
}

#' @rdname av
#'
#' @details `avfiles_backup()` can be used to back-up individual files
#'     or entire directories, recursively.  When `recursive = FALSE`,
#'     files are backed up to the bucket with names approximately
#'     `paste0(destination, "/", basename(source))`.  When `recursive
#'     = TRUE` and source is a directory `path/to/foo/', files are
#'     backed up to bucket names that include the directory name,
#'     approximately `paste0(destination, "/", dir(basename(source),
#'     full.names = TRUE))`.  Naming conventions are described in
#'     detail in `gsutil_help("cp")`.
#'
#' @param source character() file paths. for `avfiles_backup()`,
#'     `source` can include directory names when `recursive = TRUE`.
#'
#' @param destination character(1) a google bucket
#'     (`gs://<bucket-id>/...`) to write files. The default is the
#'     workspace bucket.
#'
#' @param parallel logical(1) backup files using parallel transfer?
#'     See `?avcopy()`.
#'
#' @return `avfiles_backup()` returns, invisibly, the status code of the
#'     `avcopy()` command used to back up the files.
#'
#' @examples
#' library(AnVILBase)
#' if (has_avworkspace(platform = gcp()) && interactive()) {
#'     ## backup all files in the current directory
#'     ## default buckets are gs://<bucket-id>/<file-names>
#'     avfiles_backup(dir())
#'     ## backup working directory, recursively
#'     ## default buckets are gs://<bucket-id>/<basename(getwd())>/...
#'     avfiles_backup(getwd(), recursive = TRUE)
#' }
#'
#' @export
avfiles_backup <-
    function(
        source,
        destination = "",
        recursive = FALSE,
        parallel = TRUE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name()
    )
{
    stopifnot(
        `some 'source' paths do not exist` = all(file.exists(source)),
        isScalarCharacter(destination, zchar = TRUE),
        isScalarLogical(recursive),
        isScalarLogical(parallel),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    bucket <- avstorage(namespace, name)
    destination <- .avbucket_path(bucket, destination)
    avcopy(source, destination, recursive = recursive, parallel = parallel)
}

#' @rdname av
#'
#' @details `avfiles_restore()` behaves in a manner analogous to
#'     `avfiles_backup()`, copying files from the workspace bucket to
#'     the compute node file system.
#'
#' @export
avfiles_restore <-
    function(
        source,
        destination = ".",
        recursive = FALSE,
        parallel = TRUE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name()
    )
{
    stopifnot(
        isCharacter(source),
        isScalarCharacter(destination),
        `'destination' is not a directory` = dir.exists(destination),
        isScalarLogical(recursive),
        isScalarLogical(parallel),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    bucket <- avstorage(namespace, name)
    source <- .avbucket_path(bucket, source)
    avcopy(source, destination, recursive = recursive, parallel = parallel)
}

#' @rdname av
#'
#' @return `avfiles_rm()` on success, returns a list of the return
#'     codes of `avremove()`, invisibly.
#'
#' @export
avfiles_rm <-
    function(
        source,
        recursive = FALSE,
        parallel = TRUE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name()
    )
{
    stopifnot(
        isCharacter(source),
        isScalarLogical(recursive),
        isScalarLogical(parallel),
        isScalarCharacter(namespace),
        isScalarCharacter(name)
    )

    bucket <- avstorage(namespace, name)
    source <- .avbucket_path(bucket, source)
    result <- lapply(
        source, avremove, recursive = recursive, parallel = parallel
    )
    invisible(unlist(result))
}

##
## runtimes / persistent disks
##

#' @rdname av
#'
#' @description `avruntimes()` returns a tibble containing information
#'     about runtimes (notebooks or RStudio instances, for example)
#'     that the current user has access to.
#'
#' @return `avruntimes()` returns a tibble with columns
#'
#' - id: integer() runtime identifier.
#' - googleProject: character() billing account.
#' - tool: character() e.g., "Jupyter", "RStudio".
#' - status character() e.g., "Stopped", "Running".
#' - creator character() AnVIL account, typically "user@gmail.com".
#' - createdDate character() creation date.
#' - destroyedDate character() destruction date, or NA.
#' - dateAccessed character() date of (first?) access.
#' - runtimeName character().
#' - clusterServiceAccount character() service ('pet') account for
#'   this runtime.
#' - masterMachineType character() It is unclear which 'tool' populates
#'   which of the machineType columns).
#' - workerMachineType character().
#' - machineType character().
#' - persistentDiskId integer() identifier of persistent disk (see
#'   `avdisks()`), or `NA`.
#'
#' @examples
#' if (has_avworkspace(platform = gcp()))
#'     ## from within AnVIL
#'     avruntimes()
#'
#' @importFrom dplyr rename_with
#' @importFrom BiocBaseUtils checkInstalled
#'
#' @export
avruntimes <-
    function()
{
    template <- list(
        id = integer(0),
        googleProject = character(0),
        labels.tool = character(0),
        status = character(0),
        auditInfo.creator = character(0),
        auditInfo.createdDate = character(0),
        auditInfo.destroyedDate = logical(0),
        auditInfo.dateAccessed = character(0),
        runtimeName = character(0),
        labels.clusterServiceAccount = character(0),
        runtimeConfig.masterMachineType = character(0),
        runtimeConfig.workerMachineType = logical(0),
        runtimeConfig.machineType = character(0),
        runtimeConfig.persistentDiskId = integer(0)
    )
    checkInstalled("AnVIL")
    leo <- AnVIL::Leonardo()
    response <- leo$listRuntimes()
    avstop_for_status(response, "avruntimes")
    runtimes <- flatten(response)

    .tbl_with_template(runtimes, template) |>
        rename_with(~ sub(".*\\.", "", .x))
}

#' @rdname av
#'
#' @description `avruntime()` returns a tibble with the runtimes
#'     associated with a particular google project and account number;
#'     usually there is a single runtime satisfiying these criteria,
#'     and it is the runtime active in AnVIL.
#'
#' @param project `character(1)` project (billing account) name, as
#'     returned by, e.g., `gcloud_project()` or
#'     `avworkspace_namespace()`.
#'
#' @param account `character(1)` google account (email address
#'     associated with billing account), as returned by
#'     `gcloud_account()`.
#'
#' @return `avruntime()` returns a tibble witht he same structure as
#'     the return value of `avruntimes()`.
#'
#' @export
avruntime <- function(
    project = GCPtools::gcloud_project(),
    account = GCPtools::gcloud_account()
) {
    stopifnot(
        isScalarCharacter(project),
        isScalarCharacter(account)
    )
    rt <- avruntimes()
    rt |>
        filter(.data$googleProject == project, .data$creator == account)
}

#' @importFrom dplyr pull
.runtime_pet <-
    function(
        creator,
        tool = c("Jupyter", "RStudio"),
        namespace = avworkspace_namespace()
    )
{
    tool <- match.arg(tool)
    stopifnot(
        isScalarCharacter(tool),
        isScalarCharacter(creator),
        isScalarCharacter(namespace)
    )

    runtimes <- avruntimes()
    pet <-
        runtimes |>
        filter(
            .data$tool == {{ tool }},
            .data$creator == {{ creator }},
            .data$googleProject == {{ namespace }}
        ) |>
        pull(.data$clusterServiceAccount)

    if (!isScalarCharacter(pet))
        warning("'.runtime_pet' return value is not scalar")
    pet
}

#' @name av
#'
#' @description 'avdisks()` returns a tibble containing information
#'     about persistent disks associatd with the current user.
#'
#' @return `avdisks()` returns a tibble with columns
#'
#' - id character() disk identifier.
#' - googleProject: character() billing account.
#' - status, e.g, "Ready"
#' - size integer() in GB.
#' - diskType character().
#' - blockSize integer().
#' - creator character() AnVIL account, typically "user@gmail.com".
#' - createdDate character() creation date.
#' - destroyedDate character() destruction date, or NA.
#' - dateAccessed character() date of (first?) access.
#' - zone character() e.g.. "us-central1-a".
#' - name character().
#'
#' @examples
#' if (has_avworkspace(strict = TRUE, platform = gcp()))
#'     ## from within AnVIL
#'     avdisks()
#'
#' @export
avdisks <-
    function()
{
    template <- list(
        id = integer(0),
        googleProject = character(0),
        status = character(0),
        size = integer(0),
        diskType = character(0),
        blockSize = integer(0),
        auditInfo.creator = character(0),
        auditInfo.createdDate = character(0),
        auditInfo.destroyedDate = logical(0),
        auditInfo.dateAccessed = character(0),
        name = character(0),
        zone = character(0)
    )
    checkInstalled("AnVIL")
    leo <- AnVIL::Leonardo()
    response <- leo$listDisks()
    avstop_for_status(response, "avdisks")
    runtimes <- flatten(response)

    .tbl_with_template(runtimes, template) |>
        rename_with(~sub(".*\\.", "", .x))
}
