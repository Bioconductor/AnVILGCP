#' @name gcp-methods
#'
#' @aliases avcopy avlist avremove avbackup avrestore avstorage
#'
#' @title Methods compatible with the GCP platform class
#'
#' @description `avcopy()`: copy contents of `source` to `destination`. At
#'   least one of `source` or `destination` must be Google cloud bucket;
#'   `source` can be a character vector with length greater than 1. Use
#'   `gsutil_help("cp")` for `gsutil` help.
#'
#'   `avlist()`: List contents of a google cloud bucket or, if `source` is
#'   missing, all Cloud Storage buckets under your default project ID
#'
#'   `avremove()`: remove contents of a Google Cloud Bucket.
#'
#'   `avbackup()`,`avrestore()`: synchronize a source and a destination. If the
#'   destination is on the local file system, it must be a directory or not yet
#'   exist (in which case a directory will be created).
#'
#'   `avstorage()` returns the workspace bucket, i.e., the google bucket
#'   associated with a workspace. Bucket content can be visualized under the
#'   'DATA' tab, 'Files' item.
#'
#'   `avworkspaces()`: returns a tibble with columns including the name, last
#'   modification time, namespace, and owner status.
#'
#'   `avtable_import()`: returns a `tibble()` containing the page number, 'from'
#'   and 'to' rows included in the page, job identifier, initial status of the
#'   uploaded 'chunks', and any (error) messages generated during status check.
#'   Use `avtable_import_status()` to query current status.
#'
#' @param source `character(1)`, (`character()` for `avlist()`, `avcopy()`)
#'   paths to a google storage bucket, possibly with wild-cards for file-level
#'   pattern matching.
#'
#' @param destination `character(1)`, google cloud bucket or local file system
#'   destination path.
#'
#' @param recursive `logical(1)`; perform operation recursively from `source`?.
#'   Default: `FALSE`.
#'
#' @param parallel `logical(1)`, perform parallel multi-threaded /
#'   multi-processing (default is `TRUE`).
#'
#' @param overwrite `logical(1)`, replace existing local files when
#'   copying from a bucket to the local file system? Default: `FALSE`.
#'   See Details.
#'
#' @param platform `gcp()` The cloud platform class to dispatch on as given by
#'   [AnVILBase::cloud_platform]. Typically not set manually as
#'   `cloud_platform()` returns the `"gcp"` class for Google Cloud Platform
#'   workspaces on AnVIL.
#'
#' @param force `logical(1)`: continue silently despite errors when
#'     removing multiple objects. Default: `FALSE`.
#'
#' @param exclude `character(1)` a python regular expression of bucket
#'     paths to exclude from synchronization. E.g.,
#'     `'.*(\\.png|\\.txt)$"` excludes '.png' and .txt' files.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @param delete `logical(1)`, when `TRUE`, remove files in
#'     `destination` that are not in `source`. Exercise caution when
#'     you use this option: it's possible to delete large amounts of
#'     data accidentally if, for example, you erroneously reverse
#'     source and destination.
#'
#' @param ... additional arguments passed as-is to the `gsutil` subcommand.
#'
#' @details When `destination` is on the local file system and
#'   `overwrite = FALSE` (the default), `avcopy()` checks whether the copy
#'   would replace existing local files: `destination` itself, or, when
#'   `destination` is a directory, the files (or, with `recursive = TRUE`,
#'   directories) named by `basename(source)` within it. Wildcards in
#'   `source` are matched against files in `destination`. If any exist,
#'   an interactive session asks whether to overwrite them; otherwise,
#'   or when the answer is not 'yes', `avcopy()` stops with an error
#'   and copies nothing. Use `overwrite = TRUE` to replace existing files
#'   without asking.
#'
#' @return `avcopy()`: exit status of `avcopy()`, invisibly.
#'   `avlist()`: `character()` listing of `source` content.
#'   `avremove()`: exit status of `gsutil rm`, invisibly.
#'   `avbackup()`: exit status of `gsutil rsync`, invisbly.
#'   `avrestore()`: exit status of `gsutil rsync`, invisbly.
#'   `avstorage()` returns a `character(1)` bucket identifier prefixed with
#'     `gs://`
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#'
#' @examples
#' src <-
#'    "gs://genomics-public-data/1000-genomes/other/sample_info/sample_info.csv"
#' if (has_avworkspace(platform = gcp())) {
#'    avcopy(src, tempdir())
#'    ## internal gsutil_*() commands work with spaces in source or destination
#'    destination <- file.path(tempdir(), "foo bar")
#'    avcopy(src, destination)
#'    file.exists(destination)
#' }
#' if (has_avworkspace(strict = TRUE, platform = gcp()))
#'     ## From within AnVIL...
#'     bucket <- avstorage()                        # discover bucket
#'
#' if (has_avworkspace(strict = TRUE, platform = gcp()) && interactive()) {
#'     path <- file.path(bucket, "mtcars.tab")
#'     avlist(dirname(path))                    # no 'mtcars.tab'...
#'     write.table(mtcars, gsutil_pipe(path, "w")) # write to bucket
#'     gsutil_stat(path)                           # yep, there!
#'     read.table(gsutil_pipe(path, "r"))          # read from bucket
#' }
NULL

# avcopy ------------------------------------------------------------------

## local files that copying 'source' to a local 'destination' would
## replace; follows 'gsutil cp' naming: a 'destination' directory
## receives 'basename(source)'
.avcopy_existing_targets <-
    function(source, destination)
{
    if (GCPtools::gsutil_is_uri(destination))
        return(character())

    is_dir <- dir.exists(destination) || endsWith(destination, "/")
    if (!is_dir)
        return(destination[file.exists(destination)])

    ## 'gs://bucket/dir/' copies to 'destination/dir'
    names <- basename(sub("/+$", "", source))
    targets <- file.path(sub("/+$", "", destination), names)
    has_wildcard <- grepl("[*?[]", names)
    existing <- c(
        targets[!has_wildcard][file.exists(targets[!has_wildcard])],
        Sys.glob(targets[has_wildcard])
    )
    unique(existing)
}

## wrappers to allow mocking in tests
.avcopy_interactive <- function() interactive()

#' @importFrom utils askYesNo
.avcopy_ask <- function(msg) askYesNo(msg, default = FALSE)

.avcopy_check_overwrite <-
    function(existing, overwrite)
{
    if (overwrite || !length(existing))
        return(invisible(TRUE))

    n <- length(existing)
    shown <- utils::head(existing, 5L)
    files <- paste0(
        "  ", paste(shown, collapse = "\n  "),
        if (n > length(shown)) paste0("\n  ... and ", n - length(shown), " more")
    )
    what <- sprintf(
        "'avcopy()' would overwrite %d existing local file%s:\n%s\n",
        n, if (n == 1L) "" else "s", files
    )
    hint <- "Use 'overwrite = TRUE' to replace existing files without asking."

    if (.avcopy_interactive()) {
        answer <- .avcopy_ask(paste0(what, hint, "\nOverwrite?"))
        if (isTRUE(answer))
            return(invisible(TRUE))
    }
    stop(what, "No files were copied. ", hint, call. = FALSE)
}

#' @describeIn gcp-methods copy contents of `source` to `destination` with
#'   `gsutil`
#'
#' @importFrom AnVILBase avcopy
#' @exportMethod avcopy
setMethod(
    "avcopy",
    signature = c(platform = "gcp"),
    definition =  function(
        source, destination,
        ...,
        recursive = FALSE, parallel = TRUE, overwrite = FALSE,
        platform = cloud_platform()
    ) {
        stopifnot(
            isCharacter(source), isScalarCharacter(destination),
            isScalarLogical(overwrite)
        )
        existing <- .avcopy_existing_targets(source, destination)
        .avcopy_check_overwrite(existing, overwrite)

        GCPtools::gsutil_cp(
            source = source, destination = destination,
            ...,
            recursive = recursive, parallel = parallel
        )
    }
)

# avlist ------------------------------------------------------------------

#' @describeIn gcp-methods list contents of `source` with `gsutil`
#'
#' @importFrom AnVILBase avlist
#' @importFrom BiocBaseUtils isScalarLogical
#' @exportMethod avlist
setMethod(
    "avlist",
    signature = c(platform = "gcp"),
    definition = function(
        source = character(),
        recursive = FALSE,
        ...,
        platform = cloud_platform()
    ) {
        GCPtools::gsutil_ls(
            source = source, ..., recursive = recursive
        )
    }
)

# avremove ----------------------------------------------------------------

#' @describeIn gcp-methods remove contents of `source` with `gsutil`
#'
#' @importFrom AnVILBase avremove
#' @exportMethod avremove
setMethod(
    "avremove",
    signature = c(platform = "gcp"),
    definition = function(
        source,
        recursive = FALSE,
        force = FALSE,
        parallel = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        GCPtools::gsutil_rm(
            source = source,
            ...,
            recursive = recursive, force = force, parallel = parallel
        )
    }
)

# avbackup ----------------------------------------------------------------

#' @describeIn gcp-methods backup contents of `source` with `gsutil`
#'
#' @details
#' `avbackup()': To make `"gs://mybucket/data"` match the contents
#' of the local directory `"data"` you could do:
#'
#' \code{avbackup("data", "gs://mybucket/data", delete = TRUE)}
#'
#' To make the local directory "data" the same as the contents of
#' gs://mybucket/data:
#'
#' \code{avrestore("gs://mybucket/data", "data", delete = TRUE)}
#'
#' If `destination` is a local path and does not exist, it will be
#' created.
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#' @importFrom AnVILBase avbackup
#'
#' @exportMethod avbackup
setMethod(
    "avbackup",
    signature = c(platform = "gcp"),
    definition = function(
        source, destination,
        recursive = FALSE,
        exclude = NULL,
        dry = TRUE,
        delete = FALSE,
        parallel = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        if (!GCPtools::gsutil_is_uri(destination))
            stop("'destination' must be a gs:// URI")

        gsutil_rsync(
            source = source, destination = destination,
            ...,
            exclude = exclude, dry = dry, delete = delete,
            recursive = recursive, parallel = parallel
        )
    }
)

# avrestore ---------------------------------------------------------------

#' @describeIn gcp-methods restore contents of `source` with `gsutil`
#'
#' @importFrom AnVILBase avrestore
#' @exportMethod avrestore
setMethod(
    "avrestore",
    signature = c(platform = "gcp"),
    definition = function(
        source, destination,
        recursive = FALSE,
        exclude = NULL,
        dry = TRUE,
        delete = FALSE,
        parallel = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        if (!GCPtools::gsutil_is_uri(source))
            stop("'source' must be a gs:// URI")

        gsutil_rsync(
            source = source, destination = destination,
            ...,
            exclude = exclude, dry = dry, delete = delete,
            recursive = recursive, parallel = parallel
        )
    }
)

# avstorage ---------------------------------------------------------------

#' @describeIn gcp-methods get the storage bucket location
#'
#' @param namespace `character(1)` AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name `character(1)` AnVIL workspace name as returned by, e.g.,
#'     `avworkspace_name()`.
#'
#' @importFrom AnVILBase avstorage
#' @exportMethod avstorage
setMethod(
    "avstorage",
    signature = c(platform = "gcp"),
    definition = function(
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(namespace),
            isScalarCharacter(name)
        )

        if (.avbucket_cache$exists(namespace, name)) {
            bucket <- .avbucket_cache$get(namespace, name)
        } else {
            checkInstalled("AnVIL")
            response <- AnVIL::Terra()$getWorkspace(
                namespace, URLencode(name), "workspace.bucketName"
            )
            avstop_for_status(response, "avstorage")
            bucket <- as.list(response)$workspace$bucketName
            .avbucket_cache$set(namespace, name, bucket)
        }

        paste0("gs://", bucket)
    }
)
