#' @importFrom dplyr full_join select
.tbl_with_template <-
    function(tbl, tmpl)
{
    result <- as_tibble(tmpl)
    if (nrow(tbl)) {
        have <- intersect(names(tbl), names(tmpl))
        tbl <- select(tbl, have)
        result <-
            full_join(tbl, result, by = have) |>
            select(names(tmpl))
    }
    result
}

.pretty_text <- function(..., indent = 0L, exdent = 0L) {
    text <- paste(..., collapse = " ")
    paste(strwrap(text, indent = indent, exdent = exdent), collapse = "\n")
}

.is_workspace <-
    function(x)
{
    isScalarCharacter(x) &&
        ## exactly 1 `/`
        identical(lengths(regmatches(x, gregexpr("/", x, fixed = TRUE))), 1L)
}

.is_local_directory <- function(x)
    isScalarCharacter(x) && dir.exists(x)

.isScalarCharacter_or_NULL <- function(x, na.ok = FALSE, zchar = FALSE)
    isScalarCharacter(x, na.ok, zchar) || is.null(x)

.avworkspace <- local({
    hash <- new.env(parent = emptyenv())
    function(fun, key, value, warn = TRUE) {
        sysvar <- toupper(paste0("WORKSPACE_", key))
        if (is.null(value)) {
            if (is.null(hash[[key]])) {
                ## initialize
                hash[[key]] <- Sys.getenv(sysvar)
                if (!nzchar(hash[[key]]) && warn && interactive())
                    warning(
                        "'", sysvar, "' undefined; use `", fun, "()` to set"
                    )
            }
        } else {
            hash[[key]] <- ifelse(is.na(value), Sys.getenv(sysvar), value)
        }
        hash[[key]]
    }
})

## workspace access checks for write operations ------------------------

.WORKSPACE_WRITE_ACCESS <- c("WRITER", "OWNER", "PROJECT_OWNER")

## query the user's access level ('READER', 'WRITER', 'OWNER',
## 'NO ACCESS', ...) and compute permission on a workspace. A missing
## or inaccessible workspace results in an informative HTTP 404 error.
.avworkspace_access <-
    function(namespace, name, caller)
{
    checkInstalled("AnVIL")
    response <- AnVIL::Terra()$getWorkspace(
        namespace, URLencode(name), "accessLevel,canCompute"
    )
    avstop_for_status(response, caller)
    access <- content(response)
    list(
        accessLevel = as.character(access[["accessLevel"]]),
        canCompute = isTRUE(access[["canCompute"]])
    )
}

.avworkspace_has_access <-
    function(access, compute = FALSE)
{
    level <- access[["accessLevel"]]
    length(level) == 1L && level %in% .WORKSPACE_WRITE_ACCESS &&
        (!compute || isTRUE(access[["canCompute"]]))
}

.avworkspace_check_access <-
    function(access, namespace, name, caller, compute = FALSE)
{
    workspace <- paste0(namespace, "/", name)
    level <- access[["accessLevel"]]
    if (!.avworkspace_has_access(access))
        stop(
            "\n  '", caller, "()' requires write access to workspace '",
            workspace, "'",
            "\n  your access level is '", paste(level, collapse = ""), "'",
            "\n  ask a workspace owner for WRITER access or use",
            " 'avworkspace_clone()' to create an editable copy",
            call. = FALSE
        )
    if (!.avworkspace_has_access(access, compute))
        stop(
            "\n  '", caller, "()' requires compute permission in workspace '",
            workspace, "'",
            "\n  your access level is '", level, "' without 'canCompute'",
            "\n  ask a workspace owner to grant 'Can compute'",
            call. = FALSE
        )
    invisible(access)
}

## session cache of successful access checks, keyed by
## 'namespace/name'. Only sufficient access is cached, so a user
## granted access after a failed check is re-queried on the next call.
.avworkspace_access_cache <- local({
    .key <- function(namespace, name)
        paste(namespace, name, sep = "/")
    access <- new.env(parent = emptyenv())

    list(get = function(namespace, name) {
        access[[ .key(namespace, name) ]]
    }, set = function(namespace, name, value) {
        access[[ .key(namespace, name) ]] <- value
    }, keys = function() {
        names(access)
    }, flush = function() {
        rm(list = names(access), envir = access)
    })
})

## stop with an informative error unless the user can write to (and,
## optionally, compute in) the workspace. Used by write operations
## before contacting AnVIL, since e.g., a failed asynchronous import
## otherwise reports only 'HTTP 500 Internal Server Error'.
.avworkspace_stop_if_not_writable <-
    function(namespace, name, caller, compute = FALSE)
{
    cached <- .avworkspace_access_cache$get(namespace, name)
    if (!is.null(cached) && .avworkspace_has_access(cached, compute))
        return(invisible(cached))

    access <- .avworkspace_access(namespace, name, caller)
    .avworkspace_check_access(access, namespace, name, caller, compute)
    .avworkspace_access_cache$set(namespace, name, access)
    invisible(access)
}
