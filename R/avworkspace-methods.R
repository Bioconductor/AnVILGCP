#' @name avworkspace-methods
#'
#' @title AnVIL Workspace GCP methods
#'
#' @description `avworkspace_namespace()` and `avworkspace_name()` are utiliity
#'   functions to retrieve workspace namespace and name from environment
#'   variables or interfaces usually available in AnVIL notebooks or RStudio
#'   sessions. `avworkspace()` provides a convenient way to specify workspace
#'   namespace and name in a single command.
#'
#' @details `avworkspace_namespace()` is the billing account. If the
#'     `namespace=` argument is not provided, try `gcloud_project()`,
#'     and if that fails try `Sys.getenv("WORKSPACE_NAMESPACE")`.
#'
#' `avworkspace_name()` is the name of the workspace as it appears in
#' \url{https://app.terra.bio/#workspaces}. If not provided,
#' `avworkspace_name()` tries to use `Sys.getenv("WORKSPACE_NAME")`.
#'
#' Namespace and name values are cached across sessions, so explicitly
#' providing `avworkspace_name*()` is required at most once per
#' session. Revert to system settings with arguments `NA`.
#'
#' @inheritParams gcp-methods
#'
#' @param namespace character(1) AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name character(1) AnVIL workspace name as returned by, eg.,
#'     `avworkspace_name()`.
#'
#' @param warn logical(1) when `TRUE` (default), generate a warning
#'     when the workspace namespace or name cannot be determined.
#'
#' @param workspace when present, a `character(1)` providing the
#'     concatenated namespace and name, e.g.,
#'     `"bioconductor-rpci-anvil/Bioconductor-Package-AnVIL"`
#'
#' @return `avworkspace_namespace()`, and `avworkspace_name()` return
#'     `character(1)` identifiers. `avworkspace()` returns the
#'     character(1) concatenated namespace and name. The value
#'     returned by `avworkspace_name()` will be percent-encoded (e.g.,
#'     spaces `" "` replaced by `"%20"`).
#'
#' @include gcp-class.R
#'
#' @examples
#' avworkspace_namespace()
#' avworkspace_name()
#' avworkspace()
#'
NULL

# avworkspaces ------------------------------------------------------------

#' @describeIn avworkspace-methods list workspaces in the current project as a
#'   tibble
#'
#' @importFrom AnVILBase avworkspaces
#' @exportMethod avworkspaces
setMethod(
    f = "avworkspaces",
    signature = "gcp",
    definition = function(
        ...,
        platform = cloud_platform()
    ) {
        response <- Rawls()$listWorkspaces()
        avstop_for_status(response, "avworkspaces")

        AnVILBase::flatten(response) |>
            AnVILBase::avworkspaces_clean()
    }
)

# avworkspace_namespace ---------------------------------------------------

#' @describeIn avworkspace-methods Get or set the namespace of the current
#'   workspace
#'
#' @importFrom AnVILBase avworkspace_namespace
#' @exportMethod avworkspace_namespace
setMethod(
    f = "avworkspace_namespace",
    signature = "gcp",
    definition = function(
        namespace = NULL,
        warn = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        namespace <- .avworkspace(
            "avworkspace_namespace", "NAMESPACE", namespace, warn = FALSE
        )
        if (!nzchar(namespace)) {
            namespace <- tryCatch({
                gcloud_project()
            }, error = function(e) {
                NULL
            })
            namespace <- .avworkspace(
                "avworkspace_namespace", "NAMESPACE", namespace, warn = warn
            )
        }
        namespace
    }
)

# avworkspace_name --------------------------------------------------------

#' @describeIn avworkspace-methods Get or set the name of the current workspace
#'
#' @importFrom AnVILBase avworkspace_name
#' @importFrom utils URLencode
#'
#' @exportMethod avworkspace_name
setMethod(
    f = "avworkspace_name",
    signature = "gcp",
    definition = function(
        name = NULL,
        warn = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        value <- .avworkspace("avworkspace_name", "NAME", name, warn = warn)
        URLencode(value)
    }
)

# avworkspace -------------------------------------------------------------

#' @describeIn avworkspace-methods Get the current workspace namespace and name
#'   combination
#'
#' @importFrom AnVILBase avworkspace
#' @exportMethod avworkspace
setMethod(
    f = "avworkspace",
    signature = "gcp",
    definition = function(workspace = NULL, ..., platform = cloud_platform())
    {
        stopifnot(
            `'workspace' must be NULL or of the form 'namespace/name'` =
                is.null(workspace) || .is_workspace(workspace)
        )
        if (!is.null(workspace)) {
            wkspc <- strsplit(workspace, "/")[[1]]
            avworkspace_namespace(wkspc[[1]])
            avworkspace_name(wkspc[[2]])
        }
        paste0(avworkspace_namespace(), "/", avworkspace_name())
    }
)