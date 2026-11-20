.avworkflow_job <-
    function(x)
{
    succeeded <- 0L
    failed <- 0L
    if ("Succeeded" %in% names(x$workflowStatuses))
        succeeded <- x$workflowStatuses$Succeeded
    if ("Failed" %in% names(x$workflowStatuses))
        failed <- x$workflowStatuses$Failed

    list(
        submissionId = x[["submissionId"]],
        submitter = x[["submitter"]],
        submissionDate = x[["submissionDate"]],
        status = x[["status"]],
        succeeded = succeeded,
        failed = failed,
        submissionRoot = x[["submissionRoot"]],
        methodConfigurationNamespace =
            x[["methodConfigurationNamespace"]] %||% NA_character_,
        methodConfigurationName =
            x[["methodConfigurationName"]] %||% NA_character_
    )
}

## columns added after the original columns, so positional use of
## earlier columns is unchanged
.AVWORKFLOW_JOBS_NEW_COLUMNS <-
    c("methodConfigurationNamespace", "methodConfigurationName")

.avworkflow_jobs_tbl <-
    function(submissions, namespace, name)
{
    if (length(submissions)) {
        submissions <- lapply(submissions, .avworkflow_job)
    } else {
        submissions <- list(
            submissionId = character(),
            submitter = character(),
            submissionDate = character(),
            status = character(),
            succeeded = integer(),
            failed = integer(),
            submissionRoot = character(),
            methodConfigurationNamespace = character(),
            methodConfigurationName = character()
        )
    }

    bind_rows(submissions) |>
        mutate(
            submissionDate = .POSIXct(as.numeric(
                as.POSIXct(.data$submissionDate, "%FT%T", tz="UTC")
            )),
            namespace = namespace,
            name = name
        ) |>
        relocate(all_of(.AVWORKFLOW_JOBS_NEW_COLUMNS), .after = "name") |>
        arrange(desc(.data$submissionDate))
}

#' @name avworkflow-methods
#'
#' @title AnVIL workflow methods
#'
#'@description Methods for working with AnVIL workflow execution.
#'  `avworkflow_jobs()` returns a tibble summarizing submitted workflow jobs for
#'  a namespace and name.
#'
#' @inheritParams avnotebooks-methods
#'
#' @return `avworkflow_jobs()` returns a `tibble`, sorted by
#'     `submissionDate`, with columns
#'
#' - submissionId character() job identifier from the workflow runner.
#' - submitter character() AnVIL user id of individual submitting the job.
#' - submissionDate POSIXct() date (in local time zone) of job submission.
#' - status character() job status, with values 'Accepted' 'Evaluating'
#'   'Submitting' 'Submitted' 'Aborting' 'Aborted' 'Done'
#' - succeeded integer() number of workflows succeeding.
#' - failed integer() number of workflows failing.
#' - submissionRoot character() cloud storage path of the submission
#'   outputs.
#' - namespace character() AnVIL workspace namespace.
#' - name character() AnVIL workspace name.
#' - methodConfigurationNamespace character() namespace of the workflow
#'   (method configuration) submitted, as used by the
#'   `workflow_namespace` argument of `avworkflow_configuration_get()`.
#' - methodConfigurationName character() name of the workflow submitted,
#'   as used by the `workflow_name` argument of
#'   `avworkflow_configuration_get()`.
#'
#' @details Use the `methodConfigurationNamespace` and
#'   `methodConfigurationName` columns to find jobs for a particular
#'   workflow, e.g., the most recent successful submission:
#'
#' ```
#' avworkflow_jobs() |>
#'     dplyr::filter(
#'         methodConfigurationName == "my_workflow",
#'         status == "Done", succeeded > 0, failed == 0
#'     ) |>
#'     head(1)
#' ```
#'
#' The `methodConfigurationName` is the workflow name shown in the
#' workspace WORKFLOWS tab; it may differ from the `workflowName`
#' (WDL workflow name) reported by `avworkflow_info()`.
#'
#' @examples
#' library(AnVILBase)
#' if (has_avworkspace(strict = TRUE, platform = gcp()))
#'     ## from within AnVIL
#'     avworkflow_jobs()
NULL

#' @describeIn avworkflow-methods List workflow jobs in the workspace
#'
#' @importFrom dplyr bind_rows mutate desc relocate all_of
#' @importFrom AnVILBase avworkflow_jobs avstop_for_status
#'
#' @exportMethod avworkflow_jobs
setMethod("avworkflow_jobs", signature = c(platform = "gcp"), definition =
    function(
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(namespace),
            isScalarCharacter(name)
        )

        checkInstalled("AnVIL")
        response <- AnVIL::Terra()$listSubmissions(namespace, URLencode(name))
        avstop_for_status(response, "avworkflow_jobs")

        submissions <- content(response, encoding = "UTF-8")
        .avworkflow_jobs_tbl(submissions, namespace, name)
    }
)
