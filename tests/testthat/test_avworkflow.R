.submission <- function(id, date, config, statuses = list(Succeeded = 1L)) {
    ## shape of one element of the Terra listSubmissions response
    list(
        deleteIntermediateOutputFiles = FALSE,
        methodConfigurationDeleted = FALSE,
        methodConfigurationName = config,
        methodConfigurationNamespace = "wf-ns",
        status = "Done",
        submissionDate = date,
        submissionEntity = list(entityType = "sample", entityName = "s1"),
        submissionId = id,
        submissionRoot = paste0("gs://bucket/submissions/", id),
        submitter = "user@example.org",
        useCallCache = TRUE,
        userComment = "",
        workflowStatuses = statuses
    )
}

test_that(".avworkflow_jobs_tbl() includes the workflow namespace and name", {
    submissions <- list(
        .submission("id-1", "2025-09-01T18:21:43.226Z", "wf_a"),
        .submission(
            "id-2", "2025-09-02T10:00:00.000Z", "wf_b",
            list(Succeeded = 2L, Failed = 1L)
        )
    )
    result <- .avworkflow_jobs_tbl(submissions, "ws-ns", "ws-name")

    ## original columns first and in the same order; new columns last
    expect_identical(
        names(result),
        c(
            "submissionId", "submitter", "submissionDate", "status",
            "succeeded", "failed", "submissionRoot", "namespace", "name",
            "methodConfigurationNamespace", "methodConfigurationName"
        )
    )
    ## most recent first
    expect_identical(result[["submissionId"]], c("id-2", "id-1"))
    expect_identical(result[["methodConfigurationName"]], c("wf_b", "wf_a"))
    expect_identical(
        result[["methodConfigurationNamespace"]], c("wf-ns", "wf-ns")
    )
    expect_identical(result[["succeeded"]], c(2L, 1L))
    expect_identical(result[["failed"]], c(1L, 0L))
    expect_identical(result[["namespace"]], c("ws-ns", "ws-ns"))
    expect_identical(result[["name"]], c("ws-name", "ws-name"))
    expect_s3_class(result[["submissionDate"]], "POSIXct")
})

test_that(".avworkflow_jobs_tbl() uses NA for missing workflow fields", {
    submission <- .submission("id-1", "2025-09-01T18:21:43.226Z", "wf_a")
    submission[["methodConfigurationName"]] <- NULL
    submission[["methodConfigurationNamespace"]] <- NULL
    result <- .avworkflow_jobs_tbl(list(submission), "ws-ns", "ws-name")
    expect_identical(result[["methodConfigurationName"]], NA_character_)
    expect_identical(result[["methodConfigurationNamespace"]], NA_character_)
})

test_that(".avworkflow_jobs_tbl() returns all columns with no submissions", {
    result <- .avworkflow_jobs_tbl(list(), "ws-ns", "ws-name")
    expect_identical(nrow(result), 0L)
    expect_identical(
        names(result),
        c(
            "submissionId", "submitter", "submissionDate", "status",
            "succeeded", "failed", "submissionRoot", "namespace", "name",
            "methodConfigurationNamespace", "methodConfigurationName"
        )
    )
    expect_type(result[["methodConfigurationName"]], "character")
})
