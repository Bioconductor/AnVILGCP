test_that(".avworkspace_check_access() allows write access levels", {
    for (level in c("WRITER", "OWNER", "PROJECT_OWNER")) {
        access <- list(accessLevel = level, canCompute = TRUE)
        expect_identical(
            .avworkspace_check_access(access, "ns", "nm", "avtable_import"),
            access
        )
    }
})

test_that(".avworkspace_check_access() reports insufficient access", {
    for (level in c("READER", "NO ACCESS")) {
        access <- list(accessLevel = level, canCompute = FALSE)
        expect_error(
            .avworkspace_check_access(access, "ns", "nm", "avtable_import"),
            paste0(
                "'avtable_import\\(\\)' requires write access to ",
                "workspace 'ns/nm'.*your access level is '", level, "'"
            )
        )
    }
    expect_error(
        .avworkspace_check_access(
            list(accessLevel = character(), canCompute = FALSE),
            "ns", "nm", "avdata_import"
        ),
        "requires write access"
    )
})

test_that(".avworkspace_check_access() checks compute when requested", {
    access <- list(accessLevel = "WRITER", canCompute = FALSE)
    expect_silent(
        .avworkspace_check_access(access, "ns", "nm", "avworkflow_stop")
    )
    expect_error(
        .avworkspace_check_access(
            access, "ns", "nm", "avworkflow_run", compute = TRUE
        ),
        "'avworkflow_run\\(\\)' requires compute permission"
    )
})

test_that("write operations stop before contacting AnVIL without access", {
    skip_if_not_installed("AnVIL")
    local_mocked_bindings(
        .avworkspace_access = function(namespace, name, caller)
            list(accessLevel = "READER", canCompute = FALSE),
        .avtable_import_chunks = function(...)
            stop("import should not be attempted")
    )
    .data <- data.frame(sample = c("a", "b"), value = 1:2)
    expect_error(
        avtable_import(
            .data, namespace = "ns", name = "nm", platform = gcp()
        ),
        "'avtable_import\\(\\)' requires write access"
    )
    expect_error(
        avtable_import_set(
            .data, "sample", "sample", "value",
            namespace = "ns", name = "nm", platform = gcp()
        ),
        "'avtable_import_set\\(\\)' requires write access"
    )
    expect_error(
        avtable_delete("sample", namespace = "ns", name = "nm", platform = gcp()),
        "'avtable_delete\\(\\)' requires write access"
    )
    expect_error(
        avtable_delete_values(
            "sample", "a", namespace = "ns", name = "nm", platform = gcp()
        ),
        "'avtable_delete_values\\(\\)' requires write access"
    )
    .avdata <- data.frame(
        type = "other", table = "workspace", key = "k", value = "v"
    )
    expect_error(
        avdata_import(.avdata, namespace = "ns", name = "nm"),
        "'avdata_import\\(\\)' requires write access"
    )
})

test_that(".avworkspace_stop_if_not_writable() caches only sufficient access", {
    .avworkspace_access_cache$flush()
    on.exit(.avworkspace_access_cache$flush())
    n_calls <- 0L
    level <- "READER"
    local_mocked_bindings(
        .avworkspace_access = function(namespace, name, caller) {
            n_calls <<- n_calls + 1L
            list(accessLevel = level, canCompute = FALSE)
        }
    )

    ## failures are not cached; access is re-queried each time
    expect_error(.avworkspace_stop_if_not_writable("ns", "nm", "f"))
    expect_error(.avworkspace_stop_if_not_writable("ns", "nm", "f"))
    expect_identical(n_calls, 2L)
    expect_identical(.avworkspace_access_cache$keys(), character())

    ## once granted, access is queried once and then cached
    level <- "WRITER"
    .avworkspace_stop_if_not_writable("ns", "nm", "f")
    .avworkspace_stop_if_not_writable("ns", "nm", "f")
    expect_identical(n_calls, 3L)
    expect_identical(.avworkspace_access_cache$keys(), "ns/nm")

    ## cached entry lacking compute is re-queried when compute required
    expect_error(
        .avworkspace_stop_if_not_writable("ns", "nm", "f", compute = TRUE),
        "requires compute permission"
    )
    expect_identical(n_calls, 4L)

    ## other workspaces are queried separately
    .avworkspace_stop_if_not_writable("ns", "other", "f")
    expect_identical(n_calls, 5L)
})
