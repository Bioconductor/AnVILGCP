test_that(".avcopy_existing_targets() finds files a download would replace", {
    dir <- withr::local_tempdir()
    file.create(file.path(dir, c("a.txt", "b.txt", "c.csv")))
    dir.create(file.path(dir, "sub"))
    bucket <- "gs://bucket/path"

    ## bucket destination: nothing local is replaced
    expect_identical(
        .avcopy_existing_targets(file.path(dir, "a.txt"), bucket),
        character()
    )

    ## destination is a file path
    expect_identical(
        .avcopy_existing_targets(
            file.path(bucket, "a.txt"), file.path(dir, "a.txt")
        ),
        file.path(dir, "a.txt")
    )
    expect_identical(
        .avcopy_existing_targets(
            file.path(bucket, "a.txt"), file.path(dir, "new.txt")
        ),
        character()
    )

    ## destination is a directory, with or without trailing slash
    source <- file.path(bucket, c("a.txt", "new.txt", "b.txt"))
    expected <- file.path(dir, c("a.txt", "b.txt"))
    expect_identical(.avcopy_existing_targets(source, dir), expected)
    expect_identical(
        .avcopy_existing_targets(source, paste0(dir, "/")), expected
    )

    ## recursive copy of a bucket 'directory' into an existing one
    expect_identical(
        .avcopy_existing_targets(file.path(bucket, "sub/"), dir),
        file.path(dir, "sub")
    )

    ## wildcards are matched against local files
    expect_identical(
        .avcopy_existing_targets(file.path(bucket, "*.txt"), dir),
        file.path(dir, c("a.txt", "b.txt"))
    )
    expect_identical(
        .avcopy_existing_targets(file.path(bucket, "*.tsv"), dir),
        character()
    )

    ## new directory that does not exist yet
    expect_identical(
        .avcopy_existing_targets(source, file.path(dir, "newdir/")),
        character()
    )
})

test_that(".avcopy_check_overwrite() passes without existing files", {
    expect_true(.avcopy_check_overwrite(character(), overwrite = FALSE))
    expect_true(.avcopy_check_overwrite("a.txt", overwrite = TRUE))
})

test_that(".avcopy_check_overwrite() stops when not interactive", {
    local_mocked_bindings(
        .avcopy_interactive = function() FALSE,
        .avcopy_ask = function(msg) stop("should not ask")
    )
    expect_error(
        .avcopy_check_overwrite("a.txt", overwrite = FALSE),
        paste0(
            "would overwrite 1 existing local file:\n  a.txt\n",
            "No files were copied. Use 'overwrite = TRUE'"
        )
    )
    files <- paste0("f", 1:7, ".txt")
    expect_error(
        .avcopy_check_overwrite(files, overwrite = FALSE),
        "7 existing local files:.*f5.txt\n  ... and 2 more"
    )
})

test_that(".avcopy_check_overwrite() asks when interactive", {
    asked <- NULL
    answer <- TRUE
    local_mocked_bindings(
        .avcopy_interactive = function() TRUE,
        .avcopy_ask = function(msg) {
            asked <<- msg
            answer
        }
    )
    expect_true(.avcopy_check_overwrite("a.txt", overwrite = FALSE))
    expect_match(asked, "a.txt")
    expect_match(asked, "overwrite = TRUE", fixed = TRUE)
    expect_match(asked, "Overwrite\\?$")

    for (answer in list(FALSE, NA)) {
        expect_error(
            .avcopy_check_overwrite("a.txt", overwrite = FALSE),
            "No files were copied. Use 'overwrite = TRUE'"
        )
    }
})

test_that("avcopy() checks for local files before copying", {
    dir <- withr::local_tempdir()
    destination <- file.path(dir, "a.txt")
    file.create(destination)
    copied <- FALSE
    local_mocked_bindings(
        gsutil_cp = function(...) copied <<- TRUE,
        .package = "GCPtools"
    )
    local_mocked_bindings(.avcopy_interactive = function() FALSE)

    expect_error(
        avcopy("gs://bucket/a.txt", destination, platform = gcp()),
        "would overwrite 1 existing local file"
    )
    expect_false(copied)

    avcopy(
        "gs://bucket/a.txt", destination, overwrite = TRUE, platform = gcp()
    )
    expect_true(copied)

    ## uploads are not checked
    copied <- FALSE
    avcopy(destination, "gs://bucket/", platform = gcp())
    expect_true(copied)

    expect_error(
        avcopy("gs://bucket/a.txt", dir, overwrite = NA, platform = gcp()),
        "isScalarLogical\\(overwrite\\)"
    )
})

test_that("avfiles_restore() passes 'overwrite' to avcopy()", {
    dir <- withr::local_tempdir()
    args <- NULL
    local_mocked_bindings(
        avstorage = function(...) "gs://bucket",
        avcopy = function(source, destination, ...) {
            args <<- list(source = source, destination = destination, ...)
            invisible(0L)
        }
    )
    avfiles_restore("a.txt", dir, namespace = "ns", name = "nm")
    expect_identical(args[["source"]], "gs://bucket/a.txt")
    expect_false(args[["overwrite"]])

    avfiles_restore(
        "a.txt", dir, namespace = "ns", name = "nm", overwrite = TRUE
    )
    expect_true(args[["overwrite"]])
})
