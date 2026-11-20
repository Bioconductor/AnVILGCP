context("av")

test_that(".avtable_import_set_entity() works", {
    .data <- data.frame(x=1:3, y=1:3, z_id=1:3)

    expect_identical(
        names(.avtable_import_set_entity(.data, "x")),
        c("entity:x_id", "y", "z_id")
    )

    expect_identical(
        names(.avtable_import_set_entity(.data, "y")),
        c("entity:y_id", "x", "z_id")
    )

    expect_identical(
        names(.avtable_import_set_entity(.data, "z_id")),
        c("entity:z_id", "x", "y")
    )

    ## construct ids from unknown column
    observed <- .avtable_import_set_entity(.data, "w")
    expect_identical(names(observed), c("entity:w_id", "x", "y", "z_id"))
    expect_identical(observed[["entity:w_id"]], 1:3)

    observed <- .avtable_import_set_entity(.data, "w_id")
    expect_identical(names(observed), c("entity:w_id", "x", "y", "z_id"))
    expect_identical(observed[["entity:w_id"]], 1:3)

    ## errors: duplicate or NA entities
    .data <- data.frame(x=c(1, 1), y=1)
    expect_error(
        .avtable_import_set_entity(.data, "x"),
        "!anyDuplicated\\(.data\\[\\[entity\\]\\]\\) is not TRUE"
    )

    .data <- data.frame(x=c(1, NA, 2), y=1)
    expect_error(
        .avtable_import_set_entity(.data, "x"),
        "!anyNA\\(.data\\[\\[entity\\]\\]\\) is not TRUE"
    )
})

test_that(".avtable_import_write_dataset() works with NA", {
    tbl0 <- tibble(
        issue75 = letters[1:5],
        x = c("a", "", NA, "NA", "d"),
        i = c(1:3, NA, 4L),
        r = as.numeric(i)
    )

    na <- ""
    destination <- .avtable_import_write_dataset(tbl0, na)
    result <- read.delim(destination, na.strings = na, sep = "\t")
    expect_identical(result$x, c("a", NA, NA, "NA", "d"))

    na <- NA_character_
    destination <- .avtable_import_write_dataset(tbl0, na)
    result <- read.delim(destination, na.strings = na, sep = "\t")
    expect_identical(result$x, c("a", "", NA, NA, "d"))

    na <- "__MISSING__"
    destination <- .avtable_import_write_dataset(tbl0, na)
    result <- read.delim(destination, na.strings = na, sep = "\t")
    expect_identical(result$x, tbl0$x)
})

test_that(".avtable_import_list_columns() encodes JSON arrays", {
    tbl0 <- tibble(
        tmp_id = 1:4,
        chr = list(c("A", "B", "C"), "D", character(), NULL),
        int = list(1:2, 3L, integer(), 4L),
        dbl = list(c(1.5, 2), 1/3, 0, 1e10),
        lgl = list(TRUE, c(TRUE, FALSE), logical(), FALSE),
        fct = list(factor(c("x", "y")), factor("z"), NULL, factor("x")),
        named = list(c(a = "A"), "B", "C", "D"),
        scalar = letters[1:4]
    )
    result <- .avtable_import_list_columns(tbl0)

    expect_identical(names(result), names(tbl0))
    expect_identical(result[["tmp_id"]], tbl0[["tmp_id"]])
    expect_identical(result[["scalar"]], tbl0[["scalar"]])
    expect_identical(result[["chr"]], c('["A","B","C"]', '["D"]', "[]", "[]"))
    expect_identical(result[["int"]], c("[1,2]", "[3]", "[]", "[4]"))
    expect_identical(
        result[["dbl"]],
        c("[1.5,2]", "[0.333333333333333]", "[0]", "[10000000000]")
    )
    expect_identical(
        result[["lgl"]], c("[true]", "[true,false]", "[]", "[false]")
    )
    expect_identical(result[["fct"]], c('["x","y"]', '["z"]', "[]", '["x"]'))
    expect_identical(result[["named"]], c('["A"]', '["B"]', '["C"]', '["D"]'))

    ## list column round-trips through JSON
    expect_identical(
        lapply(result[["chr"]], jsonlite::fromJSON),
        list(c("A", "B", "C"), "D", list(), list())
    )
})

test_that(".avtable_import_list_columns() is a no-op without list columns", {
    tbl0 <- tibble(x = letters[1:3], y = 1:3)
    expect_identical(.avtable_import_list_columns(tbl0), tbl0)
})

test_that(".avtable_import_list_columns() rejects NA and nested values", {
    tbl0 <- tibble(id = 1:2, x = list("A", c("B", NA)))
    expect_error(
        .avtable_import_list_columns(tbl0),
        "list column 'x' row 2 contains NA"
    )
    tbl0 <- tibble(id = 1:2, x = list("A", list("B", "C")))
    expect_error(
        .avtable_import_list_columns(tbl0),
        "list column 'x' row 2 must contain an atomic vector"
    )
    tbl0 <- tibble(id = 1, x = list(matrix(1:4, 2)))
    expect_error(
        .avtable_import_list_columns(tbl0),
        "list column 'x' row 1 must contain an atomic vector"
    )
})

test_that(".avtable_import_write_dataset() writes encoded list columns", {
    tbl0 <- tibble(
        `entity:tmp_id` = 1:2,
        cohorts = list(c("A", "B", "C"), "D"),
        x = c("a", NA)
    )
    tbl <- .avtable_import_list_columns(tbl0)
    destination <- .avtable_import_write_dataset(tbl, "NA")
    lines <- readLines(destination)
    expect_identical(
        lines,
        c(
            "entity:tmp_id\tcohorts\tx",
            "1\t[\"A\",\"B\",\"C\"]\ta",
            "2\t[\"D\"]\tNA"
        )
    )
})

test_that("avtable_nest() collapses avtable() list attributes", {
    ## shape returned by avtable() for value and entity reference lists
    tbl <- tibble(
        sample_id = c("r1", "r2", "r3"),
        scalar = c("x", "y", "z"),
        chr.itemsType = rep("AttributeValue", 3L),
        chr.items = list(c("A", "B", "C"), "D", character()),
        int.itemsType = rep("AttributeValue", 3L),
        int.items = list(1:2, 3L, 4L)
    )
    result <- avtable_nest(tbl)
    expect_identical(names(result), c("sample_id", "scalar", "chr", "int"))
    expect_identical(result[["scalar"]], tbl[["scalar"]])
    expect_identical(
        result[["chr"]], list(c("A", "B", "C"), "D", character())
    )
    expect_identical(result[["int"]], list(1:2, 3L, 4L))

    set <- tibble(
        sample_set_id = c("g1", "g2"),
        samples.itemsType = rep("EntityReference", 2L),
        samples.items = list(
            data.frame(entityType = "sample", entityName = c("r1", "r2")),
            data.frame(entityType = "sample", entityName = "r3")
        )
    )
    result <- avtable_nest(set)
    expect_identical(names(result), c("sample_set_id", "samples"))
    expect_identical(result[["samples"]], list(c("r1", "r2"), "r3"))
})

test_that("avtable_nest() is a no-op on an already nested table", {
    tbl <- tibble(
        sample_id = c("r1", "r2"),
        chr.itemsType = rep("AttributeValue", 2L),
        chr.items = list(c("A", "B"), character()),
        samples.itemsType = rep("EntityReference", 2L),
        samples.items = list(
            data.frame(entityType = "sample", entityName = c("r1", "r2")),
            data.frame(entityType = "sample", entityName = "r3")
        )
    )
    nested <- avtable_nest(tbl)
    expect_identical(avtable_nest(nested), nested)
})

test_that("avtable_nest() collapses avtable_paged() list attributes", {
    tbl <- tibble(sample_id = c("r1", "r2"), scalar = c("x", "y"))
    tbl[["chr"]] <- data.frame(itemsType = rep("AttributeValue", 2L))
    tbl[["chr"]][["items"]] <- list(c("A", "B"), character())
    result <- avtable_nest(tbl)
    expect_identical(names(result), c("sample_id", "scalar", "chr"))
    expect_identical(result[["chr"]], list(c("A", "B"), character()))
})

test_that("avtable_nest() leaves other tables unchanged", {
    tbl <- tibble(x = letters[1:3], y = 1:3)
    expect_identical(avtable_nest(tbl), tbl)
    ## '.items' without matching '.itemsType' is not a list attribute
    tbl <- tibble(x = letters[1:2], y.items = list(1, 2))
    expect_identical(avtable_nest(tbl), tbl)
    ## don't overwrite an existing column with the same name
    tbl <- tibble(
        x = 1:2, y = 3:4,
        y.itemsType = "AttributeValue", y.items = list(1, 2)
    )
    expect_identical(avtable_nest(tbl), tbl)
})

test_that(".avtable_na() works", {

    ## .avtable_na() acts when flatten() has converted "" to NA_character_
    tbl0 <- tibble(
        issue75 = letters[1:4],
        x = c("a", NA, "NA", "d"),
        i = c(1:2, NA, 3L),
        r = as.numeric(i)
    )

    tbl1 <- dplyr::mutate(tbl0, across(where(is.character), .avtable_na("")))
    expect_identical(tbl1$x, c("a", NA, "NA", "d"))

    tbl1 <- dplyr::mutate(tbl0, across(where(is.character), .avtable_na("NA")))
    expect_identical(tbl1$x, c("a", "", NA, "d"))

    tbl1 <- dplyr::mutate(
        tbl0, across(where(is.character), .avtable_na(c("", "NA")))
    )
    expect_identical(tbl1$x, c("a", NA, NA, "d"))

})

test_that(".avbucket_path() trims arguments correctly", {
    expect_error(
        .avbucket_path(),
        'argument "bucket" is missing, with no default'
    )
    expect_error(
        .avbucket_path('foo'),
        'gsutil_is_uri\\(bucket\\) is not TRUE'
    )
    expect_identical("gs://foo", .avbucket_path("gs://foo"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo", "bar"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo", "/bar"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo", "bar/"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo", "/bar/"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo/", "/bar"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo/", "bar/"))
    expect_identical("gs://foo/bar", .avbucket_path("gs://foo/", "/bar/"))
    expect_identical(
        paste0("gs://foo/", c("bar", "baz")),
        .avbucket_path("gs://foo", c("bar", "baz"))
    )
    expect_identical(
        "gs://foo/bar/baz",
        .avbucket_path("gs://foo", "bar", "baz")
    )
    expect_identical(
        paste0("gs://foo/bar/", c("baz", "bing")),
        .avbucket_path("gs://foo", "bar", c("baz", "bing"))
    )
})

