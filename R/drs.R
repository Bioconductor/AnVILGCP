.DRS_HUB <- "https://drshub.dsde-prod.broadinstitute.org"

.DRS_HUB_TEMPLATE <- list(
    drs = character(),
    fileName = character(),
    size = integer(),
    accessUrl = character(),
    timeUpdated = character(),
    timeCreated = character(),
    bucket = character(),
    name = character()
)

.DRS_NCI_CRDC_TEMPLATE <- list(
    id = character(),
    drs = character(),
    size = integer(),
    created_time = character(),
    access_url = character(),
    updated_time = character(),
    name = character()
)

.NCI_CRDC <- "https://nci-crdc.datacommons.io"

.drs_nci_crdc_service <- function(drs_url, service_url) {
    request_url <- paste(service_url, basename(drs_url), sep = "/")
    headers <- add_headers(
        "content-type" = "application/json"
    )
    response <- httr::GET(request_url, headers, encode = "raw")
    avstop_for_status(response, "DRS NCI CRDC resolution")
    lst <- c(as.list(response), list(drs = drs_url))

    resframe <- lst$access_methods
    lst$access_url <- unlist(
        unname(resframe[resframe$access_id == "https", "access_url"])
    )
    resframe <- lst$checksums
    lst$md5_checksums <- unlist(
        resframe[resframe$type == "md5", "checksum"]
    )
    lst <- lst[!names(lst) %in% c("access_methods", "checksums", "aliases")]

    is_list <-
        vapply(lst, is.list, logical(1L))
    lst[is_list] <- lapply(lst[is_list], list)

    as_tibble(lst[lengths(lst) == 1L])
}

#' @importFrom httr POST add_headers
.drs_hub_service <-
    function(drs_url, service_url, fields, token, user_project = NULL)
{
    headers <- add_headers(
        Authorization = paste("Bearer", token),
        "content-type" = "application/json"
    )
    body <- list(fields = fields, url = jsonlite::unbox(drs_url))
    ## requester pays: Google project billed for access to the signed URL;
    ## only required by TDR snapshots with 'requireUserProject' enabled
    op <- "DRS HUB resolution"
    if (!is.null(user_project) && nzchar(user_project))
        body[["userProject"]] <- jsonlite::unbox(user_project)
    else
        op <- paste0(
            op, " (no 'user_project' set; data requiring a Google ",
            "project for requester pays will fail)"
        )
    body_json <- jsonlite::toJSON(body)
    response <- POST(service_url, headers, body = body_json, encode="raw")
    avstop_for_status(response, op)

    ## add drs field to response
    lst <- c(as.list(response), list(drs = drs_url))

    ## unbox accessUrl; if accessUrl == NULL, then this is a no-op
    lst$accessUrl <- unlist(lst$accessUrl, use.names = FALSE)

    ## nest list elements so length == 1L
    is_list <-
        vapply(lst, is.list, logical(1))
    lst[is_list] <- lapply(lst[is_list], list)

    as_tibble(lst[lengths(lst) == 1L])
}

.DRS_REQ_FIELDS <- c(
    "bucket", "name", "size", "timeCreated",
    "timeUpdated", "fileName", "accessUrl"
)

.DRS_NCI_REQ_FIELDS <- c(
    "id", "name", "size", "created_time",
    "updated_time", "access_url", "checksums"
)

#' @name drs
#'
#' @title DRS (Data Repository Service) URL management
#'
#' @description `drs_hub()` resolves zero or more DRS URLs to their Google
#'   bucket location using the DRS Hub API endpoint.
#'
#' @section drs_hub:
#' `drs_hub()` uses the DRS Hub API endpoint to resolve a single or multiple DRS
#' URLs to their Google bucket location. The DRS Hub API endpoint requires a
#' `gcloud_access_token()`. The DRS Hub API service is hosted at
#' <https://drshub.dsde-prod.broadinstitute.org>.
#'
#' @param source `character()` DRS URLs (beginning with 'drs://') to resources
#'   managed by the DRS Hub server (`drs_hub()`).
#'
#' @param user_project `character(1)` Google project billed for access to
#'   requester-pays data, sent to DRS Hub as `userProject`. Resolution fails
#'   without it for AnVIL TDR snapshots with `requireUserProject` enabled;
#'   other DRS objects resolve without it. Defaults to the `GOOGLE_PROJECT`
#'   environment variable (set in Terra workspaces) or, failing that,
#'   `gcloud_project()`; when neither is available, `userProject` is omitted
#'   from the request.
#'
#'
#' @return `drs_hub()` returns a tbl with the following columns:
#'
#' - `drs`: `character()` DRS URIs
#' - `bucket`: `character()` Google cloud bucket
#' - `name`: `character()` object name in `bucket`
#' - `size`: `numeric()` object size in bytes
#' - `timeCreated`: `character()` object creation time
#' - `timeUpdated`: `character()` object update time
#' - `fileName`: `character()` local file name
#' - `accessUrl`: `character()` signed URL for object access
#'
#' @section drs_nci_crdc: `drs_nci_crdc()` resolves one or more DRS URLs to
#'   their <gdc.cancer.gov> location. The implementation allows the extraction
#'   of `access_url` values to download the DRS objects. The DRS NCI CRDC
#'   service is hosted at <https://nci-crdc.datacommons.io>.
#'
#' @examples
#' if (GCPtools::gcloud_exists() && interactive()) {
#'     drs_urls <- c(
#'         "drs://drs.anv0:v2_b3b815c7-b012-37b8-9866-1cb44b597924",
#'         "drs://drs.anv0:v2_2823eac3-77ae-35e4-b674-13dfab629dc5",
#'         "drs://drs.anv0:v2_c6077800-4562-30e3-a0ff-aa03a7e0e24f"
#'     )
#'     drs_hub(drs_urls)
#'
#'     drs_nci <- c(
#'         "drs://nci-crdc.datacommons.io/56e35487-b20f-45ba-8d84-9f16b26c85ea",
#'         "drs://nci-crdc.datacommons.io/f814f1ec-6850-4ab6-ac0f-df9f77ee185b",
#'         "drs://nci-crdc.datacommons.io/d9b591d5-7fe8-43fe-b0b3-4fc0f9736866"
#'     )
#'     drs_nci_crdc(drs_nci)
#' }
#' @export
drs_hub <- function(source = character(), user_project = .drs_user_project()) {
    stopifnot(
        is.character(user_project), length(user_project) == 1L,
        !is.na(user_project)
    )
    access_token <- GCPtools::gcloud_access_token("drs")
    Map(
        .drs_hub_service,
        source,
        MoreArgs = list(
            service_url = .hub_service_url(),
            fields = .DRS_REQ_FIELDS,
            token = access_token,
            user_project = user_project
        )
    ) |>
        do.call(rbind.data.frame, args = _) |>
        .tbl_with_template(.DRS_HUB_TEMPLATE)
}

## Google project billed for requester-pays DRS access; on Terra this is
## the workspace's GOOGLE_PROJECT, otherwise fall back to gcloud's project
.drs_user_project <- function() {
    project <- Sys.getenv("GOOGLE_PROJECT")
    if (!nzchar(project))
        project <- tryCatch(
            GCPtools::gcloud_project(), error = function(e) ""
        )
    project
}

.hub_service_url <- function(hub = .DRS_HUB) {
    paste0(hub, "/api/v4/drs/resolve")
}

.nci_crdc_service_url <- function(hub = .NCI_CRDC) {
    paste0(hub, "/ga4gh/drs/v1/objects")
}

#' @rdname drs
#'
#' @export
drs_nci_crdc <- function(source = character()) {
    Map(
        .drs_nci_crdc_service,
        source,
        MoreArgs = list(
            service_url = .nci_crdc_service_url()
        )
    ) |> do.call(rbind.data.frame, args = _) |>
        .tbl_with_template(.DRS_NCI_CRDC_TEMPLATE)
}
