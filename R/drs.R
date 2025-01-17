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

#' @importFrom httr2 request req_template req_headers req_auth_bearer_token
#'   req_body_json req_method req_perform resp_body_json
.drs_res_service <- function(drs_url, fields, token) {
    response <- request(.DRS_HUB) |>
        req_template("/api/v4/drs/resolve") |>
        req_headers(
            `Content-Type` = "application/json"
        ) |>
        req_auth_bearer_token(token) |>
        req_body_json(
            list(
                fields = fields,
                url = jsonlite::unbox(drs_url)
            )
        ) |>
        req_method("POST") |>
        req_perform() |>
        resp_body_json()

    ## add drs field to response
    lst <- c(response, list(drs = drs_url))

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
#' @examples
#' if (gcloud_exists() && interactive()) {
#'     drs_urls <- c(
#'         "drs://drs.anv0:v2_b3b815c7-b012-37b8-9866-1cb44b597924",
#'         "drs://drs.anv0:v2_2823eac3-77ae-35e4-b674-13dfab629dc5",
#'         "drs://drs.anv0:v2_c6077800-4562-30e3-a0ff-aa03a7e0e24f"
#'     )
#'     drs_hub(drs_urls)
#' }
#' @export
drs_hub <- function(source = character()) {
    access_token <- gcloud_access_token("drs")

    Map(
        .drs_res_service,
        source,
        MoreArgs = list(
            fields = .DRS_REQ_FIELDS,
            token = access_token
        )
    ) |>
        do.call(rbind.data.frame, args = _) |>
        .tbl_with_template(.DRS_HUB_TEMPLATE)
}
