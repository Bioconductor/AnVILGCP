## Changes in version 1.8.0

### New features

* `avworkflow_jobs()` includes `methodConfigurationNamespace` and
  `methodConfigurationName` columns identifying the workflow submitted;
  the new columns follow the existing ones (#4, @amstilp)
* `avtable_import()` supports list columns, e.g., `tibble(id = 1, cohorts =
  list(c("A", "B")))`; elements are uploaded as JSON arrays, which AnVIL
  imports as list attributes (#5, @smgogarten)
* New `avtable_nest()` reshapes list attributes returned by `avtable()`
  (`<name>.items` and `<name>.itemsType` columns) or `avtable_paged()` into
  list columns named as in the workspace DATA tab; `avtable()` output is
  unchanged

### Bug fixes and minor improvements

* Remove internal `gcloud` SDK helpers and the `print.gcloud_sdk_result()`
  method duplicated from `GCPtools`, avoiding the 'Registered S3 method
  overwritten' message when both packages are loaded.
* Workspace write operations (`avtable_import()`, `avtable_import_set()`,
  `avtable_delete()`, `avtable_delete_values()`, `avdata_import()`,
  `avworkflow_run()`, `avworkflow_stop()`, and
  `avworkflow_configuration_set()`) check the user's workspace access level
  and report an informative error instead of e.g., a generic HTTP 500 error
  when write (or compute) permission is lacking (#10).
* Use `all_of()` in `avtable_import_set()` to avoid a `tidyselect`
  deprecation warning.
* `avtable_import_status()` warnings explain that Terra reports
  re-importing values identical to the existing table as an `"Error"`
  ('All entities failed to update. There were 0 errors in total.';
  #8, @smgogarten)
* `avtable_import_status()` no longer re-queries jobs with status `"Error"`

## Changes in version 1.6.0

### Deprecated and defunct

* Move `gcloud` functions to `DEFUNCT` status as they have been fully migrated
  to `GCPtools`

### Bug fixes and minor improvements

* Use `GCPtools::gcloud_exists` for internal checks
* Use `with_mocked_bindings` in tests

## Changes in version 1.4.0

### Significant user-visible changes

* Deprecate `gcloud` and `gsutil` utility functions in favor of `GCPtools`
  package
* Translate `nci-crdc` URIs to URLs in `drs_nci_crdc`

### New features

* Add `drs_nci_crdc` documentation and support in `drs_hub`

### Bug fixes and minor improvements

* Add `interactive()` condition to examples to avoid execution in
  non-interactive sessions
* Use exported functions from `GCPtools` directly
* Add `GCPtools` dependency
* Resolve `NOTE` from using unqualified `GET`
* Fix package name and other typos
* Remove `test_gcloud_sdk.R`

## Changes in version 1.2.0

### New features

* Remove defunct `drs_stat`, `drs_access_url`, and `drs_cp` functions
* Defunct `bucket_location` and `bucket` arguments

### Bug fixes and minor improvements

* Filter out "prerequisites" element from response (#103)
* Clean up unused `authenticate_*` code
* Remove `httr2` from `Suggests`

## Changes in version 1.0.0

* Initial Bioconductor submission.
