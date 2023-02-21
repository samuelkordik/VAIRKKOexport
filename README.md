
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vairkkoexport

<!-- badges: start -->

[![R-CMD-check](https://github.com/samuelkordik/VAIRKKOexport/workflows/R-CMD-check/badge.svg)](https://github.com/samuelkordik/VAIRKKOexport/actions)
<!-- badges: end -->

The goal of vairkkoexport is to simplify extracting structured
information from VAIRKKO. This exported information can then be saved
for archival purposes or imported to other systems. At this time, only
exporting behavioral incidents is supported.

## Installation

You can install vairkkoexport from [GitHub](https://github.com) with:

``` r
install_github("samuelkordik/vairkkoexport")
```

## Authentication with VAIRKKO

Using this package requires valid login credentials to VAIRKKO with
access to the specific area you are exporting data from.

> ***NOTE:*** Use of this package is at your own risk. Only use this
> package with the approval of the VAIRKKO client organization.

Authentication is handled on package load, and credentials are securely
stored in the OS credential store using
[keyring](https://github.com/r-lib/keyring).

## Exporting Incidents Information

Due to the time involved in exporting large amounts of incident
information, the following is recommended for exporting incidents

``` r
library(vairkkoexport)
incidents_path <- here::here("data/incidents_export.rds")

if (fs::file_exists(incidents_path)) {
  incidents <- read_rds(incidents, incidents_path)
} else {
  incidents <- export_behavioral_incidents()
  write_rds(incidents, here::here("data/incidents_export.rds"))
}
```

The Incidents tibble that is returned contains 35 variables, some of
which may be redundant. There is one observation per action, or for
behavioral incidents with no action, one observation per incident.
Variables prefixed “detail\_” come from the incident detail page.
Variables prefixed “action\_” are from the action tab of the incident
detail page. The variable “detail_files” is a list column of tibbles
containing file information, including the file download url.

## Downloading Incident Files

Any attached files can be downloaded using the `download_incident_files`
function.

``` r
download_incident_files(incidents, here::here("downloads"))
```

## Downloading original incidents

VAIRKKO offers a print function that displays the details of each
incident. These can be downloaded as HTML files using the
`download_all_incident_prints` function.

``` r
download_all_incident_prints(here::here("incident_archive"))
```

## Exporting iForms

Use the function`download_iforms` with a path argument to download all
iForms, including attachments. Start and end dates are added to
determine range. The path argument specifies where the export will be
stored, and a directory is specifically created with the date and time
of export to organize. additional options allow finetuning format of
directory string and filename string. When errors are encountered, the
overall CSV table will indicate a donwload error for the iForm in
question.

Note: This will currently only access ALL open iForms the user has
access to.

## Downloading memos

Use the function `download_memos` with a path argument to download all
memos into a CSV file.

## Fixing CSV file paths for attachments

Most of these export functions will have CSV files with a local path to
downloaded files. To make these portable, use `add_relative_paths()`,
which searcehs all CSV files in project directory with path columns,
then add a column with a relative path and saves out as a new file.
After confirming results, you can delete the original file.
