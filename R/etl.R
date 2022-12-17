#' @export
#' This function extracts latest zipfile for user data
extract_zip <- function( ) {
  usethis::edit_r_environ("project")
  #Citation: https://stackoverflow.com/questions/23899525/using-r-to-download-zipped-data-file-extract-and-import-csv
  download.file(Sys.getenv("zip_path"),"data/latest.zip", mode="wb")
  system("unzip -d data/ data/latest.zip -y")
}
