source("src/simulation/deps.R")
source("src/config.R")

if (download_input_data) {
    file_md5 <- "becab4b9f95f3bd02ccff5f9cbc1bc5d"
    message("Downloading input data")
    download.file("https://zenodo.org/records/10300363/files/BS_EC_14_DAYS_raw.rds?download=1",
                  destfile = raw_input_rds_fn)
    message("Verifying downloaded data")
    if (!identical(unname(tools::md5sum(raw_input_rds_fn)), file_md5)) {
        stop("Downloaded file verification failed, please rety.")
    }

    # Show a message file size of the downloaded file
    message("File size of downloaded file: ", 
    round(file.size(raw_input_rds_fn)/1024^2, 2), " MB")
}
