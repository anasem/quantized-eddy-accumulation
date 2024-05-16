source("src/simulation/deps.R")
source("src/config/default.R")


if (download_input_data) {

    options(timeout = max(download_timeout, getOption("timeout")))

    # Download and verify each file in the list
    for (file_name in names(remote_files)) {
        destfile <- file.path(input_data_dir, file_name)
        file_info <- remote_files[[file_name]]
        # Check if files exist, check MD5 checksums, and download if necessary
        if (file.exists(destfile)) {
            message("File ", file_name, " already exists.")
            if (identical(unname(tools::md5sum(destfile)), file_info$md5)) {
                message("MD5 checksum matches. Skipping ", file_name)
                next
            } else {
                message("MD5 checksum does not match for ", file_name, 
                        ". Redownloading.")
            }
        }
        
        # Download the file
        download.file(file_info$url, destfile = destfile)
        
        # Verify the file's MD5 checksum
        message("Verifying downloaded data for ", file_name)
        if (!identical(unname(tools::md5sum(destfile)), file_info$md5)) {
            stop("Downloaded file verification failed for ", file_name, 
                 ", please retry.")
        }
        
        # Display the size of the downloaded file
        message("File size of ", file_name, ": ", 
                round(file.size(destfile)/1024^2, 2), " MB")
    }
}


