#' Set up processing folders
#'
#' @return creates data and dataout folder
#' @export
#'
#' @examples
#' \dontrun{
#' #standard
#'   dir_setup()
#' }

dir_setup <- function() {

  folders <- list("data-raw", "data-raw/NDOH", "data-raw/Reference Files")
  data_files <- c("NDOH File", "Disaggregate Mapping File")

  #if (!base::dir.exists(file.path(".", folders)))
    glamr::folder_setup(folders)
   #glamr::setup_gitignore()

    print(glue::glue_col("{yellow Please save the following files to the data-raw/NDOH folder:
                         } Latest NDOH File
                         {yellow Please save the following files to the data-raw/Reference Files folder:
                         } Completed Disaggregate Mapping Reference File"))



}


