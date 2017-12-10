#' A function to update R without losing packages, intended for Macs.
#'
#' @examples
#' updateR()
#'
#' @export
updateR <- function() {
  cat("You need a working directory to save and load all your packages. \n")
  # 1 if yes, 2 if not
  useCur <- menu(c("Yes", "No"), title = cat("Is this okay?", getwd(), "\n"))

  # Check if want to use current working directory, prompt for new if not.
  if (useCur == 1) {
    fp <- getwd()
  } else {
    fp <- readline("Enter the working directory to store or load your packages.")
  }
  # Change working directory.
  setwd(fp)

  # Check if they already have done Step 1: Saving the packages. Save if not.
  saved <- menu(c("Yes", "No"), title = cat("Have you already saved your packages using this function?"))
  if (saved == 2) {
    tmp <- installed.packages()
    installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
    save(installedpkgs, file = "installed_old.rda")
  } else if (saved == 1) {
    # Check that they have updated R, prompt to if not.
    updated <- menu(c("Yes", "No"), title = cat("After saving, have you closed R and installed the newest version?"))
    if (updated == 2) {
      cat("If you are here, close R, then download the latest R version from www.r-project.org \n")
    } else if (updated == 1) {
      # Check that the current version is what they think, prompt to fix if not.
      cat("This is your current version \n", R.Version()$version.string, "\n")
      checkUpdate <-  menu(c("Yes", "No"), title = cat("Is this correct?"))
      if (checkUpdate ==  1) {
        load("installed_old.rda")
        tmp <- installed.packages()
        installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
        missing <- setdiff(installedpkgs, installedpkgs.new)
        install.packages(missing)
        update.packages(ask = FALSE)
      } else {
        cat("Something went wrong and you are not using the updated version of R. Please redownload and try again. \n")
      }
    }
  }
}
