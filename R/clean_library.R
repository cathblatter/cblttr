#' clean library - a function to
#' set a clean library state - to use with caution
#'
#' @param lib defaults to .libPaths()
#'
#' @return a value
#' @export
clean_library <- function(lib = .libPaths()){

    # create a list of all installed packages
    ip <- base::as.data.frame(utils::installed.packages(lib.loc = lib))

    # if you use MRO, make sure that no packages in this library will be removed
    ip <- base::subset(ip, !base::grepl("MRO", ip$LibPath))

    # we don't want to remove base or recommended packages either\
    ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")), ]

    # determine the library where the packages are installed
    path.lib <- base::unique(ip$LibPath)

    # create a vector with all the names of the packages you want to remove
    pkgs.to.remove <- ip[,1]

    # remove the packages
    base::sapply(pkgs.to.remove, utils::remove.packages, lib = path.lib)

  }
