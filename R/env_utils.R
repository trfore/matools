# env_utils.R

utils::globalVariables(c("matools_env"))

#' @title Create An New 'matools' Environment To Store Analysis Parameters
#'
#' @description Provides a location to store package objects and variables,
#'   allowing for easy removal of data analysis parameters within an analysis
#'   pipeline. Note: sets parent environment to \code{env: empty} to avoid scope
#'   escape.
#'
#' @param force_new Boolean, when TRUE a new matools_env will be created,
#'   overwriting all existing parameters. Default is FALSE.
#'
#' @return A new [environment()], \code{matools_env}, in the `.GlobalEnv` namespace.
#' @seealso [reset_pkg_environment()]
#'
#' @examples
#' library(matools)
#'
#' set_pkg_environment(force_new = TRUE)
#' print(matools_env)
#' rm(matools_env, envir = .GlobalEnv)
#'
#' @importFrom utils menu
#' @export
#' @md
set_pkg_environment <- function(force_new = FALSE) {
  if (exists("matools_env") == FALSE || force_new == TRUE) {
    assign("matools_env", new.env(), envir = .GlobalEnv)
  } else {
    reset_env <-
      utils::menu(c("Yes", "No"),
        title = cat(
          "Warning: Package environment 'matools_env' already",
          "exist, would you like to reset it?"
        )
      )

    if (reset_env == 1) {
      assign("matools_env", new.env(), envir = .GlobalEnv)
    } else if (reset_env == 2) {
      warning("Previous matools_env inherited, prior values remain.",
        immediate. = TRUE
      )
    } else {
      stop("User exited selection menu, matools_env exists and is unchanged.")
    }
  }
}

#' @title Reset 'matools' Environment
#'
#' @description
#' This is a wrapper for \code{set_pkg_environment}
#'
#' @return A new [environment()], \code{matools_env}, in the `.GlobalEnv` namespace.
#'
#' @seealso [set_pkg_environment()]
#'
#' @examples
#' library(matools)
#'
#' # create an environment and add objects ----
#' set_pkg_environment(force_new = TRUE)
#' matools_env$parameter_1 <- TRUE
#' matools_env$parameter_2 <- FALSE
#' ls(matools_env)
#'
#' # reset the environment ----
#' reset_pkg_environment()
#' rm(matools_env, envir = .GlobalEnv)
#'
#' @export
#' @md
reset_pkg_environment <- function() {
  set_pkg_environment(force_new = TRUE)
}
