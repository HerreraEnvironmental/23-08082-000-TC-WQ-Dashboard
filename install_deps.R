# install_deps.R

yaml_file <- "deps.yaml"
cran_repo <- "https://cloud.r-project.org"

needs <- function(pkg) !requireNamespace(pkg, quietly = TRUE)

if (needs("yaml"))    install.packages("yaml",    repos = cran_repo)
if (needs("remotes")) install.packages("remotes", repos = cran_repo)

deps <- yaml::read_yaml(yaml_file)

# Helpers ----
is_installed <- function(pkg, version = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
  if (is.null(version)) return(TRUE)
  as.character(utils::packageVersion(pkg)) == as.character(version)
}

install_cran <- function(x) {
  if (length(x) == 0) return(invisible())
  for (ref in x) {
    parts <- strsplit(ref, "@", fixed = TRUE)[[1]]
    pkg <- parts[1]
    version <- if (length(parts) == 2) parts[2] else NULL
    if (is_installed(pkg, version)) next
    if (is.null(version)) {
      install.packages(pkg, repos = cran_repo)
    } else {
      remotes::install_version(pkg, version = version, repos = cran_repo)
    }
  }
}

install_github <- function(x) {
  if (length(x) == 0) return(invisible())
  for (ref in x) {
    parts <- strsplit(ref, "@", fixed = TRUE)[[1]]
    repo <- parts[1]              # "owner/repo"
    git_ref <- if (length(parts) == 2) parts[2] else NULL
    pkg <- sub(".*/", "", repo)   # extract "repo" as pkg name
    if (is_installed(pkg)) next
    if (is.null(git_ref)) {
      remotes::install_github(repo, upgrade = "never")
    } else {
      remotes::install_github(repo, ref = git_ref, upgrade = "never")
    }
  }
}

# Run installers ----
install_cran(deps$cran %||% character())
install_github(deps$github %||% character())

message("Dependencies installed.")
