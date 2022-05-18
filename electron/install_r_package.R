


get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  }
  else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  }
  else if (.Platform$OS.type == "unix") {
    "unix"
  }
  else {
    stop("Unknown OS")
  }
}

cran_like_url <- paste0("https://cran.microsoft.com/snapshot/", Sys.Date())


os <- get_os()
app_root_path <- normalizePath(here::here("electron/FastMapping"), winslash = "/",
                               mustWork = FALSE)

baseUrl <- file.path(cran_like_url, "bin", "windows", "base")
readCran <- base::readLines(baseUrl, warn = FALSE)
filename <- base::regexpr("R-[0-9.]+.+-win\\.exe", readCran)
filename <- base::regmatches(readCran, filename)
if (base::regexpr("R-[0-9.]+.+-win\\.exe", filename)[[1]] != 
    1L) {
  stop("Was unable to resolve url of R.exe installer for Windows.")
}
win_url <- base::file.path(baseUrl, filename, fsep = "/")


installer_filename <- basename(win_url)
download_path <- base::file.path(tempdir(), installer_filename, 
                                 fsep = "/")
utils::download.file(url = d_url, 
                     destfile = download_path, 
                     mode = "wb")



install_r_to_path <- base::file.path(app_root_path, "app", 
                                     fsep = "/")


quoted_install_r_to_path <- base::shQuote(install_r_to_path)
quoted_win_installer_path <- base::shQuote(download_path)
base::system(glue::glue("{quoted_win_installer_path} /SILENT /DIR={quoted_install_r_to_path}"))

rlang_path <- base::file.path(install_r_to_path, "bin",
                              fsep = "/")

## Trim_r
r_lang_path <- file.path(app_root_path, "app", "r_lang", 
                         fsep = "/")
a <- list.files(r_lang_path, recursive = T, full.names = T)
pre <- sum(file.size(a))
temp <- base::list.files(path = r_lang_path, recursive = TRUE, 
                         pattern = ".html", full.names = TRUE)
removed <- base::file.remove(temp)
message("Removed: \n", base::paste0(temp[removed], collapse = "\n"))
temp <- base::list.files(r_lang_path, recursive = TRUE, pattern = ".pdf", 
                         full.names = TRUE)
removed <- base::file.remove(temp)
base::message("Removed: \n", base::paste0(temp[removed], 
                                          collapse = "\n"))
a <- list.files(r_lang_path, recursive = T, full.names = T)
post <- sum(file.size(a))
base::message("Trimmed ", pre - post, "bytes")


library_path <- base::file.path(app_root_path, "app",
                                "r_lang", "library", fsep = "/")


## Install User App

library_path = library_path
repo_location = "github"
# repo = PPaccioretti/FastMapping@pkg
repos = cran_like_url
package_install_opts = NULL


accepted_sites <- c("github", "gitlab", "bitbucket", "local")
if (is.null(library_path)) {
  stop("install_user_app() requires library_path to be set.")
}
if (!dir.exists(library_path)) {
  stop("install_user_app() library_path wasn't found.")
}
if (length(repo_location) != 1L) {
  stop(glue::glue("install_user_app(repo_location) must be character vector of length 1"))
}
if (!repo_location %in% accepted_sites) {
  stop(glue::glue("install_user_app(repo_location) must be one of: {accepted_sites}"))
}
if (!nchar(repo) > 0) {
  stop("install_user_app(repo) must be character with > 0 characters")
}
if (!is.null(package_install_opts)) {
  if (!is.list(package_install_opts)) {
    stop("package_install_opts  must be a list of arguments.")
  }
}
remotes_code <- as.character(glue::glue("install_{repo_location}"))
repo <- as.list(repo)
passthr <- c(repo, repos = repos, c(package_install_opts, 
                                    list(force = TRUE, lib = library_path)))
os <- electricShine::get_os()
if (identical(os, "win")) {
  rscript_path <- file.path(dirname(library_path), "bin", 
                            "Rscript.exe")
}
if (identical(os, "mac")) {
  rscript_path <- file.path(dirname(library_path), "bin", 
                            "R")
}

tmp_file <- tempfile()
save(list = c("remotes_code", "passthr"), file = tmp_file)
remotes_library <- copy_remotes_package()
copy_electricshine_package()
old_R_LIBS <- Sys.getenv("R_LIBS")
old_R_LIBS_USER <- Sys.getenv("R_LIBS_USER")
old_R_LIBS_SITE <- Sys.getenv("R_LIBS_SITE")
Sys.setenv(R_LIBS = library_path)
Sys.setenv(R_LIBS_USER = remotes_library)
Sys.setenv(R_LIBS_SITE = remotes_library)
Sys.setenv(ESHINE_PASSTHRUPATH = tmp_file)
Sys.setenv(ESHINE_remotes_code = remotes_code)
tmp_file2 <- tempfile()
file.create(tmp_file2)
Sys.setenv(ESHINE_package_return = tmp_file2)
message("Installing your Shiny package into electricShine framework.")
system_install_pkgs(rscript_path)
on.exit({
  Sys.setenv(R_LIBS = old_R_LIBS)
  Sys.setenv(R_LIBS_USER = old_R_LIBS_USER)
  Sys.setenv(R_LIBS_SITE = old_R_LIBS_SITE)
  Sys.setenv(ESHINE_PASSTHRUPATH = "")
  Sys.setenv(ESHINE_remotes_code = "")
})
message("Finshed: Installing your Shiny package into electricShine framework")
user_pkg <- readLines(tmp_file2)
return(user_pkg)


