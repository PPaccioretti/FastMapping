electricShine:::create_build_directory()
electricShine::electrify()


electricShine::run_build_release


electron_build_resources <- system.file("extdata", "icon", 
                                        package = my_package_name, lib.loc = library_path)
if (nchar(electron_build_resources) == 0) {
  electron_build_resources <- base::list.files(electron_build_resources, 
                                               full.names = TRUE)
  resources <- base::file.path(app_root_path, "resources")
  base::dir.create(resources)
  base::file.copy(from = electron_build_resources, to = resources)
}




electricShine::trim_r


electricShine:::.find_win_exe_url


electricShine:::.install_win_r(build_path = build_path)
app_root_path <- file.path(build_path, app_name)
electricShine::create_folder(app_root_path)
electricShine::copy_template(app_root_path)



if (identical(os, "win")) {
  library_path <- base::file.path(app_root_path, "app",
                                  "r_lang", "library", fsep = "/")
}
if (identical(os, "mac")) {
  library_path <-
    file.path(app_root_path,
              "app/r_lang/Library/Frameworks/R.framework/Versions")
  library_path <- list.dirs(library_path, recursive = FALSE)
  library_path <-
    library_path[grep("\\d+\\.(?:\\d+|x)(?:\\.\\d+|x){0,1}",
                      library_path)][[1]]
  library_path <- file.path(library_path, "Resources/library",
                            fsep = "/")
}
if (!base::is.null(git_host)) {
  my_package_name <-
    electricShine::install_user_app(
      library_path = library_path,
      repo_location = git_host,
      repo = git_repo,
      repos = cran_like_url,
      package_install_opts = package_install_opts
    )
}
if (!is.null(local_package_path)) {
  my_package_name <-
    electricShine::install_user_app(
      library_path = library_path,
      repo_location = "local",
      repo = local_package_path,
      repos = cran_like_url,
      package_install_opts = package_install_opts
    )
}