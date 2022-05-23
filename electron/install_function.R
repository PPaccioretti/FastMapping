
install_r_and_package <-
  function(app_name = NULL,
            # product_name = "product_name",
            # short_description = NULL,
            # semantic_version = NULL,
            build_path = NULL,
            mran_date = NULL,
            cran_like_url = NULL,
            # function_name = NULL,
            git_host = NULL,
            git_repo = NULL,
            local_package_path = NULL,
            package_install_opts = NULL,
            # run_build = TRUE,
            # nodejs_path = file.path(system.file(package = "electricShine"),
            #                         "nodejs"),
            # nodejs_version = "v12.16.2",
            permission = TRUE,
            mac_url = "https://mac.r-project.org/el-capitan/R-3.6-branch/R-3.6-branch-el-capitan-sa-x86_64.tar.gz") {
    
    if (is.null(app_name)) {
      stop("electricShine::electrify() requires you to provide an 'app_name' argument specifying\n         the shiny app/package name.")
    }
    
    if (!is.null(package_install_opts)) {
      if (!is.list(package_install_opts)) {
        stop("package_install_opts in electrify() must be a list of arguments.")
      }
    }
    app_root_path <- file.path(build_path, app_name)
    if (!isTRUE(permission)) {
      permission_to_install_r <- electricShine:::.prompt_install_r(app_root_path)
    }
    else {
      permission_to_install_r <- TRUE
    }
    os <- electricShine::get_os()
    cran_like_url <- electricShine:::construct_mran_url(mran_date = mran_date, 
                                        cran_like_url = cran_like_url)
    # electricShine::create_folder(app_root_path)
    # electricShine::copy_template(app_root_path)
   
    ## ACTIVATE THIS IF YOU WANT TO INSTALL R
    #  electricShine::install_r(cran_like_url = cran_like_url, 
    #                          app_root_path = app_root_path, 
    #                          mac_url = mac_url, 
    #                          permission_to_install = permission_to_install_r)
    # electricShine::trim_r(app_root_path = app_root_path)
   
    
    if (identical(os, "win")) {
      library_path <- base::file.path(app_root_path, "app", 
                                      "r_lang", "library", fsep = "/")
    }
    if (identical(os, "mac")) {
      library_path <- file.path(app_root_path, "app/r_lang/Library/Frameworks/R.framework/Versions")
      library_path <- list.dirs(library_path, recursive = FALSE)
      library_path <- library_path[grep("\\d+\\.(?:\\d+|x)(?:\\.\\d+|x){0,1}", 
                                        library_path)][[1]]
      library_path <- file.path(library_path, "Resources/library", 
                                fsep = "/")
    }
    if (!base::is.null(git_host)) {
      my_package_name <- electricShine::install_user_app(library_path = library_path, 
                                                         repo_location = git_host, repo = git_repo, repos = cran_like_url, 
                                                         package_install_opts = package_install_opts)
    }
    if (!is.null(local_package_path)) {
      my_package_name <- electricShine::install_user_app(library_path = library_path, 
                                                         repo_location = "local", repo = local_package_path, 
                                                         repos = cran_like_url, package_install_opts = package_install_opts)
    }
    
  }

# electricShine::electrify(app_name = "FastMapping",
#                          short_description = "FastMapping",
#                          semantic_version = "1.0.0",
#                          build_path = buildPath,
#                          function_name = "run_app",
#                          git_host = "github",
#                          git_repo = "PPaccioretti/FastMapping@pkg",
#                          package_install_opts = list(type = "binary",
#                                                      dependencies = c("Depends", 
#                                                                       "Imports")
#                          ),
#                          cran_like_url = "https://cran.r-project.org"
# )


install_r_and_package(app_name = "FastMapping",
                      # product_name = "product_name",
                      # short_description = NULL,
                      # semantic_version = NULL,
                      build_path = "electron",
                      mran_date = NULL,
                      cran_like_url = "http://cran.r-project.org",
                      # function_name = NULL,
                      git_host = "github",
                      git_repo = "PPaccioretti/FastMapping@pkg",
                      package_install_opts = list(type = "binary",
                                                  dependencies = c("Depends", 
                                                                   "Imports")
                      ),
                      local_package_path = NULL,
                      # run_build = TRUE,
                      # nodejs_path = file.path(system.file(package = "electricShine"),
                      #                         "nodejs"),
                      # nodejs_version = "v12.16.2",
                      permission = TRUE,
                      mac_url = "https://mac.r-project.org/el-capitan/R-3.6-branch/R-3.6-branch-el-capitan-sa-x86_64.tar.gz")




