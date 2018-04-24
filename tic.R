add_package_checks(warnings_are_errors = FALSE)

get_stage("install") %>%
  # install lwgeom with its own library since linking again postgis source install fails sometimes
  add_code_step(install.packages("lwgeom", configure.args="--without-liblwgeom")) %>% # normal installation does not work
  add_code_step(remotes::install_github("r-spatial/stars"))

###
# deploy pkgdowm site
###
if (Sys.getenv("id_rsa") != "") {

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(orphan = TRUE, path = "docs", branch = "gh-pages"))
}
