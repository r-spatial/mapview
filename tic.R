get_stage("install") %>%
  # install lwgeom with its own library since linking again postgis source install fails sometimes
  add_step(step_install_cran("lwgeom", configure.args="--without-liblwgeom"))  # normal installation does not work

get_stage("script") %>%
  add_step(step_install_github("r-spatial/stars")) %>%
  add_step(step_install_github("hrbrmstr/albersusa")) %>%
  add_step(step_rcmdcheck(error_on = "error"))

get_stage("after_success") %>%
  add_code_step(covr::codecov())

###
# deploy pkgdowm site
###
if (Sys.getenv("id_rsa") != "") {

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(path = "docs", branch = "gh-pages"))
}
