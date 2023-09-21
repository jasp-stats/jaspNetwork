.onLoad <- function(libname, pkgname) {

  if (jaspBase:::getOS() == "osx" &&
      isTRUE(try(jaspBase::jaspResultsCalledFromJasp()))
  ) {

    # based on https://stackoverflow.com/questions/68517734/how-to-install-the-rgl-package-in-macos-big-sur
    options(rgl.useNULL = TRUE)

  }
}
