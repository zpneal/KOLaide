.onAttach <- function(lib,pkg) {
  local_version <- utils::packageVersion("KOLaide")
  packageStartupMessage("KOLaide v",local_version)
  packageStartupMessage("Cite: Neal, Z. P., Neal, J. W., Cappella, E., and Dockerty, M. (2025). KOLaide: A tool for selecting")
  packageStartupMessage("      key opinion leaders under practical constraints. GitHub. https://github.com/zpneal/KOLaid/")
  packageStartupMessage("      ")
  packageStartupMessage("Help: email zpneal@msu.edu; github zpneal/KOLaide")
  packageStartupMessage("Beta: type devtools::install_github(\"zpneal/KOLaide\", ref = \"devel\")")
}

