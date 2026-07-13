# .Rprofile — keep R startup fast in this data-heavy repo.
#
# This project ships ~0.5 GB of (gzipped) CSV/JSONL/RDS under data/. renv's autoloader scans
# the whole project tree for dependencies on every R startup, which can take many
# minutes here. We therefore do NOT source renv/activate.R. Instead, if an renv
# project library has been restored we add it to .libPaths() manually; otherwise
# we use the system/user library. `renv::restore()`/`snapshot()` still work.
#
# To install the exact pinned package versions:  Rscript -e 'renv::restore()'

local({
  libdirs <- list.dirs(file.path("renv", "library"), recursive = TRUE)
  # an renv library leaf looks like renv/library/<platform>/R-x.y/<arch>
  leaves <- libdirs[grepl("R-[0-9]+\\.[0-9]+", libdirs)]
  if (length(leaves)) .libPaths(c(leaves[[length(leaves)]], .libPaths()))
  options(
    renv.config.synchronized.check = FALSE,
    renv.config.auto.snapshot      = FALSE,
    renv.config.autoloader.enabled = FALSE
  )
})
