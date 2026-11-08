packages <- c('ggplot2','patchwork','dplyr','scales','jsonlite','ragg','svglite')
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))install.packages(missing,repos='https://cloud.r-project.org')
if(!capabilities('cairo'))stop('A Cairo-enabled R build is required for editable PDF output.')
cat('Figure dependencies available. Install Arial on the operating system for matching typography.\n')
