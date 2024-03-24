# UPDATE PACKAGES
# created by Jorge Sepulveda 5/21/21
# use after upgrading R 

install.packages(c("data.table", "XML", "rlist", "openxlsx",'ggstatsplot'))
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.13") # modify as appropriate


# find packages from previous installaton
# ps <- dir('C:/Users/jsepulveda/RStudio/R/R-4.0.3/library') # modify as appropriate
ps <- dir('/Library/Frameworks/R.framework/Versions/4.0/Resources/library/') # modify as appropriate

# find all BioConductor packages
repos <- BiocManager::repositories()
repos <- repos[startsWith(names(repos), "BioC")]
allbc <- unname(available.packages(repos=repos)[,'Package'])

# find previous bioconductor packages
bc <- intersect(ps,allbc)
BiocManager::install(bc)

# find all installed packages
inst <- rownames(installed.packages())

# install the missing ones
install.packages(setdiff(inst,ps))



