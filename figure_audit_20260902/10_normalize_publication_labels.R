# ============================================================
# Sepsis V1 — publication display-label normalizer
#
# PURPOSE
#   Correct the PUBLICATION-FACING wording for S06 / PRJNA912621
#   without changing any statistical analysis or legacy internal key.
#
# WHY KEEP THE LEGACY INTERNAL KEY?
#   The verified reproduction chain uses:
#     S06_PRJNA912621_Organ_Dysfunction_Longitudinal
#   as a historical machine identifier.
#   Renaming that key inside analysis 01–07 would create unnecessary
#   changes to already verified outputs.
#
# This script creates publication-facing CSV COPIES with:
#   S06_PRJNA912621_Cholestasis_Longitudinal
#   Cholestasis longitudinal support
#   Supportive cholestasis trajectory interaction
#
# It never overwrites expected_results.
#
# Usage:
#   Rscript 10_normalize_publication_labels.R "/path/to/package-root"
# ============================================================

options(stringsAsFactors=FALSE)
args<-commandArgs(trailingOnly=TRUE)

is_root<-function(x) dir.exists(file.path(x,"expected_results","scientific_outputs"))

script_dir<-function(){
  a<-commandArgs(trailingOnly=FALSE)
  h<-grep("^--file=",a,value=TRUE)
  if(length(h)) return(dirname(normalizePath(sub("^--file=","",h[1]),winslash="/",mustWork=FALSE)))
  getwd()
}
find_root<-function(){
  if(length(args)>=1 && nzchar(args[1])){
    x<-normalizePath(args[1],winslash="/",mustWork=TRUE)
    if(!is_root(x)) stop("Invalid package root: ",x)
    return(x)
  }
  for(st in unique(c(script_dir(),getwd()))){
    x<-normalizePath(st,winslash="/",mustWork=FALSE)
    for(i in 0:6){
      if(is_root(x)) return(x)
      p<-dirname(x); if(identical(p,x)) break; x<-p
    }
  }
  stop("Cannot auto-detect package root.")
}

ROOT<-find_root()
SRC<-file.path(ROOT,"expected_results","scientific_outputs")
OUT<-file.path(ROOT,"publication_ready_tables")
dir.create(OUT,recursive=TRUE,showWarnings=FALSE)

copy_and_normalize<-function(filename){
  src<-file.path(SRC,filename)
  if(!file.exists(src)) stop("Missing: ",src)
  x<-read.csv(src,check.names=FALSE,stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM")

  # Replace only human-facing S06 wording / display identifier.
  for(nm in names(x)){
    if(is.character(x[[nm]])){
      x[[nm]]<-gsub(
        "S06_PRJNA912621_Organ_Dysfunction_Longitudinal",
        "S06_PRJNA912621_Cholestasis_Longitudinal",
        x[[nm]], fixed=TRUE
      )
      x[[nm]]<-gsub(
        "Organ dysfunction longitudinal support",
        "Cholestasis longitudinal support",
        x[[nm]], fixed=TRUE
      )
      x[[nm]]<-gsub(
        "Supportive organ-dysfunction trajectory interaction",
        "Supportive cholestasis trajectory interaction",
        x[[nm]], fixed=TRUE
      )
    }
  }

  dst<-file.path(OUT,filename)
  write.csv(x,dst,row.names=FALSE,fileEncoding="UTF-8")
  dst
}

targets<-c(
  "21_Table1_Cohorts.csv",
  "21_Results_Number_Summary.csv"
)

made<-vapply(targets,copy_and_normalize,character(1))

writeLines(c(
  "Publication display normalization only.",
  "Statistical values: unchanged.",
  "Legacy internal analysis key in expected_results: retained.",
  "",
  paste("Created:",basename(made))
),file.path(OUT,"README_publication_label_normalization.txt"))

cat("Publication-facing normalized copies created in:\n",OUT,"\n",sep="")
