# ============================================================
# Sepsis V1 — PUBLIC figure-source audit v1.1
# Portable / package-relative
# ============================================================

options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly=TRUE)

is_root <- function(x) {
  dir.exists(file.path(x,"expected_results","scientific_outputs")) &&
    file.exists(file.path(x,"figure_audit_20260902",
                          "V1_Figure1_Screening_Flow_FINAL_20260901.csv"))
}

script_dir <- function() {
  a <- commandArgs(trailingOnly=FALSE)
  h <- grep("^--file=",a,value=TRUE)
  if(length(h)) return(dirname(normalizePath(sub("^--file=","",h[1]),winslash="/",mustWork=FALSE)))
  getwd()
}

find_root <- function() {
  if(length(args)>=1 && nzchar(args[1])) {
    x<-normalizePath(args[1],winslash="/",mustWork=TRUE)
    if(!is_root(x)) stop("Invalid package root: ",x)
    return(x)
  }
  for(st in unique(c(script_dir(),getwd()))) {
    x<-normalizePath(st,winslash="/",mustWork=FALSE)
    for(i in 0:6) {
      if(is_root(x)) return(x)
      p<-dirname(x); if(identical(p,x)) break; x<-p
    }
  }
  stop("Cannot auto-detect package root.")
}

ROOT <- find_root()
S <- file.path(ROOT,"expected_results","scientific_outputs")
A <- file.path(ROOT,"figure_audit_20260902")
OUT <- if(length(args)>=2 && nzchar(args[2])) args[2] else file.path(ROOT,"figure_source_audit_public")
dir.create(OUT,recursive=TRUE,showWarnings=FALSE)

rc <- function(p) read.csv(p,check.names=FALSE,stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM")
checks <- data.frame(Check=character(),PASS=logical(),Observed=character(),Expected=character(),
                     stringsAsFactors=FALSE)
add <- function(name,pass,obs,exp) {
  checks[nrow(checks)+1,] <<- list(name,isTRUE(pass),as.character(obs),as.character(exp))
}

scr<-rc(file.path(A,"V1_Figure1_Screening_Flow_FINAL_20260901.csv"))
get<-function(m) as.numeric(scr$Value[scr$Metric==m])
add("Figure1 raw",get("Raw_query_hit_rows")==197,get("Raw_query_hit_rows"),197)
add("Figure1 unique",get("Unique_BioProjects")==143,get("Unique_BioProjects"),143)
add("Figure1 excluded",get("Excluded")==126,get("Excluded"),126)
add("Figure1 eligible",get("Eligible_reconstructed_search")==17,get("Eligible_reconstructed_search"),17)
add("Figure1 locked recovered",get("Locked_recovered_exact_query")==6,get("Locked_recovered_exact_query"),6)
add("Figure1 added eligible",get("Additional_eligible_retrospective")==11,get("Additional_eligible_retrospective"),11)
add("Figure1 known item",get("Known_item_recovered")==1,get("Known_item_recovered"),1)
add("Figure1 final set",get("Final_frozen_V1_analytic_set")==7,get("Final_frozen_V1_analytic_set"),7)
add("Figure1 pending",get("Pending")==0,get("Pending"),0)

f2<-rc(file.path(S,"21_Figure2A_Beta_PERMANOVA_Source.csv"))
add("Figure2A rows",nrow(f2)==4,nrow(f2),4)
exp_r2<-c(A01=.014655,A02=.053709,A03=.144853,A06=.131132)
for(nm in names(exp_r2)) {
  z<-f2$R2[grepl(paste0("^",nm),f2$Analysis_Set)]
  add(paste0("Figure2A ",nm," R2"),length(z)==1 && abs(z-exp_r2[nm])<5e-6,z,exp_r2[nm])
}

f3a<-rc(file.path(S,"21_Figure3A_SDI_AUC_Source.csv"))
auc<-function(pat) f3a$AUC[grepl(pat,f3a$Dataset)]
add("Development pooled AUC",abs(auc("^Development_Pooled")-.731213)<1e-6,auc("^Development_Pooled"),.731213)
add("A04 external AUC",abs(auc("^A04")-.200980)<1e-6,auc("^A04"),.200980)
add("A05 external AUC",abs(auc("^A05")-.271605)<1e-6,auc("^A05"),.271605)

f3b<-rc(file.path(S,"21_Figure3B_Candidate_Diagnosis_Source.csv"))
revn<-sum(f3b$Diagnostic_Label=="COMPLETE_EXTERNAL_DIRECTION_REVERSAL")
absn<-sum(f3b$Diagnostic_Label=="PROJECT_LEVEL_FEATURE_ABSENCE")
add("Figure3B reversal count",revn==4,revn,4)
add("Figure3B absence count",absn==2,absn,2)

g3 <- as.character(f3b$Genus_Feature)
g3 <- sub("^Genus__", "", g3)
g3 <- sub("\\|.*$", "", g3)
missing3 <- as.character(f3b$Missing_In_External_Project) %in% c("TRUE","True","1",1)
status3 <- ifelse(missing3,"Absent","Reversed")
obs3 <- setNames(status3,g3)
exp3 <- c(
  Bilophila="Reversed",
  Butyricimonas="Reversed",
  Parabacteroides="Reversed",
  Hoylesella="Reversed",
  Campylobacter="Absent",
  Atopobium="Absent"
)
map_ok <- all(names(exp3) %in% names(obs3)) &&
          all(obs3[names(exp3)] == exp3)
add(
  "Figure3B exact genus-status mapping",
  map_ok,
  paste(names(exp3),obs3[names(exp3)],collapse="; "),
  paste(names(exp3),exp3,collapse="; ")
)

f4b<-rc(file.path(S,"21_Figure4B_T03_Shannon_Source.csv"))
add("Figure4B uses mean source",all(is.finite(f4b$Mean)) && all(is.finite(f4b$SE)),
    paste(round(f4b$Mean,6),collapse=" | "),"finite Mean + SE")

f4c<-rc(file.path(S,"21_Figure4C_T06_Shannon_Source.csv"))
pairs<-setNames(f4c$N,paste(f4c$Time_Label,f4c$Status_Resolved,sep="|"))
expn<-c("Day 1|No"=10,"Day 1|Yes"=10,"Day 3|No"=10,"Day 3|Yes"=10,"Day 7|No"=9,"Day 7|Yes"=9)
add("Figure4C group N",all(pairs[names(expn)]==expn),
    paste(names(expn),pairs[names(expn)],collapse="; "),
    paste(names(expn),expn,collapse="; "))
med_no<-f4c$Median[f4c$Time_Label=="Day 7" & f4c$Status_Resolved=="No"]
med_yes<-f4c$Median[f4c$Time_Label=="Day 7" & f4c$Status_Resolved=="Yes"]
add("Figure4C Day7 direction",length(med_no)==1 && length(med_yes)==1 && med_no>med_yes,
    paste0("No=",round(med_no,3),"; Yes=",round(med_yes,3)),"No > Yes")

sumdf<-rc(file.path(S,"21_Results_Number_Summary.csv"))
fdr<-sumdf$Value[sumdf$Topic=="T06 Shannon interaction FDR"]
add("Figure4C interaction FDR",length(fdr)==1 && as.numeric(fdr)==.266,fdr,.266)

write.csv(checks,file.path(OUT,"PUBLIC_Figure_Source_Audit.csv"),
          row.names=FALSE,fileEncoding="UTF-8")
writeLines(capture.output(sessionInfo()),file.path(OUT,"PUBLIC_Figure_Source_Audit_sessionInfo.txt"))

cat("Checks:",nrow(checks),"\nPASS:",sum(checks$PASS),"\nFAIL:",sum(!checks$PASS),"\n")
if(any(!checks$PASS)) {
  print(checks[!checks$PASS,,drop=FALSE])
  quit(status=1)
}
cat("ALL PUBLIC FIGURE SOURCE CHECKS PASS\n")
