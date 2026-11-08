suppressWarnings(Sys.setlocale('LC_ALL','English_United States.utf8'))
library(ggplot2)
library(patchwork)
library(grid)
library(jsonlite)
if(.Platform$OS.type=='windows')grDevices::windowsFonts(Arial=grDevices::windowsFont('Arial'))
full <- commandArgs(FALSE)
HERE <- dirname(normalizePath(sub('^--file=','',grep('^--file=',full,value=TRUE)[1]),winslash='/'))
FIGURE_ARGS <- commandArgs(trailingOnly=TRUE)
if(length(FIGURE_ARGS)>2L)stop('Usage: Rscript build_figures.R [input-root] [output-dir]')
if(length(FIGURE_ARGS)<1L)FIGURE_ARGS <- file.path(HERE,'frozen_inputs')
if(length(FIGURE_ARGS)<2L)FIGURE_ARGS <- c(FIGURE_ARGS,file.path(HERE,'output'))
source(file.path(HERE,'load_main_sources.R'))
source(file.path(HERE,'panel_alignment.R'))
QA <- file.path(OUT_ROOT,'QA')
dir.create(QA,showWarnings=FALSE)
PY <- Sys.getenv('V1_QA_PYTHON',Sys.which('python'))

# Resolve null grid units on the final device, in their actual layout viewport.
# Bare convertWidth(null units) returns zero and cannot measure plot rectangles.
write_patchwork_panel_layout <- function(plot,manifest_path,width_in,height_in,panel_ids=NULL,
 row_groups=NULL,column_groups=NULL,exemptions=list()) {
 probe <- tempfile(fileext='.pdf')
 cairo_pdf(probe,width=width_in,height=height_in,family='Arial')
 g <- patchworkGrob(plot)
 r <- g$layout[grepl('^panel(?:-[0-9]+)?$',g$layout$name,perl=TRUE),,drop=FALSE]
 r <- r[order(r$t,r$l),,drop=FALSE]
 if(is.null(panel_ids)) panel_ids <- letters[seq_len(nrow(r))]
 stopifnot(length(panel_ids)==nrow(r))
 grid.newpage();grid.draw(g);grid.force()
 pushViewport(viewport(layout=grid.layout(length(g$heights),length(g$widths),
   widths=g$widths,heights=g$heights,respect=g$respect)))
 panels <- lapply(seq_len(nrow(r)),function(i) {
  z<-r[i,];pushViewport(viewport(layout.pos.row=z$t:z$b,layout.pos.col=z$l:z$r))
  lo<-deviceLoc(unit(0,'npc'),unit(0,'npc'),valueOnly=TRUE)
  hi<-deviceLoc(unit(1,'npc'),unit(1,'npc'),valueOnly=TRUE)
  popViewport()
  list(id=panel_ids[i],bbox_pt=c(lo$x,lo$y,hi$x,hi$y)*72,grid_id='final-grid',
    row_start=z$t-1L,row_stop=z$b,col_start=z$l-1L,col_stop=z$r)
 })
 popViewport();dev.off();unlink(probe)
 m<-list(schema_version=1L,backend='r-patchwork',figure=list(width_pt=width_in*72,height_pt=height_in*72),panels=panels,exemptions=exemptions)
 if(!is.null(row_groups))m$row_groups<-row_groups
 if(!is.null(column_groups))m$column_groups<-column_groups
 write_json(m,manifest_path,auto_unbox=TRUE,pretty=TRUE,digits=NA)
 invisible(m)
}

# Two chromatic hues from the Okabe-Ito palette discussed in Nature Methods.
BLUE <- '#0072B2'; RED <- '#D55E00'; NEUTRAL <- '#747B82'; INK <- '#252A2D'
theme_j <- function() theme_classic(base_size=7,base_family='Arial')+
 theme(text=element_text(colour=INK),axis.text=element_text(size=6.8,colour=INK),
 axis.title=element_text(size=7),axis.line=element_line(linewidth=.30,colour=INK),
 axis.ticks=element_line(linewidth=.30),axis.ticks.length=unit(1.1,'mm'),
 plot.title=element_text(size=7.3,face='bold',hjust=0,margin=margin(b=5)),
 plot.subtitle=element_text(size=6.5,margin=margin(b=5),colour='#4D555A'),
 plot.tag=element_text(size=8,face='bold'),plot.tag.position='topleft',
 legend.title=element_blank(),legend.text=element_text(size=6.6),
 legend.key.size=unit(8,'pt'),legend.spacing.x=unit(3,'pt'),
 legend.margin=margin(0,0,2,0),plot.margin=margin(5,6,5,5),panel.grid=element_blank(),
 plot.background=element_rect(fill='white',colour=NA),panel.background=element_rect(fill='white',colour=NA))
tagged <- function(p,tag,title,subtitle=NULL) p+labs(tag=tag,title=title,subtitle=subtitle)+theme_j()
save_final <- function(p,name,w=183,h=130,groups=NULL,cols=NULL,ids=NULL,exemptions=list()) {
 width<-w/25.4;height<-h/25.4
 if(inherits(p,'patchwork'))require_patchwork_panel_alignment(p,
  manifest_path=file.path(QA,paste0(name,'.layout.json')),
  report_path=file.path(QA,paste0(name,'.alignment.json')),
  overlay_svg=file.path(QA,paste0(name,'.alignment.svg')),
  width_in=width,height_in=height,panel_ids=ids,row_groups=groups,column_groups=cols,
  exemptions=exemptions,audit_script=file.path(HERE,'audit_panel_alignment.py'),python=PY,strict=TRUE)
 path<-file.path(OUT_ROOT,name)
 svglite::svglite(paste0(path,'.svg'),width=width,height=height,bg='white')
 print(p);grDevices::dev.off()
 ggsave(paste0(path,'.pdf'),p,width=width,height=height,device=cairo_pdf,bg='white')
 ggsave(paste0(path,'.png'),p,width=width,height=height,dpi=300,device=ragg::agg_png,bg='white')
 ggsave(paste0(path,'.tiff'),p,width=width,height=height,dpi=600,device=ragg::agg_tiff,compression='lzw',bg='white')
 cat('EXPORTED ',name,'\n')
}

# Figure 1: registry reconstruction and the three prespecified analysis roles.
nodes <- function(id,x,y,w,h,label,tone='neutral') data.frame(id,x,y,w,h,label,tone)
diagram <- function(n,edges,tag,title) {
 n$xmin<-n$x-n$w/2;n$xmax<-n$x+n$w/2;n$ymin<-n$y-n$h/2;n$ymax<-n$y+n$h/2
 if(!'arrow'%in%names(edges))edges$arrow<-TRUE
 ggplot()+geom_segment(data=edges[!edges$arrow,],aes(x=x,y=y,xend=xend,yend=yend),linewidth=.30,colour='#737E84')+
 geom_segment(data=edges[edges$arrow,],aes(x=x,y=y,xend=xend,yend=yend),linewidth=.30,
  colour='#737E84',arrow=arrow(length=unit(1.05,'mm'),type='closed'))+
 geom_rect(data=n,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=tone),colour='#8F999E',linewidth=.3)+
 geom_text(data=n,aes(x=x,y=y,label=label),family='Arial',size=2.32,lineheight=1.02,colour=INK)+
 scale_fill_manual(values=c(neutral='#F5F6F6',development='#EDF5FA',external='#FCF2EB'))+
 coord_cartesian(xlim=c(0,100),ylim=c(0,100),expand=FALSE,clip='off')+
 labs(tag=tag,title=title)+theme_void(base_family='Arial')+
 theme(plot.title=element_text(size=7.3,face='bold',margin=margin(b=7)),
  plot.tag=element_text(size=8,face='bold'),plot.tag.position='topleft',legend.position='none',plot.margin=margin(5,5,5,5))
}
na<-rbind(
 nodes('search',48,94,64,10,'NCBI BioProject search\nThree query families'),
 nodes('hits',48,79,54,10,'197 query-hit rows'),nodes('unique',48,64,54,10,'143 unique BioProjects'),
 nodes('excluded',21,44,39,25,'126 excluded\n58 animal/preclinical\n42 pediatric/neonatal\n20 incompatible assays\n5 no qualifying question\n1 non-gut'),
 nodes('eligible',75,48,44,12,'17 eligible projects'),
 nodes('six',75,30,44,12,'6 projects recovered\nby exact queries','development'),
 nodes('eleven',21,22,39,13,'11 retrospectively eligible\nNot added post hoc'),
 nodes('known',21,7,39,11,'1 accession cross-check\nPRJNA691455'),
 nodes('final',75,7,44,12,'Frozen V1 analysis\n7 BioProjects','development'))
ea<-data.frame(x=c(48,48,48,48,75,75,53,46,21,40.5),y=c(89,74,59,59,42,24,48,48,28.5,7),
 xend=c(48,48,21,75,75,75,46,46,21,53),yend=c(84,69,56.5,54,36,13,48,28.5,28.5,7))
# Retrospective branch is routed through the gap between exclusion and inclusion.
ea<-ea[-c(7,8,9),];ea$arrow<-TRUE
ea<-rbind(ea,data.frame(x=c(53,46,46,21),y=c(48,48,31,31),
 xend=c(46,46,21,21),yend=c(48,31,31,28.5),arrow=c(FALSE,FALSE,FALSE,TRUE)))
nb<-rbind(
 nodes('input',50,94,96,10,'Seven public BioProjects\nGenus tables and 12 analysis sets'),
 nodes('icu',17,76,29,13,'ICU controls\nA01, A02','development'),
 nodes('healthy',50,76,29,13,'Healthy controls\nA03, A06'),
 nodes('support',83,76,29,13,'Supportive analyses\nS01-S06'),
 nodes('imeta',17,58,29,12,'Genus\nmeta-analysis','development'),
 nodes('hmeta',50,58,29,12,'Genus\nmeta-analysis'),
 nodes('sdi',17,40,29,12,'Locked six-genus\nSDI','development'),
 nodes('context',50,40,29,12,'Comparator-specific\ndysbiosis context'),
 nodes('longitudinal',83,54,29,21,'Longitudinal,\nintervention and\nclinical analyses'),
 nodes('external',17,22,29,13,'External evaluation\nA04 healthy\nA05 trauma','external'),
 nodes('failure',17,6,29,10,'Failed external\ntransfer','external'),
 nodes('diagnosis',66,6,60,10,'Post hoc diagnosis\nDirection reversal and feature absence'))
eb<-data.frame(x=c(17,50,83,17,50,83,17,50,17,17,31.5),
 y=c(89,89,89,69.5,69.5,69.5,52,52,34,15.5,6),
 xend=c(17,50,83,17,50,83,17,50,17,17,36),
 yend=c(82.5,82.5,82.5,64,64,64.5,46,46,28.5,11,6))
pa<-diagram(na,ea,'a','Project screening')
pb<-diagram(nb,eb,'b','Analysis workflow')
fig1<-pa+pb+plot_layout(widths=c(1,1.2))
save_final(fig1,'Figure_1',h=144,groups=list(c('a','b')),
 exemptions=list(list(panels='a',checks='panel-width',reason='Screening branch and analysis workflow have different structural complexity.')))

# Figure 2: community separation, followed by complementary genus contrasts.
f2a$Set<-sub('_.*$','',f2a$Analysis_Set)
f2a$Family<-ifelse(grepl('ICU',f2a$Meta_Family),'ICU control','Healthy control')
f2a$Y<-factor(paste(f2a$Set,f2a$Family,sep='  '),levels=rev(paste(f2a$Set,f2a$Family,sep='  ')))
p2a<-ggplot(f2a,aes(100*R2,Y,colour=Family))+
 geom_segment(aes(x=0,xend=100*R2,yend=Y),linewidth=.55,alpha=.35)+geom_point(aes(shape=Family),size=2.15)+
 geom_text(aes(x=100*R2+.45,label=sprintf('%.2f%%',100*R2)),hjust=0,size=2.3,colour=INK)+
 geom_text(aes(x=21,label=paste('FDR',sprintf('%.3f',FDR))),hjust=0,size=2.3,colour=INK)+
 scale_colour_manual(values=c('ICU control'=BLUE,'Healthy control'=BLUE),guide='none')+
 scale_shape_manual(values=c('ICU control'=16,'Healthy control'=17),guide='none')+
 scale_x_continuous(limits=c(0,25),breaks=c(0,5,10,15),expand=c(0,0))+
 labs(x='PERMANOVA R\u00b2 (%)',y=NULL)+theme_j()+
 theme(axis.line.y=element_blank(),axis.ticks.y=element_blank())
p2a<-tagged(p2a,'a','Community differences')
forest <- function(z,tag,title,xlab,selected=FALSE) {
 z$Genus<-pretty_genus(z$Genus_Feature);z<-z[order(z$REML_Effect),]
 z$Y<-factor(z$Genus,levels=z$Genus)
 z$Direction<-ifelse(z$REML_Effect>0,'Higher','Lower')
 z$Selected<-z$Genus%in%c('Bilophila','Butyricimonas','Parabacteroides','Hoylesella','Campylobacter','Atopobium')
 z$Opacity<-if(selected)ifelse(z$Selected,1,.38)else 1
 ggplot(z,aes(REML_Effect,Y))+
 geom_vline(xintercept=0,linetype='dashed',linewidth=.3,colour='#889198')+
 geom_segment(aes(x=REML_CI_Low,xend=REML_CI_High,yend=Y,alpha=Opacity),linewidth=.45,colour='#586269')+
 geom_point(aes(colour=Direction,alpha=Opacity),size=1.85)+
 scale_alpha_identity()+scale_colour_manual(values=c(Higher=RED,Lower=BLUE),guide='none')+
 scale_y_discrete(labels=function(x)gsub(' group$','\ngroup',gsub('NK4A136 ','NK4A136\n',x)))+
 labs(tag=tag,title=title,x=xlab,y=NULL)+theme_j()+
 theme(axis.text.y=element_text(face='italic',size=6.5),axis.line.y=element_blank(),axis.ticks.y=element_blank())
}
p2b<-forest(f2b,'b','ICU-control meta-analysis','CLR contrast\n(sepsis \u2212 ICU control)',TRUE)
p2c<-forest(f2c,'c','Healthy-control meta-analysis','CLR contrast\n(sepsis \u2212 healthy control)')
fig2<-p2a+p2b+p2c+plot_layout(design='AA\nBC',heights=c(.55,1))
save_final(fig2,'Figure_2',h=139,groups=list(c('b','c')))

# Figure 3: apparent development performance and failed locked transfer.
f3a$Label<-c('A01  ICU controls','Development pooled','A02  ICU controls','A04  Healthy controls','A05  Trauma controls')[match(f3a$Dataset,c('A01_Sepsis_vs_ICU','Development_Pooled','A02_Sepsis_vs_ICU','A04_Sepsis_vs_Healthy','A05_Sepsis_vs_Trauma'))]
f3a$Label<-ifelse(grepl('Development',f3a$Dataset),'Development pooled',ifelse(grepl('A01',f3a$Dataset),'A01  ICU controls',ifelse(grepl('A02',f3a$Dataset),'A02  ICU controls',ifelse(grepl('A04',f3a$Dataset),'A04  Healthy controls','A05  Trauma controls'))))
f3a$Y<-factor(f3a$Label,levels=rev(c('Development pooled','A01  ICU controls','A02  ICU controls','A05  Trauma controls','A04  Healthy controls')))
f3a$Stage<-ifelse(grepl('A04|A05',f3a$Dataset),'External','Development')
p3a<-ggplot(f3a,aes(AUC,Y,colour=Stage))+
 geom_vline(xintercept=.5,linetype='dashed',linewidth=.35,colour='#8F989D')+
 geom_segment(aes(x=AUC_CI_Low,xend=AUC_CI_High,yend=Y),linewidth=.55)+geom_point(aes(shape=Stage),size=2)+
 geom_text(aes(x=1.08,label=sprintf('%.3f (%.3f\u2013%.3f)',AUC,AUC_CI_Low,AUC_CI_High)),hjust=0,size=2.3,colour=INK)+
 scale_colour_manual(values=c(Development=BLUE,External=RED),guide='none')+
 scale_shape_manual(values=c(Development=16,External=17),guide='none')+
 scale_x_continuous(limits=c(0,1.55),breaks=seq(0,1,.25),expand=c(0,0))+
 labs(tag='a',title='Locked SDI discrimination',x='AUC (95% bootstrap CI)',y=NULL)+theme_j()+
 theme(axis.line.y=element_blank(),axis.ticks.y=element_blank())
f3b$Genus<-pretty_genus(f3b$Genus_Feature)
f3b$Y<-factor(f3b$Genus,levels=rev(c('Campylobacter','Atopobium','Bilophila','Butyricimonas','Parabacteroides','Hoylesella')))
f3b$Absent<-grepl('ABSENCE',f3b$Diagnostic_Label)
p3b<-ggplot(f3b,aes(y=Y))+
 geom_vline(xintercept=0,linetype='dashed',linewidth=.3,colour='#8F989D')+
 geom_segment(data=f3b[!f3b$Absent,],aes(x=Mean_External_Expected_Signed_Effect,xend=Mean_Development_Expected_Signed_Effect,yend=Y),colour='#ADB4B8',linewidth=.45)+
 geom_point(aes(x=Mean_Development_Expected_Signed_Effect),colour=BLUE,size=1.8)+
 geom_point(data=f3b[!f3b$Absent,],aes(x=Mean_External_Expected_Signed_Effect),colour=RED,shape=17,size=1.8)+
 geom_text(aes(x=2.05,label=ifelse(Absent,'Absent','Reversed')),hjust=0,size=2.18,colour=INK)+
 scale_x_continuous(limits=c(-3,3.85),breaks=c(-3,-1.5,0,1.5),expand=c(0,0))+
 labs(tag='b',title='Genus direction',subtitle='Blue: development   Vermilion: external',x='Expected-signed CLR effect',y=NULL)+theme_j()+
 theme(axis.text.y=element_text(face='italic',size=6.5),axis.line.y=element_blank(),axis.ticks.y=element_blank())
distribution <- function(prefix,comparator,tag,title,col,seed) {
 z<-f3c[grepl(paste0('^',prefix),f3c$Analysis_Set),]
 z$Group<-factor(ifelse(grepl('Sepsis',z$Comparison_Group,ignore.case=TRUE),'Sepsis',comparator),levels=c(comparator,'Sepsis'))
 nn<-table(z$Group);cc<-setNames(c(col,RED),c(comparator,'Sepsis'))
 ggplot(z,aes(Group,SDI_Equal_Balance,colour=Group,fill=Group))+
 geom_boxplot(width=.5,outlier.shape=NA,linewidth=.4,alpha=.10)+
 geom_point(aes(shape=Group),position=position_jitter(width=.08,height=0,seed=seed),size=1.15,alpha=.78)+
 scale_colour_manual(values=cc,guide='none')+scale_fill_manual(values=cc,guide='none')+
 scale_shape_manual(values=setNames(c(1,16),c(comparator,'Sepsis')),guide='none')+
 scale_x_discrete(labels=function(x)paste0(x,'\n(n = ',nn[x],')'))+
 scale_y_continuous(limits=c(-.35,7.25),breaks=c(0,3,6),expand=c(0,0))+
 labs(tag=tag,title=title,x=NULL,y='Equal-weight SDI')+theme_j()+theme(axis.ticks.x=element_blank())
}
p3c<-distribution('A04','Healthy','c','A04  Healthy controls',NEUTRAL,20261026)
p3d<-distribution('A05','Trauma','d','A05  Trauma controls',NEUTRAL,20261027)
fig3<-p3a+p3b+p3c+p3d+plot_layout(design='AA\nBC\nBD',heights=c(.85,.72,.72),widths=c(1.15,1))
save_final(fig3,'Figure_3',h=162,cols=list(c('c','d')),ids=c('a','b','c','d'))

# Figure 4: mean and SE at every frozen timepoint, with descriptive discharge.
time_label<-function(x) {y<-as.character(x);d<-c(M1_initial='Baseline',M2.5_ICU_day5='Day 5',M2.6_ICU_day6='Day 6',M2.7_ICU_day7='Day 7',M3_discharge='Discharge');i<-y%in%names(d);y[i]<-d[y[i]];y}
trajectory<-function(z,tag,title,ylab,col,subtitle=NULL,shannon=TRUE) {
 z<-z[order(z$Time_Order_Resolved),];z$X<-seq_len(nrow(z));z$Lab<-time_label(z$Time_Label)
 discharge<-tail(z$Lab,1)=='Discharge';linez<-if(discharge)z[-nrow(z),]else z
 p<-ggplot(z,aes(X,Mean))+geom_line(data=linez,colour=col,linewidth=.5)+
 geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),colour=col,width=.09,linewidth=.4)+
 geom_point(colour=col,size=1.9)+
 scale_x_continuous(breaks=z$X,labels=paste0(z$Lab,'\n(n = ',z$N,')'),expand=expansion(mult=c(.07,.09)))+
 labs(tag=tag,title=title,subtitle=subtitle,x=NULL,y=ylab)+theme_j()
 if(discharge)p<-p+geom_segment(x=z$X[nrow(z)-1],xend=tail(z$X,1),y=z$Mean[nrow(z)-1],yend=tail(z$Mean,1),linetype='dashed',linewidth=.5,colour=col)+
 geom_point(data=tail(z,1),shape=21,fill='white',colour=col,size=2.1,stroke=.55)
 if(shannon)p<-p+scale_y_continuous(limits=c(.95,3.35),breaks=c(1,2,3),expand=c(0,0))
 p
}
p4a<-trajectory(f4a,'a','PRJEB33360  Shannon','Shannon diversity',BLUE,'Time FDR = 0.0126')
p4b<-trajectory(f4b,'b','PRJNA691455  Shannon','Shannon diversity',BLUE,'Time FDR = 0.318')
f4c$X<-match(f4c$Time_Label,c('Day 1','Day 3','Day 7'))
p4c<-ggplot(f4c,aes(X,Mean,group=Status_Resolved,colour=Status_Resolved,shape=Status_Resolved))+
 geom_line(aes(linetype=Status_Resolved),linewidth=.5)+geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),width=.07,linewidth=.4)+geom_point(size=1.9)+
 geom_text(data=f4c[f4c$X==3,],aes(x=3.15,label=Status_Resolved),hjust=0,size=2.3,colour=INK,show.legend=FALSE)+
 scale_colour_manual(values=c(No=BLUE,Yes=RED),guide='none')+scale_shape_manual(values=c(No=16,Yes=17),guide='none')+
 scale_linetype_manual(values=c(No='solid',Yes='dashed'),guide='none')+
 scale_x_continuous(breaks=1:3,labels=c('Day 1\n10 / 10','Day 3\n10 / 10','Day 7\n9 / 9'),limits=c(.85,3.6),expand=c(0,0))+
 scale_y_continuous(limits=c(.95,3.35),breaks=c(1,2,3),expand=c(0,0))+
 labs(tag='c',title='PRJNA912621  Shannon',subtitle='Time \u00d7 cholestasis FDR = 0.266',x=NULL,y='Shannon diversity')+theme_j()
p4d<-trajectory(f4d,'d','PRJEB33360  SDI','Equal-weight SDI',BLUE,'Mean \u00b1 SE',FALSE)+
 scale_y_continuous(limits=c(-1.45,2.1),breaks=c(-1,0,1,2),expand=c(0,0))
fig4<-p4a+p4b+p4c+p4d+plot_layout(ncol=2)
save_final(fig4,'Figure_4',h=127,groups=list(c('a','b'),c('c','d')),cols=list(c('a','c'),c('b','d')))

# Reuse the frozen supplementary assertions, including all 17 numerical checks.
source(file.path(HERE,'load_supplement_sources.R'))
OUT_ROOT<-OUT_DIR
s1$Y<-factor(as.character(s1$Set),levels=rev(as.character(s1$Set)))
s1$Display<-paste0(as.character(s1$Set),'  ',s1$Project_ID)
overview<-function(variable,tag,title,xlab,limits,breaks,formatter) {
 ggplot(s1,aes(x=.data[[variable]],y=Y,colour=Stage))+
 geom_segment(aes(x=0,xend=.data[[variable]],yend=Y),linewidth=.5,alpha=.35,show.legend=FALSE)+geom_point(aes(shape=Stage),size=1.9)+
 geom_text(aes(label=formatter(.data[[variable]])),hjust=-.55,size=2.3,colour=INK,show.legend=FALSE)+
 scale_colour_manual(values=c(Development=BLUE,'External validation'=RED),name=NULL)+
 scale_shape_manual(values=c(Development=16,'External validation'=17),name=NULL)+
 scale_y_discrete(labels=setNames(s1$Display,as.character(s1$Set)))+
 scale_x_continuous(limits=limits,breaks=breaks,labels=scales::comma,expand=c(0,0))+
 labs(tag=tag,title=title,x=xlab,y=NULL)+theme_j()+theme(axis.line.y=element_blank(),axis.ticks.y=element_blank(),legend.position='top')
}
ps1a<-overview('Samples_N','a','Sample size','Samples (n)',c(0,180),c(0,50,100,150),function(x)as.character(x))
ps1b<-overview('Median_Depth','b','Sequencing depth','Median reads per sample',c(0,82000),c(0,20000,40000,60000),function(x)ifelse(x==round(x),scales::comma(x,accuracy=1),scales::comma(x,accuracy=.1)))
ps1<-ps1a+ps1b+plot_layout(guides='collect')&theme(legend.position='top')
save_final(ps1,'Supplementary_Figure_S1',h=88,groups=list(c('a','b')))
metric_levels<-unique(as.character(s2$Metric))
alpha_panels<-lapply(seq_along(metric_levels),function(i) {
 z<-s2[as.character(s2$Metric)==metric_levels[i],];z$Y<-factor(as.character(z$Set),levels=rev(c('A01','A02','A03','A06')))
 z$Family<-ifelse(grepl('ICU',z$Meta_Family),'ICU control','Healthy control')
 title<-c('Observed genus richness','Shannon diversity','Inverse Simpson')[i]
 ggplot(z,aes(x=Mean_Difference_A_minus_B,y=Y,colour=Family))+
 geom_vline(xintercept=0,linetype='dashed',linewidth=.3,colour='#8F989D')+
 geom_segment(aes(x=Mean_Difference_CI_Low,xend=Mean_Difference_CI_High,yend=Y),linewidth=.45)+geom_point(aes(shape=Family),size=1.9)+
 scale_colour_manual(values=c('ICU control'=BLUE,'Healthy control'=BLUE),guide='none')+
 scale_shape_manual(values=c('ICU control'=16,'Healthy control'=17),guide='none')+
 labs(tag=letters[i],title=title,x='Mean difference\n(sepsis \u2212 comparator)',y=NULL)+theme_j()+
 theme(axis.line.y=element_blank(),axis.ticks.y=element_blank())
})
ps2<-wrap_plots(alpha_panels,ncol=3)
save_final(ps2,'Supplementary_Figure_S2',h=76,groups=list(c('a','b','c')))
loo_panels<-lapply(seq_along(c('A01','A02','A04','A05')),function(i) {
 set<-c('A01','A02','A04','A05')[i];z<-s3[as.character(s3$Set)==set,]
 z$Y<-factor(as.character(z$Genus),levels=rev(c('Bilophila','Butyricimonas','Parabacteroides','Hoylesella','Campylobacter','Atopobium')))
 ggplot(z,aes(x=Delta_plot,y=Y))+
 geom_vline(xintercept=0,linetype='dashed',linewidth=.3,colour='#8F989D')+
 geom_segment(data=z[z$Column_present,],aes(x=0,xend=Delta_plot,yend=Y),linewidth=.5,colour=if(i<=2)BLUE else RED,alpha=.6)+
 geom_point(data=z[z$Column_present,],colour=if(i<=2)BLUE else RED,shape=if(i<=2)16 else 17,size=1.9)+
 geom_text(data=z[!z$Column_present,],aes(x=.055,label='Absent'),hjust=0,size=2.3,colour=INK)+
 scale_x_continuous(limits=c(-.05,.12),breaks=c(-.05,0,.05,.10),expand=c(0,0))+
 scale_y_discrete(limits=rev(c('Bilophila','Butyricimonas','Parabacteroides','Hoylesella','Campylobacter','Atopobium')))+
 labs(tag=letters[i],title=c('A01  ICU controls','A02  ICU controls','A04  Healthy controls','A05  Trauma controls')[i],x='\u0394AUC after genus omission',y=NULL)+theme_j()+
 theme(axis.text.y=element_text(face='italic',size=6.7),axis.line.y=element_blank(),axis.ticks.y=element_blank())
})
ps3<-wrap_plots(loo_panels,ncol=2)
save_final(ps3,'Supplementary_Figure_S3',h=119,groups=list(c('a','b'),c('c','d')),cols=list(c('a','c'),c('b','d')))
writeLines(capture.output(sessionInfo()),file.path(QA,'R_sessionInfo.txt'))
writeLines(capture.output(warnings()),file.path(QA,'R_warnings.txt'))
