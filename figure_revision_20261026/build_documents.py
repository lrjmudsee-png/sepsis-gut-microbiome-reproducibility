from pathlib import Path
from copy import deepcopy
import sys,json,re
from io import BytesIO
from docx import Document
from docx.shared import Inches,Pt,RGBColor
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT,WD_CELL_VERTICAL_ALIGNMENT
from reportlab.pdfgen import canvas
from reportlab.lib.styles import ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.enums import TA_LEFT
from pypdf import PdfReader,PdfWriter
import fitz

HERE=Path(__file__).resolve().parent;ROOT=HERE.parent
import argparse
parser=argparse.ArgumentParser(description='Assemble a local manuscript and supplementary PDF from author-provided originals.')
parser.add_argument('--source-dir',type=Path,required=True)
parser.add_argument('--figures',type=Path,default=HERE/'output')
parser.add_argument('--output',type=Path,default=HERE/'documents')
options=parser.parse_args()
SRC=options.source_dir.resolve();OUT=options.output.resolve();FIG=options.figures.resolve()
OUT.mkdir(parents=True,exist_ok=True)
source=SRC/'Original_Manuscript.docx'
doc=Document(source)
original_paragraphs=[p.text for p in doc.paragraphs]
tables=[deepcopy(t._element) for t in doc.tables]
legends=[
('Study selection and analysis workflow.',
 '(a) Three NCBI BioProject query families returned 197 query-hit rows and 143 unique projects. Of these, 126 were excluded and 17 were eligible in the reconstructed search. Six projects were recovered by the exact queries; 11 additional eligible projects identified retrospectively were not added to the frozen analysis. An independent accession cross-check retained PRJNA691455, giving seven BioProjects in the final analytic set. All 143 projects were adjudicated, with none pending. Exclusions comprised animal or preclinical studies (58), pediatric or neonatal-only studies (42), non-16S rRNA gene or incompatible assays or questions (20), no qualifying adult human gut 16S rRNA gene sepsis question (5), and non-gut samples (1). (b) Twelve analysis sets (A01-A06 and S01-S06) contributed to ICU-control discovery, healthy-control discovery, locked external evaluation, or supportive analyses. External data were not used for feature selection, coefficient refitting, or threshold optimization.'),
('Comparator-dependent community and genus differences.',
 '(a) PERMANOVA R² and Benjamini-Hochberg false discovery rate (FDR) in the four discovery analysis sets. (b,c) Random-effects genus meta-analyses comparing sepsis with ICU controls (A01, A02) and healthy controls (A03, A06). Points show pooled mean centered log-ratio (CLR) contrasts; horizontal lines show 95% confidence intervals. Positive values indicate higher abundance in sepsis and negative values indicate lower abundance. The dashed line marks no difference. Vermilion and blue indicate positive and negative contrasts, respectively. In a, circles and triangles denote ICU-control and healthy-control comparisons. In b, the six genera selected for the locked sepsis dysbiosis index (SDI) are emphasized; the other genera are faded. Only two of the six selected genera meet REML FDR <0.05; selection also considered directional and sensitivity evidence.'),
('Development and external evaluation of the six-genus SDI.',
 '(a) AUC estimates and 95% bootstrap confidence intervals for apparent development performance and locked external evaluation. The dashed line marks AUC = 0.5. Pooled development AUC was 0.731 (95% CI 0.656-0.806). External AUCs were 0.272 (0.105-0.451) for the primary trauma comparison (A05) and 0.201 (0.046-0.373) for the supportive healthy comparison (A04). (b) Genus effects oriented by the development-stage direction. Blue circles and vermilion triangles denote development and external estimates. Bilophila, Butyricimonas, Parabacteroides, and Hoylesella reversed direction. Campylobacter and Atopobium were absent at the external project level; no external effect is plotted for them. (c,d) SDI distributions for A04 (17 healthy, 18 sepsis samples) and A05 (18 trauma, 18 sepsis samples). Open gray and filled vermilion circles represent comparator and sepsis samples, respectively. Boxes show the median and interquartile range; whiskers extend to the most extreme observations within 1.5 interquartile ranges. A04 and A05 share the sepsis samples and originate from one BioProject. Score direction was retained during evaluation.'),
('Longitudinal microbiome trajectories.',
 '(a,b) Shannon diversity in PRJEB33360 and PRJNA691455. (c) Shannon diversity by cholestasis status in PRJNA912621; Blue circles with a solid line (No) and vermilion triangles with a dashed line (Yes) denote absence and presence of cholestasis. The paired counts on the axis are No / Yes at each timepoint. (d) Equal-weight SDI in PRJEB33360. Points and error bars show means and standard errors; sample counts appear on the axes. Open points and dashed segments identify descriptive discharge estimates (n = 3). In PRJEB33360, the day-7 Shannon coefficient was −0.772 (95% CI −1.144 to −0.401), and the omnibus time FDR was 0.0126. The corresponding PRJNA691455 time FDR was 0.318; the exploratory time-by-cholestasis interaction FDR was 0.266. Omnibus FDR families include all non-intercept terms across modeled metrics within each analysis ID.')]

titles=[
 'Public datasets and analytical roles.',
 'Community-level differences in discovery analysis sets.',
 'SDI development and external evaluation.',
 'Longitudinal Shannon diversity tests.',
 'Project-specific DADA2 filtering parameters.']
notes=[
 'A02/A03 and A04/A05 share sepsis participants. S01/S02 totals include baseline records. Observation counts must not be summed as unique patients. ICU, intensive care unit.',
 'PERMANOVA R² is reported as a proportion. FDR, Benjamini-Hochberg false discovery rate. Full alpha-diversity, dispersion and genus meta-analysis results are provided in Supplementary Data 1.',
 'Equal-weight SDI with the development direction retained. The fixed development threshold is −1.32849067. A05 and A04 are the primary and supportive external comparisons from one BioProject. CI, confidence interval.',
 'FDR families include all non-intercept omnibus terms across metrics within each analysis ID. The T01 day-7 coefficient was −0.772 (95% CI −1.144 to −0.401); this contrast is distinct from the omnibus test. Further supportive results are provided in Supplementary Data 1.',
 'All projects use maxN = 0, truncQ = 2 and rm.phix = TRUE. Paired values refer to forward and reverse reads. A truncLen of zero indicates no fixed-length truncation. Complete processing arguments are available in the archived scripts.']

def font(run,size=10,bold=False,italic=False):
 run.font.name='Arial';run.font.size=Pt(size);run.font.bold=bold;run.font.italic=italic;run.font.color.rgb=RGBColor.from_string('252A2D')
 rpr=run._element.get_or_add_rPr();rf=rpr.rFonts
 if rf is not None:
  for key in ['ascii','hAnsi','eastAsia','cs']:rf.set(qn('w:'+key),'Arial')

def para(d,text,size=10,bold=False,after=6,keep=False):
 p=d.add_paragraph();font(p.add_run(text),size,bold)
 p.paragraph_format.space_after=Pt(after);p.paragraph_format.space_before=Pt(0)
 p.paragraph_format.line_spacing=1.12;p.paragraph_format.keep_with_next=keep
 p.paragraph_format.widow_control=True
 return p

def caption(d,title,text):
 p=d.add_paragraph();font(p.add_run(title+' '),10,True);font(p.add_run(text),10)
 p.paragraph_format.line_spacing=1.08;p.paragraph_format.space_before=Pt(7);p.paragraph_format.space_after=Pt(0)
 p.paragraph_format.keep_together=True

def edge(parent,tag,**attrs):
 el=OxmlElement('w:'+tag)
 for k,v in attrs.items():el.set(qn('w:'+k),str(v))
 parent.append(el);return el

widths=[[.72,1.37,3.61,.95],[3.30,1.70,1.65],[3.45,1.0,2.20],[3.50,1.20,.90,1.05],[1.60,1.12,2.85,1.08]]

def add_table(d,i):
 para(d,f'Table {i+1}. {titles[i]}',10.5,True,after=7,keep=True)
 element=deepcopy(tables[i]);d._body._body.insert(len(d._body._body)-1,element)
 t=d.tables[-1];t.autofit=False;t.alignment=WD_TABLE_ALIGNMENT.CENTER
 if i==0:
  t.cell(0,3).text='Observations (n)'
 if i==3:t.cell(0,2).text='P value'
 pr=t._element.tblPr
 for child in list(pr):
  if child.tag in [qn('w:tblBorders'),qn('w:shd'),qn('w:tblStyle')]:pr.remove(child)
 borders=edge(pr,'tblBorders')
 for side in ['top','bottom']:edge(borders,side,val='single',sz=8,color='39454D')
 for side in ['left','right','insideH','insideV']:edge(borders,side,val='nil')
 mar=edge(pr,'tblCellMar')
 for side in ['top','bottom']:edge(mar,side,w=70,type='dxa')
 for side in ['left','right']:edge(mar,side,w=70,type='dxa')
 for j,w in enumerate(widths[i]):t.columns[j].width=Inches(w)
 for r,row in enumerate(t.rows):
  trpr=row._tr.get_or_add_trPr()
  for old in list(trpr):
   if old.tag==qn('w:trHeight'):trpr.remove(old)
  edge(trpr,'cantSplit')
  if r==0:edge(trpr,'tblHeader')
  for c,cell in enumerate(row.cells):
   cell.width=Inches(widths[i][c]);cell.vertical_alignment=WD_CELL_VERTICAL_ALIGNMENT.CENTER
   cp=cell._tc.get_or_add_tcPr()
   for old in list(cp):
    if old.tag in [qn('w:shd'),qn('w:tcBorders')]:cp.remove(old)
   if r==0:
    cb=edge(cp,'tcBorders');edge(cb,'bottom',val='single',sz=5,color='6D7982')
   for p in cell.paragraphs:
    p.paragraph_format.space_before=Pt(0);p.paragraph_format.space_after=Pt(0)
    p.paragraph_format.line_spacing=1.08;p.paragraph_format.keep_together=True
    p.paragraph_format.keep_with_next=r==0
    numeric=(i==0 and c==3)or(i in [1,2] and c>0)or(i==3 and c>=2)
    p.alignment=WD_ALIGN_PARAGRAPH.RIGHT if numeric and r>0 else WD_ALIGN_PARAGRAPH.LEFT
    for run in p.runs:font(run,9.2,r==0)
 note=para(d,notes[i],9,after=12)
 note.paragraph_format.space_before=Pt(5)

# Preserve the complete manuscript body, references and assistance disclosure.
last_reference=next(p for p in doc.paragraphs if p.text.startswith('53. NCBI BioProject PRJNA912621'))
body=doc._body._body;cut=False
for element in list(body):
 if cut and element.tag!=qn('w:sectPr'):body.remove(element)
 if element is last_reference._element:cut=True
for i,(title,text)in enumerate(legends):
 doc.add_page_break()
 p=doc.add_paragraph();p.alignment=WD_ALIGN_PARAGRAPH.CENTER
 p.paragraph_format.space_before=Pt(0);p.paragraph_format.space_after=Pt(0)
 p.paragraph_format.keep_with_next=True
 p.add_run().add_picture(str(FIG/f'Figure_{i+1}.png'),width=Inches(6.65))
 caption(doc,f'Figure {i+1}. {title}',text)
doc.add_page_break()
for i in range(5):
 if i==2:doc.add_page_break()
 add_table(doc,i)
doc.save(OUT/'Manuscript_V1.docx')

td=Document()
for s in td.sections:
 s.page_width=Inches(8.27);s.page_height=Inches(11.69)
 s.left_margin=s.right_margin=Inches(.81);s.top_margin=s.bottom_margin=Inches(.78)
for i in range(5):
 if i==2:td.add_page_break()
 add_table(td,i)
td.save(OUT/'Main_Tables.docx')

# Compose figure PDFs as vector page objects; retain reporting pages verbatim.
supp_legends=[
 ('Supplementary Figure S1. Characteristics of cross-sectional analysis sets.',
  '(a) Sample size and (b) median sequencing depth for A01-A06. Blue circles denote development analysis sets and vermilion triangles denote locked external evaluation. A02/A03 and A04/A05 share sepsis participants; counts must not be summed as unique patients. Sequencing-depth medians are shown without changing the frozen source values.'),
 ('Supplementary Figure S2. Cross-sectional alpha-diversity effects.',
  '(a) Observed genus richness, (b) Shannon diversity and (c) inverse Simpson diversity in A01, A02, A03 and A06. Points show mean sepsis-minus-comparator differences; lines show descriptive 95% confidence intervals. Both comparator families are blue; circles denote ICU controls and triangles denote healthy controls. Formal inference used Wilcoxon rank-sum tests with Benjamini-Hochberg FDR correction. The dashed line marks no difference.'),
 ('Supplementary Figure S3. Post hoc leave-one-genus-out SDI diagnosis.',
  '(a-d) Change in equal-weight SDI AUC after omission of each genus in A01, A02, A04 and A05. Positive ΔAUC indicates improved discrimination after omission. Blue circles denote development and vermilion triangles denote external evaluation. Campylobacter and Atopobium were absent at the external project level and are labeled Absent rather than assigned zero effects. All panels use a common axis and genus order. These sensitivity analyses are post hoc diagnostics, not independent validation.')]
W,H=595.276,841.89
style=ParagraphStyle('caption',fontName='Helvetica',fontSize=10,leading=14,textColor='#252A2D',alignment=TA_LEFT)
supp=fitz.open()
for i,(title,text)in enumerate(supp_legends):
 overlay=BytesIO();c=canvas.Canvas(overlay,pagesize=(W,H))
 y=H-50
 if i==0:
  c.setFont('Helvetica-Bold',16);c.drawString(48,y,'Supplementary Information');y-=25
  p=Paragraph('Context dependent sepsis gut microbiota signatures and external failure of a locked dysbiosis index',style)
  _,ph=p.wrap(W-96,80);p.drawOn(c,48,y-ph);y-=ph+28
 else:
  c.setFont('Helvetica-Bold',11);c.drawString(48,y,f'Supplementary Figure S{i+1}');y-=28
 figure=fitz.open(FIG/f'Supplementary_Figure_S{i+1}.pdf')
 fw=W-96;fh=fw*figure[0].rect.height/figure[0].rect.width
 top=H-y
 cap=Paragraph('<b>'+title+'</b> '+text,style)
 _,ch=cap.wrap(W-96,240);cap.drawOn(c,48,y-fh-20-ch)
 c.setFont('Helvetica',9);c.drawRightString(W-48,27,f'S{i+1}');c.save()
 layer=fitz.open(stream=overlay.getvalue(),filetype='pdf')
 page=supp.new_page(width=W,height=H)
 page.show_pdf_page(page.rect,layer,0)
 page.show_pdf_page(fitz.Rect(48,top,W-48,top+fh),figure,0)
old=fitz.open(SRC/'Original_Supplement.pdf')
supp.insert_pdf(old,from_page=4,to_page=5)
# Continue page labels without altering the reporting text.
for i in [3,4]:
 page=supp[i]
 for r in page.search_for('S'+str(i+2)):
  if r.y0>page.rect.height-55:page.add_redact_annot(r,fill=(1,1,1))
 page.apply_redactions()
 page.insert_text((W-60,H-27),f'S{i+1}',fontname='helv',fontsize=9)
supp.save(OUT/'Supplementary_Information.pdf',garbage=4,deflate=True)
(OUT/'revised_legends.json').write_text(json.dumps({'main':legends,'supplementary':supp_legends,'tables':list(zip(titles,notes))},ensure_ascii=False,indent=2),encoding='utf-8')
preserved=[p.text for p in doc.paragraphs[:len(original_paragraphs)] if p.text]
print('Saved manuscript, main tables and supplementary PDF.')
