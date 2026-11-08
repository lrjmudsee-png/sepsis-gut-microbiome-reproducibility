from pathlib import Path
from copy import deepcopy
import argparse,json
from docx import Document
from docx.shared import Inches,Pt,RGBColor
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT,WD_CELL_VERTICAL_ALIGNMENT

HERE=Path(__file__).resolve().parent
parser=argparse.ArgumentParser(description='Reproduce the five reviewed main tables.')
parser.add_argument('--output',type=Path,default=HERE/'output'/'Main_Tables.docx')
options=parser.parse_args()
data=json.loads((HERE/'table_sources'/'main_tables.json').read_text(encoding='utf-8'))
titles=data['titles'];notes=data['notes'];tables=[]
for rows in data['tables']:
    temporary=Document()
    table=temporary.add_table(rows=len(rows),cols=len(rows[0]))
    for row,values in zip(table.rows,rows):
        for cell,value in zip(row.cells,values):cell.text=value
    tables.append(deepcopy(table._element))

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

td=Document()
for section in td.sections:
    section.page_width=Inches(8.27);section.page_height=Inches(11.69)
    section.left_margin=section.right_margin=Inches(.81)
    section.top_margin=section.bottom_margin=Inches(.78)
for i in range(5):
    if i==2:td.add_page_break()
    add_table(td,i)
options.output.parent.mkdir(parents=True,exist_ok=True)
td.save(options.output)
print(options.output)
