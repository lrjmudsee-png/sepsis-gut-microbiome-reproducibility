from pathlib import Path
import argparse,json
from openpyxl import load_workbook
parser=argparse.ArgumentParser(description='Read workbook cells and merged ranges without modifying the workbook.')
parser.add_argument('workbook',type=Path)
parser.add_argument('output',type=Path)
args=parser.parse_args()
book=load_workbook(args.workbook,data_only=False)
data={'workbook':{sheet.title:{'rows':sheet.max_row,'cols':sheet.max_column,
    'values':list(sheet.values),'merged':[str(r) for r in sheet.merged_cells.ranges]}
    for sheet in book}}
args.output.parent.mkdir(parents=True,exist_ok=True)
args.output.write_text(json.dumps(data,ensure_ascii=False,indent=2,default=str),encoding='utf-8')
