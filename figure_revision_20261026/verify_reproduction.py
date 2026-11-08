"""Compare the rerun with reviewed scientific inputs, figure text and table cells."""
from pathlib import Path
import argparse
import csv
import hashlib
import json
import sys
import fitz
from docx import Document

HERE = Path(__file__).resolve().parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('output', nargs='?', type=Path, default=HERE/'output')
parser.add_argument('--strict-raster', action='store_true', help='Also require identical PNG bytes in the recorded environment.')
args = parser.parse_args()
output = args.output.resolve()
checks = []
sha256 = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
def record(label, passed):
    checks.append({'check': label, 'passed': bool(passed)})

for row in json.loads((HERE/'input_provenance.json').read_text(encoding='utf-8')):
    path = HERE/'frozen_inputs'/row['path']
    record('input: '+row['path'], path.is_file() and sha256(path) == row['sha256'])
references = json.loads((HERE/'reference_artifacts.json').read_text(encoding='utf-8'))
raster = []
for name, info in references['figures'].items():
    pdf = output/f'{name}.pdf'
    if pdf.is_file():
        with fitz.open(pdf) as document:
            text = ' '.join(' '.join(page.get_text() for page in document).split())
        text_hash = hashlib.sha256(text.encode('utf-8')).hexdigest()
    else:
        text_hash = None
    record('PDF text: '+name, text_hash == info['pdf_text_sha256'])
    png = output/f'{name}.png'
    matches = png.is_file() and sha256(png) == info['png_sha256']
    raster.append({'figure': name, 'exact_png_match': matches})
    if args.strict_raster:
        record('PNG bytes: '+name, matches)
    alignment = output/'QA'/f'{name}.alignment.json'
    report = json.loads(alignment.read_text(encoding='utf-8')) if alignment.is_file() else {}
    record('panel alignment: '+name, report.get('verdict') == 'PASS')
audit = output/'QA'/'figure_audit_summary.json'
rows = json.loads(audit.read_text(encoding='utf-8')) if audit.is_file() else []
record('all 14 PDF font/collision audits', len(rows) == 14 and all(row['code'] == 0 for row in rows))
source_audit = output/'PUBLIC_SuppFigure_Source_Audit.csv'
rows = list(csv.DictReader(source_audit.open(encoding='utf-8-sig', newline=''))) if source_audit.is_file() else []
record('all 17 supplementary source assertions', len(rows) == 17 and all(row['Status'] == 'PASS' for row in rows))
tables = output/'Main_Tables.docx'
actual = [[[cell.text for cell in row.cells] for row in table.rows] for table in Document(tables).tables] if tables.is_file() else []
record('all five main-table cell matrices', actual == references['main_table_cells'])
result = {'all_checks_pass': all(c['passed'] for c in checks), 'checks': checks,
          'raster_comparison': raster, 'strict_raster': args.strict_raster}
(output/'QA').mkdir(parents=True, exist_ok=True)
(output/'QA'/'reproduction_summary.json').write_text(json.dumps(result, indent=2)+'\n', encoding='utf-8')
print(json.dumps({'all_checks_pass': result['all_checks_pass'], 'checks': len(checks),
                  'exact_png_matches': sum(r['exact_png_match'] for r in raster)}, indent=2))
sys.exit(0 if result['all_checks_pass'] else 1)
