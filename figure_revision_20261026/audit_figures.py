"""Audit all seven exported figures and fail if any output or check is missing."""
from pathlib import Path
import argparse
import json
import subprocess
import sys

HERE = Path(__file__).resolve().parent
FIGURES = [f'Figure_{i}' for i in range(1, 5)] + [f'Supplementary_Figure_S{i}' for i in range(1, 4)]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('output', nargs='?', type=Path, default=HERE/'output')
options = parser.parse_args()
output = options.output.resolve()
qa = output/'QA'
qa.mkdir(parents=True, exist_ok=True)
rows = []
for name in FIGURES:
    pdf = output/f'{name}.pdf'
    if not pdf.is_file():
        rows.append({'figure': name, 'check': 'output_exists', 'code': 1})
        continue
    for script, args in [
        ('audit_pdf_text.py', ['--min-pt', '5', '--json']),
        ('audit_figure_collisions.py', ['--json-out', str(qa/f'{name}.collisions.json'),
                                     '--overlay-pdf', str(qa/f'{name}.collisions.pdf')]),
    ]:
        result = subprocess.run([sys.executable, str(HERE/script), str(pdf), *args],
                                capture_output=True, text=True, encoding='utf-8', errors='replace')
        (qa/f'{name}.{script}.log').write_text(result.stdout+result.stderr, encoding='utf-8')
        print(name, script, result.returncode)
        rows.append({'figure': name, 'check': script, 'code': result.returncode})
(qa/'figure_audit_summary.json').write_text(json.dumps(rows, indent=2)+'\n', encoding='utf-8')
raise SystemExit(0 if len(rows) == 14 and all(row['code'] == 0 for row in rows) else 1)
