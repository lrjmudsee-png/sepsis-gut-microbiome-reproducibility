import fs from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {FileBlob,SpreadsheetFile} from '@oai/artifact-tool';
const here=path.dirname(fileURLToPath(import.meta.url));
const [srcArg,inventoryArg,outArg]=process.argv.slice(2);
if(!srcArg||!inventoryArg||!outArg)throw new Error('Usage: node style_workbook.mjs input.xlsx inventory.json output.xlsx');
const src=path.resolve(srcArg),out=path.resolve(outArg);
const qa=path.join(path.dirname(out),'workbook_QA');
await fs.mkdir(qa,{recursive:true});
const data=JSON.parse(await fs.readFile(path.resolve(inventoryArg),'utf8')).workbook;
const wb=await SpreadsheetFile.importXlsx(await FileBlob.load(src));
console.log((await wb.inspect({kind:'sheet',include:'id,name',maxChars:2500})).ndjson);
const before=await wb.render({sheetName:'S8_SDI_Performance',range:'A1:H12',scale:1.3,format:'png'});
await fs.writeFile(path.join(qa,'workbook_before.png'),new Uint8Array(await before.arrayBuffer()));
if(process.argv.includes('--inspect'))process.exit(0);
const col=(i)=>{let s='';for(;i;i=Math.floor((i-1)/26))s=String.fromCharCode(65+(i-1)%26)+s;return s;};
for(const [name,s]of Object.entries(data)){
 const sheet=wb.worksheets.getItem(name),last=col(s.cols),range=sheet.getRange(`A1:${last}${s.rows}`);
 range.format.font={name:'Arial',size:10,color:'#252A2D'};
 range.format.fill='#FFFFFF';range.format.borders={preset:'none'};
 range.format.rowHeight=18;range.format.verticalAlignment='center';range.format.wrapText=false;
 sheet.showGridLines=false;
 for(const merged of s.merged){
  if(/^A[12]:/.test(merged))sheet.unmergeCells(merged);
 }
 const titleEnd=col(Math.min(s.cols,8));
 sheet.mergeCells(`A1:${titleEnd}1`);
 if(s.values[1]?.[0])sheet.mergeCells(`A2:${titleEnd}2`);
 for(let c=0;c<s.cols;c++){
  const vals=s.values.map(r=>r[c]).filter(v=>v!==null&&v!==undefined);
  const text=vals.filter(v=>typeof v==='string');
  const longest=Math.max(8,...text.map(v=>v.length));
  const width=Math.max(13,Math.min(44,longest+2));
  const cr=sheet.getRange(`${col(c+1)}1:${col(c+1)}${s.rows}`);
  cr.format.columnWidth=width;
  cr.format.horizontalAlignment=text.length>vals.length*.65?'left':'right';
 }
 sheet.getRange(`A1:${last}1`).format.font={name:'Arial',size:13,bold:true,color:'#252A2D'};
 sheet.getRange(`A1:${last}1`).format.rowHeight=27;
 if(s.values[1]?.[0]){
  sheet.getRange(`A2:${last}2`).format.font={name:'Arial',size:10,italic:true,color:'#59636B'};
  sheet.getRange(`A2:${last}2`).format.wrapText=true;
  sheet.getRange(`A2:${last}2`).format.rowHeight=34;
 }
 // Section captions are followed by their machine-readable field headers.
 for(let i=3;i<s.rows;i++){
  const row=s.values[i],used=row.filter(v=>v!==null&&v!==undefined&&v!=='');
  if(used.length===1&&typeof row[0]==='string'&&i+1<s.rows){
   const section=sheet.getRange(`A${i+1}:${last}${i+1}`);
   section.format.fill='#F1F3F4';section.format.font={name:'Arial',size:10,bold:true,color:'#252A2D'};
   section.format.rowHeight=23;
   let nextIndex=i+1;
   while(nextIndex<Math.min(i+4,s.rows)&&s.values[nextIndex].every(v=>v===null||v===undefined||v===''))nextIndex++;
   const next=s.values[nextIndex]??[],n=next.filter(v=>v!==null&&v!==undefined&&v!=='').length;
   if(n>1){
    const head=sheet.getRange(`A${nextIndex+1}:${last}${nextIndex+1}`);
    head.format.fill='#FFFFFF';head.format.font={name:'Arial',size:10,bold:true,color:'#252A2D'};
    head.format.borders={top:{style:'thin',color:'#4E5C66'},bottom:{style:'thin',color:'#4E5C66'}};
    head.format.horizontalAlignment='center';head.format.wrapText=true;head.format.rowHeight=44;
   }
  }
 }
 if(name==='00_INDEX'){
  sheet.getRange('A1').values=[['Supplementary Data 1. Numerical results']];
  sheet.getRange('A2').values=[['Cohort characteristics, statistical analyses and SDI evaluation (Tables S1-S15).']];
  sheet.getRange('A1:C2').format.wrapText=true;
  sheet.getRange('A4:C4').format.borders={top:{style:'thin',color:'#4E5C66'},bottom:{style:'thin',color:'#4E5C66'}};
  sheet.getRange('A4:C4').format.font={name:'Arial',size:10,bold:true};
  sheet.getRange('A1:A40').format.columnWidth=29;sheet.getRange('B1:B40').format.columnWidth=57;sheet.getRange('C1:C40').format.columnWidth=75;
  sheet.getRange('B5:C40').format.wrapText=true;sheet.getRange('A5:C40').format.rowHeight=32;
 }else{
  sheet.freezePanes.freezeRows(6);sheet.freezePanes.freezeColumns(1);
 }
 // Fit wrapped scientific fields without reducing font size.
 for(let i=5;i<s.rows;i++){
  let lines=1;
  for(let c=0;c<s.cols;c++){
   const v=s.values[i][c];if(typeof v!=='string'||!v)continue;
   const maxWidth=Math.max(13,Math.min(44,Math.max(8,...s.values.map(r=>typeof r[c]==='string'?r[c].length:0))+2));
   if(v.length>maxWidth){sheet.getRange(`${col(c+1)}${i+1}`).format.wrapText=true;lines=Math.max(lines,Math.ceil(v.length/(maxWidth-2)));}
  }
  if(lines>1)sheet.getRange(`A${i+1}:${last}${i+1}`).format.rowHeight=Math.min(96,lines*14+6);
 }
}
wb.recalculate();
console.log((await wb.inspect({kind:'match',searchTerm:'#REF!|#DIV/0!|#VALUE!|#NAME\\?|#NUM!|#SPILL!',options:{useRegex:true,maxResults:20},maxChars:1500})).ndjson);
for(const name of Object.keys(data)){
 const blob=await wb.render({sheetName:name,range:`A1:${col(Math.min(data[name].cols,8))}${Math.min(data[name].rows,14)}`,scale:1.2,format:'png'});
 await fs.writeFile(path.join(qa,`xlsx_${name}.png`),new Uint8Array(await blob.arrayBuffer()));
}
await(await SpreadsheetFile.exportXlsx(wb)).save(out);
console.log('WORKBOOK EXPORTED');
