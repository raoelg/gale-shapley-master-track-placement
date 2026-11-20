// compare.js -- run app.js headlessly on every cases/*/config_*.json and compare with reference.R output
// usage: node compare.js [casesDir] [collation=locale|C]
const fs = require('fs'), path = require('path');
const XLSX = require('xlsx');
const OMTP = require('./app.js');
const casesDir = process.argv[2] || path.join(__dirname, 'cases');
const collation = process.argv[3] || 'locale';

const rd = (dir, name) => { const f = path.join(dir, name + '.json'); return fs.existsSync(f) ? JSON.parse(fs.readFileSync(f, 'utf8')) : undefined; };
const eqv = (a, b) => {
  if ((a === null || a === undefined) && (b === null || b === undefined)) return true;
  if (typeof a === 'number' && typeof b === 'number') return a === b || Math.abs(a - b) <= 1e-9 * Math.max(1, Math.abs(a));
  return a === b;
};
// R data frame json {names, data: {col: [...]}} vs JS {names, rows}
function cmpDF(r, j, label, diffs) {
  if (r === null || r === undefined) { if (j) diffs.push(label + ': R has no table, JS has one'); return; }
  if (!j) { diffs.push(label + ': JS has no table'); return; }
  if (JSON.stringify(r.names) !== JSON.stringify(j.names)) { diffs.push(label + ': names differ R=' + JSON.stringify(r.names) + ' JS=' + JSON.stringify(j.names)); return; }
  const nR = r.names.length && r.data[r.names[0]] ? r.data[r.names[0]].length : 0;
  if (nR !== j.rows.length) { diffs.push(label + ': nrow R=' + nR + ' JS=' + j.rows.length); return; }
  let nd = 0;
  r.names.forEach((nm, c) => { for (let i = 0; i < nR; i++) if (!eqv(r.data[nm][i], j.rows[i][c])) { if (nd++ < 5) diffs.push(label + `: [${i + 1}, ${nm}] R=${JSON.stringify(r.data[nm][i])} JS=${JSON.stringify(j.rows[i][c])}`); } });
  if (nd > 5) diffs.push(label + `: ... ${nd} cell differences`);
}
function cmpMat(r, j, label, diffs) {
  if (!r) { if (j) diffs.push(label + ': only JS'); return; }
  if (JSON.stringify(r.colnames) !== JSON.stringify(j.colnames)) { diffs.push(label + ': colnames differ'); return; }
  if (r.nrow[0] !== j.data.length) { diffs.push(label + `: nrow R=${r.nrow[0]} JS=${j.data.length}`); return; }
  let nd = 0;
  r.data.forEach((row, i) => row.forEach((v, c) => { if (!eqv(v, j.data[i][c]) && nd++ < 5) diffs.push(label + `: [${i + 1},${c + 1}] R=${v} JS=${j.data[i][c]}`); }));
}
function cmpAdm(r, j, label, diffs) {
  if (!r) { if (j) diffs.push(label + ': only JS'); return; }
  if (!j) { diffs.push(label + ': only R'); return; }
  const rh = [].concat(r.headers || []), jh = j.map(g => g.header);
  if (JSON.stringify(rh) !== JSON.stringify(jh)) diffs.push(label + ': headers R=' + JSON.stringify(rh) + ' JS=' + JSON.stringify(jh));
  const rl = r.lists || [];
  j.forEach((g, i) => { if (JSON.stringify([].concat(rl[i] || [])) !== JSON.stringify(g.students)) diffs.push(label + ': list ' + g.header + ' differs'); });
}
function cmpBytes(rFile, jStr, label, diffs) {
  const has = fs.existsSync(rFile);
  if (!has) { if (jStr !== undefined) diffs.push(label + ': only JS (R failed)'); return; }
  if (jStr === undefined) { diffs.push(label + ': only R'); return; }
  const rb = fs.readFileSync(rFile), jb = Buffer.from(OMTP.utf16le(jStr));
  if (!rb.equals(jb)) {
    const rs = rb.toString('utf16le').split('\n'), js = jStr.split('\n');
    const k = rs.findIndex((l, i) => l !== js[i]);
    diffs.push(label + `: bytes differ (R ${rb.length} B, JS ${jb.length} B); first differing line ${k + 1}:\n   R : ${rs[k]}\n   JS: ${js[k]}`);
  }
}

const results = [];
for (const cs of fs.readdirSync(casesDir).sort()) {
  const dir = path.join(casesDir, cs);
  for (const cf of fs.readdirSync(dir).filter(f => /^config_\d+\.json$/.test(f)).sort()) {
    const cfg = JSON.parse(fs.readFileSync(path.join(dir, cf), 'utf8'));
    const rdir = cfg.outdir;
    const diffs = [], checked = [];
    const isX = /\.xlsx?$/.test(cfg.appfile);
    const appData = isX ? fs.readFileSync(cfg.appfile) : fs.readFileSync(cfg.appfile, 'utf8');
    const dat0 = OMTP.readApplications(cfg.appfile, appData, cfg.header1 !== false, XLSX);
    const guess = OMTP.guessMapping(dat0.names);
    const rg = rd(rdir, 'guess');
    for (const k of Object.keys(guess)) if ([].concat(rg[k])[0] !== guess[k]) diffs.push(`guess.${k}: R=${rg[k]} JS=${guess[k]}`);
    checked.push('guess');
    const mapping = {};
    for (const k of ['student', 'track', 'preference', 'qualification']) mapping[k] = cfg[k] !== undefined ? cfg[k] : guess[k];
    let quota = null;
    if (cfg.quotafile) {
      const qx = /\.xlsx?$/.test(cfg.quotafile);
      quota = OMTP.readQuota(cfg.quotafile, qx ? fs.readFileSync(cfg.quotafile) : fs.readFileSync(cfg.quotafile, 'utf8'), cfg.header3 !== false, XLSX);
    }
    const out = OMTP.runAll({
      dat0, quota, mapping, removeCols: cfg.remove_cols || [], qlevels: cfg.qlevels || 'E, V, O', seed: cfg.seed,
      sampleKind: cfg.samplekind, collation, withdrawrate: cfg.withdrawrate || 10, waitlist: cfg.waitlist !== false
    });
    cmpDF(rd(rdir, 'dat0'), dat0, 'dat0', diffs); checked.push('dat0');
    cmpDF(rd(rdir, 'contents0'), out.contents0, 'dat2/contents0', diffs); checked.push('dat2');
    if (rd(rdir, 'CP')) { cmpMat(rd(rdir, 'CP'), out.CP, 'CP', diffs); cmpMat(rd(rdir, 'SP'), out.SP, 'SP', diffs); checked.push('CP', 'SP');
      if ([].concat(rd(rdir, 'nduplicates'))[0] !== out.nduplicates) diffs.push('nduplicates differ'); }
    const c3 = rd(rdir, 'contents3');
    if (c3 !== undefined && c3 !== null) {
      cmpDF(c3, out.contents3 && out.contents3.table, 'contents3', diffs); checked.push('quota table');
    }
    cmpDF(rd(rdir, 'Appl'), out.Appl, 'Appl', diffs);
    cmpDF(rd(rdir, 'result'), out.result, 'result', diffs);
    cmpDF(rd(rdir, 'details'), out.details, 'details', diffs);
    if (out.Appl) checked.push('Appl', 'result', 'details');
    const sb = rd(rdir, 'summary_bar'), sp = rd(rdir, 'summary_pie');
    if (sb && !out.summary) diffs.push("summary missing in JS"); else if (sb) {
      if (JSON.stringify(sb.data.studpref) !== JSON.stringify(out.summary.ranks) || JSON.stringify(sb.data.n) !== JSON.stringify(out.summary.counts)) diffs.push('summary bar differs');
      if (sp.data.percentage.some((p, i) => !eqv(p, out.summary.percentages[i]))) diffs.push('summary pie differs');
      checked.push('plots');
    }
    cmpAdm(rd(rdir, 'admissions'), out.admissions, 'admissions', diffs);
    cmpBytes(path.join(rdir, 'download.csv'), out.download, 'download.csv', diffs);
    if (out.download !== undefined || fs.existsSync(path.join(rdir, 'download.csv'))) checked.push('download');
    const rs = rd(rdir, 'rng_before_waitlist');
    if (rs && !out.rngAfterLottery) diffs.push("JS has no RNG state (no placement)"); else if (rs) { if (rs.slice(1).some((v, i) => v !== out.rngAfterLottery[i + 1])) diffs.push('RNG state after lottery differs'); checked.push('RNG state'); }
    const rw = rd(rdir, 'waitlist');
    if (rw || out.waitlist) {
      cmpDF(rw, out.waitlist, 'waitlist', diffs);
      cmpAdm(rd(rdir, 'admissions_wl'), out.admissions_wl, 'admissions_wl', diffs);
      cmpBytes(path.join(rdir, 'download_wl.csv'), out.download_wl, 'download_wl.csv', diffs);
      checked.push('waitlist');
    }
    const rerr = rd(rdir, 'errors') || {};
    const rkeys = Array.isArray(rerr) ? [] : Object.keys(rerr), jkeys = Object.keys(out.errors);
    results.push({ case: cs + '/' + cf.replace('.json', ''), seed: cfg.seed, kind: cfg.samplekind, checked, diffs, rerr, jerr: out.errors });
  }
}
let fails = 0;
for (const r of results) {
  const ok = r.diffs.length === 0;
  if (!ok) fails++;
  console.log(`${ok ? 'PASS' : 'FAIL'}  ${r.case}  seed=${r.seed} ${r.kind}  [${r.checked.join(', ')}]`);
  if (Object.keys(r.rerr).length || Object.keys(r.jerr).length) console.log('      R errors: ' + JSON.stringify(r.rerr) + '  JS errors: ' + JSON.stringify(r.jerr));
  r.diffs.forEach(d => console.log('      ' + d));
}
console.log(`\n${results.length - fails}/${results.length} configurations identical`);
