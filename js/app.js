/*
 * Optimal Master Track Placement -- browser port of the R Shiny app (app.R + GaleShapely.R).
 *
 * Plain ES2018, no build step. The computational core (OMTP.*) has no DOM dependencies and can be
 * loaded in Node with require('./app.js'); the UI part only runs when a `document` exists.
 *
 * Reproducibility notes (see also the tooltips in index.html):
 *  - R's RNG is re-implemented exactly: Mersenne-Twister with R's set.seed() scrambling
 *    (src/main/RNG.c), unif_rand() with fixup(), and R_unif_index() for sample() in both
 *    sample.kind = "Rejection" (R >= 3.6.0) and "Rounding" (R < 3.6.0, e.g. R 3.4 in 2018).
 *  - The lottery draws sample(n()) per (College, Collegepref) group in the order dplyr (>= 1.1)
 *    visits groups: College sorted in the C locale (code point order), Collegepref by factor level,
 *    NA (= students who did not apply / are unqualified for that track) last.
 *  - tidyr::spread() orders its rows/columns with base sort(), i.e. by the R session's collation
 *    locale. Default here: "locale" = ICU root/en-US collation (Intl.Collator), which equals R in an
 *    en_US.UTF-8 session with ICU. Choose "C" to reproduce an R session running in the C locale.
 */
(function () {
  'use strict';

  // ======================================================================================
  // R random number generator (Mersenne-Twister, set.seed, unif_rand, R_unif_index, sample)
  // ======================================================================================
  const N = 624, M = 397;
  const MATRIX_A = 0x9908b0df, UPPER_MASK = 0x80000000, LOWER_MASK = 0x7fffffff;
  const TEMPERING_MASK_B = 0x9d2c5680, TEMPERING_MASK_C = 0xefc60000;
  const i2_32m1 = 2.328306437080797e-10; // 1/(2^32 - 1)

  class RRNG {
    constructor(seed, sampleKind) {
      this.mt = new Uint32Array(N);
      this.mti = N + 1;
      this.sampleKind = sampleKind === 'Rounding' ? 'Rounding' : 'Rejection';
      if (seed !== undefined && seed !== null) this.setSeed(seed);
    }
    /** set.seed(seed) for kind "Mersenne-Twister" (RNG_Init in src/main/RNG.c) */
    setSeed(seed) {
      seed = Number(seed);
      if (!isFinite(seed) || Math.abs(seed) > 2147483647) throw new Error('supplied seed is not a valid integer');
      let s = (Math.trunc(seed) | 0) >>> 0;              // asInteger(), then (Int32) = unsigned int
      for (let j = 0; j < 50; j++) s = (Math.imul(69069, s) + 1) >>> 0; // initial scrambling
      s = (Math.imul(69069, s) + 1) >>> 0;                // i_seed[0] (dummy[0] = mti) ...
      for (let j = 0; j < N; j++) { s = (Math.imul(69069, s) + 1) >>> 0; this.mt[j] = s; }
      this.mti = N;                                       // ... which FixupSeeds() sets to 624
    }
    /** Restore from R's .Random.seed (integer vector: 10403, mti, mt[0..623]) */
    setState(rs) {
      if (rs.length !== N + 2) throw new Error('.Random.seed must have length 626');
      this.mti = rs[1] | 0;
      for (let j = 0; j < N; j++) this.mt[j] = rs[j + 2] >>> 0;
    }
    getState() {
      const out = [10403, this.mti];
      for (let j = 0; j < N; j++) out.push(this.mt[j] | 0);
      return out;
    }
    sgenrand(seed) {
      const mt = this.mt;
      for (let i = 0; i < N; i++) {
        mt[i] = seed & 0xffff0000;
        seed = (Math.imul(69069, seed) + 1) >>> 0;
        mt[i] = (mt[i] | ((seed & 0xffff0000) >>> 16)) >>> 0;
        seed = (Math.imul(69069, seed) + 1) >>> 0;
      }
      this.mti = N;
    }
    genrand() {
      const mt = this.mt;
      let y;
      if (this.mti >= N) {
        if (this.mti === N + 1) this.sgenrand(4357);
        let kk;
        for (kk = 0; kk < N - M; kk++) {
          y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
          mt[kk] = mt[kk + M] ^ (y >>> 1) ^ ((y & 1) ? MATRIX_A : 0);
        }
        for (; kk < N - 1; kk++) {
          y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
          mt[kk] = mt[kk + (M - N)] ^ (y >>> 1) ^ ((y & 1) ? MATRIX_A : 0);
        }
        y = (mt[N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
        mt[N - 1] = mt[M - 1] ^ (y >>> 1) ^ ((y & 1) ? MATRIX_A : 0);
        this.mti = 0;
      }
      y = mt[this.mti++];
      y ^= (y >>> 11);
      y ^= (y << 7) & TEMPERING_MASK_B;
      y ^= (y << 15) & TEMPERING_MASK_C;
      y ^= (y >>> 18);
      return (y >>> 0) * 2.3283064365386963e-10;
    }
    unifRand() {
      const x = this.genrand();
      if (x <= 0.0) return 0.5 * i2_32m1;
      if (1.0 - x <= 0.0) return 1.0 - 0.5 * i2_32m1;
      return x;
    }
    rbits(bits) {
      let v = 0;
      for (let n = 0; n <= bits; n += 16) {
        const v1 = Math.floor(this.unifRand() * 65536);
        v = 65536 * v + v1;
      }
      return v % Math.pow(2, bits); // == v & ((1 << bits) - 1) for the 64-bit v in R
    }
    /** R_unif_index(dn): uniform integer in 0..dn-1 */
    unifIndex(dn) {
      if (this.sampleKind === 'Rounding') return Math.floor(dn * this.unifRand());
      if (dn <= 0) return 0;
      let bits = 0;
      while (Math.pow(2, bits) < dn) bits++; // (int) ceil(log2(dn))
      let dv;
      do { dv = this.rbits(bits); } while (dn <= dv);
      return dv;
    }
    /** sample.int(n, size) without replacement (do_sample in src/main/random.c); 1-based result */
    sampleInt(n, size) {
      if (size === undefined) size = n;
      if (size > n) throw new Error('cannot take a sample larger than the population when \'replace = FALSE\'');
      const y = new Array(size);
      if (size < 2) {
        for (let i = 0; i < size; i++) y[i] = this.unifIndex(n) + 1;
        return y;
      }
      const x = new Array(n);
      for (let i = 0; i < n; i++) x[i] = i;
      let nn = n;
      for (let i = 0; i < size; i++) {
        const j = this.unifIndex(nn);
        y[i] = x[j] + 1;
        x[j] = x[--nn];
      }
      return y;
    }
  }

  // ======================================================================================
  // Small R-semantics helpers
  // ======================================================================================
  const isNA = v => v === null || v === undefined || (typeof v === 'number' && isNaN(v));
  const trimR = s => (s === null || s === undefined) ? s : String(s).replace(/^[\t\r\n ]+/, '').replace(/[\t\r\n ]+$/, '');
  const keyOf = v => isNA(v) ? '\u0000NA' : 's' + v; // map key with NA == NA (dplyr join/group semantics)

  function cCompare(a, b) { // C-locale (code point) comparison, as dplyr >= 1.1 / vctrs uses for strings
    if (a === b) return 0;
    const n = Math.min(a.length, b.length);
    for (let i = 0; i < n; i++) {
      const ca = a.codePointAt(i), cb = b.codePointAt(i);
      if (ca !== cb) return ca < cb ? -1 : 1;
      if (ca > 0xffff) i++;
    }
    return a.length < b.length ? -1 : (a.length > b.length ? 1 : 0);
  }
  let _collator = null;
  function makeCollate(collation) {
    if (collation === 'C') return cCompare;
    if (!_collator) _collator = new Intl.Collator('en-US');
    return (a, b) => { const r = _collator.compare(a, b); return r !== 0 ? r : cCompare(a, b); };
  }
  const naLast = cmp => (a, b) => (isNA(a) ? (isNA(b) ? 0 : 1) : (isNA(b) ? -1 : cmp(a, b)));
  function stableSortIdx(n, cmp) { // returns permutation, stable
    const idx = Array.from({ length: n }, (_, i) => i);
    idx.sort((i, j) => cmp(i, j) || (i - j));
    return idx;
  }
  /** sort(unique(x), na.last = TRUE) */
  function sortUnique(x, collate) {
    const seen = new Set(), u = [];
    let hasNA = false;
    for (const v of x) {
      if (isNA(v)) { hasNA = true; continue; }
      if (!seen.has(v)) { seen.add(v); u.push(v); }
    }
    u.sort(typeof u[0] === 'number' ? (a, b) => a - b : collate);
    if (hasNA) u.push(null);
    return u;
  }
  /** as.numeric() on a character value */
  function asNumeric(s) {
    if (isNA(s)) return null;
    if (typeof s === 'number') return s;
    const t = trimR(s);
    if (/^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/.test(t)) return parseFloat(t);
    if (/^[+-]?0[xX][0-9a-fA-F]+$/.test(t)) return (t[0] === '-' ? -1 : 1) * parseInt(t.replace(/^[+-]/, ''), 16);
    if (/^[+-]?(inf|infinity)$/i.test(t)) return t[0] === '-' ? -Infinity : Infinity;
    return null; // includes "NA", "NaN" (treated as missing, sorted last as in dplyr::arrange)
  }
  /** as.integer() (truncation) */
  function asInteger(v) {
    const x = asNumeric(v);
    if (x === null || !isFinite(x) || Math.abs(x) > 2147483647) return null;
    return Math.trunc(x);
  }
  /** C's %.15g, which readxl uses to turn numeric cells into text */
  function fmtG15(x) {
    if (!isFinite(x)) return x > 0 ? 'inf' : (x < 0 ? '-inf' : 'nan');
    if (x === 0) return '0';
    let s = x.toExponential(14); // d.dddddddddddddde±x
    let [mant, ex] = s.split('e');
    const e = parseInt(ex, 10);
    if (e < -4 || e >= 15) {
      mant = mant.replace(/\.?0+$/, '');
      return mant + 'e' + (e < 0 ? '-' : '+') + (Math.abs(e) < 10 ? '0' : '') + Math.abs(e);
    }
    let f = x.toFixed(Math.max(0, 14 - e));
    if (f.indexOf('.') >= 0) f = f.replace(/\.?0+$/, '');
    return f;
  }
  /** as.character()/write.table formatting of an R double (15 significant digits, scipen = 0) */
  function fmtDouble(x) {
    if (isNA(x)) return 'NA';
    if (!isFinite(x)) return x > 0 ? 'Inf' : '-Inf';
    if (x === 0) return '0';
    const p = Number(x.toPrecision(15));
    let sci = p.toExponential().replace(/e([+-])(\d)$/, 'e$10$2');
    let fixed = Math.abs(p) < 1e21 ? p.toFixed(Math.max(0, (() => {
      const m = sci.split('e'), digs = m[0].replace('-', '').replace('.', '').length;
      return Math.max(0, digs - 1 - parseInt(m[1], 10));
    })())) : sci;
    return fixed.length <= sci.length ? fixed : sci;
  }

  /** base::make.names(names, unique = TRUE) */
  const RESERVED = new Set(['if', 'else', 'repeat', 'while', 'function', 'for', 'next', 'break', 'TRUE', 'FALSE', 'NULL',
    'Inf', 'NaN', 'NA', 'NA_integer_', 'NA_real_', 'NA_character_', 'NA_complex_', 'in']);
  function makeNames(names, unique) {
    const out = names.map(nm => {
      let s = isNA(nm) ? 'NA' : String(nm);
      const ch = Array.from(s);
      if (ch.length === 0 || !(/[\p{L}.]/u.test(ch[0])) || (ch[0] === '.' && ch.length > 1 && /[0-9]/.test(ch[1]))) s = 'X' + s;
      s = Array.from(s).map(c => /[\p{L}\p{N}._]/u.test(c) ? c : '.').join('');
      if (RESERVED.has(s)) s += '.';
      return s;
    });
    if (!unique) return out;
    const order = stableSortIdx(out.length, (i, j) => (names[i] !== out[i]) - (names[j] !== out[j]));
    const mu = makeUnique(order.map(i => out[i]));
    order.forEach((i, k) => { out[i] = mu[k]; });
    return out;
  }
  /** base::make.unique(names, sep = ".") */
  function makeUnique(names) {
    const out = names.slice(), used = new Set(names), seen = new Set(), cnt = new Map();
    for (let i = 0; i < out.length; i++) {
      const nm = names[i];
      if (!seen.has(nm)) { seen.add(nm); continue; }
      let c = cnt.get(nm) || 1, cand;
      do { cand = nm + '.' + c; c++; } while (used.has(cand));
      cnt.set(nm, c);
      used.add(cand);
      out[i] = cand;
    }
    return out;
  }

  // --- approximate matching (base::agrep / base::adist, unit costs) ---------------------
  /** agrep(pattern, x): pattern approximately contained in x, at most k edits (Sellers' algorithm) */
  function agrepl(pattern, x, k) {
    const p = Array.from(pattern), t = Array.from(x), m = p.length;
    let prev = new Array(m + 1);
    for (let i = 0; i <= m; i++) prev[i] = i;
    let best = prev[m];
    for (let j = 1; j <= t.length; j++) {
      const cur = new Array(m + 1);
      cur[0] = 0;
      for (let i = 1; i <= m; i++)
        cur[i] = Math.min(prev[i] + 1, cur[i - 1] + 1, prev[i - 1] + (p[i - 1] === t[j - 1] ? 0 : 1));
      best = Math.min(best, cur[m]);
      prev = cur;
    }
    return best <= k;
  }
  function levenshtein(a, b) {
    const s = Array.from(a), t = Array.from(b);
    let prev = Array.from({ length: t.length + 1 }, (_, j) => j);
    for (let i = 1; i <= s.length; i++) {
      const cur = [i];
      for (let j = 1; j <= t.length; j++)
        cur[j] = Math.min(prev[j] + 1, cur[j - 1] + 1, prev[j - 1] + (s[i - 1] === t[j - 1] ? 0 : 1));
      prev = cur;
    }
    return prev[t.length];
  }
  /** The app's selected(pattern, x): z = x[agrep(pattern, tolower(x))]; z[which.min(adist(pattern, z))] */
  function guessColumn(pattern, names) {
    const k = Math.ceil(0.1 * Array.from(pattern).length); // agrep's default max.distance = 0.1
    const z = names.filter(nm => agrepl(pattern, nm.toLowerCase(), k));
    if (z.length === 0) return '';
    let bi = 0, bd = Infinity;
    z.forEach((nm, i) => { const d = levenshtein(pattern, nm); if (d < bd) { bd = d; bi = i; } });
    return z[bi];
  }
  function guessMapping(names) {
    return {
      student: guessColumn('student', names),
      track: guessColumn('track', names),
      preference: names.indexOf('Preference') >= 0 ? 'Preference' : '', // only guessed if it matches exactly
      qualification: guessColumn('qualification', names)
    };
  }

  // ======================================================================================
  // File reading (read.csv / readxl::read_excel semantics)
  // ======================================================================================
  /** Split CSV text into records of fields (sep ",", quote '"', "" escape). Quoted flag kept. */
  function csvRecords(text) {
    if (text.charCodeAt(0) === 0xfeff) text = text.slice(1); // (deviation: R would keep a BOM in the first name)
    const recs = [];
    let rec = [], field = '', quoted = false, inQ = false, i = 0, any = false;
    const n = text.length;
    const endField = () => { rec.push({ v: field, q: quoted }); field = ''; quoted = false; };
    const endRec = () => {
      endField();
      if (!(rec.length === 1 && rec[0].v === '' && !rec[0].q)) recs.push(rec); // blank.lines.skip
      rec = [];
    };
    while (i < n) {
      const c = text[i];
      any = true;
      if (inQ) {
        if (c === '"') {
          if (text[i + 1] === '"') { field += '"'; i += 2; continue; }
          inQ = false; i++; continue;
        }
        field += c; i++; continue;
      }
      if (c === '"') { inQ = true; quoted = true; i++; continue; }
      if (c === ',') { endField(); i++; continue; }
      if (c === '\r') { endRec(); i += text[i + 1] === '\n' ? 2 : 1; any = false; continue; }
      if (c === '\n') { endRec(); i++; any = false; continue; }
      field += c; i++;
    }
    if (any || field !== '' || rec.length) endRec();
    return recs;
  }
  /** read.csv(text, header, check.names = checkNames, colClasses = "character") -> {names, rows} */
  function readCsv(text, header, checkNames) {
    const recs = csvRecords(text);
    if (recs.length === 0) return { names: [], rows: [] };
    const ncol = Math.max(...recs.slice(0, 5).map(r => r.length));
    let names;
    let body = recs;
    if (header) {
      names = recs[0].map(f => f.v);
      while (names.length < ncol) names.push('V' + (names.length + 1));
      body = recs.slice(1);
      if (checkNames) names = makeNames(names, true);
    } else {
      names = Array.from({ length: ncol }, (_, j) => 'V' + (j + 1));
    }
    const rows = body.map(r => {
      const row = new Array(names.length);
      for (let j = 0; j < names.length; j++) {
        const f = r[j];
        row[j] = f === undefined ? '' : (f.v === 'NA' ? null : f.v); // na.strings = "NA"
      }
      return row;
    });
    return { names, rows };
  }
  /** base type.convert() result for one character column (used for the quota file) */
  function typeConvert(col) {
    const vals = col.filter(v => !isNA(v) && trimR(v) !== '');
    if (vals.length === 0) return col.map(() => null);
    if (vals.every(v => /^(T|F|TRUE|FALSE|true|false|True|False)$/.test(trimR(v))))
      return col.map(v => isNA(v) || trimR(v) === '' ? null : (/^T/i.test(trimR(v)) ? 1 : 0));
    if (vals.every(v => asNumeric(v) !== null)) return col.map(v => isNA(v) || trimR(v) === '' ? null : asNumeric(v));
    return col.slice();
  }
  /** Get a 2D array of cell values from the first sheet of a workbook (SheetJS) */
  function sheetCells(XLSX, data) {
    const wb = XLSX.read(data, { type: data instanceof ArrayBuffer ? 'array' : 'buffer' });
    const ws = wb.Sheets[wb.SheetNames[0]];
    if (!ws || !ws['!ref']) return [];
    const range = XLSX.utils.decode_range(ws['!ref']);
    const cells = [];
    for (let r = range.s.r; r <= range.e.r; r++) {
      const row = [];
      for (let c = range.s.c; c <= range.e.c; c++) {
        const cell = ws[XLSX.utils.encode_cell({ r, c })];
        row.push(cell && cell.t !== 'z' && cell.v !== undefined && cell.t !== 'e' ? cell : null);
      }
      cells.push(row);
    }
    // readxl ignores leading/trailing empty rows and columns
    const nonEmpty = row => row.some(c => c !== null && !(c.t === 's' && c.v === ''));
    while (cells.length && !nonEmpty(cells[0])) cells.shift();
    while (cells.length && !nonEmpty(cells[cells.length - 1])) cells.pop();
    if (!cells.length) return [];
    let c0 = 0, c1 = cells[0].length - 1;
    const colEmpty = c => cells.every(r => r[c] === null || (r[c].t === 's' && r[c].v === ''));
    while (c0 <= c1 && colEmpty(c0)) c0++;
    while (c1 >= c0 && colEmpty(c1)) c1--;
    return cells.map(r => r.slice(c0, c1 + 1));
  }
  function cellText(cell, trim) {
    if (cell === null) return null;
    let s;
    if (cell.t === 'b') s = cell.v ? 'TRUE' : 'FALSE';
    else if (cell.t === 'n') s = fmtG15(cell.v);
    else if (cell.t === 'd') s = fmtG15(cell.v instanceof Date ? (cell.v - new Date(Date.UTC(1899, 11, 30))) / 864e5 : cell.v);
    else s = String(cell.v);
    if (trim) s = trimR(s);
    return s === '' ? null : s; // readxl: na = ""
  }
  function readxlNames(cells0) {
    let names = cells0.map((c, j) => { const s = cellText(c, true); return s === null ? '...' + (j + 1) : s; });
    const dup = new Map();
    names.forEach(n => dup.set(n, (dup.get(n) || 0) + 1));
    return names.map((n, j) => dup.get(n) > 1 ? n + '...' + (j + 1) : n); // .name_repair = "unique"
  }
  /** read_excel(path, sheet = 1, col_names = TRUE, col_types = "text", trim_ws = TRUE) %>% data.frame() */
  function readExcelText(XLSX, data) {
    const cells = sheetCells(XLSX, data);
    if (!cells.length) return { names: [], rows: [] };
    const names = makeNames(readxlNames(cells[0]), true);
    const rows = cells.slice(1).map(r => r.map(c => cellText(c, true)));
    return { names, rows };
  }
  /** read_excel(path, sheet = 1, col_names = TRUE, col_types = "numeric") */
  function readExcelNumeric(XLSX, data) {
    const cells = sheetCells(XLSX, data);
    if (!cells.length) return { names: [], rows: [] };
    const names = readxlNames(cells[0]);
    const rows = cells.slice(1).map(r => r.map(c => {
      if (c === null) return null;
      if (c.t === 'n') return c.v;
      if (c.t === 'b') return c.v ? 1 : 0;
      if (c.t === 'd') return (c.v - new Date(Date.UTC(1899, 11, 30))) / 864e5;
      return null; // text in a numeric column -> NA (readxl warns)
    }));
    return { names, rows };
  }
  /** Read the applications file like the app does */
  function readApplications(fileName, data, header, XLSX) {
    if (/\.xlsx?$/.test(fileName)) return readExcelText(XLSX, data); // header checkbox is ignored for Excel (as in R)
    return readCsv(typeof data === 'string' ? data : decodeText(data), header, true);
  }
  /** Read the quota file: returns {names, values (numbers or null), display (first row as shown)} */
  function readQuota(fileName, data, header, XLSX) {
    let t;
    if (/\.xlsx?$/.test(fileName)) t = readExcelNumeric(XLSX, data);
    else {
      t = readCsv(typeof data === 'string' ? data : decodeText(data), header, false);
      const conv = t.names.map((_, j) => typeConvert(t.rows.map(r => r[j])));
      t = { names: t.names, rows: t.rows.map((r, i) => r.map((_, j) => conv[j][i])) };
    }
    const first = t.rows.length ? t.rows[0] : t.names.map(() => null);
    return { names: t.names, raw: first, values: first.map(v => asInteger(v)) };
  }
  function decodeText(buf) {
    const u8 = buf instanceof ArrayBuffer ? new Uint8Array(buf) : buf;
    return new TextDecoder('utf-8').decode(u8);
  }

  // ======================================================================================
  // output$contents0: processed applications (dat2) + lottery (collegePrefs CP, studentPrefs SP)
  // ======================================================================================
  const MSG_LEVELS = 'Qualification column is non-numeric and contains values not listed as levels.  ' +
    'Qualification column should be numerical, or levels should be specified.  ' +
    'Check Qualification column selection and qualification levels specification.';

  /** strsplit(x, ",")[[1]] then trimws */
  function parseLevels(text) {
    if (text === '') return [];
    const parts = String(text).split(',');
    if (parts.length > 1 && parts[parts.length - 1] === '') parts.pop();
    return parts.map(trimR);
  }

  /**
   * dat0: {names, rows}; opts: {mapping: {student, track, preference, qualification}, removeCols: [],
   *   qlevels: "E, V, O", seed, rng (RRNG, will be re-seeded), collation}
   * Returns {errorTable} or {dat2, qlevels, CP, SP, nduplicates, notifications}
   */
  function processApplications(dat0, opts) {
    const collate = makeCollate(opts.collation);
    const notes = [];
    const map = opts.mapping;
    // dat[, !(names(dat) %in% input$remove_cols)]
    const keep = dat0.names.map((nm, j) => (opts.removeCols || []).indexOf(nm) < 0 ? j : -1).filter(j => j >= 0);
    let names = keep.map(j => dat0.names[j]);
    let rows = dat0.rows.map(r => keep.map(j => r[j]));
    const rename = (from, to) => { const j = names.indexOf(from); if (from !== '' && j >= 0) names[j] = to; };
    rename(map.student, 'StudentID'); rename(map.track, 'Track');
    rename(map.preference, 'Preference'); rename(map.qualification, 'Qualification');
    let iS = names.indexOf('StudentID'), iT = names.indexOf('Track'), iP = names.indexOf('Preference'), iQ = names.indexOf('Qualification');
    if (iS < 0 || iT < 0) throw new Error('Select the StudentID and Track columns.');
    if (iQ < 0) throw new Error('Select the Qualification column.');

    // Qualification is always character here (files are read with colClasses = "character" /
    // col_types = "text"), so the "numeric Qualification" branch of the app is never taken.
    const qlevels = parseLevels(opts.qlevels);
    if (!rows.every(r => !isNA(r[iQ]) && qlevels.indexOf(r[iQ]) >= 0)) {
      const uniq = [];
      const seen = new Set();
      rows.forEach(r => { const k = keyOf(r[iQ]); if (!seen.has(k)) { seen.add(k); uniq.push(isNA(r[iQ]) ? null : r[iQ]); } });
      const m = Math.max(uniq.length, qlevels.length);
      const t = { names: ['Values Found', 'Values Expected'], rows: [[MSG_LEVELS, '']] };
      for (let i = 0; i < m; i++) t.rows.push([i < uniq.length ? uniq[i] : null, i < qlevels.length ? qlevels[i] : null]);
      return { errorTable: t, notifications: notes };
    }
    if (new Set(qlevels).size !== qlevels.length) throw new Error('factor level is duplicated (check the qualification levels)');
    const nlev = qlevels.length;
    notes.push({ msg: 'Qualification turned into factor', type: 'message' });
    let qual = rows.map(r => qlevels.indexOf(trimR(r[iQ]))); // factor codes 0..nlev-1

    // Preference
    let order = rows.map((_, i) => i);
    if (iP >= 0) {
      notes.push({ msg: 'Arranged by Preference.', type: 'message' });
      const pnum = rows.map(r => asNumeric(r[iP]));
      order = stableSortIdx(rows.length, (a, b) => naLast((x, y) => x - y)(pnum[a], pnum[b]));
    }
    rows = order.map(i => rows[i]); qual = order.map(i => qual[i]);
    const pref = new Array(rows.length);
    { const cnt = new Map(); rows.forEach((r, i) => { const k = keyOf(r[iS]); const c = (cnt.get(k) || 0) + 1; cnt.set(k, c); pref[i] = c; }); }
    // remove duplicated (StudentID, Track), keep first
    const n0 = rows.length;
    { const seen = new Set(), keepIdx = [];
      rows.forEach((r, i) => { const k = keyOf(r[iS]) + '\u0001' + keyOf(r[iT]); if (!seen.has(k)) { seen.add(k); keepIdx.push(i); } });
      rows = keepIdx.map(i => rows[i]); qual = keepIdx.map(i => qual[i]); const p2 = keepIdx.map(i => pref[i]); pref.length = 0; p2.forEach(v => pref.push(v)); }
    const nduplicates = n0 - rows.length;
    if (nduplicates > 0) notes.push({ msg: 'Duplicate rows (with potentially inconsistent Qualification and/or Preference) were removed from the data set. (First occurences were kept.)', type: 'warning', sticky: true });

    // Info: per student, unique trimmed values of every other column joined by "|", columns joined by ":"
    const infoCols = names.map((nm, j) => j).filter(j => j !== iS && j !== iT && j !== iP && j !== iQ);
    const hasInfo = infoCols.length > 0; // ncol(.) > 4
    const groups = new Map();
    rows.forEach((r, i) => { const k = keyOf(r[iS]); if (!groups.has(k)) groups.set(k, []); groups.get(k).push(i); });
    let info = new Array(rows.length);
    for (const idx of groups.values()) {
      const parts = infoCols.map(j => {
        const u = [];
        idx.forEach(i => { let v = rows[i][j]; v = isNA(v) ? '' : trimR(String(v)); if (u.indexOf(v) < 0) u.push(v); });
        return u.join('|');
      });
      const sid = rows[idx[0]][iS];
      parts.push(hasInfo ? '' : (isNA(sid) ? 'NA' : sid));
      const s = parts.join(':').replace(/:[ \t\n\r\f\v]*$/, '');
      idx.forEach(i => { info[i] = s; });
    }
    const nUniqInfo = new Set(info).size, nUniqSID = groups.size;
    if (nUniqInfo < nUniqSID) info = info.map((s, i) => (isNA(rows[i][iS]) ? 'NA' : rows[i][iS]) + ':' + s);
    const dat2 = rows.map((r, i) => ({
      StudentID: isNA(r[iS]) ? null : trimR(r[iS]), Info: trimR(info[i]), Track: isNA(r[iT]) ? null : trimR(r[iT]),
      Preference: pref[i], Qualification: qlevels[qual[i]], q: qual[i]
    }));

    // ---- CP: lottery (set.seed + sample(n()) per (College, Collegepref) group) ----
    const rng = opts.rng || new RRNG();
    rng.setSeed(opts.seed);
    const qualified = dat2.filter(d => d.q !== nlev - 1);
    // spread(Track, Qualification): rows keyed by (StudentID, Info) sorted with sort(); columns = sorted tracks
    const sidLev = sortUnique(qualified.map(d => d.StudentID), collate);
    const infoLev = sortUnique(qualified.map(d => d.Info), collate);
    const keyIdx = new Map(); // "sidIdx|infoIdx" -> row
    const sidPos = new Map(sidLev.map((v, i) => [keyOf(v), i])), infoPos = new Map(infoLev.map((v, i) => [keyOf(v), i]));
    qualified.forEach(d => { keyIdx.set(sidPos.get(keyOf(d.StudentID)) * infoLev.length + infoPos.get(keyOf(d.Info)), { sid: d.StudentID, info: d.Info }); });
    const rowKeys = Array.from(keyIdx.keys()).sort((a, b) => a - b).map(k => keyIdx.get(k));
    const tracks = sortUnique(qualified.map(d => d.Track), collate);
    const cell = new Map();
    qualified.forEach(d => {
      const k = keyOf(d.StudentID) + '\u0001' + keyOf(d.Info) + '\u0001' + keyOf(d.Track);
      if (cell.has(k)) throw new Error('Each row of output must be identified by a unique combination of keys.');
      cell.set(k, d.q);
    });
    // gather(College, Collegepref, -Info, -StudentID)
    let long = [];
    tracks.forEach(t => rowKeys.forEach(rk => {
      const v = cell.get(keyOf(rk.sid) + '\u0001' + keyOf(rk.info) + '\u0001' + keyOf(t));
      long.push({ College: t, pref: v === undefined ? null : v, Info: rk.info });
    }));
    // arrange(College, Collegepref) -- C locale for College, factor level order, NA last
    const prefCmp = naLast((a, b) => a - b);
    const cmpCP = (a, b) => naLast(cCompare)(a.College, b.College) || prefCmp(a.pref, b.pref);
    long = stableSortIdx(long.length, (i, j) => cmpCP(long[i], long[j])).map(i => long[i]);
    // group_by(College, Collegepref) %>% mutate(lottery.rank = sample(n())): groups visited in sorted key order
    for (let s = 0; s < long.length;) {
      let e = s;
      while (e < long.length && cmpCP(long[s], long[e]) === 0) e++;
      const perm = rng.sampleInt(e - s, e - s);
      for (let i = s; i < e; i++) long[i].lottery = perm[i - s];
      s = e;
    }
    long = stableSortIdx(long.length, (i, j) => cmpCP(long[i], long[j]) || (long[i].lottery - long[j].lottery)).map(i => long[i]);
    // group_by(College) %>% mutate(Collegepref = 1:n()) %>% spread(College, Info)
    const cpCols = sortUnique(tracks, collate);
    const byCol = new Map(cpCols.map(t => [keyOf(t), []]));
    long.forEach(r => byCol.get(keyOf(r.College)).push(r.Info));
    const nCP = rowKeys.length;
    const CP = { colnames: cpCols, data: [] };
    for (let i = 0; i < nCP; i++) CP.data.push(cpCols.map(t => byCol.get(keyOf(t))[i]));

    // ---- SP: student preference matrix (rows = rank, columns = students (Info), cells = Track) ----
    const spref = new Map(), spRows = [];
    qualified.forEach(d => { const k = keyOf(d.StudentID); const c = (spref.get(k) || 0) + 1; spref.set(k, c); spRows.push({ info: d.Info, track: d.Track, r: c }); });
    const spCols = sortUnique(spRows.map(d => d.info), collate);
    const spColPos = new Map(spCols.map((v, i) => [keyOf(v), i]));
    const maxR = spRows.reduce((m, d) => Math.max(m, d.r), 0);
    const nrowSP = Math.max(maxR, cpCols.length);
    const SP = { colnames: spCols, data: Array.from({ length: nrowSP }, () => spCols.map(() => null)) };
    const seenSP = new Set();
    spRows.forEach(d => {
      const k = spColPos.get(keyOf(d.info)) + '|' + d.r;
      if (seenSP.has(k)) throw new Error('Each row of output must be identified by a unique combination of keys.');
      seenSP.add(k);
      SP.data[d.r - 1][spColPos.get(keyOf(d.info))] = d.track;
    });
    return { dat2, qlevels, CP, SP, nduplicates, notifications: notes, rng };
  }

  function dat2Table(dat2) {
    return {
      names: ['StudentID', 'Info', 'Track', 'Preference', 'Qualification'],
      rows: dat2.map(d => [d.StudentID, d.Info, d.Track, d.Preference, d.Qualification])
    };
  }

  // ======================================================================================
  // output$contents3: quota table (or mismatch table)
  // ======================================================================================
  function quotaDisplay(quota, CP) {
    if (!quota) return null;
    if (CP) {
      const a = CP.colnames.slice(), q = quota.names.slice();
      const m = Math.max(a.length, q.length);
      while (a.length < m) a.push(null);
      while (q.length < m) q.push(null);
      // NB: the padding with NA means a quota file with *extra* tracks is also reported as an error
      if (!a.every(x => q.some(y => (isNA(x) && isNA(y)) || x === y)))
        return { error: true, table: { names: ['Track names found in applications', 'Track names found in quota'], rows: a.map((x, i) => [x, q[i]]) } };
    }
    return { error: false, table: { names: quota.names, rows: [quota.raw] } };
  }

  // ======================================================================================
  // flexGaleShapely (deferred acceptance, replicating the dplyr loop incl. its row order)
  // ======================================================================================
  /**
   * prefsStudents: array[ncolleges rows][nstudents] of college index (1-based) or null
   * prefsColleges: array[nstudents rows][ncolleges] of student index (1-based)
   * quotas: number or array (recycled)
   * Returns Application rows in the same order as the R function, with the same columns.
   */
  function flexGaleShapley(prefsStudents, prefsColleges, quotas, snames, cnames) {
    const ns = prefsStudents.length ? prefsStudents[0].length : 0;
    const nc = prefsColleges.length ? prefsColleges[0].length : 0;
    if (prefsStudents.length !== nc || prefsColleges.length !== ns)
      throw new Error("'studentPrefs' and 'collegePrefs' are not compatible numeric matrices.");
    for (const row of prefsColleges) for (const v of row) if (isNA(v)) throw new Error("'collegePrefs' contains missing values.");
    for (let c = 0; c < nc; c++) {
      const col = prefsColleges.map(r => r[c]).sort((a, b) => a - b);
      for (let i = 1; i < col.length; i++) if (col[i] - col[i - 1] !== 1)
        throw new Error("'collegePrefs' has columns that are not complete proper orderings of students.");
    }
    for (let s = 0; s < ns; s++) {
      const col = prefsStudents.map(r => r[s]).filter(v => !isNA(v)).sort((a, b) => a - b);
      for (let i = 1; i < col.length; i++) if (col[i] === col[i - 1])
        throw new Error("'studentPrefs' has columns that are not proper orderings of colleges.");
    }
    const q = Array.isArray(quotas) ? quotas : [quotas];
    const quotaCollege = Array.from({ length: nc }, (_, c) => q[c % q.length]);
    for (let c = 0; c < nc; c++) if (isNA(quotaCollege[c]))
      throw new Error('Missing (non-numeric) quota for track ' + (cnames ? "'" + cnames[c] + "'" : c + 1) + '.');
    quotaCollege.push(ns);
    const notPlaced = nc + 1, nrowSP = nc + 1;
    // collegePrefRanks[s][c] = rank of student s in college c (order() of the column); not-placed: rank = s
    const rankOf = Array.from({ length: nc + 2 }, () => new Array(ns + 1));
    for (let c = 0; c < nc; c++) for (let r = 0; r < ns; r++) rankOf[c + 1][prefsColleges[r][c]] = r + 1;
    for (let s = 1; s <= ns; s++) rankOf[notPlaced][s] = s;
    const spAt = (r, s) => (r === nrowSP ? notPlaced : (isNA(prefsStudents[r - 1][s - 1]) ? notPlaced : prefsStudents[r - 1][s - 1]));

    // Row state (arrays indexed by row position, as in the data frame)
    let rows = Array.from({ length: ns }, (_, i) => ({ student: i + 1, studpref: 0, notOn: true, college: null, collegepref: null, rank: null, quota: null }));
    for (;;) {
      const oldSP = rows.map(r => r.studpref), oldNO = rows.map(r => r.notOn);
      for (const r of rows) {
        r.studpref = Math.min(r.studpref + (r.notOn ? 1 : 0), nrowSP);
        r.college = spAt(r.studpref, r.student);
        r.collegepref = rankOf[r.college][r.student];
      }
      rows = stableSortIdx(rows.length, (i, j) => rows[i].collegepref - rows[j].collegepref).map(i => rows[i]);
      const cnt = new Array(nc + 2).fill(0);
      for (const r of rows) { r.rank = ++cnt[r.college]; r.quota = quotaCollege[r.college - 1]; r.notOn = r.rank > r.quota; }
      // NB: R compares the old vectors (previous row order) with the new ones (new row order) positionally
      if (!rows.some(r => r.notOn) || rows.every((r, i) => oldSP[i] === r.studpref && oldNO[i] === r.notOn)) break;
    }
    return rows.map(r => {
      const notOn = r.college === notPlaced ? true : r.notOn;
      const college = notOn ? null : r.college;
      return {
        'student.name': snames ? snames[r.student - 1] : String(r.student), student: r.student, studpref: r.studpref,
        'not.onlist': notOn, college, collegepref: r.collegepref, 'rank.in.college': r.rank, quota: r.quota,
        'college.name': college === null ? null : (cnames ? cnames[college - 1] : String(college))
      };
    });
  }
  const APPL_NAMES = ['student.name', 'student', 'studpref', 'not.onlist', 'college', 'collegepref', 'rank.in.college', 'quota', 'college.name'];

  /** apply(SP, 2, match, colnames(CP)) and apply(CP, 2, match, colnames(SP)), incl. apply()'s vector-dropping */
  function indexMatrices(ctx) {
    const { SP, CP } = ctx;
    const cpPos = new Map(CP.colnames.map((v, i) => [keyOf(v), i + 1]));
    const spPos = new Map(SP.colnames.map((v, i) => [keyOf(v), i + 1]));
    const studentPrefs = SP.data.map(r => r.map(v => (isNA(v) ? null : (cpPos.get(keyOf(v)) || null))));
    const collegePrefs = CP.data.map(r => r.map(v => spPos.get(keyOf(v)) || null));
    // apply() returns a vector (not a matrix) when the matrix has a single row -> flexGaleShapely stops
    const degenerate = studentPrefs.length === 1 || collegePrefs.length === 1 || SP.colnames.length === 0 || CP.colnames.length === 0;
    return { studentPrefs, collegePrefs, degenerate };
  }
  function quotaVector(quota, CP, nstudents) {
    if (!quota) return nstudents;
    return CP.colnames.map(t => {
      const j = quota.names.indexOf(t);
      if (j < 0) throw new Error('subscript out of bounds (no quota for track \'' + t + '\')');
      return quota.values[j];
    });
  }

  // ======================================================================================
  // output$myText: placement + datAugmented + result table
  // ======================================================================================
  function runPlacement(ctx, quota) {
    const { studentPrefs, collegePrefs, degenerate } = indexMatrices(ctx);
    if (degenerate) throw new Error("'studentPrefs' and 'collegePrefs' are not compatible numeric matrices.");
    const q = quotaVector(quota, ctx.CP, ctx.SP.colnames.length);
    const Appl = flexGaleShapley(studentPrefs, collegePrefs, q, ctx.SP.colnames, ctx.CP.colnames);
    const nlev = ctx.qlevels.length;
    const applBy = new Map(Appl.map(a => [keyOf(a['student.name']), a]));
    // summarise(Highest_accepted = max(collegepref), Quotum = max(quota)) by college.name (incl. NA group)
    const agg = new Map();
    Appl.forEach(a => {
      const k = keyOf(a['college.name']);
      const g = agg.get(k) || { h: -Infinity, q: -Infinity };
      g.h = Math.max(g.h, a.collegepref); g.q = Math.max(g.q, a.quota); agg.set(k, g);
    });
    // Rank = position of (Track, Info) in the collegePrefs column
    const rankMap = new Map();
    ctx.CP.colnames.forEach((t, c) => ctx.CP.data.forEach((r, i) => rankMap.set(keyOf(t) + '\u0001' + keyOf(r[c]), i + 1)));
    const aug = ctx.dat2.map(d => {
      const a = applBy.get(keyOf(d.Info));
      const qualifiedRow = d.q !== nlev - 1;
      const notOn = a ? a['not.onlist'] : null;
      const acc = a ? a['college.name'] : null;
      let reason = null;
      if (!qualifiedRow) reason = 'Not qualified';
      else if (notOn === true) reason = 'Ranking & capacity';
      else if (notOn === false) reason = 'Accepted in ' + (isNA(acc) ? 'NA' : acc);
      const g = agg.get(keyOf(d.Track));
      const rk = rankMap.get(keyOf(d.Track) + '\u0001' + keyOf(d.Info));
      return {
        StudentID: d.StudentID, Info: d.Info, Track: d.Track, Qualification: d.Qualification, Preference: d.Preference,
        Rank: rk === undefined ? null : rk, Highest_accepted: g ? g.h : null, Quotum: g ? g.q : null,
        Accepted: (isNA(acc) || isNA(d.Track)) ? null : (acc === d.Track ? 'yes' : 'no'), Reason: reason
      };
    });
    return { Appl, datAugmented: aug, studentPrefs, collegePrefs, quota: q };
  }
  const RESULT_NAMES = ['StudentID', 'Info', 'Track', 'Qualification', 'Preference', 'Rank', 'Highest_accepted', 'Quotum', 'Accepted', 'Reason'];

  // ======================================================================================
  // output$details: betterMatches (stability check)
  // ======================================================================================
  function details(ctx, placement) {
    const { studentPrefs, collegePrefs, Appl } = placement;
    const nc = collegePrefs[0].length, ns = studentPrefs[0].length, n = studentPrefs.length;
    // studentPrefRanks[college][student] = rank or null
    const spr = Array.from({ length: n }, () => new Array(ns).fill(null));
    for (let s = 0; s < ns; s++) for (let i = 0; i < n; i++) { const x = studentPrefs[i][s]; if (!isNA(x)) spr[x - 1][s] = i + 1; }
    const cpr = Array.from({ length: ns }, () => new Array(nc));
    for (let c = 0; c < nc; c++) for (let r = 0; r < ns; r++) cpr[collegePrefs[r][c] - 1][c] = r + 1;
    // collegepref.max: one value per college group (NA group last), *not* aligned to college index
    const mx = new Map();
    Appl.forEach(a => { const k = a.college === null ? Infinity : a.college; mx.set(k, Math.max(mx.has(k) ? mx.get(k) : -Infinity, a.collegepref)); });
    const cpm = Array.from(mx.keys()).sort((a, b) => a - b).map(k => mx.get(k));
    const applBy = new Map(Appl.map(a => [a.student, a]));
    const and3 = (a, b) => (a === false || b === false) ? false : ((a === null || b === null) ? null : true);
    const lt = (a, b) => (isNA(a) || isNA(b)) ? null : a < b;
    const le = (a, b) => (isNA(a) || isNA(b)) ? null : a <= b;
    const rows = [];
    for (let c = 1; c <= nc; c++) for (let s = 1; s <= ns; s++) {
      const so = c - 1 < n ? spr[c - 1][s - 1] : null, co = cpr[s - 1][c - 1];
      const mo = c - 1 < cpm.length ? cpm[c - 1] : null;
      const a = applBy.get(s);
      const better = and3(lt(so, a.studpref), le(co, mo));
      if (better === null) continue;
      rows.push([a['student.name'], a['college.name'], a.studpref, a.collegepref, ctx.CP.colnames[c - 1], so, co, mo]);
    }
    return {
      names: ['Student', 'Accepting College', 'Student Preference', 'Accepting College Preference', 'Other College',
        'Student Preference Other', 'Other College Preference', 'Lowest Preference Accepted'], rows
    };
  }

  // ======================================================================================
  // output$waitlist: 20 withdrawal simulations (RNG stream continues after the lottery)
  // ======================================================================================
  function waitlist(ctx, placement, quota, withdrawrate, rng) {
    const { studentPrefs, collegePrefs } = placement;
    const collate = makeCollate(ctx.collation);
    // (deviation) R: quota2 = ncol(studentPrefs) refers to an undefined object when no quota file is loaded
    const quota2 = quota ? quotaVector(quota, ctx.CP, studentPrefs[0].length) : studentPrefs[0].length;
    const Appl2 = placement.Appl;
    const nsim = 20;
    const nwithdrawals = Math.trunc(collegePrefs.length * Number(withdrawrate) / 100);
    const placedPos = [];
    Appl2.forEach((a, i) => { if (!a['not.onlist']) placedPos.push(i); });
    const sims = [];
    for (let k = 0; k < nsim; k++) {
      // (deviation) R's sample(which(...)) samples 1:x when exactly one student is placed; here always from the set
      const pick = rng.sampleInt(placedPos.length, nwithdrawals).map(i => placedPos[i - 1]);
      const drop = new Set(pick.map(i => Appl2[i].student));
      const keepS = [];
      for (let s = 1; s <= studentPrefs[0].length; s++) if (!drop.has(s)) keepS.push(s);
      const newId = new Map(keepS.map((s, i) => [s, i + 1]));
      const cpDO = [];
      const ncol = collegePrefs[0].length;
      const colsDO = [];
      for (let c = 0; c < ncol; c++) colsDO.push(collegePrefs.map(r => r[c]).filter(s => !drop.has(s)).map(s => newId.get(s)));
      for (let r = 0; r < keepS.length; r++) cpDO.push(colsDO.map(col => col[r]));
      const spDO = studentPrefs.map(r => keepS.map(s => r[s - 1]));
      if (spDO.length === 1 || cpDO.length === 1) throw new Error("'studentPrefs2DO' and 'collegePrefs2DO' are not compatible numeric matrices.");
      const A = flexGaleShapley(spDO, cpDO, quota2, keepS.map(s => ctx.SP.colnames[s - 1]), ctx.CP.colnames);
      sims.push(A.map(a => [a['student.name'], a['college.name']]));
    }
    // Reduce(left_join) starting from simulation 1: students withdrawn in simulation 1 drop out entirely
    const nameToStudent = new Map(Appl2.map(a => [a['student.name'], a]));
    const simMaps = sims.map(s => new Map(s.map(([n, c]) => [n, c])));
    const base = sims[0].map(([n]) => n);
    const rowsW = base.map(n => ({ name: n, student: nameToStudent.get(n).student, cols: simMaps.map(m => (m.has(n) ? m.get(n) : null)) }));
    rowsW.sort((a, b) => a.student - b.student);
    // counts per (student, college.name), spread with sort(unique()) order, NA -> "<NA>"
    const colleges = sortUnique([].concat(...rowsW.map(r => r.cols)), collate);
    const colNames = colleges.map(c => (c === null ? '<NA>' : c));
    if (colNames.indexOf('<NA>') < 0) colNames.push('<NA>'); // (deviation) R errors if nobody is ever unplaced
    const out = [];
    for (const r of rowsW) {
      const cnt = new Map();
      r.cols.forEach(c => cnt.set(keyOf(c), (cnt.get(keyOf(c)) || 0) + 1));
      const vals = colNames.map(cn => {
        const k = cn === '<NA>' ? keyOf(null) : keyOf(cn);
        const x = (cnt.get(k) || 0) / nsim;
        return 100 * Number(x.toFixed(2)); // 100*round(./nsim, 2)
      });
      const a = nameToStudent.get(r.name);
      const naV = vals[colNames.indexOf('<NA>')];
      const notOn = a['not.onlist'];
      // filter_at(all colleges incl. <NA>, all_vars(!not.onlist & (. + `<NA>`) < 100 | not.onlist & `<NA>` < 100))
      const keepRow = vals.every(v => (!notOn && (v + naV) < 100) || (notOn && naV < 100));
      if (keepRow) out.push([a.student, r.name].concat(vals, [notOn, a['college.name']]));
    }
    return { names: ['student', 'student.name'].concat(colNames, ['not.onlist', 'college.name']), rows: out, colleges: colNames };
  }

  // ======================================================================================
  // Summary tab: plot data and per-track admission lists
  // ======================================================================================
  function summaryData(Appl) {
    const cnt = new Map();
    Appl.forEach(a => cnt.set(a.studpref, (cnt.get(a.studpref) || 0) + 1));
    const ranks = Array.from(cnt.keys()).sort((a, b) => a - b);
    const tot = Appl.length;
    return { ranks, counts: ranks.map(r => cnt.get(r)), percentages: ranks.map(r => 100 * cnt.get(r) / tot) };
  }
  /** colorRampPalette(rev(brewer.pal(9, "Blues")))(n) */
  function bluesPalette(n) {
    const blues = ['#F7FBFF', '#DEEBF7', '#C6DBEF', '#9ECAE1', '#6BAED6', '#4292C6', '#2171B5', '#08519C', '#08306B'].reverse()
      .map(h => [1, 3, 5].map(i => parseInt(h.substr(i, 2), 16)));
    const out = [];
    for (let k = 0; k < n; k++) {
      const x = n === 1 ? 0 : k / (n - 1), pos = x * 8, i = Math.min(7, Math.floor(pos)), f = pos - i;
      const rgb = [0, 1, 2].map(ch => Math.floor((blues[i][ch] + f * (blues[i + 1][ch] - blues[i][ch])) / 255 * 255 + 0.5));
      out.push('#' + rgb.map(v => v.toString(16).padStart(2, '0').toUpperCase()).join(''));
    }
    return out;
  }
  function admissions(ctx, placement, quota, wl) {
    const collate = makeCollate(ctx.collation);
    let entries = placement.Appl.map(a => ({ name: a['student.name'], college: a['college.name'], chance: null }));
    if (wl) {
      const W = [];
      const cols = wl.names.slice(2, wl.names.length - 2).filter(c => c !== '<NA>');
      cols.forEach(c => wl.rows.forEach(r => { const v = r[wl.names.indexOf(c)]; if (v > 0) W.push({ name: r[1], college: c, chance: v }); }));
      const Ws = stableSortIdx(W.length, (i, j) => W[j].chance - W[i].chance).map(i => W[i]);
      const used = new Set();
      entries.forEach(e => {
        const hit = Ws.findIndex((w, i) => !used.has(i) && w.name === e.name && w.college === e.college);
        if (hit >= 0) { used.add(hit); e.chance = Ws[hit].chance; }
      });
      Ws.forEach((w, i) => { if (!used.has(i)) entries.push({ name: w.name, college: w.college, chance: w.chance }); });
      entries.forEach(e => { if (e.chance !== null) e.name = '(' + e.name + ') [' + fmtDouble(e.chance) + '%]'; });
    }
    const groups = sortUnique(entries.map(e => e.college).filter(c => !isNA(c)), collate);
    return groups.map(g => {
      let qv = '';
      if (quota) { const j = quota.names.indexOf(g); qv = j >= 0 ? (isNA(quota.raw[j]) ? 'NA' : (typeof quota.raw[j] === 'number' ? fmtDouble(quota.raw[j]) : String(quota.raw[j]))) : '?'; }
      return { header: g + ' [' + qv + ']', students: entries.filter(e => e.college === g).map(e => e.name) };
    });
  }

  // ======================================================================================
  // downloadData: CSV (UTF-16LE, na = "") joined back onto the original columns
  // ======================================================================================
  function downloadTable(ctx, placement, mapping, wl) {
    const dat0 = ctx.dat0;
    const dropCols = [mapping.qualification, mapping.preference];
    const keepJ = dat0.names.map((n, j) => (dropCols.indexOf(n) >= 0 ? -1 : j)).filter(j => j >= 0);
    const xNames = keepJ.map(j => dat0.names[j]);
    const yCols = RESULT_NAMES.slice();
    const yTypes = { StudentID: 'chr', Info: 'chr', Track: 'chr', Qualification: 'chr', Preference: 'int', Rank: 'int', Highest_accepted: 'int', Quotum: Array.isArray(placement.quota) ? 'int' : 'dbl', Accepted: 'chr', Reason: 'chr', 'Post placement chance': 'dbl' };
    let yRows = placement.datAugmented.map(d => yCols.map(c => d[c]));
    if (wl) {
      const chance = new Map();
      const cols = wl.names.slice(2, wl.names.length - 2).filter(c => c !== '<NA>');
      cols.forEach(c => wl.rows.forEach(r => { const v = r[wl.names.indexOf(c)]; if (v > 0) chance.set(keyOf(r[1]) + '\u0001' + keyOf(c), v); }));
      yRows = yRows.map((r, i) => { const d = placement.datAugmented[i]; const v = chance.get(keyOf(d.Info) + '\u0001' + keyOf(d.Track)); return r.concat([v === undefined ? null : v]); });
      yCols.push('Post placement chance');
    }
    const kx = [xNames.indexOf(mapping.student), xNames.indexOf(mapping.track)];
    const yIdx = new Map();
    yRows.forEach(r => { const k = keyOf(r[0]) + '\u0001' + keyOf(r[2]); if (!yIdx.has(k)) yIdx.set(k, []); yIdx.get(k).push(r); });
    const yKeep = yCols.map((c, j) => j).filter(j => j !== 0 && j !== 2);
    // dplyr suffixes for clashing non-key names
    const xOut = xNames.map((n, j) => (j !== kx[0] && j !== kx[1] && yKeep.some(k => yCols[k] === n)) ? n + '.x' : n);
    const yOut = yKeep.map(k => (xNames.some((n, j) => j !== kx[0] && j !== kx[1] && n === yCols[k]) ? yCols[k] + '.y' : yCols[k]));
    const names = xOut.concat(yOut);
    const types = xNames.map(() => 'chr').concat(yKeep.map(k => yTypes[yCols[k]]));
    let rows = [];
    dat0.rows.forEach(r0 => {
      const x = keepJ.map(j => r0[j]);
      const m = yIdx.get(keyOf(x[kx[0]]) + '\u0001' + keyOf(x[kx[1]]));
      if (m) m.forEach(y => rows.push(x.concat(yKeep.map(k => y[k]))));
      else rows.push(x.concat(yKeep.map(() => null)));
    });
    // arrange(Track, Info). (deviation) R's code refers to a column literally called "Track", which only
    // exists if the track column of the uploaded file is named "Track"; otherwise the R download fails.
    // Here the selected track column is used.
    const iInfo = names.indexOf(yOut[yKeep.findIndex(k => yCols[k] === 'Info')]);
    const cmpS = naLast(cCompare);
    rows = stableSortIdx(rows.length, (a, b) => cmpS(rows[a][kx[1]], rows[b][kx[1]]) || cmpS(rows[a][iInfo], rows[b][iInfo])).map(i => rows[i]);
    return { names, types, rows };
  }
  function writeCsv(t) {
    const q = s => '"' + String(s).replace(/"/g, '""') + '"';
    const lines = [t.names.map(q).join(',')];
    t.rows.forEach(r => lines.push(r.map((v, j) => {
      if (isNA(v)) return '';
      if (t.types[j] === 'chr') return q(v);
      if (t.types[j] === 'int') return String(v);
      return fmtDouble(v);
    }).join(',')));
    return lines.join('\n') + '\n';
  }
  function utf16le(str) {
    const buf = new Uint8Array(str.length * 2);
    for (let i = 0; i < str.length; i++) { const c = str.charCodeAt(i); buf[2 * i] = c & 0xff; buf[2 * i + 1] = c >> 8; }
    return buf;
  }

  // ======================================================================================
  // Full headless pipeline (used by the Node comparison and mirrored by the UI)
  // ======================================================================================
  function runAll(opts) {
    // opts: {dat0, quota, mapping, removeCols, qlevels, seed, sampleKind, collation, withdrawrate, waitlist}
    const out = { errors: {} };
    const rng = new RRNG(null, opts.sampleKind);
    let ctx = null;
    try {
      ctx = processApplications(opts.dat0, { mapping: opts.mapping, removeCols: opts.removeCols, qlevels: opts.qlevels, seed: opts.seed, rng, collation: opts.collation });
    } catch (e) { out.errors.contents0 = e.message; }
    if (ctx && ctx.errorTable) { out.contents0 = ctx.errorTable; ctx = null; }
    if (ctx) {
      ctx.dat0 = opts.dat0; ctx.collation = opts.collation;
      out.contents0 = dat2Table(ctx.dat2); out.CP = ctx.CP; out.SP = ctx.SP; out.nduplicates = ctx.nduplicates;
    }
    if (opts.quota) out.contents3 = quotaDisplay(opts.quota, ctx && ctx.CP);
    let pl = null;
    if (ctx) {
      try { pl = runPlacement(ctx, opts.quota); } catch (e) { out.errors.myText = e.message; }
    }
    if (pl) {
      out.result = { names: RESULT_NAMES, rows: pl.datAugmented.map(d => RESULT_NAMES.map(c => d[c])) };
      out.Appl = { names: APPL_NAMES, rows: pl.Appl.map(a => APPL_NAMES.map(c => a[c])) };
      try { out.details = details(ctx, pl); } catch (e) { out.errors.details = e.message; }
      out.summary = summaryData(pl.Appl);
      try { out.admissions = admissions(ctx, pl, opts.quota, null); } catch (e) { out.errors.collegeAdmissions = e.message; }
      try { out.download = writeCsv(downloadTable(ctx, pl, opts.mapping, null)); } catch (e) { out.errors.download = e.message; }
      out.rngAfterLottery = rng.getState();
      if (opts.waitlist !== false) {
        let wl = null;
        try { wl = waitlist(ctx, pl, opts.quota, opts.withdrawrate, rng); } catch (e) { out.errors.waitlist = e.message; }
        if (wl) {
          out.waitlist = wl;
          try { out.admissions_wl = admissions(ctx, pl, opts.quota, wl); } catch (e) { out.errors.collegeAdmissions2 = e.message; }
          try { out.download_wl = writeCsv(downloadTable(ctx, pl, opts.mapping, wl)); } catch (e) { out.errors.download_wl = e.message; }
        }
      }
    }
    return out;
  }

  const OMTP = {
    RRNG, readCsv, readApplications, readQuota, guessMapping, guessColumn, makeNames, fmtG15, fmtDouble,
    processApplications, dat2Table, quotaDisplay, flexGaleShapley, runPlacement, details, waitlist, summaryData,
    bluesPalette, admissions, downloadTable, writeCsv, utf16le, runAll, RESULT_NAMES, APPL_NAMES, isNA
  };

  if (typeof module !== 'undefined' && module.exports) { module.exports = OMTP; return; }
  if (typeof window !== 'undefined') window.OMTP = OMTP;
  if (typeof document === 'undefined') return;

  // ======================================================================================
  // User interface
  // ======================================================================================
  const $ = id => document.getElementById(id);
  const S = { dat0: null, file1: null, quota: null, ctx: null, pl: null, wl: null, rng: new RRNG(null, 'Rejection'), charts: {} };

  function toast(msg, type, sticky) {
    const d = document.createElement('div');
    d.className = 'toast ' + (type || 'message');
    d.textContent = msg;
    const x = document.createElement('button'); x.textContent = '×'; x.className = 'toast-x'; x.onclick = () => d.remove();
    d.appendChild(x);
    $('toasts').appendChild(d);
    if (!sticky) setTimeout(() => d.remove(), type === 'error' ? 8000 : 2500);
  }
  function fmtCell(v) {
    if (isNA(v)) return '';
    if (typeof v === 'boolean') return v ? 'TRUE' : 'FALSE';
    if (typeof v === 'number') return fmtDouble(v);
    return String(v);
  }
  /** Simple data table with search, paging and click-to-sort (stand-in for Shiny's DataTables) */
  function renderTable(el, t, opts) {
    el.innerHTML = '';
    if (!t) return;
    opts = opts || {};
    const state = { q: '', page: 0, size: opts.paging === false ? Infinity : 25, sort: null, dir: 1 };
    const wrap = document.createElement('div');
    if (opts.paging !== false) {
      const ctl = document.createElement('div'); ctl.className = 'tbl-ctl';
      ctl.innerHTML = '<label>Show <select><option>10</option><option selected>25</option><option>50</option><option>100</option><option value="Infinity">all</option></select> entries</label><label>Search: <input type="search"></label>';
      ctl.querySelector('select').onchange = e => { state.size = Number(e.target.value); state.page = 0; draw(); };
      ctl.querySelector('input').oninput = e => { state.q = e.target.value.toLowerCase(); state.page = 0; draw(); };
      wrap.appendChild(ctl);
    }
    const scroller = document.createElement('div'); scroller.className = 'tbl-scroll';
    const table = document.createElement('table'); table.className = 'data';
    scroller.appendChild(table); wrap.appendChild(scroller);
    const info = document.createElement('div'); info.className = 'tbl-info'; wrap.appendChild(info);
    el.appendChild(wrap);
    function draw() {
      let rows = t.rows;
      if (state.q) rows = rows.filter(r => r.some(v => fmtCell(v).toLowerCase().indexOf(state.q) >= 0));
      if (state.sort !== null) {
        const j = state.sort;
        rows = rows.slice().sort((a, b) => {
          const x = a[j], y = b[j];
          if (isNA(x)) return isNA(y) ? 0 : 1; if (isNA(y)) return -1;
          return (typeof x === 'number' && typeof y === 'number' ? x - y : String(x).localeCompare(String(y))) * state.dir;
        });
      }
      const n = rows.length, pages = Math.max(1, Math.ceil(n / state.size));
      state.page = Math.min(state.page, pages - 1);
      const start = state.size === Infinity ? 0 : state.page * state.size;
      const shown = state.size === Infinity ? rows : rows.slice(start, start + state.size);
      const thead = '<thead><tr>' + t.names.map((nm, j) => '<th data-j="' + j + '">' + esc(nm) + (state.sort === j ? (state.dir > 0 ? ' ▲' : ' ▼') : '') + '</th>').join('') + '</tr></thead>';
      const tbody = '<tbody>' + shown.map(r => '<tr>' + r.map(v => '<td' + (typeof v === 'number' ? ' class="num"' : '') + '>' + esc(fmtCell(v)) + '</td>').join('') + '</tr>').join('') + '</tbody>';
      table.innerHTML = thead + tbody;
      table.querySelectorAll('th').forEach(th => th.onclick = () => { const j = Number(th.dataset.j); if (state.sort === j) state.dir = -state.dir; else { state.sort = j; state.dir = 1; } draw(); });
      info.innerHTML = '';
      if (opts.paging === false) return;
      const span = document.createElement('span');
      span.textContent = n === 0 ? 'No entries' : 'Showing ' + (start + 1) + ' to ' + (start + shown.length) + ' of ' + n + ' entries' + (n !== t.rows.length ? ' (filtered from ' + t.rows.length + ')' : '');
      info.appendChild(span);
      if (pages > 1) {
        const prev = document.createElement('button'); prev.textContent = 'Previous'; prev.disabled = state.page === 0; prev.onclick = () => { state.page--; draw(); };
        const next = document.createElement('button'); next.textContent = 'Next'; next.disabled = state.page >= pages - 1; next.onclick = () => { state.page++; draw(); };
        const pg = document.createElement('span'); pg.textContent = ' page ' + (state.page + 1) + ' / ' + pages + ' ';
        info.append(' ', prev, pg, next);
      }
    }
    draw();
  }
  function esc(s) { return String(s).replace(/[&<>"]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[c]); }

  function setOptions(sel, choices, selected, withNone) {
    sel.innerHTML = '';
    const all = (withNone ? [''] : []).concat(choices);
    all.forEach(c => { const o = document.createElement('option'); o.value = c; o.textContent = c === '' ? '(none)' : c; sel.appendChild(o); });
    sel.value = all.indexOf(selected) >= 0 ? selected : (all[0] || '');
  }
  function getMapping() {
    return { student: $('student').value, track: $('track').value, preference: $('preference').value, qualification: $('qualification').value };
  }
  function updateRemoveCols() {
    if (!S.dat0) return;
    const m = getMapping();
    const sel = $('remove_cols');
    const prev = Array.from(sel.selectedOptions).map(o => o.value);
    const choices = S.dat0.names.filter(n => [m.student, m.track, m.preference, m.qualification].indexOf(n) < 0);
    sel.innerHTML = '';
    choices.forEach(c => { const o = document.createElement('option'); o.value = c; o.textContent = c; o.selected = prev.indexOf(c) >= 0; sel.appendChild(o); });
  }

  async function readFile(file) {
    return /\.xlsx?$/i.test(file.name) ? await file.arrayBuffer() : await file.text();
  }
  async function onFile1() {
    const f = $('file1').files[0];
    if (!f) return;
    try {
      const data = await readFile(f);
      S.file1 = { name: f.name, data };
      S.dat0 = readApplications(f.name.replace(/\.XLSX?$/, m => m.toLowerCase()), data, $('header1').checked, window.XLSX);
    } catch (e) { toast('Could not read file: ' + e.message, 'error', true); return; }
    const nms = S.dat0.names, g = guessMapping(nms);
    setOptions($('student'), nms, g.student, true);
    setOptions($('track'), nms, g.track, true);
    setOptions($('preference'), nms, g.preference, true);
    setOptions($('qualification'), nms, g.qualification, true);
    updateRemoveCols();
    stageApplications();
  }
  async function onFile3() {
    const f = $('file3').files[0];
    if (!f) return;
    try {
      const data = await readFile(f);
      S.quotaFile = { name: f.name, data };
      S.quota = readQuota(f.name.replace(/\.XLSX?$/, m => m.toLowerCase()), data, $('header3').checked, window.XLSX);
    } catch (e) { toast('Could not read quota file: ' + e.message, 'error', true); return; }
    stageQuota();
  }

  // Stage A: applications + lottery (re-runs on mapping / levels / seed / sample kind changes)
  function stageApplications() {
    S.ctx = null;
    if (!S.dat0) return;
    S.rng = new RRNG(null, $('samplekind').value);
    const seedTxt = $('iseed').value.trim();
    // set.seed(NULL) re-initialises from the clock in R; here: a random seed
    const seed = seedTxt === '' ? Math.floor(Math.random() * 2147483647) : Number(seedTxt);
    let ctx;
    try {
      ctx = processApplications(S.dat0, { mapping: getMapping(), removeCols: Array.from($('remove_cols').selectedOptions).map(o => o.value), qlevels: $('qlevels').value, seed, rng: S.rng, collation: $('collation').value });
    } catch (e) {
      $('contents0').innerHTML = '<p class="err">Error: ' + esc(e.message) + '</p>';
      clearResults(); stageQuota(); return;
    }
    ctx.notifications.forEach(n => toast(n.msg, n.type, n.sticky));
    if (ctx.errorTable) { renderTable($('contents0'), ctx.errorTable); clearResults(); stageQuota(); return; }
    ctx.dat0 = S.dat0; ctx.collation = $('collation').value;
    S.ctx = ctx;
    renderTable($('contents0'), dat2Table(ctx.dat2));
    stageQuota();
  }
  // Stage B: quota table
  function stageQuota() {
    const d = quotaDisplay(S.quota, S.ctx && S.ctx.CP);
    if (!d) $('contents3').innerHTML = '';
    else {
      renderTable($('contents3'), d.table, { paging: false });
      if (d.error) toast('Found track names in student applications that have no quota specified. Please correct.', 'error', true);
    }
    stagePlacement();
  }
  function clearResults() {
    S.pl = null; S.wl = null;
    ['myText', 'details', 'waitlist', 'collegeAdmissions'].forEach(id => { $(id).innerHTML = ''; });
    drawCharts(null);
    $('downloadData').disabled = true;
  }
  // Stage C: Gale-Shapley, result, details, summary
  function stagePlacement() {
    clearResults();
    if (!S.ctx) return;
    try { S.pl = runPlacement(S.ctx, S.quota); } catch (e) { $('myText').innerHTML = '<p class="err">Error: ' + esc(e.message) + '</p>'; return; }
    renderTable($('myText'), { names: RESULT_NAMES, rows: S.pl.datAugmented.map(d => RESULT_NAMES.map(c => d[c])) });
    try { renderTable($('details'), details(S.ctx, S.pl)); } catch (e) { $('details').innerHTML = '<p class="err">Error: ' + esc(e.message) + '</p>'; }
    drawCharts(S.pl);
    drawAdmissions();
    $('downloadData').disabled = false;
    if (currentTab() === 'tab-waitlist') stageWaitlist();
  }
  function stageWaitlist() {
    if (!S.pl) return;
    $('waitlist').innerHTML = '<p class="muted">Running 20 simulations…</p>';
    setTimeout(() => {
      try { S.wl = waitlist(S.ctx, S.pl, S.quota, $('withdrawrate').value, S.rng); }
      catch (e) { S.wl = null; $('waitlist').innerHTML = '<p class="err">Error: ' + esc(e.message) + '</p>'; drawAdmissions(); return; }
      renderTable($('waitlist'), { names: S.wl.names, rows: S.wl.rows });
      drawAdmissions();
    }, 20);
  }
  function drawAdmissions() {
    const el = $('collegeAdmissions');
    el.innerHTML = '';
    if (!S.pl) return;
    let groups;
    try { groups = admissions(S.ctx, S.pl, S.quota, S.wl); } catch (e) { el.innerHTML = '<p class="err">Error: ' + esc(e.message) + '</p>'; return; }
    groups.forEach(g => {
      const d = document.createElement('div'); d.className = 'adm';
      d.innerHTML = '<h6>' + esc(g.header) + '</h6><ol>' + g.students.map(s => '<li>' + esc(s) + '</li>').join('') + '</ol>';
      el.appendChild(d);
    });
  }
  function drawCharts(pl) {
    Object.values(S.charts).forEach(c => c.destroy());
    S.charts = {};
    if (!pl || typeof Chart === 'undefined') return;
    const sd = summaryData(pl.Appl);
    const nonPlacedRank = S.ctx.CP.colnames.length + 1;
    const lbl = sd.ranks.map(String);
    S.charts.bar = new Chart($('summary'), {
      type: 'bar',
      data: { labels: lbl, datasets: [{ data: sd.counts, backgroundColor: '#595959' }] },
      options: {
        animation: false, maintainAspectRatio: false,
        plugins: { legend: { display: false }, title: { display: true, text: 'Number of students assigned to their r-th choice' },
          tooltip: { callbacks: { title: it => 'r = ' + it[0].label + (Number(it[0].label) === nonPlacedRank ? ' (not placed)' : '') } } },
        scales: { x: { title: { display: true, text: 'r' } }, y: { beginAtZero: true, title: { display: true, text: 'count' }, ticks: { precision: 0 } } }
      }
    });
    // scale_fill_manual(values = getPalette(colourCount)) with levels rev(studpref); colourCount as in R 3.x
    // (nlevels(college.name) + 1 when college.name was a factor; see notes)
    const colourCount = Math.max(S.ctx.CP.colnames.length + 1, 3);
    const pal = bluesPalette(colourCount);
    const k = sd.ranks.length;
    const colors = sd.ranks.map((_, i) => pal[k - 1 - i]);
    const labelPlugin = {
      id: 'pctLabels',
      afterDatasetsDraw(chart) {
        const { ctx } = chart, meta = chart.getDatasetMeta(0);
        ctx.save(); ctx.fillStyle = 'darkblue'; ctx.font = '12px sans-serif'; ctx.textAlign = 'center'; ctx.textBaseline = 'middle';
        meta.data.forEach((arc, i) => {
          const a = (arc.startAngle + arc.endAngle) / 2, r = arc.outerRadius + 16;
          ctx.fillText(Math.round(sd.percentages[i] * 10) / 10 + '%', arc.x + Math.cos(a) * r, arc.y + Math.sin(a) * r);
        });
        ctx.restore();
      }
    };
    S.charts.pie = new Chart($('summary2'), {
      type: 'pie',
      data: { labels: lbl, datasets: [{ data: sd.percentages, backgroundColor: colors, borderColor: '#fff', borderWidth: 1 }] },
      options: {
        animation: false, maintainAspectRatio: false, layout: { padding: 28 },
        plugins: { legend: { position: 'bottom', title: { display: true, text: 'rank' } },
          tooltip: { callbacks: { label: it => 'rank ' + it.label + ': ' + it.raw.toFixed(1) + '%' } } }
      },
      plugins: [labelPlugin]
    });
  }
  function currentTab() { const b = document.querySelector('.tabs button.active'); return b ? b.dataset.tab : null; }
  function download() {
    if (!S.pl) return;
    let csv;
    try { csv = writeCsv(downloadTable(S.ctx, S.pl, getMapping(), S.wl)); } catch (e) { toast('Download failed: ' + e.message, 'error', true); return; }
    const blob = new Blob([utf16le(csv)], { type: 'text/csv;charset=utf-16le' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = (S.file1 ? S.file1.name.replace(/\.\w+$/, '') : 'applications') + '_placement.csv';
    document.body.appendChild(a); a.click();
    setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
  }

  function initUI() {
    const now = new Date();
    $('iseed').value = String(now.getFullYear() * 10000 + (now.getMonth() + 1) * 100 + now.getDate()); // default seed: today's date, YYYYMMDD
    $('file1').addEventListener('change', onFile1);
    $('header1').addEventListener('change', () => { if (S.file1) { $('file1').dispatchEvent(new Event('change')); } });
    $('file3').addEventListener('change', onFile3);
    $('header3').addEventListener('change', () => { if (S.quotaFile) { S.quota = readQuota(S.quotaFile.name.toLowerCase(), S.quotaFile.data, $('header3').checked, window.XLSX); stageQuota(); } });
    ['student', 'track', 'preference', 'qualification'].forEach(id => $(id).addEventListener('change', () => { updateRemoveCols(); stageApplications(); }));
    ['remove_cols', 'qlevels', 'iseed', 'samplekind', 'collation'].forEach(id => $(id).addEventListener('change', stageApplications));
    const wr = $('withdrawrate');
    wr.addEventListener('input', () => { $('withdrawrate_val').textContent = wr.value; });
    wr.addEventListener('change', () => { S.wl = null; drawAdmissions(); if (currentTab() === 'tab-waitlist') stageWaitlist(); });
    document.querySelectorAll('.tabs button').forEach(b => b.addEventListener('click', () => {
      document.querySelectorAll('.tabs button').forEach(x => x.classList.toggle('active', x === b));
      document.querySelectorAll('.tabpane').forEach(p => p.classList.toggle('active', p.id === b.dataset.tab));
      if (b.dataset.tab === 'tab-waitlist' && S.pl && !S.wl) stageWaitlist();
      if (b.dataset.tab === 'tab-summary') Object.values(S.charts).forEach(c => c.resize());
    }));
    $('downloadData').addEventListener('click', download);
    $('downloadData').disabled = true;
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', initUI); else initUI();
})();
