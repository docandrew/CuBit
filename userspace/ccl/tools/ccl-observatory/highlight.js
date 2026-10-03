// CCL lexical classes for the browser: the same rules as the native
// CCL.Highlighting (userspace/ccl/src/ccl-highlighting.adb). The two are kept
// in step by golden vectors that the native classifier writes
// (tests/ccl-console, highlight-vectors.json) and highlight.test.mjs checks.
export const CLASSES = ['whitespace', 'comment', 'delimiter', 'mismatch', 'special-form',
  'operator', 'host-operation', 'type-name', 'call-name', 'name', 'number',
  'boolean', 'text', 'unterminated'];
export const MAXIMUM_DEPTH = 255;

// CCL.Highlighting.Special_Form_Name and Core_Operator_Name, and the
// built-ins of CCL.Language.Builtin_Name.
const SPECIAL_FORMS = new Set(['define', 'type', 'let', 'if', 'match', 'fn', 'handler', 'field',
  'list', 'list-of', 'stream', '->>', 'and', 'or', 'not']);
const OPERATORS = new Set(['+', '-', '*', '/', '%', '=', '/=', '<', '<=', '>', '>=', 'add',
  'subtract', 'multiply', 'divide', 'mod', 'modulo', 'equal', 'not-equal', 'less', 'less-equal',
  'greater', 'greater-equal', 'at', 'concat', 'length', 'to-string',
  'each', 'where', 'fold', 'any', 'all', 'first', 'sum', 'range', 'last', 'skip', 'reverse',
  'sort', 'sort-by', 'count', 'min', 'max', 'contains', 'upper', 'lower', 'trim', 'starts-with',
  'ends-with', 'index-of', 'replace', 'split', 'join', 'parse-int',
  'latest', 'window', 'arrived', 'lost']);
export const COMPLETION_WORDS = [...SPECIAL_FORMS, ...OPERATORS];

const space = c => c === ' ' || c === '\t' || c === '\r' || c === '\n';
const lineEnd = c => c === '\r' || c === '\n';
const wordCharacter = c => !space(c) && c !== '(' && c !== ')' && c !== '"' && c !== '#';
const isNumber = w => /^-?[0-9]+$/.test(w) && w !== '-';

function wordClass(word, head) {
  if (isNumber(word)) return 'number';
  if (word === 'true' || word === 'false') return 'boolean';
  if (SPECIAL_FORMS.has(word)) return 'special-form';
  if (OPERATORS.has(word)) return 'operator';
  if (word.includes('.')) return 'host-operation';
  if (/^[A-Z]/.test(word)) return 'type-name';
  return head ? 'call-name' : 'name';
}

// One mark per character: { cls, depth }.
export function classify(source) {
  const marks = Array.from(source, () => ({ cls: 'whitespace', depth: 0 }));
  let depth = 0, head = false, i = 0;
  while (i < source.length) {
    const c = source[i];
    let last = i;
    if (space(c)) {
      // whitespace
    } else if (c === '#') {
      while (last + 1 < source.length && !lineEnd(source[last + 1])) last++;
      for (let k = i; k <= last; k++) marks[k] = { cls: 'comment', depth: 0 };
    } else if (c === '(') {
      marks[i] = { cls: 'delimiter', depth: Math.min(depth, MAXIMUM_DEPTH) };
      depth++;
      head = true;
    } else if (c === ')') {
      if (depth === 0) marks[i] = { cls: 'mismatch', depth: 0 };
      else { depth--; marks[i] = { cls: 'delimiter', depth: Math.min(depth, MAXIMUM_DEPTH) }; }
      head = false;
    } else if (c === '"') {
      let closed = false, escaped = false;
      while (last + 1 < source.length && !closed && !lineEnd(source[last + 1])) {
        last++;
        if (escaped) escaped = false;
        else if (source[last] === '\\') escaped = true;
        else if (source[last] === '"') closed = true;
      }
      for (let k = i; k <= last; k++) marks[k] = { cls: closed ? 'text' : 'unterminated', depth: 0 };
      head = false;
    } else {
      while (last + 1 < source.length && wordCharacter(source[last + 1])) last++;
      const cls = wordClass(source.slice(i, last + 1), head);
      for (let k = i; k <= last; k++) marks[k] = { cls, depth: 0 };
      head = false;
    }
    i = last + 1;
  }
  return marks;
}

// The source as DOM-ready runs: [{ text, cls, depth }].
export function runs(source) {
  const marks = classify(source), out = [];
  for (let i = 0; i < source.length; i++) {
    const m = marks[i], prev = out[out.length - 1];
    if (prev && prev.cls === m.cls && prev.depth === m.depth && m.cls !== 'delimiter') prev.text += source[i];
    else out.push({ text: source[i], cls: m.cls, depth: m.depth });
  }
  return out;
}

// CCL.Highlighting.Balance: whether Enter should run the entry.
export function balance(source) {
  let depth = 0, stray = false, content = false, quoted = false, escaped = false, comment = false;
  for (const c of source) {
    if (comment) { if (lineEnd(c)) comment = false; }
    else if (quoted) {
      if (escaped) escaped = false;
      else if (c === '\\') escaped = true;
      else if (c === '"') quoted = false;
      else if (lineEnd(c)) { stray = true; quoted = false; }
    } else if (c === '#') comment = true;
    else if (c === '"') { quoted = true; content = true; }
    else if (c === '(') { depth++; content = true; }
    else if (c === ')') { if (depth === 0) stray = true; else depth--; content = true; }
    else if (!space(c)) content = true;
  }
  return !content ? 'empty' : stray || quoted ? 'malformed' : depth > 0 ? 'open' : 'complete';
}
