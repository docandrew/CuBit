// Values as people read them, by their type: CCL.Units (ccl-units.adb),
// checked against its golden vectors (units-vectors.json). The value itself
// is unchanged; sorting, filtering and arithmetic use the number.
export const UNITS = { Bytes: 'Bytes', Timestamp: 'Timestamp',
  UNIX_File_Permissions: 'UNIX_File_Permissions', Milliseconds: 'Milliseconds' };
export const unitOf = typeName => Object.hasOwn(UNITS, typeName) ? UNITS[typeName] : null;

const two = n => (n < 10n ? '0' : '') + (n % 100n).toString();
function size(v) {
  if (v < 1024n) return `${v} B`;
  const prefixes = ['B', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB', 'EiB'];
  let scale = 1n, step = 0;
  while (v / scale >= 1024n && step < prefixes.length - 1) { scale *= 1024n; step++; }
  let tenths = (v % scale) / (scale / 10n);
  if (tenths > 9n) tenths = 9n; // scale / 10 rounds down
  return `${v / scale}.${tenths} ${prefixes[step]}`;
}
function instant(v) {
  const minutes = v / 60000n, days = minutes / 1440n, minute = minutes % 1440n;
  const z = days + 719468n, era = z / 146097n, doe = z - era * 146097n;
  const yoe = (doe - doe / 1460n + doe / 36524n - doe / 146096n) / 365n;
  const doy = doe - (365n * yoe + yoe / 4n - yoe / 100n);
  const mp = (5n * doy + 2n) / 153n, day = doy - (153n * mp + 2n) / 5n + 1n;
  const month = mp < 10n ? mp + 3n : mp - 9n, year = yoe + era * 400n + (month <= 2n ? 1n : 0n);
  return `${year}-${two(month)}-${two(day)} ${two(minute / 60n)}:${two(minute % 60n)}`;
}
function mode(v) {
  const kind = v & 0o170000n;
  let s = kind === 0o040000n ? 'd' : kind === 0o120000n ? 'l' : (kind === 0o100000n || kind === 0n) ? '-' : '?';
  for (let bit = 0n; bit < 9n; bit++) s += (v & (1n << (8n - bit))) ? 'rwx'[Number(bit % 3n)] : '-';
  return s;
}
function duration(v) {
  if (v < 1000n) return `${v} ms`;
  if (v < 60000n) return `${v / 1000n}.${(v % 1000n) / 100n} s`;
  return `${v / 60000n} min ${two((v / 1000n) % 60n)} s`;
}
export function humanize(unit, text) {
  if (!unit || !/^[0-9]{1,19}$/.test(text)) return text;
  const v = BigInt(text);
  switch (unit) {
    case UNITS.Bytes: return size(v);
    case UNITS.Timestamp: return v === 0n ? '-' : instant(v);
    case UNITS.UNIX_File_Permissions: return v === 0n ? '-' : mode(v);
    case UNITS.Milliseconds: return duration(v);
    default: return text;
  }
}
