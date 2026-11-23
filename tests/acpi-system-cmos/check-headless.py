#!/usr/bin/env python3
"""Read-only checks of the real headless startup trace; never launches QEMU."""
from pathlib import Path
import argparse, hashlib, json, re

PATTERNS = {
    'boot_rtc_sample': r'devmgr: RTC registers ',
    'devmgr_claim_transferred': r'devmgr: cmos ledger handover\s+TRANSFERRED\s+nmi-disabled=FALSE',
    'exact_bootstrap_grant': r'devmgr: cmos bootstrap grant sent ports70/71',
    'procmgr_cached_receipt': r'procmgr: cmos bootstrap receipt cached',
    'actual_guarded_handler_read': r'acpi: cmos-handler probe status=COMPLETED\s+byte=\s*(?:[0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\s+scope-ended=TRUE',
}

def assess(text):
    matches = {name: re.search(pattern, text) for name, pattern in PATTERNS.items()}
    checks = {name: match is not None for name, match in matches.items()}
    checks['rtc_before_transfer'] = bool(matches['boot_rtc_sample'] and matches['devmgr_claim_transferred'] and matches['boot_rtc_sample'].start() < matches['devmgr_claim_transferred'].start())
    checks['receipt_before_handler'] = bool(matches['procmgr_cached_receipt'] and matches['actual_guarded_handler_read'] and matches['procmgr_cached_receipt'].start() < matches['actual_guarded_handler_read'].start())
    checks['no_failed_handover'] = not any(s in text for s in [
        'cmos bootstrap grant not accepted', 'optional cmos handover unavailable',
        'cmos-handler probe unavailable', 'cmos ledger handover HANDOVER_REFUSED',
        'cmos ledger handover HANDOVER_NEEDS_ROOM'])
    return checks

parser = argparse.ArgumentParser()
parser.add_argument('serial', nargs='?', type=Path)
parser.add_argument('--receipt', type=Path)
parser.add_argument('--self-test', action='store_true')
args = parser.parse_args()
if args.self_test:
    good = '\n'.join(['devmgr: RTC registers valid', 'devmgr: cmos ledger handover TRANSFERRED nmi-disabled=FALSE',
                      'procmgr: cmos bootstrap receipt cached', 'devmgr: cmos bootstrap grant sent ports70/71',
                      'acpi: cmos-handler probe status=COMPLETED byte= 128 scope-ended=TRUE'])
    examples = [(good, True)]
    for line in good.splitlines(): examples.append((good.replace(line, ''), False))
    examples += [(good.replace('byte= 128', 'byte= 256'), False),
                 (good.replace('scope-ended=TRUE', 'scope-ended=FALSE'), False),
                 (good + '\nprocmgr: optional cmos handover unavailable', False)]
    for text, expected in examples: assert all(assess(text).values()) == expected
    print(json.dumps(dict(self_test=True, passed=len(examples), total=len(examples), percent=100)))
    raise SystemExit
assert args.serial and args.serial.is_file(), 'Provide the headless serial log'
raw = args.serial.read_bytes(); checks = assess(raw.decode(errors='replace'))
result = dict(completed=True, passed=sum(checks.values()), total=len(checks), percent=100 * sum(checks.values()) / len(checks),
              checks=checks, serial=str(args.serial.resolve()), serial_sha256=hashlib.sha256(raw).hexdigest(),
              scope='Real startup trace gate. Transfer log is supported by the proved ledger handover postcondition; this parser alone is not a physical ownership proof.')
if args.receipt: args.receipt.write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps(result, indent=2))
raise SystemExit(0 if all(checks.values()) else 1)
