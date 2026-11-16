#!/usr/bin/env python3
"""SEC-025: procmgr reads files (ELF images, init.ccl, generations) into
imageBuf, after the exchange pages it lends for as long as it runs
(filesystem paths, config, tls.svc and metrics.svc scope lists), so no
peer can rewrite an image between procmgr's checks, its fingerprint and
the kernel's load. Static check of the layout; fails if any image reader,
the file windows or the spawn use the lent pages again, or if a lasting
loan covers more than the exchange pages."""
import pathlib
import re
import sys

SOURCE = pathlib.Path(__file__).resolve().parents[2] / 'userspace/services/procmgr/main.adb'
text = SOURCE.read_text()
problems = []
def need(pattern, what):
    if not re.search(pattern, text):
        problems.append(f'missing: {what}')
def forbid(pattern, what):
    for m in re.finditer(pattern, text):
        line = text.count('\n', 0, m.start()) + 1
        problems.append(f'line {line}: {what}')

need(r'function imageBuf return System\.Address is \(elfBuf \+ Storage_Offset \(EXCHANGE_BYTES\)\)',
     'imageBuf after the exchange pages')
need(r'EXCHANGE_BYTES : constant := INITIAL_BUF_PAGES \* PAGE_SIZE;', 'the exchange pages are the lent pages')
need(r'Image => imageBuf, Bytes => elfSize', 'the spawn reads imageBuf')
need(r'localAddr => imageBuf \+ Storage_Offset \(Offset\)', 'file windows lend imageBuf pages')
forbid(r'Image => elfBuf', 'the spawn reads the lent pages')
forbid(r'localAddr => elfBuf \+', 'a file window lends the exchange pages')
# Image readers: section, signature, description and header-field overlays.
forbid(r'Address => elfBuf \+ Storage_Offset \((offset|sh_offset|Section_At|Signature_At|Description_At|Found_At)\)',
       'an image reader overlays the lent pages')
# Every lasting loan of elfBuf covers exactly the exchange pages.
for m in re.finditer(r'Create_Via_Capability\s*\(([^;=]*?)\belfBuf\b\s*,\s*([A-Za-z_0-9]+)', text):
    if m.group(2) != 'INITIAL_BUF_PAGES':
        forbid(re.escape(m.group(0)), f'a loan of elfBuf covers {m.group(2)} pages')
for m in re.finditer(r'localAddr\s*=>\s*elfBuf\s*,\s*numPages\s*=>\s*([A-Za-z_0-9]+)', text):
    if m.group(1) != 'INITIAL_BUF_PAGES':
        problems.append(f'a loan of elfBuf covers {m.group(1)} pages')
if problems:
    for p in problems:
        print(f'SEC-025-PROCMGR-IMAGE: FAIL {p}')
    sys.exit(1)
print('SEC-025-PROCMGR-IMAGE: PASS images in private pages; lasting loans cover only the exchange pages')
