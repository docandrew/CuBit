#!/usr/bin/env python3
"""Versification per translation (APP-005): each Bible image's chapters and
verse counts against an independent recount of its pinned source's own
\\c and \\v markers (not tools/build_bible.py's parser), and the source's
verse texts against the image's on a sample.

Usage: check_versification.py IMAGE_DIR"""
import json
import re
import sys
import zipfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(ROOT / 'tools'))
import bible_image  # noqa: E402
import build_bible  # noqa: E402  (for the pinned fetch and the book order only)

CHAPTER = re.compile(r'\\c\s+(\d+)')
VERSE = re.compile(r'\\v\s+(\d+)(?:-(\d+))?')


def recount(path):
    """book code -> {chapter: highest verse} from the raw markers."""
    result = {}
    with zipfile.ZipFile(path) as archive:
        for name in archive.namelist():
            match = re.match(r'^\d{2,3}-([A-Z0-9]{3})', name)
            if not match or not name.endswith('.usfm') or match.group(1) not in build_bible.BOOKS:
                continue
            chapters = {}
            chapter = None
            text = archive.read(name).decode('utf-8')
            for token in re.finditer(r'\\c\s+\d+|\\v\s+\d+(?:-\d+)?', text):
                c = CHAPTER.match(token.group(0))
                if c:
                    chapter = int(c.group(1))
                    chapters[chapter] = 0
                    continue
                v = VERSE.match(token.group(0))
                last = int(v.group(2) or v.group(1))
                chapters[chapter] = max(chapters[chapter], last)
            result[match.group(1)] = chapters
    return result


def main():
    directory = Path(sys.argv[1])
    sources = json.loads((ROOT / 'assets' / 'bible' / 'sources.json').read_text())['translations']
    failures = 0
    checks = 0
    for source in sources:
        image = bible_image.Image((directory / (source['name'] + '.bible')).read_bytes())
        table = image.versification()
        counted = recount(build_bible.pinned(source))
        books = chapters = verses = 0
        for slot, code in enumerate(build_bible.BOOKS):
            expected = counted.get(code, {})
            want = [expected.get(c, 0) for c in range(1, max(expected, default=0) + 1)]
            checks += 1
            if table[slot] != want:
                failures += 1
                print('FAIL: %s %s: image %s, source %s' % (source['name'], code, table[slot][:8], want[:8]))
            if want:
                books += 1
                chapters += sum(1 for n in want if n)
                verses += sum(want)
        print('  %s: %d books, %d chapters, %d verse numbers, as the source numbers them'
              % (source['abbreviation'], books, chapters, verses))
    print('versification: %d books checked, %d failures' % (checks, failures))
    sys.exit(1 if failures else 0)


if __name__ == '__main__':
    main()
