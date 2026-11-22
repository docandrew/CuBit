#!/usr/bin/env python3
"""tools/build_bible.py's USFM reader on hand-made books (APP-005): the
words of Christ (\\wj) as a run attribute, across verses and paragraphs and
around nested \\+w; joined runs; the marker's separating space; refusals;
the King James Version's pilcrows as paragraph breaks; the Latin letters."""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / 'tools'))
import build_bible as bb  # noqa: E402

failures = 0


def check(condition, label):
    global failures
    if not condition:
        failures += 1
        print('FAIL: ' + label)


def segments(source):
    book = bb.parse_book('JHN', '\\id JHN\n\\c 1\n' + source + '\n')
    return {n: v.segments for n, v in book.chapters[1].items()}, book


# A span from verse 1 into verse 2 and across a paragraph into verse 3.
s, book = segments('\\p\n\\v 1 He said, \\wj “\\+w Truly|strong="G1"\\+w* I say\n'
                   '\\v 2 to you\n\\p\n\\v 3 all things.”\\wj* And he went.')
check(s[1] == [[bb.PARAGRAPH, 0, 'He said,'], [bb.PROSE, bb.WORDS_OF_CHRIST, '“Truly I say']],
      'verse 1: plain, then red from the quote: %r' % s[1])
check(s[2] == [[bb.PROSE, bb.WORDS_OF_CHRIST, 'to you']], 'verse 2: red throughout: %r' % s[2])
check(s[3] == [[bb.PARAGRAPH, bb.WORDS_OF_CHRIST, 'all things.”'], [bb.PROSE, bb.JOINED, 'And he went.']]
      or s[3] == [[bb.PARAGRAPH, bb.WORDS_OF_CHRIST, 'all things.”'], [bb.PROSE, 0, 'And he went.']],
      'verse 3: red to the close, then plain: %r' % s[3])
check(book.wj == (1, 2, 1), 'one span, into two later verses and one later paragraph: %r' % (book.wj,))

# A comma straight after the close is joined to the red run.
s, _ = segments('\\p\n\\v 1 \\wj Follow me\\wj*, he said.')
check(s[1] == [[bb.PARAGRAPH, bb.WORDS_OF_CHRIST, 'Follow me'], [bb.PROSE, bb.JOINED, ', he said.']],
      'a joined comma: %r' % s[1])

# The space after an opening marker separates; it is not text.
s, _ = segments('\\p\n\\v 1 \\w can|strong="G1"\\w*’\\w t|strong="G2"\\w* go.')
check(s[1] == [[bb.PARAGRAPH, 0, 'can’t go.']], 'no space inside a split word: %r' % s[1])

# A span left open, or a close with nothing open, is refused.
for bad in ('\\p\n\\v 1 \\wj open', '\\p\n\\v 1 shut\\wj*'):
    try:
        segments(bad)
        check(False, 'refused: %r' % bad)
    except SystemExit:
        pass

# The King James Version's pilcrows: one starting a verse starts a
# paragraph there (also after \wj, and when a paragraph already starts);
# one anywhere else is left out; none is ever text.
s, book = segments('\\p\n\\v 1 In the beginning.\n\\v 2 ¶ And God said.\n\\v 3 \\wj ¶ \\+w Ye|strong="G1"\\+w* are '
                   'the salt.\\wj*\n\\p\n\\v 4 ¶ Again.\n\\v 5 Then ¶ he went.')
check(s[1] == [[bb.PARAGRAPH, 0, 'In the beginning.']], 'verse 1 as it was: %r' % s[1])
check(s[2] == [[bb.PARAGRAPH, 0, 'And God said.']], 'a pilcrow starting a verse is a paragraph: %r' % s[2])
check(s[3] == [[bb.PARAGRAPH, bb.WORDS_OF_CHRIST, 'Ye are the salt.']],
      'a pilcrow after \\wj is a paragraph, red kept: %r' % s[3])
check(s[4] == [[bb.PARAGRAPH, 0, 'Again.']], 'a paragraph already started stays one: %r' % s[4])
check(s[5] == [[bb.PROSE, 0, 'Then he went.']], 'a pilcrow inside a verse is left out: %r' % s[5])
check(book.pilcrows == (3, 1), 'three pilcrows made paragraphs, one left out: %r' % (book.pilcrows,))
check(bb.PILCROW not in bb.DRAWN, 'the pilcrow is not a drawn character')
for code, ch in (('ae', '\u00e6'), ('oe', '\u0153'), ('e', '\u00eb')):
    check(ch in bb.DRAWN and bb.words_of('x' + ch + 'y') == ['x' + code + 'y'], 'Latin %r is drawn and folds' % ch)

print('build_bible reader tests: %d failures' % failures)
sys.exit(1 if failures else 0)
