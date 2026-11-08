#!/usr/bin/env python3
"""Compile public-domain Bible texts into CuBit's read-only Bible images.

Build-time only (APP-005). The sources are pinned in assets/bible/sources.json
by URL and SHA-256: each is fetched once (https only, curl with an empty
environment), checked against its pin, and refused on a mismatch. A fetched
file is untrusted data. It is read as a zip archive of USFM text with
bounded sizes and checked names, and nothing in it is ever executed.

Output, per translation, <out>/<name>.bible: the binary layout read by
userspace/lib/bible (Bible_Images), and <out>/BIBLE-NOTICE with every
source's URL, digest and licence statement.

Image layout (all little-endian):

  header   magic "CUBITBIB", version u32, books u32 (Bible_Canon.BOOKS),
           properties u32 (bit 0: the source marks the words of Christ;
           the other bits 0), words-of-Christ spans u32 (0 exactly when
           bit 0 is clear), then (offset u32, count u32) for each table, in
           this order:
  books    8 bytes:  first chapter u32, chapter count u16, reserved u16
  chapters 8 bytes:  first verse u32, verse count u16 (the last verse's
                     number), reserved u16
  verses   8 bytes:  first segment u32, segment count u16 (0: the
                     translation has no text for this verse), reserved u16
  segments 8 bytes:  text offset u32, length u16, kind u8, flags u8
                     (1: a blank line before; 2: the words of Christ; 4:
                     joined to the run before, no space between)
  words    16 bytes: key offset u32, key length u16, reserved u16,
                     first posting u32, posting count u32 (sorted by key)
  postings 4 bytes:  verse u32 (ascending within a word)
  keys     1 byte:   search keys, ASCII lower case
  text     1 byte:   UTF-8
  about    8 bytes:  text offset u32, length u16, kind u8, reserved u8
                     (kinds: abbreviation, title, licence, language code
                     (ISO 639-1), language name)

Books are in Bible_Canon.Book order; chapters and verses are numbered from 1
with no gaps (a missing verse has no segments). A verse's segments are its
section headings, then its text runs; a run starting a paragraph or a poetry
line says so in its kind. The King James Version's pilcrows (its 1769 paragraph
marks) are not text: one that starts a verse starts a paragraph there, any
other is left out. Search keys are the verses' words normalized as
Bible_Search.Normalize does: ASCII letters and digits in lower case, inner
apostrophes dropped, the few accented letters folded (ae, oe, a, e, o).

Usage:
  build_bible.py OUT_DIR [--only NAME] [--summary FILE]
  build_bible.py --pin NAME     fetch NAME's URL and print its SHA-256
  build_bible.py --font NAME    print the path of the pinned font NAME (the
                                reading face and the preview candidates)
"""
import argparse
import json
import os
import re
import shutil
import struct
import subprocess
import sys
import tempfile
import zipfile
from hashlib import sha256
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SOURCES = ROOT / 'assets' / 'bible' / 'sources.json'

MAGIC = b'CUBITBIB'
FORMAT_VERSION = 3
TABLES = ('books', 'chapters', 'verses', 'segments', 'words', 'postings',
          'keys', 'text', 'about')
HEADER_BYTES = 8 + 4 + 4 + 4 + 4 + 8 * len(TABLES)
# Header properties (Bible_Images.Layout): the source marks the words of
# Christ (USFM \wj), so red letters can be shown.
PROPERTY_WORDS_OF_CHRIST = 1
MAXIMUM_SPANS = 1_000_000

# Bible_Canon.Book, in order, by USFM code.
BOOKS = (
    'GEN EXO LEV NUM DEU JOS JDG RUT 1SA 2SA 1KI 2KI 1CH 2CH EZR NEH EST JOB '
    'PSA PRO ECC SNG ISA JER LAM EZK DAN HOS JOL AMO OBA JON MIC NAM HAB ZEP '
    'HAG ZEC MAL '
    'TOB JDT ESG WIS SIR BAR LJE S3Y SUS BEL DAG 1MA 2MA 3MA 4MA 1ES 2ES MAN '
    'PS2 '
    'MAT MRK LUK JHN ACT ROM 1CO 2CO GAL EPH PHP COL 1TH 2TH 1TI 2TI TIT PHM '
    'HEB JAS 1PE 2PE 1JN 2JN 3JN JUD REV').split()
assert len(BOOKS) == 85 and len(set(BOOKS)) == 85

# Bible_References ceilings: a source past them is refused (Capacity).
MAXIMUM_CHAPTER = 150
MAXIMUM_VERSE = 176
MAXIMUM_CHAPTERS = 2048
# Bible_Images ceilings.
MAXIMUM_SEGMENT_BYTES = 8192
MAXIMUM_VERSE_SEGMENTS = 64
MAXIMUM_KEY = 32
# Input bounds: a USFM file, and the whole archive, uncompressed.
MAXIMUM_SOURCE_FILE = 8 * 1024 * 1024
MAXIMUM_SOURCE_ARCHIVE = 64 * 1024 * 1024
MAXIMUM_DOWNLOAD = 64 * 1024 * 1024

# Bible_Images.Segment_Kind'Pos.
PROSE, PARAGRAPH, FLUSH_PARAGRAPH, POETRY_1, POETRY_2, POETRY_3, \
    SECTION_HEADING, MAJOR_HEADING, PSALM_TITLE, PROLOGUE = range(10)
# Not verse text: headings, and a prologue before a chapter's first verse
# (Sirach's translator's prologue).
HEADINGS = (SECTION_HEADING, MAJOR_HEADING, PSALM_TITLE, PROLOGUE)
# Bible_Images segment flags.
STANZA_BREAK = 1
# The run is the words of Christ (USFM \wj ... \wj*): red letters.
WORDS_OF_CHRIST = 2
# The run follows the one before with no space between (a style changed
# inside a line: "\wj Verily ...\wj*," leaves the comma joined).
JOINED = 4
STYLE_FLAGS = WORDS_OF_CHRIST
# Bible_Images.About_Kind'Pos.
ABOUT_ABBREVIATION, ABOUT_TITLE, ABOUT_LICENCE, ABOUT_LANGUAGE_CODE, ABOUT_LANGUAGE_NAME = range(5)
# A language: an ISO 639-1 code and its English name (Bible_Translations.Language).
LANGUAGE_CODE = re.compile('[a-z]{2}')
LANGUAGE_NAME = re.compile('[A-Z][a-z]+( [A-Z][a-z]+)*')

# USFM paragraph markers -> the kind of the run they start.
PARAGRAPHS = {
    'p': PARAGRAPH, 'pi': PARAGRAPH, 'pi1': PARAGRAPH, 'pi2': PARAGRAPH, 'pc': PARAGRAPH,
    'ph': PARAGRAPH, 'ph1': PARAGRAPH,
    'm': FLUSH_PARAGRAPH, 'mi': FLUSH_PARAGRAPH, 'pmo': FLUSH_PARAGRAPH, 'pm': FLUSH_PARAGRAPH,
    'q': POETRY_1, 'q1': POETRY_1, 'qr': POETRY_1, 'qc': POETRY_1, 'qm1': POETRY_1,
    'li': POETRY_1, 'li1': POETRY_1,
    'q2': POETRY_2, 'qm2': POETRY_2, 'li2': POETRY_2,
    'q3': POETRY_3, 'li3': POETRY_3,
}
# USFM heading markers -> heading kind (their text is not verse text).
HEADING_MARKERS = {
    's': SECTION_HEADING, 's1': SECTION_HEADING, 's2': SECTION_HEADING, 's3': SECTION_HEADING,
    'qa': SECTION_HEADING, 'sp': SECTION_HEADING,
    'ms': MAJOR_HEADING, 'ms1': MAJOR_HEADING, 'mr': MAJOR_HEADING,
    'd': PSALM_TITLE,
}
# Markers whose whole line is left out: book identification, titles,
# introductions, chapter labels and parallel references.
SKIPPED_LINES = {
    'id', 'ide', 'h', 'toc1', 'toc2', 'toc3', 'mt', 'mt1', 'mt2', 'mt3', 'imt', 'imt1', 'imt2',
    'is', 'is1', 'is2', 'ip', 'ipi', 'im', 'ili', 'ili1', 'ili2', 'iot', 'io1', 'io2', 'ie', 'rem',
    'cl', 'cp', 'cd', 'r', 'sr', 'periph',
}
# Notes, whose content is left out: footnotes and cross-references.
NOTES = {'f', 'fe', 'x', 'ef', 'ex'}
# Paragraph-level markers with no text of their own.
NO_BREAK = {'nb'}
BLANK = {'b'}

# The characters the reading faces draw (CuBit.Fonts reading slots: printable
# ASCII and Windows-1252 above it; tests/bible/test_faces.py checks each
# pinned face maps every one), and those Bible_Text folds to one of them.
# Anything else in verse text is refused, so nothing renders "?". The pilcrow
# is not among them: it is a paragraph mark, never text (PILCROW below).
DRAWN = set(chr(c) for c in range(0x20, 0x7F)) | set('\u2018\u2019\u201c\u201d\u2013\u2014\u2026'
                                                      '\u00e6\u00c6\u00e0\u00e9\u00d7\u00bd\u00a0'
                                                      '\u00eb\u00cb\u0153\u0152')
FOLDED = {'\u0113': 'e', '\u014d': 'o'}
# Hebrew letters name the stanzas of Psalm 119 in the KJV's headings
# ("\u05d0 ALEPH."); v1 draws no Hebrew (APP-005, out of scope), so they are
# left out of headings, and only of headings.
HEBREW = re.compile('[\u0590-\u05ff]+')

# Search keys: Bible_Search.Normalize (keep the two in step).
KEY_FOLDS = {'\u00e6': 'ae', '\u00c6': 'ae', '\u00e0': 'a', '\u00e9': 'e', '\u0113': 'e', '\u014d': 'o',
             '\u00eb': 'e', '\u00cb': 'e', '\u0153': 'oe', '\u0152': 'oe'}
# The King James Version's paragraph mark: a paragraph where it starts a
# verse, left out anywhere else.
PILCROW = '\u00b6'
APOSTROPHES = "'\u2019"


def fail(message):
    sys.exit('build_bible: ' + message)


# ---------------------------------------------------------------------------
# Pinned fetch.
# ---------------------------------------------------------------------------
def cache_dir():
    base = os.environ.get('XDG_CACHE_HOME') or os.path.join(os.path.expanduser('~'), '.cache')
    return Path(base) / 'cubit' / 'pinned'


def digest_of(path):
    h = sha256()
    with open(path, 'rb') as f:
        for chunk in iter(lambda: f.read(1 << 20), b''):
            h.update(chunk)
    return h.hexdigest()


def download(url, into):
    """curl with an empty environment, https only, size-bounded."""
    if not url.startswith('https://'):
        fail('refusing a non-https source: ' + url)
    curl = shutil.which('curl')
    if curl is None:
        fail('curl is not on PATH: a source that is not yet fetched needs it')
    result = subprocess.run(
        [curl, '-sSfL', '--proto', '=https', '--proto-redir', '=https',
         '--max-filesize', str(MAXIMUM_DOWNLOAD), '-o', str(into), url],
        env={}, check=False)
    if result.returncode != 0:
        fail('fetching %s failed (curl exit status %d)' % (url, result.returncode))


def pinned(source):
    """The source's bytes as a local path, checked against its pin. Looks in
    the CCL build store (files/<sha256>), then this tool's cache, then
    fetches into a new directory of its own."""
    want = source['sha256']
    if not re.fullmatch('[0-9a-f]{64}', want):
        fail('%s: malformed sha256 pin' % source['name'])
    candidates = []
    store = os.environ.get('CCL_BUILD_STORE')
    if store:
        candidates.append(Path(store) / 'files' / want)
    cached = cache_dir() / want
    candidates.append(cached)
    for candidate in candidates:
        if candidate.is_file() and digest_of(candidate) == want:
            return candidate
    cached.parent.mkdir(parents=True, exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix='bible-fetch-', dir=str(cached.parent)))
    try:
        target = work / 'download'
        download(source['url'], target)
        got = digest_of(target)
        if got != want:
            fail('%s (%s): SHA-256 is %s, sources.json pins %s; refused, nothing kept'
                 % (source['name'], source['url'], got, want))
        os.replace(target, cached)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    return cached


# ---------------------------------------------------------------------------
# USFM: a token stream (markers, text, line ends) folded into books,
# chapters, verses and segments.
# ---------------------------------------------------------------------------
FILE_NAME = re.compile(r'^\d{2,3}-([A-Z0-9]{3})[A-Za-z0-9-]*\.usfm$')
TOKEN = re.compile(r'\\(\+?)([a-z]+[0-9]*)(\*?)|(\n)|([^\\\n]+)')
NUMBER = re.compile(r'\s*(\d+)(?:-(\d+))?')
# Archive members that are not books of the canon.
NOT_BOOKS = {'FRT', 'GLO', 'BAK', 'OTH', 'INT', 'CNC', 'TDX'}


class Verse:
    def __init__(self):
        self.segments = []   # [kind, flags, text]


class Book:
    def __init__(self, code):
        self.code = code
        self.chapters = {}   # number -> {verse number -> Verse}
        self.wj = (0, 0, 0)  # words-of-Christ spans, across verses, across paragraphs
        self.pilcrows = (0, 0)  # pilcrows made paragraphs, pilcrows left out


def read_archive(path):
    """USFM files of the archive by book code, bounded and name-checked."""
    files = {}
    total = 0
    with zipfile.ZipFile(path) as archive:
        for info in archive.infolist():
            name = info.filename
            if info.is_dir() or not name.endswith('.usfm'):
                continue
            match = FILE_NAME.match(name)
            if not match:
                fail('unexpected file name in %s: %r' % (path, name))
            if info.file_size > MAXIMUM_SOURCE_FILE:
                fail('%s: %s is larger than %d bytes' % (path, name, MAXIMUM_SOURCE_FILE))
            total += info.file_size
            if total > MAXIMUM_SOURCE_ARCHIVE:
                fail('%s: more than %d bytes of USFM' % (path, MAXIMUM_SOURCE_ARCHIVE))
            data = archive.read(info)
            if len(data) != info.file_size:
                fail('%s: %s is not the size its directory says' % (path, name))
            code = match.group(1)
            if code in files:
                fail('%s: two files for %s' % (path, code))
            files[code] = data.decode('utf-8')
    return files


def clean(text):
    return re.sub(r'\s+', ' ', text)


class BookReader:
    """One book's USFM, token by token."""

    def __init__(self, code):
        self.book = Book(code)
        self.chapter = None
        self.verse = None
        self.pending_kind = None   # the kind of the next run (a paragraph marker)
        self.pending_flags = 0
        self.headings = []         # (kind, text), waiting for the next verse
        self.heading = None        # [kind, text] while a heading line is read
        self.note_depth = 0
        self.skipping = False      # a skipped line, until its end
        self.expect = None         # 'c' or 'v': the next text starts with its number
        self.styles = []           # open character styles
        # The words of Christ: open from \wj to \wj*, across verses and
        # paragraphs (counted, for the build's report).
        # USFM: one space after an opening marker separates it from its
        # text ("\\+w Most"); it is not text ("“\\+w Most" is "“Most").
        self.separator = False
        self.wj = False
        self.verse_has_text = False   # the verse's first words are read
        self.pilcrow_paragraphs = 0
        self.pilcrows_dropped = 0
        self.wj_spans = 0
        self.wj_across_verses = 0
        self.wj_across_paragraphs = 0

    def where(self):
        return '%s %s' % (self.book.code, self.chapter)

    def end_line(self):
        self.skipping = False
        if self.heading is not None:
            text = clean(HEBREW.sub('', self.heading[1])).strip()
            if text:
                self.headings.append((self.heading[0], text))
            self.heading = None

    def text(self, text):
        if self.separator:
            self.separator = False
            if text.startswith(' ') and self.expect is None:
                text = text[1:]
        if self.expect is not None:
            match = NUMBER.match(text)
            if not match:
                fail('%s: \\%s without a number: %r' % (self.where(), self.expect, text[:40]))
            first, last = int(match.group(1)), int(match.group(2) or match.group(1))
            text = text[match.end():]
            if self.expect == 'c':
                self.start_chapter(first)
                text = ''          # a chapter marker's line holds only the number
            else:
                self.start_verse(first, last)
                if text.startswith(' '):
                    text = text[1:]
            self.expect = None
        if self.note_depth > 0 or self.skipping or not text:
            return
        if self.styles and self.styles[-1] == 'w' and '|' in text:
            text = text[:text.index('|')]
        if self.heading is not None:
            self.heading[1] += text
            return
        verse = self.verse
        if verse is None:
            if self.chapter is None:
                if text.strip():
                    fail('%s: text before the first chapter: %r' % (self.book.code, text[:60]))
                return
            # A prologue: it heads the chapter's first verse.
            if self.pending_kind is not None or not self.headings or self.headings[-1][0] != PROLOGUE:
                if text.strip():
                    self.headings.append((PROLOGUE, text.lstrip()))
                    self.pending_kind = None
            else:
                self.headings[-1] = (PROLOGUE, self.headings[-1][1] + text)
            return
        if not self.verse_has_text and text.strip():
            self.verse_has_text = True
            if text.lstrip().startswith(PILCROW):
                # A pilcrow starting a verse: a paragraph starts there.
                text = text.lstrip()[1:]
                if text.startswith(' '):
                    text = text[1:]
                if self.pending_kind is None:
                    self.pending_kind = PARAGRAPH
                self.pilcrow_paragraphs += 1
                if not text.strip():
                    return
        if PILCROW in text:
            self.pilcrows_dropped += text.count(PILCROW)
            text = re.sub(PILCROW + ' ?', '', text)
            if not text:
                return
        style = WORDS_OF_CHRIST if self.wj else 0
        text = clean(text)
        segments = verse.segments
        open_run = bool(segments) and segments[-1][0] not in HEADINGS
        if self.pending_kind is not None or not open_run:
            if not text.strip():
                return
            kind = self.pending_kind if self.pending_kind is not None else PROSE
            segments.append([kind, self.pending_flags | style, text.lstrip()])
            self.pending_kind = None
            self.pending_flags = 0
        elif segments[-1][1] & STYLE_FLAGS != style:
            # A style changes inside the line: a new run, joined to the one
            # before unless a space separates them.
            if not text.strip():
                segments[-1][2] += text
                return
            last = segments[-1][2]
            joined = 0 if last.endswith(' ') or text.startswith(' ') else JOINED
            segments.append([PROSE, style | joined, text.lstrip()])
        else:
            segments[-1][2] += text

    def start_chapter(self, number):
        if number in self.book.chapters:
            fail('%s: chapter %d twice' % (self.book.code, number))
        self.chapter = number
        self.book.chapters[number] = {}
        self.verse = None
        self.skipping = True

    def start_verse(self, first, last):
        if self.chapter is None:
            fail('%s: a verse before the first chapter' % self.book.code)
        if last < first:
            fail('%s: verse range %d-%d runs backwards' % (self.where(), first, last))
        verses = self.book.chapters[self.chapter]
        if self.wj:
            self.wj_across_verses += 1
        for number in range(first, last + 1):
            if number in verses:
                fail('%s:%d twice' % (self.where(), number))
            verses[number] = Verse()
        self.verse = verses[first]
        self.verse_has_text = False
        for kind, text in self.headings:
            self.verse.segments.append([kind, 0, clean(text).strip()])
        self.headings = []

    def marker(self, plus, name, close):
        if name in NOTES:
            self.note_depth += -1 if close else 1
            if self.note_depth < 0:
                fail('%s: unbalanced note marker' % self.where())
            return
        if self.note_depth > 0:
            return
        self.separator = not close
        if name == 'wj':
            if close:
                if not self.wj:
                    fail('%s: \\wj* closes nothing' % self.where())
                self.wj = False
            elif not self.wj:
                self.wj = True
                self.wj_spans += 1
            return
        if close:
            if self.styles and self.styles[-1] == name:
                self.styles.pop()
            return
        if name == 'c':
            self.end_line()
            self.expect = 'c'
        elif name == 'v':
            self.expect = 'v'
        elif plus:
            self.styles.append(name)
        elif name in PARAGRAPHS:
            self.pending_kind = PARAGRAPHS[name]
            self.styles = []
            if self.wj:
                self.wj_across_paragraphs += 1
        elif name in BLANK:
            self.pending_flags |= STANZA_BREAK
        elif name in NO_BREAK:
            pass
        elif name in HEADING_MARKERS:
            # Its text, to the line's end, heads the next verse.
            self.heading = [HEADING_MARKERS[name], '']
        elif name in SKIPPED_LINES:
            self.skipping = True
        else:
            self.styles.append(name)   # character style: keep its text

    def finish(self):
        self.end_line()
        if self.note_depth:
            fail('%s: a note is not closed' % self.where())
        if self.wj:
            fail('%s: \\wj is not closed by the end of the book' % self.where())
        self.book.wj = (self.wj_spans, self.wj_across_verses, self.wj_across_paragraphs)
        self.book.pilcrows = (self.pilcrow_paragraphs, self.pilcrows_dropped)
        for verses in self.book.chapters.values():
            for verse in verses.values():
                for segment in verse.segments:
                    segment[2] = clean(segment[2]).strip()
                verse.segments = [s for s in verse.segments if s[2]]
        return self.book


def parse_book(code, source):
    reader = BookReader(code)
    for match in TOKEN.finditer(source):
        plus, name, close, newline, text = match.groups()
        if newline is not None:
            in_heading = reader.heading is not None
            reader.end_line()
            if not in_heading:
                reader.text(' ')
        elif text is not None:
            reader.text(text)
        else:
            reader.marker(plus, name, close)
    return reader.finish()


def read_translation(path):
    files = read_archive(path)
    unknown = sorted(set(files) - set(BOOKS) - NOT_BOOKS)
    if unknown:
        fail('%s: books CuBit does not know: %s' % (path, ' '.join(unknown)))
    return {code: parse_book(code, files[code]) for code in BOOKS if code in files}


# ---------------------------------------------------------------------------
# Checks, text and keys.
# ---------------------------------------------------------------------------
def check_repertoire(name, code, chapter, number, text):
    for ch in text:
        if ch not in DRAWN and ch not in FOLDED:
            fail('%s %s %d:%d: U+%04X %r is not in the reading face; add it to the face or a fold'
                 % (name, code, chapter, number, ord(ch), ch))


def words_of(text):
    """Bible_Search.Normalize: search keys of a text."""
    keys = []
    current = []
    for ch in text:
        if 'a' <= ch <= 'z' or '0' <= ch <= '9':
            current.append(ch)
        elif 'A' <= ch <= 'Z':
            current.append(ch.lower())
        elif ch in KEY_FOLDS:
            current.append(KEY_FOLDS[ch])
        elif ch in APOSTROPHES and current:
            continue                   # "Lord's" is "lords"
        else:
            if current:
                keys.append(''.join(current))
                current = []
    if current:
        keys.append(''.join(current))
    return keys


def verse_text(verse):
    return ' '.join(s[2] for s in verse.segments if s[0] not in HEADINGS)


# ---------------------------------------------------------------------------
# The image.
# ---------------------------------------------------------------------------
class Blob:
    def __init__(self):
        self.data = bytearray()

    def add(self, text, limit):
        encoded = text.encode('utf-8')
        if len(encoded) > limit:
            fail('text longer than its field allows (%d > %d): %r' % (len(encoded), limit, text[:60]))
        offset = len(self.data)
        self.data += encoded
        return offset, len(encoded)


def build_image(source, books):
    name = source['name']
    text = Blob()
    keys = Blob()
    book_rows, chapter_rows, verse_rows, segment_rows = [], [], [], []
    index = {}
    summary = {}
    red = [0, 0, 0]
    red_runs = 0
    pilcrows = [0, 0]
    for code in BOOKS:
        book = books.get(code)
        if book is None or not book.chapters:
            book_rows.append(struct.pack('<IHH', 0, 0, 0))
            continue
        numbers = sorted(book.chapters)
        if numbers[0] < 1 or numbers[-1] > MAXIMUM_CHAPTER:
            fail('%s %s: chapters %d..%d, outside 1..Bible_References.MAXIMUM_CHAPTER'
                 % (name, code, numbers[0], numbers[-1]))
        # Chapters 1 .. the last; one the source lacks (the KJV's Additions
        # to Esther start at chapter 10) has no verses.
        book_rows.append(struct.pack('<IHH', len(chapter_rows), numbers[-1], 0))
        counts = []
        for chapter in range(1, numbers[-1] + 1):
            verses = book.chapters.get(chapter, {})
            if chapter in book.chapters and not verses:
                fail('%s %s %d: a chapter with no verses' % (name, code, chapter))
            last = max(verses) if verses else 0
            if last > MAXIMUM_VERSE:
                fail('%s %s %d: verse %d, past Bible_References.MAXIMUM_VERSE' % (name, code, chapter, last))
            chapter_rows.append(struct.pack('<IHH', len(verse_rows), last, 0))
            counts.append(last)
            for number in range(1, last + 1):
                verse = verses.get(number)
                segments = verse.segments if verse is not None else []
                if len(segments) > MAXIMUM_VERSE_SEGMENTS:
                    fail('%s %s %d:%d: %d segments' % (name, code, chapter, number, len(segments)))
                verse_rows.append(struct.pack('<IHH', len(segment_rows), len(segments), 0))
                for kind, flags, run in segments:
                    if kind not in HEADINGS:
                        check_repertoire(name, code, chapter, number, run)
                    else:
                        run = ''.join(FOLDED.get(ch, ch) if ch in DRAWN or ch in FOLDED else '' for ch in run)
                    offset, length = text.add(run, MAXIMUM_SEGMENT_BYTES)
                    if kind in HEADINGS and flags & (WORDS_OF_CHRIST | JOINED):
                        fail('%s %s %d:%d: a heading in a styled run' % (name, code, chapter, number))
                    red_runs += 1 if flags & WORDS_OF_CHRIST else 0
                    segment_rows.append(struct.pack('<IHBB', offset, length, kind, flags))
                verse_index = len(verse_rows) - 1
                for key in set(words_of(verse_text(verse))) if verse is not None else ():
                    if len(key) > MAXIMUM_KEY:
                        fail('%s %s %d:%d: search key %r longer than %d' % (name, code, chapter, number, key, MAXIMUM_KEY))
                    index.setdefault(key, []).append(verse_index)
        summary[code] = counts
        for i in range(3):
            red[i] += book.wj[i]
        for i in range(2):
            pilcrows[i] += book.pilcrows[i]
    if len(chapter_rows) > MAXIMUM_CHAPTERS:
        fail('%s: %d chapters, more than Bible_References.MAXIMUM_CHAPTERS' % (name, len(chapter_rows)))
    word_rows, posting_rows = [], []
    for key in sorted(index, key=lambda k: k.encode('ascii')):
        postings = sorted(index[key])
        offset, length = keys.add(key, MAXIMUM_KEY)
        word_rows.append(struct.pack('<IHHII', offset, length, 0, len(posting_rows), len(postings)))
        posting_rows.extend(struct.pack('<I', v) for v in postings)
    about_rows = []
    language = source.get('language') or {}
    if not LANGUAGE_CODE.fullmatch(language.get('code', '')) or not LANGUAGE_NAME.fullmatch(language.get('name', '')):
        fail('%s: a language needs an ISO 639-1 code and an English name: %r' % (name, language))
    for kind, value in ((ABOUT_ABBREVIATION, source['abbreviation']), (ABOUT_TITLE, source['title']),
                        (ABOUT_LICENCE, source['licence']), (ABOUT_LANGUAGE_CODE, language['code']),
                        (ABOUT_LANGUAGE_NAME, language['name'])):
        offset, length = text.add(value, 0xFFFF)
        about_rows.append(struct.pack('<IHBB', offset, length, kind, 0))
    tables = {
        'books': b''.join(book_rows), 'chapters': b''.join(chapter_rows),
        'verses': b''.join(verse_rows), 'segments': b''.join(segment_rows),
        'words': b''.join(word_rows), 'postings': b''.join(posting_rows),
        'keys': bytes(keys.data), 'text': bytes(text.data), 'about': b''.join(about_rows),
    }
    counts = {
        'books': len(book_rows), 'chapters': len(chapter_rows), 'verses': len(verse_rows),
        'segments': len(segment_rows), 'words': len(word_rows), 'postings': len(posting_rows),
        'keys': len(keys.data), 'text': len(text.data), 'about': len(about_rows),
    }
    directory = b''
    body = b''
    offset = HEADER_BYTES
    for table in TABLES:
        directory += struct.pack('<II', offset, counts[table])
        body += tables[table]
        offset += len(tables[table])
    spans = red[0]
    if spans > MAXIMUM_SPANS:
        fail('%s: %d spans of the words of Christ, more than %d' % (name, spans, MAXIMUM_SPANS))
    properties = PROPERTY_WORDS_OF_CHRIST if spans > 0 else 0
    image = MAGIC + struct.pack('<IIII', FORMAT_VERSION, len(BOOKS), properties, spans) + directory + body
    if len(image) >= 2 ** 31 - 2:
        fail('%s: image too large' % name)
    counts['red'] = (red[0], red[1], red[2], red_runs)
    counts['pilcrows'] = tuple(pilcrows)
    return image, summary, counts


def notice(sources):
    lines = ['CuBit Bible texts (APP-005): sources, digests and licences.', '']
    for source in sources:
        lines += ['%s: %s (%s)' % (source['abbreviation'], source['title'], source['language']['name']),
                  '  Source:    %s' % source['url'],
                  '  SHA-256:   %s' % source['sha256'],
                  '  Retrieved: %s' % source['retrieved'],
                  '  Publisher: %s' % source['publisher'],
                  '  Licence:   %s' % source['licence'],
                  '  Converted by tools/build_bible.py: footnotes, cross-references, Strong\'s',
                  '  numbers and introductions are left out, and the King James Version\'s',
                  '  pilcrows become paragraph breaks; the words are unchanged.', '']
    return '\n'.join(lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('out', nargs='?', help='output directory (Assets/cubit-bible/1)')
    parser.add_argument('--only', help='one translation (kjv, bsb, web, vul)')
    parser.add_argument('--summary', help='write chapters and verse counts as JSON')
    parser.add_argument('--pin', help='fetch a translation\'s or font\'s URL and print its SHA-256')
    parser.add_argument('--font', help='print the local path of the pinned font NAME')
    args = parser.parse_args()
    declared = json.loads(SOURCES.read_text())
    sources = declared['translations']
    fonts = declared.get('fonts', [])
    if args.font:
        font = next((f for f in fonts if f['name'] == args.font), None)
        if font is None:
            fail('no font named ' + args.font)
        print(pinned(font))
        return
    if args.pin:
        source = next((s for s in sources + fonts if s['name'] == args.pin), None)
        if source is None:
            fail('no source named ' + args.pin)
        work = Path(tempfile.mkdtemp(prefix='bible-pin-'))
        try:
            download(source['url'], work / 'download')
            print(digest_of(work / 'download'))
        finally:
            shutil.rmtree(work, ignore_errors=True)
        return
    if not args.out:
        fail('an output directory is required')
    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    summaries = {}
    for source in sources:
        if args.only and source['name'] != args.only:
            continue
        books = read_translation(pinned(source))
        image, summary, counts = build_image(source, books)
        target = out / (source['name'] + '.bible')
        partial = target.with_suffix('.bible.partial')
        partial.write_bytes(image)
        os.replace(partial, target)
        summaries[source['name']] = summary
        print('build_bible: %s %d bytes, %d books, %d chapters, %d verses, %d words'
              % (target, len(image), sum(1 for c in summary), counts['chapters'], counts['verses'],
                 counts['words']))
        print('build_bible: %s words of Christ: %d spans (%d continue into a later verse, %d into a later '
              'paragraph), %d red runs' % ((source['abbreviation'],) + counts['red']))
        print('build_bible: %s pilcrows: %d start a paragraph, %d left out'
              % ((source['abbreviation'],) + counts['pilcrows']))
    if not args.only:
        (out / 'BIBLE-NOTICE').write_text(notice(sources))
    if args.summary:
        Path(args.summary).write_text(json.dumps(summaries, indent=0, sort_keys=True))


if __name__ == '__main__':
    main()
