#!/usr/bin/env python3
"""Compile WordNet 3.0 and SCOWL into CuBit's read-only lexicon image.

Build-time only. Both sources come from the pinned nixpkgs revision through
CUBIT_WORDNET_DICT and CUBIT_SCOWL_DICT (see flake.nix). The output is the
binary layout documented in docs/lexicon.md and read by
userspace/lib/lexicon (Lexicon.Image): fixed-width little-endian records in
sorted, indexed tables plus one text blob, so the reader never parses text.

Sources and their licences (both permissive; notices ship in LEXICON-NOTICE):
  - WordNet 3.0, Princeton University: senses, glosses, examples, relations.
  - SCOWL 2020.12.07 (Kevin Atkinson et al.), American and British word lists
    up to size 60: spelling coverage and commonness bands.
"""
import argparse
import os
import struct
import sys
from pathlib import Path

MAGIC = b'CUBITLEX'
FORMAT_VERSION = 1

# Part-of-speech codes on disk; Lexicon.Image.Part_Of_Speech'Pos order.
POS_CODE = {'n': 0, 'v': 1, 'a': 2, 's': 2, 'r': 3}
POS_FILES = (('noun', 'n'), ('verb', 'v'), ('adj', 'a'), ('adv', 'r'))

# Relation codes on disk; Lexicon.Image.Relation_Kind'Pos order.
RELATION_CODE = {
    '@': 0, '@i': 0,     # Broader (hypernym, instance hypernym)
    '~': 1, '~i': 1,     # Narrower (hyponym, instance hyponym)
    '&': 2,              # Similar (adjective clusters)
    '^': 3,              # See also
}
ANTONYM = '!'

# SCOWL sizes, most common first. A word's band is the first size listing it.
SCOWL_SIZES = (10, 20, 35, 40, 50, 55, 60)
# Band for words that only WordNet knows (rarer than every SCOWL size used).
WORDNET_ONLY_BAND = 70

# The longest gloss or example (Lexicon_Images.MAXIMUM_TEXT_ITEM_BYTES).
TEXT_ITEM_MAX = 1024

# Field limits of the on-disk records (see docs/lexicon.md).
U8_MAX = 0xFF
U16_MAX = 0xFFFF
U32_MAX = 0xFFFFFFFF

HEADER_SECTIONS = ('words', 'senses', 'synsets', 'members', 'relations',
                   'antonyms', 'examples', 'exceptions', 'text')


def fail(message):
    sys.exit('build_lexicon: ' + message)


def key_of(text):
    """Lookup key: ASCII lower case, WordNet underscores as spaces."""
    return text.replace('_', ' ').lower()


class Text:
    """One deduplicated text blob."""

    def __init__(self):
        self.blob = bytearray()
        self.seen = {}

    def add(self, text, limit):
        data = text.encode('utf-8')
        if len(data) > limit:
            fail('text longer than its field allows: %r' % text[:60])
        offset = self.seen.get(data)
        if offset is None:
            offset = len(self.blob)
            self.blob += data
            self.seen[data] = offset
        return offset, len(data)


def split_gloss(gloss):
    """WordNet glosses are 'definition; "example"; "example"'."""
    parts = []
    depth_quote = False
    current = []
    for ch in gloss:
        if ch == '"':
            depth_quote = not depth_quote
        if ch == ';' and not depth_quote:
            parts.append(''.join(current).strip())
            current = []
        else:
            current.append(ch)
    parts.append(''.join(current).strip())
    definition = []
    examples = []
    for part in parts:
        if len(part) >= 2 and part.startswith('"'):
            end = part.find('"', 1)
            examples.append(part[1:end] if end > 0 else part[1:])
        elif part:
            definition.append(part)
    return '; '.join(definition), examples


def read_synsets(dict_dir):
    synsets = {}   # (pos letter of file, offset) -> record
    order = []
    for name, letter in POS_FILES:
        with open(dict_dir / ('data.' + name), encoding='latin-1') as f:
            for line in f:
                if line.startswith('  '):
                    continue   # licence header
                body, _, gloss = line.partition(' | ')
                fields = body.split()
                offset = int(fields[0])
                ss_type = fields[2]
                count = int(fields[3], 16)
                words = []
                at = 4
                for _ in range(count):
                    lemma = fields[at]
                    # Adjective markers such as (p) and (a) are not spelling.
                    paren = lemma.find('(')
                    if paren > 0:
                        lemma = lemma[:paren]
                    words.append(lemma.replace('_', ' '))
                    at += 2
                pointer_count = int(fields[at])
                at += 1
                pointers = []
                for _ in range(pointer_count):
                    symbol, target, target_pos, source_target = fields[at:at + 4]
                    at += 4
                    pointers.append((symbol, int(target), target_pos,
                                     int(source_target[:2], 16),
                                     int(source_target[2:], 16)))
                definition, examples = split_gloss(gloss.strip())
                record = {
                    'file': letter, 'offset': offset, 'pos': ss_type,
                    'words': words, 'pointers': pointers,
                    'definition': definition, 'examples': examples,
                }
                synsets[(letter, offset)] = record
                order.append((letter, offset))
    return synsets, order


def file_letter(pos):
    return 'a' if pos == 's' else pos


def read_index(dict_dir):
    """lemma key -> [(file letter, synset offset)] in WordNet sense order."""
    senses = {}
    for name, letter in POS_FILES:
        with open(dict_dir / ('index.' + name), encoding='latin-1') as f:
            for line in f:
                if line.startswith('  '):
                    continue
                fields = line.split()
                lemma = key_of(fields[0])
                synset_count = int(fields[2])
                pointer_count = int(fields[3])
                offsets = fields[6 + pointer_count:]
                if len(offsets) != synset_count:
                    fail('index.%s: bad synset count for %s' % (name, lemma))
                senses.setdefault(lemma, []).extend(
                    (letter, int(o)) for o in offsets)
    return senses


def read_exceptions(dict_dir):
    exceptions = []
    for name, _ in POS_FILES:
        with open(dict_dir / (name + '.exc'), encoding='latin-1') as f:
            for line in f:
                fields = line.split()
                for base in fields[1:]:
                    exceptions.append((key_of(fields[0]), key_of(base)))
    return exceptions


def read_scowl(scowl_dir):
    bands = {}
    for variety in ('wamerican', 'wbritish'):
        for size in SCOWL_SIZES:
            path = scowl_dir / ('%s.%d' % (variety, size))
            with open(path, encoding='latin-1') as f:
                for line in f:
                    word = line.strip()
                    if not word or word.endswith("'s"):
                        continue   # possessives are a reader rule
                    key = key_of(word)
                    if not key.isascii():
                        continue   # the reader folds ASCII case only
                    if key not in bands or bands[key] > size:
                        bands[key] = size
    return bands


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('output', type=Path)
    parser.add_argument('--notice', type=Path, required=True)
    args = parser.parse_args()
    wordnet = os.environ.get('CUBIT_WORDNET_DICT')
    scowl = os.environ.get('CUBIT_SCOWL_DICT')
    if not wordnet or not scowl:
        parser.error('use nix develop: CUBIT_WORDNET_DICT and CUBIT_SCOWL_DICT')
    dict_dir = Path(wordnet)
    scowl_dir = Path(scowl)

    synsets, synset_order = read_synsets(dict_dir)
    index = read_index(dict_dir)
    exceptions = read_exceptions(dict_dir)
    bands = read_scowl(scowl_dir)

    # Word table: every WordNet lemma, exception base and SCOWL word.
    keys = set(index) | set(bands) | {b for _, b in exceptions}
    keys = sorted(keys, key=lambda k: k.encode('utf-8'))
    for k in keys:
        if not k.isascii() or not k or len(k) > U8_MAX:
            fail('unexpected word key %r' % k)
    word_number = {k: i for i, k in enumerate(keys)}

    synset_number = {ident: i for i, ident in enumerate(synset_order)}

    # Senses: per word in WordNet order; (synset, word) pairs.
    senses = []          # (synset index, word index, display text)
    word_first = []
    sense_of = {}        # (synset index, member number 1..) -> sense index
    for k in keys:
        word_first.append(len(senses))
        for ident in index.get(k, ()):
            s = synset_number[ident]
            record = synsets[ident]
            # Members differing only in case ("ddC", "DDC") share one sense,
            # displayed as the first.
            members = [n for n, w in enumerate(record['words'], start=1)
                       if key_of(w) == k]
            if not members:
                fail('lemma %r missing from its synset' % k)
            for n in members:
                sense_of[(s, n)] = len(senses)
            senses.append((s, word_number[k], record['words'][members[0] - 1]))
    word_first.append(len(senses))

    text = Text()
    out_words = bytearray()
    for i, k in enumerate(keys):
        off, length = text.add(k, U8_MAX)
        count = word_first[i + 1] - word_first[i]
        if count > U16_MAX:
            fail('too many senses for %r' % k)
        band = bands.get(k, WORDNET_ONLY_BAND)
        out_words += struct.pack('<IBBHI', off, length, band, count,
                                 word_first[i])

    # Members, relations and antonyms per synset/sense.
    out_synsets = bytearray()
    out_members = bytearray()
    out_relations = bytearray()
    out_examples = bytearray()
    member_count_total = 0
    relation_total = 0
    example_total = 0
    antonyms_of = {}
    for s, ident in enumerate(synset_order):
        record = synsets[ident]
        member_first = member_count_total
        members = 0
        listed = set()
        for n in range(1, len(record['words']) + 1):
            sense = sense_of.get((s, n))
            if sense is None:
                fail('synset member without an index entry: %r' % (ident,))
            if sense in listed:
                continue
            listed.add(sense)
            out_members += struct.pack('<I', sense)
            members += 1
        member_count_total += members
        relation_first = relation_total
        relations = 0
        for symbol, target, target_pos, src, dst in record['pointers']:
            target_ident = (file_letter(target_pos), target)
            if symbol == ANTONYM:
                source_sense = sense_of[(s, src)]
                target_sense = sense_of[(synset_number[target_ident], dst)]
                antonyms_of.setdefault(source_sense, []).append(target_sense)
                continue
            code = RELATION_CODE.get(symbol)
            if code is None or src != 0:
                continue   # other WordNet relations are not presented
            out_relations += struct.pack('<BI', code,
                                         synset_number[target_ident])
            relations += 1
        relation_total += relations
        example_first = example_total
        for example in record['examples']:
            off, length = text.add(example, TEXT_ITEM_MAX)
            out_examples += struct.pack('<IH', off, length)
            example_total += 1
        gloss_off, gloss_len = text.add(record['definition'], TEXT_ITEM_MAX)
        if (members > U8_MAX or relations > U16_MAX
                or len(record['examples']) > U8_MAX):
            fail('synset field overflow at %r' % (ident,))
        out_synsets += struct.pack(
            '<BIHIBIBIH', POS_CODE[record['pos']], gloss_off, gloss_len,
            example_first, len(record['examples']), member_first, members,
            relation_first, relations)

    out_senses = bytearray()
    out_antonyms = bytearray()
    antonym_total = 0
    for i, (s, w, display) in enumerate(senses):
        off, length = text.add(display, U8_MAX)
        targets = antonyms_of.get(i, [])
        out_senses += struct.pack('<IIIBBI', s, w, off, length, len(targets),
                                  antonym_total)
        for t in targets:
            out_antonyms += struct.pack('<I', t)
        antonym_total += len(targets)

    out_exceptions = bytearray()
    exception_rows = sorted(set(exceptions),
                            key=lambda e: (e[0].encode(), e[1].encode()))
    for inflected, base in exception_rows:
        off, length = text.add(inflected, U8_MAX)
        out_exceptions += struct.pack('<IBI', off, length, word_number[base])

    tables = {
        'words': (out_words, len(keys)),
        'senses': (out_senses, len(senses)),
        'synsets': (out_synsets, len(synset_order)),
        'members': (out_members, member_count_total),
        'relations': (out_relations, relation_total),
        'antonyms': (out_antonyms, antonym_total),
        'examples': (out_examples, example_total),
        'exceptions': (out_exceptions, len(exception_rows)),
        'text': (text.blob, len(text.blob)),
    }
    header_size = len(MAGIC) + 4 + 8 * len(HEADER_SECTIONS)
    header = bytearray(MAGIC + struct.pack('<I', FORMAT_VERSION))
    body = bytearray()
    for name in HEADER_SECTIONS:
        data, count = tables[name]
        header += struct.pack('<II', header_size + len(body), count)
        body += data
    image = header + body
    if len(image) > U32_MAX:
        fail('image too large for 32-bit offsets')
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_bytes(image)

    notice = []
    notice.append('CuBit lexicon data notices\n==========================\n\n')
    notice.append('Dictionary and thesaurus data: WordNet 3.0.\n\n')
    notice.append((dict_dir.parent / 'lib' / 'wnres' / 'license.txt')
                  .read_text(encoding='latin-1'))
    notice.append('\n\nSpelling word lists: SCOWL 2020.12.07 (sizes up to 60,'
                  ' American and British).\n\n')
    notice.append((scowl_dir.parent.parent / 'lib' / 'aspell' / 'Copyright')
                  .read_text(encoding='latin-1'))
    args.notice.write_text(''.join(notice), encoding='utf-8')
    print('lexicon: %d words, %d senses, %d synsets, %d bytes'
          % (len(keys), len(senses), len(synset_order), len(image)))


if __name__ == '__main__':
    main()
