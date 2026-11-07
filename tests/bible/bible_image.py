"""An independent Python reader of CuBit Bible images (tools/build_bible.py
layout), for the hosted tests: dumps chapters and checks an image against
its source's own chapter and verse markers."""
import struct

TABLES = ('books', 'chapters', 'verses', 'segments', 'words', 'postings', 'keys', 'text', 'about')
WIDTH = {'books': 8, 'chapters': 8, 'verses': 8, 'segments': 8, 'words': 16, 'postings': 4,
         'keys': 1, 'text': 1, 'about': 8}
KINDS = ('prose', 'paragraph', 'flush', 'poetry1', 'poetry2', 'poetry3', 'heading', 'major', 'title',
         'prologue')


class Image:
    def __init__(self, data):
        if data[:8] != b'CUBITBIB':
            raise ValueError('bad magic')
        self.data = data
        self.version, self.book_slots = struct.unpack_from('<II', data, 8)
        self.tables = {}
        for i, name in enumerate(TABLES):
            offset, count = struct.unpack_from('<II', data, 16 + 8 * i)
            if offset + count * WIDTH[name] > len(data):
                raise ValueError(name + ' outside the image')
            self.tables[name] = (offset, count)

    def row(self, table, index, fmt):
        offset, count = self.tables[table]
        if not 0 <= index < count:
            raise IndexError(table)
        return struct.unpack_from(fmt, self.data, offset + index * WIDTH[table])

    def text(self, offset, length):
        base, count = self.tables['text']
        if offset + length > count:
            raise IndexError('text')
        return self.data[base + offset: base + offset + length].decode('utf-8')

    def book(self, slot):
        first, count, _ = self.row('books', slot, '<IHH')
        return first, count

    def chapter(self, slot, number):
        first, count = self.book(slot)
        if not 1 <= number <= count:
            return None
        verse, verses, _ = self.row('chapters', first + number - 1, '<IHH')
        return verse, verses

    def segments(self, verse_index):
        first, count, _ = self.row('verses', verse_index, '<IHH')
        result = []
        for i in range(first, first + count):
            offset, length, kind, flags = self.row('segments', i, '<IHBB')
            result.append((KINDS[kind], flags, self.text(offset, length)))
        return result

    def verse_text(self, slot, chapter, verse):
        first, verses = self.chapter(slot, chapter)
        return ' '.join(t for k, f, t in self.segments(first + verse - 1)
                        if k not in ('heading', 'major', 'title', 'prologue'))

    def versification(self):
        result = []
        for slot in range(self.book_slots):
            first, count = self.book(slot)
            result.append([self.row('chapters', first + c, '<IHH')[1] for c in range(count)])
        return result

    def about(self):
        offset, count = self.tables['about']
        result = {}
        for i in range(count):
            text_offset, length, kind, _ = self.row('about', i, '<IHBB')
            result[kind] = self.text(text_offset, length)
        return result
