"""Fill tests/ccl-build/fetched-tree/build-catalog.ccl: @URL@ and the eight
32-bit words @W1@ .. @W8@ of a tree identity (64 hex digits), printed.

usage: fill-catalog.py TEMPLATE IDENTITY URL
"""
import sys

DIGEST_WORDS = 8
WORD_HEX_DIGITS = 8

template, identity, url = sys.argv[1:4]
text = open(template).read().replace("@URL@", url)
for word in range(DIGEST_WORDS):
    digits = identity[WORD_HEX_DIGITS * word:WORD_HEX_DIGITS * (word + 1)]
    text = text.replace("@W%d@" % (word + 1), "0x" + digits)
sys.stdout.write(text)
