findwords
=========

Find the words in a series of strings.

You'll need [Chicken Scheme](https://call-cc.org) installed, or a few shims and
another scheme like racket.

findwords has some sane defaults. It'll try to read a dictionary from
`/usr/share/dict/words` or from `$WORDLIST_FILE` if it's set.

If you give findwords a some strings as arguments, it'll try to find some composite words in them. Otherwise, it'll go into a repl mode which just accepts strings from stdin.

Don't be alarmed, it takes a while to start, for me about 30 seconds to index 250k words.
