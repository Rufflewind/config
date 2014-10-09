#!/usr/bin/env python
import sys
from re import search

def warn(message):
    sys.stderr.write("{0}: {1}\n".format(sys.argv[0], message))

def remove_singles(files):
    hashes_to_delete = []
    for file_hash, filenames in files.items():
        if len(filenames) <= 1:
            hashes_to_delete.append(file_hash)
    for file_hash in hashes_to_delete:
        del files[file_hash]

def filter_files(predicate, files):
    for filenames in files.values():
        new_filenames = tuple(filter(predicate, filenames))
        filenames.clear()
        filenames.extend(new_filenames)

def dump_files(files):
    for filenames in files.values():
        for filename in filenames:
            print(filename)
        print("")

def load_hashes_from_file(filename):
    with open(filename) as f:
        return load_hashes(f)

def load_hashes(file_object):
    files = {}
    for line in file_object:
        line = line[:-1]
        m = search("^([0-9a-f]+)  (.+)$", line)
        if not m:
            if line:
                warn("cannot parse: " + repr(line))
            continue
        file_hash, filename = m.groups()
        file_hash = int(file_hash, base=16)
        if file_hash in files:
            files[file_hash].append(filename)
        else:
            files[file_hash] = [filename]
    remove_singles(files)
    return files

# an example of how to use this script
#
# find >find.out "$@" -type f -exec md5sum "{}" +
files = load_hashes_from_file("find.out")
patt = "|".join('''
/\.git/
/\.gitignore
/dist/
/LICENSE(\.md)?
/Setup\.hs
'''[1:-1].split("\n"))
filter_files(lambda s: not search(patt, s), files)
remove_singles(files)
dump_files(files)
