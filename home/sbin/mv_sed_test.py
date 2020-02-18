#!/usr/bin/env python3
import collections
import functools
import os
import pathlib
import random
import tempfile
import unittest

import mv_sed

def random_path(rand, median_length, random_segment):
    segments = random_list(rand, median_length, random_segment)
    return pathlib.PurePosixPath(os.path.join("/", *segments))

def random_list(rand, median_length, random_element):
    p = 0.5 ** (1 / median_length) if median_length else 0.0
    elements = []
    while rand.random() < p:
        elements.append(random_element(rand))
    return elements

class TestMvSed(unittest.TestCase):

    def test_lookup_with_path_compression(self):
        m = {0: 0, 1: 1}
        self.assertEqual(mv_sed.lookup_with_path_compression(m, 0), 0)
        self.assertEqual(m, {0: 0, 1: 1})

        m = {0: 0, 1: 0}
        self.assertEqual(mv_sed.lookup_with_path_compression(m, 1), 0)
        self.assertEqual(m, {0: 0, 1: 0})

        m = {0: 0, 1: 0, 2: 1, 3: 2, 4: 4}
        self.assertEqual(mv_sed.lookup_with_path_compression(m, 3), 0)
        self.assertEqual(m, {0: 0, 1: 0, 2: 0, 3: 0, 4: 4})

    def _find_conflicting_paths(self, paths):
        conflicts = mv_sed.find_conflicting_paths(paths)

        for cause, count in collections.Counter(conflicts.values()).items():
            with self.subTest(cause=cause):
                # each conflict group should have more than 1 member
                self.assertGreater(count, 1)
                # each conflict group should point directly to root
                self.assertEqual(conflicts[cause], cause)

        for conflict, cause in conflicts.items():
            with self.subTest(conflict=conflict, cause=cause):
                conflict_path = paths[conflict]
                cause_path = paths[cause]
                if conflict_path == cause_path:
                    # when paths are equal, root should be the lower index
                    self.assertLessEqual(cause, conflict)
                else:
                    # otherwise, root should be higher in hierarchy
                    self.assertIn(cause_path, conflict_path.parents)

        # idempotence test
        nonconflicting_paths = [path for i, path in enumerate(paths)
                                if i not in conflicts]
        self.assertEqual(
            mv_sed.find_conflicting_paths(nonconflicting_paths),
            {},
        )

        # ensure remaining files do not conflict
        files = frozenset(nonconflicting_paths)
        self.assertCountEqual(files, nonconflicting_paths)
        dirs = frozenset(ancestor
                         for path in files
                         for ancestor in path.parents)
        self.assertEqual(dirs & files, frozenset())

        return conflicts

    def test_find_conflicting_paths(self):
        self.assertEqual(self._find_conflicting_paths([]), {})
        self.assertEqual(self._find_conflicting_paths([
            pathlib.PureWindowsPath("a"),
            pathlib.PureWindowsPath("A/b"),
        ]), {0: 0, 1: 0})
        self.assertEqual(self._find_conflicting_paths([
            pathlib.PurePosixPath("a"),
            pathlib.PurePosixPath("a/b"),
        ]), {0: 0, 1: 0})
        self.assertEqual(self._find_conflicting_paths([
            pathlib.PurePosixPath("a/b/c"),
            pathlib.PurePosixPath("a"),
            pathlib.PurePosixPath("ab"),
        ]), {0: 1, 1: 1})
        self.assertEqual(self._find_conflicting_paths([
            pathlib.PurePosixPath("a"),
            pathlib.PurePosixPath("b"),
            pathlib.PurePosixPath("a"),
            pathlib.PurePosixPath("a/b"),
        ]), {0: 0, 2: 0, 3: 0})

    def test_find_conflicting_paths_random(self):
        rand = random.Random(0x1a766736a8255aa6)
        rand_segment = lambda r: r.choice(["a", "ab", "b", "c"])
        max_num_segments = 30
        max_num_paths = 20
        n = 100
        for i in range(n):
            num_segments = i / n * max_num_segments
            num_paths = i / n * max_num_paths
            rand_path = functools.partial(
                random_path,
                median_length=num_segments,
                random_segment=rand_segment,
            )
            paths = random_list(rand, num_paths, rand_path)
            with self.subTest(paths=paths):
                self._find_conflicting_paths(paths)

    def test_validate_mass_rename(self):
        with tempfile.TemporaryDirectory() as tmp_str:
            tmp = pathlib.Path(tmp_str)
            for name in ["a", "b", "d", "e", "f", "g"]:
                tmp.joinpath(name).write_bytes(b"")
            conflict = f"DEST_CONFLICT:{tmp.joinpath('c')}"
            self.assertEqual(mv_sed.validate_mass_rename(
                [
                    tmp.joinpath("a"),
                    tmp.joinpath("b"),
                    tmp.joinpath("c1"),
                    tmp.joinpath("c2"),
                    tmp.joinpath("d"),
                    tmp.joinpath("f"),
                    tmp.joinpath("g"),
                    tmp.joinpath("s"),
                    tmp.joinpath("m"),
                ],
                [
                    tmp.joinpath("a"),
                    tmp.joinpath("r"),
                    tmp.joinpath("c"),
                    tmp.joinpath("c"),
                    tmp.joinpath("e"),
                    tmp.joinpath("G/f"),
                    tmp.joinpath("g/f"),
                    tmp.joinpath("s"),
                ],
            ), [
                (mv_sed.Action.NOP, tmp.joinpath("a"), tmp.joinpath("a")),
                (mv_sed.Action.RENAME, tmp.joinpath("b"), tmp.joinpath("r")),
                (conflict, tmp.joinpath("c1"), tmp.joinpath("c")),
                (conflict, tmp.joinpath("c2"), tmp.joinpath("c")),
                ("DEST_EXISTS", tmp.joinpath("d"), tmp.joinpath("e")),
                ("DEST_PARENT_NOT_EXIST", tmp.joinpath("f"), tmp.joinpath("G/f")),
                ("DEST_PARENT_NOT_DIR", tmp.joinpath("g"), tmp.joinpath("g/f")),
                ("SOURCE_NOT_EXIST", tmp.joinpath("s"), tmp.joinpath("s")),
                ("DEST_NOT_SET", tmp.joinpath("m"), tmp.joinpath("m")),
            ])
        self.assertEqual(mv_sed.validate_mass_rename([], [pathlib.Path("a")]), [
            ("SOURCE_NOT_SET", pathlib.Path("a"), pathlib.Path("a")),
        ])

if __name__ == "__main__":
    unittest.main()
