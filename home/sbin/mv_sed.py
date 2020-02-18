#!/usr/bin/env python
#
# Rename files using a regular expression (sed script).
#
import argparse, os, subprocess, sys

def validate_mass_rename(fns, new_fns, dry_run, overwrite):
    if len(new_fns) != len(fns):
        raise ValueError("different number of filenames: {0} != {1}{2}"
                         .format(len(fns), len(new_fns),
                                 "".join("\n  " + x for x in new_fns)))

    # sanity checking
    new_abs_fns = set()
    for fn, new_fn in zip(fns, new_fns):
        new_abs_fn = os.path.abspath(new_fn)
        if not os.path.lexists(fn):
            raise ValueError("does not exist: {0}".format(fn))
        if fn == new_fn:
            continue
        if not overwrite and os.path.lexists(new_fn):
            raise ValueError("refusing to overwrite: {0}".format(new_fn))
        if new_abs_fn in new_abs_fns:
            raise ValueError("name will clash: {0}".format(new_fn))
        new_abs_fns.add(new_abs_fn)

    # do the renames
    for fn, new_fn in zip(fns, new_fns):
        if fn == new_fn:
            yield "unchanged", fn, new_fn
        elif dry_run:
            yield "dry_run", fn, new_fn
        else:
            yield "rename", fn, new_fn

def main():
    PROG = os.path.basename(sys.argv[0])

    opts = argparse.ArgumentParser(
        description="Renames every file using a sed script.")
    opts.add_argument("SEDSCRIPT", nargs=1, help="sed script")
    opts.add_argument("FILE", nargs="+", help="filename")
    opts.add_argument("-n", "--dry-run", action="store_true",
                      help="don't actually rename anything")
    opts.add_argument("-f", "--force", action="store_true",
                      help="allow overwrites")
    args = opts.parse_args()

    patt = args.SEDSCRIPT[0]
    fns = args.FILE
    abs_fns = tuple(map(os.path.normpath, fns))

    inp = "".join(fn + "\n" for fn in abs_fns)
    try:
        out = subprocess.check_output(["sed", patt],
                                      input=inp,
                                      universal_newlines=True)
    except subprocess.CalledProcessError as e:
        sys.exit(e.returncode)

    new_abs_fns = out.splitlines()
    if len(new_abs_fns) == len(abs_fns):
        new_fns = tuple(os.path.relpath(new_abs_fn)
                        if os.path.isabs(fn) else new_abs_fn
                        for fn, new_abs_fn in zip(fns, new_abs_fns))
    else:
        new_fns = new_abs_fns

    try:
        for kind, fn, new_fn in validate_mass_rename(
                fns,
                new_fns,
                dry_run=args.dry_run,
                overwrite=args.force):
            if kind == "unchanged":
                print("{0} (unchanged)".format(fn))
            else:
                if kind == "rename":
                    os.rename(fn, new_fn)
                print("{0} => {1}{2}".format(
                    fn,
                    new_fn,
                    " (dry run)" if kind == "dry_run" else "",
                ))

    except ValueError as e:
        sys.exit("{0}: {1}".format(PROG, e))

if __name__ == "__main__":
    main()
