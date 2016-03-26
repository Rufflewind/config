import os

def shell_escape(s):
    if '\0' in s:
        raise ValueError("cannot escape NUL")
    return "'{0}'".format(s.replace("'", "'\''"))

def save_translation_script(filename, table):
    src, dst = table
    assert len(src) == len(dst)
    table = sorted(x for x in zip(src, dst) if x[0] != x[1])
    src, dest = map("".join, zip(*table))
    with open(filename, "wb") as f:
        f.write("#!/bin/sh\n"
                "utr {0} {1}\n"
                .format(shell_escape(src), shell_escape(dest))
                .encode("utf-8"))
    os.chmod(filename, 0o755)

def generate_translation_scripts(src, dsts, dir="~/.local/sbin"):
    '''Generate translation scripts in the given directory.
    src="""<original>"""
    dsts={"<name>": """<translation>"""}
    '''
    filename_template = os.path.join(os.path.expanduser(dir), "utr.{0}")
    for name, dst in dsts.items():
        save_translation_script(filename_template.format(name), (src, dst))
