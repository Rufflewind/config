import hashlib, json, os, re, subprocess, sys
from subprocess import DEVNULL, run

#@snip/_try_remove[
#@requires: mod:os
#@origin: try_remove
def _try_remove(path):
    try:
        os.remove(path)
    except OSError:
        return False
    return True
#@]

#@snip/_hash_file[
#@origin: hash_file
def _hash_file(hasher, file, block_size=(1 << 20)):
    if isinstance(file, str):
        with open(file, "rb") as f:
            return _hash_file(hasher, f, block_size=block_size)
    h = hasher()
    for block in iter(lambda: file.read(block_size), b""):
        h.update(block)
    return h
#@]

def _md5sum(filename):
    return _hash_file(hashlib.md5, filename).hexdigest()

def _retry_command(args, retries, retry_cleanup_hook=None):
    cmd = args[0]
    args = args[1:]
    if retries <= 0:
        raise ValueError("'retries' argument must be positive")
    while True:
        subretries = min(retries, 4)
        retries -= subretries
        try:
            p = run(["acd_cli", cmd, "-r", str(subretries)] + args,
                    stdin=DEVNULL, stdout=sys.stderr, check=True)
        except subprocess.CalledProcessError:
            if not retries:
                raise
        else:
            break
        if retry_cleanup_hook:
            retry_cleanup_hook()
        sync()

def sync():
    # it likes to print stuff to stdout instead of stderr :/
    run(["acd_cli", "sync"], check=True, stdin=DEVNULL, stdout=sys.stderr)

def mkdir(dir, parents=False):
    try:
        r = run(["acd_cli", "mkdir"] + (["-p"] if parents else []) + [dir],
                check=True,
                stdin=DEVNULL,
                stdout=DEVNULL,
                stderr=subprocess.PIPE,
                universal_newlines=True)
    except:
        # show stderr only if it fails
        sys.stderr.write(r.stderr)
        raise

def remove(fn):
    run(["acd_cli", "rm", fn], stdin=DEVNULL, stdout=DEVNULL, check=True)

def listdir(dir):
    r = run(["acd_cli", "ls", dir],
            check=True,
            stdin=DEVNULL,
            stdout=subprocess.PIPE,
            universal_newlines=True)
    entries = []
    sanity_tested = False
    for line in r.stdout.split("\n"):
        line = line.lstrip()
        if not line:
            continue
        entry, = re.match(r"^\[[-\w]+\] \[\w\] ([^/]+)/?$", line).groups()
        entries.append(entry)
        if not sanity_tested:
            if not exists(os.path.join(dir, entry)):
                raise ValueError("unrecognized output format of 'acd_cli ls'")
            sanity_tested = True
    return entries

def exists(fn):
    return not run(["acd_cli", "resolve", fn],
                   stdin=DEVNULL,
                   stdout=DEVNULL,
                   stderr=DEVNULL).returncode

def md5sum(fn, silent=True):
    r = run(["acd_cli", "metadata", fn],
            check=True,
            stdin=DEVNULL,
            stdout=subprocess.PIPE,
            stderr=(DEVNULL if silent else None),
            universal_newlines=True)
    return json.loads(r.stdout)["contentProperties"]["md5"]

def download(remote_fn, local_dir, retries, connections):
    assert 1 <= int(connections) <= 8
    local_fn = os.path.join(local_dir, os.path.basename(remote_fn))

    # avoid unnecessary downloads
    remote_hashsum = md5sum(remote_fn, silent=False)
    if os.path.isfile(local_fn):
        if _md5sum(local_fn) == remote_hashsum:
            return
    _try_remove(local_fn)

    # download, retrying if needed
    def retry_cleanup_hook():
        _try_remove(local_fn)
    _retry_command(["download", "-x", str(connections), remote_fn, local_dir],
                   retries=retries, retry_cleanup_hook=retry_cleanup_hook)

    # prevent spurious successes
    if _md5sum(local_fn) != remote_hashsum:
        raise ValueError("downloaded file does not match original")

    return remote_hashsum

def upload(local_fn, remote_dir, retries, connections):
    assert 1 <= int(connections) <= 8
    remote_fn = os.path.join(remote_dir, os.path.basename(local_fn))

    # avoid unnecessary uploads
    local_hashsum = _md5sum(local_fn)
    try:
        remote_hashsum = md5sum(remote_fn)
    except subprocess.CalledProcessError:
        remote_hashsum = None
    if local_hashsum == remote_hashsum:
        return
    elif remote_hashsum is None:
        mkdir(remote_dir, parents=True)
    else:
        remove(remote_fn)

    # upload, retrying if needed
    _retry_command(["upload", "-x", str(connections), local_fn, remote_dir],
                   retries=retries)

    # prevent spurious successes
    remote_hashsum = md5sum(remote_fn, silent=False)
    if local_hashsum != remote_hashsum:
        raise ValueError("uploaded file does not match original")

    return remote_hashsum
