import hashlib, json, logging, os, random, re, subprocess, sys, time

PREFIX = os.environ.get("RCLONE_ACD_BACKEND_PREFIX", "acd:")
assert PREFIX.endswith(":")
TIMEOUT = float(os.environ.get("RCLONE_ACD_BACKEND_TIMEOUT", 60.0))

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

def run(cmd, *args, **kwargs):
    logging.debug("$ " + " ".join(cmd))
    return subprocess.run(cmd, *args, **kwargs)

def backoff(f):
    max_delay = 1.0
    remaining = TIMEOUT
    while True:
        try:
            return f()
        except Exception as e:
            err = e
        delay = random.uniform(0.0, max_delay)
        if remaining < delay:
            raise err
        time.sleep(delay)
        remaining -= delay
        if max_delay < 256.0:
            max_delay *= 2.0

def _md5sum(filename):
    return _hash_file(hashlib.md5, filename).hexdigest()

def remove(fn):
    run(["rclone", "delete", PREFIX + fn],
        check=True,
        stdin=subprocess.DEVNULL)
    # we have to wait for deleted file to go away otherwise the remove might
    # occur after the copy, which would be very bad
    @backoff
    def _():
        try:
            logging.debug("waiting for deleted file to go away")
            md5sum(fn)
        except Exception:
            pass
        else:
            raise ValueError("deleted file still exists")

def listdir(dir):
    r = run(["rclone", "ls", "--include=/*", PREFIX + dir],
            check=True,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True)
    entries = []
    sanity_tested = False
    for line in r.stdout.split("\n"):
        line = line.lstrip()
        if not line:
            continue
        entry, = re.match(r"^ *\d+ ([^ ].*)$", line).groups()
        entries.append(entry)
        if not sanity_tested:
            try:
                md5sum(os.path.join(dir, entry))
            except Exception:
                raise ValueError("output of 'rclone lsd' did not make sense")
            sanity_tested = True
    return entries

def md5sum(fn):
    r = run(["rclone", "md5sum", PREFIX + fn],
            check=True,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True)
    h = r.stdout[:32]
    if not re.match('[0-9a-f]{32}$', h):
        raise ValueError("did not receive a valid md5 hash: {!r}".format(h))
    return h

def download(remote_fn, local_dir):
    local_fn = os.path.join(local_dir, os.path.basename(remote_fn))

    # avoid unnecessary downloads
    remote_hashsum = md5sum(remote_fn)
    if os.path.isfile(local_fn):
        if _md5sum(local_fn) == remote_hashsum:
            return
    _try_remove(local_fn)

    # download, retrying if needed
    run(["rclone", "copy", PREFIX + remote_fn, local_dir],
        check=True,
        stdin=subprocess.DEVNULL)

    # prevent spurious successes
    if _md5sum(local_fn) != remote_hashsum:
        raise ValueError("downloaded file does not match original")

    return remote_hashsum

def upload(local_fn, remote_dir):
    remote_fn = os.path.join(remote_dir, os.path.basename(local_fn))

    # avoid unnecessary uploads
    local_hashsum = _md5sum(local_fn)
    try:
        remote_hashsum = md5sum(remote_fn)
    except subprocess.CalledProcessError:
        remote_hashsum = None
    if local_hashsum == remote_hashsum:
        return
    elif remote_hashsum is not None:
        remove(remote_fn)

    # upload, retrying if needed
    run(["rclone", "copy", local_fn, PREFIX + remote_dir],
        check=True,
        stdin=subprocess.DEVNULL)

    # it might take a while to get the right md5sum
    # yay for eventual consistency \o/
    @backoff
    def _():
        logging.debug("waiting for md5sum of uploaded file to match original")
        remote_hashsum = md5sum(remote_fn)
        if local_hashsum != remote_hashsum:
            raise ValueError("uploaded file does not match original")

    return remote_hashsum
