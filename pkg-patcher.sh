#!/bin/sh
set -eu
dir=`dirname "$0"`

branch=${branch-master}
gitdir=${gitdir-$pkgname}
patch=${patch-$pkgname.patch}

# postconditions: initialized
initialize() {
    [ -f "$gitdir"/.git/_init ] || {
        git clone --single-branch -b "$branch" "$remote" "$gitdir"
        git -C "$gitdir" config user.name nobody
        git -C "$gitdir" config user.email nobody@nobody
        touch "$gitdir"/.git/_init
    }
}

# preconditions: initialized
# postconditions: initialized, consistent
make_consistent() {
    if [ "`git -C "$gitdir" ls-files --unmerged`" ]; then
        echo >&2 "error: must fix all conflicts in tree first"
        exit 1
    fi
    [ -f "$gitdir"/.git/_consistent ] || {
        if [ -f "$patch" ]; then
            commit=`head -n 1 "$patch"`
            git -C "$gitdir" clean -d -f -q
            git -C "$gitdir" reset -q --hard
            git -C "$gitdir" checkout -q "$commit"
            git -C "$gitdir" apply --whitespace=nowarn <"$patch"
            git -C "$gitdir" add -A
        fi
        touch "$gitdir"/.git/_consistent
    }
}

# preconditions: initialized, consistent
# postconditions: initialized, consistent, saved
save() {
    {
        git -C "$gitdir" rev-parse HEAD
        git -C "$gitdir" diff --binary --cached --full-index
    } >"$patch"
}

# preconditions: initialized, consistent, saved
# postconditions: initialized, consistent
rebase() {
    if [ "`git -C "$gitdir" ls-files -d -m -o`" ]; then
        echo >&2 "error: tree contains changes not in index"
        exit 1
    fi
    rm "$gitdir"/.git/_consistent
    git -C "$gitdir" stash -q
    git -C "$gitdir" reset -q --hard origin/"$branch"
    git -C "$gitdir" stash pop -q || {
        touch "$gitdir"/.git/_consistent
        e=$?
        git -C "$gitdir" stash drop -q
        exit $?
    }
    git -C "$gitdir" add -A
    touch "$gitdir"/.git/_consistent
}

case ${1-} in
    sync)
        initialize
        make_consistent
        save
        ;;
    rebase)
        initialize
        make_consistent
        save
        git -C "$gitdir" fetch -p --all
        rebase
        save
        ;;
    *)
        cat >&2 <<EOF
usage: `basename "$0"` (sync|rebase)

  sync
     If not exists, clone and apply the patch on the last known good commit.
     Update the patch with changes in the working tree.

  rebase
     Like sync, but also rebases the patch to the latest commit

EOF
        exit 1
        ;;
esac
