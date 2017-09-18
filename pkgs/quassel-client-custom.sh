#!/bin/sh
set -eu
dir=`dirname "$0"`

remote=git://git.archlinux.org/svntogit/community.git
branch=packages/quassel
gitdir=quassel-client-custom.git
patch=quassel-client-custom.patch

init_if_absent() {
    [ -f "$gitdir"/.git/_init ] || {
        git clone --single-branch -b "$branch" "$remote" "$gitdir"
        git -C "$gitdir" config user.name nobody
        git -C "$gitdir" config user.email nobody@nobody
        touch "$gitdir"/.git/_init
    }
}

restore_if_inconsistent() {
    if [ "`git -C "$gitdir" ls-files --unmerged`" ]; then
        echo >&2 "please fix the conflicts"
        exit 1
    fi
    [ -f "$gitdir"/.git/_consistent ] || {
        commit=`head -n 1 "$patch"`
        git -C "$gitdir" clean -d -f -q
        git -C "$gitdir" reset -q --hard
        git -C "$gitdir" checkout -q "$commit"
        git -C "$gitdir" apply --whitespace=nowarn <"$patch"
        touch "$gitdir"/.git/_consistent
    }
}

save() {
    git -C "$gitdir" add -A
    {
        git -C "$gitdir" rev-parse HEAD
        git -C "$gitdir" diff --binary --cached --full-index
    } >"$patch"
}

rebase() {
    rm "$gitdir"/.git/_consistent
    git -C "$gitdir" add -A
    git -C "$gitdir" stash -q
    git -C "$gitdir" reset -q --hard origin/"$branch"
    git -C "$gitdir" stash pop -q || {
        touch "$gitdir"/.git/_consistent
        e=$?
        git -C "$gitdir" stash drop -q
        exit $?
    }
    touch "$gitdir"/.git/_consistent
}

case ${1-} in
    sync)
        init_if_absent
        restore_if_inconsistent
        save
        ;;
    rebase)
        init_if_absent
        restore_if_inconsistent
        save
        git -C "$gitdir" fetch -p --all
        rebase
        save
        ;;
    *)
        cat >&2 <<EOF
usage: `basename "$0"` (sync|rebase)
EOF
        exit 1
        ;;
esac
