#! /usr/bin/env zsh

setopt ERR_EXIT
setopt PIPE_FAIL

#
# build youtube search binary (requires go)
# and make other preparations for Clojure
# environment.
#

cd "$(git rev-parse --show-toplevel)"

(cd yt-search && go build)

# todo: run test search,
#   running authorization flow if necessary

if [[ ! -f resources/config.edn ]]; then
    cp resources/config.edn.example \
       resources/config.edn
fi

clj -A:cider-clj
