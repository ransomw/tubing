# tubing

Clojure UI for YouTube.

### setup

the system consists of a few loosly-coupled components

* [`yt-search`] search YouTube (in `yt-search/`)
* a download scheduler that could, with some refactoring,
  apply to any collection of operating system subprocesses
  forked off from the Clojure runtime/JVM
* [`youtube-dl`](https://ytdl-org.github.io) (external)
* wrapper functions to shell out to `yt-search` and
  `youtube-dl`, pushing downloads onto the scheduler,
  and best intentions to build a proper dashboard
  with `trikl`

ensure that Clojure and golang are present, and `./run.sh`.

now a Cider connection to the process in your terminal
is a Clojure UI for YouTube.

start by evaluating some of the comments in `src/dev.clj`.
