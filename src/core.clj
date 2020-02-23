(ns core
  (:require [clojure.core.async :as a]
            [clojure.set :refer [difference]]
            [clojure.string :as string]
            [com.stuartsierra.component :as component]
            [me.raynes.conch :as sh]
            [me.raynes.fs :as fs]))

(defn run-command-bg
  [name {:keys [dir] :as opts} & args]
  (let [!out-atom (atom [])
        out-atom-redirect
        (reify sh/Redirectable
          (redirect [_ options k proc]
            (let [s (k proc)
                  w #(swap! !out-atom conj %)]
              (if (char? (first s))
                (w (apply str s))
                (doseq [x s] (w x))))))]
    (sh/let-programs [p name]
      {:!out
       !out-atom
       :res
       (apply p (conj (vec args)
                      {:verbose true
                       :background true
                       :out out-atom-redirect
                       :buffer :line
                       :dir dir
                       }))})))

(defn bg-command-exit-ok?
  [{:keys [res]}]
  (and (realized? res)
       (some->
        (try
          @res
          (catch Exception e
            nil))
        :exit-code
        ((fn [f]
           (when (realized? f) @f)))
        (= 0))))

(defn bg-command-exception-map
  [{:keys [res] :as proc}]
  (when-not (bg-command-exit-ok? proc)
    (try
      (do @(:res proc) nil)
      (catch Exception e
        (Throwable->map e)))))

(defn parse-youtube-dl-out
  "on a successful download, the last line is a status bar
  where carriage returns are used to distinguish updates to terminal.
  plan on checking process return code for successful download."
  [out]
  (let [youtube-results-lines
        (if (string? out)
          (string/split out #"\n")
          out)]
    {:file
     #_
     (->> youtube-results-lines
          (map
           #(re-find #"\[download\] Destination: (.*)" %))
          (filter some?)
          (map second)
          first
          )
     (first
      (filter some?
              (map
               (fn [result-line]
                 (->>
                  [(re-find #"\[download\] Destination: (.*)"
                            result-line)
                   (re-find #"\[download\] (.*) has already been downloaded"
                            result-line)]
                  (filter some?) (map second) first)
                 )
               youtube-results-lines
               )))
     :src-tag
     (some->> youtube-results-lines
       first
       (re-find #"\[(.*)\]")
       second)}))

;; youtube-dl-specific scheduler.
;; "http://...."  is the only identifier
;;    on scheduling tasks/procs.

;; LATER: split this out into separate protocols
(defprotocol CashSchedulerYTDL
  (schedule [this id-url]) ;; => nil
  (scheduled? [this id-url]) ;; => bool
  (done? [this id-url]) ;; => bool
  (done-ok? [this id-url]) ;; => bool
  (file-path [this id-url]) ;; => str
  ;; ... with or without .part,
  ;;     or nil if filename unknown
  ;;               or filename nexist.
  (open-file [this id-str]) ;; => java.io.File
  ;; ... or nil if not done-ok.
  )

(def ^:dynamic *max-ytdl-mc-proc* 3)

(defn new-youtube-dl-proc
  [ytdl-url out-dir]
  (let [ytdl-url (str ytdl-url)
        youtube-dl
        (partial run-command-bg
                 "youtube-dl"
                 {:dir out-dir})]
    (if (re-find #"^https?://www.youtube.com"
                 ytdl-url)
      (youtube-dl "-f" "18" ytdl-url)
      (youtube-dl ytdl-url))))

;; ytdl-sched-* denotes orig protocol impl,
;;    ... except for *-kickoff-next.
;;;;
(defn ytdl-sched-scheduled?
  [{:keys [!procs]} id-url]
  (some? ((-> @!procs keys set) id-url)))

(defn ytdl-sched-schedule
  [{:keys [!procs] :as this} id-url]
  (do
    (when (not (ytdl-sched-scheduled? this id-url))
      (swap! !procs assoc id-url nil))
    nil)) ;; => nil

(defn ytdl-sched-done?
  [{:keys [!procs] :as this} id-url]
  (and (ytdl-sched-scheduled? this id-url)
       (some-> @!procs (get id-url) :res
               realized?)))

(defn ytdl-sched-done-ok?
  [{:keys [!procs] :as this} id-url]
  (and (ytdl-sched-done? this id-url)
       (let [ok? (bg-command-exit-ok?
                  (get @!procs id-url))]
         (if (nil? ok?) false ok?))))

(defn ytdl-sched-file-path
  [{:keys [!procs] :as this} id-url]
  (when (ytdl-sched-scheduled? this id-url)
    (when-let [filename
               (some-> @!procs
                       (get id-url) :!out deref
                       parse-youtube-dl-out :file)]
      (let [filepath (str (:dir this) "/" filename)]
        (when (or (fs/exists? filepath)
                  (fs/exists? (str filepath ".part")))
          (if (ytdl-sched-done-ok? this id-url)
            filepath
            (str filepath ".part")))))))

(defn ytdl-sched-open-file [this id-url]
  (when (done-ok? this id-url)
    ;; todo:
    nil)) ;; => stream to web dl .. java.File???
;; for download by listr_keeper.lua
;;              or listr_keeper.py.
;;;;


;;; round-robin scheduling is the basis
;;  of all planned scheduling techniques.
(defn ytdl-sched-kickoff-next
  [this]
  (let [resources?
        ;; could make this a function defined
        ;; in new-* .. i.e. make scheduler aware
        ;; of correspondence between processes
        ;; and available resources.
        (fn [procs]
          (> *max-ytdl-mc-proc*
             (->> procs vals (filter some?)
                  (remove (comp realized? :res))
                  count)))]
    (swap!
     (:!procs this)
     (fn [procs]
       (if (resources? procs)
         (if-let [id-url ;; work?
                  (first
                   (->> procs
                        (filter (fn [[_ v]]
                                  (empty? v)))
                        (map first)))]
           ;;; ... side-effects in swap! are a bad idea.
           ;; perhaps `kickoff` means "make ok to start,"
           ;; not "start if ok."
           ;;
           #_(assoc procs id-url (:dir this))
           ;;
           ;; then there can be a seperate part of
           ;; async flow that starts those processes that have
           ;; clearance for takeoff.
           ;;
           ;; changing semantics of internal data structures
           ;; is risky, however.
           (let [new-proc (new-youtube-dl-proc
                           id-url
                           (:dir this))]
             (assoc procs id-url new-proc))
           procs)
         procs)))))

(defn ytdl-sched-new-proc-ids
  [old-procs new-procs]
  (apply difference
   (map set [(->> new-procs
                  (remove (comp empty? second))
                  (map first))
             (->> old-procs
                  (remove (comp empty? second))
                  (map first))])))

;;; currently-unused,
;;  alternate event-based scheduling strategy
;;
;; handle state change out of top-lvl atoms.
;; don't respond to every, e.g., stdout line.
;;
;; todo: make this a multi-method
(defn ytdl-sched-atom-change-event-handler
  [this what _the-thing old new]
  (when (= what :!procs)
    (when-let [new-proc-id
               (first (ytdl-sched-new-proc-ids
                       old new))]
      ;; register event-handler on `:res`, a `future`,
      ;; aka a one-time event.
      ;;
      ;; ... or try placing this `future` directly
      ;;   in `*-kickoff-next` instead, now that it's
      ;;     (future & body)
      ;;  not
      ;;     (future (fn [] & body))
      ;;
      (future (try
                @(:res (get new new-proc-id))
                (catch Exception e
                  nil))
              (ytdl-sched-kickoff-next this))))
  (when (= what :!heartbeats)
    (println "heartbeat change unhandled"))
  nil)

;; possible to setup event-based rather than
;; thread-based (as in go-loop below)
;; by using watchers?
;; (might have to add atoms to this)
(defn ytdl-sched-kickoff-watch
  [this]
  (doseq [ak [:!procs]]
    (add-watch
     (ak this) ak
     (partial ytdl-sched-atom-change-event-handler
              this))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; using the component lifecycle isn't necessary to
;; drive 'desktop

(defrecord MCashYTDL []
  component/Lifecycle
  (start [component]
    (let [dir (fs/temp-dir "clj-news-youtube-dl-cache")]
      (-> component
          (assoc :dir dir
                 ;; ....begin in-memory,
                 ;; durable atom later.
                 ;; :!db (durable-atom (str dir "db.atom"))
                 :!procs (atom nil)))))
  (stop [component]
    (-> component
        (dissoc :dir #_:!db))))

;;###
;;***extend protocols by substituting pure fns***
;;to do otherwise is to invite repl reloads
;;###
(extend-protocol CashSchedulerYTDL
  MCashYTDL
  (schedule [this id-url]
    (ytdl-sched-schedule this id-url)) ;; => nil
  (scheduled? [this id-url]
    (ytdl-sched-scheduled? this id-url))
  (done? [this id-url]
    (ytdl-sched-done? this id-url))
  (done-ok? [this id-url]
    (ytdl-sched-done-ok? this id-url))
  (file-path [this id-url]
    (ytdl-sched-file-path this id-url))
  (open-file [this id-url]
    (ytdl-sched-open-file this id-url)))

;; if scheduling requires periodic updates
;; on the order of seconds
;; from an internal timer,
;; use this function.  (cf. event-based above)
(defn new-mcash-ytdl-heartbeat
  [*cmcash* hours seconds]
  (let [!loop-cnt (atom 0)
        !kickoff-cnt (atom 0)
        !added-ids (atom nil)]
    {:!loop-cnt !loop-cnt
     :!kickoff-cnt !kickoff-cnt
     :!added-ids !added-ids
     :heartbeat-expired-chan
     (a/go-loop [i 1]
       (if (< i (* 60 60 hours))
         (let [before
               @(some-> *cmcash*
                        :!procs)
               after
               (ytdl-sched-kickoff-next
                *cmcash*)
               _ (swap! !loop-cnt inc)
               this-loop-added-ids
               (ytdl-sched-new-proc-ids
                before after)]
           (when (not (empty? this-loop-added-ids))
             (swap! !kickoff-cnt inc)
             (doseq [id this-loop-added-ids]
               (swap! !added-ids conj id)))
           (a/<! (a/timeout (* 1000 seconds)))
           (recur (inc i)))
         "timeout"))}))

(defn new-mcash-ytdl
  [dir & {:keys [hours seconds] :or {hours 3
                                     seconds 1}}]
  (let [*cmcash* (-> (map->MCashYTDL {})
                     (assoc :dir dir
                            :!procs (atom nil)
                            :!heartbeats (atom nil)))]
    ;; scheduler kickoff
    (swap! (:!heartbeats *cmcash*) conj
           (new-mcash-ytdl-heartbeat
            *cmcash* hours seconds))
    *cmcash*))

;; util
(defn add-timeout-to-future
  [min f]
  (future (let [res-c (a/chan)
                timeout-c (a/timeout (* 1000 60 min))]
            (a/go (a/<! res-c @f))
            (first (a/alts! [res-c timeout-c])))))
