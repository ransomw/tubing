(ns dev
  (:require [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [doc]]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set]
            [clojure.spec.alpha :as spec]
            [clojure.tools.namespace.repl :refer
             [set-refresh-dirs
              refresh
              refresh-all]]
            [clojure.core.async :as a]
            ;; [reloaded.repl :refer [system init]]
            [clj-http.client :as http-c]
            [cheshire.core :as jcat]
            [core :as c]
            [desktop :as d]
            [tick.alpha.api :as tick-a]
            ))


(defn ns-names-from-ns-map
  [ns-sym]
  (->> (filter
        (fn [[_ el-var]]
          (let [mns (-> el-var meta :ns)]
            (if (nil? mns)
              false
              (= (ns-name mns) ns-sym)
              ))
          )
        (ns-map ns-sym))
       (map first)))


;; for multi-scheduler-setup below
(defonce !schedulers (atom {}))

(comment
  "test drive scheduler"

  (require ['stream-yt])

  (def ^:dynamic *!!cmbsh*
    (let [yts #(partial stream-yt/yt-search %
                         :max-results 50)
          dns {:snl {:stream
                     (yts
                      (str "saturday night live"
                           " SNL "
                           "1977"))
                     :out "/tv/snl"}
               :seinfeld {:stream
                          (yts
                           (str "seinfeld " " "
                                " kramer"))
                          :out "/tv/seinfeld"}}
          {:keys [stream out]
           } (
              :snl
              dns)]
      (d/search-and-youtube-dl
       stream
       ;; the directory here is relative
       ;; to `$HOME/tmp`
       out)))

  (realized? *!!cmbsh*)

  ;; has meta db
  (map :title
       (:full-search-result @*!!cmbsh*))

  ;; dashboard
  ((juxt :num-exit-oks :num-exited
         :num-started :num-tot)
   (d/ytdl-scheduler-stats-agg
    (:mcash-ytdl @*!!cmbsh*)))

  ;; these are all partial percentages
  ;; ... probably best to have labelled
  ;;  perc.s and only for those that
  ;;  have not `exited` according to semantics
  ;; of above `juxt`
  (:stat-line
   (d/ytdl-scheduler-stats-agg
    (:mcash-ytdl @*!!cmbsh*)))

  ;; scheduling internals :/ such as they are

  (let [oia (fn [an hb]
              (when-let [!a (get hb an)]
                @!a))]
    (->> @*!!cmbsh* :mcash-ytdl
         :!heartbeats deref first
         ((juxt (partial oia :!loop-cnt)
                (partial oia :!kickoff-cnt)
                (partial oia :!added-ids)))))

  ;; todo:
  ;; there are two alternate scheduler
  ;; implementations (see `core`).
  ;; write corresponding desktop/dashboard
  ;; functionality for each w/ `trikl`.

  ;; this is the stat that's internally
  ;; compared to
  ;; `core/*max-ytdl-mc-proc*`
  ;; during the heartbeat loop
  (->> (-> @*!!cmbsh* :mcash-ytdl
           :!procs deref)
       vals (filter some?)
       (remove (comp realized? :res))
       count)

  ;; currently heartbeats kicked off
  ;; by inspecting loop count for completion.
  ;; since results via channel don't support
  ;; polling,
  ;; store result to atom alongside `:!*-cnt`
  ;; atoms.
  (-> @*!!cmbsh* :mcash-ytdl
      :!heartbeats deref
      first
      :!loop-cnt deref)

  ;;
  ;; kickoff additional heartbeat(s)
  ;;

  (swap! (:!heartbeats (:mcash-ytdl @*!!cmbsh*))
         conj
         (core/new-mcash-ytdl-heartbeat
          (:mcash-ytdl @*!!cmbsh*)
          ;; duration (hours)
          4
          ;; interval (seconds)
          3))


  )

(comment
  "multi-scheduler-setup and teardown"

  (let [name-kw :snl-01]
    (swap! !schedulers assoc
           name-kw
           *!!cmbsh*)
    nil)

  (keys @!schedulers)

  (let [name-kw :snl-01]
    (def ^:dynamic *!!cmbsh*
      (get @!schedulers
           name-kw))
    nil)

  )

(comment
  "debug scheduler and/or add violence"

  (def ^:dynamic *sr*
    (:full-search-result @*!!cmbsh*))

  (def ^:dynamic *cmcash*
    (:mcash-ytdl @*!!cmbsh*))

   ;; choke engine
   ;; by adding timeouts to all active processes
   ;; timeout in mintues
   (let [active-ids
         (clojure.set/difference
          (set
           (filter
            (comp some?
                  (partial core/done? *cmcash*))
            *sr*))
          (set
           (filter
            (partial core/done? *cmcash*)
            *sr*)))]
     (for [active-id active-ids]
       (swap! (:!procs *cmcash*)
              update active-id
              (partial core/add-timeout-to-future
                       45))))

   (let [active-ids
         (clojure.set/difference
          (set
           (filter
            (comp some?
                  (partial core/done? *cmcash*))
            *sr*))
          (set
           (filter
            (partial core/done? *cmcash*)
            *sr*))
          )]
     (->>
      (:procs
       {:stdout
        (map
         (comp last
               (partial d/ytdl-scheduler-stdout
                        *cmcash*))
         active-ids)
        :procs
        (into {}
              (filter
               (comp (partial contains? active-ids)
                     first))
              (-> *cmcash* :!procs deref))
        })
      vals first :res))


   )

(comment
  "later on, add playback"
  (def ^:dynamic *playlist*
    (map (partial core/file-path *cmcash*)
         *sr*))
  "or compose to prevent feature creep"
  )
