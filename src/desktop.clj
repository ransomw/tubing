(ns desktop
  (:require [clojure.set]
            [me.raynes.fs :as fs]
            [trikl.core :as tui]
            [core]))

(defn ytdl-sched-schedule-all
  [this urls]
  (doseq [href
          ;; this is a possible bug in core/schedule
          (clojure.set/difference
           (set urls)
           (-> this :!procs deref
               keys set))]
    (core/schedule this href)))

;; attach `stream-*/*stream*` to out directory
(defn search-and-youtube-dl
  [stream dir]
  (let [*!!o* (future (stream))
        *cmcash* (core/new-mcash-ytdl
                  (str (fs/home)
                       "/tmp"
                       dir))]
    (future
      (let [fsr @*!!o*
            _ (ytdl-sched-schedule-all
               *cmcash*
               (mapv :href fsr))]
        {:mcash-ytdl
         *cmcash*
         :full-search-result
         fsr}))))

(defn ytdl-scheduler-stdout
  "this is the first step in building le dashboard"
  [cmcash id-url]
  (some-> cmcash :!procs deref
          (get id-url)
          :!out deref))

(defn ytdl-scheduler-stats-agg
  [ys]
  (let [ks (-> ys :!procs deref keys)
        *cmcash* ys
        *sr* ks]
    {:num-tot
     (count *sr*)
     :num-started
     (count
      (filter
       some?
       (map (partial core/done? *cmcash*) *sr*)))
     :num-with-stdout
     (count
      (filter
       #(and % (not (empty? %)))
       (map
        (partial ytdl-scheduler-stdout *cmcash*)
        *sr*)))
     :num-with-filename
     (count
      (filter
       some?
       (map (partial core/file-path *cmcash*)
            *sr*)))
     :num-exited ;; via future
     (count
      (filter
       identity
       (map (partial core/done? *cmcash*)
            *sr*)))
     :num-exit-oks
     (count
      (filter
       true?
       (map (partial core/done-ok? *cmcash*)
            *sr*)))
     :filenames
     (filter
      some?
      (map (partial core/file-path *cmcash*)
           *sr*))
     :stat-line
     (->>
      (map
       (partial ytdl-scheduler-stdout *cmcash*)
       *sr*)
      (remove #(or (nil? %) (empty? %)))
      (map
       (fn [lines]
         (->> (map
               #(re-find
                 #"^\[download\] *([^ ]*).*" %)
               lines)
              (remove nil?)
              (map second)
              last)))
      (remove #(or (nil? %) (empty? %)))
      (remove #(or (= "100%" %)
                   (= "100.0%" %))))
     :stdout-from-err-procs
     (->> *sr*
          (filter
           (comp identity
                 (partial core/done? *cmcash*)))
          (filter
           (comp not true?
                 (partial core/done-ok? *cmcash*)))
          (map
           (partial
            ytdl-scheduler-stdout *cmcash*)))
     }))

;;;;

(comment


  (def ^:dynamic *!tui-clients* (atom []))

  (def ^:dynamic *tui-port* 1357)

  (def ^:dynamic *tui-stop-server*
    (tui/start-server
     #(swap! *!tui-clients* conj %)
     *tui-port*))

  (tui/render
   (last @*!tui-clients*)
   [:box {:x 10 :y 5 :width 20 :height 10
          :styles {:bg [50 50 200]}}
    [:box {:x 1 :y 1 :width 18 :height 8
           :styles {:bg [200 50 0]}}
     [:cols
      [:rows
       [:span {:styles {:fg [100 250 100]}}
        "hello \n"]
       [:span {:styles {:fg [100 250 100]}}
        "hello world"]]
      [:rows
       [:span {:styles {:fg [100 250 100]}}
        "you"]
       [:span {:styles {:fg [100 250 100]}}
        "there"]
       ]]]])

  )
