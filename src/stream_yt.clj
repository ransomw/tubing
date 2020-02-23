(ns stream-yt
  (:require [clojure.string :as string]
            [clojure.java.io]
            [me.raynes.conch :as sh]
            ))

(defn yt-search
  "wrap the binary built by `run.sh`"
  [terms & {:keys [max-results]
            :or {max-results 10}}]
  (let [repo-root
        ;; https://stackoverflow.com/a/26365097
        (.getCanonicalPath (clojure.java.io/file "."))
        build-dir
        (str repo-root "/" "yt-search")
        stdout-to-search-res
        (fn [out-str]
          (->> (string/split out-str #"\n")
               (take-while (comp not empty?))
               (map (fn [line]
                      (-> (->> line
                               (re-find #"^\[(.*)\] (.*)$")
                               (drop 1)
                               (zipmap [:href :title]))
                          )))
               (filter (comp not empty?))
               (map (fn [r] (update r :href
                                    #(str "https://www.youtube.com/watch?v=" %))))))]
    (->
     (sh/let-programs [s (str build-dir "/yt-search")]
       (s "--query" (str terms)
          "--max-results" (str max-results)
          {:dir build-dir}))
     stdout-to-search-res)))

(def ^:dynamic *stream*
  (partial yt-search "saturday night live"
           :max-results 50))

(comment
  (yt-search "gnarls barkley")
  )
