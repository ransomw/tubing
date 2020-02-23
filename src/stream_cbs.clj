(ns stream-cbs
  (:require [clojure.string :as string]
            [clj-http.client :as http-c]
            [hickory.core :as puccih]
            [hickory.select :as hsel]
            ))

(defn cbs-stream
  []
  (let [fcbs
        (->
         "https://www.cbsnews.com/"
         (http-c/get)
         :body puccih/parse puccih/as-hickory)
        id-re
        #"https://www.cbsnews.com/video/(.*)/"
        video-selector
        (hsel/child (hsel/and
                     (hsel/class
                      "item--type-video")
                     (hsel/tag :article))
                    hsel/first-child)
        title-selector
        (hsel/descendant
         (hsel/class "item--type-video")
         (hsel/and (hsel/tag :h4)
                   (hsel/class "item__hed")))
        ]
    (->> (map #(zipmap [:href :title] [%1 %2])
              (->> fcbs
                   (hsel/select video-selector)
                   (map :attrs)
                   (map :href)
                   vec)
              (->> fcbs
                   (hsel/select title-selector)
                   (map :content)
                   (map string/join)
                   (map string/trim)
                   vec)
              )
         (filter #(re-matches id-re (:href %)))
         )))

;; todo: spec
(def ^:dynamic *stream* cbs-stream)

(comment

  (def *fcbs-00*
    (->
     "https://www.cbsnews.com/"
     (http-c/get)
     :body puccih/parse puccih/as-hickory))


  (let [fcbs
        *fcbs-00*
        id-re
        #"https://www.cbsnews.com/video/(.*)/"
        video-selector
        (hsel/child (hsel/and
                     (hsel/class
                      "item--type-video")
                     (hsel/tag :article))
                    hsel/first-child)
        title-selector
        (hsel/descendant
         (hsel/class "item--type-video")
         (hsel/and (hsel/tag :h4)
                   (hsel/class "item__hed")))
        ]
    (->> (map #(zipmap [:href :title] [%1 %2])
              (->> fcbs
                   (hsel/select video-selector)
                   (map :attrs)
                   (map :href)
                   vec)
              (->> fcbs
                   (hsel/select title-selector)
                   (map :content)
                   (map string/join)
                   (map string/trim)
                   vec)
              )
         (filter #(re-matches id-re (:href %)))
         ))

)
