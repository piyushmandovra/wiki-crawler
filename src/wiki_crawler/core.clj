(ns wiki-crawler.core
  (:require [clojure.string :as s])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]))

(use 'clojure.repl)

(def base-url
  "https://en.wikipedia.org")

(defn fetch [page-link]
  (.get (Jsoup/connect (str base-url page-link))))

;; type hint
(defn text [^Element x]
  (.text x))

(defn title [^Document doc]
  (text (first (.select doc "h1"))))

(defn last-modified [^Document doc]
  (let [lm-text (text (first (.select doc "#footer-info-lastmod")))]
    (last (s/split lm-text #"last modified on "))))

(defn first-paragraph [^Document doc]
  (first (.select doc "#mw-content-text p")))

(defn first-paragraph-text [^Document doc]
  (text (first-paragraph doc)))

(defn links-to-follow [^Document doc]
  (->> (.select (first-paragraph doc) "a")
       (map #(.attr % "href"))
       (filter #(.startsWith % "/wiki"))))

(defn table-of-contents [^Document doc]
  (map text (.select doc "#toc li .toctext")))

(defn write [{:keys [title] :as page}]
  (let [filename (s/replace title #"\s" "")]
    (spit (str "pages/" filename) page)))

(def counter (atom 0))

(defn info [url num-links-to-follow]
  (let [doc (fetch url)]
    {:title             (title doc)
     :last-modified     (last-modified doc)
     :first-paragraph   (first-paragraph-text doc)
     :table-of-contents (table-of-contents doc)
     :links-to-follow   (take num-links-to-follow (links-to-follow doc))}))


(defn scrape [url num-links-to-follow depth]
  (let [ page (info url num-links-to-follow)]
  (write page)
    (when-not (zero? depth)
      (doall (pmap (fn [link]
                     (scrape link num-links-to-follow (dec depth)))
                   (:links-to-follow page))))))



(defn scrape-new [url num-to-follow depth]
  (swap! counter inc)
  (let [page (info url num-to-follow)]
    (write page)
    (when-not (zero? depth)
      (->> page
           :links-to-follow
           (map (fn [link] (future (scrape-new url num-to-follow (dec depth))) ))
           (doall)
           (map deref)
           (doall)))))


