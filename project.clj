(defproject wiki-crawler "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
    :global-vars {*warn-on-reflection* true
                *assert* false}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.jsoup/jsoup "1.9.2"]])
