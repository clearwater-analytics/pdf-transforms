(defproject pdf-transforms "0.1.0"
  :description "A collections of functions for transforming pdf documents"
  :url "https://github.com/clearwater-analytics/pdf-transforms"
  :dependencies
  [[org.clojure/clojure "1.9.0"]
   [org.apache.pdfbox/pdfbox "2.0.3"]
   [org.bouncycastle/bcmail-jdk15 "1.46"]
   [org.bouncycastle/bcprov-jdk15 "1.46"]
   [prismatic/plumbing "0.5.5"]
   [environ "1.1.0"]]

  :plugins [[lein-environ "1.1.0"]]

  )
