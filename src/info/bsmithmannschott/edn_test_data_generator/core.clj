(ns info.bsmithmannschott.edn-test-data-generator.core
  (:require [clojure.test.generative.generators :as gen]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]))


;;; a quick hack to benchmarking data for edn parsers
;;; redefine dst-dir, as you like, then run write-test-data

(def dst-dir "/tmp")
(declare write-test-data)


(defn gen-decimal [] (BigDecimal. (gen/double)))

(defn write-test-data [file]
  (spit file (with-out-str
               (prn (create-test-data)))))



(def large-size 2048)
(def med-size 256)
(def small-size 8)

(def name-sizer #(+ (gen/uniform 1 6)
                    (gen/uniform 0 6)
                    (gen/uniform 0 6)
                    (gen/uniform 0 6)))

(def large-sizer (constantly large-size))
(def med-sizer (constantly med-size))
(def small-sizer (constantly small-size))

(defn dump-edn [file edn]
  (spit file (with-out-str
               (println ";;; -*- mode: clojure; coding: utf-8 -*-")
               (println ";;; https://github.com/edn-format/edn")
               (println)
               (pp/pprint edn)
               (println))))

(defn make-symbol [] (gen/symbol name-sizer))
(defn make-keyword [] (gen/keyword name-sizer))

(def make-char
  (let [chars (concat (map char (range 33 127))
                      "\n\t\f\r ")]
    (fn [] (gen/rand-nth chars))))

(defn make-bigint []
  (* (bigint (gen/long))
     (bigint (gen/long))))

(defn make-bigdec []
  (BigDecimal. (/ (gen/double)
                  (max (gen/double) 0.0000001))))

(def make-inst
  (let [now (.getTime (java.util.Date.))]
    (fn [] 
      (java.util.Date. (gen/uniform 0 (* 2 now))))))

(def make-uuid
  (fn [] (java.util.UUID/randomUUID)))

(def make-nil (constantly nil))


(def tree-depth 6)

(defn map-tree [d]
  (if (pos? d)
    (gen/hash-map make-keyword  
                  #(map-tree (dec d)) (constantly (- tree-depth (dec d))))
    (gen/scalar)))

(defn vector-tree [d]
  (if (pos? d)
    (gen/vec #(vector-tree (dec d)) (constantly (- tree-depth (dec d))))
    (gen/scalar)))

(def make-scalar
  (let [scalars [(constantly nil)
                 gen/boolean
                 make-char
                 make-symbol
                 make-keyword
                 gen/string
                 gen/int
                 gen/long
                 gen/double
                 make-bigint
                 make-bigdec]]
    (fn []
      ((gen/rand-nth scalars)))))

(def jobs
  {"vector-of-nil" #(vec (repeat large-size nil))
   "list-of-nil" #(repeat large-size nil)
   "vector-of-booleans" #(gen/vec gen/boolean large-sizer)
   "vector-of-chars" #(gen/vec make-char large-sizer)
   "vector-of-symbols" #(gen/vec make-symbol large-sizer)
   "vector-of-keywords" #(gen/vec make-keyword large-sizer)
   "vector-of-strings" #(gen/vec gen/string large-sizer)
   "vector-of-ints" #(gen/vec gen/int large-sizer)
   "vector-of-longs" #(gen/vec gen/long large-sizer)
   "vector-of-doubles" #(gen/vec gen/double large-sizer)
   "vector-of-bigints" #(gen/vec make-bigint large-sizer)
   "vector-of-bigdecs" #(gen/vec make-bigdec large-sizer)
   "vector-of-instants" #(gen/vec make-inst large-sizer)
   "vector-of-uuid" #(gen/vec make-uuid large-sizer)
   "set-of-keywords" #(gen/set make-keyword large-sizer)
   "set-of-symbols"  #(gen/set make-symbol large-sizer)
   "set-of-longs" #(gen/set gen/long large-sizer)
   "large-keyword-map"  #(gen/hash-map make-keyword nil large-sizer)
   "large-symbol-map" #(gen/hash-map make-symbol nil large-sizer)
   "vector-of-vectors" (fn []
                         (gen/vec #(gen/vec nil small-sizer) 
                                  med-sizer))
   "map-of-maps" (fn []
                   (gen/hash-map make-keyword 
                                 (constantly 
                                  (gen/hash-map make-keyword
                                                nil small-sizer))
                                 med-sizer))
   "vecor-of-maps" (fn []
                     (gen/vec (constantly
                               (gen/hash-map make-keyword
                                             nil small-sizer))
                              med-sizer))
   "map-tree" #(map-tree tree-depth)
   "vector-tree" #(vector-tree tree-depth)
   "mixed-vector" #(gen/vec make-scalar large-sizer)
   })


(defn write-test-data []
  (dorun 
   (pmap #(dump-edn (io/file dst-dir (str %1 ".edn")) (%2))
         (keys jobs) (vals jobs))))