(ns crypto-challenge.core
  (:gen-class)
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.data.priority-map :refer :all]
            [clojure.data.codec.base64 :as b64]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bit Manipulation / Text Encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn encode-hex
  "Encode a string as hex"
  [s]
  (format "%x" (new java.math.BigInteger (.getBytes s))))

(defn encode-hex-bytes
  "Encode a string as hex, return as a seq of bytes"
  [s]
  (.getBytes (encode-hex s)))

(defn decode-hex-bytes
  "Decode a hex-encoded string as a seq of bytes"
  [s]
  (map (fn [[x y]] (char (Integer/parseInt (str x y) 16)))
       (partition 2 s)))

(defn decode-hex
  "Decode a hex-encoded string as a string"
  [s]
  (apply str (decode-hex-bytes s)))

(defn encode-base64
  "Encode a string as base64"
  [s]
  (String. (b64/encode (.getBytes s)) "UTF-8"))

(defn decode-base64
  "Decode a base64-encoded string"
  [s]
  (String. (b64/decode (.getBytes s))))

(defn apply-xor-bytes
  "XOR two strings, get the result as a seq of bytes"
  [a b]
  (let [abytes (.getBytes a)
        bbytes (.getBytes b)
        result (map bit-xor abytes bbytes)]
    result))

(defn apply-xor
  "XOR two strings, get the result as a string"
  [a b]
  (let [result (apply-xor-bytes a b)]
    (apply str (map char result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn repeat-char
  "Builds a string of a given character of `n` length"
  [c n]
  (apply str (take n (repeatedly #(str c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn intersect
  "Get the intersection of two sequences"
  [xs ys]
  (clojure.set/intersection (set xs) (set ys)))

(defn to-prioritymap
  "Convert a map of any kind to a priority map"
  [m]
  (apply priority-map (apply concat m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frequency Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def english-frequencies
  "Get a map of the letters of the alphabet and their average
   frequency of appearance in most English text"
  (priority-map \space, 12.50, ;; Spaces are more common than the rest
                \E, 12.49, \T, 9.28, \A, 8.04, \O, 7.64,  \I, 7.57, \N, 7.23,
                \S, 6.51,  \R, 6.28, \H, 5.05, \L, 4.07,  \D, 3.82, \C, 3.34,
                \U, 2.73,  \M, 2.51, \F, 2.40, \P, 2.14,  \G, 1.87, \W, 1.68,
                \Y, 1.66,  \B, 1.48, \V, 1.05, \K, 0.54,  \X, 0.23, \J, 0.16,
                \Q, 0.12,  \Z, 0.09))

(def common-bigrams
  "A vector of common English bigrams"
  (clojure.string/split "TH HE IN EN NT RE ER AN TI ES ON AT SE ND OR AR AL TE CO DE TO RA ET ED IT SA EM RO ST OF HI AS IS" #"\s"))

(def common-trigrams
  "A vector of common English trigrams"
  (clojure.string/split "THE AND THA ENT ING ION TIO FOR NDE HAS NCE EDT TIS OFT STH MEN" #"\s"))

(defn ngram
  "Generate a vector of n-grams in the given string"
  [n s]
  (vec (distinct (map #(apply str %)
                 (partition n 1 (.toUpperCase s))))))

(defn ngram-commonality
  "Score a string on the number of common English n-grams it contains"
  [n s common-ngrams]
  (let [ngrams   (ngram n s)
        common   (intersect ngrams common-ngrams)
        score    (count common)
        maxscore (count common-ngrams)]
    (Math/ceil (* (/ score maxscore) 100))))

(defn score-letter-frequency
  "Score a string on the amount of overlap it has with average English letter frequency"
  [text]
  (let [alphabet        (vec (char-array "ABCDEFGHIJKLMNOPQRSTUVWXYZ "))
        english-freqs   (rseq english-frequencies)
        text-freqs      (rseq (to-prioritymap
                                 (select-keys (frequencies (.toUpperCase text))
                                              alphabet)))
        most-frequent   #(keys (take %2 %))
        less-frequent   #(keys (take-last %2 %))
        n-most-frequent 6
        top             (count (intersect (most-frequent english-freqs n-most-frequent)
                                          (most-frequent text-freqs    n-most-frequent)))
        bottom          (count (intersect (less-frequent english-freqs n-most-frequent)
                                          (less-frequent text-freqs    n-most-frequent)))]
    (Math/ceil (* (/ (+ top bottom) 12) 100))))

(defn is-english?
  "Combine letter, bigram, and trigram frequency scores to score a string on the likelihood that it is an English phrase.
   Weighting is applied most heavily to trigrams, followed by bigrams, then letters.
   The score produced is between 0 and 100. 100 being almost certainly English, 0 obviously not English."
  [s]
  (let [frequency-score (score-letter-frequency s)
        bigram-score    (* (ngram-commonality 2 s common-bigrams) 2)
        trigram-score   (* (ngram-commonality 3 s common-trigrams) 3)
        total-score     (+ frequency-score bigram-score trigram-score)
        interpolated    (Math/ceil (/ (* (- total-score 100) 100) 100))]
    (if (> total-score 100)
      interpolated
      (+ 100 interpolated))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File IO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-resource
  [r]
  (-> r
      io/resource
      io/file))

(defn get-lines
  [r]
  (let [contents (slurp (get-resource r))]
    (clojure.string/split contents #"\n")))

;; CHALLENGE #1
;; Convert the hex-encoded string to base64 and back, preserving the original value.

(defn hex-to-base64-and-back
  []
  (let [original "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        expected "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        decoded  (decode-hex    original)
        base64   (encode-base64 decoded)
        hex      (decode-base64 base64)
        encoded  (encode-hex    hex)]
    (is (= expected base64))
    (is (= original encoded))))


;; CHALLENGE #2
;; Given two equal length buffers, produce their XOR sum

(defn xor-two-buffers
  []
  (let [one      (decode-hex "1c0111001f010100061a024b53535009181c")
        two      (decode-hex "686974207468652062756c6c277320657965")
        expected (decode-hex "746865206b696420646f6e277420706c6179")
        result   (apply-xor one two)]
    (is (= result expected))))


;; CHALLENGE #3
;; Decrypt a cipher which was XOR'd against a single character key
;; using frequency analysis to determine if decryption was successful

(defn xor-map
  "Generate a map of the result of applying an XOR of each key in `cipherkeys` against `cipher`"
  [cipherkeys cipher]
  (let [mapped (map #(assoc {} (first %) (apply-xor % cipher)) cipherkeys)]
    (apply array-map (vec (apply concat (map first mapped))))))

(defn decrypt-single-char-xor
  "Decrypts a given cipher, assuming it was XOR'd with a single character key"
  [cipher]
  (let [decoded      (decode-hex cipher)
        len          (count decoded)
        ;; Get a sequence of all the single character cipher keys we wish to test
        single-chars (map str (char-array "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
        ;; Map the single char key candidates to keys of equal length as the cipher
        cipherkeys   (map #(apply str (repeat len %)) single-chars)
        ;; Get a map of applying XOR to each key and the decoded cipher
        potentials   (xor-map cipherkeys decoded)
        ;; Score each result based on the likelihood it's English
        scores       (map #(assoc {} (key %) (is-english? (val %))) potentials)
        ;; Our candidate for successful decryption is the highest scoring result
        candidate    (key (first (reduce #(if (> (val (last %))
                                                 (val (last %2)))
                                            %
                                            %2)
                                         scores)))]
    (get potentials candidate)))

;; CHALLENGE #4
;; In a file of 60 random strings XOR'd with a single character key,
;; find the one which is valid English
(defn detect-single-char-xor-message
  []
  (let [strings    (get-lines "gistfile1.txt")
        candidates (map decrypt-single-char-xor strings)]))

;;(hex-to-base64-and-back)
;;(xor-two-buffers)
;;(decrypt-single-char-xor "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
