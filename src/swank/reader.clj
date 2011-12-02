(ns swank.reader
  (:require [clojure.zip :as z])
  (:import (clojure.lang Counted)))

(defn x [f]
  (let [foo 1
        a 10
        barbaz 3]
    (f foo)))

(def EOF (Object.))
(def BOF :BOF)
(def EOL (Object.))
(def no-data (Object.))
(def end-of-vector (Object.))

(defrecord Node [location data])

(defprotocol TextNode
  (position [tn])
  (subnodes [tn])
  (text [tn])
  (data [tn])
  (rebuild [tn subnodes]))

(defrecord SymbolNode [location symbol]
  TextNode
  (position [tn] location)
  (subnodes [tn] nil)
  (text [tn] (str symbol))
  (data [tn] symbol)
  (rebuild [tn subnodes] tn))

(defrecord StringNode [location str]
  TextNode
  (position [tn] location)
  (subnodes [tn] nil)
  (text [tn] str)
  (data [tn] str)
  (rebuild [tn subnodes] tn))

(defrecord SpaceNode [location space]
  TextNode
  (position [tn] location)
  (subnodes [tn] nil)
  (text [tn] space)
  (data [tn] no-data)
  (rebuild [tn subnodes] tn))

(defrecord ListNode [location lst]
  TextNode
  (position [tn] location)
  (subnodes [tn] lst)
  (text [tn]
    (format "(%s)"
            (apply str (map text (subnodes tn)))))
  (data [tn] (remove (partial = no-data) (map data lst)))
  (rebuild [tn subnodes]
    (ListNode. location subnodes)))

(defrecord VectorNode [location v]
  TextNode
  (position [tn] location)
  (subnodes [tn] (seq v))
  (text [tn]
    (format "[%s]" (apply str (map text (subnodes tn)))))
  (data [tn] (vec (remove (partial = no-data) (map data (subnodes tn)))))
  (rebuild [tn subnodes]
    (VectorNode. location subnodes)))

(defprotocol IRead
  (location [rdr])
  (pushback-character [rdr char])
  (read-character [rdr])
  (read-object [rdr]))

(defmulti read-form (fn [char rdr] char))

(defmethod read-form \( [char rdr]
  (loop [buf []]
    (let [obj (read-object rdr)]
      (cond
       (= EOL obj) (ListNode. (location rdr) (seq buf))
       (= EOF obj) (throw (IllegalStateException. "foo"))
       :else (recur (conj buf obj))))))

(defmethod read-form \[ [char rdr]
  (loop [buf []]
    (let [obj (read-object rdr)]
      (cond
       (= end-of-vector obj) (VectorNode. (location rdr) (seq buf))
       (= EOF obj) (throw (IllegalStateException. "foo"))
       :else (recur (conj buf obj))))))

(defmethod read-form  \) [char rdr] EOL)

(defmethod read-form  \] [char rdr] end-of-vector)

(defn read-whitspace [char rdr]
  (loop [buf [] char char]
    (if (Character/isWhitespace char)
      (recur (conj buf char) (read-character rdr))
      (do
        (pushback-character rdr char)
        (SpaceNode. (location rdr) (apply str buf))))))

(defn read-symbol [char rdr]
  (loop [buf [] char char]
    (if (or (Character/isWhitespace char)
            (contains? (methods read-form) char))
      (do
        (pushback-character rdr char)
        (let [s (apply str buf)]
          (if (.startsWith s ":")
            (SymbolNode. (location rdr) (keyword (subs s 1 (count s))))
            (SymbolNode. (location rdr) (symbol s)))))
      (recur (conj buf char) (read-character rdr)))))

(defn read-a-string [char rdr]
  (loop [buf [] char char]
    (cond
     (= char \") (StringNode. (location rdr) (apply str buf))
     (= char \\) (recur (conj buf (read-character rdr))
                        (read-character rdr))
     :else (recur (conj buf char) (read-character rdr)))))

(defmethod read-form EOF [char rdr] EOF)

(defmethod read-form :default [char rdr]
  (cond
   (Character/isWhitespace char) (read-whitspace char rdr)
   (= char \") (read-a-string (read-character rdr) rdr)
   :else (read-symbol char rdr)))

(deftype LispTextReader [ins location buf]
  IRead
  (location [rdr] @location)
  (pushback-character [rdr char]
    (swap! buf conj char)
    (swap! location dec))
  (read-character [rdr]
    (let [r (if (empty? @buf)
              (.read ins)
              (let [c (peek @buf)]
                (swap! buf pop)
                c))]
      (if (= -1 r)
        EOF
        (do
          (swap! location inc)
          (char r)))))
  (read-object [rdr]
    (if (neg? @location)
      (do
        (reset! location 0)
        (reify
          TextNode
          (position [_] 0)
          (subnodes [_] nil)
          (text [_] "")
          (data [_])
          Object
          (toString [_] "BOF")))
      (let [character (read-character rdr)]
        (read-form character rdr)))))


(defn lisp-text-reader [ins]
  (LispTextReader. ins (atom -1) (atom [])))

(def node-zip (partial z/zipper subnodes subnodes rebuild))

(defn skip-whitespace-nodes [loc]
  (if (instance? SpaceNode (z/node loc))
    (recur (z/right loc))
    loc))

(defn align-let [loc]
  (let [x (-> loc z/down z/right skip-whitespace-nodes)
        spacer-size (->> (z/node x)
                         subnodes
                         (partition-all 2)
                         (map first)
                         (map text)
                         (map count)
                         (apply max)
                         inc)
        x (z/down x)]
    (loop [x x]
      (if (instance? SpaceNode (z/node x))
        (recur (z/right x))
        (let [s (count (text (z/node x)))
              x (-> x
                    z/right
                    (z/replace
                     (SpaceNode.
                      -1 (apply str (repeat (- spacer-size s) " "))))
                    z/right)]
                  (if (z/right x)
                    (recur (z/right x))
                    x))))))

(defn read*
  ([ins] (read* ins *out*))
  ([ins out]
     (binding [*out* out]
       (let [rdr (lisp-text-reader ins)]
         (dotimes [i 100]
           (let [o (read-object rdr)]
             (loop [x (node-zip o)]
               (if (z/end? x)
                 (print (text (z/root x)))
                 (if (and (instance? ListNode (z/node x))
                          (= 'let (data (first (subnodes (z/node x))))))
                   (recur (z/next (align-let x)))
                   (recur (z/next x)))))))))))
