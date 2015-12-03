(ns mq.core)

(use '[clojure.string :only (join)])

(defn gen-primes "Generates an infinite, lazy sequence of prime numbers"
  []
  (let [reinsert (fn [table x prime]
                   (update-in table [(+ prime x)] conj prime))]
    (defn primes-step [table d]
                 (if-let [factors (get table d)]
                   (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                          (inc d))
                   (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))
                                                 (inc d))))))
    (primes-step {} 2)))

(defn m [n] 
  (loop [m 1 i 0]
    (if (> m n)
      i
      (recur (* m (nth (gen-primes) i)) (inc i)))))

(def start-at 1000000)

(def slice 100000)

(def primes-1
  (take slice (drop start-at (gen-primes))))

(def primes-2
  (take slice (drop (+ start-at slice) (gen-primes))))

(defn res [f i]
  (let [ps (take (* f (m (* (last example-primes-1) (last example-primes-2)))) (gen-primes))]
    (join ", " (drop 1 (map #(mod i %) ps)))))

(defn line [i]
  (let [ p (rand-nth primes-1) q (rand-nth primes-2)]
    (str (res 1 p) ", " (res 1 q) ", " (res 3 (* p q)))))

(defn products [n] 
  (join "\n" (map #(line %) (range n))))

 

