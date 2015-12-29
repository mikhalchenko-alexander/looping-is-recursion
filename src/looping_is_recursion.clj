(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (== exp 0)
                   acc
                   (recur (* acc base) base (dec exp))))]
  (helper 1 base exp)))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn last-element [a-seq]
  (cond
   (empty? a-seq)
   nil

   (== 1 (count a-seq))
   (first a-seq)

   :else
   (recur (rest a-seq))))

(last-element [])      ;=> nil
(last-element [1 2 3]) ;=> 3
(last-element [2 5])   ;=> 5

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))
   true

   (or (empty? seq1) (empty? seq2))
   false

   (not= (first seq1) (first seq2))
   false

   :else
   (recur (rest seq1) (rest seq2))))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false


(defn find-first-index [pred a-seq]
  (loop [idx 0
         sq a-seq]
    (cond
     (empty? sq)
     nil

     (pred (first sq))
     idx

     :else
     (recur (inc idx) (rest sq)))))

(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])                                    ;=> nil

(defn avg [a-seq]
  (loop [s a-seq
         sum 0
         cnt 0]
    (if (empty? s)
     (/ sum cnt)
     (recur (rest s) (+ sum (first s)) (inc cnt)))))

(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn parity [a-seq]
  (def toggle (fn [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem))))

  (loop [acc #{}
         s a-seq]
    (if
      (empty? s)
      acc
      (recur (toggle acc (first s)) (rest s)))))

(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}

(defn fast-fibo [n]
  (loop [n-curr 0
         f-curr 0
         f-next 1]
    (if (== n-curr n)
      f-curr
      (recur
       (inc n-curr)
       f-next
       (+ f-curr f-next)))))

(fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=> 3
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8

(defn cut-at-repetition [a-seq]
  (loop [acc []
         unique #{}
         s a-seq]
    (if (or (empty? s) (contains? unique (first s)))
      acc
      (recur (conj acc (first s)) (conj unique (first s)) (rest s)))))

(cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]
