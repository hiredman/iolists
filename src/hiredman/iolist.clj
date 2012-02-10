(ns hiredman.iolist)

(defprotocol Writer
  (write-bytes [writer bytes])
  (write-integers [writer bytes] "write array of ints")
  (write-longs [writer longs] "write array of longs")
  (write-array [writer arr])
  (write-character-sequence [writer character-sequence]))

(defprotocol Word
  (write! [word writer]))

(extend-protocol Word
  (Class/forName "[B")
  (write! [word writer]
    (io!
     (write-bytes writer word)
     nil))
  CharSequence
  (write! [word writer]
    (io!
     (write-character-sequence writer word)
     nil))
  Object
  (write! [word writer]
    (io!
     (if (and word (.isArray (class word)))
       (do
         (extend-protocol Word
           (class word)
           (write! [word writer]
             (write-array writer word)))
         (write word writer))
       (let [[f & r] (seq word)]
         (when f
           (if (coll? f)
             (recur (concat f r) writer)
             (do
               (write f writer)
               (recur r writer))))))
     nil)))

(extend-protocol Writer
  java.io.OutputStream
  (write-array [os arr]
    (dotimes [i (count arr)]
      (write (aget arr i) os))
    nil)
  (write-longs [os longs]
    (dotimes [i (count longs)]
      (let [l (aget longs i)]
        (.write os (int (bit-and l (long 0xFFFFFFFF00000000))))
        (.write os (int (bit-and l (long 0x00000000FFFFFFFF))))))
    nil)
  (write-integers [os integers]
    (dotimes [i (count integers)]
      (.write os (aget integers i)))
    nil)
  (write-bytes [os bytes]
    (.write os bytes)
    nil)
  (write-character-sequence [os chars]
    (dotimes [i (count chars)]
      (.write os (int (.charAt chars i))))
    nil))
