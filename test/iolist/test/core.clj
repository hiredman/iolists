(ns iolist.test.core
  (:use [clojure.test]
        [hiredman.iolist])
  (:require [clojure.java.jmx :as jmx])
  (:import (java.io ByteArrayInputStream
                    ByteArrayOutputStream)))

(defn used []
  (:used (:HeapMemoryUsage (jmx/mbean "java.lang:type=Memory"))))

(defmacro with-constant-heap [& body]
  (let [runs 120]
    `(letfn [(f# []
               (let [u# (used)
                     b# (do ~@body)
                     _# (Thread/sleep 1000)
                     u1# (used)]
                 (- u1# u#)))]
       (is (> 1e3
              (double (/ (apply + (doall (repeatedly ~runs f#)))
                         ~runs)))))))

(deftest the-test
  (let [baos (ByteArrayOutputStream. 1024)]
    (testing "write a string"
      (write "foo" baos)
      (is (= "foo" (String. (.toByteArray baos)))))
    (.reset baos)
    (testing "write a byte array"
      (write (.getBytes "foo") baos)
      (is (= "foo" (String. (.toByteArray baos)))))
    (.reset baos)
    (testing "write a collection"
      (write ["foo" {"bar" "baz"}] baos)
      (is (= "foobarbaz" (String. (.toByteArray baos)))))
    (.reset baos)
    (testing "write a byte array nested in a collection"
      (write ["foo" {"bar" (.getBytes "baz")}] baos)
      (is (= "foobarbaz" (String. (.toByteArray baos)))))))

#_(deftest t-constant-size
  (let [data #(repeat 1e4 {"foo" ["bar" (.getBytes "h")]})]
    (with-open [fouts (java.io.FileOutputStream. "/tmp/mm")]
      (with-constant-heap
        (doall (data))
        #_(write (data) fouts)))))
