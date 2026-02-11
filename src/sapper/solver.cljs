(ns sapper.solver
  (:require
   [clojure.string :as str]))

(def ^:private wasm nil)
(def ^:private input-buf nil)
(def ^:private output-buf nil)
(def ^:private encoder (js/TextEncoder.))
(def ^:private decoder (js/TextDecoder.))

(defn init [cb]
  (-> (js/fetch "solver.wasm")
    (.then #(.arrayBuffer %))
    (.then #(js/WebAssembly.instantiate %))
    (.then (fn [result]
             (let [instance (.-instance result)
                   exports  (.-exports instance)
                   memory   (.-memory exports)]
               (set! wasm exports)
               (set! input-buf
                 (js/Uint8Array. (.-buffer memory) (.getInputBuf exports) 8192))
               (set! output-buf
                 (js/Uint8Array. (.-buffer memory) (.getOutputBuf exports) 8192))
               (cb))))
    (.catch (fn [err]
              (println "Failed to load solver.wasm:" err)
              (cb)))))

(defn solve [input]
  (when wasm
    (let [encoded  (.encode encoder input)
          _        (.set input-buf encoded)
          len      (.solve wasm (.-length encoded))]
      (when (pos? len)
        (let [result (.decode decoder (.slice output-buf 0 len))]
          (str/replace result "\n" ""))))))
