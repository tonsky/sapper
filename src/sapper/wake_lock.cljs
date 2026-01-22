(ns sapper.wake-lock)

(def sentinel
  nil)

(defn release []
  #_(println "release wake-lock" sentinel)
  (when sentinel
    (-> (.release sentinel)
      (.then
        (fn []
          (set! sentinel nil)))
      (.catch
        (fn [err]
          (println err)
          (set! sentinel nil))))))

(defn request []
  #_(println "request wake-lock")
  (when-some [lock (.-wakeLock js/navigator)]
    (-> (.request lock "screen")
      (.then
        (fn [s]
          (set! sentinel s)
          (.addEventListener sentinel "release"
            (fn [_]
              (set! sentinel nil)))))
      (.catch println))))

(defn maybe-restore [keep-awake]
  (when (and
          keep-awake
          (nil? sentinel))
    (request)))
