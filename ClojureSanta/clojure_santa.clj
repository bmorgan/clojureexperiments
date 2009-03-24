(def printerQueue (java.util.concurrent.LinkedBlockingQueue. 3));used to execute print for threads.

(defstruct stateStruct :availableSpace :numWaiters :numConsulting)
(def helpersState (struct stateStruct (ref 0) (ref 0) (ref 0)))
(def deerState (struct stateStruct (ref 0) (ref 0) (ref 0)))
(def state)

(defn myPrint [& message]
  (.put printerQueue message))

(defn coordinateWithSanta []
  (do
    (Thread/sleep (rand-int 5)) ;sleep for some time representing corrrdinating w. santa (e.g. geting my new job assignment)
    (dosync (alter (state :numConsulting) dec) ;notify santa that i am finished.
    (myPrint (.getName (Thread/currentThread)) "done")
    )))

(defn enterOffice []
  ;//wait for santa with java Semaphore.
  (when-not
   (dosync (when (> @(state :availableSpace) 0.0)
               ;(myPrint (.getName (Thread/currentThread)) "went into office")
               (alter (state :availableSpace) dec);take up space in office
               (alter (state :numWaiters) dec)
               true));remove from waiters.
    (Thread/sleep 10)
    (recur)))

(defn meetWithSanta []
   ;//notify santa that i am waiting.
  (dosync (alter (state :numWaiters) inc))
  (enterOffice);after this returns i shoul dbe int santa's office.
  (myPrint (.getName (Thread/currentThread)) "---- went into office ----")
  (coordinateWithSanta)
  ;(exitOffice);no need to exit office explicitly, as long as santa knows i am done coordinating.
)

(defn helperMainLoop [myState maxOutTime] ;available space should be a reference to how many slots remain in santa's office.
  (binding [state myState] ;bind available space to thread local binding.
    (while true ;loop forever
      (Thread/sleep (rand-int maxOutTime)) ;wait for random amount of time, doing my job
      (meetWithSanta)    ;try to meet with santa
      )))

(defn santaDoConsult [groupState groupSize];groupState is the state of the group we picked.
  ;set numConsulting, and notify appropriate group by setting availableSpace
  (do
    (dosync
      (ref-set (groupState :numConsulting) groupSize)
      ;notify group by making space available
      (ref-set (groupState :availableSpace) groupSize))
    ;now wait for the group to finish consulting
    (loop [];just loop until no more consults
      (if (> @(groupState :numConsulting) 0);no need to do transaction, as we are only reading,
        (do (Thread/sleep 11) (recur)))) ;and numConsulting is never inc outside of this func
  ))

(defn santaWaitForGroups []
  (do
    ;wait till either helpers or reindeer have enough waiters to let into office
    (if (>= @(deerState :numWaiters) 8)
      (do (myPrint "santa meeting with reindeer") 
        (santaDoConsult deerState 8)
        (myPrint "santa finished meeting with deer")
        (myPrint ""))
      (if (>= @(helpersState :numWaiters) 3)
        (do (myPrint "santa meeting with helpers") 
          (santaDoConsult helpersState 3)
          (myPrint "santa finished meeting with helpers")
          (myPrint ""))
        (Thread/sleep 7)));nothing is ready yet.
    (recur)))

(defn runPrinter []
  (.start @(def printer (Thread. (fn []
    (do
      (prn (.take printerQueue))
      (recur)))))))

(runPrinter)

(defn main []
    (runPrinter))

(defn startHelpers []
  (let [t 300]
  (.start @(def helper1 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper2 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper3 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper4 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper5 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper6 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper7 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper8 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper9 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper10 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper11 (Thread. (fn [] (helperMainLoop helpersState t)))))
  (.start @(def helper12 (Thread. (fn [] (helperMainLoop helpersState t)))))
    ))

(defn startDeer []
  (let [t 200]
  (.start @(def deer1 (Thread. (fn [] (helperMainLoop deerState t)))))
  (.start @(def deer2 (Thread. (fn [] (helperMainLoop deerState t)))))
  (.start @(def deer3 (Thread. (fn [] (helperMainLoop deerState t)))))
  (.start @(def deer4 (Thread. (fn [] (helperMainLoop deerState t)))))
  (.start @(def deer5 (Thread. (fn [] (helperMainLoop deerState t)))))
  (.start @(def deer6 (Thread. (fn [] (helperMainLoop deerState t)))))
  (.start @(def deer7 (Thread. (fn [] (helperMainLoop deerState t)))))
  (.start @(def deer8 (Thread. (fn [] (helperMainLoop deerState t)))))
  ))

(defn startSanta []
  (.start @(def santa (Thread. (fn [] (santaWaitForGroups))))))

(startHelpers)
(startDeer)
(startSanta)

;(Thread/activeCount)


