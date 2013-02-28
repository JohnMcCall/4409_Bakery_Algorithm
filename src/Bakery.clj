(ns Bakery)
(use 'clojure.test)
(require '[clojure.java.io :as io])

(def console (agent *out*))
(def character-log (agent (io/writer "bakery-states.log" :append true)))

(defn write
  [^java.io.Writer w & content]
  (doseq [x (interpose " " content)]
    (.write w (str x)))
  (doto w
    (.write "\n")
    .flush))


(defn log-reference
  [reference & writer-agents]
  (add-watch reference :log
             (fn [_ reference old new]
               (doseq [writer-agent writer-agents]
                 (send-off writer-agent write new)))))

;; Fibonacci Calculator
(defn fib [n]
  (if (or (= n 0) (= n 1))
    1
    (+ (fib (- n 1)) (fib (- n 2)))
    )
  )

(defn get-fib [server n]
  (assoc server :pastry (fib n))
  )

;; Customers and Servers
(defn customer [id]
  (agent {:id id :ticket-number nil :result nil})
  )

(defn server [id]
  (agent {:id id :pastry nil})
  )


;; Free servers
(def free-servers (ref #{}))
(log-reference free-servers console)
(log-reference free-servers character-log)

;; The Ticket Machine
(def ticket-machine (atom -1))
 
(defn next-number []
  (swap! ticket-machine inc))

;; The "Now Serving" sign
(def now-serving-sign (atom 0))
(log-reference now-serving-sign character-log)

(defn serve-next [id reference oldValue newValue] 
  (if (< (count oldValue) (count newValue)) 
    (if (empty? oldValue)     ; when we put a server into the list
      (swap! now-serving-sign inc)
      nil
      )
     (if (not (empty? newValue))  ; when we take a server out of the list
       (swap! now-serving-sign inc)
       nil
       )
    )
  )

;; Making Pastries
(defn make-pastries [server]
  (let [pastry-number (rand-int 50)]
  (send server get-fib pastry-number)
  (await server)
  )
)

;; Watch function that watches for when the server gets the pastry then gives it to the customer
(defn watch-server [customer reference oldValue newValue]
  (let [pastry (:pastry newValue)] 
    (send customer #(assoc %1 :result %2) pastry)
    (send reference #(assoc %1 :pastry %2) nil)
    (dosync (alter free-servers #(conj %1 %2) reference))
    (remove-watch reference customer)
    )
  )

(defn start-serving [server customer]
  (add-watch server customer watch-server)
  (future (make-pastries server))
  )

(defn get-server []
  (let [the-server (first @free-servers)]
    (dosync
      (alter free-servers #(disj %1 %2) the-server)
      the-server
      )
    )
  )

(defn serve-customer [customer]
  (let [the-server (get-server)]
	  (start-serving the-server customer)
   )
  )

(defn watch-sign [customer reference oldValue newValue]
  (if (= newValue (@customer :ticker-number))
    (serve-customer customer)
    nil
    )
  )

;; Take a number
;; Give the customer a number and add the customer to the watch.
(defn take-a-number [customer]
  (send customer #(assoc %1 :ticket-number %2) (next-number))
  (await customer)
  (if (= @now-serving-sign (:ticket-number @customer))
    (serve-customer customer)
    (add-watch now-serving-sign customer watch-sign)
    )
  )

(defn add-to-free-servers [serverList]
  (let [add (fn [server] (dosync (alter free-servers #(conj %1 %2) server)) server)]
    (doseq [i serverList] 
        (add i)
    ))
  )

;; Make People
(defn make-people [c s]
  (let [customerList (map customer (range c))
        serverList (map #(server (+ c %)) (range s))]
    (doseq [customer customerList] (log-reference customer character-log))
    (doseq [server serverList] (log-reference server character-log))
    (add-to-free-servers serverList)
    (add-watch free-servers 2 serve-next)
    (doseq [i customerList]
      (future 
        (Thread/sleep (rand-int 1000))
        (take-a-number i)
        )
      )
    )
  )

;; Tests!!
(comment
; Fibonacci Tests
(is (= (fib 0) 1))
(is (= (fib 1) 1))
(is (= (fib 2) 2))
(is (= (fib 5) 8))

; Customers Tests
(def customer1 (customer 1))
(is (= (@customer1 :id) 1))
(is (= (@customer1 :ticket-number) nil))
(send customer1 #(assoc %1 :ticket-number %2) 2)
(is (= (@customer1 :ticket-number) 2))

; Ticket Machine Tests
(next-number)
(is (= @ticket-machine 0))
(next-number)
(next-number)
(is (= @ticket-machine 2))

; Making Pastries and testing Servers
(def server1 (server 1))
(is (= (@server1 :id) 1))
(is (= (@server1 :pastry) nil))
(send server1 #(assoc %1 :pastry %2) (fib 5))
(is (= (@server1 :pastry) 8))

; Adding a server to the free-server-list
(is (empty? @free-servers))
(dosync (alter free-servers #(conj %1 %2) server1))
(is (= 1 (count @free-servers)))

; Test add-to-free-servers -- It works, but the tests can't run it for some reason
(def server2 (server 2))
(def server3 (server 3))
(def server4 (server 4))
(def server5 (server 5))
(add-to-free-servers [server5])
(is (= 2 (count @free-servers)))
(add-to-free-servers [server2 server3 server4])
(is (= 5 (count @free-servers)))

; Tests for Take a number
(def customer2 (customer 2))
(is (= @ticket-machine 2))
(take-a-number customer2)
(is (= 3 (:ticket-number @customer2)))

)