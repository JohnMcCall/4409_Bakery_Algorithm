(ns Bakery)
(use 'clojure.test)

;; Fibonacci Calculator
(defn fib [n]
  (if (or (= n 0) (= n 1))
    1
    (+ (fib (- n 1)) (fib (- n 2)))
    )
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

;; The Ticket Machine
(def ticket-machine (atom -1))
 
(defn next-number []
  (swap! ticket-machine inc))

;; The "Now Serving" sign
(def now-serving-sign (atom 0))



(defn serve-next [id reference oldValue newValue] 
  (println oldValue newValue)
  (if (< (count oldValue) (count newValue))
    (if (empty? oldValue)
      (do (swap! now-serving-sign inc)
          (println "Now serving:" @now-serving-sign))
      nil
      )
     (if (not (empty? newValue))
       (do (swap! now-serving-sign inc)
          (println "Now serving:" @now-serving-sign))
       nil
       )
    )
  )

;; Making Pastries
(defn make-pastries [server]
  (let [pastry-number (rand-int 5)]
  ;;(println "computing the fib of" pastry-number)
  (send server #(assoc %1 :pastry %2) (fib pastry-number))
  )
)

;; Serve the Customer
(defn finished-serving [server]
  (dosync
    (alter free-servers #(conj %1 %2) server)
    (println "Server number:" (:id @server) "is now free")
    (send server #(assoc %1 :pastry %2) nil)
    )
  )

(defn start-serving [server customer]
  (println "Server" (:id @server) "is helping Customer" (:id @customer) "by ")
  (def stuff (future (make-pastries server)))
  (let [pastry (:pastry @server)]
    (send customer #(assoc %1 :result %2) pastry)
    )
  )

(defn get-server []
  (let [the-server (first @free-servers)]
    (dosync
      (alter free-servers #(disj %1 %2) server)
      (println "Server" (:id @the-server) "is now busy")
      the-server
      )
    )
  )

(defn serve-customer [customer]
  (let [the-server (get-server)]
	  (start-serving the-server customer)
	  (finished-serving the-server)
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
  (next-number)
  (send customer #(assoc %1 :ticket-number %2) @ticket-machine)
  (println "Customer" (:id @customer) "took number:" @ticket-machine)
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
  (let [customerList (flatten (for [i (range c)] (conj [] (customer i))))
        serverList (flatten (for [i (range s)] (conj [] (server (+ i c)))))]
    (add-to-free-servers serverList)
    (add-watch free-servers 0 serve-next)
    (doseq [i customerList]
       (Thread/sleep (rand-int 1000))
       (take-a-number i)
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