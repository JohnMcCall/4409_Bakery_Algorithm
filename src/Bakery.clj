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


;; Make People
(defn make-people [c s]
  (let [customerList [] serverList []]
    (for [i (range c)] (conj customerList (customer i)))
    (for [i (range s)] (conj serverList (server (+ i c))))
  )
)

;; Free servers
(def free-servers (ref {}))

;; The Ticket Machine
(def ticket-machine (atom -1))
 
(defn next-number []
  (swap! ticket-machine inc))

;; The "Now Serving" sign
(def now-serving-sign (ref 0))

(defn finished-serving [server]
  (dosync
    (alter now-serving-sign inc)
    (println "Now serving:" @now-serving-sign)
    (alter free-servers #(conj %) server)
    (println "Server number:" (@server :id) "is now free")
    (send server #(assoc %1 :pastry %2) nil)
    )
  )

;; Take a number
;; Give the customer a number and add the customer to the watch.
(defn take-a-number [customer]
  
  )

(defn watch []
  
  )

;; Making Pastries
(defn make-pastries [server]
  (send server #(assoc %1 :pastry %2) (fib (rand-int 50)))
  )


;; Tests!!

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