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
  (if (not (empty? @reference))
	  (do (swap! now-serving-sign inc)
        (println "Now serving:" @now-serving-sign))
    nil
    )
  )

;; Making Pastries
(defn make-pastries [server]
  (let [pastry-number (rand-int 50)]
  (print "computing the fib of" pastry-number)
  (send server #(assoc %1 :pastry %2) (fib pastry-number))
  )
)

;; Serve the Customer
(defn finished-serving [server]
  (dosync
    (alter free-servers #(conj %) server)
    (println "Server number:" (@server :id) "is now free")
    (send server #(assoc %1 :pastry %2) nil)
    )
  )

(defn start-serving [server customer]
  (println "Server " (:id @server) " is helping Customer " (:id @customer) " by ")
  (future (make-pastries server))
  (let [pastry (:pastry @server)]
    (send customer #(assoc %1 :result %2) pastry)
    )
  )

(defn get-server []
  (let [the-server (first @free-servers)]
    (disj @free-servers the-server)
    (println "Server " (:id @the-server) " is now busy")
    the-server
    )
  )

(defn serve-customer [customer]
  (let [the-server (get-server)]
	  (start-serving the-server customer)
	  (finished-serving the-server)
   )
  )

;; Take a number
;; Give the customer a number and add the customer to the watch.
(defn take-a-number [customer]
  (next-number)
  (send customer #(assoc %1 :ticket-number %2) @ticket-machine)
  (add-watch now-serving-sign customer watch-sign)
  (println "Customer " (:id @customer) "took number: " @ticket-machine)
  )

(defn watch-sign [customer reference oldValue newValue]
  (if (= newValue (@customer :ticker-number))
    (serve-customer customer)
    nil
    )
  )

;; Make People
(defn make-people [c s]
  (let [customerList (transient [])]
    (for [i (range c)] (conj! customerList (customer i)))
    (for [i (range s)] (alter free-servers #(conj %1 (server (+ i c)))))
    ;;(alter free-servers (fn [] serverList))
    (add-watch free-servers 0 serve-next)
   ;; (println (empty? (persistent! customerList)))
    (map (fn [customer] 
           (do (Thread/sleep (rand-int 1000))
               (take-a-number customer)
           )
         ) (persistent! customerList)
     )
  )
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