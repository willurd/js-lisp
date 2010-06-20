;; ================================================================================
;; BOOK CODE
;; ================================================================================

;; This is the original new-account declaration. js-lisp doesn't have
;; default values for optional parameters yet, so it had to be modified.
;; Type-checking for the parameters was added just for fun.
;;(defun new-account (name &opt (balance 0.00) (interest-rate .06))

(setq *default-balance* 0.00)
(setq *default-interest-rate* 0.06)

(defun new-account (name::string &opt balance::number
	                                  interest-rate::number)
  "Create a new account that knows the following messages:"
  (||= balance *default-balance*)
  (setq interest-rate (if (is-undefined interest-rate)
                          *default-interest-rate*
                        interest-rate))
  (lambda (message)
    (case message
      (withdraw (lambda (amt)
                  (if (<= amt balance)
                      (dec balance amt)
                    'insufficient-funds)))
      (deposit  (lambda (amt) (inc balance amt)))
      (name     (lambda () name))
      (balance  (lambda () balance))
      (interest-rate (lambda () interest-rate))
      (interest (lambda ()
                  (inc balance (* interest-rate balance)))))))

(defun get-method (object message)
  "Return the method that implements message for this object."
  ;; The call to funcall was removed because it behaves
  ;; differently in js-lisp.
  (object message))

(defun send (object message & args)
  "Get the function to implement the message,
  and apply the function to the args."
  (apply (get-method object message) args))

;; ================================================================================
;; TESTS
;; ================================================================================

(test "(new-account)"
  :testNoArguments (lambda ()
    (this.assertRaises ArgumentError #'new-account nil))
  :testOneArgument (lambda ()
    (this.assertNotRaises ArgumentError #'new-account nil "name"))
  :testTwoArguments (lambda ()
    (this.assertNotRaises ArgumentError #'new-account nil "name" 0))
  :testThreeArguments (lambda ()
    (this.assertNotRaises ArgumentError #'new-account nil "name" 0 0))
  :testDefaultBalance (lambda ()
    (let ((a (new-account "name")))
      (this.assertEqual (send a 'balance) *default-balance*)))
  :testReturnType (lambda ()
    (this.assertTrue (is-function (new-account "name")))))

(test "(get-method)"
  :testNoArguments (lambda ()
    (this.assertRaises ArgumentError #'get-method nil))
  :testOneArgument (lambda ()
    (this.assertRaises ArgumentError #'get-method nil (lambda)))
  :testTwoArguments (lambda ()
    (this.assertNotRaises ArgumentError #'get-method nil (lambda) 'test))
  :testGetAccountMethod (lambda ()
    (let ((a (new-account "name")))
      (this.assertTrue (is-function (get-method a 'balance))))))

(test "(send)"
  :testNoArguments (lambda ()
    (this.assertRaises ArgumentError #'send nil))
  :testOneArgument (lambda ()
    (let ((a (new-account "name")))
      (this.assertRaises ArgumentError #'send nil a)))
  :testTwoArguments (lambda ()
    (let ((a (new-account "name")))
      (this.assertNotRaises ArgumentError #'send nil a 'balance)))
  :testAccountBalance (lambda ()
    (let ((a (new-account "name" 100.02)))
      (this.assertEqual (send a 'balance) 100.02))))

(let ((acct (new-account "J. Random Customer" 1000.00)))
  (test "PAIP example code"
    :testWithdraw (lambda ()
      (send acct 'withdraw 500.00)
      (this.assertEqual (send acct 'balance) 500))
    :testDeposit (lambda ()
      (send acct 'deposit 123.45)
      (this.assertEqual (send acct 'balance) 623.45))
    :testName (lambda ()
      (this.assertEqual (send acct 'name) "J. Random Customer"))))
