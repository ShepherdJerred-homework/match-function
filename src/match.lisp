(defun is-pattern (predicate)
        (if
                (member predicate '(u v w x y z))
                t
                nil))

(defun both-null (l r)
        (and (null l) (null r)))

(defun either-null (l r)
        (or (null l) (null r)))
        
(defun either-atom (l r)
        (or (atom l) (atom r)))

(defun match-recur (term pattern sub)
        (if
                (both-null term pattern)
                sub
                (if
                        (either-null term pattern)
                        nil
                        (if
                                (eq (car term) (car pattern))
                                (match-recur (cdr term) (cdr pattern) sub)
                                (if 
                                        (is-pattern (car pattern))
                                        (match-recur (cdr term) (cdr pattern) (cons sub (cons (cons (car pattern) (car term)) nil)))
                                        (if
                                                (either-atom (car pattern) (car term))
                                                nil
                                                ()))))))
 
(defun match (term pattern)
        (match-recur term pattern '(nil)))

(and
        (and 
                (eq (is-pattern 'x) t)
                (eq (is-pattern 'a) nil))
        (and
                (eq (both-null 'nil 'nil) t)
                (eq (both-null 'a 'nil) nil)
                (eq (both-null 'a 'b) nil))
        (and
                (eq (either-null 'nil 'nil) t)
                (eq (either-null 'a 'nil) t)
                (eq (either-null 'a 'b) nil))
        (and
                (eq (match '(+ a b) '(+ a x)) ((x . b)))
                (eq (match '(* a b) '(* a b)) (nil))
                (eq (match '(f a b) '(f a a)) nil)
                (eq (match '(+ a b) '(- a b)) nil)
                (eq (match '(+ (- b c) a) '(+ x y)) ((x - b c) (y . a)))
                (eq (match '(loves a b) '(loves x x)) nil)
                (eq (match '(loves joe pie) '(loves x pie)) ((x . joe)))
                (eq (match '(+ a (+ b a)) '(+ x (+ y x))) ((x . a) (y . b)))))

