(defun is-pattern (predicate)
        (if
                (member predicate '(u v w x y z))
                t
                nil))

(defun both-null (l r)
        (and (null l) (null r)))

(defun either-null (l r)
        (or (null l) (null r)))

(defun match-one (term pattern)
        (if
                (eq term pattern)
                '(nil)
                (if
                        (is-pattern pattern)
                        (cons pattern term)
                        nil)))

(defun format-matches (matches)
        (let 
                (
                        (c-match (car matches)) 
                        (n-match (cdr matches)))
                (if
                        (null n-match)
                        nil
                  if (null c-match)
                  (cons (format-matches n-match)))))
                        
                       
                  
    

   

(defun match (term pattern)
        (let
                (
                        (c-term (car term))
                        (c-pattern (car pattern))
                        (n-term (cdr term))
                        (n-pattern (cdr pattern)))
                (if
                        (both-null n-term n-pattern)
                        (match-one c-term c-pattern) 
                        (cons (match-one c-term c-pattern) (match n-term n-pattern)))))

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
    (eq (match '(loves joe pie) '(loves x pie)) nil)
    (eq (match '(+ a (+ b a)) '(+ x (+ y x))) ((x . a) (y . b)))))

