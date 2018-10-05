; match
; Jerred Shepherd

; predicate: a symbol
; returns: T if predicate is a pattern variable (u, v, w, x, y, z), NIL otherwise
(defun is-pattern (predicate)
        (if
                (member predicate '(u v w x y z))
                t
                nil))

; l: an S-Expression
; r: an S-Expression
; returns: T if both l and r are NULL, NIL otherwise
(defun both-null (l r)
        (and (null l) (null r)))

; l: an S-Expression
; r: an S-Expression
; returns: T if either l or r are NULL, NIL otherwise
(defun either-null (l r)
        (or (null l) (null r)))
        
; l: an S-Expression
; r: an S-Expression
; returns: T if either l or r are and atom, NIL otherwise
(defun either-atom (l r)
        (or (atom l) (atom r)))

; sub: a list of lists which contain pattern variables
; pattern: a variable to check for
; returns: T if sub contains pattern, NIL otherwise
(defun is-pattern-conflict (sub pattern value)
        (if
                (null sub)
                nil 
                (if
                        (eq (caar sub) pattern)
                        (if
                                (eq (cdr (car sub)) value)
                                (is-pattern-conflict (cdr sub) pattern value)
                                t)
                        (is-pattern-conflict (cdr sub) pattern value))))

(defun is-pattern-used (sub pattern value)
        (if
                (null sub)
                nil 
                (if
                        (eq (caar sub) pattern)
                        t      
                        (is-pattern-used (cdr sub) pattern value))))

; term: a list to match
; pattern: a pattern list
; sub: substituions that can be applied to pattern to make it identical to the term
; returns: (NIL) if term and pattern already match, NIL if term and pattern cannot match, a list of substitutions that can be applied to the pattern to make it identical to the substitution
(defun match-recur (term pattern sub)
        (if
                (both-null term pattern)
                sub
                (if
                        (either-null term pattern)
                        nil
                        (if
                                (either-atom term pattern)
                                nil
                                (if
                                        (eq (car term) (car pattern))
                                        (match-recur (cdr term) (cdr pattern) sub)
                                        (if 
                                                        (is-pattern (car pattern))
                                                        (if
                                                                (is-pattern-conflict sub (car pattern) (car term))
                                                                nil
                                                                (if
                                                                        (is-pattern-used sub (car pattern) (car term))
                                                                        (match-recur (cdr term) (cdr pattern) sub)
                                                                        (if 
                                                                                (null (car sub))
                                                                                (match-recur (cdr term) (cdr pattern) (list (cons (car pattern) (car term))))
                                                                                (match-recur (cdr term) (cdr pattern) (append sub (list (cons (car pattern) (car term))))))))
                                                        (let    
                                                                (
                                                                        (matched-list (match-recur (car term) (car pattern) sub)))
                                                                (if 
                                                                        (null matched-list)
                                                                        nil
                                                                        (match-recur (cdr term) (cdr pattern) matched-list)))))))))

(defun match (term pattern)
        (match-recur term pattern '(nil)))

; Test cases
; (and
;         (and
;                 (equal (is-pattern-conflict '((X . A)) 'X 'A) nil))
;         (and 
;                 (equal (is-pattern 'x) t)
;                 (equal (is-pattern 'a) nil))
;         (and
;                 (equal (both-null 'nil 'nil) t)
;                 (equal (both-null 'a 'nil) nil)
;                 (equal (both-null 'a 'b) nil))
;         (and
;                 (equal (either-null 'nil 'nil) t)
;                 (equal (either-null 'a 'nil) t)
;                 (equal (either-null 'a 'b) nil))
;         (and
;                 (equal (match 'a 'b) nil)
;                 (equal (match '(+ a b) '(+ y z)) '((y . a) (z . b)))
;                 (equal (match '(+ a b) '(+ a x)) '((x . b)))
;                 (equal (match '(* a b) '(* a b)) '(nil))
;                 (equal (match '(f a b) '(f a a)) nil)
;                 (equal (match '(+ a b) '(- a b)) nil)
;                 (equal (match '(+ (- b c) a) '(+ x y)) '((x - b c) (y . a)))
;                 (equal (match '(loves a b) '(loves x x)) nil)
;                 (equal (match '(loves joe pie) '(loves x pie)) '((x . joe)))
;                 (equal (match '(+ a (+ b a)) '(+ x (+ y x))) '((x . a) (y . b)))))

