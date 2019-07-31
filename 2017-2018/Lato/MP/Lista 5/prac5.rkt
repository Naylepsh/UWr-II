#lang racket

;; pomocnicza funkcja dla list tagowanych o określonej długości
(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))

;; reprezentacja danych wejściowych (z ćwiczeń)
(define (var? x)
  (symbol? x))

(define (var x)
  x)

(define (var-name x)
  x)

;; przydatne predykaty na zmiennych
(define (var<? x y)
  (symbol<? x y))

(define (var=? x y)
  (eq? x y))

(define (literal? x)
  (and (tagged-tuple? 'literal 3 x)
       (boolean? (cadr x))
       (var? (caddr x))))

(define (literal pol x)
  (list 'literal pol x))

(define (literal-pol x)
  (cadr x))

(define (literal-var x)
  (caddr x))

(define (clause? x)
  (and (tagged-list? 'clause x)
       (andmap literal? (cdr x))))

(define (clause . lits)
  (cons 'clause lits))

(define (clause-lits c)
  (cdr c))

(define (cnf? x)
  (and (tagged-list? 'cnf x)
       (andmap clause? (cdr x))))

(define (cnf . cs)
    (cons 'cnf cs))

(define (cnf-clauses x)
  (cdr x))

;; oblicza wartość formuły w CNF z częściowym wartościowaniem. jeśli zmienna nie jest
;; zwartościowana, literał jest uznawany za fałszywy.
(define (valuate-partial val form)
  (define (val-lit l)
    (displayln (assoc (literal-var l) val))
    (displayln (literal-var l))
    (displayln val)
    (let ((r (assoc (literal-var l) val)))
      (cond
       [(not r)  false]
       [(cadr r) (literal-pol l)]
       [else     (not (literal-pol l))])))
  (define (val-clause c)
    (ormap val-lit (clause-lits c)))
  (andmap val-clause (cnf-clauses form)))

;; reprezentacja dowodów sprzeczności

(define (axiom? p)
  (tagged-tuple? 'axiom 2 p))

(define (proof-axiom c)
  (list 'axiom c))

(define (axiom-clause p)
  (cadr p))

(define (res? p)
  (tagged-tuple? 'resolve 4 p))

(define (proof-res x pp pn)
  (list 'resolve x pp pn))

(define (res-var p)
  (cadr p))

(define (res-proof-pos p)
  (caddr p))

(define (res-proof-neg p)
  (cadddr p))

;; sprawdza strukturę, ale nie poprawność dowodu
(define (proof? p)
  (or (and (axiom? p)
           (clause? (axiom-clause p)))
      (and (res? p)
           (var? (res-var p))
           (proof? (res-proof-pos p))
           (proof? (res-proof-neg p)))))

;; procedura sprawdzająca poprawność dowodu
(define (check-proof pf form)
  (define (run-axiom c)
    (displayln (list 'checking 'axiom c))
    (and (member c (cnf-clauses form))
         (clause-lits c)))
  (define (run-res x cpos cneg)
    (displayln (list 'checking 'resolution 'of x 'for cpos 'and cneg))
    (and (findf (lambda (l) (and (literal-pol l)
                                 (eq? x (literal-var l))))
                cpos)
         (findf (lambda (l) (and (not (literal-pol l))
                                 (eq? x (literal-var l))))
                cneg)
         (append (remove* (list (literal true x))  cpos)
                 (remove* (list (literal false x)) cneg))))
  (define (run-proof pf)
    (cond
     [(axiom? pf) (run-axiom (axiom-clause pf))]
     [(res? pf)   (run-res (res-var pf)
                           (run-proof (res-proof-pos pf))
                           (run-proof (res-proof-neg pf)))]
     [else        false]))
  (null? (run-proof pf)))


;; reprezentacja wewnętrzna

;; sprawdza posortowanie w porządku ściśle rosnącym, bez duplikatów
(define (sorted? vs)
  (or (null? vs)
      (null? (cdr vs))
      (and (var<? (car vs) (cadr vs))
           (sorted? (cdr vs)))))

(define (sorted-varlist? x)
  (and (list? x)
       (andmap (var? x))
       (sorted? x)))

;; klauzulę reprezentujemy jako parę list — osobno wystąpienia pozytywne i negatywne. Dodatkowo
;; pamiętamy wyprowadzenie tej klauzuli (dowód) i jej rozmiar.
(define (res-clause? x)
  (and (tagged-tuple? 'res-int 5 x)
       (sorted-varlist? (second x))
       (sorted-varlist? (third x))
       (= (fourth x) (+ (length (second x)) (length (third x))))
       (proof? (fifth x))))

(define (res-clause pos neg proof)
  (list 'res-int pos neg (+ (length pos) (length neg)) proof))

(define (res-clause-pos c)
  (second c))

(define (res-clause-neg c)
  (third c))

(define (res-clause-size c)
  (fourth c))

(define (res-clause-proof c)
  (fifth c))

;; przedstawia klauzulę jako parę list zmiennych występujących odpowiednio pozytywnie i negatywnie
(define (print-res-clause c)
  (list (res-clause-pos c) (res-clause-neg c)))

;; sprawdzanie klauzuli sprzecznej
(define (clause-false? c)
  (and (null? (res-clause-pos c))
       (null? (res-clause-neg c))))

;; pomocnicze procedury: scalanie i usuwanie duplikatów z list posortowanych
(define (merge-vars xs ys)
  (cond [(null? xs) ys]
        [(null? ys) xs]
        [(var<? (car xs) (car ys))
         (cons (car xs) (merge-vars (cdr xs) ys))]
        [(var<? (car ys) (car xs))
         (cons (car ys) (merge-vars xs (cdr ys)))]
        [else (cons (car xs) (merge-vars (cdr xs) (cdr ys)))]))

(define (remove-duplicates-vars xs)
  (cond [(null? xs) xs]
        [(null? (cdr xs)) xs]
        [(var=? (car xs) (cadr xs)) (remove-duplicates-vars (cdr xs))]
        [else (cons (car xs) (remove-duplicates-vars (cdr xs)))]))

(define (rev-append xs ys)
  (if (null? xs) ys
      (rev-append (cdr xs) (cons (car xs) ys))))

;; TODO: miejsce na uzupełnienie własnych funkcji pomocniczych
(define (contains? c var)
  ;; sprawdza czy lista c zawiera var
  (cond
    [(null? c) #f]
    [(var=? var (car c)) #t]
    [(var<? var (car c)) #f]
    [else (contains? (cdr c) var)]))

(define (find-common-var pos neg)
  ;; zwraca wspolna zmienna lub #f w przypadku, gdy jej nie ma
  (cond
    [(null? neg) #f]
    [(contains? pos (car neg)) (car neg)]
    [else (find-common-var pos (cdr neg))]))

(define (make-resolve c1 c2 var-to-skip)
  ;; UWAGA! Kolejnosc c1 c2 ma znaczenie
  ;; c1 to ta klauzula, w ktorej zmienna wystepowala pozytywnie
  ;; c2 to ta, w ktorej wystepowala negatywnie
  ;; var-to-skip to zmienna na ktorej 'robimy' rezolucje
  (define new-pos (remove var-to-skip (merge-vars (res-clause-pos c1) (res-clause-pos c2))))
  (define new-neg (remove var-to-skip (merge-vars (res-clause-neg c1) (res-clause-neg c2))))
  ;; tworzymy rezolwente
  (res-clause new-pos new-neg (proof-res var-to-skip (res-clause-proof c1) (res-clause-proof c2))))

(define (clause-trivial? c)
  ;; Jeżeli istnieje taka zmienna, że wystepuje ona pozytywnie i negatywnie, to
  ;; klauzula jest trywialna
  (if (find-common-var (res-clause-pos c) (res-clause-neg c))
      #t
      #f))

(define (resolve c1 c2)
  (define common-var1 (find-common-var (res-clause-pos c1) (res-clause-neg c2)))
  (define common-var2 (find-common-var (res-clause-pos c2) (res-clause-neg c1)))
  (cond
    ;;stworz rezolwente bez trivial-var1/2
    [common-var1 (make-resolve c1 c2 common-var1)] 
    [common-var2 (make-resolve c2 c1 common-var2)]
    [else #f]))

(define (resolve-single-prove s-clause checked pending)
  ;; TODO: zaimplementuj!
  ;; Poniższa implementacja działa w ten sam sposób co dla większych klauzul — łatwo ją poprawić!
  (let* ((resolvents   (filter-map (lambda (c) (resolve c s-clause))
                                     checked))
         (sorted-rs    (sort resolvents < #:key res-clause-size)))
    (subsume-add-prove (cons s-clause checked) pending sorted-rs)))

;; wstawianie klauzuli w posortowaną względem rozmiaru listę klauzul
(define (insert nc ncs)
  (cond
   [(null? ncs)                     (list nc)]
   [(< (res-clause-size nc)
       (res-clause-size (car ncs))) (cons nc ncs)]
   [else                            (cons (car ncs) (insert nc (cdr ncs)))]))

;; sortowanie klauzul względem rozmiaru (funkcja biblioteczna sort)
(define (sort-clauses cs)
  (sort cs < #:key res-clause-size))

;; główna procedura szukająca dowodu sprzeczności
;; zakładamy że w checked i pending nigdy nie ma klauzuli sprzecznej
(define (resolve-prove checked pending)
  (cond
   ;; jeśli lista pending jest pusta, to checked jest zamknięta na rezolucję czyli spełnialna
   [(null? pending) (generate-valuation (sort-clauses checked))]
   ;; jeśli klauzula ma jeden literał, to możemy traktować łatwo i efektywnie ją przetworzyć
   [(= 1 (res-clause-size (car pending)))
    (resolve-single-prove (car pending) checked (cdr pending))]
   ;; w przeciwnym wypadku wykonujemy rezolucję z wszystkimi klauzulami już sprawdzonymi, a
   ;; następnie dodajemy otrzymane klauzule do zbioru i kontynuujemy obliczenia
   [else
    (let* ((next-clause  (car pending))
           (rest-pending (cdr pending))
           (resolvents   (filter-map (lambda (c) (resolve c next-clause))
                                     checked))
           (sorted-rs    (sort-clauses resolvents)))
      (subsume-add-prove (cons next-clause checked) rest-pending sorted-rs))]))

;; procedura upraszczająca stan obliczeń biorąc pod uwagę świeżo wygenerowane klauzule i
;; kontynuująca obliczenia. Do uzupełnienia.
(define (subsume-add-prove checked pending new)
  (cond
   [(null? new)                 (resolve-prove checked pending)]
   ;; jeśli klauzula do przetworzenia jest sprzeczna to jej wyprowadzenie jest dowodem sprzeczności
   ;; początkowej formuły
   [(clause-false? (car new))   (list 'unsat (res-clause-proof (car new)))]
   ;; jeśli klauzula jest trywialna to nie ma potrzeby jej przetwarzać
   [(clause-trivial? (car new)) (subsume-add-prove checked pending (cdr new))]
   [else
    ;; TODO: zaimplementuj!
    ;; Poniższa implementacja nie sprawdza czy nowa klauzula nie jest lepsza (bądź gorsza) od już
    ;; rozpatrzonych; popraw to!
    (subsume-add-prove checked (insert (car new) pending) (cdr new))
    ]))

(define (generate-valuation resolved)
  (define (get-first-val-of c)
    ;; tworzy liste (zmienna wartosc) na podstawie przynaleznosci zmiennej
    (if (null? (res-clause-pos c))
        (list (car (res-clause-neg c)) #f)
        (list (car (res-clause-pos c)) #t)))
  ;; pomocnicze selektory
  (define (val-var val)
    (first val))
  (define (val-value val)
    (second val))
   
  (define (remove-empty cs)
    ;; pomocnicza prodecura usuwajaca puste klauzule
    (cond
      [(null? cs) null]
      [(or (null? (car cs))
           (and (null? (res-clause-pos (car cs)))
                (null? (res-clause-neg (car cs)))))
       (remove-empty (cdr cs))]
      [else (cons (car cs) (remove-empty (cdr cs)))]))
  (define (iter cs val-list)
    ;; pobieramy pierwsza zmienna z pierwszej klauzuli
    ;; na podstawie jej wartosciowania decydujemy jak uproscic nastepne klauzule
    ;; nastepnie przechodzimy do nastepnej, juz uproszczonej klauzuli i ponawiamy schemat
    ;; dopoki zostaly jeszcze jakies klauzule
    (cond
      [(null? cs) val-list]
      [(null? (first cs)) val-list]
      [else
       (define val (get-first-val-of (first cs)))
       (define (simplify c)
         (cond
           [(val-value val)
            ;; jesli (<zmienna> #t) znajduje sie w dodatniej postaci w danej klauzuli,
            ;; to sigma(klauzula) = #t, zatem wyrzucamy ta klauzule
            ;; jesli (<zmienna> #t) znajduje sie w ujemnej postaci w danej klauzuli,
            ;; to usuwamy ta zmienna z ujemnej czesci, bo i tak nic tam nie wnosi
            (if (contains? (res-clause-pos c) (val-var val))
                null
                (res-clause (res-clause-pos c) (remove (val-var val) (res-clause-neg c))
                            (res-clause-proof c)))]
           [else
            ;; jesli (<zmienna> #f) znajduje sie ujemnej postaci w danej klauzuli,
            ;; to sigma(klauzula) = #t, zatem wyrzucamy ta klauzule [...]
            (if (contains? (res-clause-neg c) (val-var val))
                null
                (res-clause (remove (val-var val) (res-clause-pos c)) (res-clause-neg c)
                            (res-clause-proof c)))]))
       ;; wyrzucamy puste klauzule i iterujemy dalej
       ;(iter (remove null (map simplify (cdr cs))) (cons val val-list))]))
       (iter (remove-empty (map simplify (cdr cs))) (cons val val-list))]))
  (cons 'sat (iter resolved null)))


;; procedura przetwarzające wejściowy CNF na wewnętrzną reprezentację klauzul
(define (form->clauses f)
  (define (conv-clause c)
    (define (aux ls pos neg)
      (cond
       [(null? ls)
        (res-clause (remove-duplicates-vars (sort pos var<?))
                    (remove-duplicates-vars (sort neg var<?))
                    (proof-axiom c))]
       [(literal-pol (car ls))
        (aux (cdr ls)
             (cons (literal-var (car ls)) pos)
             neg)]
       [else
        (aux (cdr ls)
             pos
             (cons (literal-var (car ls)) neg))]))
    (aux (clause-lits c) null null))
  (map conv-clause (cnf-clauses f)))

(define (prove form)
  (let* ((clauses (form->clauses form)))
    (subsume-add-prove '() '() clauses)))

;; procedura testująca: próbuje dowieść sprzeczność formuły i sprawdza czy wygenerowany
;; dowód/waluacja są poprawne. Uwaga: żeby działała dla formuł spełnialnych trzeba umieć wygenerować
;; poprawną waluację.
(define (prove-and-check form)
  (let* ((res (prove form))
         (sat (car res))
         (pf-val (cadr res)))
    (if (eq? sat 'sat)
        (valuate-partial pf-val form)
        (check-proof pf-val form))))

;;; TODO: poniżej wpisz swoje testy
;; Testy do cw. 1

(define  p (literal #t 'p))
(define -p (literal #f 'p))
(define  q (literal #t 'q))
(define -q (literal #f 'q))
(define  r (literal #t 'r))
(define -r (literal #f 'r))

(define pV-q (clause p -q))
(define qV-r (clause q -r))
(define form1 (cnf pV-q qV-r))
(define c1 (car (form->clauses form1)))
(define c2 (cadr (form->clauses form1)))
(displayln "rezolwenta (p V -q) (q V -r):")
(resolve c1 c2)

(define form2 (cnf (clause p) (clause -p)))
(define c3 (car (form->clauses form2)))
(define c4 (cadr (form->clauses form2)))
(displayln "rezolwenta (p) (-p):")
(resolve c3 c4)

(define -pVqV-r (clause -p q -r))
(define form3 (cnf pV-q -pVqV-r))
(define c5 (car (form->clauses form3)))
(define c6 (cadr (form->clauses form3)))
(displayln "rezolwenta (p V -q) (-p V q V -r):")
(resolve c5 c6)

;(contains? (res-clause-pos c5) 'p) ;powinno byc #t
;(contains? (res-clause-pos c5) 'q) ;powinno byc #f
;(contains? (res-clause-neg c5) 'q) ;powinno byc #t

(clause-trivial? (resolve c5 c6)) ;(q V -q V -r) -> powinno dac #t
(clause-trivial? (resolve c1 c2)) ;(p V r) -> powinno dac #f

;; Testy do cw. 2
(generate-valuation (list c6)) ;klauzula (q V -p V -r)
(generate-valuation (list c3 c6)) ;klauzule: (p) (q V -p V -r)
(generate-valuation (list c1 c2)) ;klauzule: (p V -q) (q V r)
(generate-valuation (list c2 c4)) ;klauzule: (q V r) (p)
(generate-valuation (form->clauses (cnf (clause p) (clause q) (clause r)))) ;klauzule: (p) (q) (r)
(generate-valuation (list c1 c4)) ;klauzule (p V -q) (p)
(define pVqVr (clause p q r))
(define -rV-qV-p (clause -r -q -p))
(define -qVr (clause -q r))
(define -rVp (clause -r p))
(generate-valuation (form->clauses (cnf pVqVr -rV-qV-p -qVr -rVp)))
