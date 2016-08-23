
(defparameter N1 '40)
(defparameter N2 '10)

(defparameter *grammar*
  '((sentence -> (noun-phrase verb-phrase)
		 (complementizer-phrase verb-phrase)
	)
    (noun-phrase -> (DeterminerSingular Adjective NounSingular) 
		    (DeterminerSingular Adjective NounSingular preposition-phrase)
		    (DeterminerPrual Adjective NounPrual) 
		    (DeterminerPrual Adjective NounPrual preposition-phrase)
		    (DeterminerPrual NounPrual) 
		    (DeterminerSingular NounSingular) 
		    (Adjective NounPrual)
		    (NounPrual)
		    (Adjective NounPrual preposition-phrase)
		    (DeterminerPrual NounPrual complementizer-phrase preposition-phrase)
		    (NounSingular)
		    (noun-phrase AND_S noun-phrase)
		    (DeterminerSingular Adjective NounSingular complementizer-phrase)
		    (DeterminerSingular NounSingular preposition-phrase)
		)
    (verb-phrase -> (TransitiveVerb noun-phrase)
		    (Adverb TransitiveVerb noun-phrase)
		    (Auxiliary verb-phrase) 
		    (IntransitiveVerb Preposition verb-phrase)
		    (TransitiveVerb Adjective)
		    (Auxiliary NOT_S BE_S Adjective TO_S noun-phrase)
		    (Auxiliary BE_S Adjective TO_S verb-phrase)
		    (TransitiveVerb noun-phrase preposition-phrase)
		    (Auxiliary BE_S PastTenseVerb preposition-phrase)
		)
    (preposition-phrase -> (Preposition noun-phrase))
    (complementizer-phrase -> (Complementizer sentence) 
			      (Complementizer verb-phrase)
		)
    (Complementizer -> that but whether)
    (DeterminerPrual -> these some the several A_FEW few)
    (DeterminerSingular -> the a this)
    (NounSingular -> man cat dog woman table chair car professor student company stock snack computer internet university) 
    (NounPrual -> education population business women government users writers years glasses shops workers)
    (TransitiveVerb -> hit take eat see like make kick write clean)
    (IntransitiveVerb -> arrive die sit go lie sneeze)
    (Adverb -> completely strongly quickly slowly)
    (PastTenseVerb -> finished wrote designed achieved)
    (Adjective -> meaningless adorable big questionable desired tall well professional visible beautiful)
    (Auxiliary -> will should must)
    (Preposition -> in or of to SUCH_AS with )
    ;the special and, in the phrase "xxx and xxx"
    (AND_S -> and)
    ;the special to, in the phrase "be able to, be xxx to"
    (TO_S -> to)
    ;the special be
    (BE_S -> be)
    ;the special not, in "will not be able to, should not xxx"
    (NOT_S -> not)
    ;such as
    (SUCH_AS -> (SUCH_S AS_S))
    (SUCH_S -> such)
    (AS_S -> as)
    ;a few
    (A_FEW -> (A_S FEW_S))
    (A_S -> a)
    (FEW_S -> few)
	)
  "A grammar for a few English sentences with a few words.")

(defun random-elt (list)
  (elt list
       (random (length list))))

(setq MAX_DEPTH (+ '100 N2))

(defun random-sentence (phrase)
  "wrapper for the original random-sentence, if we found the overflow marker, then we simply generate the random sentence once again until it does not overflow."
   (let ((gened-sent (random-sentence-depth phrase 0)))
      (cond
          (
             (find-marker gened-sent)
             (random-sentence phrase)
          )
          (t gened-sent)
      )
   )
)

(defun random-sentence-depth (phrase depth)
  "Generate a random sentence or phrase with depth counting"
  (cond ((listp phrase)
         (mappend #'random-sentence-depth phrase depth))
        ((rewrites phrase)
	 (cond
            ((>= depth MAX_DEPTH)
	       '(!!!)
	    )
            (t  (random-sentence-depth 
			(random-elt (rewrites phrase)) (+ '1 depth))
	    )
	 )
        )
        
	(t (list phrase))
   )
)

(defun generate-tree (phrase)
  "wrapper for the original generate-tree, if we found the overflow marker, then we simply generate tree once again until it does not overflow."
   (let ((gened-tree (generate-tree-depth phrase 0)))
      (cond
          (
             (find-marker gened-tree)
             (generate-tree phrase)
          )
          (t gened-tree)
      )
   )
)



(defun generate-tree-depth (phrase depth)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree-depth phrase (create-lst (top-length phrase) depth)))
        ((rewrites phrase)
         (cond
             ( (>= depth MAX_DEPTH)
               '(!!!)
             )
             ( t (cons phrase
                  (generate-tree-depth (random-elt (rewrites phrase)) (+ 1 depth))))
         )
        )
        (t (list phrase))))

(defun find-marker (lst)
   "find whether the list containing the overflow marker '!!!' "
   (cond 
         ((null lst) nil)
         ((atom (first lst))
              (if (equal (first lst) '!!!)
                 't
              (find-marker (rest lst)))
         )
         (t (or (find-marker (first lst))
         (find-marker (rest lst))))
    )
)

(defun search-elt (lst elt)
     "search whether the given elt appear in the lst"
     (cond ((null lst) nil)
           ((atom (first lst))
                (if (equal (first lst) elt)
                    't
                    (search-elt (rest lst) elt)))
            (t (or (search-elt (first lst) elt)
                          (search-elt (rest lst) elt)))
     )
)

(defun mappend (fn list depth)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list (create-lst (top-length list) depth))))

(defun create-lst (leng depth)
  "create a list with the number of elements which value is depth equals to leng"
   (cond ((<= leng 0) ())
	 (t (cons depth (create-lst (- leng 1) depth))))
)

(defun top-length (lst)
   "count the number of top elements in a list"
   (cond ((null lst) 0)
        (t (+ 1 (top-length (rest lst))))))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*))
)


(defun find-tree-depth (tree)
  "Return the maximum depth of a given tree"
     (cond
         ((null tree) '0)
         (
            (atom (first tree))
            (max 
               1 
               (find-tree-depth (rest tree))
            )
         )
         (t 
            (max 
              (find-tree-depth (rest tree)) 
              (+ 1 (find-tree-depth (first tree)))
            )
         )
     )
)

(defun reconstruct-sentence (tree)
   "from a given tree, reconstruct the sentence"
      (cond
         ((null tree) ())
         ((atom tree) ())
         ( 
            (and (= (top-length tree) 2) (atom (second tree)))
            (rest tree)
         )
         (t  
            (append (reconstruct-sentence (first tree)) (reconstruct-sentence (rest tree)))
        )
      )
)

(setf exclude-phrase-lst '(Complementizer DeterminerPrual DeterminerSingular Auxiliary Preposition AND_S TO_S BE_S NOT_S SUCH_S AS_S))

(defun gen-exclude-lst (phrase-lst)
     "this function generate the list of words that can appear twice in a sentence"
      (cond
          ((null phrase-lst) ())
          (t
             (append
                 (rewrites (first phrase-lst))
                 (gen-exclude-lst (rest phrase-lst))
             )
          )
      )
)

(setf exclude-lst (gen-exclude-lst exclude-phrase-lst))

(defun word-not-appear-twice (sent)
   "find whether some word in the given sentence does not appear twice"
   (cond
      ((null sent) 'T)
      ((search-elt exclude-lst (first sent))
         (word-not-appear-twice (rest sent))
      )
      (t
         (cond
            (
               (search-elt (rest sent) (first sent))
               nil
            )
            (t
                (word-not-appear-twice (rest sent))
            )
         )
      )
   )
)

(setq max-and '2)
(defun not-too-many-and (sent)
    "check whether the sentence has fewer than max-and 'and', because sometimes the sentence will be xxx and xxx and xxx and xxx and xxx ..."
    (let ((and-count (count-and sent 0)))
        (cond
             ((<= and-count max-and) 'T)
             (t nil)
        )
    )
)

(defun count-and (sent and-count)
    (cond
        ((null sent) and-count)
        (t
             (cond
                 (
                      (equal 'AND (first sent))
                      (count-and (rest sent) (+ 1 and-count))
                 )
                 (t  (count-and (rest sent) and-count)
                 )
             )
        )
    )
)

(setq aux-lst (rewrites 'Auxiliary))

(defun no-adjacent-aux (sent)
    "additional requirement, check the sentence does not have adjacent auxiliary, for example 'must will should be fine' is allowed in the grammar VP->Aux VP, but should not be allowed in real life"
   (cond
       ((null sent) 'T)
       ((search-elt aux-lst (first sent))
           (cond
               ((search-elt aux-lst (first (rest sent)))
                  nil
               )
               (t
                 (no-adjacent-aux (rest sent))
               )
           )
       )
       (t
            (no-adjacent-aux (rest sent))
       )
   ) 
)

(defun validp (tree)
    "given a generated syntax tree, return whether it is a valid sentence"
    (let ((sent (reconstruct-sentence tree)))
        (cond
            (
              (and 
                 (<= (length sent) N1) 
                 (<= (find-tree-depth tree) N2)
                 (word-not-appear-twice sent)
	         (no-adjacent-aux sent) 
                 (not-too-many-and sent)
              ) 'T
            )
            (t nil)
        )
    )
)

(defun generateValid (phrase)
    (let (
           (gen-tree (generate-tree phrase))
         )
         (cond
             ((validp gen-tree) (reconstruct-sentence gen-tree))
             (t (generateValid phrase))
         )
    )
)

;(random-sentence 'sentence)
