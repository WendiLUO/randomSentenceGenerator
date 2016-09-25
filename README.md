# randomSentenceGenerator
To generate sentences grammarly correct, basic grammar rules are listed, such as :
 1. Noun phrase
 2. Verb phrase
 3. Preposition phrase
 4. Complementizer phrase
 5. Complementizer
 6. Determiner prual
 7. Determiner singular 
 8. Noun singular
 9. Noun prual
 10. Transitive verb
 11. Intransitive verb
 12. Adverb 
 13. Past tense verb 
 14. Adjective
 15. Auxiliary
 16. Preposition 

To avoid this program becomes a infinite loop, some basic rules are listed, such as:
1. one meaningful word cannot appear twice in one sentence;
2. a sentence must of limited length;
3. recursive level must less than a given constant;
4. avoid too many "and"(or the sentence can be: A and B and C and D and E and ... );
5. Adjective cannot come next to another adjective;

Example sentences generated:
(SNACK QUICKLY KICK A UNIVERSITY SUCH AS BIG USERS OR DESIRED USERS)

(A DESIRED WOMAN QUICKLY HIT THE INTERNET)

(SOME WORKERS CLEAN PROFESSIONAL WRITERS OR TABLE)

(THESE SHOPS THAT WRITE TALL TO STOCK SEE THE MEANINGLESS UNIVERSITY BUT SEE WELL)

(WHETHER THESE QUESTIONABLE EDUCATION SHOULD BE PROFESSIONAL TO SHOULD BE DESIGNED IN THIS ADORABLE
 CHAIR IN PROFESSIONAL GLASSES EAT THIS DESIRED COMPANY)

(BUT EAT THE PROFESSIONAL CHAIR THAT WHETHER SHOULD NOT BE QUESTIONABLE TO A FEW MEANINGLESS YEARS
 OF THE CHAIR HIT THESE BUSINESS IN THE WELL UNIVERSITY SUCH AS WELL SHOPS OF FEW WELL WOMEN TO
 CHAIR AND SEVERAL POPULATION BUT SHOULD BE MEANINGLESS TO WRITE WELL WORKERS OR A WOMAN OF
 MEANINGLESS USERS WITH A PROFESSIONAL CAR OR COMPANY IN TABLE WITH THE GLASSES THAT EAT
 PROFESSIONAL EDUCATION IN THE DESIRED BUSINESS IN STOCK OF A MAN MUST BE PROFESSIONAL TO SHOULD BE
 QUESTIONABLE TO DIE TO LIKE TALL USERS)
 
 How to run:
 in clisp, run (load "random_sentence_generate.lisp")
 then, run (random-sentence 'sentence) to generate random sentence.
