;;;; author-words.lisp

(in-package :author-words)

(defparameter *data-directory*
  (merge-pathnames "data/" (asdf:system-source-directory :author-words)))

(defparameter *frequency-file-path*
  (merge-pathnames "word-frequencies.txt" *data-directory*))

;; Hash table storing baseline English word counts from Peter Norvig / Google Trillion Word corpus
(defparameter *general-frequencies* (make-hash-table :test #'equal))
(defparameter *total-corpus-words* 556115727130)
(defparameter *frequencies-loaded-p* nil)

;; Dictionary mapping lemmatized word/stem -> list of example sentences
(defparameter *word-sentences* (make-hash-table :test #'equal))

(defparameter *stop-words*
  (let ((table (make-hash-table :test #'equal))
        (words '("a" "about" "above" "after" "again" "against" "all" "am" "an" "and"
                 "any" "are" "aren" "as" "at" "be" "because" "been" "before" "being"
                 "below" "between" "both" "but" "by" "can" "cannot" "could" "couldn"
                 "did" "didn" "do" "does" "doesn" "doing" "don" "down" "during" "each"
                 "few" "for" "from" "further" "had" "hadn" "has" "hasn" "have" "haven"
                 "having" "he" "her" "here" "hers" "herself" "him" "himself" "his"
                 "how" "i" "if" "in" "into" "is" "isn" "it" "its" "itself" "me" "more"
                 "most" "my" "myself" "no" "nor" "not" "of" "off" "on" "once" "only"
                 "or" "other" "ought" "our" "ours" "ourselves" "out" "over" "own"
                 "same" "she" "should" "shouldn" "so" "some" "such" "than" "that"
                 "the" "their" "theirs" "them" "themselves" "then" "there" "these"
                 "they" "this" "those" "through" "to" "too" "under" "until" "up"
                 "very" "was" "wasn" "we" "were" "weren" "what" "when" "where" "which"
                 "while" "who" "whom" "why" "with" "won" "would" "wouldn" "you"
                 "your" "yours" "yourself" "yourselves" "shall" "thou" "thee" "thy"
                 "said" "one" "will" "now" "must" "might" "like" "may" "upon" "also"
                 "well" "good" "great" "come" "came" "went" "see" "saw" "seen"
                 "take" "took" "taken" "make" "made" "know" "knew" "known" "think"
                 "thought" "tell" "told" "say" "says" "saying" "get" "got")))
    (dolist (w words table)
      (setf (gethash w table) t))))

(defun load-general-frequencies (&optional (file *frequency-file-path*))
  "Load background general English word frequencies into memory."
  (let ((path (uiop:ensure-pathname file :want-non-wild t)))
    (when (probe-file path)
      (clrhash *general-frequencies*)
      (with-open-file (stream path :direction :input :external-format :utf-8)
        (loop for line = (read-line stream nil nil)
              while line do
                (let ((tab-pos (position #\Tab line)))
                  (when tab-pos
                    (let* ((word (string-downcase (subseq line 0 tab-pos)))
                           (count-str (subseq line (1+ tab-pos)))
                           (count (parse-integer count-str :junk-allowed t)))
                      (when (and count (plusp count))
                        (setf (gethash word *general-frequencies*) count)))))))
      (setf *frequencies-loaded-p* t)
      (hash-table-count *general-frequencies*))))

(defun ensure-frequencies-loaded ()
  (unless *frequencies-loaded-p*
    (load-general-frequencies)))

(defun in-reference-corpus-p (word)
  "True if WORD exists in the background reference corpus."
  (ensure-frequencies-loaded)
  (plusp (gethash (string-downcase word) *general-frequencies* 0)))

(defun clean-whitespace (str)
  "Collapse all whitespace runs into single spaces and trim boundaries."
  (let ((trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) str)))
    (with-output-to-string (out)
      (let ((in-space nil))
        (loop for c across trimmed do
          (if (find c '(#\Space #\Newline #\Tab #\Return))
              (unless in-space
                (write-char #\Space out)
                (setf in-space t))
              (progn
                (write-char c out)
                (setf in-space nil))))))))

(defun split-sentences (text)
  "Splits TEXT into clean, trimmed sentence strings."
  (let ((sentences nil)
        (len (length text))
        (start 0)
        (i 0))
    (loop while (< i len) do
      (let ((c (char text i)))
        (cond
          ;; Sentence terminal punctuation: . ? !
          ((and (find c '(#\. #\? #\!))
                (or (= (1+ i) len)
                    (find (char text (1+ i)) (list #\Space #\Newline #\Tab #\" (code-char 39) #\” #\’))))
           (let ((end (1+ i)))
             ;; Include trailing quotation marks or closing brackets
             (loop while (and (< end len) (find (char text end) (list #\" (code-char 39) #\” #\’ #\) #\])))
                   do (incf end))
             (let ((s (clean-whitespace (subseq text start end))))
               (when (plusp (length s))
                 (push s sentences)))
             (setf i end)
             ;; Skip whitespace before next sentence
             (loop while (and (< i len) (find (char text i) '(#\Space #\Newline #\Tab #\Return)))
                   do (incf i))
             (setf start i)))
          (t (incf i)))))
    (when (< start len)
      (let ((s (clean-whitespace (subseq text start len))))
        (when (plusp (length s))
          (push s sentences))))
    (nreverse sentences)))

(defun build-word-sentences (text &key (lemmatize t))
  "Construct a dictionary mapping lemmatized words / stems to their list of example sentences."
  (let ((table (make-hash-table :test #'equal))
        (sentences (split-sentences text)))
    (dolist (sent sentences)
      (let ((tokens (tokenize sent))
            (seen-in-sent (make-hash-table :test #'equal)))
        (dolist (tok tokens)
          (let ((key (if lemmatize (stem-word tok) tok)))
            (unless (gethash key seen-in-sent)
              (setf (gethash key seen-in-sent) t)
              (push sent (gethash key table)))))))
    ;; Reverse so sentences appear in chronological reading order
    (maphash (lambda (k sents)
               (setf (gethash k table) (nreverse sents)))
             table)
    (setf *word-sentences* table)
    table))

(defun word-sentences (word &key (table *word-sentences*) (lemmatize t))
  "Retrieve the list of example sentences where WORD appears.
WORD can be either an inflected surface word or a lemmatized stem."
  (let* ((w (string-downcase word))
         (stem (if lemmatize (stem-word w) w)))
    (or (gethash stem table)
        (gethash w table)
        nil)))

(defun tokenize (text)
  "Extract lowercase alphabetic word tokens from TEXT."
  (let ((tokens nil)
        (len (length text))
        (start nil))
    (dotimes (i len)
      (let ((c (char text i)))
        (if (alpha-char-p c)
            (unless start (setf start i))
            (when start
              (push (string-downcase (subseq text start i)) tokens)
              (setf start nil)))))
    (when start
      (push (string-downcase (subseq text start len)) tokens))
    (nreverse tokens)))

(defun inverse-corpus-frequency (word)
  "Calculate Inverse Corpus Frequency (ICF / Divergence from Average) for WORD without Laplace smoothing.
Returns (log (total-words / general-count)).
If the word is not in the reference corpus (count 0), uses count = 1 to return maximum ICF."
  (ensure-frequencies-loaded)
  (let* ((general-count (gethash (string-downcase word) *general-frequencies* 0)))
    (if (plusp general-count)
        (log (/ (float *total-corpus-words*) (float general-count)))
        (log (float *total-corpus-words*)))))

(defun calculate-dispersion (seg-counts total-count num-segments &key (mode :evenness))
  "Calculate how evenly distributed a word is across the document segments.
Returns a normalized dispersion score in [0.01, 1.0].
MODE:
  :evenness  -> Gries Deviation of Proportions (1 - DP). 1.0 is perfectly even throughout text.
  :coverage  -> Ratio of segments in which the word appears at least once (active-segments / K).
  :entropy   -> Normalized Shannon entropy across the segment distribution."
  (case mode
    (:coverage
     (let ((active (count-if #'plusp seg-counts)))
       (max 0.01 (/ (float active) (float num-segments)))))
    (:entropy
     (let ((ent 0.0)
           (log-k (log (float num-segments))))
       (loop for c across seg-counts do
         (when (plusp c)
           (let ((p (/ (float c) (float total-count))))
             (decf ent (* p (log p))))))
       (max 0.01 (/ ent log-k))))
    (otherwise ;; :evenness (default)
     (let ((dp 0.0)
           (expected (/ 1.0 (float num-segments))))
       (loop for c across seg-counts do
         (let ((p (/ (float c) (float total-count))))
           (incf dp (abs (- p expected)))))
       ;; DP in [0, 1]; evenness = 1 - 0.5 * DP
       (max 0.01 (- 1.0 (* 0.5 dp)))))))

(defun calculate-word-scores (text &key (lemmatize t)
                                        (min-count 1)
                                        (min-icf nil)
                                        (drop-unseen nil)
                                        (tf-weight 1.0)
                                        (divergence-weight 2.0)
                                        (dispersion-weight 0.0)
                                        (tf-mode :inverse)
                                        (dispersion-mode :evenness)
                                        (num-segments 10)
                                        (include-sentences nil)
                                        (max-sentences 3))
  "Analyze TEXT and compute scores with the three linguistic levers:
1. TF-WEIGHT (alpha): Exponent/weight for Author Frequency (Lever 1). Default 1.0.
   - Combined with :TF-MODE :INVERSE to prioritize words used with restraint/sparingly.
2. DIVERGENCE-WEIGHT (beta): Exponent/weight for Divergence from Average / ICF (Lever 2). Default 2.0.
3. DISPERSION-WEIGHT (gamma): Exponent/weight for Evenness of Position across text (Lever 3). Default 0.0.

TF-MODE: How author term frequency is calculated:
  :inverse            -> (1 / TF) [INVERTED TF: prioritizes words used sparingly, default]
  :sublinear-inverse  -> 1 / (1 + log(TF)) [gentler inverted TF]
  :sublinear          -> (1 + log(TF)) [rewards repetition]
  :linear             -> TF [strictly linear repetition]
  :sqrt               -> sqrt(TF)
  :normalized         -> (TF / total-tokens)"
  (ensure-frequencies-loaded)
  ;; Always build background sentence dictionary so (word-sentences word) works on demand
  (build-word-sentences text :lemmatize lemmatize)
  (let* ((tokens (tokenize text))
         (total-tokens (max 1 (length tokens)))
         (raw-counts (make-hash-table :test #'equal))
         (segment-counts (make-hash-table :test #'equal))
         (lemma-map (make-hash-table :test #'equal))
         (k (max 2 num-segments)))
    ;; 1. Count occurrences and track position across segments
    (let ((i 0))
      (dolist (tok tokens)
        (when (and (>= (length tok) 3)
                 (not (gethash tok *stop-words*)))
          (let* ((term (if lemmatize (stem-word tok) tok))
                 (seg-idx (min (1- k) (floor (* i k) total-tokens))))
            (incf (gethash term raw-counts 0))
            ;; Track segment distribution
            (let ((seg-vec (gethash term segment-counts)))
              (unless seg-vec
                (setf seg-vec (make-array k :element-type 'fixnum :initial-element 0))
                (setf (gethash term segment-counts) seg-vec))
              (incf (aref seg-vec seg-idx)))
            ;; Track surface forms for lemma
            (when lemmatize
              (let ((forms (gethash term lemma-map)))
                (unless forms (setf forms (make-hash-table :test #'equal)))
                (incf (gethash tok forms 0))
                (setf (gethash term lemma-map) forms)))))
        (incf i)))
    ;; 2. Score words: Lever 1 (Inverted TF) * Lever 2 (Divergence / ICF) * Lever 3 (Dispersion / Position)
    (let ((results nil))
      (maphash
       (lambda (term count)
         (when (>= count min-count)
           (let* ((display-word
                    (if (and lemmatize (gethash term lemma-map))
                        (let ((best term)
                              (best-c 0))
                          (maphash (lambda (w c)
                                     (when (> c best-c)
                                       (setf best w best-c c)))
                                   (gethash term lemma-map))
                          best)
                        term))
                  (in-corpus-p (in-reference-corpus-p display-word)))
             (when (and (or (not drop-unseen) in-corpus-p)
                        (let ((icf (inverse-corpus-frequency display-word)))
                          (or (null min-icf) (>= icf min-icf))))
               (let* ((icf (inverse-corpus-frequency display-word))
                      (seg-vec (gethash term segment-counts))
                      (dispersion (calculate-dispersion seg-vec count k :mode dispersion-mode))
                      (active-segs (count-if #'plusp seg-vec))
                      (tf-val (ecase tf-mode
                                (:inverse (/ 1.0 (float count)))
                                (:sublinear-inverse (/ 1.0 (+ 1.0 (log (float count)))))
                                (:sublinear (+ 1.0 (log (float count))))
                                (:linear (float count))
                                (:sqrt (sqrt (float count)))
                                (:normalized (/ (float count) (float total-tokens)))))
                      (tf-comp (expt tf-val tf-weight))
                      (divergence-comp (expt icf divergence-weight))
                      (dispersion-comp (expt dispersion dispersion-weight))
                      (score (* tf-comp divergence-comp dispersion-comp))
                      (entry (list :word display-word
                                   :count count
                                   :icf icf
                                   :dispersion dispersion
                                   :coverage (format nil "~d/~d" active-segs k)
                                   :score score)))
                 (when include-sentences
                   (let ((all-sents (word-sentences term :lemmatize lemmatize)))
                     (setf (getf entry :sentences)
                           (if (and max-sentences (numberp max-sentences))
                               (subseq all-sents 0 (min max-sentences (length all-sents)))
                               all-sents))))
                 (push entry results))))))
       raw-counts)
      (sort results #'> :key (lambda (x) (getf x :score))))))

(defun uncommon-words (source &key (top-n 30)
                                   (min-count 1)
                                   (min-icf nil)
                                   (drop-unseen nil)
                                   (lemmatize t)
                                   (tf-weight 1.0)
                                   (divergence-weight 2.0)
                                   (dispersion-weight 0.0)
                                   (tf-mode :inverse)
                                   (dispersion-mode :evenness)
                                   (num-segments 10)
                                   (include-sentences nil)
                                   (max-sentences 3))
  "Extract uncommon, distinctive words from SOURCE (file path or raw text string).
DEFAULT SETTING: INVERTED TF (:TF-MODE :INVERSE, :MIN-COUNT 1, :DISPERSION-WEIGHT 0.0)
Prioritizes words used with literary restraint/rarity by the author.

THE THREE LEVERS:
  :TF-WEIGHT (alpha)        - Lever 1: Exponent for Inverted TF. Default 1.0.
  :DIVERGENCE-WEIGHT (beta) - Lever 2: Exponent for General Rarity (ICF). Default 2.0.
  :DISPERSION-WEIGHT (gamma)- Lever 3: Exponent for Position Evenness across text. Default 0.0.

TF-MODES:
  :inverse            -> 1 / TF [INVERTED TF: prioritizes words used sparingly, default]
  :sublinear-inverse  -> 1 / (1 + log(TF)) [gentler inverted TF]
  :sublinear          -> (1 + log(TF)) [direct TF: rewards repetition]
  :linear             -> TF [strictly linear repetition]
  :sqrt               -> sqrt(TF)
  :normalized         -> TF / total-words

OTHER OPTIONS:
  :DROP-UNSEEN              - If T, drops words not present in reference corpus (default NIL).
  :MIN-COUNT                - Minimum times author must use the word (default 1).
  :MIN-ICF                  - Optional hard cutoff for minimum ICF (e.g. 10.0 or 15.0).
  :LEMMATIZE                - Group inflections (e.g. scrivener/scriveners -> scrivener). Default T.
  :INCLUDE-SENTENCES        - If T, includes :sentences in result plist (default NIL).
  :MAX-SENTENCES            - Number of example sentences per word if :include-sentences is T (default 3).
  :TOP-N                    - Number of results to return (default 30)."
  (let* ((text (if (or (pathnamep source)
                       (and (stringp source) (probe-file (resolve-path source))))
                   (extract source)
                   source))
         (scored (calculate-word-scores text
                                        :lemmatize lemmatize
                                        :min-count min-count
                                        :min-icf min-icf
                                        :drop-unseen drop-unseen
                                        :tf-weight tf-weight
                                        :divergence-weight divergence-weight
                                        :dispersion-weight dispersion-weight
                                        :tf-mode tf-mode
                                        :dispersion-mode dispersion-mode
                                        :num-segments num-segments
                                        :include-sentences include-sentences
                                        :max-sentences max-sentences)))
    (subseq scored 0 (min top-n (length scored)))))
