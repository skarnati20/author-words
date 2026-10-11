;;;; package.lisp

(defpackage #:author-words
  (:use #:cl)
  (:export #:file-type
           #:resolve-path
           #:extract
           #:extract-pdf
           #:extract-pdf-ocr
           #:ocr-available-p
           #:extract-epub
           #:extract-azw3
           #:extract-kfx
           #:extract-text
           #:stem-word
           #:stem-tokens
           #:tokenize
           #:split-sentences
           #:clean-whitespace
           #:build-word-sentences
           #:word-sentences
           #:*word-sentences*
           #:in-reference-corpus-p
           #:calculate-dispersion
           #:inverse-corpus-frequency
           #:calculate-word-scores
           #:uncommon-words
           #:load-general-frequencies
           #:*general-frequencies*
           #:*stop-words*))
