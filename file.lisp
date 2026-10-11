;;;; file.lisp

(in-package :author-words)


(defun resolve-path (file-path)
  (pathname (uiop:native-namestring file-path)))

(defun file-type (file-path)
  (pathname-type (resolve-path file-path)))

(defparameter *ocr-binary-path*
  (merge-pathnames "bin/ocr-pdf"
                   (asdf:system-source-directory :author-words)))

(defparameter *ebook-extractor-script*
  (merge-pathnames "bin/extract-ebook.py"
                   (asdf:system-source-directory :author-words)))

(defun ocr-available-p ()
  (and (probe-file *ocr-binary-path*) t))

(defun extract-pdf-ocr (file-path)
  "Extract text from PDF using native OCR engine."
  (let ((path (uiop:native-namestring (resolve-path file-path))))
    (if (probe-file *ocr-binary-path*)
        (uiop:run-program (list (uiop:native-namestring *ocr-binary-path*) path)
                          :output :string
                          :ignore-error-status t)
        (pdf:extract-pdf-text path))))

(defun extract-pdf (file-path)
  (let ((resolved-path (resolve-path file-path)))
    (if (ocr-available-p)
        (let ((text (extract-pdf-ocr resolved-path)))
          (if (and text (plusp (length text)))
              text
              (pdf:extract-pdf-text resolved-path)))
        (pdf:extract-pdf-text resolved-path))))

(defun run-ebook-extractor (file-path)
  "Invoke helper script to unpack and extract eBook text."
  (let ((path (uiop:native-namestring (resolve-path file-path)))
        (script (uiop:native-namestring *ebook-extractor-script*)))
    (multiple-value-bind (out err code)
        (uiop:run-program (list "python3" script path)
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (if (zerop code)
          out
          (error "eBook extraction error: ~A" err)))))

(defun extract-epub (file-path)
  (let ((text (run-ebook-extractor file-path)))
    (if (and text (plusp (length (string-trim '(#\Space #\Newline #\Tab) text))))
        text
        (error "Failed to extract text from EPUB: ~A" file-path))))

(defun extract-azw3 (file-path)
  (let ((text (run-ebook-extractor file-path)))
    (if (and text (plusp (length (string-trim '(#\Space #\Newline #\Tab) text))))
        text
        (error "Failed to extract text from AZW3: ~A" file-path))))

(defun extract-kfx (file-path)
  (let ((text (run-ebook-extractor file-path)))
    (if (and text (plusp (length (string-trim '(#\Space #\Newline #\Tab) text))))
        text
        (error "Failed to extract text from KFX: ~A" file-path))))

(defun extract-text (file-path)
  (let* ((resolved-path (resolve-path file-path))
	 (text (uiop:read-file-string resolved-path)))
    text))

(defun extract (file-path)
  (let ((type (file-type file-path)))
    (cond
      ((string-equal type "pdf") (extract-pdf file-path))
      ((string-equal type "epub") (extract-epub file-path))
      ((or (string-equal type "azw3") (string-equal type "azw") (string-equal type "mobi"))
       (extract-azw3 file-path))
      ((string-equal type "kfx") (extract-kfx file-path))
      (t (extract-text file-path)))))
