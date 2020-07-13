;;; .local/straight/repos/google-translator.el/google-translator.el -*- lexical-binding: t; -*-

(require 'google-translator-tk)

(defconst google-translator-request-url "https://translate.google.cn/translate_a/single")

(defgroup google-translator nil ""
  :group 'Tools
  :version "0.0.1")

(defcustom google-translator-cache-file "~/.google-translator" ""
  :type 'string
  :group 'google-translator)

(defcustom google-translator-show-result-function
  'google-translator-default-show-result-function
  ""
  :type 'function
  :group 'google-translator)

(defcustom google-translator-format-result-function
  'google-translator-default-format-result-function
  ""
  :type 'function
  :group 'google-translator)

(defvar google-translator-cache-data
  (if (file-exists-p google-translator-cache-file)
      (with-temp-buffer
        (insert-file-contents google-translator-cache-file)
        (read (buffer-string)))
    (make-hash-table :test #'equal)))

(defvar google-translator-buffer "*google-translator*" "")

(defvar google-translator-move-commands
  '(next-line
    previous-line
    backward-button
    backward-char
    backward-delete-char
    backward-delete-char-untabify
    backward-kill-paragraph
    backward-kill-sentence
    backward-kill-sexp
    backward-kill-word
    backward-list
    backward-page
    backward-paragraph
    backward-sentence
    backward-sexp
    backward-to-indentation
    backward-to-word
    backward-up-list
    backward-word
    forward-button
    forward-char
    forward-line
    forward-list
    forward-page
    forward-paragraph
    forward-same-syntax
    forward-sentence
    forward-sexp
    forward-symbol
    forward-to-indentation
    forward-to-word
    forward-whitespace
    forward-word
    evil-forward-WORD-begin
    evil-forward-WORD-end
    evil-forward-arg
    evil-forward-char
    evil-forward-paragraph
    evil-forward-section-begin
    evil-forward-section-end
    evil-forward-sentence-begin
    evil-forward-word-begin
    evil-forward-word-end
    evil-backward-WORD-begin
    evil-backward-WORD-end
    evil-backward-arg
    evil-backward-char
    evil-backward-paragraph
    evil-backward-section-begin
    evil-backward-section-end
    evil-backward-sentence-begin
    evil-backward-word-begin
    evil-backward-word-end
    evil-next-line
    evil-previous-line
    isearch-exit
    baidu-translator-translate-thing-at-point))

(defun google-translator-format-url (query-params)
  (format "%s?%s" google-translator-request-url
          (google-translator-format-query-string query-params)))

(defun google-translator-format-query-string (query-params)
  (mapconcat #'(lambda (p)
                 (format "%s=%s"
                         (url-hexify-string (car p))
                         (url-hexify-string (cdr p))))
             query-params "&"))

(defun google-translator-build-url (from to text)
  (google-translator-format-url
   `(("client" . "t")
     ("ie"     . "UTF-8")
     ("oe"     . "UTF-8")
     ("sl"     . ,from)
     ("tl"     . ,to)
     ("q"      . ,text)
     ("dt"     . "bd")
     ("dt"     . "ex")
     ("dt"     . "ld")
     ("dt"     . "md")
     ("dt"     . "qc")
     ("dt"     . "rw")
     ("dt"     . "rm")
     ("dt"     . "ss")
     ("dt"     . "t")
     ("dt"     . "at")
     ("pc"     . "1")
     ("otf"    . "1")
     ("srcrom" . "1")
     ("ssel"   . "0")
     ("tsel"   . "0")
     ("tk"     . ,(google-translator-gen-tk text)))))

(defun google-translator-http-get (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (when (not (string-match "200 OK" (buffer-string)))
      (error "Problem connecting to the server"))
    (re-search-forward "^$" nil 'move)
    (prog1
        (buffer-substring-no-properties (point) (point-max))
      (kill-buffer))))

(defun google-translator-request (from to text)
  (let* ((url (google-translator-build-url from to text))
         (result (google-translator-http-get url)))
    (aref (json-read-from-string result) 0)))

(defun google-translator-trim-space (text)
  (setq text (replace-regexp-in-string "\n\s*$" "" text)))

(defun google-translator-puthash (origin translation)
  (setq origin (google-translator-trim-space origin))
  (setq translation (google-translator-trim-space translation))
  (puthash origin (concat origin "\n" translation "\n") google-translator-cache-data))

(defun google-translator-cache-result (json)
  (mapc (lambda (item) 
          (let ((origin (aref item 1))
                (translation (aref item 0)))
            (google-translator-puthash origin translation)))
        (seq-filter 'seq-first json)))

(defun google-translator-default-format-result-function (json)
  (mapconcat (lambda (item)
               (let ((origin (google-translator-trim-space (aref item 1)))
                     (translation (google-translator-trim-space (aref item 0))))
                 (concat origin "\n" translation "\n")))
             (seq-filter #'seq-first json) ""))

(defun google-translator-default-show-result-function (result)
  (require 'posframe)
  (when (posframe-workable-p)
    (posframe-show
     google-translator-buffer
     :string result
     :timeout 100
     :poshandler 'posframe-poshandler-frame-bottom-center
     :min-width (frame-width)
     :internal-border-width 10)
    (unwind-protect
        (push (read-event) unread-command-events)
      (posframe-delete google-translator-buffer))))

(defun google-translator-translate (from to text)
  (let ((json (google-translator-request from to text)))
    (if (null json)
        (message "Nothing to translate.")
      (let ((result (funcall google-translator-format-result-function json))
            (show-function (or google-translator-show-result-function #'print)))
        (google-translator-cache-result json)
        (funcall show-function result)))))

(defun google-translator-transform-special-text (text)
  (when text
    (when (derived-mode-p 'Info-mode)
      ;; (setq text (replace-regexp-in-string "^‘\\(.*\\)’$" "\\1\n" text)) ;; ‘inhibit-same-window’
      ;; (setq text (replace-regexp-in-string "\\*Note \\([^:]*\\)::" "See \\1" text))
      ;; (setq text (replace-regexp-in-string "^[\\*-]{2,}" "" text)) ; remove ****** or ------------
      (setq text (replace-regexp-in-string "^\s*-- .*$" "\\&@@@" text)) ; -- function 
      (setq text (replace-regexp-in-string "\s+" " " text))
      ;; (setq text (replace-regexp-in-string "^[=\\*-]+$" "" text)) ; ============ -----------
      ;; (setq text (replace-regexp-in-string "$^" " " text))
      )
    (setq text (replace-regexp-in-string "\s*\n\s*" " " text))
    (setq text (replace-regexp-in-string "@@@" "\n" text))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (progn
               (forward-sentence)
               (not (equal (point) (point-max))))
        (insert "\n"))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defvar google-translator-timer nil)

(defmacro google-translator-lazy-execute (func args)
  `(setq google-translator-timer
         (run-with-idle-timer
          0.5 nil
          (lambda (args) (apply func args))
          args)))

(defun google-translator-sentence-at-point ()
  (google-translator-transform-special-text (thing-at-point 'sentence t)))

(defun google-translator-paragraph-at-point ()
  (google-translator-transform-special-text (thing-at-point 'paragraph t)))

(defun google-translator-translate-sentence-at-point ()
  (interactive)
  (let* ((sentence (google-translator-transform-special-text
                    (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (google-translator-sentence-at-point))))
         (cache (gethash sentence google-translator-cache-data))
         (from "en")
         (to "zh-cn"))
    (if cache
        (funcall google-translator-show-result-function cache)
      (google-translator-translate
       from to
       (google-translator-paragraph-at-point)))))

(defun google-translator-translate-sentence-at-point ()
  (interactive)
  (let* ((paragraph
          (google-translator-transform-special-text
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (google-translator-paragraph-at-point))))
         (from "en")
         (to "zh-cn"))
    (google-translator-translate
     from to
     paragraph
     )))

(define-minor-mode google-translator-follow-mode
  "follow mode"
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (cond (google-translator-follow-mode
         (progn
           (add-hook 'post-command-hook #'google-translator-follow-mode-translate nil t)))
        (t (remove-hook 'post-command-hook #'google-translator-follow-mode-translate t))))

(defun google-translator-follow-mode-translate ()
  (let* ((sentence (google-translator-transform-special-text
                    (google-translator-sentence-at-point)))
         (cache (gethash sentence google-translator-cache-data))
         func args)
    (when google-translator-timer (cancel-timer google-translator-timer))
    (setq func (if cache google-translator-show-result-function #'google-translator-translate))
    (setq args (if cache (list cache) (list "en" "zh-cn" (google-translator-paragraph-at-point))))
    (google-translator-lazy-execute func args)))


(provide 'google-translator)
