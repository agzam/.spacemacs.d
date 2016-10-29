(setq ag-synonyms-packages '(synonyms))

(setq ag-synonyms-excluded-packages '())

(defun ag-synonyms/init-synonyms ()
  :defer t
  :init
  :config
  (setq synonyms-file        "~/.spacemacs.d/ag-synonyms/mthes10/mthesaur.txt"
        synonyms-cache-file  "~/.spacemacs.d/ag-synonyms/mthes10/mthesaur.txt.cache")
  
  (defvar synonyms-thesaurus-url "http://www.thesaurus.com/browse/")

  (defun ag/thesaurus-definition-no-read ()
    "opens word at thesaurus.com"
    (interactive)
    (let ((word (word-at-point)))
      (browse-url (concat synonyms-thesaurus-url word))))

  (defun ag/thesaurus-definition (arg)
    (interactive (list (read-string "word: ")))
    (browse-url (concat synonyms-thesaurus-url arg)))

      ;; (when chosen
      ;;       (goto-char (car thesaurus-bounds-of-looked-up-word))
      ;;       (delete-region (car thesaurus-bounds-of-looked-up-word)
      ;;                      (cdr thesaurus-bounds-of-looked-up-word))
      ;;       (insert chosen))
      ;; ))

  ;; (thesaurus-choose-synonym-and-replace word))

  (defun ag/replace-word-other-window ()
    "Replaces wrd in adjacent window with the one that selected (under cursor) in active window"
    (interactive)
    (let* ((bnd-1 (ag/region-or-symbol-bounds))
           (str-1 (buffer-substring-no-properties
                   (car bnd-1)
                   (cdr bnd-1)))
           (bnd-2 (progn
                    (select-window (previous-window))
                    (ag/region-or-symbol-bounds))))
      (if bnd-2
          (progn
            (delete-region (car bnd-2) (cdr bnd-2))
            (insert str-1))
        (insert str-1))
      (switch-to-buffer-other-window "*Synonyms*")
      (quit-window)
      ))

  (defun switch-to-synonyms (&optional x y z)
    (switch-to-buffer-other-window "*Synonyms*"))

  (advice-add 'synonyms-no-read :after #'switch-to-synonyms))

  ;; (require 'thesaurus)
  ;; (setq thesaurus-bhl-api-key "268e6dcef65d60cea2f54799ec62eec1")

  ;; (defun thesaurus-fetch-synonyms (word)
  ;;   "replacing theasurus.el generic function with this, so it won't use message-box"   
  ;;   (let ((synonym-list nil)
  ;;         (buf (thesaurus-get-buffer-for-word word)))
  ;;     (if buf
  ;;         (progn
  ;;           (with-current-buffer buf
  ;;             (rename-buffer (concat "*thesaurus* - " word) t)
  ;;             (goto-char (point-min))
  ;;             (if (> (thesaurus-process-http-headers) 0)
  ;;                 (while (not (= (point-min) (point-max)))
  ;;                   (let ((elt (thesaurus-parse-one-line)))
  ;;                     (if elt
  ;;                         (add-to-list 'synonym-list elt))))
  ;;               (message "No synonyms found.")))
  ;;           (kill-buffer buf)
  ;;           (nreverse synonym-list)))))

  ;; (defun ag/thesaurus-choose-synonym-no-read ()
  ;;   (interactive)
  ;;   (let* ((word (ag/buffer-substring)) 
  ;;          (reg (ag/region-or-symbol-bounds))
  ;;          (chosen (thesaurus-prompt-user-with-choices (thesaurus-get-synonyms word))))
  ;;     (when chosen (progn
  ;;                    (delete-region (car reg) (cdr reg))
  ;;                    (insert chosen))))))
