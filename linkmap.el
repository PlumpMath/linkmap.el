;;;; -*- lexical-binding: nil; -*-
;;;; linkmap.el is starts here

;;; Author: aJchemist <vachilleus@gmail.com>
;;; Version: 0.1

;;; apropos .*file-name-.*
;;; file-truename file-relative-name make-symbolic-link file-symlink-p abbreviate-file-name
;;; ref `org-id-locations-load'

(defvar lm/log-buffer-name "*LINK MAP*")

;;;###autoload
(defun lm-table-check (&optional base-directory)
  (interactive)
  (let ((default-directory (or base-directory default-directory))
        table-dir table-data)
    (setq table-dir (directory-file-name default-directory)
          table-data (lm/get-table-data))
    (if table-data
        (let ((left-table (car table-data))
              (table-alist (cdr table-data)))
          (mapc #'lm/record/check-link table-alist)
          ;; (lm/left-table/check left-table table-dir)
          (message "See results in %s" lm/log-buffer-name)))))

(defun lm/log (string &optional fix)
  (with-current-buffer (get-buffer-create lm/log-buffer-name)
    (let ((point (point))
          (eobp? (eobp)))
      (goto-char (point-max))
      (insert (concat (if fix "    " "* ") string (if fix nil "\n")))
      (if eobp? (goto-char (point-max)) (goto-char point)))))

(defun lm/get-table-data ()
  (if (file-exists-p ".link_map")
      (with-temp-buffer
        (insert-file-contents-literally ".link_map") ;ascii char only
        (goto-char (point-min))
        (skip-chars-forward "; ")
        (let ((start (point)) left-table)
          (unless (looking-at "(")
            (skip-chars-forward "^\n") ; .link_map file head must have newline
            (setq left-table (buffer-substring start (point))))
          (skip-chars-forward "^(")
          (goto-char (scan-sexps (point) 1))
          (cons left-table (preceding-sexp))))
    nil))

(defun lm/left-table/check (left-table table-dir)
  (if (file-exists-p left-table)
      (let ((left-table-dir (file-name-directory left-table)))
        (let ((default-directory left-table-dir))
          (lm/get-table-data)))))

(defun lm/record-type (record)
  (cond ((null (car record)) :absolute)
        (t :relative)))

(defmacro with-record (record &rest body)
  "Anaphoric linkmap macro"
  `(let* ((@record ,record)
          (@linkmap (cdr record))
          (@src (car @linkmap))
          (@tgt (cdr @linkmap))
          (@A (expand-file-name @src))
          (@B (expand-file-name @tgt)))
     ,@body))

(put 'with-record 'lisp-indent-function 1)

(defun lm/record/check-link (record)
  ;; add relsymlink check
  (with-record record
    (let ((B-> (file-symlink-p @B))
          (^B (file-name-directory @B)))
      (if (and B-> (string= (expand-file-name B-> ^B) @A))
          (lm/log (format "[ \x2713 ] %S" @record))
        (progn
          (lm/log (format "[ \x2715 ] %S" @record))
          (lm/log (format "make link %s -> %s ... "
                          @B (lm/make-source-relative @A @B)) 'fix)
          (and (lm/record/make-link @record)
               (lm/log "DONE\n" 'fix)))))))

(defun lm/record/make-link (record)
  (with-record record
    (if (equal (lm/record-type @record) :absolute)
        (lm/make-symbolic-link @A @B 'ok-if-already-exists)
      (lm/make-symbolic-link
       (directory-file-name (lm/make-source-relative @A @B))
       @B 'ok-if-already-exists))))

(defmacro lm/make-symbolic-link (fname lname &optional ok-if-already-exists)
  `(progn (make-symbolic-link ,fname ,lname ,ok-if-already-exists)
          (file-symlink-p ,lname)))

;; ref dired-make-relative-symlink
(defun lm/make-source-relative (A B)
  "A and B is absolute paths, and source is A and target is B."
  (let* ((index 0)
         (lenA (length A))
         (lenB (length B))
         (minAB (min lenA lenB))
         next sub source target)
    (while (and (setq next (string-match "/" A index)) 
                (< (setq next (1+ next)) minAB)
                ;; For the comparison, both substrings must end in
                ;; `/', so NEXT is *one plus* the result of the
                ;; string-match.
                ;; E.g., consider the case of linking "/tmp/a/abc"
                ;; to "/tmp/abc" erroneously giving "/tmp/a" instead
                ;; of "/tmp/" as common initial component.
                ;; and also prevent infinite loop.
                (string-equal (substring A 0 next)
                              (substring B 0 next)))
      (setq index next))
    (setq target B
          sub (substring A 0 index)
          source (substring A index))
    (if (string= sub "/") 
        (setq source A)                 ; No common initial file name found
      (let ((tem (substring B index))
            (start 0)
            (count 0))
        ;; Count number of slashes we must compensate for ...
        (while (setq start (string-match "/" tem start))
          (setq count (1+ count)
                start (1+ start)))
        ;; ... and prepend a "../" for each slash found:
        (dotimes (_n count source)
          (setq source (concat "../" source)))))))

(provide 'linkmap)

;;;; linkmap.el is starts here
