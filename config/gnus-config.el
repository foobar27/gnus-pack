(live-add-pack-lib "vendor/gnus/lisp")
(live-add-pack-lib "vendor/gnus/contrib")
(require 'gnus-load)

(setq gnus-pack-use-w3m nil)
(when (not (or (eq system-type 'windows-nt)
               (eq system-type 'cygwin)))
  (live-add-pack-lib "vendor/emacs-w3m")
  (require 'w3m-load)
  (setq gnus-pack-use-w3m t))

;; configuration
(setq gnus-pack-prefer-text t)

(setq message-send-mail-function 'smtpmail-send-it)

(defun gnus-pack-setup ()
  (progn
    (setq gnus-init-file (concat (file-name-directory (file-truename (or load-file-name buffer-file-name))) ".gnus"))
    (if gnus-pack-prefer-text
        (eval-after-load "gnus-sum"
          '(add-to-list
            'gnus-newsgroup-variables '(mm-discouraged-alternatives
                                         . '("text/html" "text/richtext")))))))

(defun gnus-pack-setup-dot-gnus ()
  (setq starttls-use-gnutls t)
  ;;  (gnus-demon-add-scanmail)
  (setq gnus-check-new-newsgroups nil
        gnus-asynchronous t
        gnus-use-article-prefetch t
        gnus-agent t
        mail-user-agent 'gnus-user-agent
        ;;mail-source-delete-incoming 7
        gnus-posting-styles '((".*" (name user-full-name)))
        )
  (when (require 'w3m nil 'noerror)
    (setq gnus-article-wash-function 'gnus-article-wash-html-with-w3m
          mm-text-html-renderer 'w3m))
  (setq mm-inline-text-html-with-images t
        mm-inline-large-images t
        gnus-permanently-visible-groups "mail")

  (setq gnus-select-method '(nnnil "")
        nnmail-crosspost nil
        nnmail-mail-splitting-decodes t)
  (add-hook 'message-mode-hook 'turn-on-auto-fill)
  )


(defun gnus-pack-setup-gmail-smtp (email password)
  (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials `(("smtp.gmail.com" 587 ,email ,password))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587))

(defun gnus-pack-setup-gmail-pop3 (email password)
  (require 'pop3)
  (if (not (boundp 'mail-sources))
      (setq mail-sources '()))
  (add-to-list 'mail-sources
               `(pop :server "pop.gmail.com"
                     :port 995
                     :user ,email
                     :password ,password
                     :connection ssl
                     :leave t)))

(require 'epg)

;;(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; formatting, see http://www.emacswiki.org/emacs/TomRauchenwald
;; eye candy
(copy-face 'font-lock-variable-name-face 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'font-lock-constant-face 'gnus-face-7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-face-7 'gnus-summary-normal-unread)
(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(copy-face 'font-lock-constant-face 'gnus-face-9)
(set-face-foreground 'gnus-face-9 "gray70")
(setq gnus-face-9 'gnus-face-9)
(setq gnus-summary-make-false-root 'dummy)
(setq gnus-summary-make-false-root-always nil)
(defun oxy-unicode-threads ()
  (interactive)
  (setq gnus-summary-dummy-line-format "                    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
        ;;gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
;;        gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %s\n"
        gnus-summary-line-format "%8{%20&user-date;│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %s\n"
        ;;gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "■ "
	gnus-sum-thread-tree-false-root "□ "
	gnus-sum-thread-tree-single-indent "▣ "
	gnus-sum-thread-tree-leaf-with-other "├─▶ "
	gnus-sum-thread-tree-vertical "│"
	gnus-sum-thread-tree-single-leaf "└─▶ "))

(defun oxy-unicode-threads-heavy ()
  (interactive)
  (setq gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
	gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "┏● "
	gnus-sum-thread-tree-false-root " ○ "
	gnus-sum-thread-tree-single-indent " ● "
	gnus-sum-thread-tree-leaf-with-other "┣━━❯ "
	gnus-sum-thread-tree-vertical "┃"
	gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(oxy-unicode-threads)

;; Replacing common prefixes of group names with spaces
;; see: http://www.emacswiki.org/emacs/GnusFormatting#toc5
(defvar gnus-user-format-function-g-prev "" "")
(defun empty-common-prefix (left right)
  "Given `left' '(\"foo\" \"bar\" \"fie\") and `right' '(\"foo\"
    \"bar\" \"fum\"), return '(\"   \" \"   \" \"fum\")."
  (if (and (cdr right)			; always keep the last part of right
    	   (equal (car left) (car right)))
      (cons (make-string (length (car left)) ? )
    	    (empty-common-prefix (cdr left) (cdr right)))
    right))

(defun gnus-user-format-function-g (arg)
  "The full group name, but if it starts with a previously seen
    prefix, empty that prefix."
  (if (equal gnus-user-format-function-g-prev gnus-tmp-group) ; line-format is updated on exiting the summary, making prev equal this
      gnus-tmp-group
    (let* ((prev (split-string-and-unquote gnus-user-format-function-g-prev "\\."))
    	   (this (split-string-and-unquote gnus-tmp-group "\\.")))
      (setq gnus-user-format-function-g-prev gnus-tmp-group)
      (combine-and-quote-strings
       (empty-common-prefix prev this)
       "."))))
(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%ug%)\n")

;; buffer switching, see: http://www.emacswiki.org/emacs/SwitchToGnus
(defun switch-to-gnus (&optional arg)
  "Switch to a Gnus related buffer.
    Candidates are buffers starting with
     *mail or *reply or *wide reply
     *Summary or
     *Group*

    Use a prefix argument to start Gnus if no candidate exists."
  (interactive "P")
  (let (candidate
	(alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
		 ("^\\*Group")
		 ("^\\*Summary")
		 ("^\\*Article" nil (lambda ()
				      (buffer-live-p gnus-article-current-summary))))))
    (catch 'none-found
      (dolist (item alist)
	(let (last
	      (regexp (nth 0 item))
	      (optional (nth 1 item))
	      (test (nth 2 item)))
	  (dolist (buf (buffer-list))
	    (when (and (string-match regexp (buffer-name buf))
		       (> (buffer-size buf) 0))
	      (setq last buf)))
	  (cond ((and last (or (not test) (funcall test)))
		 (setq candidate last))
		(optional
		 nil)
		(t
		 (throw 'none-found t))))))
    (cond (candidate
	   (switch-to-buffer candidate))
	  (arg
	   (gnus))
	  (t
	   (error "No candidate found")))))
