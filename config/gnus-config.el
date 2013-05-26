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

; buffer switching, see: http://www.emacswiki.org/emacs/SwitchToGnus

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
