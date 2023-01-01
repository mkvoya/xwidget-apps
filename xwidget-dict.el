;;; xwidget-dict.el --- The dict apps based on Emacs xwidget -*- lexical-binding:t -*-
;;; Commentary:
;;; Some code adapted from popweb.

;;; Code:

(require 'xwidget)
(require 'posframe)

(defvar xwidget-dict--session nil "The xwidget instance used for dict.")
(defvar xwidget-dict--buffer nil "The buffer used for dict.")
(defvar xwidget-dict--js-onload "" "Javascript code to run on page loaded.")
(defvar xwidget-dict--posframe-point (point) "Popup position.")

(defun xwidget-dict-ensure ()
  "Ensure an xwidget instance exist for dict."
  (unless xwidget-dict--session
    (setq xwidget-dict--buffer (xwidget-webkit--create-new-session-buffer "About:blank"))
    (setq xwidget-dict--session (xwidget-webkit-last-session))
  ))

;; We have to patch the xwidget-webkit-callback function to inject js code after a page is fully loaded.
(defvar xwidget-webkit-loaded-hook nil "Hook functions to run when a page is loaded.")
(defun xwidget-webkit-callback (xwidget xwidget-event-type)
  "Callback for xwidgets.
XWIDGET instance, XWIDGET-EVENT-TYPE depends on the originating xwidget."
  (if (not (buffer-live-p (xwidget-buffer xwidget)))
      (xwidget-log
       "error: callback called for xwidget with dead buffer")
    (cond ((eq xwidget-event-type 'load-changed)
           (let ((title (xwidget-webkit-title xwidget))
                 (uri (xwidget-webkit-uri xwidget)))
             (when-let ((buffer (get-buffer "*Xwidget WebKit History*")))
               (with-current-buffer buffer
                 (revert-buffer)))
             (with-current-buffer (xwidget-buffer xwidget)
               (if (string-equal (nth 3 last-input-event)
                                 "load-finished")
                   (progn
                     (setq xwidget-webkit--loading-p nil)
                     (cancel-timer xwidget-webkit--progress-update-timer))
                 (unless xwidget-webkit--loading-p
                   (setq xwidget-webkit--loading-p t
                         xwidget-webkit--progress-update-timer
                         (run-at-time 0.5 0.5 #'xwidget-webkit--update-progress-timer-function
                                      xwidget)))))
             ;; This function will be called multi times, so only
             ;; change buffer name when the load actually completes
             ;; this can limit buffer-name flicker in mode-line.
             (when (or (string-equal (nth 3 last-input-event)
                                     "load-finished")
                       (> (length title) 0))
               (with-current-buffer (xwidget-buffer xwidget)
                 (force-mode-line-update)
                 (xwidget-log "webkit finished loading: %s" title)
                 ;; Do not adjust webkit size to window here, the
                 ;; selected window can be the mini-buffer window
                 ;; unwantedly.
                 (rename-buffer
                  (format-spec
                   xwidget-webkit-buffer-name-format
                   `((?T . ,title)
                     (?U . ,uri)))
                  t)
                 (run-hooks 'xwidget-webkit-loaded-hook)
                 ))))
          ((eq xwidget-event-type 'decide-policy)
           (let ((strarg  (nth 3 last-input-event)))
             (if (string-match ".*#\\(.*\\)" strarg)
                 (xwidget-webkit-show-id-or-named-element
                  xwidget
                  (match-string 1 strarg)))))
          ;; TODO: Response handling other than download.
          ((eq xwidget-event-type 'download-callback)
           (let ((url  (nth 3 last-input-event))
                 (mime-type (nth 4 last-input-event))
                 (file-name (nth 5 last-input-event)))
             (xwidget-webkit-save-as-file url mime-type file-name)))
          ((eq xwidget-event-type 'javascript-callback)
           (let ((proc (nth 3 last-input-event))
                 (arg  (nth 4 last-input-event)))
             (funcall proc arg)))
          (t (xwidget-log "unhandled event:%s" xwidget-event-type)))))


(defun xwidget-dict-apply-js-onload ()
  "Apply the js patch code on page loaded."
  (xwidget-webkit-execute-script
   xwidget-dict--session
   xwidget-dict--js-onload)
  (when (posframe-workable-p)
    (posframe-show xwidget-dict--buffer
                   :position xwidget-dict--posframe-point
                   :border-color "red"
                   :width 60
                   ;; :timeout 3
                   :height 20
                   :border-width 1))
  (xwidget-dict--posframe-set-delayed-autohide)
  )

(defun xwidget-dict--posframe-hide-after-move ()
  "Hide posframe after move."
  (posframe-hide xwidget-dict--buffer)
  (remove-hook 'post-command-hook #'xwidget-dict--posframe-hide-after-move))

(defun xwidget-dict--posframe-set-delayed-autohide ()
  "Hold the popframe for one second."
  (run-with-timer 1 nil
                  (lambda () (add-hook 'post-command-hook #'xwidget-dict--posframe-hide-after-move))))


;;;###autoload
(defun xwidget-dict-lookup (word)
  "Lookup a WORD."
  (interactive (list (read-from-minibuffer "Word: " (thing-at-point 'word t))))
  (xwidget-dict-ensure)
  (let ((url (format "http://www.bing.com/dict/search?mkt=zh-cn&q=%s" word)))
    (setq xwidget-dict--posframe-point (point))
    (setq xwidget-dict--js-onload
          "window.scrollTo(0, 0); document.getElementsByTagName('html')[0].style.visibility = 'hidden'; document.getElementsByClassName('lf_area')[0].style.visibility = 'visible'; document.getElementsByTagName('header')[0].style.display = 'none'; document.getElementsByClassName('contentPadding')[0].style.padding = '10px';")
    (xwidget-webkit-goto-uri xwidget-dict--session url)))

;;;###autoload
(defun xwidget-dict-lookup-at-point ()
  "Lookup word at point and how in xwidget frame."
  (interactive)
  (xwidget-dict-lookup (if mark-active
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end))
                         (thing-at-point 'word t))))

(defun xwidget-dict-enable()
  "Enable app."
  (add-hook 'xwidget-webkit-loaded-hook #'xwidget-dict-apply-js-onload)
  )

(defun xwidget-dict-disable()
  "Disable app."
  (remove-hook 'xwidget-webkit-loaded-hook #'xwidget-dict-apply-js-onload)
  )

;;;###autoload
(define-minor-mode xwidget-dict-mode
  "Online dictionary using xwidget."
  :global t
  (if xwidget-dict-mode
      (xwidget-dict-enable)
    (xwidget-dict-disable)))

(provide 'xwidget-dict)
;;; xwidget-dict.el ends here.
