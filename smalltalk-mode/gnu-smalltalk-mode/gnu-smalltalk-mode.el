;;; gnu-smalltalk-mode.el ---

;;;###autoload
(add-to-list 'auto-mode-alist 
      (cons "\\.star\\'"
            (catch 'archive-mode
              (dolist (mode-assoc auto-mode-alist 'archive-mode)
                (and (string-match (car mode-assoc) "Starfile.zip")
                     (functionp (cdr mode-assoc))
                     (throw 'archive-mode (cdr mode-assoc)))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.st\\'" . smalltalk-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.star\\'" . inhibit-file-local-variables-regexps))

;;;###autoload
(autoload 'gst "gst-mode.elc" "" t)

;;;###autoload
(autoload 'smalltalk-mode "smalltalk-mode.elc" "" t)

;;; gnu-smalltalk-mode.el ends here
