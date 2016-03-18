;;; helloworld.el ---

(defun helloworld-fun () "" 
       (interactive) 
       (print "hello world") 
       nil)

;;;###autoload
(autoload 'helloworld-fun "helloworld" "" t)

(provide 'helloworld-fun)
