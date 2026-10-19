(defsystem "lisp-ai-tools"
  :description "The tools every project under ~/lisp-ai/ shares: form-edit changes a Lisp file as forms."
  :depends-on ("eclector")
  :components ((:file "form-edit")))
