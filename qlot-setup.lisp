;;;; tools/qlot-setup.lisp -- loaded by Qlot (via QLOT_SETUP_FILE, set in tools/qlot) in place of its own
;;;; setup file. Qlot's cl+ssl hard-codes /opt/homebrew/opt/openssl/lib/libcrypto.dylib, which Homebrew
;;;; now points at openssl@4, and OpenSSL 4 removed ASN1_STRING_data. So load Qlot's real setup, then
;;;; pin BOTH libraries to openssl@3 (never one of each) before cl+ssl is loaded.
;;; Qlot's own start-up replaces SBCL's UIOP and prints hundreds of redefinition warnings; mute those about libraries only.
(load (merge-pathnames "quiet-init.lisp" *load-truename*))

(let (*load-verbose*)
  (load (merge-pathnames ".bundle-libs/setup.lisp" #p"/Users/tom/.local/share/qlot/qlot/")))
(let (*load-verbose*)
  (asdf:load-asd #p"/Users/tom/.local/share/qlot/qlot/.bundle-libs/software/cl+ssl-20260101-git/cl+ssl.asd")
  (asdf:load-system :cl+ssl/config))
(cl+ssl/config:define-libssl-path "/opt/homebrew/opt/openssl@3/lib/libssl.dylib")
(cl+ssl/config:define-libcrypto-path "/opt/homebrew/opt/openssl@3/lib/libcrypto.dylib")
