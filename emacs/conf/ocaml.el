;; Configuration for OCaml, supported through Tuareg mode. 

(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
  (add-to-list 'completion-ignored-extensions ext))
(setq ocamlspot-command (executable-find "ocamlspot"))
(autoload 'ocamlspot-query "ocamlspot" "OCamlSpot")

(add-hook 'tuareg-mode-hook
          '(lambda ()
             (local-set-key "\C-c;" 'ocamlspot-query)
             (local-set-key "\C-c\C-t" 'ocamlspot-type)
             (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
             (local-set-key "\C-c\C-u" 'ocamlspot-use)
             (local-set-key "\C-ct" 'caml-types-show-type)))

