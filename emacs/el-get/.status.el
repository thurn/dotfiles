((command-frequency status "installed" recipe
                    (:name command-frequency :website "http://xahlee.org/emacs/command-frequency.html" :description "This page lists emacs's commands in the order of their frequency of use." :type http :url "http://xahlee.org/emacs/command-frequency.el" :features "command-frequency"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (flex-isearch status "removed" recipe nil)
 (kill-ring-ido status "installed" recipe
                (:name kill-ring-ido :auto-generated t :type emacswiki :description "command for kill-ring browsing with ido" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/kill-ring-ido.el"))
 (textmate status "installed" recipe
           (:name textmate :description "TextMate minor mode for Emacs" :type github :pkgname "defunkt/textmate.el" :features textmate :post-init
                  (textmate-mode)))
 (undo-tree status "installed" recipe
            (:name undo-tree :description "Treat undo history as a tree" :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :prepare
                   (progn
                     (autoload 'undo-tree-mode "undo-tree.el" "Undo tree mode; see undo-tree.el for details" t)
                     (autoload 'global-undo-tree-mode "undo-tree.el" "Global undo tree mode" t)))))
