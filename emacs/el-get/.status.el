((el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (kill-ring-ido status "installed" recipe
                (:name kill-ring-ido :auto-generated t :type emacswiki :description "command for kill-ring browsing with ido" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/kill-ring-ido.el"))
 (undo-tree status "installed" recipe
            (:name undo-tree :description "Treat undo history as a tree" :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :prepare
                   (progn
                     (autoload 'undo-tree-mode "undo-tree.el" "Undo tree mode; see undo-tree.el for details" t)
                     (autoload 'global-undo-tree-mode "undo-tree.el" "Global undo tree mode" t)))))
