(require "helix/configuration.scm")

(define-lsp "steel-language-server" (command "steel-language-server") (args '()))
(define-language "scheme"
                 (language-servers '("steel-language-server")))

(require "mattwparas-helix-package/helix.scm")
(require "mattwparas-helix-package/cogs/file-tree.scm")

; (require "mattwparas-helix-package/cogs/recentf.scm")
; (recentf-snapshot)

; (require "steel-pty/term.scm")
; (set-default-shell! "/use/bin/nu")

; (require "helix-file-watcher/file-watcher.scm")
; (spawn-watcher)
