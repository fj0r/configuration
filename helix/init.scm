(require "helix/configuration.scm")

(define-lsp "steel-language-server" (command "steel-language-server") (args '()))
(define-language "scheme"
                 (language-servers '("steel-language-server")))

;;(require "mattwparas-helix-package/helix.scm")
;;(require "mattwparas-helix-package/cogs/file-tree.scm")
