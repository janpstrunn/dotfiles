;;; ../../dotfiles/.config/doom/packages/lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (after! lsp-ui
    (setq lsp-ui-sideline-enable t)      ; Enable sideline messages
    (setq lsp-ui-sideline-show-diagnostics t) ; Show diagnostics in the buffer
    (setq lsp-ui-flycheck-enable t)      ; Enable flycheck integr(setq sideline-backends-right '(sideline-lsp sideline-flycheck))ation with LSP
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-sideline-show-code-actions t)))
