;;; joos-peephole.el --- syntax highlighting for JOOS peephole optimization

;;; Commentary:
;;  major mode for editing DSL for peephole optimiztion pattern for JOOS

;;; Code:
(setq joos-peephole-highlights
      '(( "\\.pattern\\|\\.end\\|\\.def\\|\\.pre\\|\\.schema\\|\\.extern\\|=>" . font-lock-keyword-face )
        ( "\\[ *[a-zA-Z_$][a-zA-Z0-9_$]* *\\]" . font-lock-variable-name-face )
        ( "\\([a-zA-Z_$][a-zA-Z0-9_$]*/\\)*[a-zA-Z_$][a-zA-Z0-9_$]*([a-zA-Z0-9_$;\\[]*)[a-zA-Z0-9_$;\\[]*" . font-lock-function-name-face)
        ( "\\_<\\(nop\\|i2c\\|new\\|instanceof\\|checkcast\\|imul\\|ineg\\|irem\\|isub\\|idiv\\|iadd\\|iinc\\|goto\\|ifeq\\|ifne\\|if_acmpeq\\|if_acmpne\\|ifnull\\|ifnonnull\\|if_icmpeq\\|if_icmpgt\\|if_icmplt\\|if_icmple\\|if_icmpge\\|if_icmpne\\|ireturn\\|areturn\\|return\\|aload\\|astore\\|iload\\|istore\\|dup\\|pop\\|swap\\|ldc\\|aconst_null\\|getfield\\|putfield\\|invokevirtual\\|invokenonvirtual\\)\\_>" . font-lock-builtin-face)
        ( "\\_<\\([+-]?\\(0\\|\\([1-9][0-9]*\\)\\)\\)\\_>" . font-lock-constant-face )
        ))

(define-derived-mode joos-peephole-mode prog-mode "joos-peephole-mode"
  "major mode for editing DSL for peephole optimiztion pattern for JOOS"
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq indent-line-function 'insert-tab)
  (setq font-lock-defaults '(joos-peephole-highlights)
        tab-width          2))

(provide 'joos-peephole-mode)
;;; joos-peephole.el ends here
