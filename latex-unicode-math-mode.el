;;; latex-unicode-math-mode.el --- Input method for Unicode math symbols -*- Coding: utf-8 -*-

;; Copyright 2016 Christoph Dittmann
;;
;; Author: Christoph Dittmann <github@christoph-d.de>
;; Version: 0.1
;; URL: https://github.com/Christoph-D/latex-unicode-math-mode

;;; Commentary:
;; An Emacs minor mode for entering Unicode math symbols in LaTeX-mode
;; (provided by AUCTeX).  It automatically replaces inputs like `\phi`
;; with `φ` and `\in` with `∈`.  These replacements happen inside of
;; math environments or everywhere, depending on the configuration.
;;
;; If you use pdflatex, you probably want to add
;; `\usepackage{unicode-math-mode}` to your tex file in order to make
;; pdflatex aware of the Unicode characters.  Use `M-x
;; latex-unicode-save-sty-file` to put this file somewhere where
;; pdflatex will find it.  If you update this package, you may need to
;; do this again to get the latest sty file.
;;
;;; Code:

(require 'robin)

(defun latex-unicode-math-mode-define-rules (rules)
  (dolist (rule rules)
    (robin-modify-package "math-symbols-tex" (car rule) (cadr rule))))

(defun latex-unicode-math-mode-define-letter-rules (long short base-symbol)
  (dotimes (i 26) ; A-Z
    (let ((symbol (+ base-symbol i))
          (letter (string (+ ?A i))))
      ;; Do not declare unassigned codepoints.
      (when (not (eq (get-char-code-property symbol 'general-category) 'Cn))
        (let ((l (replace-regexp-in-string "!" letter long))
              (s (replace-regexp-in-string "!" letter short)))
          (robin-modify-package "math-symbols-tex" l (string symbol))
          (robin-modify-package "math-symbols-tex" s symbol))))))

(defun latex-unicode-math-mode-update-rules ()
  "(Re-)initialize the robin package."
  (robin-define-package "math-symbols-tex" "Unicode math symbols")
  (latex-unicode-math-mode-define-rules latex-unicode-math-mode-rules-generic)
  (latex-unicode-math-mode-define-rules latex-unicode-math-mode-rules-greek)
  (latex-unicode-math-mode-define-rules latex-unicode-math-mode-rules-arrows)
  (latex-unicode-math-mode-define-rules latex-unicode-math-mode-rules-doublestruck)
  (latex-unicode-math-mode-define-rules latex-unicode-math-mode-rules-extra)
  (dolist (r latex-unicode-math-mode-letter-rules)
    (apply 'latex-unicode-math-mode-define-letter-rules r)))

(defun latex-unicode-math-mode-set-variable (var newvalue)
  "Sets VAR to NEWVALUE and updates the LaTeX unicode math robin package.
Usually called when a customized variable changes."
  (set var newvalue)
  (latex-unicode-math-mode-update-rules))

(defgroup latex-unicode-math nil
  "LaTeX Unicode math symbols"
  :prefix "latex-unicode-math-"
  :group 'tex)

(defcustom latex-unicode-math-mode-letter-rules
  '(("\\mathfrak{!}" "\\!!" ?𝔄)
    ;; Use "MATHEMATICAL BOLD SCRIPT CAPITAL" letters because to me they
    ;; look a lot better than the non-bold versions.
    ("\\mathcal{!}" "\\!!!" ?𝓐))
  "Generate rules for the capital letters A-Z.
In the long/short form, the exclamation mark ! will be replaced
by each of the letters A-Z.  Every entry in this list will
generate 26 rules."
  :type '(repeat (list :tag "Rule pattern"
                  (string :tag "Long form")
                  (string :tag "Short form")
                  (character :tag "Base symbol")))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)

(defcustom latex-unicode-math-mode-rules-generic
  '(
    ;; Invert (see latex-unicode-math-invert-region) only works with
    ;; replacements where the right-hand side is a single letter, not a
    ;; one-letter string.  So we use one-letter strings for one-way
    ;; replacements, where invert wouldn't make sense.
    ("~=" "≠")
    ("!=" "≠")
    ("\\not=" "≠")
    ("\\ne " "≠ ")
    ("\\neq" ?≠)

    ("<=" "≤")
    ("\\le " "≤ ")
    ("\\leq" ?≤)

    (">=" "≥")
    ("\\ge " "≥ ")
    ("\\geq" ?≥)

    ("\\succ" ?≻)
    ("\\prec" ?≺)
    ("\\succeq" ?≽)
    ("\\preceq" ?≼)

    ("\\approx" ?≈)
    ("\\not\\approx" ?≉)
    ("==" "≡")
    ("\\equiv" ?≡)
    ("!==" "≢")
    ("\\not\\equiv" ?≢)
    (":=" "≔")
    ("\\coloneq" ?≔)
    ("\\simeq" ?≃)
    ("\\not\\simeq" ?≄)
    ("\\cong" ?≅)
    ("\\not\\cong" "≇")
    ("\\ncong" ?≇)
    ("\\sim" ?∼)
    ("\\not\\sim" "≁")
    ("\\nsim" ?≁)

    ("\\wedge" ?∧)
    ("\\vee" ?∨)
    ("\\neg" ?¬)
    ("\\forall" ?∀)
    ("\\exists" ?∃)
    ("\\nexists" ?∄)

    ("\\models" ?⊧)
    ("\\nmodels" "⊭")
    ("\\not\\models" ?⊭)
    ("\\top" ?⊤)
    ("\\bot" ?⊥)
    ("\\Diamond" ?◊)
    ("\\Box" ?□)

    ("\\subset" ?⊂)
    ("\\nsubset" ?⊄)
    ("\\subseteq" ?⊆)
    ("\\subsetneq" ?⊊)
    ("\\nsubseteq" ?⊈)
    ("\\supset" ?⊃)
    ("\\supseteq" ?⊇)
    ("\\supsetneq" ?⊋)
    ("\\nsupseteq" ?⊉)
    ("\\setminus" ?∖)
    ("\\cup " "∪ ")
    ("\\cap " "∩ ")
    ("\\in " "∈ ")
    ("\\not\\in" "∉")
    ("\\notin" ?∉)
    ("\\times" ?×)
    ("\\ast" ?∗)
    ("\\sqsubset" ?⊏)
    ("\\sqsubseteq" ?⊑)
    ("\\sqsubsetneq" ?⋤)
    ("\\nsqsubseteq" ?⋢)
    ("\\sqsupset" ?⊐)
    ("\\sqsupseteq" ?⊒)
    ("\\sqsupsetneq" ?⋥)
    ("\\nsqsupseteq" ?⋣)
    ("\\sqcup" ?⊔)
    ("\\sqcap" ?⊓)

    ("\\circ" ?⚬)
    ("\\cdot" ?·)
    ("\\oplus" ?⊕)
    ("\\ominus" ?⊖)
    ("\\otimes" ?⊗)
    ("\\odot" ?⊙)
    ("\\pm" ?±)

    ("\\lfloor" ?⌊)
    ("\\rfloor" ?⌋)
    ("\\lceil" ?⌈)
    ("\\rceil" ?⌉)

    ("\\empty" "∅")
    ("\\emptyset" ?∅)
    ("\\infty" ?∞)
    ("\\partial" ?∂)
    ("\\nabla" ?∇)
    ("\\cdots" ?⋯)
    ("..." "…")
    ("\\ldots" ?…)

    ;; Superscripts conflict with ' in math mode, so we do not use them
    ;; for now.  In particular, I do not know how to handle $X'²$.  With
    ;; \DeclareUnicodeCharacter{00B2}{^2} LaTeX gives a "Double
    ;; superscript" error.  The prime character ' is an active character
    ;; and uses \futurelet trickery to avoid the double superscript error
    ;; in $X'^2$.

    ;; Subscripts don't look good in my font, so we omit them for now.
    )
  "Generic rules for `latex-unicode-math-mode'."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Input")
                       (choice :tag "Output" string character)))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)

(defcustom latex-unicode-math-mode-rules-greek
  '(("\\alpha" ?α)
    ("\\beta" ?β)
    ("\\gamma" ?γ)
    ("\\delta" ?δ)
    ;; Always use \varepsilon.
    ("\\epsilon" "ε")
    ("\\varepsilon" ?ε)
    ("\\zeta" ?ζ)
    ("\\eta" ?η)
    ("\\theta" ?θ)
    ("\\iota" ?ι)
    ("\\kappa" ?κ)
    ("\\lambda" ?λ)
    ("\\mu" ?μ)
    ("\\nu" ?ν)
    ("\\xi" ?ξ)
    ("\\pi" ?π)
    ("\\rho" ?ϱ)
    ("\\sigma" ?σ)
    ("\\tau" ?τ)
    ("\\phi" ?φ)
    ("\\chi" ?χ)
    ("\\psi" ?ψ)
    ("\\omega" ?ω)

    ("\\Gamma" ?Γ)
    ("\\Delta" ?Δ)
    ("\\Theta" ?Θ)
    ("\\Lambda" ?Λ)
    ("\\Xi" ?Ξ)
    ("\\Pi" ?Π)
    ("\\Sigma" ?Σ)
    ("\\Phi" ?Φ)
    ("\\Psi" ?Ψ)
    ("\\Omega" ?Ω))
  "Greek letters for `latex-unicode-math-mode'."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Input")
                       (choice :tag "Output" string character)))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)

(defcustom latex-unicode-math-mode-rules-arrows
  '(("\\gets " "← ")
    ("<-" "←")
    ("\\leftarrow" ?←)

    ("<--" "⟵")
    ("\\longleftarrow" ?⟵)

    ("\\to " "→ ")
    ("->" "→")
    ("\\rightarrow" ?→)

    ("-->" "⟶")
    ("\\longrightarrow" ?⟶)

    ("<->" "↔")
    ("\\leftrightarrow" ?↔)

    ("<-->" "⟷")
    ("\\longleftrightarrow" ?⟷)

    ("=>" "⇒")
    ("\\Rightarrow" ?⇒)

    ("==>" "⟹")
    ("\\Longrightarrow" ?⟹)

    ("<=>" "⇔")
    ("\\Leftrightarrow" ?⇔)

    ("<==>" "⟺")
    ("\\Longleftrightarrow" "⟺")
    ("\\iff" ?⟺)

    ("\\mapsto" ?↦)
    )
  "Arrows for `latex-unicode-math-mode'."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Input")
                       (choice :tag "Output" string character)))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)

(defcustom latex-unicode-math-mode-rules-doublestruck
  '(("\\mathbb{F}" "𝔽")
    ("\\F" ?𝔽)
    ("\\mathbb{N}" "ℕ")
    ("\\N" ?ℕ)
    ("\\mathbb{Q}" "ℚ")
    ("\\Q" ?ℚ)
    ("\\mathbb{R}" "ℝ")
    ("\\R" ?ℝ)
    ("\\mathbb{Z}" "ℤ")
    ("\\Z" ?ℤ))
  "Double struck letters for `latex-unicode-math-mode'."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Input")
                       (choice :tag "Output" string character)))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)

(defcustom latex-unicode-math-mode-rules-extra
  nil
  "Extra rules for `latex-unicode-math-mode'."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Input")
                       (choice :tag "Output" string character)))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)


;; Now that all rules have been declared, initialize the robin
;; package.
(latex-unicode-math-mode-update-rules)

;; robin-invert-region only works with single letter definitions.
;; Some of the replacements in "math-symbols-tex" are strings.  We
;; redefine these here as single letters to make robin-invert-region
;; happy.
(robin-define-package
 "math-symbols-tex-invert-helpers"
 "Unicode math symbols (helpers)"
 ("\\to" ?→)
 ("\\cup" ?∪)
 ("\\cap" ?∩)
 ("\\in" ?∈)
 )

(register-input-method
 "math-symbols-tex" "math" 'robin-use-package "" "Unicode math symbols")
(register-input-method
 "math-symbols-tex-invert-helpers" "math" 'robin-use-package "" "Unicode math symbols (helpers)")


(defun latex-unicode-convert-buffer ()
  "Convert buffer to use Unicode math symbols."
  (interactive "*")
  (save-excursion
    (robin-convert-buffer "math-symbols-tex")))

(defun latex-unicode-invert-buffer ()
  "Convert Unicode in the buffer back to LaTeX macros."
  (interactive "*")
  (save-excursion
    (robin-invert-buffer "math-symbols-tex-invert-helpers")
    (robin-invert-buffer "math-symbols-tex")))

(defun latex-unicode-convert-region (begin end)
  "Convert region from BEGIN to END to use Unicode math symbols."
  (interactive "*r")
  (save-excursion
    (robin-convert-region begin end "math-symbols-tex")))

(defun latex-unicode-invert-region (begin end)
  "Convert Unicode in region from BEGIN to END back to LaTeX macros."
  (interactive "*r")
  (save-excursion
    (robin-invert-region begin end "math-symbols-tex-invert-helpers")
    (robin-invert-region begin end "math-symbols-tex")))

(defconst latex-unicode-sty-file
  (concat (file-name-directory load-file-name) "unicode-math-mode.sty"))
(defun latex-unicode-save-sty-file (dest)
  "Write the sty file declaring the Unicode symbols to DEST.
Required to compile .tex files with Unicode symbols with
pdflatex."
  (interactive "DEnter path to save unicode-math-mode.sty: ")
  (copy-file latex-unicode-sty-file dest)
  (message "Wrote %s" (concat dest (file-name-nondirectory latex-unicode-sty-file))))

(defun latex-unicode-math-set-input-method ()
  "Activate the input method iff point is in a math environment."
  (if (texmathp)
      (activate-input-method 'math-symbols-tex)
    (deactivate-input-method)))

;;;###autoload
(define-minor-mode latex-unicode-math-mode
  "Dynamically enable the Unicode math input method in LaTeX math mode."
  nil "𝓜" nil ;; 𝓜 for Unicode math
  (if latex-unicode-math-mode
      (progn
        ;; This mode is incompatible with latex-unicode-mode.
        (latex-unicode-mode -1)
        (add-hook 'post-command-hook 'latex-unicode-math-set-input-method nil t))
    (progn
      (remove-hook 'post-command-hook 'latex-unicode-math-set-input-method t)
      (when current-input-method
        (deactivate-input-method)))))

;;;###autoload
(define-minor-mode latex-unicode-mode
  "Enable the Unicode math input method everywhere in the buffer."
  nil "𝓤" nil ;; 𝓤 for Unicode
  (if latex-unicode-mode
      (progn
        ;; This mode is incompatible with latex-unicode-math-mode.
        (latex-unicode-math-mode -1)
        (activate-input-method 'math-symbols-tex))
    (when current-input-method
      (deactivate-input-method))))

(provide 'latex-unicode-math-mode)

;;; latex-unicode-math-mode.el ends here
