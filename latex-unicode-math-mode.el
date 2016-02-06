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

(robin-define-package
 "math-symbols-tex"
 "Unicode math symbols"

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

 ("\\gets " "← ")
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
 ("\\models" ?⊧)
 ("\\nmodels" "⊭")
 ("\\not\\models" ?⊭)
 ("\\top" ?⊤)
 ("\\bot" ?⊥)
 ("\\Diamond" ?◊)
 ("\\Box" ?□)

 ("\\alpha" ?α)
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
 ("\\Omega" ?Ω)

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

 ("\\mathbb{F}" "𝔽")
 ("\\F" ?𝔽)
 ("\\mathbb{N}" "ℕ")
 ("\\N" ?ℕ)
 ("\\mathbb{Q}" "ℚ")
 ("\\Q" ?ℚ)
 ("\\mathbb{R}" "ℝ")
 ("\\R" ?ℝ)
 ("\\mathbb{Z}" "ℤ")
 ("\\Z" ?ℤ)

 ;; "MATHEMATICAL FRAKTUR CAPITAL" letters
 ("\\mathfrak{A}" "𝔄")
 ("\\AA" ?𝔄)
 ("\\mathfrak{B}" "𝔅")
 ("\\BB" ?𝔅)
 ("\\mathfrak{D}" "𝔇")
 ("\\DD" ?𝔇)
 ("\\mathfrak{E}" "𝔈")
 ("\\EE" ?𝔈)
 ("\\mathfrak{F}" "𝔉")
 ("\\FF" ?𝔉)
 ("\\mathfrak{G}" "𝔊")
 ("\\GG" ?𝔊)
 ("\\mathfrak{J}" "𝔍")
 ("\\JJ" ?𝔍)
 ("\\mathfrak{K}" "𝔎")
 ("\\KK" ?𝔎)
 ("\\mathfrak{L}" "𝔏")
 ("\\LL" ?𝔏)
 ("\\mathfrak{M}" "𝔐")
 ("\\MM" ?𝔐)
 ("\\mathfrak{N}" "𝔑")
 ("\\NN" ?𝔑)
 ("\\mathfrak{O}" "𝔒")
 ("\\OO" ?𝔒)
 ("\\mathfrak{P}" "𝔓")
 ("\\PP" ?𝔓)
 ("\\mathfrak{Q}" "𝔔")
 ("\\QQ" ?𝔔)
 ("\\mathfrak{S}" "𝔖")
 ("\\SS" ?𝔖)
 ("\\mathfrak{T}" "𝔗")
 ("\\TT" ?𝔗)
 ("\\mathfrak{U}" "𝔘")
 ("\\UU" ?𝔘)
 ("\\mathfrak{V}" "𝔙")
 ("\\VV" ?𝔙)
 ("\\mathfrak{W}" "𝔚")
 ("\\WW" ?𝔚)
 ("\\mathfrak{X}" "𝔛")
 ("\\XX" ?𝔛)
 ("\\mathfrak{Y}" "𝔜")
 ("\\YY" ?𝔜)

 ;; The following are "MATHEMATICAL BOLD SCRIPT CAPITAL" letters
 ;; because to me they look a lot better than the non-bold versions.
 ("\\mathcal{A}" "𝓐")
 ("\\AAA" ?𝓐)
 ("\\mathcal{B}" "𝓑")
 ("\\BBB" ?𝓑)
 ("\\mathcal{C}" "𝓒")
 ("\\CCC" ?𝓒)
 ("\\mathcal{D}" "𝓓")
 ("\\DDD" ?𝓓)
 ("\\mathcal{E}" "𝓔")
 ("\\EEE" ?𝓔)
 ("\\mathcal{F}" "𝓕")
 ("\\FFF" ?𝓕)
 ("\\mathcal{G}" "𝓖")
 ("\\GGG" ?𝓖)
 ("\\mathcal{H}" "𝓗")
 ("\\HHH" ?𝓗)
 ("\\mathcal{I}" "𝓘")
 ("\\III" ?𝓘)
 ("\\mathcal{J}" "𝓙")
 ("\\JJJ" ?𝓙)
 ("\\mathcal{K}" "𝓚")
 ("\\KKK" ?𝓚)
 ("\\mathcal{L}" "𝓛")
 ("\\LLL" ?𝓛)
 ("\\mathcal{M}" "𝓜")
 ("\\MMM" ?𝓜)
 ("\\mathcal{N}" "𝓝")
 ("\\NNN" ?𝓝)
 ("\\mathcal{O}" "𝓞")
 ("\\OOO" ?𝓞)
 ("\\mathcal{P}" "𝓟")
 ("\\PPP" ?𝓟)
 ("\\mathcal{Q}" "𝓠")
 ("\\QQQ" ?𝓠)
 ("\\mathcal{R}" "𝓡")
 ("\\RRR" ?𝓡)
 ("\\mathcal{S}" "𝓢")
 ("\\SSS" ?𝓢)
 ("\\mathcal{T}" "𝓣")
 ("\\TTT" ?𝓣)
 ("\\mathcal{U}" "𝓤")
 ("\\UUU" ?𝓤)
 ("\\mathcal{V}" "𝓥")
 ("\\VVV" ?𝓥)
 ("\\mathcal{W}" "𝓦")
 ("\\WWW" ?𝓦)
 ("\\mathcal{X}" "𝓧")
 ("\\XXX" ?𝓧)
 ("\\mathcal{Y}" "𝓨")
 ("\\YYY" ?𝓨)
 ("\\mathcal{Z}" "𝓩")
 ("\\ZZZ" ?𝓩)

 ;; Superscripts conflict with ' in math mode, so we do not use them
 ;; for now.  In particular, I do not know how to handle $X'²$.  With
 ;; \DeclareUnicodeCharacter{00B2}{^2} LaTeX gives a "Double
 ;; superscript" error.  The prime character ' is an active character
 ;; and uses \futurelet trickery to avoid the double superscript error
 ;; in $X'^2$.

 ;; Subscripts don't look good in my font, so we omit them for now.
 )

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
  (robin-convert-buffer "math-symbols-tex"))

(defun latex-unicode-invert-buffer ()
  "Convert Unicode in the buffer back to LaTeX macros."
  (interactive "*")
  (robin-invert-buffer "math-symbols-tex-invert-helpers")
  (robin-invert-buffer "math-symbols-tex"))

(defun latex-unicode-convert-region (begin end)
  "Convert region from BEGIN to END to use Unicode math symbols."
  (interactive "*r")
  (robin-convert-region begin end "math-symbols-tex"))

(defun latex-unicode-invert-region (begin end)
  "Convert Unicode in region from BEGIN to END back to LaTeX macros."
  (interactive "*r")
  (robin-invert-region begin end "math-symbols-tex-invert-helpers")
  (robin-invert-region begin end "math-symbols-tex"))

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
