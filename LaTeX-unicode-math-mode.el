;; Define an input method for unicode math symbols.
(robin-define-package
 "math-symbols-tex"
 "Unicode math symbols"
 ("~=" "≠")
 ("!=" "≠")
 ("\\ne " "≠ ")
 ("\\neq" ?≠)

 ("<=" "≤")
 ("\\le " "≤ ")
 ("\\leq" ?≤)

 (">=" "≥")
 ("\\ge " "≥ ")
 ("\\geq" ?≥)

 ("\\wedge" ?∧)
 ("\\vee" ?∨)
 ("\\neg" ?¬)
 ("\\forall" ?∀)
 ("\\exists" ?∃)
 ("\\nexists" ?∄)
 ("\\rightarrow" ?→)
 ("\\to " "→ ")
 ("-> " "→")
 ("\\leftrightarrow" ?↔)
 ("=>" "⇒")
 ("\\Rightarrow" ?⇒)
 ("<=>" "⇔")
 ("\\Leftrightarrow" ?⇔)
 ("\\iff" ?⇔)
 ("\\equiv" ?≡)
 ("\\mapsto" ?↦)
 ("\\models" ?⊧)
 ("\\nmodels" ?⊭)
 ("\\top" ?⊤)
 ("\\bot" ?⊥)
 ("\\Diamond" ?◊)
 ("\\Box" ?□)

 ("\\alpha" ?α)
 ("\\beta" ?β)
 ("\\delta" ?δ)
 ("\\gamma" ?γ)
 ("\\phi" ?φ)
 ("\\psi" ?ψ)
 ("\\chi" ?χ)
 ("\\rho" ?ϱ)
 ("\\sigma" ?σ)
 ("\\pi" ?π)
 ("\\mu" ?μ)
 ("\\nu" ?ν)
 ("\\omega" ?ω)

 ("\\Delta" ?Δ)
 ("\\Gamma" ?Γ)
 ("\\Sigma" ?Σ)
 ("\\Phi" ?Φ)
 ("\\Psi" ?Ψ)
 ("\\Pi" ?Π)
 ("\\Omega" ?Ω)

 ("\\subseteq" ?⊆)
 ("\\subsetneq" ?⊊)
 ("\\nsubseteq" ?⊈)
 ("\\setminus" ?∖)
 ("\\cup " "∪ ")
 ("\\cap " "∩ ")
 ("\\in " "∈ ")
 ("\\notin" ?∉)
 ("\\times" ?×)

 ("\\circ" ?⚬)
 ("\\cdot" ?·)
 ("\\cdots" "\\cdots")
 ("\\oplus" ?⊕)
 ("\\ominus" ?⊖)
 ("\\otimes" ?⊗)
 ("\\odot" ?⊙)

 ("\\lfloor" ?⌊)
 ("\\rfloor" ?⌋)
 ("\\lceil" ?⌈)
 ("\\rceil" ?⌉)

 ("\\empty" ?∅)
 ("\\infty" ?∞)

 ("\\N" ?ℕ)
 ("\\Q" ?ℚ)
 ("\\R" ?ℝ)
 ("\\Z" ?ℤ)

 ("\\AAA" ?𝓐)
 ("\\BBB" ?𝓑)
 ("\\CCC" ?𝓒)
 ("\\DDD" ?𝓓)
 ("\\GGG" ?𝓖)
 ("\\HHH" ?𝓗)
 ("\\III" ?𝓘)
 ("\\JJJ" ?𝓙)
 ("\\LLL" ?𝓛)
 ("\\MMM" ?𝓜)
 ("\\NNN" ?𝓝)
 ("\\PPP" ?𝓟)

 ("^2" ?²)
 ("^3" ?³)
 ("^i" ?ⁱ)
 ("^j" ?ʲ)
 ("^k" ?ᵏ)
 ("^l" ?ˡ)
 ("^m" ?ᵐ)
 ("^n" ?ⁿ)

 ;; Subscripts don't look good, so we omit them for now.
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


(defun LaTeX-unicode-math-convert-buffer ()
  "Convert buffer to use unicode math symbols."
  (interactive "*")
  (robin-convert-buffer "math-symbols-tex"))

(defun LaTeX-unicode-math-invert-buffer ()
  "Convert all unicode math symbols in the buffer back to LaTeX macros."
  (interactive "*")
  (robin-invert-buffer "math-symbols-tex-invert-helpers")
  (robin-invert-buffer "math-symbols-tex"))

(defun LaTeX-unicode-math-convert-region (begin end)
  "Convert REGION to use unicode math symbols."
  (interactive "*r")
  (robin-convert-region begin end "math-symbols-tex"))

(defun LaTeX-unicode-math-invert-region (begin end)
  "Convert all unicode math symbols in REGION back to LaTeX macros."
  (interactive "*r")
  (robin-invert-region begin end "math-symbols-tex-invert-helpers")
  (robin-invert-region begin end "math-symbols-tex"))


(defun LaTeX-unicode-math-set-input-method ()
  (if (texmathp) ;; If the point is inside a math environment
      (activate-input-method 'math-symbols-tex)
    (inactivate-input-method)))

(define-minor-mode LaTeX-unicode-math-mode
  "Dynamically enable the unicode math input method in LaTeX math mode."
  nil "𝓜" nil
  (if LaTeX-unicode-math-mode
      (progn
        ;; This mode is incompatible with LaTeX-unicode-global-mode.
        (LaTeX-unicode-global-mode -1)
        (add-hook 'post-command-hook 'LaTeX-unicode-math-set-input-method nil t))
    (progn
      (remove-hook 'post-command-hook 'LaTeX-unicode-math-set-input-method t)
      (when current-input-method
        (deactivate-input-method)))))

(define-minor-mode LaTeX-unicode-global-mode
  "Enable the unicode math input method everywhere in the buffer."
  nil "𝓖" nil
  (if LaTeX-unicode-global-mode
      (progn
        ;; This mode is incompatible with LaTeX-unicode-math-mode.
        (LaTeX-unicode-math-mode -1)
        (activate-input-method 'math-symbols-tex))
    (when current-input-method
      (deactivate-input-method))))

(provide 'LaTeX-unicode-math-mode)
