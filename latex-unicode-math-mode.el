;;; latex-unicode-math-mode.el --- Input method for Unicode math symbols -*- Coding: utf-8 -*-

;; Copyright 2016-2017 Christoph Dittmann
;;
;; Author: Christoph Dittmann <github@christoph-d.de>
;; URL: https://github.com/Christoph-D/latex-unicode-math-mode

;;; Commentary:
;; An Emacs minor mode for entering Unicode math symbols in
;; LaTeX-mode, with a sty file to make pdflatex Unicode-aware.  This
;; minor mode automatically replaces inputs like `\phi` with `ϕ` and
;; `\in` with `∈`.  These replacements happen inside of math
;; environments or everywhere, depending on the configuration.
;;
;; If you use pdflatex, you can add `\usepackage{unicode-math-mode}`
;; to your tex file in order to make pdflatex aware of the Unicode
;; characters.  Use `M-x latex-unicode-save-sty-file` to put this file
;; somewhere where pdflatex will find it.
;;
;; If you update this package, you may need to do this again to get
;; the latest sty file.
;;
;;; Code:

(require 'robin)

(defgroup latex-unicode-math nil
  "LaTeX Unicode math symbols
Invert (see `latex-unicode-math-invert-region') only works with
replacements where the right-hand side is a single letter, not a
one-letter string.  So it is recommended to use one-letter
strings instead of characters for one-way replacements, where
invert wouldn't make sense."
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
    ("~=" "≠")
    ("!=" "≠")
    ("\\not=" "≠")
    ("\\ne " "≠ ")

    ("<=" "≤")
    ("\\le " "≤ ")

    (">=" "≥")
    ("\\ge " "≥ ")

    ("\\not\\approx" ?≉)
    ("==" "≡")
    ("!==" "≢")
    ("\\nequiv" "≢")
    ("\\not\\equiv" ?≢)
    (":=" "≔")
    ("\\nsimeq" "≄")
    ("\\not\\simeq" ?≄)
    ("\\not\\cong" "≇")
    ("\\not\\sim" "≁")

    ("\\nmodels" "⊭")
    ("\\not\\models" ?⊭)

    ("\\cup " "∪ ")
    ("\\cap " "∩ ")
    ("\\in " "∈ ")
    ("\\ni " "∋ ")
    ("\\not\\in" "∉")

    ("||" "‖")

    ("[|" "⟦")
    ("\\llbracket" ?⟦)
    ("|]" "⟧")
    ("\\rrbracket" ?⟧)

    ("\\empty" "∅")
    ("..." "…")
    )
  "Generic rules for `latex-unicode-math-mode'.
Superscripts are not declared because they conflict with ' in
math mode.  In particular, I do not know how to handle $X'²$.
With \\DeclareUnicodeCharacter{00B2}{^2} LaTeX gives a \"Double
superscript\" error.  The prime character ' is an active
character and uses \\futurelet trickery to avoid the double
superscript error in $X'^2$.

Subscripts don't look good in my font, so we omit them for now."
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
    ("\\phi" ?ϕ)
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
  "Greek letters for `latex-unicode-math-mode'.
The default is to use \\varepsilon as the target for ε for
`latex-unicode-math-invert-buffer' (by declaring it as a single
character and not as a string)."
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
    ("\\mapsto" ?↦))
  "Arrows for `latex-unicode-math-mode'."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Input")
                       (choice :tag "Output" string character)))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)

(defcustom latex-unicode-math-mode-rules-doublestruck
  '(("\\mathbb{F}" "𝔽")
    ("\\IF" ?𝔽)
    ("\\mathbb{N}" "ℕ")
    ("\\IN" ?ℕ)
    ("\\mathbb{Q}" "ℚ")
    ("\\IQ" ?ℚ)
    ("\\mathbb{R}" "ℝ")
    ("\\IR" ?ℝ)
    ("\\mathbb{Z}" "ℤ")
    ("\\IZ" ?ℤ))
  "Double struck letters for `latex-unicode-math-mode'."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Input")
                       (choice :tag "Output" string character)))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)

(defcustom latex-unicode-math-mode-rules-emacs
'(
    ;; More math symbols, taken from lisp/textmodes/tex-mode.el from
    ;; emacs25, minus a few symbols that would collide with others.
    ("\\Box" ?□)
    ("\\Bumpeq" ?≎)
    ("\\Cap" ?⋒)
    ("\\Cup" ?⋓)
    ("\\Diamond" ?◊)
    ("\\Downarrow" ?⇓)
    ("\\H{o}" ?ő)
    ("\\Im" ?ℑ)
    ("\\Join" ?⋈)
    ("\\Ll" ?⋘)
    ("\\Lleftarrow" ?⇚)
    ("\\Lsh" ?↰)
    ("\\Re" ?ℜ)
    ("\\Rrightarrow" ?⇛)
    ("\\Rsh" ?↱)
    ("\\Subset" ?⋐)
    ("\\Supset" ?⋑)
    ("\\Uparrow" ?⇑)
    ("\\Updownarrow" ?⇕)
    ("\\Vdash" ?⊩)
    ("\\Vert" ?‖)
    ("\\Vvdash" ?⊪)
    ("\\aleph" ?ℵ)
    ("\\amalg" ?∐)
    ("\\angle" ?∠)
    ("\\approx" ?≈)
    ("\\approxeq" ?≊)
    ("\\ast" ?∗)
    ("\\asymp" ?≍)
    ("\\backcong" ?≌)
    ("\\backepsilon" ?∍)
    ("\\backprime" ?‵)
    ("\\backsim" ?∽)
    ("\\backsimeq" ?⋍)
    ("\\barwedge" ?⊼)
    ("\\because" ?∵)
    ("\\beth" ?ℶ)
    ("\\between" ?≬)
    ("\\bigcap" ?⋂)
    ("\\bigcirc" ?◯)
    ("\\bigcup" ?⋃)
    ("\\bigstar" ?★)
    ("\\bigtriangledown" ?▽)
    ("\\bigtriangleup" ?△)
    ("\\bigvee" ?⋁)
    ("\\bigwedge" ?⋀)
    ("\\blacklozenge" ?✦)
    ("\\blacksquare" ?▪)
    ("\\blacktriangle" ?▴)
    ("\\blacktriangledown" ?▾)
    ("\\blacktriangleleft" ?◂)
    ("\\blacktriangleright" ?▸)
    ("\\bot" ?⊥)
    ("\\bowtie" "⋈")
    ("\\boxminus" ?⊟)
    ("\\boxplus" ?⊞)
    ("\\boxtimes" ?⊠)
    ("\\bullet" ?•)
    ("\\bumpeq" ?≏)
    ("\\cdots" ?⋯)
    ("\\centerdot" ?·)
    ("\\checkmark" ?✓)
    ("\\cdot" ?⋅)
    ("\\circ" ?∘)
    ("\\circeq" ?≗)
    ("\\circlearrowleft" ?↺)
    ("\\circlearrowright" ?↻)
    ("\\circledR" ?®)
    ("\\circledS" ?Ⓢ)
    ("\\circledast" ?⊛)
    ("\\circledcirc" ?⊚)
    ("\\circleddash" ?⊝)
    ("\\clubsuit" ?♣)
    ("\\coloneq" ?≔)
    ("\\complement" ?∁)
    ("\\cong" ?≅)
    ("\\coprod" "∐")
    ("\\curlyeqprec" ?⋞)
    ("\\curlyeqsucc" ?⋟)
    ("\\curlypreceq" "≼")
    ("\\curlyvee" ?⋎)
    ("\\curlywedge" ?⋏)
    ("\\curvearrowleft" ?↶)
    ("\\curvearrowright" ?↷)
    ("\\dag" ?†)
    ("\\daleth" ?ℸ)
    ("\\dashv" ?⊣)
    ("\\ddag" ?‡)
    ("\\ddots" ?⋱)
    ("\\diamond" ?⋄)
    ("\\diamondsuit" ?♢)
    ("\\divideontimes" ?⋇)
    ("\\doteq" ?≐)
    ("\\doteqdot" ?≑)
    ("\\dotplus" ?∔)
    ("\\dotsquare" ?⊡)
    ("\\downarrow" ?↓)
    ("\\downdownarrows" ?⇊)
    ("\\downleftharpoon" ?⇃)
    ("\\downrightharpoon" ?⇂)
    ("\\ell" ?ℓ)
    ("\\emptyset" ?∅)
    ("\\eqcirc" ?≖)
    ("\\eqcolon" ?≕)
    ("\\eqslantgtr" ?⋝)
    ("\\eqslantless" ?⋜)
    ("\\equiv" ?≡)
    ("\\exists" ?∃)
    ("\\fallingdotseq" ?≒)
    ("\\flat" ?♭)
    ("\\forall" ?∀)
    ("\\frown" ?⌢)
    ("\\geq" ?≥)
    ("\\geqq" ?≧)
    ("\\gg" ?≫)
    ("\\ggg" ?⋙)
    ("\\gimel" ?ℷ)
    ("\\gnapprox" "⋧")
    ("\\gneq" ?≩)
    ("\\gnsim" ?⋧)
    ("\\gtrdot" ?⋗)
    ("\\gtreqless" ?⋛)
    ("\\gtrless" ?≷)
    ("\\gtrsim" ?≳)
    ("\\gvertneqq" "≩")
    ("\\hbar" ?ℏ)
    ("\\heartsuit" ?♥)
    ("\\hookleftarrow" ?↩)
    ("\\hookrightarrow" ?↪)
    ("\\imath" ?ı)
    ("\\infty" ?∞)
    ("\\int" ?∫)
    ("\\intercal" ?⊺)
    ("\\langle" 10216)          ; Literal ?⟨ breaks indentation.
    ("\\lceil" ?⌈)
    ("\\ldots" ?…)
    ("\\leadsto" ?↝)
    ("\\leftarrowtail" ?↢)
    ("\\leftharpoondown" ?↽)
    ("\\leftharpoonup" ?↼)
    ("\\leftleftarrows" ?⇇)
    ;; ("\\leftparengtr" ?〈), see bug#12948.
    ("\\leftrightarrows" ?⇆)
    ("\\leftrightharpoons" ?⇋)
    ("\\leftrightsquigarrow" ?↭)
    ("\\leftthreetimes" ?⋋)
    ("\\leq" ?≤)
    ("\\leqq" ?≦)
    ("\\lessapprox" "≲")
    ("\\lessdot" ?⋖)
    ("\\lesseqgtr" ?⋚)
    ("\\lessgtr" ?≶)
    ("\\lesssim" ?≲)
    ("\\lfloor" ?⌊)
    ("\\lhd" ?◁)
    ("\\rhd" ?▷)
    ("\\ll" ?≪)
    ("\\llcorner" ?⌞)
    ("\\lnapprox" "⋦")
    ("\\lneq" ?≨)
    ("\\lnsim" ?⋦)
    ("\\looparrowleft" ?↫)
    ("\\looparrowright" ?↬)
    ("\\lozenge" ?✧)
    ("\\lq" ?‘)
    ("\\lrcorner" ?⌟)
    ("\\ltimes" ?⋉)
    ("\\lvertneqq" "≨")
    ("\\maltese" ?✠)
    ("\\measuredangle" ?∡)
    ("\\mho" ?℧)
    ("\\models" ?⊧)
    ("\\mp" ?∓)
    ("\\multimap" ?⊸)
    ("\\nLeftarrow" ?⇍)
    ("\\nLeftrightarrow" ?⇎)
    ("\\nRightarrow" ?⇏)
    ("\\nVDash" ?⊯)
    ("\\nVdash" ?⊮)
    ("\\nabla" ?∇)
    ("\\napprox" ?≉)
    ("\\natural" ?♮)
    ("\\ncong" ?≇)
    ("\\nearrow" ?↗)
    ("\\neg" ?¬)
    ("\\neq" ?≠)
    ("\\nexists" ?∄)
    ("\\ngeq" ?≱)
    ("\\ngeqq" "≱")
    ("\\ngeqslant" "≱")
    ("\\ngtr" ?≯)
    ("\\nleftarrow" ?↚)
    ("\\nleftrightarrow" ?↮)
    ("\\nleq" ?≰)
    ("\\nleqq" "≰")
    ("\\nleqslant" "≰")
    ("\\nless" ?≮)
    ("\\nmid" ?∤)
    ;; ("\\not" ?̸)              ;FIXME: conflict with "NOT SIGN" ¬.
    ("\\notin" ?∉)
    ("\\nparallel" ?∦)
    ("\\nprec" ?⊀)
    ("\\npreceq" ?⋠)
    ("\\nrightarrow" ?↛)
    ("\\nsim" ?≁)
    ("\\nsubset" ?⊄)
    ("\\nsubseteq" ?⊈)
    ("\\nsucc" ?⊁)
    ("\\nsucceq" ?⋡)
    ("\\nsupset" ?⊅)
    ("\\nsupseteq" ?⊉)
    ("\\ntriangleleft" ?⋪)
    ("\\ntrianglelefteq" ?⋬)
    ("\\ntriangleright" ?⋫)
    ("\\ntrianglerighteq" ?⋭)
    ("\\nvDash" ?⊭)
    ("\\nvdash" ?⊬)
    ("\\nwarrow" ?↖)
    ("\\odot" ?⊙)
    ("\\oint" ?∮)
    ("\\ominus" ?⊖)
    ("\\oplus" ?⊕)
    ("\\oslash" ?⊘)
    ("\\otimes" ?⊗)
    ("\\parallel" ?∥)
    ("\\partial" ?∂)
    ("\\perp" "⊥")
    ("\\pitchfork" ?⋔)
    ("\\prec" ?≺)
    ("\\precapprox" "≾")
    ("\\preceq" ?≼)
    ("\\precnapprox" "⋨")
    ("\\precnsim" ?⋨)
    ("\\precsim" ?≾)
    ("\\prod" ?∏)
    ("\\propto" ?∝)
    ("\\rangle" 10217)            ; Literal ?⟩ breaks indentation.
    ("\\rceil" ?⌉)
    ("\\rfloor" ?⌋)
    ("\\rightarrowtail" ?↣)
    ("\\rightharpoondown" ?⇁)
    ("\\rightharpoonup" ?⇀)
    ("\\rightleftarrows" ?⇄)
    ("\\rightleftharpoons" ?⇌)
    ;; ("\\rightparengtr" ?⦔) ;; Was ?〉, see bug#12948.
    ("\\rightrightarrows" ?⇉)
    ("\\rightthreetimes" ?⋌)
    ("\\risingdotseq" ?≓)
    ("\\rtimes" ?⋊)
    ("\\times" ?×)
    ("\\sbs" ?﹨)
    ("\\searrow" ?↘)
    ("\\setminus" ?∖)
    ("\\sharp" ?♯)
    ("\\sim" ?∼)
    ("\\simeq" ?≃)
    ("\\smile" ?⌣)
    ("\\spadesuit" ?♠)
    ("\\sphericalangle" ?∢)
    ("\\sqcap" ?⊓)
    ("\\sqcup" ?⊔)
    ("\\sqsubset" ?⊏)
    ("\\sqsubseteq" ?⊑)
    ("\\sqsupset" ?⊐)
    ("\\sqsupseteq" ?⊒)
    ("\\square" ?□)
    ("\\squigarrowright" ?⇝)
    ("\\star" ?⋆)
    ("\\straightphi" ?φ)
    ("\\subset" ?⊂)
    ("\\subseteq" ?⊆)
    ("\\subsetneq" ?⊊)
    ("\\succ" ?≻)
    ("\\succapprox" "≿")
    ("\\succcurlyeq" "≽")
    ("\\succeq" ?≽)
    ("\\succnapprox" "⋩")
    ("\\succnsim" ?⋩)
    ("\\succsim" ?≿)
    ("\\sum" ?∑)
    ("\\supset" ?⊃)
    ("\\supseteq" ?⊇)
    ("\\supsetneq" ?⊋)
    ("\\surd" ?√)
    ("\\swarrow" ?↙)
    ("\\therefore" ?∴)
    ("\\top" ?⊤)
    ("\\triangle" ?▵)
    ("\\triangledown" ?▿)
    ("\\triangleleft" ?◃)
    ("\\trianglelefteq" ?⊴)
    ("\\triangleq" ?≜)
    ("\\triangleright" ?▹)
    ("\\trianglerighteq" ?⊵)
    ("\\twoheadleftarrow" ?↞)
    ("\\twoheadrightarrow" ?↠)
    ("\\ulcorner" ?⌜)
    ("\\uparrow" ?↑)
    ("\\updownarrow" ?↕)
    ("\\upleftharpoon" ?↿)
    ("\\uplus" ?⊎)
    ("\\uprightharpoon" ?↾)
    ("\\upuparrows" ?⇈)
    ("\\urcorner" ?⌝)
    ("\\u{i}" ?ĭ)
    ("\\vDash" ?⊨)
    ("\\varepsilon" ?ε)
    ("\\varphi" ?φ)
    ("\\varrho" ?ϱ)
    ("\\varsigma" ?ς)
    ("\\vartriangleleft" ?⊲)
    ("\\vartriangleright" ?⊳)
    ("\\vdash" ?⊢)
    ("\\vdots" ?⋮)
    ("\\vee" ?∨)
    ("\\veebar" ?⊻)
    ("\\wedge" ?∧)
    ("\\wp" ?℘)
    ("\\wr" ?≀)
    ("\\ordfeminine" ?ª)
    ("\\ordmasculine" ?º)
    ("\\lambdabar" ?ƛ)
    ("\\celsius" ?℃))
  "Rules for `latex-unicode-math-mode' taken from `tex-mode.el'."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Input")
                       (choice :tag "Output" string character)))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)

(defcustom latex-unicode-math-mode-rules-extra
  nil
  "Extra rules for `latex-unicode-math-mode'.
Please add your own rules here."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Input")
                       (choice :tag "Output" string character)))
  :group 'latex-unicode-math
  :set 'latex-unicode-math-mode-set-variable
  :initialize 'custom-initialize-default)


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
  (latex-unicode-math-mode-define-rules latex-unicode-math-mode-rules-emacs)
  (latex-unicode-math-mode-define-rules latex-unicode-math-mode-rules-extra)
  (dolist (r latex-unicode-math-mode-letter-rules)
    (apply 'latex-unicode-math-mode-define-letter-rules r)))

(defun latex-unicode-math-mode-set-variable (var newvalue)
  "Sets VAR to NEWVALUE and updates the LaTeX unicode math robin package.
Usually called when a customized variable changes."
  (set var newvalue)
  (latex-unicode-math-mode-update-rules))

;; Now that all rules and initialization methods have been declared,
;; we create the main robin package.
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
  (copy-file latex-unicode-sty-file dest 1) ; ask user about overwriting
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
