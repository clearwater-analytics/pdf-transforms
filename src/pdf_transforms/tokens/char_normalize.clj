(ns pdf-transforms.tokens.char-normalize)

;;;; Unicode to character mappings
(def char-mappings {
                    "\u2018" "'"
                    "\u2019" "'"
                    "\u201c" "\""
                    "\u201d" "\""
                    "\u201a" "*"  ;bullet point
                    "\u2022" "*"  ;bullet point
                    "\u00cf" "."
                    "\u00c0" "."
                    "\u00cc" "-"
                    "\u2014" "-"
                    "\u0002" "-"
                    "\u00d0" "-"
                    "\u00b0" "+"
                    "\u00d1" "fi"
                    "\ufb01" "fi"
                    "\ufb00" "ff"
                    "\ufb03" "ff"
                    "\u00c5" "ff"
                    "\ufb02" "fl"
                    "\ufffd" "."
                    "\u00bf" "-"
                    "\u00a3" "x"
                    "\u00a0" " "
                    "\u2008" " "
                    "\u2009" " "
                    "\u200A" " "
                    "\u200B" " "
                    "\u0000" ""})

(defn replace-unicode-chars [string]
  (or (char-mappings string) string))
