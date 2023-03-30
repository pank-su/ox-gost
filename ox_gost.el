(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'ox)
(require 'ox-publish)
(require 'ox-latex)


;; Создаём потомка экспорта ox-latex
(org-export-define-derived-backend 'ox-gost 'latex
  ;; Объявляем меню
  :menu-entry '(?g "Export to GOST 7.32" org-gost-export-to-pdf)
  :filters-alist '(:filter-table . gost-filter-table)
  ;; TODO: NOT WORKED
  :options-alist '((:latex-classes nil nil org-gost-classes nil))
  )

(defun gost-filter-table (text backend info)
  (replace-regexp-in-string "Continued on next page" "Продолжение на следующей странице" (replace-regexp-in-string "Continued from previous page" "Продолжение с предыдущей страницы" text))
      (text))

;; Просто экспортируем файл в pdf
(defun org-gost-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to GOST 7.32 file"
  (interactive)
    (let ((outfile (org-export-output-file-name ".tex" subtreep)))
      (org-export-to-file 'latex outfile
        async subtreep visible-only body-only ext-plist
        #'org-latex-compile)))

(setq org-gost-classes
  '(("extarticle" "\\documentclass[14pt]{extarticle}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection{%s}")
	  ("\\paragraph{%s}" . "\\paragraph*{%s}")
	  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	 ("article" "\\documentclass[11pt]{article}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	  ("\\paragraph{%s}" . "\\paragraph*{%s}")
	  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	 ("report" "\\documentclass[11pt]{report}"
	  ("\\part{%s}" . "\\part*{%s}")
	  ("\\chapter{%s}" . "\\chapter*{%s}")
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	 ("book" "\\documentclass[11pt]{book}"
	  ("\\part{%s}" . "\\part*{%s}")
	  ("\\chapter{%s}" . "\\chapter*{%s}")
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq org-gost-title-command (concat
			       "\\begin{titlepage}\n\n"
			       "\\centering{ГУАП}\n\n"
			       "\\vspace{32pt}\n\n"
			       "\\centering{ФАКУЛЬТЕТ СРЕДНЕГО ПРОФЕССИОНАЛЬНОГО ОБРАЗОВАНИЯ}\n\n"
			       "\\vspace{60pt}\n\n"
			       "\\raggedright{ОТЧЕТ \\\\
ЗАЩИЩЕН С ОЦЕНКОЙ}\n"
			       "\\vspace{14pt}\n\n"
			       "\\raggedright{ПРЕПОДАВАТЕЛЬ}\n\n"
			       "\\vspace{12pt}\n\n"
			       "\\begin{tabularx}{\\textwidth}{ >{\\centering\\arraybackslash}X >{\\centering\\arraybackslash}X >{\\centering\\arraybackslash}X }\n"
			       "\t преподаватель & & %d \\\\ \n"
			       "\t \\hrulefill & \\hrulefill & \\hrulefill \\\\ \n"
			       "\\footnotesize{должность, уч. степень, звание} & \\footnotesize{подпись, дата} & \\footnotesize{инициалы, фамилия} \\\\ \n"
			       "\\end{tabularx} \n \n"
			       "\\vspace{48pt} \n\n"
			       "\\centering{ОТЧЕТЫ О ЛАБОРАТОРНЫХ РАБОТАХ} \n\n"
			       "\\vspace{76pt} \n\n"
			       "\\centering{По дисциплине: %t} \n\n"
			       "\\vspace*{\\fill} \n\n"
			       "\\raggedright{РАБОТУ ВЫПОЛНИЛ} \n\n"
			       "\\vspace{10pt} \n\n"
			       "\\begin{tabularx}{\\textwidth}{>{\\raggedright\\arraybackslash}X  >{\\centering\\arraybackslash}X >{\\centering\\arraybackslash}X >{\\centering\\arraybackslash}X }\n"
			       "\t СТУДЕНТ ГР. № & 021к & & %a \\\\ \n"
			       "\t & \\hrulefill & \\hrulefill & \\hrulefill \\\\ \n"
			       "\t &  & \\footnotesize{подпись, дата} & \\footnotesize{инициалы, фамилия} \\\\ \n"
			       "\\end{tabularx} \n \n"
			       "\\vspace*{\\fill} \n\n"
			       "\\centering{Санкт-Петербург \\the\\year} \n\n"
			       "\\end{titlepage}\n"
			       ))
  


(setq org-gost-toc-command "\n\\tableofcontents \\clearpage\n")

;; (defcustom org-gost-listings 'minted)
