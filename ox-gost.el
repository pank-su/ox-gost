(require 'org-macs)

(require 'cl-lib)
(require 'ox)
(require 'ox-publish)
(require 'ox-latex)
(require 'engrave-faces)

;; Создаём потомка экспорта ox-latex
(org-export-define-derived-backend 'gost 'latex
  ;; Объявляем меню
  :menu-entry '(?g "Export to GOST 7.32" org-gost-export-to-pdf)
  :filters-alist '((:filter-table . gost-filter-table))
  :options-alist '(
		   (:latex-classes nil nil org-gost-classes)
		   (:latex-toc-command nil nil org-gost-toc-command)
		   (:latex-title-command nil nil org-gost-title-command)
		   (:latex-class nil nil "extarticle")
		   (:latex-minted-options nil nil org-gost-minted-options)
		   (:latex-default-table-environment nil nil org-gost-default-table-environment)
		   (:latex-header nil nil org-gost-latex-headers newline)
		   (:latex-src-block-backend nil nil org-gost-src-block-backend)
		   ;; Мои собственные параметры
		   ;; Будут использоваться в будующем 
		   (:teacher "TEACHER" nil org-gost-teacher newline)
		   (:education-organization "EDUORG" nil org-gost-education-organization parse)
		   (:type-of-work: "TYPE" nil org-gost-type-work parse)
		   )
  :translate-alist '((template . org-gost-template))
  )

(defcustom org-gost-education-organization "Образовательная организация"
  "Укзывается название организациии для титульного листа"
  :group 'org-gost
  :type 'string)

(defcustom org-gost-teacher "Преподаватель"
  "Преподаватели для титульного листа"
  :group 'org-gost
  :type 'string)
(defcustom org-gost-type-work "Тип работы"
  "Тип работы для титульного лица"
  :group 'org-gost
  :type 'string)

(defun org-gost-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
	(spec (org-latex--format-spec info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     ((or author email) (format "\\author{%s}\n" (or author email)))))
     ;; Date.
     ;; LaTeX displays today's date by default. One can override this by
     ;; inserting \date{} for no date, or \date{string} with any other
     ;; string to be displayed as the date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
	    (formatted-subtitle
	     (when subtitle
	       (format (plist-get info :latex-subtitle-format)
		       (org-export-data subtitle info))))
	    (separate (plist-get info :latex-subtitle-separate)))
       (concat
	(format "\\title{%s%s}\n" title
		(if separate "" (or formatted-subtitle "")))
	(when (and separate subtitle)
	  (concat formatted-subtitle "\n"))))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))
     ;; engrave-faces-latex preamble
     (when (eq org-gost-src-block-backend 'engraved) ;; Исправить на использование из get info значения
       (org-latex-generate-engraved-preamble info))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
	(cond ((not (plist-get info :with-title)) nil)
	      ((string= "" title) nil)
	      ((not (stringp command)) nil)
	      ((string-match "\\(?:[^%]\\|^\\)%s" command)
	       (format command title))
	      (t command))))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat (when (integerp depth)
		   (format "\\setcounter{tocdepth}{%d}\n" depth))
		 (plist-get info :latex-toc-command))))
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
	  (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))


(defun gost-filter-table (text backend info)
  "Функция исправления таблички"
  (replace-regexp-in-string "Continued on next page" "Продолжение на следующей странице" (replace-regexp-in-string "Continued from previous page" "Продолжение с предыдущей страницы" text)))

;; Просто экспортируем файл в pdf
(defun org-gost-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to GOST 7.32 file"
  (interactive)
    (let ((outfile (org-export-output-file-name ".tex" subtreep)))
      (org-export-to-file 'gost outfile
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

;; Надо как-то исправить
(setq org-gost-latex-headers 
      "\\usepackage[russian]{babel}
\\usepackage{tempora}
\\usepackage{geometry}
\\geometry{a4paper, left=30mm, top=20mm, bottom=20mm, right=15mm }
\\usepackage{graphicx}
\\usepackage{array}
\\usepackage{tabularx}
\\usepackage{listings}
\\usepackage{float}
\\usepackage{setspace}
\\usepackage{tabularx}
\\usepackage{longtable}
\\usepackage{titlesec}
\\titleformat*{\\section}{\\large\\bfseries}
\\titleformat*{\\subsection}{\\normalsize\\bfseries}
\\titleformat*{\\subsubsection}{\\normalsize\\bfseries}
\\addto\\captionsrussian{\\renewcommand{\\contentsname}{\\centering \\normalsize СОДЕРЖАНИЕ}}
\\addtocontents{toc}{\\protect\\thispagestyle{empty}}
\\usepackage{titletoc}
\\titlecontents{section}[0pt]{}{\\contentsmargin{0pt} \\thecontentslabel\\enspace}{\\contentsmargin{0pt}}{\\titlerule*[0.5pc]{.}\\contentspage}[]
\\dottedcontents{subsection}[3.1em]{}{1.5em}{0.5pc}
\\usepackage{caption}
\\DeclareCaptionLabelSeparator{custom}{ -- }
\\captionsetup[figure]{name=Рисунок, labelsep=custom, font={onehalfspacing}, justification=centering}
\\usepackage{ragged2e}
\\justifying
\\setlength\\parindent{1.25cm}
\\sloppy
\\usepackage{indentfirst}
\\usepackage{multirow}
\\usepackage{lscape}
\\usepackage{xcolor}
\\usepackage{fvextra}
" )


(setq
   org-gost-minted-options '(("breaklines" "true")
	 ("float" "t")
	 ("breakanywhere" "true")
	 ("fontsize" "\\footnotesize"))
   org-gost-default-table-environment "longtable"
   org-gost-src-block-backend 'engraved)

;;;###autoload

(provide 'ox-gost)
