python-mode sh-mode lua-mode

(header
 comment-start "-*- time-stamp-pattern: \"changed[\\s]+:[\\s]+%%$\"; -*-" n
 comment-start "AUTHOR INFORMATION " (make-string (- fill-column (current-column)) (string-to-char comment-start)) n
 comment-start "file    : " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) n
 comment-start "author  : " user-full-name " <" user-mail-address ">" n
 comment-start n
 comment-start "created : " (format-time-string "%04Y-%02m-%02d %02H:%02M:%02S") " (" user-full-name ")" n
 comment-start "changed : " (format-time-string "%Y-%m-%d %H:%M:%S") " (" user-full-name ")" n
 comment-start "DESCRIPTION " (make-string (- fill-column (current-column)) (string-to-char comment-start)) n
 comment-start "..." n
 comment-start "LICENSE " (make-string (- fill-column (current-column)) (string-to-char comment-start)) n
 comment-start "..." n
 (make-string (- fill-column (current-column)) (string-to-char comment-start)) n
 r)

(sep comment-start (s str) " " (make-string (- fill-column (length str) (length comment-start) (current-indentation) 1) (string-to-char comment-start)) n> r)

sh-mode

(shebang "#!/usr/bin/env " (if (equal (file-name-extension (or (buffer-file-name) (buffer-name))) "zsh") "zsh" "bash") r)

org-mode

(PROJ
 (when (setq-local proj-level (or (org-current-level) 0)) nil)
 "PROJ " p " [0/1] :" (s tag) ":" n
 ":PROPERTIES:" n
 ":CATEGORY: " (upcase (concat " " tag)) n
 ":VISIBILITY: hide" n
 ":COOKIE_DATA: recursive todo" n
 ":END:" n
 (make-string (+ 1 proj-level) ?*) " Information                                                             :info:" n
 ":PROPERTIES:" n
 ":VISIBILITY: hide" n
 ":END:" n
 (make-string (+ 1 proj-level) ?*) " Notes                                                                  :notes:" n
 ":PROPERTIES:" n
 ":VISIBILITY: hide" n
 ":END:" n
 (make-string (+ 1 proj-level) ?*) " Tasks                                                                  :tasks:" n
 ":PROPERTIES:" n
 ":VISIBILITY: content" n
 ":END:" n
 (make-string (+ 2 proj-level) ?*) " TODO " p
 )

(protokol
 "#+BRAIN_PARENTS: 3c9bed62-0bba-4f63-9cdc-be1c1dbeed34" n
 "#+FILETAGS: showchildren:" n
 "#+TITLE: Protokoll: Jour fixe vom " (format-time-string "%-d. %-d. %Y") n
 "#+OPTIONS: date:nil" n
 "#+LATEX_CLASS_OPTIONS: [ngerman, a4paper]" n
 "#+LATEX_HEADER: \\usepackage[margin=2.5cm]{geometry}" n
 "#+LATEX_HEADER: \\usepackage{parskip}" n
 "#+LATEX_HEADER: \\usepackage[dvipsnames]{xcolor}" n
 "#+MACRO: todo @@latex:\\color{Green}{\\textbf{@@ToDo@@latex:}}@@" n
 "#+OPTIONS: toc:nil" n n
 "| Ort | Webex |" n
 "| Zeit | [" (format-time-string "%Y-%m-%d %a")" 11:00] - [" (format-time-string "%Y-%m-%d %a") " 12:00] |" n
 "| Teilnehmende | " p "|" n
 "| Protokoll | Marcel Arpogaus |" n n
 "+TOC: headlines 2" n n
 "* " r n n
 "* To-dos" n
 " - XXX : YYY"
 )
