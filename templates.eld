python-base-mode sh-base-mode lua-mode

(header
 comment-start "-*- time-stamp-pattern: \"changed[\\s]+:[\\s]+%%$\"; -*-" n
 comment-start "%% Author " (make-string (- fill-column (current-column)) (string-to-char comment-start)) n
 comment-start "file    : " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) n
 comment-start "author  : " user-full-name " <" user-mail-address ">" n
 comment-start n
 comment-start "created : " (format-time-string "%04Y-%02m-%02d %02H:%02M:%02S") " (" user-full-name ")" n
 comment-start "changed : " (format-time-string "%Y-%m-%d %H:%M:%S") " (" user-full-name ")" n n
 comment-start "%% License " (make-string (- fill-column (current-column)) (string-to-char comment-start)) n n
 comment-start "%% Description " (make-string (- fill-column (current-column)) (string-to-char comment-start)) n n
 r)

(sep comment-start "%% " (s str) " " (make-string (- fill-column (length str) (length comment-start) (current-indentation) 4) (string-to-char comment-start)) n> r)

sh-mode bash-ts-mode

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

(minutes
 "#+FILETAGS: showchildren:" n
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
 " - XXX :: YYY"
 )

(README
 (p "Please enter the host (i.e.: github): " forge t)
 (p "Please enter the domain (i.e.: com): " domain t)
 (p "Please enter your user name: " user t)
 (p "Please enter the repo name: " repo t)
 "# Inspired by: https://github.com/othneildrew/Best-README-Template" n
 "#+OPTIONS: toc:nil" n n
 "[[https://" forge "." domain "/" user "/" repo "/graphs/contributors][https://img.shields.io/" forge "/contributors/" user "/" repo ".svg?style=" (p "flat-square" style) "]]" n
 "[[https://" forge "." domain "/" user "/" repo "/network/members][https://img.shields.io/" forge "/forks/" user "/" repo ".svg?style=" style "]]" n
 "[[https://" forge "." domain "/" user "/" repo "/stargazers][https://img.shields.io/" forge "/stars/" user "/" repo ".svg?style=" style "]]" n
 "[[https://" forge "." domain "/" user "/" repo "/issues][https://img.shields.io/" forge "/issues/" user "/" repo ".svg?style=" style "]]" n
 "[[https://" forge "." domain "/" user "/" repo "/blob/main/LICENSE][https://img.shields.io/" forge "/license/" user "/" repo ".svg?style=" style "]]" n
 "[[https://" forge "." domain "/" user "/" repo "/actions/workflows/test.yaml][https://img.shields.io/" forge "/actions/workflow/status/" user "/" repo "/test.yaml.svg?label=test&style=" style "]]" n
 "[[https://" forge "." domain "/" user "/" repo "/actions/workflows/release.yaml][https://img.shields.io/" forge "/actions/workflow/status/" user "/" repo "/release.yaml.svg?label=release&style=" style "]]" n
 "[[https://" forge "." domain "/" user "/" repo "/blob/main/.pre-commit-config.yaml][https://img.shields.io/badge/pre--commit-enabled-brightgreen.svg?logo=pre-commit&style=" style "]]" n
 "[[https://linkedin.com/in/" user "][https://img.shields.io/badge/-LinkedIn-black.svg?style=" style "&logo=linkedin&colorB=555]]" n n
 "* " (p (string-replace "-" " " repo)) n n
 "#+TOC: headlines 2 local" n n
 "** About The Project" n n "..." n n
 "** Getting Started" n n
 "This is an example of how you may give instructions on setting up your" n
 "project locally. To get a local copy up and running follow these simple" n
 "example steps." n n
 "*** Prerequisites" n n "- A" n "- B" n n
 "*** Installation" n n "1. Step" n "2. Step" n n
 "** Usage" n n "..." n n
 "** Contributing" n n "Any Contributions are greatly appreciated! If you have a question, an issue or would like to contribute, please read our [[file:CONTRIBUTING.md][contributing guidelines]]." n n
 "** License" n n
 "Distributed under the [[file:LICENSE][" (p "TODO: License") "]]" n n
 "** Contact" n n
 "[[https://" forge "." domain "/" user "/][" user-full-name "]] - [[mailto:" user-mail-address "][" user-mail-address "]] (encrypted with [[https://rot13.com/][ROT13]])" n n
 "Project Link:" n
 "[[https://" forge "." domain "/" user "/" repo"]]" n n
 "** Acknowledgments" n n "..."
 )

(python-shields
 "[[https://pypi.org/project/" project "][https://img.shields.io/pypi/v/" (p (project-name (project-current)) project) ".svg?style=" (p "flat-square" style) "]]" n
 "[[https://pypi.org/project/" project "][https://img.shields.io/pypi/pyversions/" project ".svg?style=" style "]]" q
 )
