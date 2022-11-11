;; ---------------------------------------------------------------
;; Copyright © 2022 Jason J.A. Stephenson <jason@sigio.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; ---------------------------------------------------------------

;; Functions and Macros Developed for Use at CW MARS.

(defgroup cwmars nil "Customizable settings for my CW MARS code."
  :group 'local)

(defcustom cwmars-db-updates-remote-path "/mnt/evergreen/db-updates/"
  "Path to the db-updates git repository on the MOBIUS servers."
  :type 'string :group 'cwmars :require 'cwmars)

(defcustom cwmars-db-updates-local-path "~/CWMARS/db-updates/"
  "Path to the local db-updates git repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-apps-local-path "~/CWMARS/moss/apps/"
  "Path to the local MOBIUS apps repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-apps-remote-path "/mnt/evergreen/apps/"
  "Path to the remote MOBIUS apps repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-apps-dev-local-path "~/CWMARS/moss/apps_dev/"
  "Path to the local MOBIUS apps_dev repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-apps-dev-remote-path "/mnt/evergreen/apps_dev/"
  "Path to the remote MOBIUS apps_dev repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-utilities-local-path "~/CWMARS/moss/utilities/"
  "Path to the local MOBIUS utilities repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-utilities-remote-path "/mnt/evergreen/utilities/"
  "Path to the remote MOBIUS utilities repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-files-path "~/CWMARS/files/"
  "Path to the root where CWMARS' files are stored."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-sql-path "~/CWMARS/sql/"
  "Path to my local git repository of CWMARS sql scripts."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-one-offs-path "~/CWMARS/one-offs/"
  "Path to collection of assorted scripts."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-evergreen-path "~/Src/Evergreen/"
  "Path to my local Evergreen source code repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-ncipserver-path "~/Src/NCIPServer/"
  "Path to my local NCIPServer source code repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-opensrf-path "~/Src/OpenSRF/"
  "Path to my local OpenSRF source code repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defcustom cwmars-sipserver-path "~/Src/SIPServer/"
  "Path to my local SIPServer source code repository."
  :type 'directory :group 'cwmars :require 'cwmars)

(defun tuesday-or-thursday ()
  "Find the next Tuesday or Thursday (including today) and return
the date as a string in the format of YYYYMMDD."
  (let* ((now (decode-time))
         (dow (nth 6 now))
         (dom (nth 3 now)))
    (format-time-string
     "%Y%m%d"
     (apply #'encode-time
            (cond ((< dow 2)
                   (setcar (nthcdr 3 now) (+ dom (- 2 dow)))
                   (setcar (nthcdr 6 now) (+ dow (- 2 dow)))
                   now)
                  ((= dow 3)
                   (setcar (nthcdr 3 now) (+ dom 1))
                   (setcar (nthcdr 6 now) (+ dow 1))
                   now)
                  ((= dow 5)
                   (setcar (nthcdr 3 now) (+ dom 4))
                   (setcar (nthcdr 6 now) 2)
                   now)
                  ((= dow 6)
                   (setcar (nthcdr 3 now) (+ dom 3))
                   (setcar (nthcdr 6 now) 2)
                   now)
                  (t now))))))

(defun cwmars-make-sql-updates (pattern)
  "Scan cwmars-db-updates-local-path for files to add to the
current buffer and prepend \\i cwmars-db-updates-remote-path."
  (interactive
   (let ((default-pattern (tuesday-or-thursday)))
     (list
      (read-string "Date for file search: " default-pattern nil default-pattern t))))
  (let* ((file-name (concat cwmars-db-updates-local-path pattern "_updates.sql"))
         (buffer (find-file file-name)))
    (save-excursion
      (let ((skip (list (file-name-nondirectory buffer-file-name)))
            (start-str (format "\\i %s" cwmars-db-updates-remote-path)))
        (if (< (point-min) (point-max))
            (let ((search-regexp (format "^\\%s\\(.+\\)$" start-str)))
              (goto-char (point-min))
              (while (re-search-forward search-regexp (point-max) t)
                (add-to-list 'skip (match-string 1)))
              (goto-char (point-max))))
        (dolist (elt (directory-files cwmars-db-updates-local-path nil pattern))
          (unless (member elt skip)
            (insert (format "%s%s\n" start-str elt)))))
      (if (buffer-modified-p)
          (save-buffer)))))

(defun cwmars-find-db-update (filename)
  "Wrapper for find-file to find a db update sql script."
  (interactive
   (list
    (read-file-name "File: " cwmars-db-updates-local-path nil nil
                    (concat (tuesday-or-thursday) "_CHANGEME.sql"))))
  (find-file (expand-file-name filename cwmars-db-updates-local-path)))

(defun cwmars-deploy (what)
  "Deploy code on production via SSH."
  (interactive "sDeploy what repository? ")
  (let ((varsym (intern-soft (format "cwmars-%s-remote-path" what))))
    (if varsym
        (let ((bufname (format "cwdeploy-%s" what))
              (command (format "cd %s && git pull" (symbol-value varsym))))
          (start-process bufname bufname "ssh" "mossdb" command)
          (switch-to-buffer-other-window bufname))
      (message "No remote path variable for %s." what))))

(defun cwmars-lib-notices-dates (lib bday bmonth byear eday emonth eyear)
  "Writes a bash script to get backdated notices for a given library between begin and end dates, inclusive."
  (interactive "sLibrary Code: \nnBegin Day: \nn Begin Month: \nnBegin Year: \nnEnd Day: \nnEnd Month: \nnEnd Year: ")
  (let ((current-date (decode-time (encode-time 0 0 0 bday bmonth byear)))
        (stop-date (decode-time (encode-time 0 0 0 (1+ eday) emonth eyear))))
    (when current-prefix-arg
      (insert "#!/bin/bash\n"))
    (insert "\ncd /mnt/evergreen/utilities/overdue\n")
    (while (not (and (= (nth 3 current-date) (nth 3 stop-date))
                     (= (nth 4 current-date) (nth 4 stop-date))
                     (= (nth 5 current-date) (nth 5 stop-date))))
      (insert (format "./notices_for_lib_date.sh %s %d-%02d-%02d\n" lib (nth 5 current-date) (nth 4 current-date)
                      (nth 3 current-date)))
      (setq current-date (decode-time (encode-time 0 0 0 (1+ (nth 3 current-date)) (nth 4 current-date)
                                                   (nth 5 current-date)))))
    (insert (format "\ncd /mnt/evergreen/circ_notices/%s\n" lib))
    (insert "./create-index.sh\n")))

;; Because of scan errors caused by the long string when used in a let
;; block, I put this in a global constant outside of the function.
;; This is the substitution pattern to use when generating the SQL to
;; insert asset.stat_cat_entry_copy_map entries for a new member's
;; copies in cwmars-make-aris-inserts.
(defconst cwmars-aris-replacement-str
  "INSERT INTO asset.stat_cat_entry_copy_map
(stat_cat, stat_cat_entry, owning_copy)
SELECT stat_cat, \\2, \\1
FROM asset.stat_cat_entry
WHERE id = \\2
ON CONFLICT ON CONSTRAINT sce_once_per_copy
DO UPDATE
SET stat_cat_entry = \\2
WHERE stat_cat_entry_copy_map.owning_copy = \\1
AND stat_cat_entry_copy_map.stat_cat = (SELECT stat_cat FROM asset.stat_cat_entry WHERE id = \\2);
INSERT INTO asset.stat_cat_entry_copy_map
(stat_cat, stat_cat_entry, owning_copy)
SELECT stat_cat, \\3, \\1
FROM asset.stat_cat_entry
WHERE id = \\3
ON CONFLICT ON CONSTRAINT sce_once_per_copy
DO UPDATE
SET stat_cat_entry = \\3
WHERE stat_cat_entry_copy_map.owning_copy = \\1
AND stat_cat_entry_copy_map.stat_cat = (SELECT stat_cat FROM asset.stat_cat_entry WHERE id = \\3);")

(defun cwmars-make-aris-inserts ()
  "Take a 3-column CSV where columns are 1. a copy id, 2. a stat
cat entry id, and 3. a stat cat entry id, and turn that into a
series of SQL insert statements."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\)$" nil t)
    (replace-match cwmars-aris-replacement-str)))

(defconst cwmars-replace-labels-str
  "SELECT * INTO acn
FROM asset.call_number
WHERE id = \\1;
acn.label := '\\2';
PERFORM cwmars.update_acn(acn);
")

(defun cwmars-make-acn-label-update ()
  "Take a 2-column CSV where the columns are 1. an
asset.call_number.id and 2. an unquoted asset.call_number.label
and turn that into a SQL DO statement to update the label of the
asset.call_number to the value from the CSV.  The label column
needs to have the new, desired value and should be in a format
that will work inside a single-quoted SQL string, i.e single
quotes/apostrophes should be doubled, etc."
  (interactive)
  (goto-char (point-min))
  (insert "DO\n$$\nDECLARE\n    acn asset.call_number%ROWTYPE;\nBEGIN\n\n")
  (while (re-search-forward "^\\([0-9]+\\),\\(.+\\)$" nil t)
    (replace-match cwmars-replace-labels-str t))
  (insert "\nEND\n$$;"))

(define-skeleton cwmars-db-update
  "Wrapper for a PostgreSQL database update script."
  nil
  "\\qecho " _ | (concat (skeleton-read "Message? ") "\n")
  "\nBEGIN;\n\n"
  @ _ ?\n
  "\nCOMMIT;\n")

(define-skeleton cwmars-activate-hold-notifications
  "Create SQL to activate hold notifications for a member library."
  "Enter org. unit id: "
  "\\qecho SP " (skeleton-read "Enter Ticket Number: ")
  ": Activate Hold Notifications for " (skeleton-read "Enter library name: ") ?\n
  "BEGIN;\n"
  "SELECT cwmars.clone_action_trigger_event_def_for_org(5, " str ");\n"
  "SELECT cwmars.clone_action_trigger_event_def_for_org(101, " str ");\n"
  "SELECT cwmars.clone_action_trigger_event_def_for_org(7, " str ");\n"
  "COMMIT;")

(define-skeleton cwmars-default-phone-upsert
  "Write SQL to update the default phone notify usr setting for a library's patrons."
  nil
  '(setq str (skeleton-read "Enter org. unit id: "))
  "DO $$\n"
  "DECLARE\n"
  "    data RECORD;\n"
  "    count INTEGER;\n"
  "BEGIN\n"
  "    count := 0;\n"
  "    FOR data IN\n"
  "        SELECT usr.id, '\"' || usr.day_phone || '\"' as day_phone\n"
  "        FROM actor.usr\n"
  "        LEFT JOIN actor.usr_setting phone ON phone.usr = usr.id\n"
  "        AND phone.name = 'opac.default_phone'\n"
  "        LEFT JOIN actor.usr_setting notify ON notify.usr = usr.id\n"
  "        AND notify.name = 'opac.hold_notify'\n"
  "        WHERE usr.deleted = FALSE AND usr.day_phone IS NOT NULL AND usr.day_phone <> ''\n"
  "        AND (phone IS NULL OR phone.value = '\"\"')\n"
  "        AND (notify IS NULL OR notify.value ILIKE '%phone%')\n"
  "        AND usr.home_ou "
  (format (if current-prefix-arg
              "IN (SELECT id FROM actor.org_unit_descendants(%s))"
            "= %s")
          str)
  ?\n
  "    LOOP\n"
  "        INSERT INTO actor.usr_setting\n"
  "        (usr, name, value)\n"
  "        VALUES\n"
  "        (data.id, 'opac.default_phone', data.day_phone)\n"
  "        ON CONFLICT ON CONSTRAINT usr_once_per_key\n"
  "        DO UPDATE\n"
  "        SET value = data.day_phone\n"
  "        WHERE usr_setting.usr = data.id\n"
  "        AND usr_setting.name = 'opac.default_phone';\n"
  "        count := count + 1;\n"
  "    END LOOP;\n"
  "    RAISE NOTICE '% Rows Affected', count;\n"
  "END $$;")

;; Create a prefix map for CW MARS commands and bind them.
(require 'my-funcs)                     ; For make-find-file-command macro
(defvar cwmars-map)
(define-prefix-command 'cwmars-map)
(global-set-key (kbd "s-m") 'cwmars-map)
(define-key cwmars-map (kbd "a") 'cwmars-make-aris-inserts)
(define-key cwmars-map (kbd "d") 'cwmars-db-update)
(define-key cwmars-map (kbd "f") 'cwmars-find-db-update)
(define-key cwmars-map (kbd "l") 'cwmars-make-acn-label-update)
(define-key cwmars-map (kbd "pa") (lambda () (interactive) (cwmars-deploy "apps")))
(define-key cwmars-map (kbd "pd") (lambda () (interactive) (cwmars-deploy "apps-dev")))
(define-key cwmars-map (kbd "pu") (lambda () (interactive) (cwmars-deploy "utilities")))
(define-key cwmars-map (kbd "s") 'cwmars-make-sql-updates)
(define-key cwmars-map (kbd "u") (lambda () (interactive) (cwmars-deploy "db-updates")))
;; Key bindings to open files and directories that I commonly use.
(define-key cwmars-map (kbd "A") (make-find-file-command cwmars-apps-local-path))
(define-key cwmars-map (kbd "B") (make-find-file-command cwmars-apps-dev-local-path))
(define-key cwmars-map (kbd "C") (make-find-file-command (expand-file-name "cwmars_custom" cwmars-utilities-local-path)))
(define-key cwmars-map (kbd "D") (make-find-file-command cwmars-db-updates-local-path))
(define-key cwmars-map (kbd "E") (make-find-file-command cwmars-evergreen-path))
(define-key cwmars-map (kbd "F") (make-find-file-command cwmars-files-path))
(define-key cwmars-map (kbd "M") (make-find-file-command (expand-file-name "misc" cwmars-files-path)))
(define-key cwmars-map (kbd "N") (make-find-file-command cwmars-ncipserver-path))
(define-key cwmars-map (kbd "O") (make-find-file-command cwmars-opensrf-path))
(define-key cwmars-map (kbd "P") (make-find-file-command cwmars-sipserver-path))
(define-key cwmars-map (kbd "Q") (make-find-file-command cwmars-sql-path))
(define-key cwmars-map (kbd "S") (make-find-file-command (expand-file-name "scripts" cwmars-utilities-local-path)))
(define-key cwmars-map (kbd "T") (make-find-file-command (expand-file-name "Tickets" cwmars-files-path)))
(define-key cwmars-map (kbd "U") (make-find-file-command cwmars-utilities-local-path))
(define-key cwmars-map (kbd "V") (make-find-file-command (expand-file-name "overdue" cwmars-utilities-local-path)))
(define-key cwmars-map (kbd "Z") (make-find-file-command cwmars-one-offs-path))

(provide 'cwmars)
