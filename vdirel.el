;;; vdirel.el --- Manipulate vdir (i.e., vCard) repositories

;; Copyright (C) 2015 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Author: Aurélien Bosché <aurelien_bosche@yahoo.com>
;; Version: 0.1.0
;; GIT: https://github.com/taurgal/vdirel
;; Package-Requires: ((emacs "24.4") (org-vcard "0.1.0") (helm "1.7.0") (seq "1.11"))
;; Created: 09 Dec 2015
;; Keywords: vdirsyncer vdir vCard carddav contact addressbook helm

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Manipulate vdir (i.e., vCard) repositories from Emacs

;;; Code:
(require 'org-vcard)
(require 'seq)
(require 'helm)

(defgroup vdirel nil
  "Manipulate vdir (i.e., vCard) repositories from Emacs"
  :group 'applications)

(defcustom vdirel-repositories nil
  "List of paths to vdir folders."
  :group 'vdirel)

(defvar vdirel-cache-contacts '()
  "Cache where contacts are stored to avoid repeated parsing.
This is an alist mapping a repository name to a contact list.")

(defcustom vdirel-helm-input-history '()
  "Input history for the helm-vdirel session."
  :group 'vdirel)

(defcustom vdirel-create-org-entry-if-needed t
  "Whether to create an org headline when it is not found."
  :group 'vdirel)

(defcustom vdirel-org-entry-template
  "\n* {:full-name}\n  :PROPERTIES:\n  :CUSTOM_ID: {:UID}\n  :END:"
  "Template used for org headline.
See also `vdirel-create-org-entry-if-needed' and
`vdirel-expand-template'. Possible keys are taken from the vcard
with the addition of :full-name."
  :group 'vdirel)

;; (
;;  ("VDIREL-FILENAME" . "/home/cassou/Documents/configuration/contacts/5007154e-e4e4-491e-ab4e-2bfc6970444c.vcf")
;;  ("VERSION" . "3.0")
;;  ("PRODID" . "-//ASynK v2.1.0-rc2+//EN")
;;  ("UID" . "5007154e-e4e4-491e-ab4e-2bfc6970444c")
;;  ("EMAIL;TYPE=home" . "email1@foo.com")
;;  ("EMAIL;TYPE=home" . "email2@foo.com")
;;  ("EMAIL" . "email3@foo.com")
;;  ("EMAIL" . "email4@foo.com")
;;  ("EMAIL" . "email5@foo.com")
;;  ("FN" . "First Last")
;;  ("N" . "First;Last;;;")
;;  ("REV" . "20150612T164658Z")
;;  ("TEL;TYPE=voice" . "+33242934873")
;;  ("TEL;TYPE=voice" . "+33399898111"))

(defun vdirel-get-repository-name (repository)
  "Return the name of the repository."
  (plist-get repository :name))

(defun vdirel-get-repository-org-filename (repository)
  "Return the name of the repository."
  (plist-get repository :org-filename))

(defun vdirel-get-repository-dir (repository)
  "Return the directory of the repository."
  (expand-file-name (plist-get repository :dir)))

(defun vdirel-get-contact-property (property contact)
  "Return value of first property named PROPERTY in CONTACT.
Return nil if PROPERTY is not in CONTACT."
  (assoc-default property contact #'string= nil))

(defun vdirel-get-contact-properties (property contact)
  "Return values of all properties named PROPERTY in CONTACT."
  (vdirel-get-contact-matching-properties
   (lambda (propname) (string= propname property))
   contact))

(defun vdirel-get-contact-matching-properties (pred contact)
  "Return values of all properties whose name match PRED in CONTACT."
  (seq-map #'cdr (seq-filter (lambda (pair)
                               (funcall pred (car pair)))
                             contact)))

(defun vdirel-contact-fullname (contact)
  "Return the fullname of CONTACT."
  (or
   (vdirel-get-contact-property "FN" contact)
   (replace-regexp-in-string
    ";" " "
    (vdirel-get-contact-property "N" contact))))

(defun vdirel-contact-emails (contact)
  "Return a list of CONTACT's email addresses."
  (vdirel-get-contact-matching-properties
   (lambda (property) (string-match "^EMAIL" property))
   contact))


(defun vdirel-get-contacts-from-cache (repository)
  "Return the contacts in cache for REPOSITORY."
  (plist-get
     (find-if
      (lambda (contact-data) (equal (plist-get contact-data :repository) repository))
      vdirel-cache-contacts)
     :contacts-list))

(defun vdirel-get-repository-contact-files (repository)
  "Return a list of vCard files in REPOSITORY."
  (directory-files
   (vdirel-get-repository-dir repository) t "\.vcf$" t))

(defun vdirel-get-contact-files ()
  "Return a list of vCard files."
  (let ((contact-file-list '(nil)))
    (dolist (repo vdirel-repositories contact-file-list)
      (nconc contact-file-list (vdirel-get-repository-contact-files repo)))
    (cdr contact-file-list)
    ))

(defun vdirel-parse-file-to-contact (filename)
  "Return a list representing the vCard in FILENAME.
Each element in the list is a cons cell containing the vCard property name
in the `car', and the value of that property in the `cdr'.  Parsing is done
through `org-vcard-import-parse'."
  (with-temp-buffer
    (insert-file-contents filename)
    (cons
     (cons "VDIREL-FILENAME" filename)
     (car (org-vcard-import-parse "buffer")))))

(defun vdirel-build-contacts-from-repository (repository)
  "Return a list of contacts in REPOSITORY."
  (mapcar
   (lambda (contact)
     (cons `("VDIREL:REPOSITORY" . ,repository) contact)
     )
  (mapcar
   #'vdirel-parse-file-to-contact
   (vdirel-get-repository-contact-files repository))))

(defun vdirel-recreate-cache ()
  "Parse all contacts in REPOSITORY and store the result."
  (interactive)
  (setq vdirel-cache-contacts nil)
  (dolist (repository vdirel-repositories)
    (let* ((contacts (vdirel-build-contacts-from-repository repository)))
      (add-to-list
       'vdirel-cache-contacts
        (list :repository repository :contacts-list contacts)
        )))
  vdirel-cache-contacts
  )
  
(defun vdirel-debug-info (string &rest objects)
  "Log STRING with OBJECTS as if using `format`."
  (apply #'message (concat "[carldavel] info: " string) objects))

;;;###autoload
(defun vdirel-vdirsyncer-sync-server ()
  "Ask vdirsyncer to sync REPOSITORY with the server.
You probably want to call `vdirel-refresh-cache' right after
this.  Currently, REPOSITORY is ignored and \"vdirsyncer sync\" is called
without further argument."
  (interactive)
  (vdirel-debug-info "Executing vdirsyncer sync")
  (save-excursion
    (with-current-buffer (get-buffer-create "*vdirel-server-sync*")
      (call-process
       "vdirsyncer"
       nil
       (current-buffer)
       nil
       "sync")))
  (vdirel-debug-info "Finshed executing vdirsyncer sync"))


(defun vdirel-helm-email-candidates (contacts)
  "Return a list of contact emails for every contact in CONTACTS."
  (seq-map (lambda (contact)
             (cons (vdirel-expand-template "{:name} {:mail} {:categories}"
                           (list
                            :name
                            (format "%-20.20s"
                            (vdirel-contact-fullname contact))
                            :mail
                            (format "%-40.40s"
                            (mapconcat (lambda (x) (concat "<" x ">"))
                                       (vdirel-contact-emails contact) ", "))
                            :categories
                            (mapconcat 'identity
                                       (vdirel-get-contact-properties "CATEGORIES" contact)
                                       ", "))
                           )
                      contact
                      ))
              contacts))

(defun vdirel-helm-insert-contact-email (candidate)
  "Print selected contacts as comma-separated text.
CANDIDATE is ignored."
  (ignore candidate)
  (insert (mapconcat (lambda (contact)
                       (format "\"%s\" <%s>"
                               (vdirel-contact-fullname contact)
                               (car (vdirel-contact-emails contact))))
                     (helm-marked-candidates)
                     ", ")))

(defun vdirel-helm-open-contact-vcard (candidate)
  "Open vcard file correpsonding to candidate.
If it is not already in the persistent window and revert to the
previous buffer otherwise."
  (let* ((current (window-buffer helm-persistent-action-display-window))
         (filepath  (vdirel-get-contact-property "VDIREL-FILENAME" candidate))
         (bufname (file-name-nondirectory filepath)))
      (if (or (helm-follow-mode-p)
              (eql current (get-buffer helm-current-buffer))
              )
          (find-file filepath)
        (switch-to-buffer helm-current-buffer))))

(defvar helm-find-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'vdirel-helm-open-org-entry-vcard)
    (delq nil map))
  "Keymap for `vdirel-helm-select-email'.")

(defun vdirel-helm-open-org-entry-vcard ()
  "Delete current candidate without quitting."
  (interactive)
  (with-helm-alive-p
    (helm-attrset
     'open-orgfile
     '(vdirel-helm-open-org-entry-vcard-helper . never-split))
    (helm-execute-persistent-action 'open-orgfile)))
(put 'vdirel-helm-open-org-entry-vcard 'helm-only t)

(defun vdirel-helm-open-org-entry-vcard-helper (candidate)
  "Open org file for CANDIDATE."
  (let* ((repo (vdirel-get-contact-property "VDIREL:REPOSITORY" candidate))
         (orgfilename (vdirel-get-repository-org-filename repo))
         (custid (concat "#" (vdirel-get-contact-property "UID" candidate)))
         (current (window-buffer helm-persistent-action-display-window))
         (bufname (file-name-nondirectory orgfilename))
         (match))
    (if
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents orgfilename)
              (org-link-search custid)
              t)
          (error nil))
        (progn
          (find-file orgfilename)
          (org-link-search custid)
          )
      (if vdirel-create-org-entry-if-needed
          (progn
            (find-file orgfilename)
            (goto-char (point-max))
            (insert
             (vdirel-expand-template
              vdirel-org-entry-template
              (nconc `(:full-name ,(vdirel-contact-fullname candidate))
                    (vdirel-alist-plist candidate t))
              "??"
              )))
        (message "vdirel: CUSTOM_ID %s in %s not found." custid orgfilename)
        )
      )))


(defun vdirel-cached-repository-names ()
  "Return a list of vCard files."
  (seq-uniq
   (mapcar 'vdirel-get-repository-name vdirel-repositories)
   #'string=)
  )

;; (defun vdirel-merge-cached-repos()
;;   "Return a list of vCard files."
;;   (interactive)
;;   (let ((merged-repo-list '(nil))
;;         (contact-list-with-repo-name)
;;         (contacts-repos-with-given-name))
;;     (dolist (name (vdirel-cached-repository-names) merged-repo-list)
;;       (setq contacts-repos-with-given-name '(nil))
;;       (setq contact-list-with-repo-name
;;             (seq-filter (lambda (contact) (string=
;;                                            name
;;                                            (plist-get
;;                                             (plist-get contact :repository)
;;                                             :name)))
;;                         vdirel-cache-contacts))
;;       (dolist (contact-list contact-list-with-repo-name)
;;         (nconc contacts-repos-with-given-name
;;                (plist-get contact-list :contacts-list))
;;         )
;;       (nconc merged-repo-list `(,contacts-repos-with-given-name))
;;       )
;;     (cdr merged-repo-list)
;;     ))

(setq vdirel-helm-sources 3)

;;;###autoload
(defun vdirel-helm-select-email (&optional refresh)
  "Prompt for a contact from the vdir repos using helm.
With a prefix argument, first recreate the cache. With a double
prefix argument, first sync the vdir and recreate the cache."
  (interactive
   (list (cond ((equal '(4) 'cache)
                (equal '(16) current-prefix-arg) 'server)
               )))
  (when (eq refresh 'server)
    (vdirel-vdirsyncer-sync-server))
  (when (memq refresh '(cache server))
    (vdirel-recreate-cache))
  (let ((helm-source-vdirel))
    (setq helm-source-vdirel nil)
    (dolist (repo vdirel-repositories)
      (setq helm-source-vdirel
            (add-to-list
             'helm-source-vdirel
             (helm-build-sync-source (vdirel-get-repository-name repo)
               :candidates (vdirel-helm-email-candidates (vdirel-get-contacts-from-cache repo))
               :action (helm-make-actions
                        "INSERT" 'vdirel-helm-insert-contact-email
                        "VISIT VCARD" 'vdirel-helm-open-contact-vcard
                        "VISIT ORG" 'vdirel-helm-open-org-entry-vcard
                        )
               :persistent-action  'vdirel-helm-open-contact-vcard
               :persistent-help "Open vcard"
               :keymap 'helm-find-files-map
               )
             )))
    (helm :prompt "Contacts: "
          :buffer "*helm vdirel*"
          :sources helm-source-vdirel
          :history 'vdirel-helm-input-history
          )
    ))

(defun vdirel-alist-plist (alist &optional symbol_keys)
  "Returns a property list containing the same keys and values as
the association list ALIST in the same order. If SYMBOL_KEYS is
non nil then add a colon in font of all keys that are strings and
convert them to a symbol."
  (let ((plist) (key))
    (dolist (pair alist)
      (setq key (car pair))
      (push (cond ((and (stringp key) symbol_keys)
                   (intern (concat ":" key)))
                  (t key))
                  plist)
      (push (cdr pair) plist))
    (nreverse plist)))

(defun vdirel-expand-template (s plist &optional plist_def)
  "Better string formatting using a plist.
Subsistitute {:key} in the string S with the value of the
property :key in PLIST if it is there otherwise use PLIST_DEF (a
string or a plist)."
  (replace-regexp-in-string "{\\(:[$&*+-_<>[:alpha:]]+\\)}"
                            (lambda (symb_name_as_str)
                              (let* ((keyword (intern (substring symb_name_as_str 1 -1)))
                                    (value (or (plist-get plist keyword)
                                               (cond ((stringp plist_def) plist_def)
                                                     (t (plist-get plist_def keyword))))))
                                (format "%s" value)))
                            s))

(let ((candidate (car (plist-get (car vdirel-cache-contacts) :contacts-list))))
  (cons `(:full-name ,(vdirel-contact-fullname candidate))
        (vdirel-alist-plist candidate t)))

((:full-name "e") 
:VDIREL:REPOSITORY (:name "Perso" :dir "/home/taurgal/.contacts/pers/default/" :org-filename "/home/taurgal/.contacts/work/work.org")
 :VDIREL-FILENAME "/home/taurgal/.contacts/pers/default/448162fe0c8cc619.vcf" :VERSION "3.0" :N ";e;;;" :FN "e" :ORG "d" :REV "2017-05-09T16:38:24Z" :UID "448162fe0c8cc619" :item1\.EMAIL "d" :NOTE "f" :X-PHONETIC-FIRST-NAME "" :X-PHONETIC-LAST-NAME "" :item1\.X-ABLabel "")

(provide 'vdirel)

;;; vdirel.el ends here

;;  LocalWords:  vCard
