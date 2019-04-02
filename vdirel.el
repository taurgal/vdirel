;;; vdirel.el --- Manipulate vdir (i.e., vCard) repositories

;; Copyright (C) 2015 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Author: Aurélien Bosché <aurelien_bosche@yahoo.com>
;; Version: 1.0.0
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
(require 'helm-mu)

(defgroup vdirel nil
  "Manipulate vdir (i.e., vCard) repositories from Emacs"
  :group 'applications)

(defface vdirel-contacts-name-face
  (cond  ((boundp 'helm-mu-contacts-name-face)
          helm-mu-contacts-name-face)
         (t
  '((((background dark)) :foreground "white")
    (t :foreground "black"))))
  "Face for names in contacts list."
  :group 'vdirel-faces)

(defface vdirel-contacts-address-face
  (cond
   ((boundp 'helm-mu-contacts-address-face)
          helm-mu-contacts-address-face)
         (t
  '((((background dark)) :foreground "#1e90ff")
    (t :foreground "#1e90ff"))))
  "Face for email addresses in contacts list."
  :group 'vdirel-faces)

(defface vdirel-contacts-group-face
  '((((background dark)) :foreground "green")
    (t :foreground "green"))
  "Face for email addresses in contacts list."
  :group 'vdirel-faces)

(defface vdirel-contacts-categories-face
      '((((background dark)) :foreground "red")
        (t :foreground "red"))
  "Face for email addresses in contacts list."
  :group 'vdirel-faces)


(defcustom vdirel-repositories nil
  "List of repositories.
A repository is a plist with at least a :name and a :dir property."
  :group 'vdirel)

(defvar vdirel-cache-contacts '()
  "Cache where contacts are stored to avoid repeated parsing.
This is a list of property lists having keys :repository
and :contacts-list.")

(defcustom vdirel-helm-input-history '()
  "Input history for the helm-vdirel session."
  :group 'vdirel)

(defcustom vdirel-create-org-headline-if-needed 'ask
  "Whether to create an org headline when it is not found.
nil means never create a new entry, the symbol ask means ask the
user whether to include the new headline, anything else means
always create the headline."
  :group 'vdirel)

(defcustom vdirel-org-entry-template
  "\n* {:full-name}\n  :PROPERTIES:\n  :CUSTOM_ID: {:UID}\n  :END:"
  "Template used for org headlines.
See also `vdirel-create-org-headline-if-needed' and
`vdirel-expand-template'. Possible keys are taken from the vcard
with the addition of :full-name."
  :group 'vdirel)

(defun vdirel-expand-template (s plist &optional plist_def)
  "String formatting utility using a plist.
Subsistitute {:key} in the string S with the value of the
property :key in PLIST if it is there otherwise use PLIST_DEF (a
string or a plist) for default values. If PLIST_DEF is a string
then it is the default value for all replacements, otherwise the
default value is looked up in PLIST_DEF just like values are
looked up in PLIST."
  (setq ss s
        pp plist)
  (condition-case nil
      (replace-regexp-in-string "{\\(:[$&*+-_<>[:alpha:]]+\\)}"
                                (lambda (symb_name_as_str)
                                  (let* ((keyword (intern (substring symb_name_as_str 1 -1)))
                                         (value (or (plist-get plist keyword)
                                                    (cond ((stringp plist_def) plist_def)
                                                      (t (plist-get plist_def keyword))))))
                                    (format "%s" value)))
                                s)
    ((error nil))))

(defun vdirel-get-repository-name (repository)
  "Return the name of the REPOSITORY."
  (plist-get repository :name))

(defun vdirel-get-repository-from-name (reponame)
  "Return the repository with name REPONAME."
  (let ((repolist
         (-filter
          (lambda (r) (string= (vdirel-get-repository-name r) reponame))
          vdirel-repositories)))
    (cond ((> (length repolist) 1)
           (error "There are %d≠1 repositories named %s." (length repolist) reponame))
          ((< (length repolist) 1)
           (error "There are no repositories named %s." (length repolist) reponame))
          (t (first repolist)))
          ))

(defun vdirel-get-repository-org-filename (repository)
  "Return the filename of the org-file of REPOSITORY."
  (plist-get repository :org-filename))

(defun vdirel-get-repository-dir (repository)
  "Return the directory of the REPOSITORY."
  (let ((dir (plist-get repository :dir)))
    (cond ((stringp dir) (expand-file-name dir))
          (t dir))
    ))

(defun vdirel-get-repository-priority (repository)
  "Return the directory of the REPOSITORY."
  (or (plist-get repository :priority) 0))

(defun vdirel-get-contact-property (property contact &optional default)
  "Return the value of the first property named PROPERTY in CONTACT.
Return nil if PROPERTY is not in CONTACT."
  (assoc-default property contact #'string= default))

(defun vdirel-get-contact-properties (property contact)
  "Return values of all properties named PROPERTY in CONTACT."
  (vdirel-get-contact-matching-properties
   (lambda (propname) (string= propname property))
   contact))

(defun vdirel-get-contact-properties-surrounded (property contact s)
  "Return values of all properties named PROPERTY in CONTACT."
  (let ((prop (mapconcat 'identity
                         (vdirel-get-contact-matching-properties
                          (lambda (propname) (string= propname property))
                          contact)
                         ",")))
    (concat s (replace-regexp-in-string "," s prop) s)
    ))

(defun vdirel-get-contact-matching-properties (pred contact)
  "Return values of all properties whose name match PRED in CONTACT."
  (seq-map #'cdr (seq-filter (lambda (pair)
                               (funcall pred (car pair)))
                             contact)))

(defun vdirel-string-or-nil-if-empty (s)
  (if (or (null s) (string-empty-p s)) nil s))

(defun vdirel-contact-fullname (contact)
  "Return the fullname of CONTACT (but without additional names).
Use the FN field in contact if present, otherwise use the N field."
  (or
   (and (stringp contact)
        (mapconcat 'identity (cdr (split-string contact "\t" t "[\t ]+")) " "))
   (vdirel-string-or-nil-if-empty
    (vdirel-get-contact-property "FN" contact "")
    )
   (and (vdirel-get-contact-property "N" contact "")
        (pcase (split-string (vdirel-get-contact-property "N" contact "")
                        ";")
          (`(,surname ,name ,add-names ,pref ,suff)
           (mapconcat
            'identity
            (-filter (lambda (s) (not (or (null s) (string-empty-p s))))
                     (list pref surname name suff))
            " ")
           )
     (_ (error "Malformed `N' property in VCARD"))
     ))
   (vdirel-get-contact-property "NONAME" contact)
   ))

(defun vdirel-contact-uid (contact)
  "Return the UID of CONTACT or nil."
  (vdirel-get-contact-property "UID" contact))

(defun vdirel-contact-emails (contact)
  "Return a list of CONTACT's email addresses."
  (or
   (and (stringp contact) (car (split-string contact "\t" t "[\t ]+")))
  (vdirel-get-contact-matching-properties
   (lambda (property) (string-match "^EMAIL" property))
   contact)))


(defun vdirel-get-contacts-from-cache (repository)
  "Return the contacts in cache for REPOSITORY."
  (plist-get
     (find-if
      (lambda (contact-data) (equal (plist-get contact-data :repository) repository))
      vdirel-cache-contacts)
     :contacts-list))

(defun vdirel-get-repository-contact-files (repository)
  "Return a list of vCard files in REPOSITORY."
  (let ((dir (vdirel-get-repository-dir repository)))
    (cond ((stringp dir) (directory-files dir  t "\.vcf$" t))
          (error "This is not a vdir: '%S'" dir))
    ))

(defun vdirel-is-vcard-repo (repository)
  (stringp (vdirel-get-repository-dir repository)))

(defun vdirel-filter-vcard-repos ()
  "Return a list of vCard files in REPOSITORY from `vdirel-repositories'."
  (-filter #'vdirel-is-vcard-repo vdirel-repositories)
  )

(defun vdirel-get-contact-files ()
  "Return a list of vCard files."
  (interactive)
  (let ((contact-file-list))
    (dolist (repo vdirel-repositories contact-file-list)
      (push (vdirel-get-repository-contact-files repo) contact-file-list))
    ))

(defun vdirel-parse-file-to-contact (filename)
  "Return a list representing the vCard in FILENAME.
Each element in the list is a cons cell containing the vCard property name
in the `car', and the value of that property in the `cdr'.  Parsing is done
through `org-vcard-import-parse'."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((parsed-repr (org-vcard-import-parse "buffer")))
      (cond ((> (length parsed-repr) 1)
             (error "There should be at most one vcard in %s" filename))
            ((< (length parsed-repr) 1)
             (error "No vcard found in %s" filename)))
      (cons
       (cons "VDIREL-FILENAME" filename)
       (car parsed-repr)))))

(defun vdirel-build-contacts-from-repository (repository)
  "Return a list of contacts in REPOSITORY."
  (let ((repodir (vdirel-get-repository-dir repository)))
    (cond ((stringp repodir)
           (mapcar
            (lambda (contact)
              (cons `("VDIREL-REPOSITORY" . ,repository) contact)
              )
            (mapcar
             #'vdirel-parse-file-to-contact
             (vdirel-get-repository-contact-files repository))))
          ((eq repodir :mu)
           (helm-mu-contacts-init)
           ))
    ))

(defun vdirel-recreate-cache ()
  "Parse all contacts in the repositories and store the result.
Repositories are those in `vdirel-repositories'"
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
             (cons (vdirel-expand-template "{:name} {:mail} {:categories} {:group}"
                           (list
                            :name
                            (propertize
                             (format "%-20.20s"
                                     (vdirel-contact-fullname contact)
                                     'face 'vdirel-contacts-name-face))
                            :mail
                            (propertize
                             (format "%-35.35s"
                                     (mapconcat (lambda (x) (concat "<" x ">"))
                                                (vdirel-contact-emails contact) ", "))
                                     'face 'vdirel-contacts-address-face)
                            :categories
                            (format "%s"
                            (propertize
                             (vdirel-get-contact-properties-surrounded "CATEGORIES" contact ":")
                             'face 'vdirel-contacts-categories-face))
                            :group
                            (format "%-20.20s"
                                    (propertize
                                     (vdirel-get-contact-properties-surrounded "GROUP" contact "~")
                                     'face 'vdirel-contacts-group-face))
                            )
                           )
                      contact
                      ))
              contacts))

(defun vdirel-helm-insert-contact-email (candidate)
  "Print selected contacts as comma-separated text.
CANDIDATE is ignored."
  (ignore candidate)
  (insert (mapconcat (lambda (contact)
                       (if (vdirel-contact-fullname contact)
                           (format "%S <%s>"
                                   (vdirel-contact-fullname contact)
                                   (car (vdirel-contact-emails contact)))
                         (format "<%s>"
                         (car (vdirel-contact-emails contact)))))
                     (helm-marked-candidates)
                     ", ")))

(defun vdirel-helm-run-compose-mail-to ()
  "Wrapper around `vdirel-helm-compose-mail-to'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'vdirel-helm-compose-mail-to)))
(put 'helm-ff-run-query-replace-on-marked 'vdirel-helm-run-compose-mail-to t)

(defun vdirel-helm-compose-mail-to (_candidate &optional all-mails)
  "Compose mail and make selected candidates recipients.
Use only the first mail adress of selected candidate if ALL-MAILS
is nil and insert all mail adresses otherwise. CANDIDATE is
ignored."
  (interactive)
  (ignore _candidate)
  (let
      ((candidate (cond (_candidate _candidate)
                        (t (helm-marked-candidates))))
       (mails-list-of-list
    (mapcar (lambda (candidate)
              (mapcar
               (lambda (mail)
                (format "\"%s\" <%s>"
                        (vdirel-contact-fullname candidate)
                        mail))
               (vdirel-contact-emails candidate)))
              (helm-marked-candidates)))
   (mails-list)
   (filter-mails-adr (cond (all-mails #'identity) (t (lambda (x) (list (car x)))))))
  (dolist (mails mails-list-of-list)
    (setq mails-list (append mails-list (funcall filter-mails-adr mails))))
  (compose-mail (mapconcat 'identity mails-list ", "))
  ))


(defun vdirel-helm-open-contact-vcard (candidate)
  "Open vcard file of CANDIDATE.
Open vcard file if it is not already in the persistent window and
revert to the previous buffer otherwise."
  (let* ((current (window-buffer helm-persistent-action-display-window))
         (filepath  (vdirel-get-contact-property "VDIREL-FILENAME" candidate))
         (bufname (file-name-nondirectory filepath)))
      (if (or (helm-follow-mode-p)
              (eql current (get-buffer helm-current-buffer))
              )
          (find-file filepath)
        (switch-to-buffer helm-current-buffer))))

(defun vdirel-helm-create-contact-vcard (candidate)
  "Create vcard file of CANDIDATE.
Ask for a repository name and store CANDIDATE in the corresponding vdir."
  (interactive)
  (let* ((reponame
          (completing-read "Create vcard in repo:" (vdirel-cached-repository-names) #'identity t))
         (repo (vdirel-get-repository-from-name reponame))
         (repodir (vdirel-get-repository-dir repo))
         (UID (org-id-uuid))
         (filepath (format "%s/%s.vcf" repodir UID)))
    (find-file filepath)
    (yas-expand-snippet-by-name "complete vcard")
    ))

(defvar helm-vdirel-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'vdirel-helm-run-open-org-entry-vcard)
    (define-key map (kbd "C-c v") 'vdirel-helm-run-open-org-entry-vcard)
    (define-key map (kbd "C-c c") 'vdirel-helm-run-create-contact-vcard)
    (define-key map (kbd "C-c m") 'vdirel-helm-run-compose-mail-to)
    (delq nil map))
  "Keymap for `helm-vdirel-select-contact'.")

(defun vdirel-helm-run-open-org-entry-vcard ()
  "Execute `vdirel-helm-open-org-entry-vcard' without quitting."
    (interactive)
    (with-helm-alive-p
      (helm-attrset 'open-org 'vdirel-helm-open-org-entry-vcard)
      (helm-execute-persistent-action 'vdirel-helm-open-org-entry-vcard)))
(put 'vdirel-helm-run-open-org-entry-vcard 'helm-only t)

(defun vdirel-helm-run-open-contact-vcard ()
  "Execute `vdirel-helm-open-contact-vcard' without quitting."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'open-vcard 'vdirel-helm-open-contact-vcard)
    (helm-execute-persistent-action 'open-vcard)))
(put 'vdirel-helm-run-open-contact-vcard 'helm-only t)

(defun vdirel-helm-run-create-contact-vcard ()
  "Execute `vdirel-helm-create-contact-vcard' without quitting."
  (interactive)
    (helm-exit-and-execute-action #'vdirel-helm-create-contact-vcard))
(put 'vdirel-helm-run-create-contact-vcard 'helm-only t)

(defun vdirel-helm-open-org-entry-vcard (candidate)
  "Open org file for CANDIDATE."
  (interactive)
  (let* ((repo (vdirel-get-contact-property "VDIREL-REPOSITORY" candidate))
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
      (if (cond ((eql vdirel-create-org-headline-if-needed 'ask)
                 (yes-or-no-p "Headline does not exists. Create it?"))
                (t vdirel-create-org-headline-if-needed))
          (progn
            (find-file orgfilename)
            (goto-char (point-max))
            (if (not (bolp)) (insert "\n"))
            (insert
             (vdirel-expand-template
              vdirel-org-entry-template
              (nconc `(:full-name
                       ,(vdirel-contact-fullname candidate)
                       :UID
                       ,(or (vdirel-contact-uid candidate) (org-id-uuid)))
                    (vdirel-alist-plist candidate t))
              "??"
              )))
        (message "vdirel: CUSTOM_ID %s in %s not found." custid orgfilename)
        )
      )))

(defun vdirel-cached-repository-names ()
  "Return the list of names of the defined repositories.
See `vdirel-repositories'."
  (interactive)
  (seq-uniq
   (mapcar 'vdirel-get-repository-name vdirel-repositories)
   #'string=)
  )

(defun helm-vdirel-select-contact (&optional refresh)
  "Prompt for a contact from the vdir repos using helm.
With a prefix argument REFRESH, first recreate the cache. With a
double prefix argument REFRESH, first sync the vdir and recreate
the cache."
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
    (dolist (repo  vdirel-repositories)
      (setq helm-source-vdirel
            (add-to-list
             'helm-source-vdirel
             (helm-build-sync-source (vdirel-get-repository-name repo)
               :header-name (lambda (s)
                              (format
                               "%s (vdir repo %S)"
                               s
                               (vdirel-get-repository-dir
                                (vdirel-get-repository-from-name
                                 s))))
               :candidates (vdirel-helm-email-candidates (vdirel-get-contacts-from-cache repo))
               :action (helm-make-actions
                        "INSERT" 'vdirel-helm-insert-contact-email
                        "VISIT VCARD" 'vdirel-helm-open-contact-vcard
                        "VISIT ORG" 'vdirel-helm-open-org-entry-vcard
                        "COMPOSE MAIL" 'vdirel-helm-compose-mail-to
                        )
               :persistent-action  'vdirel-helm-open-contact-vcard
               :persistent-help "Toggle showing vcard in current window"
               :keymap 'helm-vdirel-map
               )
             t
             )))
    (helm :prompt "Contacts: "
          :buffer "*helm vdirel*"
          :sources helm-source-vdirel
          :history 'vdirel-helm-input-history
          )
    ))

(set-face-attribute 'helm-source-header nil
                    :background "orange3"
                    :height 250)

(defun vdirel-alist-plist (alist &optional symbol_keys)
  "Alist to plist converter.
Returns a property list containing the same keys and values as
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

(defun helm-mu-contacts-init ()
  "Retrieves contacts from mu."
  (let* ((cmd (concat
              mu4e-mu-binary
              " cfind --format=mutt-ab  "
              helm-mu-command-arguments
              (if helm-mu-contacts-personal " --personal" "")
              (format
               " --after=%d"
               (truncate (float-time (date-to-time helm-mu-contacts-after))))))
         (contacts (cdr (split-string (shell-command-to-string cmd) "\n")))
         (contact-alist)
         (res))
    (dolist (contact contacts res)
      (setq contact-alist nil)
      (pcase (split-string contact "\t")
        ((and `(,mail . ,l) (guard
                             (string-match-p
                             vdirel-hide-adress-regexp
                             mail))))
        ((or `(,mail ,fullname ,_) `(,fullname ,mail))
         (push  (cons "FN"  fullname) contact-alist)
         (push  (cons "EMAIL"  mail) contact-alist)
         (push '("VDIREL-REPOSITORY" . (:name "Mu database" :dir nil :org-filename nil)) contact-alist))
        ((and `(,mail) (guard (and (not (null mail)) (not (string= mail "")))))
         (push  (cons "EMAIL"  mail) contact-alist)
         (push '("VDIREL-REPOSITORY" . (:name "Mu database" :dir nil :org-filename nil)) contact-alist)))
      (when contact-alist (push contact-alist res))
      )
    res
    ))

(provide 'vdirel)
;;; vdirel.el ends here
