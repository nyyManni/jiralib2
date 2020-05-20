;;; jiralib2.el --- JIRA REST API bindings to Elisp

;; Copyright (C) 2017 Henrik Nyman

;; Author: Henrik Nyman <h@nyymanni.com>
;; URL: https://github.com/nyyManni/jiralib2
;; Keywords: comm, jira, rest, api
;; Version: 1.0
;; Package-Requires: ((emacs "25") (request "0.3") (dash "2.14.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a programatic interface to JIRA.  It provides access to
;; JIRA from other programs, but no user level functionality.

;; jiralib2 supports three methods of authentication: cookie, basic and token.
;; Cookie auth is the same which is used in the browser, and works by
;; requesting a session id from the API. Basic auth works by including the
;; Authorization-header in all the requests. Token authentication is similar
;; to the basic authentication, but uses a server-side generated token instead
;; of the password, and is only available with JIRA Cloud.
;; OAuth login is not supported.

;; Jira References:

;; Primary reference (on current Jira, only REST is supported):
;; https://docs.atlassian.com/jira/REST/cloud/

;;; Code:

(require 'request)
(require 'dash)
(require 'json)
(require 'url-parse)

(defgroup jiralib2 nil
  "Jiralib2 customization group."
  :group 'applications)

(defcustom jiralib2-url "http://localhost:8080/"
  "The address of the jira host."
  :type 'string
  :group 'jiralib2)

(defcustom jiralib2-user-login-name nil
  "Username to use to login into JIRA."
  :group 'jiralib2
  :type 'string)

(defcustom jiralib2-user-account-id nil
  "Account ID to use for calls to JIRA API."
  :group 'jiralib2
  :type 'string)

(defcustom jiralib2-auth 'cookie
  "Authentication mode for JIRA."
  :group 'jiralib2
  :type '(choice
	  (const :tag "Cookie authentication" cookie)
	  (const :tag "Token authentication" token)
	  (const :tag "Basic authentication" basic)))

(defvar jiralib2-json-array-type 'list
  "Set the sequene type used for json parsing.")

(defvar jiralib2-token nil
  "Authentication token used by token auth.")

(defvar jiralib2--session nil
  "Contains the cookie of the active JIRA session.")

(defvar jiralib2-post-login-hook nil
  "Run after a successful login has been performed.")

(defun jiralib2-session-login (&optional username password)
  "Login to JIRA with USERNAME and PASSWORD. Save cookie in `jiralib2--session'."
  (interactive)
  (setq jiralib2--session
        (let* ((username (or username
                             jiralib2-user-login-name
                             (read-string "Username: ")))
               (password (or password
                             (and (eq jiralib2-auth 'token) jiralib2-token)
                             (read-passwd (format "Password or token for user %s: "
                                                  username)))))
          (cond ((member jiralib2-auth '(basic token))
                 (base64-encode-string (format "%s:%s" username password) t))
                ((eq jiralib2-auth 'cookie)
                 (let* ((json-array-type jiralib2-json-array-type)
                        (reply-data
                         (request (concat jiralib2-url "/rest/auth/1/session")
                                  :type "POST"
                                  :headers `(("Content-Type" . "application/json"))
                                  :parser 'json-read
                                  :sync t
                                  :data (json-encode `((username . ,username)
                                                       (password . ,password)))))
                        (status-code (request-response-status-code reply-data))
                        (auth-info (cdar (jiralib2--verify-status reply-data)))
                        (session-token (format "%s=%s"
                                               (cdr (assoc 'name auth-info))
                                               (cdr (assoc 'value auth-info)))))
                   session-token)))))
  (run-hooks 'jiralib2-post-login-hook))

(defun jiralib2--verify-status (response)
  "Check status code of RESPONSE, return data or throw an error."
  (let ((status-code (request-response-status-code response)))
    (cond ((not status-code)
           (user-error "Request failed: Could not reach the server"))

          ((= status-code 401)
           (user-error "Request failed: invalid password"))

          ;; Several failed password attempts require you to answer
          ;; a captcha, that must be done in the browser.
          ((= status-code 403)
           (user-error "Login denied: please login in the browser"))

          ((= status-code 404)
           (user-error "Request failed: Wrong URL path"))

          ((and (>= status-code 400) (< status-code 500))
           (user-error "Request failed: invalid request: %s"
                       (request-response-data response)))

          ((>= status-code 500)
           (error "Request failed: Server error"))

          ;; status codes 200 - 399 should be ok.
          (t (request-response-data response)))))

(defun jiralib2-get-user-info ()
  "Fetch information on currently logged in user."
  (jiralib2-session-call "/rest/api/2/myself"))

(defun jiralib2-get-user-account-id ()
  "Fetch Account ID of currently logged in user."
  (cdr (assoc 'accountId (jiralib2-session-call "/rest/api/2/myself"))))

(defun jiralib2-verify-setup ()
  "Verify that server and login are configured correctly."
  (interactive)
  (let ((info (jiralib2-get-user-info)))
    (message
     "Successfully logged in\n\nFull Name:  %s\nEmail:      %s\nAccount ID: %s\n"
     (alist-get 'displayName info)
     (alist-get 'emailAddress info)
     (alist-get 'accountId info))))

(defun jiralib2--session-call (path args)
  "Do a call to PATH with ARGS using current session.
Does not check for session validity."
  (let ((json-array-type jiralib2-json-array-type))
    (apply #'request (concat jiralib2-url path)
           :headers `(("Content-Type" . "application/json")
                      ,(cond ((eq jiralib2-auth 'cookie)
                              `("cookie" . ,jiralib2--session))
                             ((member jiralib2-auth '(basic token))
                              `("Authorization" . ,(format "Basic %s"
                                                           jiralib2--session)))))
           :sync t
           :parser 'json-read
           args)))

(defun jiralib2-session-call (path &rest args)
  "Do a call to PATH with ARGS using current session.
If no session exists, or it has expired, login first."
  (unless jiralib2--session
    (jiralib2-session-login))

  (let ((response (jiralib2--session-call path args)))

    (unless (request-response-status-code response)
      (user-error "Call failed: Could not reach the server"))

    ;; The session has probably expired. Login and try again.
    (when (= (request-response-status-code response) 401)
      (message "Session expired, retrying...")
      (jiralib2-session-login)
      (setq response (jiralib2--session-call path args)))
    (jiralib2--verify-status response)))

(defun jiralib2-get-issue (issue-key)
  "Get the issue with key ISSUE-KEY."
  (jiralib2-session-call (format "/rest/api/2/issue/%s" issue-key)))

(defun jiralib2-add-comment (issue-key body)
  "Add comment to issue ISSUE-KEY with contents BODY."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/comment" issue-key)
                         :type "POST"
                         :data (json-encode `((body . ,body)))))


(defun jiralib2-delete-comment (issue-key comment-id)
  "Remove comment COMMENT-ID from issue ISSUE-KEY."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/comment/%s"
                                 issue-key comment-id)
                         :type "DELETE"))

(defun jiralib2-edit-comment (issue-key comment-id body)
  "Update comment COMMENT-ID from issue ISSUE-KEY with body BODY."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/comment/%s"
                                 issue-key comment-id)
                         :type "PUT"
                         :data (json-encode `((body . ,body)))))

(defun jiralib2-get-comment (issue-key comment-id)
  "Get comment COMMENT-ID of issue ISSUE-KEY."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/comment/%s"
                                 issue-key comment-id)))


(defun jiralib2--get-users (project-key)
  "Download assignable users information given the PROJECT-KEY."
  (let ((offset 0)
        (users nil)
        (max-results 1000)
        (results nil)
        (fmt "/rest/api/2/user/assignable/search?project=%s&maxResults=%d&startAt=%d"))

    (setq results (jiralib2-session-call (format fmt project-key max-results offset))
          users (append users results)
          offset (+ offset max-results))
    (while (= (length results) max-results)
      (setq results (jiralib2-session-call (format fmt project-key max-results offset))
            users (append users results)
            offset (+ offset max-results))
      )
    users))

(defvar jiralib2--users-cache nil)
(defun jiralib2-get-users (project-key)
  "Return assignable users information given the PROJECT-KEY."

  (or jiralib2--users-cache
      (setq jiralib2--users-cache 
            (jiralib2--get-users project-key))))

(defun jiralib2-get-assignable-users (issue-key)
  "Get the assignable users for ISSUE-KEY."
  (let ((offset 0)
        (users nil)
        (max-results 1000)
        (results nil)
        (fmt "/rest/api/2/user/assignable/search?issueKey=%s&maxResults=%d&startAt=%d"))

    (setq results (jiralib2-session-call (format fmt issue-key max-results offset))
          users (append users results)
          offset (+ offset max-results))

    (while (= (length results) max-results)
      (setq results (jiralib2-session-call (format fmt issue-key max-results offset))
            users (append users results)
            offset (+ offset max-results))
      )
    users))


(defun jiralib2-assign-issue (issue-key account-id)
  "Assign issue with ISSUE-KEY to account-id."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/assignee" issue-key)
                         :type "PUT"
                         :data (json-encode `((accountId . ,account-id)))))

(defun jiralib2-do-jql-search (jql &optional limit)
  "Run a JQL query and return the list of issues that matched.
LIMIT is the maximum number of queries to return. Note that JIRA has an internal
limit of how many queries to return, as such, it might not be possible to find
*ALL* the issues that match a query.

DEPRECATED, use `jiralib2-jql-search' instead."
  (unless (or limit (numberp limit))
    (setq limit 1000))
  (append
   (cdr
    (assoc 'issues
           (jiralib2-session-call "/rest/api/2/search"
                                  :type "POST"
                                  :data (json-encode
                                         `((jql . ,jql)
                                           (maxResults . ,limit))))))
   nil))

(defun jiralib2-jql-search (jql &rest fields)
  "Run a JQL query and return the list of issues that matched.
FIELDS specify what fields to fecth.
JIRA has a limit on how many issues can be retrieved at once, and if the query
matches for more than that, all the results are fetched with multiple queries."
  (let ((total nil)
        (offset 0)
        (issues nil))
    (while (or (not total) (< (length issues) total))
      (let ((reply (jiralib2-session-call "/rest/api/2/search"
                                    :type "POST"
                                    :data (json-encode
                                           `((jql . ,jql)
                                             (startAt . ,offset)
                                             (maxResults . 1000)
                                             (fields . ,fields))))))
        (unless total (setq total (alist-get 'total reply)))
        (setq issues (-concat issues (alist-get 'issues reply)))
        (setq offset (length issues))))
    issues))

(defun jiralib2-get-actions (issue-key)
  "Get available actions for the issue ISSUE-KEY.
The issues are returned as a list of ((name . <name>) (id . <id>)) alists."
  (mapcar
   (lambda (trans)
     `(,(cdr (assoc 'id trans)) . ,(cdr (assoc 'name trans))))
   (cdadr
    (jiralib2-session-call (format "/rest/api/2/issue/%s/transitions"
                                   issue-key)))))

(defun jiralib2-do-action (issue-key action-id)
  "Move the issue ISSUE-KEY to another state with action ACTION-ID."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/transitions" issue-key)
                         :type "POST"
                         :data (json-encode `((transition . ((id . ,action-id)))))))


(defun jiralib2-get-worklog (issue-key &optional only-mine)
  "Get worklogs of the issue ISSUE-KEY.
With ONLY-MINE set to t, only return worklogs logged by me."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/worklog" issue-key)))

(defun jiralib2-add-worklog (issue-key timestamp seconds message)
  "Add a worklog to issue ISSUE-KEY with message MESSAGE.
Use TIMESTAMP as start time and SECONDS as amount of logged work in seconds."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/worklog" issue-key)
                         :type "POST"
                         :data (json-encode `((comment . ,message)
                                              (started . ,timestamp)
                                              (timeSpentSeconds . ,seconds)))))

(defun jiralib2-get-project (key)
  "Get details of project KEY."
  (jiralib2-session-call (format "/rest/api/2/project/%s" key)))

(defvar jiralib2--projects-cache nil)
(defun jiralib2-get-projects ()
  "Get a list of all projects."
  (or jiralib2--projects-cache
      (setq jiralib2--projects-cache
            (jiralib2-session-call "/rest/api/2/project"))))

(defvar jiralib2--issuetypes-cache nil)
(defun jiralib2-get-issuetypes ()
  "Get a list of all issuetypes."
  (or jiralib2--issuetypes-cache
      (setq jiralib2--issuetypes-cache
            (jiralib2-session-call "/rest/api/2/issuetype"))))

(defun jiralib2-create-issue (project-id type summary description &rest args)
  "Create a new issue.
Issue of type TYPE gets created in PROJECT-ID with SUMMARY and DESCRIPTION.
ARGS is an association list containing extra attributes for the call."
  (jiralib2-session-call "/rest/api/2/issue/"
                         :type "POST"
                         :data (json-encode
                                `((fields . ((project . ((key . ,project-id)))
                                             (issuetype . ((name . ,type)))
                                             (summary . ,summary)
                                             (description . ,description)
                                             ,@args))))))

(defun jiralib2-update-issue (issue-id &rest args)
  "Update the issue ISSUE-ID with data ARGS.
ARGS is an association list of the fields to set for the issue."
  (jiralib2-session-call (format "/rest/api/2/issue/%s" issue-id)
                         :type "PUT"
                         :data (json-encode
                                `((fields . ,args)))))

(defun jiralib2-update-summary-description (issue-id summary description)
  "Change the summary and description of issue ISSUE-ID to SUMMARY and DESCRIPTION."
  (jiralib2-update-issue issue-id
                         `(description . ,description)
                         `(summary . ,summary)))

(defun jiralib2-set-issue-type (issue-id type)
  "Change the issue type of ISSUE-ID to TYPE."
  (jiralib2-update-issue issue-id `(issuetype . ((name . ,type)))))

(defun jiralib2-if-plan-issue (issue-id datestring)
  "Plan issue for next week and IntraFind PSO team"
  (jiralib2-session-call (format "/rest/api/2/issue/%s" issue-id)
                         :type "PUT"
                         :data (json-encode
                                `((fields . ((customfield_10131  ((value . "Ja")))
                                             (customfield_10122 . ,datestring)))))))

(defun jiralib2-if-unplan-issue(issue-id)
  "Unplan issue, removing toggle and date."
  (jiralib2-session-call (format "/rest/api/2/issue/%s" issue-id)
                         :type "PUT"
                         :data (json-encode
                                `((fields . ((customfield_10131  ((value . ,json-null)))
                                             (customfield_10122 . ,json-null)))))))

(defun jiralib2-board-issues (board-id args)
  "Get issues of board BOARD-ID. Restrict the fetched fields with ARGS."
  (alist-get 'issues
             (jiralib2-session-call
              (format "/rest/agile/1.0/board/%s/issue?%s" board-id
                      (request--urlencode-alist args)))))

(defun jiralib2-session-logout ()
  "Close the current session."
  (when (eq jiralib2-auth 'cookie)
    (jiralib2-session-call "/rest/auth/1/session"
                           :type "DELETE"))

  (setq jiralib2--issuetypes-cache nil
        jiralib2--projects-cache   nil
        jiralib2--users-cache      nil
        jiralib2--session          nil))

(provide 'jiralib2)
;;; jiralib2.el ends here
