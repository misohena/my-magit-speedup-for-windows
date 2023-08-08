;;; my-magit-process-cache.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage

;; To make it faster only when creating commits, put the following
;; code in your init.el.
;;
;; (with-eval-after-load "magit"
;;   (require 'my-magit-process-cache)
;;   (my-magit-process-cache-commit-msg-mode 1))

;; For overall speedup, put the following code in your init.el instead
;; of the above code. However, there is no guarantee that it will work
;; correctly.
;;
;; (with-eval-after-load "magit"
;;   (require 'my-magit-process-cache)
;;   (my-magit-process-cache-clear-on-refresh-mode 1))

;; To always enable caching, run the following command.
;;
;; M-: (my-magit-process-cache-turn-on)
;;
;; However, once cached, the results do not change, so Magit will not
;; behave correctly after making changes to the repository.
;; In that case, you need to manually clear cache or disable
;; caching. (i.e. almost useless)
;;
;; M-x my-magit-process-cache-clear-cache-all
;; M-x my-magit-process-cache-turn-off
;;
;; If you can pinpoint exactly when the cache is invalidated, you'll
;; probably get the fastest performance.

;;; Code:

(require 'subr-x)
(require 'magit)
(require 'cl-lib)

(defgroup my-magit-process-cache nil
  "Globally cache the results of git process calls for Magit."
  :prefix "my-magit-process-cache-"
  :group 'magit
  :group 'tools)

;;;; Cache Management

(defvar my-magit-process-cache--cache nil)

(defvar my-magit-process-cache--keep-args-regexp nil)

(defun my-magit-process-cache--current-key (args output-type)
  "Create a cache key."
  (list (file-name-as-directory (expand-file-name default-directory)) ;;~/foo => c:/home/user/foo/
        args
        output-type))

(defsubst my-magit-process-cache--key-dir (key) (nth 0 key))
(defsubst my-magit-process-cache--key-args (key) (nth 1 key))
(defsubst my-magit-process-cache--key-output-type (key) (nth 2 key))

(defun my-magit-process-cache--get-cache (key)
  "Get cached result."
  (let ((key-dir (car key))
        (key-rest (cdr key)))
    (alist-get key-rest
               (alist-get key-dir my-magit-process-cache--cache nil nil #'equal)
               nil nil #'equal)))

(defun my-magit-process-cache--add-cache (key result)
  "Add RESULT as cache."
  (let ((key-dir (car key))
        (key-rest (cdr key)))
    (push (cons key-rest result)
          (alist-get key-dir my-magit-process-cache--cache nil nil #'equal)))
  result)

(defun my-magit-process-cache--remove-cache-if-not (pred)
  (dolist (dir-cache my-magit-process-cache--cache)
    (let ((dir (car dir-cache)))
      (setcdr dir-cache (seq-filter (lambda (key-rest-result) (funcall pred (cons dir (car key-rest-result)))) (cdr dir-cache)))))
  (setq my-magit-process-cache--cache
        (seq-filter #'cdr my-magit-process-cache--cache)))

(defun my-magit-process-cache--clear-cache-all-forced ()
  (setq my-magit-process-cache--cache nil))

(defun my-magit-process-cache--clear-cache-all-default ()
  (if my-magit-process-cache--keep-args-regexp
      (my-magit-process-cache--remove-cache-if-not
       (lambda (key)
         (string-match-p my-magit-process-cache--keep-args-regexp
                         (mapconcat #'shell-quote-argument (my-magit-process-cache--key-args key) " "))))
    (my-magit-process-cache--clear-cache-all-forced)))

(defun my-magit-process-cache-clear-cache-all ()
  "Clear all cache."
  (interactive)
  (my-magit-process-cache--clear-cache-all-forced))

(defun my-magit-process-cache-clear-cache-dir (directory)
  "Clear cache for the DIRECTORY."
  (interactive (list default-directory))
  (setf (alist-get directory my-magit-process-cache--cache nil 'remove #'equal) nil))


;;;; Output Buffer (destination argument of call-process)

(defun my-magit-process-cache--get-buffer (destination &optional create-p)
  "Returns the buffer corresponding to DESTINATION.
DESTINATION is the destination argument of call-process."
  (when (consp destination)
    (setq destination (car destination))) ;; buffer/string/t/nil/0/:file
  (cond
   ((bufferp destination) destination)
   ((stringp destination) (if create-p (get-buffer-create destination) (get-buffer destination)))
   ((eq destination t) (current-buffer))
   (t nil)))

(defsubst my-magit-process-cache--get-buffer-create (destination)
  "Returns the buffer corresponding to DESTINATION.
DESTINATION is the destination argument of call-process.
Create a new buffer if necessary."
  (my-magit-process-cache--get-buffer destination t))

(defun my-magit-process-cache--output-type (destination)
  "Returns the type of output corresponding to DESTINATION.
DESTINATION is the destination argument of call-process.

Returns one of the following:

  \\='discard = Output is discarded.
  \\='mix = Stdout and stderr are written to the same buffer.
  \\='stdout-only = Only stdout is written to the buffer.
  \\='unknown = other situation.

If DESTINATION includes a file name, returns \\='unknown. "
  ;; call-processにDESTINATION引数を渡したときの出力結果の種類を返します。
  ;;
  ;;   discard = 出力は破棄されます。
  ;;   mix = 標準出力と標準エラー出力は同一のバッファに出力されます。
  ;;   stdout-only = 標準出力のみバッファに出力されます。
  ;;   unknown = その他の状況です。
  ;;
  ;; 出力先にファイルが指定されている場合、キャッシュの対象にするのは面倒なので
  ;; 一律unknownを返しキャッシュ無しでの実行を促します。

  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Synchronous-Processes.html
  ;; buffer => mix
  ;; string => mix
  ;; t => mix
  ;; nil => discard
  ;; 0 => discard
  ;; (:file file-name) => unknown
  ;; (real err)
  ;;   (nil nil) => discard
  ;;   (nil t) => discard (2>&1 >null ?)
  ;;   (nil "file") => unknown (1>null 2>file !?)
  ;;   (t/buffer/string nil) => stdout-only
  ;;   (t/buffer/string t) => mix
  ;;   (t/buffer/string "file") => unknown
  (let* ((separate-p (and (consp destination)
                          (not (keywordp (car destination)))))
         ;; buffer/string/t/nil/0/(:file ...)
         (real-dst (if separate-p (car destination) destination))
         ;; nil:discard/t:mix/string:file
         (error-dst (if separate-p (cadr destination) t)))
    (pcase real-dst
      ((or 'nil 0)
       (pcase error-dst
         ('nil 'discard)
         ('t 'discard) ;; 2>&1 >null ?
         ((pred stringp) 'unknown) ;; 1>null 2>file !?
         (_ 'unknown)))
      ((or (pred bufferp) (pred stringp) 't)
       (pcase error-dst
         ('nil 'stdout-only)
         ('t 'mix)
         (_ 'unknown))) ;;file?
      (_ 'unknown)))) ;;file?


;;;; Execution and Result

(defsubst my-magit-process-cache--result (exit-status output-string)
  (cons exit-status output-string))

(defsubst my-magit-process-cache--result-exit-status (result)
  (car result))

(defsubst my-magit-process-cache--result-output-string (result)
  (cdr result))

(defun my-magit-process-cache--execute (original-call-process
                                        program infile destination display args)
  ;; プロセスを呼び出し、結果を (exit-status output-string) の形で返します。
  (let* ((begin
          (when-let ((buffer (my-magit-process-cache--get-buffer destination))) ;;exists or not?
            (with-current-buffer buffer (point))))
         (exit-status
          (apply original-call-process program infile destination display args))
         (output-string
          (when-let ((buffer (my-magit-process-cache--get-buffer destination)))
            (with-current-buffer buffer (buffer-substring (or begin (point-min)) (point))))))
    (my-magit-process-cache--result exit-status output-string)))


;;;; Cache Target

(defvar my-magit-process-cache--target 'all)

(defun my-magit-process-cache--target-p (program destination args)
  (pcase my-magit-process-cache--target
    ('all t)
    ('t t)
    ('nil nil)
    ((pred functionp)
     (funcall my-magit-process-cache--target program destination args))
    ((pred stringp) ;;regexp
     (string-match-p
      my-magit-process-cache--target
      (mapconcat #'shell-quote-argument args " ")))))

;;;; Overriding call-process

;; Non-nilの場合キャッシュされた結果と最新の実行結果を比較して異なって
;; いたらメッセージを出力する。
;; キャッシュされていても再度実行するので当然遅くなる。
;; デバッグ用。
(defconst my-magit-process-cache--validate-cached-results nil)

(defun my-magit-process-cache--call-process
    (original-call-process program infile destination display &rest args)
  ;; call-processに対する:around adviceです。
  ;;
  ;; Gitの呼び出し結果をキャッシュするよう試みます。
  ;;
  ;; 次の時はキャッシュ等余計なことはせず単にcall-processの処理を行います。
  ;; - PROGRAMがmagit-git-executable変数の値ではない(この関数はローカルディレクトリの時だけ呼ばれるので(magit-git-executable)関数ではない)
  ;; - INFILEが指定されている(ファイル入力はサポートしない)
  ;; - DISPLAYが指定されている(どのような処理をすべきかよく分からないので)
  ;; - DESTINATIONがサポートしていない形式(ファイル出力等面倒なので)
  ;;
  ;; キャッシュは情報をキーとします。
  ;; - default-directory(カレントディレクトリ)
  ;; - ARGS
  ;; - DESTINATIONの値に基づく出力の組み合わせ(discard, mix, stdout-only)
  ;; (現在の所)環境変数は考慮しません。

  (let ((output-type
         (if (or (not (equal program magit-git-executable)) ;; not git (magit-git-executable = local only)
                 infile ;; input file specified
                 display ;; redisplay required
                 (not (my-magit-process-cache--target-p program destination args)))
             'unknown
           (my-magit-process-cache--output-type destination))))
    (if (eq output-type 'unknown)
        ;; Don't use cache
        (apply original-call-process program infile destination display args)

      (let* ((key (my-magit-process-cache--current-key args output-type))
             (cached-result (my-magit-process-cache--get-cache key)))
        (if cached-result
            ;; Use cached result
            (if my-magit-process-cache--validate-cached-results
                ;; Validate cached-result (For debug)
                (let ((result (my-magit-process-cache--execute original-call-process program infile destination display args)))
                  (unless (equal result cached-result)
                    (message "Incorrect cached result found!!\n  key=%s\n  result:%s\n  cached:%s" key result cached-result))
                  (my-magit-process-cache--result-exit-status result))
              ;; Insert output-string to destination buffer and return
              ;; exit-status
              (when-let ((buffer (my-magit-process-cache--get-buffer-create destination)))
                (with-current-buffer buffer
                  (insert (my-magit-process-cache--result-output-string cached-result))))
              (my-magit-process-cache--result-exit-status cached-result))
          ;; Create a new cache entry
          (let ((result (my-magit-process-cache--execute original-call-process program infile destination display args)))
            (my-magit-process-cache--add-cache key result)
            (my-magit-process-cache--result-exit-status result)))))))

(defun my-magit-process-cache-turn-on ()
  (my-magit-process-cache--clear-cache-all-default)
  (advice-add #'call-process :around #'my-magit-process-cache--call-process))

(defun my-magit-process-cache-turn-off ()
  (interactive)
  (my-magit-process-cache--clear-cache-all-default)
  (advice-remove #'call-process #'my-magit-process-cache--call-process))


;;;; Global Mode

;;;;; Manual Cache Clearing Mode

(define-minor-mode my-magit-process-cache-mode
  "Enable caching always."
  :type 'boolean
  :group 'my-magit-process-cache
  :global t
  :init-value nil
  (if my-magit-process-cache-mode
      ;; Turn on
      (progn
        (my-magit-process-cache-turn-on)
        (message "Turn on my-magit-process-cache-mode"))
    ;; Turn off
    (my-magit-process-cache-turn-off)
    (message "Turn off my-magit-process-cache-mode")))

;;;;; Mode to clear cache on refresh

(define-minor-mode my-magit-process-cache-clear-on-refresh-mode
  "Enable caching and clear cache on magit-refresh."
  ;; Gitのキャッシュ有効化し、magit-refreshでキャッシュをクリアするようにします。
  ;;
  ;; 速度 : まあまあ
  ;; 正確性 : まあまあ
  ;;
  ;; 速度は常にキャッシュを有効にするよりは遅いです。
  ;; 変更操作を正確に検出できればもっと速くできるでしょう。
  ;;
  ;; 変更操作をしてすぐにmagit-refreshが実行される場合は問題ありませんが、
  ;; 変更操作の直後に変更対象の内容を取得するような場合は間違った動作を
  ;; 引き起こす可能性があります。"
  :type 'boolean
  :group 'my-magit-process-cache
  :global t
  :init-value nil
  (if my-magit-process-cache-clear-on-refresh-mode
      ;; Turn on
      (progn
        (add-hook 'magit-pre-refresh-hook #'my-magit-process-cache--clear-cache-all-default -100) ;;first
        (my-magit-process-cache-turn-on)
        (message "Turn on my-magit-process-cache-clear-on-refresh-mode"))
    ;; Turn off
    (remove-hook 'magit-pre-refresh-hook #'my-magit-process-cache--clear-cache-all-default)
    (my-magit-process-cache-turn-off)
    (message "Turn off my-magit-process-cache-clear-on-refresh-mode")))

;;;;; Mode to cache only on commit

(define-minor-mode my-magit-process-cache-commit-msg-mode
  "Enable caching only when creating an edit buffer for commit messages."
  ;; コミットメッセージの編集バッファを作成するときだけGitのキャッシュ有効化します。
  ;;
  ;; 速度 : 全体的にはいまいち
  ;; 正確性 : ほぼ問題なし
  ;;
  ;; コミット時以外では何も変わりません。
  ;;
  ;; コミット毎にキャッシュがクリアな状態からはじめるので、
  ;; 常にキャッシュを有効にするよりは遅いです。
  ;;
  ;; コミットメッセージの編集バッファを作成する部分には状態の変更操作が無いので
  ;; 間違った動作はしないはずです。
  ;; Magitのコードが変更された場合はその限りではありません。
  :type 'boolean
  :group 'my-magit-process-cache
  :global t
  :init-value nil
  (if my-magit-process-cache-commit-msg-mode
      ;; Turn on
      (progn
        (advice-add #'magit-commit-create :around #'my-magit-process-cache--commit-create)
        (message "Turn on my-magit-process-cache-commit-msg-mode"))
    ;; Turn off
    (my-magit-process-cache--commit-create-end)
    (advice-remove #'magit-commit-create #'my-magit-process-cache--commit-create)
    (message "Turn off my-magit-process-cache-commit-msg-mode")))

(defun my-magit-process-cache--commit-create (original-fun &rest args)
  (my-magit-process-cache-turn-on)

  ;; Reserve cache stop.
  (if (or (memq 'magit-commit-diff with-editor-filter-visit-hook)
          (memq 'magit-commit-diff server-switch-hook))
      ;; If magit-commit-diff is going to be run, it's better after it's done.
      (advice-add #'magit-commit-diff :after #'my-magit-process-cache--commit-create-end)
    ;; Otherwise around the end of git-commit-setup?
    (add-hook 'git-commit-setup-hook #'my-magit-process-cache--commit-create-end 100)) ;;last
  ;; I put it in git-refresh just in case.
  (add-hook 'magit-pre-refresh-hook #'my-magit-process-cache--commit-create-end -100) ;;first

  ;; Call original commit-create.
  (let ((result (apply original-fun args)))
    ;; Cancel if it fails.
    (unless result
      (my-magit-process-cache--commit-create-end))
    result))

(defun my-magit-process-cache--commit-create-end ()
  (advice-remove #'magit-commit-diff #'my-magit-process-cache--commit-create-end)
  (remove-hook 'git-commit-setup-hook #'my-magit-process-cache--commit-create-end)
  (remove-hook 'magit-pre-refresh-hook #'my-magit-process-cache--commit-create-end)
  (my-magit-process-cache-turn-off))

;;;;; Always Cache Mode

(defconst my-magit-process-cache-always-mode--keep-args-regexp
  "\\(\\bconfig \\(core\\.commentchar\\|-z --get-all magit\\.extension\\)\\'\\|\\brev-parse \\(--show-toplevel\\|--git-dir\\|--is-bare-repository\\)\\'\\)") ;;include config --list -z?

(define-minor-mode my-magit-process-cache-always-mode
  ""
  :type 'boolean
  :group 'my-magit-process-cache
  :global t
  :init-value nil
  (if my-magit-process-cache-always-mode
      ;; Turn on
      (progn
        (setq my-magit-process-cache--keep-args-regexp
              my-magit-process-cache-always-mode--keep-args-regexp)
        (setq my-magit-process-cache--target
              my-magit-process-cache-always-mode--keep-args-regexp)
        (my-magit-process-cache-turn-on)
        (advice-add #'magit-commit-create :around #'my-magit-process-cache-always-mode--commit-create)
        (advice-add #'magit-status :before #'my-magit-process-cache-always-mode--clear-all-cache)
        (message "Turn on my-magit-process-cache-always-mode"))
    ;; Turn off
    (setq my-magit-process-cache--keep-args-regexp nil)
    (setq my-magit-process-cache--target 'all)
    (my-magit-process-cache-always-mode--commit-setup-finished)
    (advice-remove #'magit-commit-create #'my-magit-process-cache-always-mode--commit-create)
    (advice-remove #'magit-status #'my-magit-process-cache-always-mode--clear-all-cache)
    (my-magit-process-cache-turn-off)
    (message "Turn off my-magit-process-cache-always-mode")))

(defun my-magit-process-cache-always-mode--commit-create (original-fun &rest args)
  (my-magit-process-cache--clear-cache-all-default)
  (setq my-magit-process-cache--target 'all)
  ;; Reserve cache stop.
  (if (or (memq 'magit-commit-diff with-editor-filter-visit-hook)
          (memq 'magit-commit-diff server-switch-hook))
      ;; If magit-commit-diff is going to be run, it's better after it's done.
      (advice-add #'magit-commit-diff :after #'my-magit-process-cache-always-mode--commit-setup-finished)
    ;; Otherwise around the end of git-commit-setup?
    (add-hook 'git-commit-setup-hook #'my-magit-process-cache-always-mode--commit-setup-finished 100)) ;;last
  ;; I put it in git-refresh just in case.
  (add-hook 'magit-pre-refresh-hook #'my-magit-process-cache-always-mode--commit-setup-finished -100) ;;first

  ;; Call original commit-create.
  (let ((result (apply original-fun args)))
    ;; Cancel if it fails.
    (unless result
      (my-magit-process-cache--commit-create-end))
    result))

(defun my-magit-process-cache-always-mode--commit-setup-finished ()
  (my-magit-process-cache--clear-cache-all-default)
  (setq my-magit-process-cache--target
        my-magit-process-cache-always-mode--keep-args-regexp)

  (advice-remove #'magit-commit-diff #'my-magit-process-cache-always-mode--commit-setup-finished)
  (remove-hook 'git-commit-setup-hook #'my-magit-process-cache-always-mode--commit-setup-finished)
  (remove-hook 'magit-pre-refresh-hook #'my-magit-process-cache-always-mode--commit-setup-finished))

(defun my-magit-process-cache-always-mode--clear-all-cache (&rest _args)
  (my-magit-process-cache--clear-cache-all-forced))


(provide 'my-magit-process-cache)
;;; my-magit-process-cache.el ends here
