;;; my-magit-speedup-settings.el ---                 -*- lexical-binding: t; -*-

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

;; (with-eval-after-load "magit"
;;   (require 'my-magit-sppedup-settings))

;;; Code:

(require 'magit)
(require 'vc-git)
(require 'my-magit-process-cache)

;;;; for vc-git.el

;; ■vc-git-registeredの最適化
;; .git/COMMIT_EDITMSG等を即座にgit登録外と判定します。
;; これによりプロセスの起動回数を削減できます。
;; 警告: もし意図的にgitの管理下で.git/COMMIT_EDITMSGという名前のディレ
;;       クトリ名やファイル名を使いたいならこのコードは問題になります。
;;       (gitがそのようなディレクトリ名を許容するのか知りませんが)
(defun my-vc-git-registered-for-ignoring-commit-filenames (original-fun file)
  (if (string-match-p (concat "\\.git" git-commit-filename-regexp) file)
      nil
    (funcall original-fun file)))
(advice-add #'vc-git-registered :around #'my-vc-git-registered-for-ignoring-commit-filenames)

;;;; for git-commit.el

;; ■メジャーモード無指定
;; コミットメッセージ編集バッファでのメジャーモードの切り替えを一回
;; 抑制できます。
;; 節約できる時間はフックの中に何があるかにもよりますが、最低でも数
;; 十ms、多ければ数百msにもなります。
;; 欠点: コミットメッセージ編集バッファがfundamental-modeになる。
(setq git-commit-major-mode nil)

;; ■bug-reference-mode抑制
;; コミットメッセージ編集バッファでbug-reference-modeが起動するのを
;; 抑制します。
;; これによりプロセスの起動回数を削減できます。
;; 欠点: コミットメッセージでバグ番号をリンク化できない。
(remove-hook 'git-commit-setup-hook 'bug-reference-mode)

;; ■ブランチ名の色分けを抑制
;; コミットメッセージ編集バッファでブランチ名を取得できないようにし
;; ます。
;; これによりプロセスの起動回数を削減できます。
;; 欠点: コミットメッセージ編集バッファでローカルブランチとリモート
;;       ブランチの色分けが行われなくなります。
(defun my-git-commit-setup-font-lock-for-blocking-branch-names-retrieval (original-fun &rest args)
  (cl-letf (((symbol-function 'magit-list-local-branch-names) (lambda () nil)))
    (apply original-fun args)))
(advice-add #'git-commit-setup-font-lock :around #'my-git-commit-setup-font-lock-for-blocking-branch-names-retrieval)

;;;; for magit

;;;;; git commit

;; ;; ■call-processのキャッシュ
;; ;; コミットメッセージ処理時だけ全プロセスの結果をキャッシュします。
;; (my-magit-process-cache-commit-msg-mode 1)
;; →my-magit-process-cache-always-modeへ移行

;; ■call-processのキャッシュ
;; 全期間にわたって、大丈夫そうなものだけキャッシュします。
;; 警告: configやディレクトリ構造が変わったときに正しく動作しなくなります。
;;       その場合はM-x my-magit-process-cache-clear-cache-allを実行するか
;;       git-statusでMagitバッファを作り直して下さい。
(my-magit-process-cache-always-mode 1)

;; ■magit-file-tracked-pで.git/COMMIT_EDITMSGを無視
;; .git/COMMIT_EDITMSG等を即座にgit登録外と判定します。
;; これによりプロセスの起動回数を削減できます。
;; 警告: もし意図的にgitの管理下で.git/COMMIT_EDITMSGという名前のディレ
;;       クトリ名やファイル名を使いたいならこのコードは問題になります。
;;       (gitがそのようなディレクトリ名を許容するのか知りませんが)
(defun my-magit-file-tracked-p-for-ignoring-commit-filenames (original-fun file)
  (if (string-match-p (concat "\\.git" git-commit-filename-regexp) file)
      nil
    (funcall original-fun file)))
(advice-add #'magit-file-tracked-p :around #'my-magit-file-tracked-p-for-ignoring-commit-filenames)

;; ;; ■変わりづらい情報のキャッシュを維持
;; ;; configやリポジトリのディレクトリ構造をキャッシュしたままにします。
;; ;; git-status実行時には全ての情報をクリアします。
;; ;; これによりプロセスの起動回数を削減できます。
;; ;; 警告: configやディレクトリ構造が変わったときに正しく動作しなくなります。
;; ;;       その場合はM-x my-magit-process-cache-clear-cache-allを実行するか
;; ;;       git-statusでMagitバッファを作り直して下さい。
;; (setq my-magit-process-cache--keep-args-regexp "\\(\\bconfig\\b\\|\\brev-parse \\(--show-toplevel\\|--git-dir\\|--is-bare-repository\\)\\'\\)")
;; (defun my-magit-status-for-clearing-cache (&rest _args)
;;   (my-magit-process-cache--clear-cache-all-forced))
;; (advice-add #'magit-status :before #'my-magit-status-for-clearing-cache)
;; →my-magit-process-cache-always-modeへ移行

;; ■staged, unstagedチェックの回避
;; Magitバッファでc c(magit-commit-create)を実行したときに
;; magit-commit-diffの中でmagit-anything-staged-pが必ずt、
;; magit-anything-unstaged-pが必ずnilを返すようにします。
;; これによりプロセスの起動回数を削減できます。
;; 警告: 一部のシチュエーションで正しいdiffが取れない場合があるかも
;;       しれません。
(defun my-magit-commit-diff-1-for-avoid-call-process (original-fun &rest args)
  (let ((command (magit-repository-local-get 'this-commit-command)))
    (if (memq command
              ;; magit-commit-diff-1内に書かれている特別な対応が必要なコマンド一覧
              '(magit-commit--rebase magit-commit-amend magit-commit-reword magit-commit--all handle-switch-frame))
        (apply original-fun args)
      ;; magit-commit-create等特別な対応が必要ないコマンドなら
      ;; 必ずstage=t, unstaged=nilで良い、と思う。
      ;; (nil nil ,_) が気になるけど、そんなシチュエーションあるの？
      (cl-letf (((symbol-function 'magit-anything-staged-p) (lambda () t))
                ((symbol-function 'magit-anything-unstaged-p) (lambda () nil)))
        (apply original-fun args)))))
(advice-add #'magit-commit-diff-1 :around #'my-magit-commit-diff-1-for-avoid-call-process)


;;;;; git commit以外

;; ;; ■magit-auto-revert-modeでvc-git-rootを使う
;; ;; ファイルを開くときに(正確にはメジャーモードが変わるときに) .gitディ
;; ;; レクトリが存在しないときはauto-revert-modeを立ち上げるかどうかのチェッ
;; ;; クを即座に打ち切ります。
;; ;; これによりプロセスの起動回数を削減できます。
;; ;; 警告: もし.gitディレクトリ無しにgit管理下のファイルがある場合は正しく動作しなくなります。
;; (defun my-magit-turn-on-auto-revert-mode-if-desired-for-use-vc-git-root (original-fun &optional file)
;;   (if file
;;       (funcall original-fun file)
;;     (when (and buffer-file-name
;;                (vc-git-root buffer-file-name))
;;       (funcall original-fun file))))
;; (advice-add #'magit-turn-on-auto-revert-mode-if-desired :around #'my-magit-turn-on-auto-revert-mode-if-desired-for-use-vc-git-root)
;; ↓の設定を使うので不要。

;; ■magit-toplevelを不完全だが高速なものに差し替え
;; magit-toplevelは頻繁に呼び出されるので高速化の効果は大きいです。
;; 欠点: 特殊な形式のリポジトリを一切認識しなくなります。
(defun my-magit-toplevel-fast-but-imperfect (original-fun &optional directory)
  (magit--with-refresh-cache
      (cons (or directory default-directory) 'magit-toplevel)
    (magit--with-safe-default-directory directory
      (save-match-data
        (cond
         ;; Remote
         ((file-remote-p default-directory)
          (funcall original-fun directory))
         ;; Submodule
         ((string-match "\\`\\(.*/\\)\\.git/modules/\\(.*\\)\\'" default-directory)
          (concat (match-string 1 default-directory)
                  (match-string 2 default-directory)))
         ;; Does not support:
         ;; - environments for git directory (GIT_DIR, GIT_WORK_TREE, etc)
         ;; - bare repository
         ;; - find-file-visit-truename
         (t
          (when-let ((root-dir (vc-git-root default-directory)))
            (magit-expand-git-file-name root-dir))))))))
(advice-add #'magit-toplevel :around #'my-magit-toplevel-fast-but-imperfect)


(provide 'my-magit-speedup-settings)
;;; my-magit-speedup-settings.el ends here
