(defun my/load-erlang-path ()
  "Load Erlang OTP bin path depending on OS."
  (interactive)
  (let* ((erl-bin
          (pcase system-type
            ;; Windows
            ('windows-nt
             (let ((userprofile (getenv "USERPROFILE")))
               (concat userprofile
                       "/.elixir-install/installs/otp/27.3.4/bin")))
            ;; Linux
            ('gnu/linux
             (expand-file-name "~/.elixir-install/installs/otp/27.3.4/bin"))
            ;; macOS
            ('darwin
             (expand-file-name "~/.elixir-install/installs/otp/27.3.4/bin"))
            (_
             (error "Unsupported system: %s" system-type)))))
    (unless (file-directory-p erl-bin)
      (error "Erlang bin not found: %s" erl-bin))
    (add-to-list 'exec-path erl-bin)
    (setenv "PATH"
            (concat erl-bin
                    (if (eq system-type 'windows-nt) ";" ":")
                    (getenv "PATH")))
    (message "Erlang loaded (%s): %s" system-type erl-bin)))
