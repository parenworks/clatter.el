;;; test-smart.el --- Tests for clatter-smart.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-smart)

(ert-deftest clatter-test-smart-noise-inactive-nick ()
  "A nick with only noisy events is considered noisy."
  (let ((clatter-smart-threshold 0.5)
        (clatter-smart-noise '(join part)))
    (with-temp-buffer
      (should (clatter-smart-eval (current-buffer) "mynick" 'join))
      (should (clatter-smart-eval (current-buffer) "mynick" 'part)))))

(ert-deftest clatter-test-smart-noise-inactive-then-active-nick ()
  "A nick with multiple noisy, then non-noisy, then noisy events transitions
between states based on active-p, then SNR ratios."
  (let ((clatter-smart-threshold 0.5)
        (clatter-smart-noise '(join part away)))
    (with-temp-buffer
      ;; These are all considered noisy as they do not flip the active-p
      ;; flag
      (should (clatter-smart-eval (current-buffer) "mynick" 'join))
      (should (clatter-smart-eval (current-buffer) "mynick" 'away))
      (should (clatter-smart-eval (current-buffer) "mynick" 'away))
      (should (clatter-smart-eval (current-buffer) "mynick" 'away))
      (should (clatter-smart-eval (current-buffer) "mynick" 'away))
      ;; Flip active-p by recording a 'privmsg
      ;; Since active-p is now t, noisiness will be determined solely by SNR.
      ;;
      ;; SNR = 1/5  = 0.2  (PRIVMSG=1; NOISE=5)
      (should (= 0.2 (clatter-smart-put (current-buffer) "mynick" 'privmsg)))
      ;; SNR = 1/6 ~= 1.6  (PRIVMSG=1; NOISE=6)
      (should (clatter-smart-eval (current-buffer) "mynick" 'part))
      ;; SNR = 2/6 ~= 0.33 (PRIVMSG=2; NOISE=6)
      (should (< 0.3 (clatter-smart-put (current-buffer) "mynick" 'privmsg)))
      ;; SNR = 3/6  = 0.5  (PRIVMSG=3; NOISE=6)
      (should (= 0.5 (clatter-smart-put (current-buffer) "mynick" 'privmsg)))
      ;; SNR = 4/6 ~= 0.66 (PRIVMSG=4; NOISE=6)
      (should (< 0.6 (clatter-smart-put (current-buffer) "mynick" 'privmsg)))
      ;; At this point we're above the SNR threshold (0.5), so the following
      ;; 'away shall not be considered noisy.
      ;;
      ;; SNR = 4/7 ~= 0.57 (PRIVMSG=4; NOISE=7)
      (should-not (clatter-smart-eval (current-buffer) "mynick" 'away))
      ;; After another 'away, we're exactly at the cutoff point.
      ;;
      ;; SNR = 4/8  = 0.5  (PRIVMSG=4; NOISE=8)
      (should-not (clatter-smart-eval (current-buffer) "mynick" 'away))
      ;; The next 'away should be deemed noisy, as it pushes us below
      ;; the cutoff point.
      ;;
      ;; SNR = 4/9 ~= 0.44 (PRIVMSG=4; NOISE=9)
      (should (clatter-smart-eval (current-buffer) "mynick" 'away)))))

(ert-deftest clatter-test-smart-noise-preserves-state-across-nick-change ()
  "Nick carries over its SNR and active-p values to the new nick."
  (let ((clatter-smart-noise '(join part quit nick away))
        (clatter-smart-threshold 0.5))
    (with-temp-buffer
      (should (= most-positive-fixnum (clatter-smart-put (current-buffer) "alice" 'privmsg)))
      (should-not (clatter-smart-eval (current-buffer) "alice" "alice_"))
      (should-not (clatter-smart-eval (current-buffer) "alice_" 'part))
      (should (clatter-smart-eval (current-buffer) "alice_" 'part)))))

(ert-deftest clatter-test-smart-activate-hook-fires-once-on-first-signal ()
  "The activate hook fires only on the inactive-to-active transition.
Never-active nicks' noise must not fire it; later signals must not re-fire."
  (let ((clatter-smart-noise '(join part))
        (fires 0))
    (with-temp-buffer
      (let ((probe (lambda (&rest _) (cl-incf fires))))
        (add-hook 'clatter-smart-activate-hook probe)
        (unwind-protect
            (progn
              ;; Pure noise: never active, no fire.
              (should (clatter-smart-eval (current-buffer) "mynick" 'join))
              (should (zerop fires))
              ;; First signal: fires once.
              (clatter-smart-put (current-buffer) "mynick" 'privmsg)
              (should (= 1 fires))
              ;; Subsequent signals and noise: no re-fire.
              (clatter-smart-put (current-buffer) "mynick" 'privmsg)
              (clatter-smart-eval (current-buffer) "mynick" 'part)
              (should (= 1 fires)))
          (remove-hook 'clatter-smart-activate-hook probe))))))

(ert-deftest clatter-test-smart-activate-hook-skips-active-nick ()
  "A nick-change signal does not fire the hook for the already-active nick."
  (let ((clatter-smart-noise '(join part quit nick away))
        (fires 0))
    (with-temp-buffer
      (clatter-smart-put (current-buffer) "alice" 'privmsg)
      (let ((probe (lambda (&rest _) (cl-incf fires))))
        (add-hook 'clatter-smart-activate-hook probe)
        (unwind-protect
            (progn
              (should-not (clatter-smart-eval
                           (current-buffer) "alice" "alice_"))
              (should (zerop fires)))
          (remove-hook 'clatter-smart-activate-hook probe))))))

(provide 'test-smart)
;;; test-smart.el ends here
