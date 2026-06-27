;;; test-pals.el --- Tests for pals and fools lists -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for clatter-pals.el: membership, add/remove helpers, muting,
;; and pal nick-face highlighting.

;;; Code:

(require 'ert)
(require 'clatter-pals)
(require 'clatter-hl-nicks)

(ert-deftest clatter-pals-membership-case-insensitive ()
  (let ((clatter-pals '("Alice" "bob" ("Tom" . "IRCNet"))))
    (should (clatter-pal-p "alice"))
    (should (clatter-pal-p "ALICE"))
    (should (clatter-pal-p "Bob"))
    (should (clatter-pal-p "bob" "XNet"))
    (should (clatter-pal-p "Tom" "IRCNet"))
    (should-not (clatter-pal-p "Tom" "NoNet"))
    (should-not (clatter-pal-p "Tom"))
    (should-not (clatter-pal-p "carol"))
    (should-not (clatter-pal-p nil))))

(ert-deftest clatter-fools-membership-case-insensitive ()
  (let ((clatter-fools '("louduser" ("remoteuser" . "ExampleNet"))))
    (should (clatter-fool-p "LoudUser"))
    (should (clatter-fool-p "LoudUser" "OtherNet"))
    (should (clatter-fool-p "remoteuser" "ExampleNet"))
    (should-not (clatter-fool-p "remoteuser"))
    (should-not (clatter-fool-p "remoteuser" "OtherNet"))
    (should-not (clatter-fool-p "someone"))
    (should-not (clatter-fool-p "someone" "somenet"))))

(ert-deftest clatter-nick-list-add-idempotent ()
  (should (equal (clatter--nick-list-add "alice" '("alice")) '("alice")))
  (should (equal (clatter--nick-list-add "ALICE" '("alice")) '("alice")))
  (should (equal (clatter--nick-list-add "bob" '("alice")) '("bob" "alice")))
  (should (equal (clatter--nick-list-add "remoteuser" '("alice" ("remoteuser" . "ExampleNet")))
                 '("remoteuser" "alice")))
  (should (equal (clatter--nick-list-add "remoteuser" '("alice" ("remoteuser" . "ExampleNet")) "ExampleNet")
                 '("alice" ("remoteuser" . "ExampleNet"))))
  (should (equal (clatter--nick-list-add "remoteuser" '("alice" "remoteuser") "ExampleNet")
                 '("alice" "remoteuser"))))

(ert-deftest clatter-nick-list-remove-case-insensitive ()
  (should (equal (clatter--nick-list-remove "ALICE" '("alice" "bob")) '("bob")))
  (should (equal (clatter--nick-list-remove "carol" '("alice" "bob"))
                 '("alice" "bob")))
  (should (equal (clatter--nick-list-remove "remoteuser" '("alice" "bob" ("remoteuser" . "ExampleNet")) "ExampleNet")
                 '("alice" "bob")))
  (should (equal (clatter--nick-list-remove "remoteuser" '("alice" "bob" ("remoteuser" . "ExampleNet")))
                 '("alice" "bob")))
  (should (equal (clatter--nick-list-remove "remoteuser" '("alice" "bob" ("remoteuser" . "ExampleNet")) "OtherNet")
                 '("alice" "bob" ("remoteuser" . "ExampleNet")))))

(ert-deftest clatter-muted-combines-ignore-and-fools ()
  (let ((clatter-ignore-list '("spammer"))
        (clatter-fools '("louduser")))
    (should (clatter-muted-p '("spammer" nil nil)))
    (should (clatter-muted-p '("LoudUser" nil nil)))
    (should-not (clatter-muted-p '("friend" nil nil)))))

(ert-deftest clatter-sender-invisibility-distinguishes-ignore-and-fools ()
  (let ((clatter-ignore-list '("spammer!*@*"))
        (clatter-fools '("louduser")))
    (should (eq (clatter-sender-invisibility '("spammer" "u" "h")) 'muted))
    (should (eq (clatter-sender-invisibility '("LoudUser" nil nil)) 'clatter-fool))
    (should-not (clatter-sender-invisibility '("friend" nil nil)))))

(ert-deftest clatter-pal-gets-pal-face ()
  "A pal's nick maps to the `clatter-pal' face; others to a palette face."
  (let ((clatter-pals '("alice")))
    (should (eq (clatter-hl-nick-face-symbol "alice") 'clatter-pal))
    (should (eq (clatter-hl-nick-face-symbol "ALICE") 'clatter-pal))
    (should-not (eq (clatter-hl-nick-face-symbol "bob") 'clatter-pal))
    (should (facep (clatter-hl-nick-face-symbol "bob")))))

(provide 'test-pals)

;;; test-pals.el ends here
