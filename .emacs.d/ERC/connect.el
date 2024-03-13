(require 'erc)
(defun erc-bnc ()
  "Connect to BNC using ERC."
  (interactive)
  (erc-tls :server "bnc.irccloud.com" :nick "kompowiec2/irc.pirc.pl" :port "6697"
           :full-name "kompowiec2"
           :password "bnc@erc:660570e7877221969424ea3bf6e383ec"))

