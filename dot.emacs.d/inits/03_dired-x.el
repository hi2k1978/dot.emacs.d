;dired to dired-x
;(add-hook 'dired-load-hook (lambda () (load "dired-x") ))
(setq dired-listing-switches "-lha --group-directories-first --ignore='.[^.]*'")
