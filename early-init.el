;; Increase the garbage collection threshold so we don't spend a bunch of time
;; in startup doing GC. We'll turn it back lower (so we do small quick ones)
;; after we're done loading. 
;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))

(let (my-font my-height)
  (if (eq system-type 'windows-nt)
      (setq my-font "D2Coding-12"
            my-height 38)
    (setq my-font "Monospace-14"
          my-height 40))
  
  (setq default-frame-alist
        `((width . 90)
          (height . ,my-height)
          (font . ,my-font)
          (background-mode . dark)
          (background-color . "gray10")
          (foreground-color . "white")
          (mouse-color . "white")
          (vertical-scroll-bars . right))))
