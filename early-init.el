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
