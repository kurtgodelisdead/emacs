

(autoload 'cycle-buffer "cycle-buffer/cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer/cycle-buffer" "Cycle backward." t)
(autoload 'cycle-buffer-permissive "cycle-buffer/cycle-buffer" "Cycle forward allowing *buffers*." t)
(autoload 'cycle-buffer-backward-permissive "cycle-buffer/cycle-buffer" "Cycle backward allowing *buffers*." t)
(autoload 'cycle-buffer-toggle-interesting "cycle-buffer/cycle-buffer" "Toggle if this buffer will be considered." t)
(global-set-key [(f7)]        'cycle-buffer-backward)
(global-set-key [(f8)]       'cycle-buffer)
(global-set-key [(shift f7)]  'cycle-buffer-backward-permissive)
(global-set-key [(shift f8)] 'cycle-buffer-permissive)
