;;; ai --- Config file related to all things AI
;;; Commentary:
;;; Code:

(require 'secrets)


(use-package gptel
  :straight t
  :defines
  gptel-make-anthropic
  gptel-api-key
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key os-secret-anthropic-key)
  (setq gptel-api-key os-secret-openai-api-key)
  ())




(provide 'ai)
;;; ai.el ends here
