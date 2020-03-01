(defvar exwm--terminal-command "xfce4-terminal"
  "Terminal command to run.")

(defvar exwm--default-browser-command '("brave" . "brave-browser")
  "Command to run default browser and its class-instance")

(defvar exwm--suspend-command-args '("" nil "systemctl" "suspend")
  "command to run to suspend")

(defvar exwm-app-launcher--prompt "$ "
  "Prompt for the EXWM application launcher")

(defvar exwm--hide-tiling-modeline nil
  "Whether to hide modeline.")
