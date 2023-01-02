# Dotfiles
My dotfiles

# Requirements
## Install packages
```
paru -S asdf-vm
asdf plugin add python
npm i tree-sitter-elisp
```
## Install fonts (fetch the latest font version)
```
mkdir -p ~/.local/share/fonts
wget https://download.jetbrains.com/fonts/JetBrainsMono-2.242.zip ~/Downloads/JetBrainsFont.zip
unzip -j JetBrainsMono-2.242.zip "fonts/ttf/*" -d ~/.local/share/fonts
fc-cache -f -v
```
## Setup keyboard key-repeat and frequency
```
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 15
gsettings set org.gnome.desktop.peripherals.keyboard delay 300
```
## Configuring
```
sh
gh repo clone dbohomiakov/.dotfiles
cd .dotfiles
stow -t .emacs.d emacs.d
stow -t ~/.tmux.conf tmux.conf
stow -t ~/.config/nvim/ nvim/
stow -t ~/.config/alacritty/ alacritty/
```
## Setup emacs to work correctly in terminal (colorscheme)
```
tic -x -o ~/.terminfo terminfo-24bit.src
alias et="TERM=xterm-24bit emacs -nw"
alias e="TERM=xterm-24bit emacsclient -nw"
```
