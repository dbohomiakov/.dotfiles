# Dotfiles
My dotfiles

# Requirements
## Install packages
```
paru -S asdf-vm
asdf plugin add python
```
## Install fonts (fetch the latest font version)
```
mkdir -p ~/.local/share/fonts
wget https://download.jetbrains.com/fonts/JetBrainsMono-2.242.zip ~/Downloads/JetBrainsFont.zip
unzip -j JetBrainsMono-2.242.zip "fonts/ttf/*" -d ~/.local/share/fonts
fc-cache -f -v
```
## Configuring
```
sh
gh repo clone dbohomiakov/.dotfiles
cd .dotfiles
stow .
```
