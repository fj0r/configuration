exts=(

## remote
ms-vscode-remote.vscode-remote-extensionpack

## keymap
tuttieee.emacs-mcx
    # rkwan94.vscode-emacs-improved
    # lfs.vscode-emacs-friendly
    # vscodevim.vim
    # migrs.vimacs
wmaurer.vscode-jumpy

## 括号匹配
coenraads.bracket-pair-colorizer-2

## bookmark
alefragnani.Bookmarks

## git
codezombiech.gitignore
eamodio.gitlens
mhutchie.git-graph

## 项目管理 <->
alefragnani.project-manager
## todo
Gruntfuggly.todo-tree
## 比较
ryu1kn.partial-diff

## 列对齐
wwm.better-align

## 远程工作空间 <->
    # liximomo.remotefs

## 自定义
be5invis.vscode-custom-css

## 监控
mutantdino.resourcemonitor

## docker <->
ms-kubernetes-tools.vscode-kubernetes-tools
peterjausovec.vscode-docker
p1c2u.docker-compose

## language
Lulus.vscode-regexp-preivew
    # LouisWT.regexp-preview
    # formulahendry.code-runner

### shell
rogalmic.zsh-debug

### just
skellock.just

### haskell
dramforever.vscode-ghc-simple
    # alanz.vscode-hie-server
ndmitchell.haskell-ghcid
phoityne.phoityne-vscode
justusadam.language-haskell

zjhmale.idris

### rust
rust-lang.rust
hdevalke.rust-test-lens
vadimcn.vscode-lldb

### js
octref.vetur
dariofuzinato.vue-peek
eg2.vscode-npm-script
    # chakrounanas.turbo-console-log
ddot.vscode-ast
    # sirtobi.pegjs-language

### py
ms-python.python
almenon.arepl
dongli.python-preview
VisualStudioExptTeam.vscodeintellicode

### sql
ckolkman.vscode-postgres
    # bajdzis.vscode-database
    # mtxr.sqltools
    # julialang.language-julia
    # freebroccolo.reasonml

### html, css
formulahendry.auto-close-tag
amandeepmittal.pug
sysoev.language-stylus

### yaml, toml
andyyaldoo.vscode-json
tuxtina.json2yaml
redhat.vscode-yaml
bungcip.better-toml
dotjoshjohnson.xml

## web <->
humao.rest-client

## 主题/图标
johnpapa.vscode-peacock

file-icons.file-icons
victorzevallos.vscode-theme-django
ivanhernandez.theme-clear-dawn
alber70g.solarized-light-differentiated

## 拼写检查
streetsidesoftware.code-spell-checker

## 路径智能提示
christian-kohler.path-intellisense

## SVG
cssho.vscode-svgviewer

)

for i in ${exts[@]}; do
    echo "code --install-extension $i"
done

