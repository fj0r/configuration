set nocompatible
"set shortmess=atI

syntax on
colorscheme slate

set wildmenu
set laststatus=1
set ruler
"set number
"set cmdheight=1
"set linespace=2

set display=lastline
set wrap
set nolinebreak
set textwidth=0

let mapleader="\<Space>"

set shiftwidth=4
set expandtab
set softtabstop=-1
set autoindent

set history=500

set incsearch
set ignorecase
set hlsearch
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

set foldmethod=marker

" 以下字符将被视为单词的一部分 (ASCII)：
"set iskeyword+=33-47,58-64,91-96,123-128

" 记忆最后编辑状态
au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif
set viminfo='1000,f1,<500
