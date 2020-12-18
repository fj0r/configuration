set nocompatible

set linespace=2

syntax on
colorscheme desert

" 简洁启动模式
set shortmess=atI
" set guioptions=gmrLtT  m:菜单 T:工具栏 r:滚动条
set guioptions=gLt
" 在状态栏位置显示补全菜单
"set wildmenu
set cmdheight=1
set ruler

" 显示不完整的段落
set display=lastline
" 自动折行
set wrap
" 按完整单词折行
set nolinebreak
" 行宽（输入时自动插入换行符）
set textwidth=0

" j k 在屏幕行间上下移动；gj gk 在物理行间上下移动
noremap j gj
noremap k gk
noremap gj j
noremap gk k
" 使用 Space 作为 leader
let mapleader="\<Space>"


set expandtab
set tabstop=4
set shiftwidth=4
set autoindent
" 插入模式下，“←”如何删除光标前的字符：行首空白、换行符、插入点之前的字符
set backspace=indent,eol,start

" bracketed-paste.vim
if exists("g:loaded_bracketed_paste")
  finish
endif
let g:loaded_bracketed_paste = 1

let &t_ti .= "\<Esc>[?2004h"
let &t_te = "\e[?2004l" . &t_te

function! XTermPasteBegin(ret)
  set pastetoggle=<f29>
  set paste
  return a:ret
endfunction

execute "set <f28>=\<Esc>[200~"
execute "set <f29>=\<Esc>[201~"
map <expr> <f28> XTermPasteBegin("i")
imap <expr> <f28> XTermPasteBegin("")
vmap <expr> <f28> XTermPasteBegin("c")
cmap <f28> <nop>
cmap <f29> <nop>

" 命令行历史纪录
set history=500

" 禁用增量搜索
set incsearch
" 搜索时忽略大小写
set ignorecase
" 高亮显示搜索结果
set hlsearch

" 设定折叠方式
"set foldmethod=manual

" 以下字符将被视为单词的一部分 (ASCII)：
"set iskeyword+=33-47,58-64,91-96,123-128

" 记忆最后编辑状态
au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif
set viminfo='1000,f1,<500

chdir $VIM/..

if has('gui_running') && has("win32")
    map <F11> :call libcallnr("gvimfullscreen.dll", "ToggleFullScreen", 0)<CR>
endif

set rtp+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

Plugin 'Lokaltog/vim-easymotion'

call vundle#end()