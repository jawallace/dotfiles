" Plugin Initialization
set nocompatible

filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'

Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/plugin/'}

Plugin 'flazz/vim-colorschemes'

Plugin 'vim-syntastic/syntastic'

Plugin 'ctrlpvim/ctrlp.vim'

Plugin 'rust-lang/rust.vim'

Plugin 'tpope/vim-fugitive'

Plugin 'airblade/vim-gitgutter'

Plugin 'xolox/vim-misc'

Plugin 'xolox/vim-notes'

Plugin 'scrooloose/nerdcommenter'

Plugin 'majutsushi/tagbar'

Plugin 'ludovicchabant/vim-gutentags'

Plugin 'scrooloose/nerdtree'

Plugin 'Xuyuanp/nerdtree-git-plugin'

Plugin 'Yggdroot/indentLine'

call vundle#end()
filetype plugin indent on

" Set leader to space
let mapleader = "<Space>" 

" Indent Options
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4   
set expandtab       

" Text Options
set textwidth=120
set scrolloff=5
set sidescrolloff=5
set number
set showmatch

" Display Options
syntax on
set background=dark
colorscheme solarized

" Search Options
set incsearch
set ignorecase
set smartcase 
set hlsearch

set autoread
set ruler
set wildmenu

set comments=sl:/*,mb:\ *,elx:\ */

set laststatus=2

nmap j gj
nmap k gk

map <C-c> <Esc>

" Ctrl-P Plugin Option
let g:ctrlp_map = '<Leader>p'
let g:ctrlp_match_window_bottom = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_custom_ignore = '\v\~$|\.(o|swp|pyc|wav|mp3|ogg|blend)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])|__init__\.py'
let g:ctrlp_working_path_mode = 0
let g:ctrlp_dotfiles = 0
let g:ctrlp_switch_buffer = 0

" Syntastic Options
let g:syntastic_rust_checkers = ['cargo']
let g:rustfmt_autosave = 1

" Indent Guide Options
let g:indentLine_char = '•'
let g:indentLine_leadingSpaceChar = '•'
let g:indentLine_leadingSpaceEnabled = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NerdTree Options
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Start by default
autocmd VimEnter * NERDTree
map <F7> :NerdTreeToggle<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TagBar Options
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Start by default
nmap <F8> :TagbarToggle<CR>

