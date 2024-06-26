"
" dthurn's vimrc
"

" Only run the script once
if exists("g:did_myvimrc")
  finish
endif

let g:did_myvimrc = 1

" No vi compatibility!:
set nocompatible     
" Line Numbers
set number
" Swap case is an operator
set tildeop          
" Help vim guess my background color
set background=dark
" Lots of history!
set history=10000    
" I have lots of RAM
set maxmemtot=2000000
" LOTS of RAM
set maxmem=2000000   
" Don't jump to start of line on page up/dwn
set nostartofline    
" Allow cursor where there's no character
set virtualedit=block
" Allow direction keys to wrap to the next line
set whichwrap=bs<>   
" Menu for auto complete
set wildmenu                  
" Completion list to longest common match and then full
set wildmode=list:longest,full "
" Use tab for wild
set wildchar=<TAB>
" Ignore some characters for completion
set wildignore=.svn,CVS,.git,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif
" Support for non-native file formats
set fileformats=unix,dos,mac 
" Do not redraw while running macros (much faster).
set lazyredraw 
" Wrap long lines at 'breakat' char instead of last character
set linebreak
" Allow current buffer to be in the background without a disk write
set hidden
" Enable non-stupid backspace key behavior
set backspace=indent,eol,start
" Enable the mouse in supporting terminals
set mouse=a
" Enable the ruler with the format:
" {buffer number}{modified/readonly flag}: {file type}
" [current line, current column] {position percentage in file}
set ruler
set rulerformat=%25(%n%m%r:\ %Y\ [%l,%v]\ %p%%%)
" Configure Status Line
set statusline = "%f\\ %h%m%r%=%l,%v\\ \\ \\ \\ %P" 
set laststatus=2 
" Start scrolling 3 lines before the cursor reaches the bottom
set scrolloff=3
" save backups centrally
set backupdir=~/.vim
set directory=~/.vim
" remember some stuff after quiting vim:
" marks, registers, searches, buffer list
set viminfo='20,<50,s10,h,%
" Disable bell
set noerrorbells
" Disable scroll bars in gvim
set guioptions-=r
" Keep going until you find a tags file
set tags=tags;/


" Enable the filetype plugin
" Not sure if both of these are needed?
"filetype plugin indent on
"filetype plugin on

" Default indentation
" ftplugins to override
set tabstop=2
set softtabstop=2
set shiftwidth=2
set smarttab
set expandtab

" Colorize NERDTree
let NERDChristmasTree = 1

" Build help documentation on vim start
helptags ~/.vim/doc

if &t_Co > 2 
    syntax on
    set hlsearch
    set incsearch 
endif

" Support for sablecc and stringtemplate file types
au BufRead,BufNewFile *.sablecc set syntax=sablecc
au BufRead,BufNewFile *.st set syntax=stringtemplate

" Highlight lines that are over 80 characters in length
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

" Highlight tab characters
syn match tab display "\t"
hi link tab Error

" Kill trailing whitespace on save
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd FileType c,cabal,cpp,haskell,javascript,php,python,readme,text
  \ autocmd BufWritePre <buffer>
  \ :call <SID>StripTrailingWhitespaces()

" This is sort of a hack, but t_Co never seems to get set correctly
set t_Co=256

" Default vim colorscheme
colorscheme desert256

source ~/.vim_mappings.vim

" Source local vim config if itexists
let VIM_CONFIG=expand("~/.vim_config.vim")
if filereadable(VIM_CONFIG) | exe "source " . VIM_CONFIG | endif

