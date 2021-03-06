" Map s to save and Ctrl-s to s
noremap <C-s> s
noremap s :w<CR>

" Make Y consistent with C and D
noremap Y y$

" ` jumps to line and column, ' just jumps to line.
nnoremap ' `
nnoremap ` '

" H for start of line, L for end of line
noremap H ^
noremap L $

" Map gh to clear hilighting
noremap gh :noh<CR>

" Map gl to invoke shell commands
noremap gl :!

" Switch macro playback and macro record 
noremap q @
noremap @ q

" Map Control-L to Escape
" noremap! <C-l> 

" Map gs to invoke a perl substitute regular expression
noremap gs :%!perl -p -i -e 's/

" Map gd to run current file
noremap gd :!./%<CR>

" Map o and O to exit insert mode
noremap o o<ESC>
noremap O O<ESC>

" Enable Nerd Tree
noremap gn :NERDTree 
noremap gnt :NERDTree .<CR>

" Disable Nerd Tree
noremap gq :NERDTreeClose<CR>

" Completion
" noremap! <C-j> <C-x><C-u>

" Vimperator-style tab navigation
" noremap <C-n> OB
" noremap <C-p> OA

" Sober Keybinding System
inoremap <C-a> <C-o>^
inoremap <C-f> <C-o>w
inoremap <C-s> <ESC>/
inoremap <C-l> <C-o>b
inoremap <C-e> <C-o>x
inoremap <C-r> <C-o>db
inoremap <C-u> <C-o>p
inoremap  <C-o>u

" Map j and k to work with screen lines, not logical lines
nnoremap k gk
nnoremap j gj
nnoremap gk k
nnoremap gj j

" Swap ; and :
noremap ; :
noremap : ;

" toggle paste mode
set pastetoggle=<C-g>

" John Zila's * and # Keybindings
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
nnoremap * :set hls<CR>:let @/="<C-r><C-w>"<CR>
