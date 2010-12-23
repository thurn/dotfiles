noremap ; :
noremap : ;

noremap <C-a> gT
noremap <C-e> gt

noremap <C-i> <C-d>
noremap <M-f> <C-u>

noremap s ;

noremap j 3j
noremap k 3k

set hintkeys='asdf;lkjghvnru'
set newtab='all'
set complete=lS

highlight -a Hint text-transform: uppercase;
set! browser.tabs.closeWindowWithLastTab=false
highlight Hint::after content: attr(number) !important;
au Fullscreen .* -js document.getElementById("addon-bar").removeAttribute("moz-collapsed")

set fullscreen