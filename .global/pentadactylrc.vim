noremap ; :
noremap : ;

noremap <C-a> gT
noremap <C-e> gt
noremap <C-m> gt

noremap <C-i> <C-d>
noremap <M-f> <C-u>

noremap s ;

noremap j 3j
noremap k 3k

set hintkeys='asdf;lkjghvnru'
set newtab='all'
set complete=lS

highlight -a Hint text-transform: uppercase;
"set! browser.tabs.closeWindowWithLastTab=false
highlight Hint::after content: attr(number) !important;
au Fullscreen .* -js document.getElementById("addon-bar").removeAttribute("moz-collapsed")

map <C-1> ohttp://news.ycombinator.com<CR>
map <C-2> ohttp://programming.reddit.com<CR>
map <C-3> ohttp://www.reddit.com/r/all/top/<CR>
map <C-4> ohttp://fffffffuuuuuuuuuuuu.reddit.com<CR>
map <C-5> ohttp://adviceanimals.reddit.com<CR>
map <C-6> ohttp://facebook.com<CR>
map <C-7> ohttp://www.heavengames.com/cgi-bin/forums/display.cgi?action=t&fn=1<CR>



