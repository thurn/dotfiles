"
" Starter vimrc for a new system
"

source ~/.global/vimrc
source ~/.global/vim_mappings

" Source local vim mappings if they exist
let VIM_MAPPINGS=expand("~/.vim_mappings")
if filereadable(VIM_MAPPINGS) | exe "source " . VIM_MAPPINGS | endif
