"
" Thor's vimrc
"

source ~/.global/vimrc
source ~/.global/vim_mappings

" Source local vim config if it exists
let VIM_MAPPINGS=expand("~/.vim_config")
if filereadable(VIM_MAPPINGS) | exe "source " . VIM_MAPPINGS | endif
