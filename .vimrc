"
" dthurn's vimrc
"

source ~/.global/vimrc
source ~/.global/vim_mappings

" Source local vim config if it exists
let VIM_CONFIG=expand("~/.vim_config")
if filereadable(VIM_CONFIG) | exe "source " . VIM_CONFIG | endif
