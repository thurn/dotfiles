"
" dthurn's vimrc
"

source ~/.global/vimrc
source ~/.global/vim_mappings.vim

" Source local vim config if itexists
let VIM_CONFIG=expand("~/.vim_config")
if filereadable(VIM_CONFIG) | exe "source " . VIM_CONFIG | endif
