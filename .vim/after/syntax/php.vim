if exists('b:current_syntax')
  let s:current_syntax=b:current_syntax
  unlet b:current_syntax
endif
syn include @XmlSyntax syntax/xml.vim
if exists('s:current_syntax')
  let b:current_syntax=s:current_syntax
endif
syn region xhpCode start="<\(fbt\|\(ui\|x\|m\):[a-zA-Z0-9:\-]*\|a\|div\|span\|ul\)[^>/]*[^/]*>" keepend end="</\(fbt\|\(ui\|x\|m\):[a-zA-Z0-9:\-]*\|a\|div\|span\|ul\)>[ \n]*\()\|;\|,\)" contains=@XmlSyntax
syn cluster phpClConst add=xhpCode,xhpCodeSingleTag
syn cluster phpClInside add=xhpCode,xhpCodeSingleTag
syn cluster phpClFunction add=xhpCode,xhpCodeSingleTag
syn cluster phpClTop add=xhpCode,xhpCodeSingleTag
