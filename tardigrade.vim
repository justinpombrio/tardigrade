syn keyword kw func if then else and or is do while for break continue not let case end
syn keyword kw block return
syn match kw "->"
syn match kw "="
syn match punct "[\[\](){};,]"
syn match punct "=>"
syn keyword const true false
syn match const "[0-9]+"
" For test cases
syn keyword meta TEST EXPECT END

syn match op ">"
syn match op "<"
syn match op ">="
syn match op "<="
syn match op "=="
syn match op "!="
syn match op "[-+*/]"
syn match type "\<[A-Z][a-zA-Z0-9_]*"

syn region comm start="//" end="$"

hi def link punct   Delimiter
hi def link kw      Keyword
hi def link comm    Comment
hi def link op      Operator
hi def link type    Type
hi def link meta    PreProc
hi def link const   Constant
