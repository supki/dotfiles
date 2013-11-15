"=============================================================================
" What Is This: Add some conceal operator for your haskell files
" Author:       Vincent Berthoux <twinside@gmail.com>
if exists('g:no_haskell_conceal') || !has('conceal') || &enc != 'utf-8'
    finish
endif

syntax match hsNiceOperator "\\" conceal cchar=λ
syntax match hsNiceOperator "\\\\" conceal cchar=∖
syntax match hsNiceOperator "<-" conceal cchar=←
syntax match hsNiceOperator "->" conceal cchar=→
syntax match hsNiceOperator "\<pi\>" conceal cchar=π
syntax match hsNiceOperator /\s%\s/ms=s+1,me=e-1 conceal cchar=÷
syntax match hsNiceOperator "\<not\>" conceal cchar=¬
syntax match hsNiceOperator /\s\*\s/ms=s+1,me=e-1 conceal cchar=×

sy match hs_DeclareFunction /^[a-z_(]\S*\(\s\|\n\)*::/me=e-2 contains=hs_FunctionName,hs_OpFunctionName

syntax match hsNiceOperator "=>" conceal cchar=⇒
syntax match hsNiceOperator "\:\:" conceal cchar=:
syntax match hsNiceOperator "\<forall\>" conceal cchar=∀

syntax match hsNiceOperator /\s\.\s/ms=s+1,me=e-1 conceal cchar=∘

hi link hsNiceOperator Operator
hi! link Conceal Operator
setlocal conceallevel=2
