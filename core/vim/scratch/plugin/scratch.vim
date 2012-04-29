" This is modified version of scratch plugin by Yegappan Lakshmanan

if exists('loaded_scratch') || &cp
    finish
endif
let loaded_scratch=1

" Scratch buffer name
let ScratchBufferName = "__Scratch__"

" ScratchBufferOpen
" Open the scratch buffer
function! s:ScratchBufferOpen(vertically)
    let split_vertically = a:vertically

    " Check whether the scratch buffer is already created
    let scr_bufnum = bufnr(g:ScratchBufferName)
    if scr_bufnum == -1
        " open a new scratch buffer
        if split_vertically == 1
            exe "vnew " . g:ScratchBufferName
        else
            exe "new " . g:ScratchBufferName
        endif
    else
        " Scratch buffer is already created. Check whether it is open
        " in one of the windows
        let scr_winnum = bufwinnr(scr_bufnum)
        if scr_winnum != -1
            " Jump to the window which has the scratch buffer if we are not
            " already in that window
            if winnr() != scr_winnum
                exe scr_winnum . "wincmd w"
            endif
        else
            " Create a new scratch buffer
            if split_vertically == 1
                exe "vsplit +buffer" . scr_bufnum
            else
                exe "split +buffer" . scr_bufnum
            endif
        endif
    endif
endfunction

function! s:ScratchBufferToggle(new_win)
    let split_win = a:new_win
    let scr_bufwinnum = bufwinnr(g:ScratchBufferName)
    let cur_bufwinnum = bufwinnr('%')
    if cur_bufwinnum == scr_bufwinnum
        let scr_bufnum = bufnr(g:ScratchBufferName)
        exe "q"
    else
        :call s:ScratchBufferOpen(split_win)
    endif
endfunction


" ScratchMarkBuffer
" Mark a buffer as scratch
function! s:ScratchMarkBuffer()
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal buflisted
endfunction

autocmd BufNewFile __Scratch__ call s:ScratchMarkBuffer()

" Command to open the scratch buffer in a new split window
command! -nargs=0 Scratch call s:ScratchBufferToggle(0)
" Command to open the scratch buffer in a new vsplit window
command! -nargs=0 VScratch call s:ScratchBufferToggle(1)
