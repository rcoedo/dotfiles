vim.cmd([[
  nnoremap <leader>ev :tabedit $MYVIMRC<cr>
  nnoremap <leader>rc :source $MYVIMRC<cr>
  nnoremap <leader>bd :bd<cr>
  nnoremap <leader>ft :lua open_nerd_tree()<cr>
  nnoremap <leader>[ :b#<cr>


  nnoremap <bs> :nohl<cr>
  nnoremap <tab> <c-w><c-w>

  " files
  nmap <leader>ff :Telescope find_files<cr>
  nmap <leader>fs :Telescope live_grep<cr>

  " buffers
  nmap <leader>bb :Telescope buffers<cr>

  " help
  nmap <leader>ht :Telescope help_tags<cr>

  " lsp
  nmap <leader>ls :Lspsaga hover_doc<cr>
  nnoremap <silent><buffer> <Leader>lf :lua vim.lsp.buf.format({ async = true })<cr>
  "xnoremap <silent><buffer> <Leader>lfr :lua vim.lsp.buf.range_formatting({})<cr>

  " diagnostics
  nmap <leader>dd :Trouble document_diagnostics<cr>
  nmap <leader>ds :Lspsaga show_line_diagnostics<cr>
  nmap <leader>dn :Lspsaga diagnostic_jump_next<cr>
  nmap <leader>dp :Lspsaga diagnostic_jump_prev<cr>

  " Emacs style insert mode bindings
  imap <C-b> <Left>
  imap <C-f> <Right>
  imap <C-a> <Esc>0i
  imap <C-e> <End>
  imap <C-d> <Del>
  imap <C-h> <BS>
  imap <C-k> <Esc>ld$i
  cmap <C-p> <Up>
  cmap <C-n> <Down>
  cmap <C-b> <Left>
  cmap <C-f> <Right>
  cmap <C-a> <Home>
  cmap <C-e> <End>
  cnoremap <C-d> <Del>
  cnoremap <C-h> <BS>
  cnoremap <C-k> <C-f>D<C-c><C-c>:<Up>
]])
