vim.cmd [[
  nnoremap <leader>ev :tabedit $MYVIMRC<cr>
  nnoremap <leader>vc :source $MYVIMRC<cr>

  noremap <leader>bd :bd<cr>
  noremap <leader>fff :lua open_nerd_tree()<cr>
  noremap <leader>ft :NERDTreeToggle<cr>

  nmap <BS> :noh<cr>
  nmap <tab> <c-w><c-w>

  nmap <leader>ff :Telescope find_files<cr>
  nmap <leader>fb :Telescope file_browser<cr>
  nmap <leader>ss :Telescope live_grep<cr>
  nmap <leader>bb :Telescope buffers<cr>
  nmap <leader>ht :Telescope help_tags<cr>

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
]]
