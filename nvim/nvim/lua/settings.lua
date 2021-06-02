vim.cmd [[
  syntax enable
  colorscheme gruvbox

  filetype plugin indent on
  au FileType gitcommit set tw=72
  au FileType javascript setlocal ts=2 sw=2 expandtab
  au FileType go setlocal ts=4 sw=4 expandtab
  au FileType lua setlocal ts=2 sw=2 expandtab
]]

vim.g.mapleader = ' '

vim.opt.shell = 'sh'
vim.opt.background = 'dark'
vim.opt.compatible = false
vim.opt.errorbells = false
vim.opt.visualbell = false
vim.opt.backup = false
vim.opt.wb = false
vim.opt.swapfile = false
vim.opt.history = 1000
vim.opt.autoread = true
vim.opt.number = true
vim.opt.ruler = true
vim.opt.cursorline = true
vim.opt.so = 7
vim.opt.wildmenu = true
vim.opt.wildmode = 'list:longest'
vim.opt.wildignore = { '*.o', '*~', '*.pyc' }
vim.opt.wildignore = vim.opt.wildignore + { '*/node_modules/**'  }
vim.opt.backspace = { 'eol', 'start', 'indent' }
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.lazyredraw = true
vim.opt.magic = true
vim.opt.showmatch = true
vim.opt.mat = 2
vim.opt.tm = 500
vim.opt.list = true
vim.opt.listchars = { tab = '⋅›', trail = '⋅', nbsp = '⋅' }
vim.opt.ffs= { 'unix', 'dos', 'mac' }
vim.opt.expandtab = true
vim.opt.smarttab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.lbr = true
vim.opt.tw = 120
vim.opt.ai = true
vim.opt.si = true
vim.opt.wrap = true
vim.opt.laststatus = 2
vim.opt.ttyfast = true
vim.opt.mouse = 'a'
vim.opt.clipboard = 'unnamed'
