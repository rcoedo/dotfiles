vim.cmd([[
  syntax enable
  colorscheme tokyonight

  filetype plugin indent on
  au FileType gitcommit set tw=72
  au FileType go setlocal ts=4 sw=4 expandtab
  au BufRead,BufNewFile *.swcrc set filetype=json
]])

vim.opt.autoread = true
vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave", "CursorHold" }, { command = "checktime" })

vim.opt.shell = "sh"
vim.opt.background = "dark"
vim.opt.compatible = false
vim.opt.errorbells = false
vim.opt.visualbell = false
vim.opt.backup = false
vim.opt.wb = false
vim.opt.swapfile = false
vim.opt.history = 1000
vim.opt.number = true
vim.opt.ruler = true
vim.opt.cursorline = true
vim.opt.so = 7
vim.opt.wildmenu = true
vim.opt.wildmode = "list:longest"
vim.opt.wildignore = { "*.o", "*~", "*.pyc", "*/node_modules/**" }
vim.opt.backspace = { "eol", "start", "indent" }
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.lazyredraw = true
vim.opt.magic = true
vim.opt.showmatch = true
vim.opt.mat = 2
vim.opt.tm = 200
vim.opt.list = true
vim.opt.listchars = { tab = "  ", trail = "⋅", nbsp = "⋅" }
vim.opt.ffs = { "unix", "dos", "mac" }
vim.opt.expandtab = true
vim.opt.smarttab = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.lbr = true
vim.opt.tw = 120
vim.opt.autoindent = false
vim.opt.smartindent = false
vim.opt.wrap = true
vim.opt.ttyfast = true
vim.opt.mouse = "a"
vim.opt.clipboard = "unnamed"
vim.opt.signcolumn = "yes"
vim.opt.pumheight = 12
vim.opt.laststatus = 3
vim.opt.termguicolors = true
-- vim.opt.cmdheight = 0
vim.opt.splitkeep = "screen"
vim.opt.splitright = true
vim.opt.splitbelow = true

vim.g.markdown_fenced_languages = {
	"ts=typescript",
	"lua",
	"js",
	"python",
}

local sign = function(opts)
	vim.fn.sign_define(opts.name, {
		texthl = opts.name,
		text = opts.text,
		numhl = "",
	})
end

sign({ name = "DiagnosticSignError", text = "✘" })
sign({ name = "DiagnosticSignWarn", text = "▲" })
sign({ name = "DiagnosticSignHint", text = "⚑" })
sign({ name = "DiagnosticSignInfo", text = "" })

vim.diagnostic.config({
	virtual_text = false,
	signs = true,
	update_in_insert = false,
	underline = true,
	severity_sort = true,
	float = {
		focusable = false,
		border = "rounded",
		source = "always",
		header = "",
		prefix = "",
	},
})

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })
