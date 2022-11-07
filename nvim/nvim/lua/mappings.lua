vim.g.mapleader = " "
local map = vim.api.nvim_set_keymap

map("n", ",", "<leader>", { noremap = false })
map("n", "<tab>", "<c-w><c-w>", { noremap = false })
map("n", "<leader><leader>", "<c-w><c-w>", { noremap = false })
map("n", "<bs>", "<cmd>nohl<cr>", { noremap = false })
map("n", "<a-h>", "<c-w>h", { noremap = false })
map("n", "<a-j>", "<c-w>j", { noremap = false })
map("n", "<a-k>", "<c-w>k", { noremap = false })
map("n", "<a-l>", "<c-w>l", { noremap = false })

-- Emacs style insert mode bindings
map("i", "<c-b>", "<left>", { noremap = false })
map("i", "<c-f>", "<right>", { noremap = false })
map("i", "<c-a>", "<home>", { noremap = false })
map("i", "<c-e>", "<end>", { noremap = false })
map("i", "<c-d>", "<del>", { noremap = false })
map("i", "<c-h>", "<bs>", { noremap = false })
map("i", "<c-k>", "<esc>lDa", { noremap = false })
-- Emacs style command mode bindings
map("c", "<c-p>", "<up>", { noremap = false })
map("c", "<c-n>", "<down>", { noremap = false })
map("c", "<c-b>", "<left>", { noremap = false })
map("c", "<c-f>", "<right>", { noremap = false })
map("c", "<c-a>", "<home>", { noremap = false })
map("c", "<c-e>", "<end>", { noremap = false })
map("c", "<c-d>", "<del>", { noremap = true })
map("c", "<c-h>", "<bs>", { noremap = true })
map("c", "<c-k>", "<c-\\>e(strpart(getcmdline(), 0, getcmdpos() - 1))<cr>", { noremap = true })

-- nmap <leader>ld :Lspsaga hover_doc<cr>
-- nmap <leader>lp :Lspsaga peek_definition<cr>
-- nmap <leader>ls :Lspsaga lsp_finder<cr>
-- nmap <leader>les :Lspsaga show_line_diagnostics<cr>
-- nmap <leader>len :Lspsaga diagnostic_jump_next<cr>
-- nmap <leader>lep :Lspsaga diagnostic_jump_prev<cr>
-- nmap <leader>a :Lspsaga code_action<cr>

local wk = require("which-key")
wk.register({
	-- Quick mappings
	["<leader>/"] = { "<cmd>Telescope live_grep<cr>", "Live search" },
	["<leader>f"] = { "<cmd>Telescope find_files<cr>", "Pick file" },
	["<leader>d"] = { "<cmd>Telescope diagnostics<cr>", "Pick diagnostic" },
	["<leader>b"] = { "<cmd>Telescope buffers<cr>", "Pick buffer" },
	["<leader>a"] = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Pick code action" },
	["<leader>t"] = { "<cmd>lua open_nerd_tree()<cr>", "Toggle file tree", noremap = false },
	["<leader>,"] = { "<c-w><c-w>", "Next window" },
	["<leader>0"] = { "<cmd>close<cr>", "Close window" },
	["<leader>2"] = { "<cmd>split<cr>", "Split horizontally" },
	["<leader>3"] = { "<cmd>vsplit<cr>", "Split vertically" },

	-- Pick table
	["<leader>p"] = { name = "+Pick" },
	["<leader>pc"] = { "<cmd>Telescope commands<cr>", "Pick command" },
	["<leader>pb"] = { "<cmd>Telescope buffers<cr>", "Pick buffer" },
	["<leader>pd"] = { "<cmd>Telescope diagnostics<cr>", "Pick diagnostic" },
	["<leader>pf"] = { "<cmd>Telescope find_files<cr>", "Pick file" },
	["<leader>pa"] = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Pick code action" },
	-- ["<leader>pa"] = { "<cmd>lua vim.lsp.buf.range_code_action()<cr>", "Pick code action" },

	-- Refactor
	["<leader>r"] = { name = "+Refactor" },
	["<leader>rr"] = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename" },
	["<leader>rf"] = { "<cmd>lua vim.lsp.buf.format({ async = true })<cr>", "Format" },

	-- Close table
	["<leader>x"] = { name = "+Close" },
	["<leader>xu"] = { "<cmd>bd<cr>", "Close buffer" },
	["<leader>xi"] = { "<cmd>close<cr>", "Close window" },

	-- Go table
	["gl"] = { "<cmd>lua vim.diagnostic.open_float()<cr>", "Show diagnostic" },
	["gk"] = { "<cmd>lua vim.lsp.buf.hover()<cr>", "Show hover" },
	["gK"] = { "<cmd>lua vim.lsp.buf.signature_help()<cr>", "Show signature" },
	["gd"] = { "<cmd>lua vim.lsp.buf.definition()<cr>", "Go to definition" },
	["gD"] = { "<cmd>lua vim.lsp.buf.declaration()<cr>", "Go to declaration" },
	["gi"] = { "<cmd>lua vim.lsp.buf.implementation()<cr>", "Go to implementation" },
	["go"] = { "<cmd>lua vim.lsp.buf.type_definition()<cr>", "Go to type definition" },
	["gr"] = { "<cmd>lua vim.lsp.buf.references()<cr>", "Go to references" },

	-- Previous table
	["["] = { name = "+Previous" },
	["[b"] = { "<cmd>bp<cr>", "Previous buffer" },
	["[d"] = { "<cmd>lua vim.diagnostic.goto_prev()<CR>", "Previous diagnostic" },

	-- Next table
	["]"] = { name = "+Next" },
	["]b"] = { "<cmd>bn<cr>", "Next buffer" },
	["]d"] = { "<cmd>lua vim.diagnostic.goto_next()<CR>", "Next diagnostic" },

	-- Ignored
	["<leader><leader>"] = "which_key_ignore",
})
