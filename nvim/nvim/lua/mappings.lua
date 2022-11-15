vim.g.mapleader = " "
local map = vim.keymap.set

local noremap_opts = { noremap = true }
local opts = { noremap = false }

map("n", "<tab>", "<c-w><c-w>", opts)
map("n", "<bs>", "<cmd>nohl<cr>", opts)
map("n", "-", "<cmd>Telescope file_browser path=%:p:h<cr>", opts)
map("n", "<a-h>", "<c-w>h", opts)
map("n", "<a-j>", "<c-w>j", opts)
map("n", "<a-k>", "<c-w>k", opts)
map("n", "<a-l>", "<c-w>l", opts)

-- Emacs style insert mode bindings
map("i", "<c-b>", "<left>", opts)
map("i", "<c-f>", "<right>", opts)
map("i", "<c-a>", "<home>", opts)
map("i", "<c-e>", "<end>", opts)
map("i", "<c-d>", "<del>", opts)
map("i", "<c-h>", "<bs>", opts)
map("i", "<c-k>", "<esc>lDa", opts)

-- Emacs style command mode bindings
map("c", "<c-p>", "<up>", opts)
map("c", "<c-n>", "<down>", opts)
map("c", "<c-b>", "<left>", opts)
map("c", "<c-f>", "<right>", opts)
map("c", "<c-a>", "<home>", opts)
map("c", "<c-e>", "<end>", opts)
map("c", "<c-d>", "<del>", noremap_opts)
map("c", "<c-h>", "<bs>", noremap_opts)
map("c", "<c-k>", "<c-\\>e(strpart(getcmdline(), 0, getcmdpos() - 1))<cr>", noremap_opts)

map("n", "*", "<Plug>(asterisk-z*)", { noremap = false })
map("n", "#", "<Plug>(asterisk-z#)", { noremap = false })
map("n", "g*", "<Plug>(asterisk-gz*)", { noremap = false })
map("n", "g#", "<Plug>(asterisk-gz#)", { noremap = false })

vim.keymap.set({ "n", "v", "o" }, "zf", "<Plug>(leap-forward-to)", { desc = "Leap forward to" })
vim.keymap.set({ "n", "v", "o" }, "zF", "<Plug>(leap-backward-to)", { desc = "Leap backward to" })
vim.keymap.set({ "n", "v", "o" }, "zt", "<Plug>(leap-forward-till)", { desc = "Leap forward until" })
vim.keymap.set({ "n", "v", "o" }, "zT", "<Plug>(leap-backward-till)", { desc = "Leap backward until" })

local normal_mappings = {
	-- Quick mappings
	["<leader>f"] = { "<cmd>Telescope file_browser path=%:p:h<cr>", "Pick file" },
	["<leader>t"] = { "<cmd>Telescope find_files<cr>", "Pick fuzzy file" },
	["<leader>g"] = { "<cmd>Telescope live_grep<cr>", "Live search" },
	["<leader>d"] = { "<cmd>Telescope diagnostics bufnr=0<cr>", "Pick diagnostic" },
	["<leader>D"] = { "<cmd>Telescope diagnostics<cr>", "Pick workspace diagnostic" },
	["<leader>u"] = { "<cmd>Telescope buffers<cr>", "Pick buffer" },
	["<leader>a"] = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Pick code action" },
	["<leader>0"] = { "<cmd>close<cr>", "Close window" },
	["<leader>2"] = { "<cmd>split<cr>", "Split horizontally" },
	["<leader>3"] = { "<cmd>vsplit<cr>", "Split vertically" },
	["<leader>/"] = { "<cmd>Telescope current_buffer_fuzzy_find<cr>", "Live search current buffer" },
	["<leader><leader>"] = { "<cmd>Telescope resume<cr>", "Pick resume" },

	-- Pick table
	["<leader>p"] = { name = "+Pick" },
	["<leader>pb"] = { "<cmd>Telescope buffers<cr>", "Pick buffer" },
	["<leader>pc"] = { "<cmd>Telescope commands<cr>", "Pick command" },
	["<leader>pd"] = { "<cmd>Telescope diagnostics bufnr=0<cr>", "Pick diagnostic" },
	["<leader>pD"] = { "<cmd>Telescope diagnostics<cr>", "Pick workspace diagnostic" },
	["<leader>pt"] = { "<cmd>Telescope find_files<cr>", "Pick fuzzy file" },
	["<leader>pf"] = { "<cmd>Telescope file_browser path=%:p:h<cr>", "Pick file" },
	["<leader>pr"] = { "<cmd>Telescope resume<cr>", "Pick resume" },
	["<leader>pa"] = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Pick code action" },
	-- ["<leader>pa"] = { "<cmd>lua vim.lsp.buf.range_code_action()<cr>", "Pick code action" },

	-- Show
	["<leader>s"] = { name = "+Show" },
	["<leader>sd"] = { "<cmd>lua vim.diagnostic.open_float()<cr>", "Show diagnostic" },
	["<leader>sh"] = { "<cmd>lua vim.lsp.buf.hover()<cr>", "Show hover" },
	["<leader>ss"] = { "<cmd>lua vim.lsp.buf.signature_help()<cr>", "Show signature" },
	["<leader>st"] = { "<cmd>Telescope help_tags<cr>", "Show help tags" },
	["<leader>sn"] = { "<cmd>Telescope notify<cr>", "Show notification history" },

	-- Refactor
	["<leader>r"] = { name = "+Refactor" },
	["<leader>rr"] = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename" },
	["<leader>rf"] = { "<cmd>lua vim.lsp.buf.format({ async = true })<cr>", "Format" },

	-- Close table
	["<leader>q"] = { name = "+Quit" },
	["<leader>qu"] = { "<cmd>bd<cr>", "Close buffer" },
	["<leader>qU"] = { "<cmd>bd!<cr>", "Close buffer!" },
	["<leader>qi"] = { "<cmd>close<cr>", "Close window" },
	["<leader>qI"] = { "<cmd>close!<cr>", "Close window!" },
	["<leader>qt"] = { "<cmd>tabclose<cr>", "Close tab" },
	["<leader>qT"] = { "<cmd>tabclose!<cr>", "Close tab!" },

	-- Go table
	["gd"] = { "<cmd>lua vim.lsp.buf.definition()<cr>", "Go to definition" },
	["gD"] = { "<cmd>lua vim.lsp.buf.declaration()<cr>", "Go to declaration" },
	["gi"] = { "<cmd>lua vim.lsp.buf.implementation()<cr>", "Go to implementation" },
	["go"] = { "<cmd>lua vim.lsp.buf.type_definition()<cr>", "Go to type definition" },
	["gr"] = { "<cmd>lua vim.lsp.buf.references()<cr>", "Go to references" },

	-- Previous table
	["["] = { name = "+Previous" },
	["[b"] = { "<cmd>bp<cr>", "Previous buffer" },
	["[t"] = { "<cmd>tabprevious<cr>", "Previous tab" },
	["[d"] = { "<cmd>lua vim.diagnostic.goto_prev()<CR>", "Previous diagnostic" },

	-- Next table
	["]"] = { name = "+Next" },
	["]b"] = { "<cmd>bn<cr>", "Next buffer" },
	["]t"] = { "<cmd>tabnext<cr>", "Next tab" },
	["]d"] = { "<cmd>lua vim.diagnostic.goto_next()<CR>", "Next diagnostic" },
}

local visual_mappings = {
	["<leader>/"] = { '"zy:Telescope current_buffer_fuzzy_find default_text=<C-r>z<cr>', "Live search current buffer" },
	["<leader>g"] = { '"zy:Telescope live_grep default_text=<C-r>z<cr>', "Live search" },
	["<leader>f"] = { '"zy:Telescope find_files default_text=<C-r>z<cr>', "Search file" },
}

local motion_mappings = {
	["<leader>f"] = { "<Plug>(leap-forward-to)", "Leap forward to" },
	["<leader>F"] = { "<Plug>(leap-backward-to)", "Leap backward to" },
	["<leader>t"] = { "<Plug>(leap-forward-till)", "Leap forward until" },
	["<leader>T"] = { "<Plug>(leap-backward-till)", "Leap backward until" },
}

local wk = require("which-key")
wk.register(normal_mappings, { mode = "n" })
wk.register(visual_mappings, { mode = "v" })
wk.register(motion_mappings, { mode = "o" })
