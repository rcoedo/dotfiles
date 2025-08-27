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

-- map("n", "<a-h>", require("smart-splits").move_cursor_left, opts)
-- map("n", "<a-j>", require("smart-splits").move_cursor_down, opts)
-- map("n", "<a-k>", require("smart-splits").move_cursor_up, opts)
-- map("n", "<a-l>", require("smart-splits").move_cursor_right, opts)

-- -- moving between splits
-- vim.keymap.set('n', '<C-h>', require('smart-splits').move_cursor_left)
-- vim.keymap.set('n', '<C-j>', require('smart-splits').move_cursor_down)
-- vim.keymap.set('n', '<C-k>', require('smart-splits').move_cursor_up)
-- vim.keymap.set('n', '<C-l>', require('smart-splits').move_cursor_right)
-- -- swapping buffers between windows
-- vim.keymap.set('n', '<leader><leader>h', require('smart-splits').swap_buf_left)
-- vim.keymap.set('n', '<leader><leader>j', require('smart-splits').swap_buf_down)
-- vim.keymap.set('n', '<leader><leader>k', require('smart-splits').swap_buf_up)
-- vim.keymap.set('n', '<leader><leader>l', require('smart-splits').swap_buf_right)

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

-- map("n", "*", "<Plug>(asterisk-z*)", { noremap = false })
-- map("n", "#", "<Plug>(asterisk-z#)", { noremap = false })
-- map("n", "g*", "<Plug>(asterisk-gz*)", { noremap = false })
-- map("n", "g#", "<Plug>(asterisk-gz#)", { noremap = false })

vim.keymap.set({ "n", "v", "o" }, "zf", "<Plug>(leap-forward-to)", { desc = "Leap forward to" })
vim.keymap.set({ "n", "v", "o" }, "zF", "<Plug>(leap-backward-to)", { desc = "Leap backward to" })
vim.keymap.set({ "n", "v", "o" }, "zt", "<Plug>(leap-forward-till)", { desc = "Leap forward until" })
vim.keymap.set({ "n", "v", "o" }, "zT", "<Plug>(leap-backward-till)", { desc = "Leap backward until" })

local wk = require("which-key")
wk.add({
  -- Normal mode
  {
    mode = "n",
    -- Quick mappings
    { "<leader>/",        "<cmd>Telescope current_buffer_fuzzy_find<cr>",            desc = "Live search current buffer" },
    { "<leader>0",        "<cmd>close<cr>",                                          desc = "Close window" },
    { "<leader>2",        "<cmd>split<cr>",                                          desc = "Split horizontally" },
    { "<leader>3",        "<cmd>vsplit<cr>",                                         desc = "Split vertically" },
    { "<leader><leader>", "<cmd>Telescope resume<cr>",                               desc = "Pick resume" },
    { "<leader>D",        "<cmd>Telescope diagnostics<cr>",                          desc = "Pick workspace diagnostic" },
    { "<leader>a",        "<cmd>lua vim.lsp.buf.code_action()<cr>",                  desc = "Pick code action" },
    { "<leader>d",        "<cmd>Telescope diagnostics bufnr=0<cr>",                  desc = "Pick diagnostic" },
    { "<leader>f",        "<cmd>Telescope file_browser path=%:p:h<cr>",              desc = "Pick file" },
    { "<leader>g",        "<cmd>Telescope live_grep<cr>",                            desc = "Live search" },
    { "<leader>w",        "<cmd>Telescope grep_string<cr>",                          desc = "Find word" },
    { "<leader>o",        "<cmd>Telescope ghq<cr>",                                  desc = "Pick project" },
    { "<leader>t",        "<cmd>Telescope find_files<cr>",                           desc = "Pick fuzzy file" },
    { "<leader>u",        "<cmd>Telescope buffers<cr>",                              desc = "Pick buffer" },

    -- Pick table
    { "<leader>p",        group = "Pick" },
    { "<leader>pD",       "<cmd>Telescope diagnostics<cr>",                          desc = "Pick workspace diagnostic" },
    { "<leader>pa",       "<cmd>lua vim.lsp.buf.code_action()<cr>",                  desc = "Pick code action" },
    { "<leader>pc",       "<cmd>Telescope commands<cr>",                             desc = "Pick command" },
    { "<leader>pd",       "<cmd>Telescope diagnostics bufnr=0<cr>",                  desc = "Pick diagnostic" },
    { "<leader>pf",       "<cmd>Telescope file_browser path=%:p:h<cr>",              desc = "Pick file" },
    { "<leader>po",       "<cmd>Telescope ghq<cr>",                                  desc = "Pick project" },
    { "<leader>pt",       "<cmd>Telescope find_files<cr>",                           desc = "Pick fuzzy file" },
    { "<leader>pu",       "<cmd>Telescope buffers<cr>",                              desc = "Pick buffer" },
    { "<leader>pn",       "<cmd>Telescope notify<cr>",                               desc = "Pick notifications" },
    { "<leader>pr",       "<cmd>Telescope lsp_references<cr>",                       desc = "Pick references" },

    -- Quit table
    { "<leader>q",        group = "Quit" },
    { "<leader>qI",       "<cmd>close!<cr>",                                         desc = "Close window!" },
    { "<leader>qT",       "<cmd>tabclose!<cr>",                                      desc = "Close tab!" },
    { "<leader>qU",       "<cmd>bd!<cr>",                                            desc = "Close buffer!" },
    { "<leader>qi",       "<cmd>close<cr>",                                          desc = "Close window" },
    { "<leader>qt",       "<cmd>tabclose<cr>",                                       desc = "Close tab" },
    { "<leader>qu",       "<cmd>bd<cr>",                                             desc = "Close buffer" },

    -- Refactor table
    { "<leader>r",        group = "Refactor" },
    { "<leader>rf",       "<cmd>lua vim.lsp.buf.format({ async = true })<cr>",       desc = "Format" },
    { "<leader>rr",       "<cmd>lua vim.lsp.buf.rename()<cr>",                       desc = "Rename" },

    -- Show table
    { "<leader>s",        group = "Show" },
    { "<leader>sc",       "<cmd>Telescope lsp_outgoing_calls<cr>",                   desc = "Show outgoing calls" },
    { "<leader>sd",       "<cmd>lua vim.diagnostic.open_float()<cr>",                desc = "Show diagnostic" },
    { "<leader>sh",       "<cmd>lua vim.lsp.buf.hover()<cr>",                        desc = "Show hover" },
    { "<leader>sn",       "<cmd>Telescope notify<cr>",                               desc = "Show notification history" },
    { "<leader>sr",       "<cmd>Telescope lsp_references<cr>",                       desc = "Show references" },
    { "<leader>ss",       "<cmd>lua vim.lsp.buf.signature_help()<cr>",               desc = "Show signature" },
    { "<leader>st",       "<cmd>Telescope help_tags<cr>",                            desc = "Show help tags" },
    { "<leader>su",       "<cmd>Telescope lsp_incoming_calls<cr>",                   desc = "Show usages" },

    -- Delete table
    { "<leader>x",        group = "Delete" },
    { "<leader>xu",       "<cmd>lua require('lib').confirm_and_delete_buffer()<cr>", desc = "Delete buffer" },

    -- Previous table
    { "[",                group = "Previous" },
    { "[b",               "<Plug>(buf-surf-back)",                                   desc = "Previous buffer" },
    { "[d",               "<cmd>lua vim.diagnostic.goto_prev()<CR>",                 desc = "Previous diagnostic" },
    { "[t",               "<cmd>tabprevious<cr>",                                    desc = "Previous tab" },

    -- Next table
    { "]",                group = "Next" },
    { "]b",               "<Plug>(buf-surf-forward)",                                desc = "Next buffer" },
    { "]d",               "<cmd>lua vim.diagnostic.goto_next()<CR>",                 desc = "Next diagnostic" },
    { "]t",               "<cmd>tabnext<cr>",                                        desc = "Next tab" },

    -- Go table
    { "gD",               "<cmd>lua vim.lsp.buf.declaration()<cr>",                  desc = "Go to declaration" },
    { "gJ",               "<Plug>(leap-forward-till)",                               desc = "Leap forward until" },
    { "gK",               "<Plug>(leap-backward-till)",                              desc = "Leap backward until" },
    { "gd",               "<cmd>Telescope lsp_definitions<cr>",                      desc = "Go to definition" },
    { "gi",               "<cmd>Telescope lsp_implementations<cr>",                  desc = "Go to implementation" },
    { "gj",               "<Plug>(leap-forward-to)",                                 desc = "Leap forward to" },
    { "gk",               "<Plug>(leap-backward-to)",                                desc = "Leap backward to" },
    { "go",               "<cmd>lua vim.lsp.buf.type_definition()<cr>",              desc = "Go to type definition" },
  },

  -- Visual mode
  {
    mode = { "v" },
    -- Quick mappings
    { "<leader>/",  '"zy:Telescope current_buffer_fuzzy_find default_text=<C-r>z<cr>', desc = "Live search current buffer" },
    { "<leader>f",  '"zy:Telescope find_files default_text=<C-r>z<cr>',                desc = "Search file" },
    { "<leader>g",  '"zy:Telescope live_grep default_text=<C-r>z<cr>',                 desc = "Live search" },
    { "<leader>w",  '"zy:Telescope grep_string',                                       desc = "Find word" },

    -- Edit table
    { "<leader>e",  group = "Edit" },
    { "<leader>es", ":sort<cr>",                                                       desc = "Sort" },
  },

  -- Motion mode
  {
    mode = { "o" },
    { "<leader>F", "<Plug>(leap-backward-to)",   desc = "Leap backward to" },
    { "<leader>T", "<Plug>(leap-backward-till)", desc = "Leap backward until" },
    { "<leader>f", "<Plug>(leap-forward-to)",    desc = "Leap forward to" },
    { "<leader>t", "<Plug>(leap-forward-till)",  desc = "Leap forward until" },
  },
})
