require("packer").startup(function(use)
	-- Packer
	use({ "wbthomason/packer.nvim", opt = true })

	-- themes
	use("gruvbox-community/gruvbox")
	use("tanvirtin/monokai.nvim")

	-- nerdcommenter
	use("preservim/nerdcommenter")

	use("kyazdani42/nvim-web-devicons")

	use({
		"nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
		config = function()
			require("lualine").setup({
				--options = {
				--theme = 'powerline'
				--}
			})
		end,
	})

	-- tpope stuff
	use("tpope/vim-surround")
	use("tpope/vim-repeat")
	use("tpope/vim-vinegar")
	use("tpope/vim-ragtag")
	use("tpope/vim-endwise")
	use("tpope/vim-speeddating")
	use("tpope/vim-fugitive")

	use("terryma/vim-expand-region")
	use("bronson/vim-trailing-whitespace")

	use({
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup({})
		end,
	})

	use({
		"windwp/nvim-ts-autotag",
		config = function()
			require("nvim-treesitter.configs").setup({
				autotag = {
					enable = true,
				},
			})
		end,
	})

	use({
		"norcalli/nvim-colorizer.lua",
		config = function()
			require("colorizer").setup()
		end,
	})

	use({
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup({
				current_line_blame = true,
				current_line_blame_opts = {
					virt_text = true,
					virt_text_pos = "eol",
					delay = 500,
					ignore_whitespace = false,
				},
			})
		end,
	})

	-- nerdtree
	use({
		"preservim/nerdtree",
		config = function()
			vim.g.NERDTreeDirArrowExpandable = "▸"
			vim.g.NERDTreeDirArrowCollapsible = "▾"
			vim.g.NERDTreeIgnore = { "node_modules" }
			vim.g.NERDTReeMinimalUI = 1

			function open_nerd_tree()
				local readable = vim.fn.filereadable(vim.fn.bufname(vim.fn.expand("%:p")))
				if readable == 1 then
					vim.cmd("NERDTreeFind")
				else
					vim.cmd("NERDTreeToggle")
				end
			end
		end,
	})

	-- completion
	use({
		"hrsh7th/nvim-compe",
		config = function()
			vim.o.completeopt = "menuone,noselect"
			require("compe").setup({
				enabled = true,
				source = {
					path = true,
					buffer = true,
					nvim_lsp = true,
				},
			})
		end,
	})

	-- telescope
	use("nvim-lua/popup.nvim")
	use("nvim-lua/plenary.nvim")
	use("nvim-telescope/telescope.nvim")

	-- treesitter
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = {
					"rust",
					"fish",
					"lua",
					"javascript",
					"typescript",
					"tsx",
					"toml",
					"fish",
					"json",
					"yaml",
					"html",
					"scss",
				},
				highlight = {
					enable = true,
				},
				rainbow = {
					enable = true,
					extended_mode = true,
					max_file_lines = 1000,
				},
			})
		end,
	})
	use("nvim-treesitter/playground")
	use("p00f/nvim-ts-rainbow")

	-- lsp
	use({
		"neovim/nvim-lspconfig",
		config = function()
			--for type, icon in pairs({ Error = '', Warning = '', Info = '', Hint = '' }) do
			--local hl = "DiagnosticSign" .. type
			--vim.fn.sign_define(hl, { text = icon, texthl= hl, numhl = hl })
			--end
			require("lspconfig").tsserver.setup({})
			vim.lsp.handlers["textDocument/publishDiagnostics"] =
				vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
					underline = true,
					virtual_text = false,
					signs = false,
					c, --virtual_text = {
					--spacing = 4,
					--prefix = ''
					--}
				})
		end,
	})

	use({
		"jose-elias-alvarez/null-ls.nvim",
		requires = { "nvim-lua/plenary.nvim" },
		config = function()
			--local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
			require("null-ls").setup({
				sources = {
					require("null-ls").builtins.formatting.prettierd,
					require("null-ls").builtins.formatting.stylua,
					require("null-ls").builtins.diagnostics.eslint,
					require("null-ls").builtins.completion.spell,
				},
				--on_attach = function(client, bufnr)
				--if client.supports_method("textDocument/formatting") then
				--vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
				--vim.api.nvim_create_autocmd("BufWritePre", {
				--group = augroup,
				--buffer = bufnr,
				--callback = function()
				--vim.lsp.buf.format({ bufnr = bufnr })
				--end,
				--})
				--end
				--end,
			})
		end,
	})

	--use {
	--'MunifTanjim/prettier.nvim',
	--requires = { "jose-elias-alvarez/null-ls.nvim" },
	--config = function()
	--require("prettier").setup()
	--end
	--}

	use({
		"williamboman/mason.nvim",
		config = function()
			require("mason").setup({
				ui = {
					icons = {
						package_installed = "✓",
						package_pending = "➜",
						package_uninstalled = "✗",
					},
				},
			})
		end,
	})

	use({
		"folke/trouble.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = function()
			require("trouble").setup({
				-- your configuration comes here
				-- or leave it empty to use the default settings
				-- refer to the configuration section below
			})
		end,
	})

	use({
		"glepnir/lspsaga.nvim",
		config = function()
			require("lspsaga").init_lsp_saga({
				border_style = "rounded",
				code_action_lightbulb = { enable = false },
			})
		end,
	})

	-- syntax plugins
	use("dag/vim-fish")
end)
