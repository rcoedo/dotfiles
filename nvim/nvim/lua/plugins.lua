vim.cmd([[packadd packer.nvim]])
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

require("packer").startup(function(use)
	-- Packer
	use({ "wbthomason/packer.nvim" })

	-- themes
	use("folke/tokyonight.nvim")
	use("kyazdani42/nvim-web-devicons")
	use({
		"nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons" },
		config = function()
			require("lualine").setup({
				options = {
					theme = "tokyonight",
				},
			})
		end,
	})

	-- tpope & edit stuff
	use("tpope/vim-commentary")
	use("tpope/vim-surround")
	use("tpope/vim-repeat")
	use("tpope/vim-vinegar")
	use("tpope/vim-ragtag")
	use("tpope/vim-endwise")
	use("tpope/vim-speeddating")
	use("tpope/vim-fugitive")

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

	-- telescope
	use({
		"nvim-telescope/telescope.nvim",
		requires = { "nvim-lua/plenary.nvim" },
		config = function()
			local actions = require("telescope.actions")
			local telescope = require("telescope")
			telescope.setup({
				defaults = {
					mappings = {
						i = {
							["<esc>"] = actions.close,
						},
					},
				},
			})
		end,
	})

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
				indent = {
					enable = false,
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
	use("p00f/nvim-ts-rainbow")
	use("HerringtonDarkholme/yats.vim")
	-- use("nvim-treesitter/playground")

	use({
		"windwp/nvim-autopairs",
		wants = "nvim-treesitter",
		module = { "nvim-autopairs.completion.cmp", "nvim-autopairs" },
		config = function()
			require("nvim-autopairs").setup({
				map_cr = true,
			})
		end,
	})

	-- Completion
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			-- other cmp plugins
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-cmdline",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			-- snippets
			"hrsh7th/cmp-vsnip",
			"hrsh7th/vim-vsnip",
			"onsails/lspkind.nvim",
			"windwp/nvim-autopairs",
		},
		config = function()
			local cmp = require("cmp")
			if not cmp then
				return
			end

			local lspkind = require("lspkind")

			local mapping_helpers = {
				fallback = function(fallback)
					fallback()
				end,
				do_or_complete = function(fn)
					return function()
						if cmp.visible() then
							fn()
						else
							cmp.complete()
						end
					end
				end,
			}

			cmp.setup({
				performance = {
					trigger_debounce_time = 500,
				},
				completion = {
					completeopt = "menu,menuone,noinsert",
				},
				snippet = {
					expand = function(args)
						vim.fn["vsnip#anonymous"](args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert({
					["<C-u>"] = cmp.mapping.scroll_docs(-4),
					["<C-d>"] = cmp.mapping.scroll_docs(4),
					["<Tab>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
					["<S-Tab>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
					["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
					["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
					["<CR>"] = cmp.mapping.confirm({ select = false }),
					-- Toggle completion
					["<C-Space>"] = cmp.mapping(function()
						if cmp.visible() then
							cmp.close()
						else
							cmp.complete()
						end
					end),
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp" },
					{ name = "vsnip" },
				}),
				formatting = {
					fields = { "kind", "abbr", "menu" },
					format = function(entry, vim_item)
						local kind = lspkind.cmp_format({ mode = "symbol_text", maxwidth = 50 })(entry, vim_item)
						local strings = vim.split(kind.kind, "%s", { trimempty = true })
						kind.kind = " " .. strings[1] .. " "
						kind.menu = "        " .. strings[2] --.. " (" .. entry.source.name .. ")"
						return kind
					end,
				},
				window = {
					documentation = vim.tbl_deep_extend("force", cmp.config.window.bordered(), {
						max_height = 15,
						max_width = 70,
					}),
					completion = vim.tbl_deep_extend("force", cmp.config.window.bordered(), {
						winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
						col_offset = -3,
					}),
				},
			})

			cmp.setup.cmdline({ "/", "?" }, {
				mapping = cmp.mapping.preset.cmdline(),
				completion = {
					completeopt = "menu,menuone,noselect",
				},
				sources = {
					{ name = "buffer" },
				},
			})

			cmp.setup.cmdline(":", {
				mapping = {
					["<C-p>"] = cmp.mapping(mapping_helpers.fallback, { "i", "c" }),
					["<C-n>"] = cmp.mapping(mapping_helpers.fallback, { "i", "c" }),
					["<S-Tab>"] = cmp.mapping(mapping_helpers.do_or_complete(cmp.select_prev_item), { "i", "c" }),
					["<Tab>"] = cmp.mapping(mapping_helpers.do_or_complete(cmp.select_next_item), { "i", "c" }),
				},
				completion = {
					completeopt = "menu,menuone,noselect",
				},
				sources = cmp.config.sources({
					{ name = "path" },
				}, {
					{ name = "cmdline" },
				}),
			})

			local cmp_autopairs = require("nvim-autopairs.completion.cmp")
			cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done({ map_char = { tex = "" } }))
		end,
	})

	-- LSP
	use({
		"williamboman/mason-lspconfig.nvim",
		requires = {
			"williamboman/mason.nvim",
			"neovim/nvim-lspconfig",
			"hrsh7th/cmp-nvim-lsp",
		},
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

			local lsp_options = {
				flags = { debounce_text_changes = 150 },
				capabilities = require("cmp_nvim_lsp").default_capabilities(),
			}

			local mason_lspconfig = require("mason-lspconfig")

			mason_lspconfig.setup_handlers({
				function(server_name)
					require("lspconfig")[server_name].setup(lsp_options)
				end,
				["sumneko_lua"] = function()
					require("lspconfig").sumneko_lua.setup(vim.tbl_deep_extend("force", lsp_options, {
						settings = {
							Lua = {
								diagnostics = {
									-- Get the language server to recognize the 'vim' global
									globals = { "vim" },
								},
								workspace = {
									-- Make the server aware of Neovim runtime files
									library = vim.api.nvim_get_runtime_file("", true),
								},
							},
						},
					}))
				end,
			})

			mason_lspconfig.setup({
				ensure_installed = { "tsserver", "sumneko_lua", "html", "cssls", "jsonls" },
			})
		end,
	})

	use({
		"jose-elias-alvarez/null-ls.nvim",
		requires = { "lukas-reineke/lsp-format.nvim" },
		config = function()
			local null_ls = require("null-ls")
			---@diagnostic disable-next-line: redundant-parameter
			null_ls.setup({
				on_attach = require("lsp-format").on_attach,
				sources = {
					null_ls.builtins.formatting.prettier,
					null_ls.builtins.formatting.stylua,
					null_ls.builtins.diagnostics.eslint,
					-- null_ls.builtins.completion.spell,
				},
			})
		end,
	})

	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup({
				window = {
					border = "single",
					padding = { 0, 0, 0, 0 },
				},
			})
		end,
	})

	-- use({
	-- 	"glepnir/lspsaga.nvim",
	-- 	config = function()
	-- 		require("lspsaga").init_lsp_saga({
	-- 			border_style = "rounded",
	-- 			code_action_lightbulb = { enable = false },
	-- 		})
	-- 	end,
	-- })
end)
