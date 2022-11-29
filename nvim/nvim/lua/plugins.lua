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
	-- use("tpope/vim-vinegar")
	use("tpope/vim-ragtag")
	use("tpope/vim-endwise")
	-- use("tpope/vim-speeddating")
	use("tpope/vim-fugitive")
	use("wellle/targets.vim")
	use("ton/vim-bufsurf")

	use({
		"nat-418/boole.nvim",
		config = function()
			require("boole").setup({
				mappings = {
					increment = "<C-a>",
					decrement = "<C-x>",
				},
				allow_caps_additions = {
					{ "enable", "disable" },
					{ "enabled", "disabled" },
				},
			})
		end,
	})

	use({
		"ggandor/leap.nvim",
		config = function()
			local leap = require("leap")
			leap.opts.case_sensitive = true
			leap.opts.max_highlighted_traversal_targets = 0
		end,
	})

	use({
		"rapan931/lasterisk.nvim",
		config = function()
			vim.keymap.set("n", "*", function()
				require("lasterisk").search()
			end)
			vim.keymap.set({ "n", "x" }, "g*", function()
				require("lasterisk").search({ is_whole = false })
			end)
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

	-- telescope
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use({
		"nvim-telescope/telescope.nvim",
		requires = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-fzf-native.nvim",
			"nvim-telescope/telescope-file-browser.nvim",
			"rcarriga/nvim-notify",
		},
		config = function()
			local telescope = require("telescope")
			local actions = require("telescope.actions")

			telescope.setup({
				defaults = {
					prompt_prefix = "~ ",
					selection_caret = "  ",
					mappings = {
						i = {
							["<c-u>"] = false,
							["<c-d>"] = false,
							["<left>"] = false,
							["<down>"] = false,
							["<up>"] = false,
							["<right>"] = false,
							["<pageup>"] = false,
							["<pagedown>"] = false,
							["<tab>"] = false,
							["<s-tab>"] = false,
							["<c-v>"] = false,
							["<c-x>"] = false,
							["<c-l>"] = false,
							["<c-c>"] = false,
							["<a-q>"] = false,
							["<c-q>"] = false,
							["<esc>"] = actions.close,
							["<c-b>"] = actions.preview_scrolling_up,
							["<c-f>"] = actions.preview_scrolling_down,
							["<c-space>"] = actions.toggle_selection,
							["<c-cr>"] = actions.select_vertical,
							["<c-s-cr>"] = actions.select_horizontal,
						},
					},
				},
				layout_strategy = "flex",
				pickers = {
					diagnostics = {
						theme = "dropdown",
						-- previewer = false,
						layout_config = {
							width = 0.75,
						},
					},
					buffers = {
						theme = "dropdown",
						mappings = {
							i = {
								["<c-d>"] = actions.delete_buffer,
							},
						},
					},
					current_buffer_fuzzy_find = {
						theme = "ivy",
						sorting_strategy = "ascending",
						previewer = false,
					},
					find_files = {
						theme = "ivy",
						previewer = false,
					},
				},
				extensions = {
					fzf = {
						fuzzy = true,
						override_generic_sorter = true,
						override_file_sorter = true,
					},
					notify = {
						theme = "dropdown",
					},
					file_browser = {
						theme = "ivy",
						previewer = false,
						grouped = true,
						hijack_netrw = true,
						hide_parent_dir = true,
						mappings = {
							i = {
								["<c-j>"] = telescope.extensions.file_browser.actions.create_from_prompt,
								["<c-l>"] = telescope.extensions.file_browser.actions.goto_parent_dir,
								["<tab>"] = actions.select_default,
							},
						},
					},
				},
			})

			telescope.load_extension("fzf")
			telescope.load_extension("notify")
			telescope.load_extension("file_browser")
		end,
	})

	use({
		"stevearc/dressing.nvim",
		config = function()
			require("dressing").setup({})
		end,
	})

	use({
		"rcarriga/nvim-notify",
		config = function()
			require("notify").setup()
			vim.notify = require("notify")
		end,
	})

	-- treesitter
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		requires = { "nvim-treesitter/nvim-treesitter-textobjects", "p00f/nvim-ts-rainbow" },
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
					"json",
					"yaml",
					"html",
					"scss",
				},
				indent = {
					enable = true,
				},
				highlight = {
					enable = true,
				},
				autotag = {
					enable = true,
				},
				rainbow = {
					enable = true,
					extended_mode = true,
					max_file_lines = 1000,
				},
				textobjects = {
					select = {
						enable = true,

						-- Automatically jump forward to textobj, similar to targets.vim
						lookahead = true,

						keymaps = {
							-- You can use the capture groups defined in textobjects.scm
							["af"] = { query = "@function.outer", desc = "@function.outer" },
							["if"] = { query = "@function.inner", desc = "@function.inner" },
							["ac"] = { query = "@class.outer", desc = "@class.outer" },
							["ic"] = { query = "@class.inner", desc = "@class.inner" },
							["ao"] = { query = "@conditional.outer", desc = "@conditional.outer" },
							["io"] = { query = "@conditional.inner", desc = "@conditional.inner" },
							["al"] = { query = "@loop.outer", desc = "@loop.outer" },
							["il"] = { query = "@loop.inner", desc = "@loop.inner" },
						},
						-- You can choose the select mode (default is charwise 'v')
						--
						-- Can also be a function which gets passed a table with the keys
						-- * query_string: eg '@function.inner'
						-- * method: eg 'v' or 'o'
						-- and should return the mode ('v', 'V', or '<c-v>') or a table
						-- mapping query_strings to modes.
						selection_modes = {
							["@parameter.outer"] = "v", -- charwise
							["@function.outer"] = "V", -- linewise
							["@class.outer"] = "<c-v>", -- blockwise
						},
						-- If you set this to `true` (default is `false`) then any textobject is
						-- extended to include preceding or succeeding whitespace. Succeeding
						-- whitespace has priority in order to act similarly to eg the built-in
						-- `ap`.
						--
						-- Can also be a function which gets passed a table with the keys
						-- * query_string: eg '@function.inner'
						-- * selection_mode: eg 'v'
						-- and should return true of false
						include_surrounding_whitespace = true,
					},
				},
			})
		end,
	})
	-- Better indentation for .tsx
	use("HerringtonDarkholme/yats.vim")
	-- Better fish support
	use("dag/vim-fish")

	use("windwp/nvim-ts-autotag")
	use({
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup({
				check_ts = true,
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
				ensure_installed = { "vimls", "tsserver", "sumneko_lua", "html", "cssls", "jsonls" },
			})
		end,
	})

	use({
		"jose-elias-alvarez/null-ls.nvim",
		-- format on save
		requires = { "lukas-reineke/lsp-format.nvim" },
		config = function()
			local null_ls = require("null-ls")
			local js_options = {
				prefer_local = "node_modules/.bin",
			}
			---@diagnostic disable-next-line: redundant-parameter
			null_ls.setup({
				on_attach = require("lsp-format").on_attach,
				sources = {
					null_ls.builtins.formatting.stylua,
					null_ls.builtins.formatting.prettier.with({
						prefer_local = "node_modules/.bin",
					}),
					null_ls.builtins.diagnostics.eslint.with({ only_local = "node_modules/.bin" }),
					null_ls.builtins.diagnostics.stylelint.with({
						prefer_local = "node_modules/.bin",
					}),
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
