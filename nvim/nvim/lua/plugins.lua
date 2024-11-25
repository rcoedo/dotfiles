local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
vim.opt.termguicolors = true

require("lazy").setup({
  -- themes
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("tokyonight").setup({
        style = "night",
        styles = {
          floats = "normal",
          sidebars = "normal",
        },
        on_colors = function(colors)
          colors.border = colors.blue0
        end,
        on_highlights = function(highlights, colors)
          highlights.DiagnosticUnnecessary = { fg = colors.comment, underline = true }
        end,
      })
    end,
  },
  "kyazdani42/nvim-web-devicons",
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "kyazdani42/nvim-web-devicons", "folke/tokyonight.nvim" },
    config = function()
      require("lualine").setup({
        options = {
          theme = "tokyonight",
        },
      })
    end,
  },

  -- tpope & edit stuff
  "tpope/vim-commentary",
  "tpope/vim-surround",
  "tpope/vim-abolish",
  "tpope/vim-repeat",
  -- "tpope/vim-vinegar",
  "tpope/vim-ragtag",
  "tpope/vim-endwise",
  -- "tpope/vim-speeddating",
  "tpope/vim-fugitive",
  "wellle/targets.vim",
  "ton/vim-bufsurf",

  -- {
  -- 	"airblade/vim-rooter",
  -- 	config = function()
  -- 		vim.g.rooter_cd_cmd = "lcd"
  -- 	end,
  -- },

  {
    "notjedi/nvim-rooter.lua",
    config = function()
      require("nvim-rooter").setup()
    end,
  },

  -- {
  -- 	"zbirenbaum/copilot.lua",
  -- 	config = function()
  -- 		require("copilot").setup({
  -- 			suggestion = { enabled = true },
  -- 			panel = { enabled = false },
  -- 		})
  -- 	end,
  -- },

  -- {
  --   "ggandor/leap.nvim",
  --   config = function()
  --     local leap = require("leap")
  --     leap.opts.case_sensitive = true
  --     leap.opts.max_highlighted_traversal_targets = 0
  --   end,
  -- },

  -- {
  -- 	"rapan931/lasterisk.nvim",
  -- 	config = function()
  -- 		vim.keymap.set("n", "*", function()
  -- 			require("lasterisk").search()
  -- 		end)
  -- 		vim.keymap.set({ "n", "x" }, "g*", function()
  -- 			require("lasterisk").search({ is_whole = false })
  -- 		end)
  -- 	end,
  -- },
  --
  -- {
  -- 	"mrjones2014/smart-splits.nvim",
  -- 	lazy = false,
  -- 	version = ">=1.0.0",
  -- 	config = function()
  -- 		require("smart-splits").setup({
  -- 			at_edge = "stop",
  -- 		})
  -- 	end,
  -- },

  {
    "nvim-tree/nvim-tree.lua",
    version = "*",
    dependencies = {
      "kyazdani42/nvim-web-devicons",
    },
    config = function()
      require("nvim-tree").setup({})
    end,
  },

  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup()
    end,
  },

  {
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
  },

  { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  -- telescope
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-fzf-native.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
      "rcoedo/telescope-ghq.nvim",
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
              ["<c-bs>"] = actions.delete_buffer,
              ["<c-b>"] = actions.preview_scrolling_up,
              ["<c-f>"] = actions.preview_scrolling_down,
              ["<c-space>"] = actions.toggle_selection,
              ["<c-cr>"] = actions.select_vertical,
              ["<c-s-cr>"] = actions.select_horizontal,
              ["<C-i>"] = "which_key",
            },
          },
        },
        layout_strategy = "flex",
        pickers = {
          diagnostics = {
            theme = "dropdown",
            -- previewer = false,
            layout_config = {
              width = 1,
            },
          },
          buffers = {
            theme = "dropdown",
            mappings = {
              i = {
                ["<c-d>"] = actions.delete_buffer,
                ["<c-bs>"] = actions.delete_buffer,
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
                ["<c-bs>"] = telescope.extensions.file_browser.actions.remove,
                ["<tab>"] = actions.select_default,
              },
            },
          },
          ghq = {
            theme = "ivy",
          },
        },
      })
      telescope.load_extension("fzf")
      telescope.load_extension("notify")
      telescope.load_extension("file_browser")
      telescope.load_extension("ghq")
    end,
  },

  {
    "stevearc/dressing.nvim",
    config = function()
      require("dressing").setup({})
    end,
  },

  {
    "rcarriga/nvim-notify",
    config = function()
      require("notify").setup()
      vim.notify = require("notify")
    end,
  },

  -- treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects", "HiPhish/nvim-ts-rainbow2" },
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "rust",
          "fish",
          "lua",
          "bash",
          "javascript",
          "typescript",
          "tsx",
          "toml",
          "json",
          "yaml",
          "html",
          "scss",
          "markdown",
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
        -- rainbow = {
        --   enable = true,
        --   query = "rainbow-parens",
        --   strategy = require("ts-rainbow").strategy.global,
        -- },
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
  },

  -- Better indentation for .tsx
  "HerringtonDarkholme/yats.vim",

  -- Edgedb (no treesitter support as of April 2023)
  "edgedb/edgedb-vim",

  "windwp/nvim-ts-autotag",
  {
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup({
        check_ts = true,
      })
    end,
  },

  -- Completion
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      -- other cmp plugins
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      -- snippets
      "hrsh7th/cmp-vsnip",
      "hrsh7th/vim-vsnip",
      "onsails/lspkind.nvim",
      -- "zbirenbaum/copilot-cmp",
    },
    config = function()
      local cmp = require("cmp")
      if not cmp then
        return
      end

      local lspkind = require("lspkind")
      lspkind.init({
        symbol_map = {
          TypeParameter = "",
          -- Copilot = "",
        },
      })

      -- require("copilot_cmp").setup()

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
          -- ["<Tab>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
          -- ["<S-Tab>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
          ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
          ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
          ["<CR>"] = cmp.mapping.confirm({ select = false, behavior = cmp.ConfirmBehavior.Insert }),
          -- Toggle completion
          ["<c-space>"] = cmp.mapping(function()
            if cmp.visible() then
              cmp.close()
            else
              cmp.complete()
            end
          end),
        }),
        sources = cmp.config.sources({
          -- { name = "copilot", group_index = 2 },
          { name = "nvim_lsp", group_index = 2 },
          { name = "vsnip",    group_index = 2 },
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
        sorting = {
          priority_weight = 2,
          comparators = {
            -- require("copilot_cmp.comparators").prioritize,
            -- Below is the default comparitor list and order for nvim-cmp
            cmp.config.compare.offset,
            -- cmp.config.compare.scopes, --this is commented in nvim-cmp too
            cmp.config.compare.exact,
            cmp.config.compare.score,
            cmp.config.compare.recently_used,
            cmp.config.compare.locality,
            cmp.config.compare.kind,
            cmp.config.compare.sort_text,
            cmp.config.compare.length,
            cmp.config.compare.order,
          },
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
          ["<S-Tab>"] = cmp.mapping(mapping_helpers.do_or_complete(cmp.select_prev_item), {
            "i",
            "c",
          }),
          ["<Tab>"] = cmp.mapping(mapping_helpers.do_or_complete(cmp.select_next_item), {
            "i",
            "c",
          }),
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
  },

  {

    "williamboman/mason.nvim",
    config = function()
      local mason = require("mason")
      mason.setup({
        ui = {
          icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗",
          },
        },
      })
    end,
  },

  -- LSP
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = {
      "williamboman/mason.nvim",
      "neovim/nvim-lspconfig",
      "jose-elias-alvarez/null-ls.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    config = function()
      local lspconfig = require("lspconfig")
      local mason = require("mason")
      local mason_lspconfig = require("mason-lspconfig")
      local null_ls = require("null-ls")
      local cmp_nvim_lsp = require("cmp_nvim_lsp")

      mason.setup({
        ui = {
          icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗",
          },
        },
      })

      -- local function auto_on_buffer_save(callback)
      --   return function(augroup, client, bufnr)
      --     if client.supports_method("textDocument/formatting") then
      --       vim.api.nvim_create_autocmd("BufWritePre", {
      --         group = "Buffer",
      --         buffer = bufnr,
      --         callback = function()
      --           callback(client, bufnr)
      --           -- vim.lsp.buf.format({ async = false })
      --           -- vim.lsp.buf.code_action({ context = { only = { "source.organizeImports.rome" } }, apply = true })
      --         end,
      --       })
      --     end
      --   end
      -- end

      -- local auto_format_on_save = auto_on_buffer_save(function()
      --   vim.lsp.buf.format({ async = false })
      -- end)

      local default_options = {
        flags = { debounce_text_changes = 150 },
        capabilities = cmp_nvim_lsp.default_capabilities(),
      }

      local option_overrides = {
        ["tsserver"] = {
          single_file_support = false,
          root_dir = lspconfig.util.root_pattern("package.json"),
        },
        ["denols"] = {
          root_dir = lspconfig.util.root_pattern("deno.json"),
        },
        ["jsonls"] = {
          init_options = {
            provideFormatter = false,
          },
        },
        ["biome"] = {
          -- on_attach = function( client, bufnr)
          --   local augroup = vim.api.nvim_create_augroup("RomeOnSave", {})
          --   auto_format_on_save(augroup, client, bufnr)
          -- end,
          root_dir = lspconfig.util.root_pattern("biome.json", "biome.jsonc"),
          single_file_support = false,
        },
        ["rust-analyzer"] = {
          assist = {
            importEnforceGranularity = true,
            importPrefix = "crate",
          },
          cargo = {
            allFeatures = true,
          },
          checkOnSave = {
            command = "clippy",
          },
          inlayHints = { locationLinks = false },
          diagnostics = {
            enable = true,
            experimental = {
              enable = true,
            },
          },
        },
        ["lua_ls"] = {
          settings = {
            Lua = {
              runtime = {
                version = "LuaJIT",
              },
              diagnostics = {
                globals = { "vim" },
              },
              workspace = {
                library = vim.api.nvim_get_runtime_file("", true),
                checkThirdParty = false,
              },
              telemetry = {
                enable = false,
              },
            },
          },
        },
        ["null-ls"] = {
          -- on_attach = function(client, bufnr)
          -- auto_format_on_save(client, bufnr)
          -- end,
          -- on_attach = function(attached_client, bufnr)
          --   if attached_client.supports_method("textDocument/formatting") then
          --   	local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
          --   	vim.api.nvim_create_autocmd("BufWritePre", {
          --   		group = augroup,
          --   		buffer = bufnr,
          --   		callback = function()
          --   			vim.lsp.buf.format({
          --   				bufnr = bufnr,
          --   				filter = function(client)
          --   					return client.name == "null-ls"
          --   				end,
          --   			})
          --   		end,
          --   	})
          --   end
          -- on_attach = function(client, bufnr)
          -- if client.supports_method("textDocument/formatting") then
          --   vim.api.nvim_clear_autocmds({ group = LspActions, buffer = bufnr })
          --   vim.api.nvim_create_autocmd("BufWritePre", {
          --     group = LspActions,
          --     buffer = bufnr,
          --     callback = function()
          --       -- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
          --       -- on later neovim version, you should use vim.lsp.buf.format({ async = false }) instead
          --       vim.lsp.buf.format({ async = false })
          --     end,
          --   })
          -- end
          -- end, -- end,
          sources = {
            -- fish
            null_ls.builtins.diagnostics.fish,
            null_ls.builtins.formatting.fish_indent,
            -- prisma
            -- null_ls.builtins.formatting.prismafmt,
            -- lua
            null_ls.builtins.formatting.stylua,
            -- bash
            null_ls.builtins.formatting.shfmt,
            -- null_ls.builtins.code_actions.shellcheck,
            -- css
            null_ls.builtins.diagnostics.stylelint.with({ only_local = "node_modules/.bin" }),
            -- js/ts
            -- null_ls.builtins.formatting.rome.with({
            --   command = { "biome" },
            --   extra_filetypes = { "jsonc" },
            --   only_local = "node_modules/.bin",
            -- }),
            -- null_ls.builtins.code_actions.eslint.with({ only_local = "node_modules/.bin" }),
            -- null_ls.builtins.diagnostics.eslint.with({ only_local = "node_modules/.bin" }),
            -- null_ls.builtins.formatting.prettier.with({ only_local = "node_modules/.bin" }),
            -- null_ls.builtins.completion.spell,
          },
        },
      }

      local function get_lsp_options(server_name)
        return option_overrides[server_name]
            and vim.tbl_deep_extend("force", default_options, option_overrides[server_name])
            or default_options
      end

      null_ls.setup(get_lsp_options("null-ls"))

      mason_lspconfig.setup({
        ensure_installed = {
          "bashls",
          "cssls",
          "html",
          "jsonls",
          "lua_ls",
          "marksman",
          "taplo",
          "rust_analyzer",
          "biome",
          -- "rustfmt",
          "tsserver",
          "vimls",
          "denols",
        },
        handlers = {
          function(server_name)
            lspconfig[server_name].setup(get_lsp_options(server_name))
          end,
        },
      })
    end,
  },

  {
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup({
        window = {
          border = "single",
          padding = { 0, 0, 0, 0 },
        },
      })
    end,
  },
})
