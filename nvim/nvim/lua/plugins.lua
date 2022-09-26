require("packer").startup(function(use)
    -- Packer
    use {'wbthomason/packer.nvim', opt = true}

    -- gruvbox theme
    use 'gruvbox-community/gruvbox'
    use 'tanvirtin/monokai.nvim'

    -- nerdcommenter
    use 'preservim/nerdcommenter'

    use 'kyazdani42/nvim-web-devicons'
    --use 'ryanoasis/vim-devicons'
    use {
       'hoob3rt/lualine.nvim',
       requires = {'kyazdani42/nvim-web-devicons', opt = true},
       config = function()
          require'lualine'.setup {
             --options = {
                --theme = 'powerline'
             --}
          }
       end
    }

    -- tpope stuff
    use 'tpope/vim-surround'
    use 'tpope/vim-repeat'
    use 'tpope/vim-vinegar'
    use 'tpope/vim-ragtag'
    use 'tpope/vim-endwise'
    use 'tpope/vim-speeddating'
    use 'tpope/vim-fugitive'

    use 'terryma/vim-expand-region'
    use 'bronson/vim-trailing-whitespace'

    -- nerdtree
    use {
       'preservim/nerdtree',
       config = function()
          vim.g.NERDTreeDirArrowExpandable = '▸'
          vim.g.NERDTreeDirArrowCollapsible = '▾'
          vim.g.NERDTreeIgnore = {'node_modules'}
          vim.g.NERDTReeMinimalUI = 1

          function open_nerd_tree()
             local readable = vim.fn.filereadable(vim.fn.bufname(vim.fn.expand('%:p')))
             if readable == 1 then
                vim.cmd 'NERDTreeFind'
             else
                vim.cmd 'NERDTreeToggle'
             end
          end
       end
    }

    -- completion
    use {
       'hrsh7th/nvim-compe',
       config = function()
          vim.o.completeopt = "menuone,noselect"
          require'compe'.setup {
                enabled = true,
                source = {
                   path = true,
                   buffer = true,
                   nvim_lsp = true,
                }
          }
       end
    }

    -- telescope
    use 'nvim-lua/popup.nvim'
    use 'nvim-lua/plenary.nvim'
    use 'nvim-telescope/telescope.nvim'

    -- treesitter
    use 'p00f/nvim-ts-rainbow'
    use {
       'nvim-treesitter/nvim-treesitter',
       run =':TSUpdate',
       config = function()
          require'nvim-treesitter.configs'.setup {
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
               "scss"
             },
             highlight = {
                enable = true,
             },
             rainbow = {
                enable = true,
                extended_mode = true, -- Highlight also non-parentheses delimiters, boolean or table: lang -> boolean
                max_file_lines = 1000, -- Do not enable for files with more than 1000 lines, int
             }
          }
       end
    }
    use 'nvim-treesitter/playground'

    -- lsp
    use {
       'neovim/nvim-lspconfig',
       config = function()
         require'lspconfig'.tsserver.setup {}
         vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
         vim.lsp.diagnostic.on_publish_diagnostics, {
           underline = true,
           -- This sets the spacing and the prefix, obviously.
           virtual_text = {
             spacing = 4,
             prefix = ''
           }
         }
         )
       end
    }

    use {
      'glepnir/lspsaga.nvim',
      config = function()
        local saga = require("lspsaga")
        saga.init_lsp_saga {
          border_style = "round",
        }
      end
    }

    use {
      'fgheng/winbar.nvim',
      config = function()
        require('winbar').setup({
          enabled = true,

          show_file_path = true,
          show_symbols = true,

          colors = {
            path = '', -- You can customize colors like #c946fd
            file_name = '',
            symbols = '',
          },

          icons = {
            file_icon_default = '',
            seperator = '>',
            editor_state = '●',
            lock_icon = '',
          },

          exclude_filetype = {
            'help',
            'startify',
            'dashboard',
            'packer',
            'neogitstatus',
            'NvimTree',
            'Trouble',
            'alpha',
            'lir',
            'Outline',
            'spectre_panel',
            'toggleterm',
            'qf',
          }
        })
      end
    }

    use {
      "SmiteshP/nvim-navic",
      requires = "neovim/nvim-lspconfig",
      config = function()
        local navic = require("nvim-navic")

        require("lspconfig").clangd.setup {
          on_attach = function(client, bufnr)
            navic.attach(client, bufnr)
          end
        }
      end
    }

    -- syntax stuff
    use 'dag/vim-fish'
end)
