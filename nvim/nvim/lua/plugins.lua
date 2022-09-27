require("packer").startup(function(use)
    -- Packer
    use {'wbthomason/packer.nvim', opt = true}

    -- themes
    use 'gruvbox-community/gruvbox'
    use 'tanvirtin/monokai.nvim'

    -- nerdcommenter
    use 'preservim/nerdcommenter'

    use 'kyazdani42/nvim-web-devicons'

    use {
       'nvim-lualine/lualine.nvim',
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
                extended_mode = true,
                max_file_lines = 1000,
             }
          }
       end
    }
    use 'nvim-treesitter/playground'
    use 'p00f/nvim-ts-rainbow'

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

    -- syntax plugins
    use 'dag/vim-fish'
end)
