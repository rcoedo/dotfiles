require("packer").startup(function(use)
    -- Packer
    use {'wbthomason/packer.nvim', opt = true}

    -- gruvbox theme
    use 'gruvbox-community/gruvbox'
    use 'tanvirtin/monokai.nvim'

    -- nerdcommenter
    use 'preservim/nerdcommenter'

    use 'kyazdani42/nvim-web-devicons'
    use {
       'nvim-lualine/lualine.nvim',
       requires = {'kyazdani42/nvim-web-devicons', opt = true}
    }

    -- nerdtree
    use 'ryanoasis/vim-devicons'
    use 'preservim/nerdtree'

    -- tpope stuff
    use 'tpope/vim-surround'
    use 'tpope/vim-repeat'
    use 'tpope/vim-vinegar'
    use 'tpope/vim-ragtag'
    use 'tpope/vim-endwise'
    use 'tpope/vim-speeddating'
    use 'tpope/vim-fugitive'

    -- others
    use 'terryma/vim-expand-region'
    use 'bronson/vim-trailing-whitespace'

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
    use "nvim-telescope/telescope-file-browser.nvim"

    -- treesitter
    use 'p00f/nvim-ts-rainbow'
    use {
       'nvim-treesitter/nvim-treesitter',
       run =':TSUpdate',
       config = function()
          require'nvim-treesitter.configs'.setup {
             ensure_installed = { "javascript", "lua", "rust" },
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

    use "williamboman/mason.nvim"
    use "williamboman/mason-lspconfig.nvim"
    use "neovim/nvim-lspconfig"
    use {
      "glepnir/lspsaga.nvim",
      branch = "main",
      config = function()
        local saga = require("lspsaga")

        saga.init_lsp_saga({
          -- your configuration
        })
      end,
    }

    -- syntax stuff
    use 'dag/vim-fish'
end)
