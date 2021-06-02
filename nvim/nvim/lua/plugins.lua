require("packer").startup(function(use)
    -- Packer
    use {'wbthomason/packer.nvim', opt = true}

    -- gruvbox theme
    use 'gruvbox-community/gruvbox'

    -- nerdcommenter
    use 'preservim/nerdcommenter'

    use 'kyazdani42/nvim-web-devicons'
    use 'ryanoasis/vim-devicons'
    use {
       'hoob3rt/lualine.nvim',
       requires = {'kyazdani42/nvim-web-devicons', opt = true},
       config = function()
          require'lualine'.setup {
             options = {
                theme = 'gruvbox'
             }
          }
       end
    }

    -- nerdtree
    use {
       'preservim/nerdtree',
       config = function()
          vim.g.NERDTreeDirArrowExpandable = '▸'
          vim.g.NERDTreeDirArrowCollapsible = '▾'
          vim.g.NERDTreeIgnore = {'node_modules'}

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
    use 'AndrewRadev/splitjoin.vim'

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
             ensure_installed = "maintained",
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
       end
    }

    -- syntax stuff
    use 'dag/vim-fish'
end)
