" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif

packadd packer.nvim

try

lua << END
  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time("Luarocks path setup", true)
local package_path_str = "/Users/rcoedo/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/Users/rcoedo/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/Users/rcoedo/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/Users/rcoedo/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/Users/rcoedo/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time("Luarocks path setup", false)
time("try_loadstring definition", true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time("try_loadstring definition", false)
time("Defining packer_plugins", true)
_G.packer_plugins = {
  gruvbox = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/gruvbox"
  },
  ["lualine.nvim"] = {
    config = { "\27LJ\2\n]\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\foptions\1\0\0\1\0\1\ntheme\fgruvbox\nsetup\flualine\frequire\0" },
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/lualine.nvim"
  },
  nerdcommenter = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/nerdcommenter"
  },
  nerdtree = {
    config = { "\27LJ\2\n³\1\0\0\a\0\t\1\0256\0\0\0009\0\1\0009\0\2\0006\2\0\0009\2\1\0029\2\3\0026\4\0\0009\4\1\0049\4\4\4'\6\5\0B\4\2\0A\2\0\0A\0\0\2\t\0\0\0X\1\5€6\1\0\0009\1\6\1'\3\a\0B\1\2\1X\1\4€6\1\0\0009\1\6\1'\3\b\0B\1\2\1K\0\1\0\19NERDTreeToggle\17NERDTreeFind\bcmd\b%:p\vexpand\fbufname\17filereadable\afn\bvim\2Ú\1\1\0\2\0\v\0\0196\0\0\0009\0\1\0'\1\3\0=\1\2\0006\0\0\0009\0\1\0'\1\5\0=\1\4\0006\0\0\0009\0\1\0005\1\a\0=\1\6\0006\0\0\0009\0\1\0)\1\1\0=\1\b\0003\0\t\0007\0\n\0K\0\1\0\19open_nerd_tree\0\22NERDTReeMinimalUI\1\2\0\0\17node_modules\19NERDTreeIgnore\bâ–¾ NERDTreeDirArrowCollapsible\bâ–¸\31NERDTreeDirArrowExpandable\6g\bvim\0" },
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/nerdtree"
  },
  ["nvim-compe"] = {
    config = { "\27LJ\2\n \1\0\0\4\0\n\0\r6\0\0\0009\0\1\0'\1\3\0=\1\2\0006\0\4\0'\2\5\0B\0\2\0029\0\6\0005\2\a\0005\3\b\0=\3\t\2B\0\2\1K\0\1\0\vsource\1\0\3\rnvim_lsp\2\tpath\2\vbuffer\2\1\0\1\fenabled\2\nsetup\ncompe\frequire\21menuone,noselect\16completeopt\6o\bvim\0" },
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/nvim-compe"
  },
  ["nvim-lspconfig"] = {
    config = { "\27LJ\2\nH\0\0\3\0\4\0\b6\0\0\0'\2\1\0B\0\2\0029\0\2\0009\0\3\0004\2\0\0B\0\2\1K\0\1\0\nsetup\rtsserver\14lspconfig\frequire\0" },
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\nÁ\1\0\0\4\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\4\0=\3\5\0025\3\6\0=\3\a\2B\0\2\1K\0\1\0\frainbow\1\0\3\18extended_mode\2\venable\2\19max_file_lines\3è\a\14highlight\1\0\1\venable\2\1\0\1\21ensure_installed\15maintained\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-ts-rainbow"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/nvim-ts-rainbow"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = false,
    needs_bufread = false,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/opt/packer.nvim"
  },
  playground = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/playground"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["vim-devicons"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-devicons"
  },
  ["vim-endwise"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-endwise"
  },
  ["vim-expand-region"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-expand-region"
  },
  ["vim-fish"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-fish"
  },
  ["vim-fugitive"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-fugitive"
  },
  ["vim-ragtag"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-ragtag"
  },
  ["vim-repeat"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-repeat"
  },
  ["vim-speeddating"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-speeddating"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-surround"
  },
  ["vim-trailing-whitespace"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-trailing-whitespace"
  },
  ["vim-vinegar"] = {
    loaded = true,
    path = "/Users/rcoedo/.local/share/nvim/site/pack/packer/start/vim-vinegar"
  }
}

time("Defining packer_plugins", false)
-- Config for: nvim-treesitter
time("Config for nvim-treesitter", true)
try_loadstring("\27LJ\2\nÁ\1\0\0\4\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\4\0=\3\5\0025\3\6\0=\3\a\2B\0\2\1K\0\1\0\frainbow\1\0\3\18extended_mode\2\venable\2\19max_file_lines\3è\a\14highlight\1\0\1\venable\2\1\0\1\21ensure_installed\15maintained\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-treesitter")
time("Config for nvim-treesitter", false)
-- Config for: lualine.nvim
time("Config for lualine.nvim", true)
try_loadstring("\27LJ\2\n]\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\foptions\1\0\0\1\0\1\ntheme\fgruvbox\nsetup\flualine\frequire\0", "config", "lualine.nvim")
time("Config for lualine.nvim", false)
-- Config for: nerdtree
time("Config for nerdtree", true)
try_loadstring("\27LJ\2\n³\1\0\0\a\0\t\1\0256\0\0\0009\0\1\0009\0\2\0006\2\0\0009\2\1\0029\2\3\0026\4\0\0009\4\1\0049\4\4\4'\6\5\0B\4\2\0A\2\0\0A\0\0\2\t\0\0\0X\1\5€6\1\0\0009\1\6\1'\3\a\0B\1\2\1X\1\4€6\1\0\0009\1\6\1'\3\b\0B\1\2\1K\0\1\0\19NERDTreeToggle\17NERDTreeFind\bcmd\b%:p\vexpand\fbufname\17filereadable\afn\bvim\2Ú\1\1\0\2\0\v\0\0196\0\0\0009\0\1\0'\1\3\0=\1\2\0006\0\0\0009\0\1\0'\1\5\0=\1\4\0006\0\0\0009\0\1\0005\1\a\0=\1\6\0006\0\0\0009\0\1\0)\1\1\0=\1\b\0003\0\t\0007\0\n\0K\0\1\0\19open_nerd_tree\0\22NERDTReeMinimalUI\1\2\0\0\17node_modules\19NERDTreeIgnore\bâ–¾ NERDTreeDirArrowCollapsible\bâ–¸\31NERDTreeDirArrowExpandable\6g\bvim\0", "config", "nerdtree")
time("Config for nerdtree", false)
-- Config for: nvim-compe
time("Config for nvim-compe", true)
try_loadstring("\27LJ\2\n \1\0\0\4\0\n\0\r6\0\0\0009\0\1\0'\1\3\0=\1\2\0006\0\4\0'\2\5\0B\0\2\0029\0\6\0005\2\a\0005\3\b\0=\3\t\2B\0\2\1K\0\1\0\vsource\1\0\3\rnvim_lsp\2\tpath\2\vbuffer\2\1\0\1\fenabled\2\nsetup\ncompe\frequire\21menuone,noselect\16completeopt\6o\bvim\0", "config", "nvim-compe")
time("Config for nvim-compe", false)
-- Config for: nvim-lspconfig
time("Config for nvim-lspconfig", true)
try_loadstring("\27LJ\2\nH\0\0\3\0\4\0\b6\0\0\0'\2\1\0B\0\2\0029\0\2\0009\0\3\0004\2\0\0B\0\2\1K\0\1\0\nsetup\rtsserver\14lspconfig\frequire\0", "config", "nvim-lspconfig")
time("Config for nvim-lspconfig", false)
if should_profile then save_profiles() end

END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
