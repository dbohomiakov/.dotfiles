local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
  print "Installing packer close and reopen Neovim..."
  vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- Have packer use a popup window
packer.init({
  display = {
    open_fn = function()
      return require("packer.util").float({ border = "rounded" })
    end,
  },
})

require("packer").startup(function(use)
  use("wbthomason/packer.nvim")
  -- Keyboard
  use("folke/which-key.nvim")
  use("max397574/better-escape.nvim")
  -- Mason
  use("williamboman/mason.nvim")
  use("williamboman/mason-lspconfig.nvim")
  -- LSP
  use("neovim/nvim-lspconfig")
  use("jose-elias-alvarez/null-ls.nvim")
  use({
    "filipdutescu/renamer.nvim",
    branch = "master",
    requires = { {"nvim-lua/plenary.nvim"} }
  })
  use({
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require("trouble").setup {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      }
    end
  })
  -- Virtualenv
  use("AckslD/swenv.nvim")
  -- Debug
  use({
      "andrewferrier/debugprint.nvim",
      config = function()
          require("debugprint").setup()
      end,
  })
  -- Outline
  use("simrat39/symbols-outline.nvim")
  -- 
  -- Colorscheme
  -- use("shaunsingh/nord.nvim")
  -- use("andersevenrud/nordic.nvim")
  use {
      'maxmx03/solarized.nvim',
      config = function()
        vim.o.background = 'dark' -- or 'light'
  
        vim.cmd.colorscheme 'solarized'
      end
  }
  -- UI improvements
  use("stevearc/dressing.nvim")
  use("rebelot/heirline.nvim")
  -- Surround
  use({"echasnovski/mini.nvim", branch = "stable"}) -- TODO: check if it is better then below
  use("kylechui/nvim-surround")
  -- Treesitter
  use({
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
  })
  use("nvim-treesitter/nvim-treesitter-textobjects")
  use("nvim-treesitter/nvim-treesitter-context")
  -- Completitions
  use("hrsh7th/nvim-cmp")
  use("hrsh7th/cmp-nvim-lsp")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/cmp-path")
  use("hrsh7th/cmp-cmdline")
  use("saadparwaiz1/cmp_luasnip")
  -- Telescope
  use({
    "nvim-telescope/telescope.nvim",
    requires = { {"nvim-lua/plenary.nvim"} }
  })
  use("nvim-telescope/telescope-file-browser.nvim")
  use({
    "nvim-telescope/telescope-fzf-native.nvim",
    run = "make",
  })
  use("gbrlsnchs/telescope-lsp-handlers.nvim")
  use("nvim-telescope/telescope-live-grep-args.nvim")
  -- Neoclip
  use({
    "AckslD/nvim-neoclip.lua",
    requires = {
      {"kkharji/sqlite.lua", module = "sqlite"},
      {"nvim-telescope/telescope.nvim"},
    },
  })
  -- Snippets
  use("L3MON4D3/LuaSnip")
  use("rafamadriz/friendly-snippets")
  -- Comments
  use("terrortylor/nvim-comment")
  -- Close brackets
  use("rstacruz/vim-closer")
  -- Match start/end of logical blocks
  use("andymass/vim-matchup")
  -- Autopairs
  use("windwp/nvim-autopairs")
  -- Project
  use("ahmedkhalf/project.nvim")
  -- Icons
  use("kyazdani42/nvim-web-devicons")
  -- File tree
  use({
    "kyazdani42/nvim-tree.lua",
    requires = {
      "kyazdani42/nvim-web-devicons", -- optional, for file icons
    },
    tag = "nightly" -- optional, updated every week. (see issue #1193)
  })
  -- Tests
  use({
    "nvim-neotest/neotest",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "antoinemadec/FixCursorHold.nvim"
    }
  })
  use("nvim-neotest/neotest-python")
  -- Navigation
  use("numToStr/Navigator.nvim")
  -- JUST TO TEST!!!!!!!!!!!!!!!!!!!!!
  use({
    'sindrets/diffview.nvim',
    requires = 'nvim-lua/plenary.nvim'
  })
  use('ThePrimeagen/harpoon')
  use("jvgrootveld/telescope-zoxide")
  use('nvim-telescope/telescope-github.nvim')
  use("nvim-lualine/lualine.nvim")
  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end)
