local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'
--local x = vim.cmd("fnamemodify(resolve(expand('<sfile>:p')), ':h')")

if fn.empty(fn.glob(install_path)) > 0 then
	execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
end
execute 'packadd packer.nvim'

local D = os.getenv('HOME')..'/nvim-lua'
local o = vim.o

-- This is a magic line that will take your pain away.
o.rtp = string.format('%s,%s', D, o.rtp)

-- Vanilla Config
require 'plugins'
require 'settings'
require 'autocmd'
require 'keybinds'

-- Plugins Config
require 'plugins/_theme'
require 'plugins/_colorizer'
require 'plugins/_fzf'
require 'plugins/_statusline'
require 'plugins/_easymotion'
require 'plugins/_indentline'
require 'plugins/_nerdtree'
require 'plugins/_tmux-navigator'
require 'plugins/_git-messenger'
require 'plugins/_term'
require 'plugins/_coc'

-- require "plugins/_tree"
-- require "plugins/_devicons"
-- require "plugins/_goyo"

