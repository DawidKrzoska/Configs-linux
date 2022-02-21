filetype on
filetype plugin on
filetype indent on
syntax on
set number
set shiftwidth=4
set tabstop=4
set expandtab
set nobackup
set nowrap
set incsearch
set smartcase
set showmode
set showmatch
set hlsearch
set history=100

set wildmenu
set wildmode=list:longest
set wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx

set foldmethod=marker

" PLUGINS ---------------------------------------------------------------- {{{

call plug#begin()
    Plug 'dense-analysis/ale'
    Plug 'nvim-treesitter/nvim-treesitter'
    Plug 'nvim-orgmode/orgmode'
    Plug 'glepnir/zephyr-nvim'
    Plug 'nvim-treesitter/nvim-treesitter'
    Plug 'akinsho/toggleterm.nvim'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'romgrk/barbar.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tjdevries/express_line.nvim'
call plug#end()

" }}}
colorscheme zephyr
