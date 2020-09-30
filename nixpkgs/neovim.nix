{ pkgs, config, ... }:

{
  programs.neovim = {
    enable = true;
    vimAlias = true;
    extraConfig =
      ''
        vnoremap <C-c> "+y
        map <C-p> "+P
        let mapleader = "\\"

        nnoremap <silent> <C-L> :nohlsearch<CR><C-L>

        let g:go_def_mapping_enabled = 0
        let g:go_fmt_command = 'goimports'
        let g:go_highlight_types = 1
        let g:go_highlight_fields = 1
        let g:go_highlight_functions = 1
        let g:go_highlight_function_calls = 1
        let g:go_highlight_extra_types = 1

        autocmd FileType go nmap <leader>t <Plug>(go-test-func)
        autocmd FileType go nmap <leader>T <Plug>(go-test)
        nnoremap <leader>q :cclose<CR>

        let g:ale_linters = {
        \   'go': ['golangci-lint --disable= wsl, deadcode'],
        \   'tex': ['chktex'],
        \   'proto': ['buf-check-lint',],
        \   'sql': ['sqlint',],
        \   'python': ['flake8', 'pylint'],
        \   'vue': ['eslint'],
        \   'javascript': ['eslint'],
        \}
        let g:ale_fixers = {
        \   'sql': ['pgformatter',],
        \   'python': ['black','reorder-python-imports'],
        \   'javascript': ['prettier'],
        \   'vue': ['prettier'],
        \   'json': ['jq'],
        \   'nix': ['nixpkgs-fmt'],
        \}

        let g:ale_set_loclist = 0
        let g:ale_set_quickfix = 1
        let g:ale_fix_on_save = 1

        nmap <silent> <leader>k <Plug>(ale_previous_wrap)
        nmap <silent> <leader>j <Plug>(ale_next_wrap)

        nmap <silent><leader>s :FZF<cr>

        if executable('ag')
        let g:ackprg = 'ag --vimgrep'
        endif

        set hidden
        set cmdheight=2
        set updatetime=300
        set shortmess+=c

        inoremap <silent><expr> <TAB>
        \ pumvisible() ? coc#_select_confirm() :
        \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',\"\"])\<CR>":
        \ <SID>check_back_space() ? "\<TAB>" :
        \ coc#refresh()

        function! s:check_back_space() abort
        let col = col('.') - 1
        return !col || getline('.')[col - 1]  =~# '\s'
        endfunction

        let g:coc_snippet_next = '<tab>'
        nmap <silent> gd <Plug>(coc-definition)
        nmap <silent> gy <Plug>(coc-type-definition)
        nmap <silent> gi <Plug>(coc-implementation)
        nmap <silent> gr <Plug>(coc-references)

        nnoremap <silent> K :call <SID>show_documentation()<CR>

        function! s:show_documentation()
        if &filetype == 'vim'
        execute 'h '.expand('<cword>')
        else
        call CocAction('doHover')
        endif
        endfunction 
        nmap <leader>rn <Plug>(coc-rename)


        let g:ClangFormatAutoEnable=1

        colorscheme gruvbox
        set bg=dark
        let g:gruvbox_contrast_dark = 1 

        let g:airline#extensions#coc#enabled = 1
        let g:airline_theme='gruvbox'

        set nocompatible
        filetype plugin on
        filetype indent on
        syntax on
        set hlsearch 

        set number relativenumber

        augroup numbertoggle
        autocmd!
        autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
        autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
        augroup END

        highlight Comment cterm=italic gui=italic

        let &t_SI = "\e[4 q"
        let &t_EI = "\e[2 q"
        set termguicolors

        let g:tex_flavor = 'latex'

        autocmd FileType json,jsonnet setlocal nowritebackup noswapfile nobackup

        let g:vim_markdown_folding_disabled = 1
        let vim_markdown_preview_github=1

        nnoremap <silent> <S-Left> :vertical resize +3<CR>
        nnoremap <silent> <S-Right> :vertical resize -3<CR>
        nnoremap <silent> <S-Up> :resize +3<CR>
        nnoremap <silent> <S-Down> :resize -3<CR>
      '';
    plugins = with pkgs.vimPlugins; [
      vim-nix
      ack-vim
      gruvbox
      vim-airline
      vim-airline-themes
      vim-go
      vim-fugitive
      vim-polyglot
      fzf-vim
      fzfWrapper
      ale
      coc-nvim
      coc-go
      coc-json
      coc-html
      coc-yaml
      coc-python
    ];
  };
}
