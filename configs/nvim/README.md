My current configuration is based on next examples:

- https://www.lazyvim.org/installation            -- LazyVim is a Neovim setup powered by 💤 lazy.nvim to make it easy to customize and extend your config.

I do use almost anything from this configuration right now. One thing that is nice-to-have is [the started configuration example](git clone https://github.com/LazyVim/starter ~/.config/nvim).

- https://github.com/sadiksaifi/nvim              -- IDE for Web Development using Neovim

I do not use the same sctructure, eg. I do not have /core dir (/config instead).
There are some plugins I think I want to take away.
There is one thing that looks custom comparing with other Neovim configuration. 
Inside /lua dir I have `lazy-setup.lua` file which also a necessity to resolve loader loop for lazy.nvim plugin.

-- https://github.com/ecosse3/nvim/tree/master    -- A non-minimal Neovim config built to work most efficiently with Frontend Development

There are a ton of things I do not want to see in my configration, but... I need to find out the set of plugins that can really boost web frontend productivity.

- https://github.com/bluz71/vim-nightfly-colors   -- A dark midnight theme for modern Neovim & classic Vim.
- https://github.com/josean-dev/dev-environment-files/blob/1bcf8bfd532c1fe549798a0f4a3ab351970de3d3/.config/nvim/lua/josean/plugins/colorscheme.lua -- The real example of nightfly colorscheme usage.

I am using this theme nowadays. 
Unfortunately I had a personal preference to use the standard Terminal.app on macOS, which [does not support](https://stackoverflow.com/a/73290225/3527499) true colors.
Check out the real example link I had attached up above.
