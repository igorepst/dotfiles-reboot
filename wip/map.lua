map('n', '[b', ':<C-U>exe v:count1 . "bprevious!"<CR>', {expr = true})
map('n', ']b', ':<C-U>exe v:count1 . "bnext!"<CR>', {expr = true})
map('n', '[c', ':<C-U>exe v:count1 . "cprevious!"<CR>', {expr = true})
map('n', ']c', ':<C-U>exe v:count1 . "cnext!"<CR>', {expr = true})

