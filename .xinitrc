LANG=ja_JP.UTF-8
export LANG

s uim-xim

startkde 2>&1\
  |{ type ts >/dev/null 2>&1 && ts '%Y-%m-%d %H:%M:%S' || cat; } \
  |tee -a "$HOME/.xinit-errors" \
  ;

