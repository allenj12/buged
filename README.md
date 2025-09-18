# Buged, a minimal nano-like editor, currently mac-os only
## Dependencies
* chez scheme
* ncurses

``` sh
brew install ncurses chez
```
## Install
``` sh 
./build.ss
```
``` sh
cp main.boot $(find /opt/homebrew -name petite.boot | sed 's/petite\.boot/buged.boot/')
```
``` sh
ln -s /opt/homebrew/bin/petite /opt/homebrew/bin/buged
```

## Update

``` sh
git pull
```
``` sh
./build.ss
```
``` sh
cp main.boot $(find /opt/homebrew -name buged.boot)
```