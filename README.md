# Buged, a minimal nano-like editor, currently mac-os only

![bugedDemo](https://github.com/user-attachments/assets/c822e318-4a59-48e7-a035-4648c2455574)

## Dependencies
* chez scheme

``` sh
brew install chezscheme
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
