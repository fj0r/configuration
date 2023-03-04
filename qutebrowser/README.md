## Unresolved
- [ ] RSS
- [ ] hover translate
    - https://github.com/potamides/yomichad
- [ ] Pocket
- [ ] SingleFile
- [ ] hints pointer cursor
  - [hints don't show on some pages](https://github.com/qutebrowser/qutebrowser/issues/178)
  - inline style: c.hints.selectors['links'] += ['[style*="cursor: pointer"]']
  - surfingkeys: `getComputedStyle(e).cursor === "pointer"`
  - argo: `'.icon'`

### [ ] hints pointer cursor
#### surfingkeys hints
```js
    var cssSelector = "a, button, select, input, textarea, summary, *[onclick], *[contenteditable=true], *.jfk-button, *.goog-flat-menu-button, *[role=button], *[role=link], *[role=menuitem], *[role=option], *[role=switch], *[role=tab], *[role=checkbox], *[role=combobox], *[role=menuitemcheckbox], *[role=menuitemradio]";
    if (runtime.conf.clickableSelector.length) {
        cssSelector += ", " + runtime.conf.clickableSelector;
    }

    return e.matches(cssSelector)
        || getComputedStyle(e).cursor === "pointer"
        || getComputedStyle(e).cursor.substr(0, 4) === "url("
        || e.closest("a, *[onclick], *[contenteditable=true], *.jfk-button, *.goog-flat-menu-button") !== null;
```


## Writing qutebrowser userscripts

https://qutebrowser.org/doc/userscripts.html

### qutescript
```sh
git clone https://github.com/hiway/python-qutescript.git qutescript
cd qutescript
pip install -e .
```
## userscripts list

https://github.com/qutebrowser/qutebrowser/blob/master/misc/userscripts/README.md

### pdfjs
```sh
sudo apt install libjs-pdf
```

### bitwarden

https://github.com/qutebrowser/qutebrowser/blob/master/misc/userscripts/qute-bitwarden

```sh
sudo apt install keyutils rofi
pip3 install tldextract
npm install -g @bitwarden/cli
bw login
```

### code_select.py

https://github.com/LaurenceWarne/qute-code-hint

```sh
pip3 install pyperclip --user
# wayland
pip3 install pyclip
```
### tab manager

https://codeberg.org/mister_monster/tab-manager


