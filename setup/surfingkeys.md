自定义搜索引擎
```javascript
addSearchAliasX('u', '谷歌', 'https://www.google.ca/search?q=', 's', 'https://www.google.ca/complete/search?client=chrome-omni&gs_ri=chrome-ext&oit=1&cp=1&pgcl=7&q=', function(response) {
    var res = JSON.parse(response.text);
    return res[1];
});
settings.defaultSearchEngine = 'u';
```

当打开标签页的数量超过设定值时，使用搜索栏来查找标签页。
```
settings.tabsThreshold = 80;
```

是否在搜索栏下面自动选择第一个匹配的结果。
```
settings.focusFirstCandidate = true;
```

搜索栏下面每页显示多少条结果。
```
settings.omnibarMaxResults = 15;
```


```
mapkey('<Ctrl-t>', 'Choose a tab with omnibar', function() {
	    Front.openOmnibar({type: "Tabs"});
});
```
