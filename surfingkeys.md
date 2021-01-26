自定义搜索引擎
```javascript
addSearchAliasX('u', '谷歌', 'https://www.google.ca/search?q=', 's', 'https://www.google.ca/complete/search?client=chrome-omni&gs_ri=chrome-ext&oit=1&cp=1&pgcl=7&q=', function(response) {
    var res = JSON.parse(response.text);
    return res[1];
});
settings.defaultSearchEngine = 'u';
```
