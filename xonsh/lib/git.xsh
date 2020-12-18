from lib.utils import mk_alias

mk_alias({
    'gs': 'git status',
    'gc': 'git checkout',
    'gci': 'git commit',
    'gca': 'git commit -a',
    'gcaa': 'git commit -a --amend',
    'gn': 'git checkout -b',
    'gb': 'git branch',
    'gbd': 'git branch -D',
    'gpl': 'git pull',
    'gps': 'git push',
    'gl': 'git log',
    'glp': 'git log -p',
    'gly': 'git log --since=yesterday',
    'glt': 'git log --since=today',
    'glm': 'git log --since=midnight',
    'gm': 'git merge',
    'gr': 'git rebase -i --autosquash',
    'gd': 'git diff',
    'gdc': 'git diff --cached',
    'ga': 'git add .',
    'gut': 'git reset HEAD --',
    'grhh': 'git reset --hard HEAD',
    'glst': 'git log -1 HEAD',
    'gt': 'git tag',
    'gta': 'git tag -a',
    'gtd': 'git tag -d',
    'ggc': 'git reflog expire --all --expire=now and git gc --prune=now --aggressive'
})




#function gpsu {
#    local default='origin'
#    eval $__default_indirect_object
#    git push -u $y $z
#}
#
#function gtdr {
#    local default='origin'
#    eval $__default_indirect_object
#    git push $y :refs/tags/$z
#}
#alias ggc='git reflog expire --all --expire=now && git gc --prune=now --aggressive'
#function grad {
#    local default='origin'
#    eval $__default_indirect_object
#    git remote add $y $z
#}
#function gcf  { vim .git/config }