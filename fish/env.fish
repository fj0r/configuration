set -x CFG $HOME/pub/Configuration
set -x WHEEL $HOME/wheel

if test -x $WHEEL/VSCode-linux-x64/bin/code
    set -x PATH $WHEEL/VSCode-linux-x64/bin         $PATH
end

if test (uname) = Darwin; and test -d /Library/PostgreSQL/10/bin
    set -x PATH /Library/PostgreSQL/10/bin          $PATH
end

if test (uname) = Linux; and test -d $WHEEL/node
    set -x JS_HOME      $WHEEL/node
    set -x PATH         $JS_HOME/bin                $PATH
end
if test -d $WHEEL/__npm__
    set -x JS_MODULE $WHEEL/__npm__
    set -x PATH      $JS_MODULE/bin                 $PATH
    set -x NODE_PATH $JS_MODULE/lib/node_modules
end

if test -d $WHEEL/stack
    set -x STACK_HOME $WHEEL/stack
    set -x PATH $STACK_HOME                         $PATH
    set -x PATH $HOME/.local/bin                    $PATH
end

if test -d $HOME/.cargo
    set -x RUST_HOME     $HOME/.cargo
    set -x PATH          $RUST_HOME/bin               $PATH
    set -x RUST_SRC_PATH $WHEEL/rustc/src
end

switch (uname)
    case Linux
        if test -d $WHEEL/jdk
            set -x JAVA_HOME $WHEEL/jdk
        end
    case Darwin
        set -x JAVA_HOME (/usr/libexec/java_home -v 1.8)
end
if test $JAVA_HOME
    set -x PATH         $JAVA_HOME/bin              $PATH
    set -x CLASSPATH    . $JAVA_HOME/lib $JAVA_HOME/jre/lib
end

if test -d $HOME/anaconda3
    set -x PYTHON_HOME      $HOME/anaconda3
    set -x PATH             $PYTHON_HOME/bin                $PATH
end

if test -d /usr/local/bin
    set -U fish_user_paths /usr/local/bin $fish_user_paths
end
