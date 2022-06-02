def "nu-complete kube pods" [] {
    kubectl get pods | from ssv | get NAME
}

def "nu-complete kube deployments" [] {
    kubectl get deployments | from ssv | get NAME
}

def "nu-complete kube ctx" [] { kubectx | lines}

def "nu-complete kube ns" [] { kubens | lines }


##################

def kcc [ctx: string@"nu-complete kube ctx"] {
    kubectx $ctx
}

def kn [ns: string@"nu-complete kube ns"] {
    kubens $ns
}

def kgpo [] {
    kubectl get pods -o json
    | from json
    | get items
    | each {|x|
        let rs = $x.status.containerStatuses.0.restartCount
        {
            namespace: $x.metadata.namespace,
            name: $x.metadata.name,
            status: $x.status.phase,
            restarts: ($rs | split row ' '| get 0 | into int),
            age: ((date now) - ($x.status.startTime | into datetime))
        }}
}

def kgp [] {
    kubectl get pods -o wide | from ssv
    | rename name ready status restarts age ip node
    | each {|x| ($x| update restarts ($x.restarts|split row ' '| get 0 | into int)) }
}

def kgpa [] {
    kubectl get pods -o wide -A | from ssv
    | rename ns name ready status restarts age ip node nominated-node readiness-gates
    | each {|x| ($x| update restarts ($x.restarts|split row ' '| get 0 | into int)) }
}

def kgno [] {
    kubectl get nodes | from ssv | rename name status roles age version
}

def ked [pod: string@"nu-complete kube deployments"] {
    kubectl edit deployments $pod
}

def kep [pod: string@"nu-complete kube pods"] {
    kubectl edit pod $pod
}

def kdp [pod: string@"nu-complete kube pods"] {
    kubectl describe pod $pod
}

def ka [pod: string@"nu-complete kube pods"] {
    kubectl exec -it $pod -- bash
}

def kl [pod: string@"nu-complete kube pods"] {
    kubectl logs $pod
}

def klf [pod: string@"nu-complete kube pods"] {
    kubectl logs -f $pod
}

def kaf [file: path] {
    kubectl apply -f $file
}

def kak [file: path] {
    kubectl apply -k $file
}

def kk [file: path] {
    kubectl kustomize
}
