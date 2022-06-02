def _kube_pods [] {
    kubectl get pods | from ssv | get NAME
}

def _kube_deployments [] {
    kubectl get deployments | from ssv | get NAME
}

def _kube_ctx [] { kubectx | lines}

def _kube_ns [] { kubens | lines }


##################

def kcc [ctx: string@_kube_ctx] {
    kubectx $ctx
}

def kn [ns: string@_kube_ns] {
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

def ked [pod: string@_kube_deployments] {
    kubectl edit deployments $pod
}

def kep [pod: string@_kube_pods] {
    kubectl edit pod $pod
}

def kdp [pod: string@_kube_pods] {
    kubectl describe pod $pod
}