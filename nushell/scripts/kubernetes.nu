def kgp [] {
    kubectl get pods -o json
    | from json
    | get items
    | each {|x| {
        namespace: $x.metadata.namespace,
        name: $x.metadata.name,
        status: $x.status.phase,
        restart: $x.status.containerStatuses.0.restartCount,
        age: ((date now) - ($x.status.startTime | into datetime))
      } }
}

def kgpa [] {
    kubectl get pods -A | from ssv
}

def kubectxes [] { kubectx | lines}
def kubenss [] { kubens | lines }

def kcc [ctx: string@kubectxes] {
    kubectx $ctx
}

def kn [ns: string@kubenss] {
    kubens $ns
}
