test_label / "/tmp/shit" -> [
	   main
	   cal
	   [
                {- s1 s2 s3 (s3 s2 s1) -}
		rt3 {- s2 s3 s1 (s2 s1 s3) -}
		swp {- s3 s2 s1 (s1 s2 s3) -}
		ret

	   ]
	   :mirror {- s1 s2 s3  -> {s3 s2 s1} -}
	   [
	     ffi print
	     mirror
	     cal
	     snz
	     ret
	     mirror
	     cal
	     ovr {- fib 2 fib 1 fib 2 fib n-}
	     add {- fib 3 fib 2 fib n -}
	     swp {- fib 2 fib 3 fib n-}
	     mirror
	     cal
	     dec
	     mirror
	     cal
	     fib
	     cal
	     ret
           ]
	   :fib {- fib 1 fib 2 fib n -}
	   [
	    20
	    1
	    1
	    fib
	    cal
	    ffi print
	    ext
	   ]
	   :main
]