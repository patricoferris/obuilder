include S.SANDBOX

(** MacOS Sandboxing -- Experimental 
    
    Sandboxing on MacOS is done using users. Each build environment is a separate user 
    that comes with homebrew and opam installed along with opam-repository as in the 
    docker-based images.
    
    Importantly, the sandboxing relies on a FUSE filesystem being mounted at /usr/local 
    in order to intercept calls the homebrew and remap them to /Users/$USER/local, this allows 
    a normal homebrew to be installed with packages working correctly. The users are called mac<uid> 
    in order to be able to map them just by checking the fuse context. 

    All in all the main thing is that the command <cmd> is translated to sudo -u <user> -i <cmd> 
    which exactly what runmac run <user> <cmd> does under-the-hood. 
    
    Caching is a... TODO
*)