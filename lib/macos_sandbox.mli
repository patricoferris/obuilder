(* SANDBOXING ON MACOS
   
   Sandboxing on MacOS is achieved in two ways: 
     - For security: sandbox-exec is used to control access to various OS things
     - For easier isolation: OS users are the "units" of isolation in terms of "builders"

    --- Homebrew Hackery ---
   Homebrew nees to believe it is installed in /usr/local otherwise say goodbye to your bottles 
   and everything installs from source... To do this, we use a FUSE filesystem mounted to /usr/local 
   to redirect calls to the user-who-is-calling's home directory. 

   Unfortunately (for us) Homebrew uses file-locking which isn't implemented for OSXFuse so 
   parallel builds will think other homebrew processes are active which they are but FUSE is handling 
   the redirection so it should be okay! For now, we are using this very slighlty modified homebrew. 
    
    Homebrew: https://github.com/patricoferris/brew 
    Installed by: https://github.com/patricoferris/install/tree/disable-locking
    
    --- Fuse Hackery ---
   In addition to this, the scheme for using OBuilder with MacOS requires the following: 
     (1) Fuse Filesystem that can do some clever redirection
     (2) SIP disabled in order to mount to /usr/local 

   Each user's home directory is set to the file-system snapshot of the next build step in the 
   <name>.spec file. This way we get caching for free and since no two identically hashed builds
   can occur at the same time, we won't trample on each other!

   For the mapping to work, the FUSE daemon must be able to work out where to send user-specific 
   commands. Originally this would have been /User/<prefix><uid> but since we are now using the 
   hash (e.g. /zfs/<hash>) we need some mechanism of informing FUSE that user <uid> is currently 
   using /zfs/<hash> as their home directory. To do this we use /data/scoreboard where every build
   step updates a symlink there for ./<uid> to /zfs/<hash>. The Filesystem can then look up dynamically
   the mapping for each call and redirect to the right snapshot. 

    --- System Compilers ---
   Building a compiler within a user's directory (or anywhere) with opam unfortunately makes it 
   completely un-relocatable which is a problem for restoring from snap shots. So instead this 
   scheme requires that system compilers be installed somewhere accessible by everyone e.g. 
   /data/compiler/<version>. 

    --- Sharing Files ---
   In order for different users (uids) to share snapshots enabling caching to work, all files have umask
   set to g+w. 

    --- Important Env Variables ---
   The follow environment variables need to be set for the users: 
     - HOMEBREW_DISABLE_LOCKING=1
     - HOMBREW_NO_AUTO_UPDATE=1       (maybe not?)
     - TMPDIR=~/tmp                   (I think opam needs this?)
     - HOME=/zfs/<hash> 

    --- Various Gotchas ---
   (1) If you install zfs & zpool into /usr/local then obviously once FUSE is mounted they're gone so you need 
       to move these outside of that directory. This is easy enough for the binaries but you also need to move 
       the dynamically loaded libraries too -- these are: libnvpair, libuutil, libzpool and libzfs(_core). If 
       you move them to /data/lib then be sure to set DYLD_FALLBACK_LIBRARY_PATH=/data/lib (maybe even in the 
       calls to zfs for MacOS i.e. `DYLD...=/data/lib zfs create...`)
   (2) We only support relocatable packages, sorry.
*)
include S.SANDBOX

val create : 
  uid:int -> 
  fallback_library_path:string -> 
  scoreboard:string ->
  t 
(** [create ~uid ~fallback_library_path ~scoreboard] generates a new 
    Macos configuration which will build inside a user "mac<uid>" and the dynamic 
    libraries for zfs/zpool must also be in [fallback_library_path] as usually 
    they are in /usr/local which won't be available (see note above). The [scoreboard]
    will be the location symlinks are created letting you know the home directory 
    of different users.
*)