home / "~" -> [
     ffi def
     ext
]
hardware / "~/hardware" -> [
	 0
	 "hardware"
	 ffi tmux
	 ext
]
sources / "~/sources" -> [
	ffi def
	ext
]
sanoma / "~/sources/sanoma" -> [
       ffi def
       ext
]
sanoma|dcp-core / "~/sources/sanoma/dcp-core" -> [
		ffi def
		ext
]
sanoma|dcp-content-hub / "~/sources/sanoma/dcp-content-hub" -> [
		       ffi def
		       ext
]
sanoma|dcp-analyis / "~/sources/sanoma/dcp-analysis" -> [
		   ffi def
		   ext
]
zsh-scripts / "~/scripts/" -> [
	      ffi def
	      ext
]
conf / "~/sources/vim-zsh-vimperator-xmonad-configuration" -> [
     ffi def
     ext
]
xmonad / "~/.xmonad" -> [
       ffi def
       ext
]
books / "~/Dropbox/Sanoma Shared Stuff/Boeken" -> [
      "recoll"
       ffi spawn
       "books"
       ffi shell
       ext
]
mathematics / "~/Dropbox/Sanoma Shared Stuff/Boeken/Mathematics" -> [
      "recoll"
       spawn
       cal
      "xmonad"
       shell
       cal
       ext
]
browser / "~" -> [
	"firefox"
	spawn
	cal
	ext
]
chat / "~" -> [
     "jitsy"
     spawn
     cal
     ext
]
