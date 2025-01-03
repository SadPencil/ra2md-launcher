# RA2MD Launcher

This program tries to determine whether the game Yuri's Revenge is installed from Steam, Origin, retail CD, or First Decade CD.

For the Steam installation, it will launch the game via `gamemd.exe`. Otherwise, it will launch the game via `ra2md.exe`. This is because the legit `gamemd.exe` (except the Steam one) has a check that requires players to launch the game via `ra2md.exe` or `yuri.exe` first.

## Approach to determine the game installation

This program will check the following:
- The hash value of known `gamemd.exe` files from Steam, Origin, retail CD, and First Decade CD.
- If the `gamemd.exe` file is not regonized by hash, the program will check the file size. 

Note that determining by the file size is not a robust way. The difference of file size is primarily due to introducing a large icon in the Steam installation. Hence, if someone is replacing the icon for the retail version of `gamemd.exe`, I suggest also [remove the need of Yuri.exe so you can launch from Gamemd.exe](http://www.ppmsite.com/forum/viewtopic.php?t=38645) using [C&C Executable Modifier](https://ppmforums.com/topic-37926/cc-executable-modifier/).