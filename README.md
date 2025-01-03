# RA2MD Launcher

This program tries to determine whether the game Yuri's Revenge is installed from Steam, Origin, retail CD, or First Decade CD.

For the Steam installation, it will launch the game via `gamemd.exe`. Otherwise, it will launch the game via `ra2md.exe`. This is because the legit `gamemd.exe` (except the Steam one) has a check that requires players to launch the game via `ra2md.exe` first.