del /S *.~*
del /S *.dcu
del /S *.rsm
del /S *.dsm
del /S *.map
del /S *.log

@echo off
for %%f in (*.exe) do (
    if not "%%f"=="dfu-util-static.exe" del "%%f"
)