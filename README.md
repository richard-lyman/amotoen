
== Install Go ==
    (Make sure you install the same platform of Go as you do of MinGW (32-bit or 64-bit))
    install go 32 bit (http://code.google.com/p/go/downloads/list?q=OpSys-Windows+Type%3DInstaller)
        The default should create a dir c:\Go - this is the <GO_INSTALL_DIR>

== Install Go and MinGW (this gets us 'gcc', which we use for sqlite and iup through cgo) ==
    (Make sure you install the same platform of Go as you do of MinGW (32-bit or 64-bit))
    install mingw (32 bit is default) (http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/)
        This is a GUI installer - it's super simple
        MAKE SURE YOU SELECT TO INSTALL MSYS - whatever else you do, at _least_ also do this
        The default should create a dir c:\MinGW - this is the <MINGW_INSTALL_DIR>
        You'll need to add the <MINGW_INSTALL_DIR>/bin folder to the system path
            Press the keys Win+Pause
            Click on 'Advanced system settings'
            Click on 'Environment Variables'
            Scroll down in the 'System variables' until you see the 'Path' variable
            Select the 'Path' variable row
            Click 'Edit'
            Press the Home key
            Type 'C:\MinGW\bin;'
            Click Ok
            Click Ok
            Click Ok
            Close the 'Control Panel Home' window

== Cleaning up after Go ==
    Sometimes libraries aren't 'cleaned' if the related library source is updated.
    C:\Go\pkg\windows_386\some\libraries\folder - and delete the *.a file
