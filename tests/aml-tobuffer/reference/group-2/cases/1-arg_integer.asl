DefinitionBlock ("", "DSDT", 1, "CUBIT", "TOBTARG", 1) { Method (HELP,1) { ToBuffer ("AB", Arg0) Return (Arg0) } Method (CTRL,0) {Return (One)} Method (TEST,0) {Return (HELP (9))} }
