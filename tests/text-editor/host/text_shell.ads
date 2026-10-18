with Text_Shells;
with Text_Host_Files; use Text_Host_Files;

--  Linux-hosted: the shell bound to the host stand-in (granted directories
--  and an in-process clipboard).
package Text_Shell is new Text_Shells (Host_Type => Text_Host_Files.Host_Files);
