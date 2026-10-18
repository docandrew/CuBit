with Text_Shells;
with Text_Host_Native; use Text_Host_Native;

--  On CuBit: the shell bound to the native host (static binding; the user
--  runtime has no tagged types).
package Text_Shell is new Text_Shells (Host_Type => Text_Host_Native.Host);
