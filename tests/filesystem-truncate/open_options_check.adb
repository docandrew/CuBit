with Ada.Text_IO;
with CuBit.Filesystems; use CuBit.Filesystems;

procedure Open_Options_Check is
   Expected : Boolean;
   Access_Mode : Open_Options;
begin
   for Options in Open_Options range 0 .. 8191 loop
      Access_Mode := Options and 3;
      Expected :=
        (Options and not (3 or 64 or 512 or 1024 or 2048)) = 0 and then
        Access_Mode /= 3 and then
        ((Options and 1024) = 0 or else
           ((Options and 64) /= 0 and then (Options and 512) = 0)) and then
        ((Options and (512 or 2048)) = 0 or else Access_Mode in 1 | 2);
      pragma Assert (Valid_Open_Options (Options) = Expected);
   end loop;
   pragma Assert (Valid_Open_Options (OPEN_READ_WRITE or OPEN_DENY_SHARING));
   pragma Assert (Valid_Open_Options
     (OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE or OPEN_DENY_SHARING));
   pragma Assert (not Valid_Open_Options (OPEN_READ_ONLY or OPEN_DENY_SHARING));
   pragma Assert (not Valid_Open_Options (OPEN_READ_WRITE or OPEN_EXCLUSIVE));
   Ada.Text_IO.Put_Line ("OPEN-OPTIONS-CHECK: PASS 8192 encodings");
end Open_Options_Check;
