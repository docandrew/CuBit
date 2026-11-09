------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body CuBit.Last_Words is

   Registered : Reporter := null;

   procedure Register (Handler : Reporter) is
   begin
      if Registered = null then
         Registered := Handler;
      end if;
   end Register;

   procedure Report (Text : String; Line : Natural) is
   begin
      if Registered /= null then
         Registered.all (Text, Line);
      end if;
   end Report;

end CuBit.Last_Words;
