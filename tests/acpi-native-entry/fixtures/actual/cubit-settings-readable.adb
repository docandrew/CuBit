------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Settings.Transport;

package body CuBit.Settings.Readable is
   package T renames CuBit.Settings.Transport;

   function Read return Reading is
      Stored : Text;
      Result : T.Status;
      Item : Value;
      Decoded : Boolean;
   begin
      T.Get (Service, Key, Stored, Result);
      case Result is
         when T.Not_Found => return (Kind => Absent);
         when T.Refused   => return (Kind => Denied);
         when T.Failed    => return (Kind => Unavailable);
         when T.OK        => null;
      end case;
      Decode (Image (Stored), Item, Decoded);
      if not Decoded then
         return (Kind => Invalid);
      end if;
      return (Kind => Present, Item => Item);
   end Read;

end CuBit.Settings.Readable;
