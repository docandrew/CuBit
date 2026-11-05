------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Settings.Transport;

package body CuBit.Settings.Family is
   package T renames CuBit.Settings.Transport;

   function Read (Member : String) return Reading is
      Stored : Text;
      Result : T.Status;
      Item : Value;
      Decoded : Boolean;
   begin
      if Member'Length = 0 then
         return (Kind => Absent);
      end if;
      T.Get (Service, Prefix & Member, Stored, Result);
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

   procedure For_Each (Result : out Outcome) is
      Names : Text;
      Count : Natural;
      Status : T.Status;
      First : Positive := 1;
   begin
      T.List (Service, Prefix, Names, Count, Status);
      case Status is
         when T.OK        => Result := Present;
         when T.Not_Found => Result := Absent; return;
         when T.Refused   => Result := Denied; return;
         when T.Failed    => Result := Unavailable; return;
      end case;
      for I in 1 .. Names.Length loop
         if Names.Data (I) = ASCII.NUL then
            declare
               Key : constant String := Names.Data (First .. I - 1);
            begin
               if Key'Length > Prefix'Length
                 and then Key (Key'First .. Key'First + Prefix'Length - 1) = Prefix
               then
                  Visit (Key (Key'First + Prefix'Length .. Key'Last));
               end if;
            end;
            First := I + 1;
         end if;
      end loop;
   end For_Each;

end CuBit.Settings.Family;
