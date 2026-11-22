------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Settings.Transport;

package body CuBit.Settings.Writable_Family is
   package T renames CuBit.Settings.Transport;

   procedure Write (Member : String; Item : Value; Result : out Change_Outcome) is
      Source : constant String := Encode (Item);
      Status : T.Status;
   begin
      if Source'Length > Maximum_Text then
         Result := Too_Long;
         return;
      end if;
      T.Set (Service, Prefix & Member, Source, Status);
      Result := T.Changed (Status);
   end Write;

   procedure Delete (Member : String; Result : out Change_Outcome) is
      Status : T.Status;
   begin
      T.Delete (Service, Prefix & Member, Status);
      Result := T.Changed (Status);
   end Delete;

end CuBit.Settings.Writable_Family;
