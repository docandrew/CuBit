pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with CuBit.Process_IDs;
with CuBit.Control_Events;
with Channel_Wake_ABI;
with I2C_Event_Policy;
procedure Event_Policy_Tests is
   package E renames I2C_Event_Policy;
   package P renames CuBit.Process_IDs;
   package C renames CuBit.Control_Events;
   use type E.Action;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_64;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Natural'Image (Checks); end if;
   end Check;
   type Case_Data is record
      Label : Interfaces.Unsigned_32;
      Length : Interfaces.Unsigned_8;
      W0 : Interfaces.Unsigned_64;
      Expected : E.Action;
   end record;
   Cases : constant array (Positive range <>) of Case_Data :=
     [(C.Control_Label, 2, 1, E.Stop_After_Drain),
      (C.Control_Label, 2, 2, E.Ignore),
      (C.Control_Label, 2, 3, E.Ignore),
      (C.Control_Label, 2, 0, E.Ignore),
      (C.Control_Label, 2, Interfaces.Unsigned_64'Last, E.Ignore),
      (C.Grant_Revoked_Label, 3, 0, E.Disable_Logging),
      (C.Grant_Returned_Label, 3, 0, E.Disable_Logging),
      (Channel_Wake_ABI.Wake_Label, 1, 0, E.Route_Channel_Wake),
      (Channel_Wake_ABI.Wake_Label, 2, 0, E.Route_Channel_Wake),
      (Channel_Wake_ABI.Wake_Label, 3, 0, E.Route_Channel_Wake),
      (Channel_Wake_ABI.Wake_Label, 4, 0, E.Route_Channel_Wake),
      (0, 0, 0, E.Ignore),
      (Interfaces.Unsigned_32'Last, 4, 1, E.Ignore)];
   type Publishers is array (Positive range <>) of Interfaces.Unsigned_64;
   Peers : constant Publishers := [1, 2 ** 32, 16#FFFF_1111_0000_1234#,
                                    Interfaces.Unsigned_64'Last];
begin
   for Item of Cases loop
      Check (E.Classify (P.No_Process, Item.Label, Item.Length, Item.W0, 0, 0) = Item.Expected);
      for Peer of Peers loop
         Check (E.Classify (P.From_Word (Peer), Item.Label, Item.Length, Item.W0, 0, 0) = E.Ignore);
      end loop;
   end loop;
   for Length in Interfaces.Unsigned_8 loop
      if Length /= 2 then
         Check (E.Classify (P.No_Process, C.Control_Label, Length, 1, 0, 0) = E.Ignore);
      end if;
      if Length /= 3 then
         Check (E.Classify (P.No_Process, C.Grant_Revoked_Label, Length, 0, 0, 0) = E.Ignore);
         Check (E.Classify (P.No_Process, C.Grant_Returned_Label, Length, 0, 0, 0) = E.Ignore);
      end if;
      if Length not in 1 .. 4 then
         Check (E.Classify (P.No_Process, Channel_Wake_ABI.Wake_Label, Length, 0, 0, 0) = E.Ignore);
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("I2C event policy checks" & Natural'Image (Checks));
end Event_Policy_Tests;
