pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with System;
with System.Storage_Elements;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with DW_I2C_Transfer;
with DW_I2C_Request_Types;
with I2C_Grant_Copy;
procedure Grant_Copy_Tests is
   package T renames DW_I2C_Transfer;
   package Q renames DW_I2C_Request_Types;
   package P renames CuBit.Process_IDs;
   package G renames CuBit.Grant_References;
   use type T.Count;
   use type T.Byte;
   use type Q.Copy_Result;
   use type P.Process_ID;
   use type G.Reference;
   use type Interfaces.Unsigned_64;
   type Bytes is array (T.Count range <>) of T.Byte;
   Shared : aliased Bytes (0 .. T.Maximum_Transfer_Bytes) := [others => 16#CC#];
   Owner : constant P.Process_ID := P.From_Word (16#FFFF_1234_5678_9999#);
   Reference : constant G.Reference := (G.Maximum_Slot, G.Generation'Last);
   Acquires, Releases, Checks : Natural := 0;
   Length : Natural := 0;
   Direction : Boolean := False;
   Allow_Acquire, Allow_Return : Boolean := True;
   Address_Mode : Natural := 0;
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   procedure Acquire
     (Ref : G.Reference; Caller : P.Process_ID; Count : Interfaces.Unsigned_64;
      Writable : Boolean; Address : out System.Address; Acquired : out Boolean) is
   begin
      Check (Ref = Reference and Caller = Owner, "exact opaque owner and maximum generation reference");
      Check (Count = Interfaces.Unsigned_64 (Length) and Writable = Direction, "exact length/access");
      Acquires := Acquires + 1; Acquired := Allow_Acquire;
      Address := (case Address_Mode is
        when 0 => Shared (1)'Address, when 1 => System.Null_Address,
        when others => System.Storage_Elements.To_Address
          (System.Storage_Elements.Integer_Address'Last));
   end Acquire;
   procedure Release (Ref : G.Reference; Returned : out Boolean) is
   begin
      Check (Ref = Reference and Acquires = 1, "release exact acquired generation once");
      Releases := Releases + 1; Returned := Allow_Return;
   end Release;
   package Copy is new I2C_Grant_Copy (Bytes, Acquire, Release);
   procedure Case_Run (Size : T.Count; Write : Boolean; Which : Natural) is
      Result : Q.Copy_Result;
      Data : Bytes (T.Count'Last - Size .. T.Count'Last - 1);
   begin
      Length := Natural (Size); Direction := Write; Acquires := 0; Releases := 0;
      Allow_Acquire := Which /= 1; Allow_Return := Which not in 2 | 4;
      Address_Mode := (if Which in 3 | 4 then 1 elsif Which = 5 then 2 else 0);
      Shared := [others => 16#CC#]; Data := [others => 16#55#];
      if Write then Copy.Write_Output (Owner, Reference, Data, Result);
      else Copy.Read_Input (Owner, Reference, Data, Result);
      end if;
      if Size = 0 then
         Check (Result = Q.Copied and Acquires = 0 and Releases = 0, "empty copy has no grant operations");
      else
         Check (Acquires = 1 and Releases = (if Which = 1 then 0 else 1), "one return for every successful acquisition");
         Check (Result = (case Which is
           when 0 => Q.Copied, when 2 | 4 => Q.Uncertain, when others => Q.Refused), "typed copy/release result");
         if Write then
            Check ((for all I in Shared'Range => Shared (I) =
              (if I in 1 .. Size and Which in 0 | 2 then 16#55# else 16#CC#)),
              "only requested output bytes change; uncertain release does not roll back");
         else
            Check ((for all B of Shared => B = 16#CC#), "input copy never writes shared bytes");
            Check ((for all B of Data => B = (if Which = 0 then 16#CC# else 0)),
              "failed input release never exposes copied bytes");
         end if;
      end if;
   end Case_Run;
begin
   for Write in Boolean loop
      for Which in 0 .. 5 loop
         Case_Run (0, Write, Which);
         Case_Run (2, Write, Which);
         Case_Run (16, Write, Which);
         Case_Run (T.Count'Last, Write, Which);
      end loop;
   end loop;
   -- One-byte copy never uses an invalid max-address fixture (no wrap).
   Case_Run (1, False, 0); Case_Run (1, True, 0);
   declare
      Oversized : Bytes (T.Count'Range) := [others => 16#55#];
      Result : Q.Copy_Result;
   begin
      Acquires := 0; Releases := 0;
      Copy.Read_Input (Owner, Reference, Oversized, Result);
      Check (Result = Q.Refused and Acquires = 0 and Releases = 0 and
        (for all B of Oversized => B = 0), "oversized input refused before acquisition/conversion");
      Copy.Write_Output (Owner, Reference, Oversized, Result);
      Check (Result = Q.Refused and Acquires = 0 and Releases = 0,
        "oversized output refused before acquisition/conversion");
   end;
   Ada.Text_IO.Put_Line ("I2C grant copy checks" & Checks'Image);
end Grant_Copy_Tests;
