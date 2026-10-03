with Ada.Unchecked_Conversion;
with System;
with CuBit.Memory_Grants;

package body Desktop_Input_Transfer with SPARK_Mode => Off is
   package MG renames CuBit.Memory_Grants;
   use type W.Word;
   function To_Word is new Ada.Unchecked_Conversion (System.Address, W.Word);
   function To_Address is new Ada.Unchecked_Conversion (W.Word, System.Address);

   procedure Acquire
     (Owner : W.Identity; Grant : GR.Reference;
      Mapping : out W.Word; Acquired : out Boolean)
   is
      Address : System.Address;
   begin
      MG.Acquire (Grant, Owner, 0, W.Byte_Count, MG.Write_Access, Address, Acquired);
      Mapping := To_Word (Address);
   end Acquire;

   procedure Write
     (Mapping : W.Word; Payload : W.Snapshot_Words; Written : out Boolean)
   is
   begin
      Written := False;
      if Mapping = 0 or else Mapping mod 8 /= 0 or else
        Mapping > W.Word'Last - W.Byte_Count
      then
         return;
      end if;
      declare
         Target : W.Snapshot_Words
           with Import, Address => To_Address (Mapping), Volatile;
      begin
         -- Fixed 320-byte metadata copy. The receiver reads only after its
         -- synchronous reply, never by racing these individual stores.
         Target := Payload;
      end;
      Written := True;
   end Write;

   procedure Return_Loan (Grant : GR.Reference; Confirmed : out Boolean) is
   begin
      MG.Return_Acquisition (Grant, Confirmed);
   end Return_Loan;
end Desktop_Input_Transfer;
