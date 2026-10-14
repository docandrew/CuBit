------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Libc_Startup - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Launch_Envelopes;

package body CuBit.Libc_Startup is

   package ST renames CuBit.Startup_Tables;
   package LE renames CuBit.Launch_Envelopes;
   use type ST.Capability_Kind;
   use type ST.Entry_State;
   use type ST.Decode_Result;

   --  Where the kernel maps a launch block (kernel Process_Launch).
   Block_Address : constant := 16#0000_5A00_0000_0000#;

   Current : ST.Table;
   pragma Suppress_Initialization (Current);
   Loaded : Boolean := False;

   procedure Load (Block_Length : Unsigned_64) is
      Payload : LE.Block_Length;
      First   : Positive;
      Count   : LE.Table_Count;
      Present : Boolean;
      Outcome : ST.Decode_Result;
   begin
      if Loaded or else Block_Length not in 1 .. LE.Maximum_Block_Bytes then
         return;
      end if;
      declare
         Mapped : constant LE.Block (1 .. Natural (Block_Length))
         with Import, Address => System'To_Address (Block_Address);
      begin
         LE.Split (Mapped, Payload, First, Count, Present);
         if not Present or else Count = 0 then
            return;
         end if;
         declare
            --  One copy (single fetch): decoded from it alone.
            Copy : constant ST.Bytes (1 .. Count) :=
              [for I in 1 .. Count => Mapped (First + I - 1)];
         begin
            ST.Decode (Copy, Current, Outcome);
            Loaded := Outcome = ST.Decoded and then ST.Complete (Current);
         end;
      end;
   end Load;

   function Entry_Count return Natural is (if Loaded then Natural (Current.Count) else 0);

   function Handle_At
     (I : Positive; Kind : CuBit.Startup_Tables.Capability_Kind) return Unsigned_64 is
     (if I <= Entry_Count
         and then Current.Entries (ST.Entry_Index (I)).Kind = Kind
         and then Current.Entries (ST.Entry_Index (I)).State = ST.Granted
      then Current.Entries (ST.Entry_Index (I)).Handle else 0);

   function Granted
     (Name : String; Kind : CuBit.Startup_Tables.Capability_Kind) return Unsigned_64
   is
      Index : ST.Entry_Count;
   begin
      if not Loaded or else not ST.Valid_Name_Text (Name) then
         return 0;
      end if;
      Index := ST.Find (Current, ST.To_Name (Name));
      if Index = 0 then
         return 0;
      end if;
      return Handle_At (Positive (Index), Kind);
   end Granted;

   function Endpoint_Named (Name : String) return CuBit.Handles.Endpoint is
      Word : constant Unsigned_64 := Granted (Name, ST.Endpoint);
   begin
      return CuBit.Handles.C_ABI.To_Endpoint ((Opaque => Word));
   end Endpoint_Named;

   function Startup_Endpoint
     (Name : System.Address) return CuBit.Handles.C_ABI.C_Endpoint
   is
      use type System.Address;
      --  The name and the NUL after it, at most.
      Text : String (1 .. ST.Maximum_Name_Bytes + 1) with Import, Address => Name;
   begin
      if Name = System.Null_Address then
         return CuBit.Handles.C_ABI.No_C_Endpoint;
      end if;
      for Length in 0 .. ST.Maximum_Name_Bytes loop
         if Text (Length + 1) = ASCII.NUL then
            return CuBit.Handles.C_ABI.To_C (Endpoint_Named (Text (1 .. Length)));
         end if;
      end loop;
      return CuBit.Handles.C_ABI.No_C_Endpoint;
   end Startup_Endpoint;

end CuBit.Libc_Startup;
