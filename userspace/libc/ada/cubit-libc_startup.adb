------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Libc_Startup - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Launch_Envelopes;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Libc_Process;

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

   --  sysexits.h's EX_CONFIG, as CuBit.Startup_Check.Startup_Refused.
   Startup_Refused : constant := 78;

   --  The whole launch block's length, kept by the shared start code.
   Block_Length : Unsigned_64
   with Import, Convention => C, External_Name => "__cubit_launch_block_length";

   procedure Require
     (Expected : System.Address; Count : Interfaces.C.unsigned_long; Program : System.Address)
   is
      use type System.Address;
      use type Interfaces.C.unsigned_long;
      Maximum_Program : constant := 256;
      --  The longest C string read, bounded: a name never exceeds the table's.
      function C_Length (Text : System.Address; Limit : Natural) return Natural is
         Bytes : constant ST.Bytes (1 .. Limit) with Import, Address => Text;
      begin
         if Text = System.Null_Address then
            return 0;
         end if;
         for I in Bytes'Range loop
            if Bytes (I) = 0 then
               return I - 1;
            end if;
         end loop;
         return Limit;
      end C_Length;

      --  The refusal is one write, so other programs' output cannot split it.
      Note_Bytes : constant := 160;
      Note : String (1 .. Maximum_Program + ST.Maximum_Name_Bytes + Note_Bytes);
      Note_Length : Natural := 0;
      procedure Put (Text : String) is
         Room : constant Natural := Natural'Min (Text'Length, Note'Length - Note_Length);
      begin
         Note (Note_Length + 1 .. Note_Length + Room) := Text (Text'First .. Text'First + Room - 1);
         Note_Length := Note_Length + Room;
      end Put;
      procedure Put (Text : System.Address; Length : Natural) is
         Bytes : constant String (1 .. Length) with Import, Address => Text;
      begin
         Put (Bytes);
      end Put;

      procedure Stop (Why : String; Name : System.Address; Name_Length : Natural) is
         Ignore : Unsigned_64;
      begin
         Put ("startup: ");
         Put (Program, C_Length (Program, Maximum_Program));
         Put (Why);
         if Name_Length > 0 then
            Put ("""");
            Put (Name, Name_Length);
            Put ("""");
         end if;
         Put ("; stopping (CAP-003 startup check)" & ASCII.LF);
         CuBit.Libc_Process.Debug_Write (Note'Address, Interfaces.C.size_t (Note_Length));
         loop
            Ignore := CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Exit_Process, Startup_Refused);
         end loop;
      end Stop;

      Entries : constant Natural :=
        (if Count > Interfaces.C.unsigned_long (ST.Maximum_Entries) then ST.Maximum_Entries
         else Natural (Count));
      type Expectation_Array is array (1 .. Entries) of C_Expectation with Convention => C;
      Declared : constant Expectation_Array with Import, Address => Expected;
   begin
      Load (Block_Length);
      if not Loaded then
         Stop (": the launch block carried no valid startup table", System.Null_Address, 0);
      end if;
      for E of Declared loop
         declare
            Length : constant Natural := C_Length (E.Name, ST.Maximum_Name_Bytes);
            Name : constant String (1 .. Length) with Import, Address => E.Name;
            Index : constant ST.Entry_Count :=
              (if Length in 1 .. ST.Maximum_Name_Bytes then ST.Find (Current, ST.To_Name (Name)) else 0);
         begin
            if Index = 0 then
               Stop (": its startup table has no ", E.Name, Length);
            elsif ST.Capability_Kind'Enum_Rep (Current.Entries (Index).Kind) /= Unsigned_64 (E.Kind) then
               Stop (": its startup table has another kind of ", E.Name, Length);
            elsif ST.Need'Enum_Rep (Current.Entries (Index).Requirement) /= Unsigned_64 (E.Need) then
               Stop (": its startup table disagrees on the need of ", E.Name, Length);
            elsif E.Need = ST.Need'Enum_Rep (ST.Required)
              and then Current.Entries (Index).State /= ST.Granted
            then
               Stop (": it was not granted its required ", E.Name, Length);
            end if;
         end;
      end loop;
   end Require;

end CuBit.Libc_Startup;
