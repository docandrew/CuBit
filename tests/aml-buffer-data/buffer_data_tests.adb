with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Data;
with AML_Objects;
procedure Buffer_Data_Tests is
   use type AML_Decode.Bytes;
   package O renames AML_Objects;
   use type O.State;
   use type O.Allocation_Status;
   use type O.Object_Kind;
   use type Integer_Value;
   use type Byte;
   Store, Prior : O.State;
   ID, Child, Sentinel : O.Object_ID;
   Consumed : Natural;
   Status : AML_Decode.Status;
   Allocation : O.Allocation_Status;
   Checks : Natural := 0;
   Empty_Data : constant Bytes (1 .. 0) := [others => 0];
   Large : constant Bytes (1 .. 1025) := [others => 16#A7#];
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   function Block (Opcode : Byte; Payload : Bytes) return Bytes is
      Encoding : Positive := 1;
      Extent, Remaining : Natural;
      Header : Bytes (1 .. 4) := [others => 0];
   begin
      if Payload'Length + 1 > 63 then Encoding := 2; end if;
      if Payload'Length + 2 > 4095 then Encoding := 3; end if;
      if Payload'Length + 3 > 1_048_575 then Encoding := 4; end if;
      Extent := Payload'Length + Encoding;
      if Encoding = 1 then Header (1) := Byte (Extent);
      else
         Header (1) := Byte ((Encoding - 1) * 64 + Extent mod 16);
         Remaining := Extent / 16;
         for I in 2 .. Encoding loop
            Header (I) := Byte (Remaining mod 256); Remaining := Remaining / 256;
         end loop;
      end if;
      return Bytes'[Opcode] & Header (1 .. Encoding) & Payload;
   end Block;
   function Buffer_Code (Count : Integer_Value; Initializer : Bytes) return Bytes is
      Literal : Bytes (1 .. 9) := [others => 0];
   begin
      Literal (1) := 16#0E#;
      for I in 0 .. 7 loop Literal (I + 2) := Byte ((Count / 2 ** (I * 8)) mod 256); end loop;
      return Block (16#11#, Literal & Initializer);
   end Buffer_Code;
   procedure Verify_Buffer (Object : O.Object_ID; Size : Natural; Initializer : Bytes) is
      Correct : Boolean := True;
   begin
      Check (O.Kind (Store, Object) = O.Buffer_Object and then O.Length (Store, Object) = Size,
        "buffer type and uncapped size");
      for I in 1 .. Size loop
         Correct := Correct and then O.Stored_Byte (Store, Object, I - 1) =
           (if I <= Initializer'Length then Initializer (Initializer'First + I - 1) else 0);
      end loop;
      Check (Correct, "exact initializer and zero tail");
   end Verify_Buffer;
   procedure Check_Accepted (Count : Integer_Value; Initializer : Bytes; Width : Integer_Width) is
      Code : constant Bytes := Buffer_Code (Count, Initializer);
      Shifted : constant Bytes (7 .. Code'Length + 6) := Code;
      High : constant Bytes (Positive'Last - Code'Length + 1 .. Positive'Last) := Code;
      Size : constant Natural := Natural (Integer_Value'Max
        (Count and 16#FFFF_FFFF#, Integer_Value (Initializer'Length)));
   begin
      Store := O.Empty;
      O.New_Bytes (Store, O.Buffer_Object, [16#D3#, 16#7E#], Sentinel, Allocation);
      Check (Allocation = O.Allocated, "nonempty prior sentinel");
      AML_Data.Load (Store, Shifted & [16#CC#], Width, ID, Consumed, Status);
      Check (Status = Accepted and then Consumed = Code'Length, "top-level exact consumed excludes suffix");
      Verify_Buffer (ID, Size, Initializer);
      Check (O.Stored_Byte (Store, Sentinel, 0) = 16#D3# and then O.Stored_Byte (Store, Sentinel, 1) = 16#7E#,
        "prior bytes preserved");
      Store := O.Empty;
      AML_Data.Load (Store, High, Width, ID, Consumed, Status);
      Check (Status = Accepted and then Consumed = Code'Length, "high array bound including empty initializer");
      Verify_Buffer (ID, Size, Initializer);
      Store := O.Empty;
      declare
         Package_Code : constant Bytes := Block (16#12#, Bytes'[2] & Code & [16#0A#, 16#42#]);
      begin
         AML_Data.Load (Store, Package_Code, Width, ID, Consumed, Status);
         Check (Status = Accepted and then Consumed = Package_Code'Length
           and then O.Kind (Store, ID) = O.Package_Object, "package admits buffer child and following scalar");
         Child := O.Element (Store, ID, 0); Verify_Buffer (Child, Size, Initializer);
         Check (O.Integer_Data (Store, O.Element (Store, ID, 1)) = 16#42#, "child advancement preserves next operand");
      end;
   end Check_Accepted;
   procedure Reject (Code : Bytes; Width : Integer_Width; Expected : AML_Decode.Status) is
   begin
      Prior := Store;
      AML_Data.Load (Store, Code, Width, ID, Consumed, Status);
      Check (Status = Expected and then ID = 0 and then Consumed = 0 and then Store = Prior,
        "failed load rolls back all allocation and input advancement");
   end Reject;
   type Context is record Reads : Natural := 0; end record;
   function Read_Count (Environment : Context; Data : Bytes; Width : Integer_Width)
     return AML_Data.Count_Result is
      pragma Unreferenced (Environment);
      Count : constant Integer_Result := Read_Integer (Data, Width);
   begin
      if Count.Kind = Accepted then return (Accepted, Count.Value, Count.Consumed); end if;
      return (Kind => Count.Kind, others => <>);
   end Read_Count;
   procedure Read_Member (Environment : in out Context; Package_ID : O.Object_ID;
     Element : Natural; Data : Bytes; Result : out AML_Data.Member_Result) is
      pragma Unreferenced (Package_ID, Element);
   begin
      if Data'Length < 4 then Result := (Kind => Truncated, others => <>); return; end if;
      Environment.Reads := Environment.Reads + 1;
      Result := (Accepted, AML_Data.Deferred_Member, O.No_Object, 4);
   end Read_Member;
   procedure Load_Bound is new AML_Data.Load_Bound (Context, Read_Count, Read_Member);
   Environment : Context;
   Lengths : constant array (Positive range <>) of Integer_Value := [0, 1, 1024, 1025, 8192];
begin
   for Width in Integer_Width loop
      for Count of Lengths loop Check_Accepted (Count, Empty_Data, Width); end loop;
      Check_Accepted (0, Large, Width); Check_Accepted (1025, Large, Width); Check_Accepted (8192, Large, Width);
      Check_Accepted (16#1_0000_0001#, [16#5A#], Width);
      Store := O.Empty;
      O.New_Bytes (Store, O.Buffer_Object, [16#55#], Sentinel, Allocation);
      Check (Allocation = O.Allocated, "failure sentinel");
      Reject (Buffer_Code (O.Max_Bytes, Empty_Data), Width, Limit_Exceeded);
      Reject (Buffer_Code (Integer_Value'Last, Empty_Data), Width, Limit_Exceeded);
      Reject (Block (16#12#, Bytes'[2] & Buffer_Code (8192, [16#7E#]) & [16#02#]), Width, Unsupported);
      Reject (Block (16#12#, Bytes'[2] & Buffer_Code (8192, Empty_Data)
        & Buffer_Code (O.Max_Bytes, Empty_Data)), Width, Limit_Exceeded);
      Reject (Bytes'[16#11#, 5, 16#FF#], Width, Truncated);
      Prior := Store; Environment := (Reads => 7);
      Load_Bound (Store, Block (16#12#, Bytes'[2, 16#41#, 16#42#, 16#43#, 16#30#]
        & Buffer_Code (O.Max_Bytes, Empty_Data)), Width, Environment, ID, Consumed, Status);
      Check (Status = Limit_Exceeded and then ID = 0 and then Consumed = 0
        and then Store = Prior and then Environment.Reads = 7,
        "buffer quota failure rolls back earlier deferred member context");
      Store := O.Empty;
      for I in 1 .. O.Max_Objects loop
         O.New_Integer (Store, 0, ID, Allocation);
         Check (Allocation = O.Allocated, "object quota setup");
      end loop;
      Reject (Buffer_Code (0, Empty_Data), Width, Limit_Exceeded);
   end loop;
   Ada.Text_IO.Put_Line ("Buffer data checks" & Checks'Image);
end Buffer_Data_Tests;
