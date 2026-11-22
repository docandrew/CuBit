with Ada.Text_IO;
with AML_Data;
with AML_Decode; use AML_Decode;
with AML_Names;
with AML_Objects; use AML_Objects;
procedure Member_Data_Tests is
   use type AML_Names.Parse_Status;
   use type Integer_Value;
   type Callback_Mode is (Normal, Missing, Zero_ID, Future_ID, No_Progress, Too_Long);
   type Context is record
      Source : Object_ID;
      Mode : Callback_Mode;
      Visits : Natural := 0;
   end record;
   function Read_Count (Environment : Context; Data : Bytes; Width : Integer_Width)
     return AML_Data.Count_Result
   is
      pragma Unreferenced (Environment);
      N : constant Integer_Result := Read_Integer (Data, Width);
   begin
      if N.Kind = Accepted then return (Accepted, N.Value, N.Consumed); end if;
      return (Kind => N.Kind, others => <>);
   end Read_Count;
   procedure Read_Member (Environment : in out Context; Package_ID : Object_ID;
      Element : Natural; Data : Bytes; Result : out AML_Data.Member_Result) is
      pragma Unreferenced (Package_ID, Element);
      Name : constant AML_Names.Name_Result := AML_Names.Read_Name (Data);
   begin
      Environment.Visits := Environment.Visits + 1;
      if Environment.Mode = Missing or else Name.Kind /= AML_Names.Accepted
        or else Name.Count /= 1 or else Name.Parts (1) /= "SRC0"
      then Result := (Kind => Unsupported, others => <>); return; end if;
      Result := (Accepted, AML_Data.Resolved_Member,
        (case Environment.Mode is when Zero_ID => 0, when Future_ID => Max_Objects,
         when others => Environment.Source),
        (case Environment.Mode is when No_Progress => 0, when Too_Long => Natural'Last,
         when others => Name.Consumed));
   end Read_Member;
   procedure Load_Raw is new AML_Data.Load_Bound (Context, Read_Count, Read_Member);
   procedure Load (Store : in out State; Data : Bytes; Width : Integer_Width;
      Environment : Context; ID : out Object_ID; Consumed : out Natural;
      Status : out AML_Decode.Status) is
      Local : Context := Environment;
   begin
      Load_Raw (Store, Data, Width, Local, ID, Consumed, Status);
      if Status /= Accepted and then Local /= Environment then
         raise Program_Error with "Callback context changed on failure";
      elsif Status = Accepted and then Local.Visits /= Environment.Visits + 2 then
         raise Program_Error with "Successful callback context not published";
      end if;
   end Load;
   Code : constant Bytes := [16#12#,10,2,16#53#,16#52#,16#43#,16#30#,16#53#,16#52#,16#43#,16#30#];
   Store : State := Empty;
   Before : State;
   Source, Root : Object_ID;
   Allocation : Allocation_Status;
   Parsed : AML_Decode.Status;
   Consumed : Natural;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   New_Integer (Store, 7, Source, Allocation); Check (Allocation = Allocated);
   Before := Store;
   for Mode in Missing .. Too_Long loop
      Load (Store, Code, Bits_64, (Source, Mode, 0), Root, Consumed, Parsed);
      Check (Parsed /= Accepted and Root = 0 and Consumed = 0 and Store = Before);
   end loop;
   for Length in 0 .. Code'Length - 1 loop
      Load (Store, Code (1 .. Length), Bits_64, (Source, Normal, 0), Root, Consumed, Parsed);
      Check (Parsed /= Accepted and Root = 0 and Consumed = 0 and Store = Before);
   end loop;
   declare Bad : Bytes := Code; begin
      Bad (8 .. 11) := [16#46#,16#41#,16#49#,16#4C#];
      Load (Store, Bad, Bits_64, (Source, Normal, 0), Root, Consumed, Parsed);
      Check (Parsed = Unsupported and Root = 0 and Consumed = 0 and Store = Before);
   end;
   for Width in Integer_Width loop
      Load (Store, Code, Width, (Source, Normal, 0), Root, Consumed, Parsed);
      Check (Parsed = Accepted and Consumed = Code'Length and Kind (Store, Root) = Package_Object);
      Check (Element (Store, Root, 0) = Source and Element (Store, Root, 1) = Source);
      Check (Live_Count (Store) = Live_Count (Before) + 1);
      Store := Before;
   end loop;
   declare High : constant Bytes (Positive'Last - Code'Length + 1 .. Positive'Last) := Code; begin
      Load (Store, High, Bits_64, (Source, Normal, 0), Root, Consumed, Parsed);
      Check (Parsed = Accepted and Consumed = Code'Length);
      Set_Integer (Store, Source, 9);
      Check (Integer_Data (Store, Element (Store, Root, 0)) = 9);
   end;
   Ada.Text_IO.Put_Line ("MEMBER-DATA: PASS" & Checks'Image);
end Member_Data_Tests;
