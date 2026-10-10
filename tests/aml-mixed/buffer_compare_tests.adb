pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with AML_References;
with AML_String_Order;
with Test_Namespace; use Test_Namespace;
procedure Buffer_Compare_Tests is
   use Owned;
   use type Integer_Value;
   use type AML_Objects.Allocation_Status;
   use type AML_String_Order.Ordering;
   A, B : Arena;
   OK : Boolean;
   Checks : Natural := 0;
   L, R, Foreign, Bad : Datum;
   Status : Execution_Status;
   Value : Integer_Value;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Buffer_Value (Owner : in out Arena; Data : Bytes; Item : out Datum) is
      ID : AML_Objects.Object_ID;
      H : AML_References.Object_Handle;
      Allocated : AML_Objects.Allocation_Status;
   begin
      Append (Owner, Data, ID, Allocated); Check (Allocated = AML_Objects.Allocated);
      Make_Source (Owner, ID, H, OK); Check (OK);
      Read_Source (Owner, H, Item, Status); Check (Status = Returned);
   end Buffer_Value;
   procedure Pair (Left, Right : Bytes; Order : AML_String_Order.Ordering) is
   begin
      Reset (A, OK); Check (OK);
      Buffer_Value (A, Left, L); Buffer_Value (A, Right, R);
      declare
         Before : constant State := Snapshot (A);
      begin
         for W in Integer_Width loop
            for Op in Byte range 16#93# .. 16#95# loop
               Compare_Byte_Values (A, Op, L, R, W, Value, Status);
               Check (Status = Returned and then Value =
                 (if (case Op is when 16#93# => Order = AML_String_Order.Equal,
                                 when 16#94# => Order = AML_String_Order.Greater,
                                 when others => Order = AML_String_Order.Less)
                  then (if W = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last) else 0));
               Check (Snapshot (A) = Before);
            end loop;
         end loop;
      end;
   end Pair;
   procedure Reject (Left, Right : Datum; Op : Byte := 16#93#) is
      Before : constant State := Snapshot (A);
   begin
      Compare_Byte_Values (A, Op, Left, Right, Bits_64, Value, Status);
      Check (Status = Unsupported_Value and then Value = 0 and then Snapshot (A) = Before);
   end Reject;
begin
   Pair ([], [], AML_String_Order.Equal);
   Pair ([], [0], AML_String_Order.Less);
   Pair ([0], [], AML_String_Order.Greater);
   Pair ([0, 1], [0, 1], AML_String_Order.Equal);
   Pair ([0, 1], [0, 2], AML_String_Order.Less);
   Pair ([0, 255], [0, 127], AML_String_Order.Greater);
   Pair ([1], [1, 0], AML_String_Order.Less);
   Pair ([1, 0], [1], AML_String_Order.Greater);
   Pair ([2], [1, 255, 255], AML_String_Order.Greater);
   Pair ([1, 255], [2], AML_String_Order.Less);
   declare High : constant Bytes (Positive'Last - 2 .. Positive'Last) := [0,128,255]; begin
      Pair (High, [0,128,255], AML_String_Order.Equal);
   end;
   Bad := L; Bad.Object.ID := R.Object.ID; Reject (Bad, R);
   Reset (B, OK); Check (OK); Buffer_Value (B, [0,128,255], Foreign);
   Reject (Foreign, R); Reject (L, Foreign);
   Reject (L, R, 0);
   -- Buffer/Integer is now supported: zero converts to eight zero bytes.
   declare Before : constant State := Snapshot (A); begin
      Compare_Byte_Values (A, 16#93#, L, (Integer_Datum, 0, Ordinary_Integer),
                          Bits_64, Value, Status);
      Check (Status = Returned and then Value = 0 and then Snapshot (A) = Before);
   end;
   Reject ((Integer_Datum, 0, Ordinary_Integer), R);
   Bad := L; Bad.Object.Type_Code := 0; Bad.Object.Size := Natural'Last;
   Compare_Byte_Values (A, 16#93#, Bad, R, Bits_64, Value, Status);
   Check (Status = Returned and then Value = Integer_Value'Last);
   Reset (A, OK); Check (OK); Buffer_Value (A, [0,128,255], R);
   Reject (L, R); Reject (R, L);
   Ada.Text_IO.Put_Line ("BUFFER-COMPARE PASS" & Checks'Image);
end Buffer_Compare_Tests;
