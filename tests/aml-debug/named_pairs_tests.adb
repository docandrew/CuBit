with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
procedure Named_Pairs_Tests is
   use type AML_Decode.Integer_Value;
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   A : Arena;
   OK : Boolean;
   Loaded_Result : Load_Status;
   Status : Execution_Status;
   Source : Datum;
   Handle : AML_References.Object_Handle;
   Target : Reference;
   ID : AML_Objects.Object_ID;
   Checks : Natural := 0;
   Fixture : constant Bytes :=
     [16#08#,65,65,65,65,16#0B#,16#34#,16#12#,
      16#08#,66,66,66,66,16#0D#,49,70,0,
      16#08#,67,67,67,67,16#11#,5,16#0A#,2,16#0A#,16#F0#,
      16#08#,68,68,68,68,1,
      16#08#,69,69,69,69,16#0D#,97,0,
      16#08#,70,70,70,70,16#11#,5,16#0A#,2,0,0];
   procedure Check (C : Boolean) is
   begin Checks := Checks + 1; if not C then raise Program_Error with Checks'Image; end if; end Check;
   function Text (S : String) return Bytes is
      B : Bytes (1 .. S'Length);
   begin for I in B'Range loop B (I) := Character'Pos (S (S'First + I - 1)); end loop; return B; end Text;
begin
   for Width in Integer_Width loop
      for From_Node in Node_ID range 1 .. 3 loop
         for To_Node in Node_ID range 4 .. 6 loop
            Reset (A, OK); Check (OK);
            Load (A, Fixture, Width, Loaded_Result); Check (Loaded_Result = Loaded);
            Make_Source (A, Data_Object (Snapshot (A), From_Node), Handle, OK); Check (OK);
            Read_Source (A, Handle, Source, Status); Check (Status = Returned);
            Make_Named_Reference (A, To_Node, Target, OK); Check (OK);
            ID := Data_Object (Snapshot (A), To_Node);
            Store_Reference_Value (A, Target, Width, Source, Status); Check (Status = Returned);
            Check (Data_Object (Snapshot (A), To_Node) = ID);
            if To_Node = 4 then
               Check (Integer_Data (Snapshot (A), To_Node) =
                 (case From_Node is when 1 => 16#1234#, when 2 => 16#1F#, when others => 16#F00A#));
            elsif To_Node = 5 then
               Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), ID) =
                 (case From_Node is when 1 => Text ((if Width = Bits_32 then "00001234" else "0000000000001234")),
                   when 2 => Text ("1F"), when others => Text ("0x0A 0xF0")));
            else
               Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), ID) =
                 (case From_Node is when 1 => Bytes'[16#34#,16#12#],
                   when 2 => Bytes'[49,70], when others => Bytes'[16#0A#,16#F0#]));
            end if;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("NAMED PAIRS PASS" & Checks'Image);
end Named_Pairs_Tests;
