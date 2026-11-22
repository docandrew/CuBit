with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with Config_Worker_Protocol; use Config_Worker_Protocol;

procedure Protocol_Tests is
   use type CCL.Objects.Build_Result;
   Types : CCL.Types.Registry;
   Contract, Other_Contract : CCL.Objects.Binding;
   Value, Bad_Value : CCL.Objects.Image;
   Built : CCL.Objects.Build_Result;
   Request, Reply, Bad : Frame;
   OK : Boolean;
   Count : Natural := 0;
   type Words_32 is array (Positive range <>) of Unsigned_32;
   type Words_64 is array (Positive range <>) of Unsigned_64;
   Bad_Lengths : constant Words_32 := [0, 129, 2 ** 31, Unsigned_32'Last];
   Bad_Actions : constant Words_32 := [0, 3, Unsigned_32'Last];
   Bad_Tokens : constant Words_64 := [0, Number'Last];
   procedure Check (Condition : Boolean) is
   begin
      Count := Count + 1;
      if not Condition then raise Program_Error with "protocol check" & Count'Image; end if;
   end Check;

   procedure Start (Action : Operation := Commit; Revision : Number := 1) is
   begin
      Make_Request (Action, 10, 20, Revision, "org.cubit.test", "default",
                    Contract, Value, Request, OK);
      Check (OK and Valid_Request (Request, Contract));
   end Start;
begin
   Check (Frame'Size = Frame_Bytes * 8 and Frame'Object_Size = Frame_Bytes * 8);
   Check (Request.Value'Position = 4096 and Request.Padding'Position = 304);
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, OK); Check (OK);
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 5], Other_Contract, OK); Check (OK);
   Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (42), Built);
   Check (Built = CCL.Objects.Added);
   Bad_Value := Value; Bad_Value.Reserved := 1;
   Check (Valid_Name ("a") and Valid_Name ("com.example.app") and Valid_Name ("prod-1_2"));
   Check (not Valid_Name ("") and not Valid_Name (".a") and not Valid_Name ("a."));
   Check (not Valid_Name ("a..b") and not Valid_Name ("a/b") and not Valid_Name ("a" & ASCII.NUL));
   for Length in 1 .. Maximum_Name + 1 loop
      Check (Valid_Name ([1 .. Length => 'x']) = (Length <= Maximum_Name));
   end loop;
   for Code in Character loop
      Check (Valid_Name ("a" & Code & "b") =
        (Code in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.'));
   end loop;
   declare
      Offset_Name : constant String (5 .. 7) := "a.b";
   begin
      Make_Request (Load, 10, 20, 0, Offset_Name, Offset_Name, Contract, Value, Request, OK);
      Check (OK and Request.Name (1 .. 3) = "a.b");
   end;
   for Op in Operation loop
      Start (Op, (if Op = Load then 0 else 1));
      for Kind in Reply_Kind loop
         for Rev in Number range 0 .. 3 loop
            Make_Reply (Request, Kind, Rev, Contract, Value, Reply, OK);
            Check (OK =
              (if Op = Load then
                 ((Kind = Loaded and Rev > 0) or
                    (Kind in Absent | Load_Failed and Rev = 0))
               else
                 ((Kind = Committed and Rev = 2) or
                    (Kind = Conflict and Rev /= 1) or
                    (Kind = Rejected and Rev = 1) or
                    (Kind = Uncertain and Rev = 0))));
            Check (Valid_Reply (Reply, Request, Contract) = OK);
         end loop;
      end loop;
   end loop;
   Start;
   Make_Reply (Request, Committed, 2, Contract, Value, Reply, OK); Check (OK);
   Check (not Valid_Request (Request, Other_Contract));
   Check (not Valid_Reply (Reply, Request, Other_Contract));
   -- Unknown codes and arbitrary lengths must be rejected before narrowing.
   for N of Bad_Lengths loop
      Bad := Request; Bad.Name_Length := N; Check (not Valid_Request (Bad, Contract));
      Bad := Request; Bad.Context_Length := N; Check (not Valid_Request (Bad, Contract));
   end loop;
   for N of Bad_Actions loop
      Bad := Request; Bad.Action := N; Check (not Valid_Request (Bad, Contract));
   end loop;
   for N of Bad_Tokens loop
      Bad := Request; Bad.Token := N; Check (not Valid_Request (Bad, Contract));
   end loop;
   Bad := Request; Bad.Session := 0; Check (not Valid_Request (Bad, Contract));
   Bad := Request; Bad.Format := 2; Check (not Valid_Request (Bad, Contract));
   Bad := Request; Bad.Reserved := 1; Check (not Valid_Request (Bad, Contract));
   Bad := Request; Bad.Reply := 1; Check (not Valid_Request (Bad, Contract));
   Bad := Request; Bad.Revision := Maximum_Revision; Check (not Valid_Request (Bad, Contract));
   Bad := Request; Bad.Value := Bad_Value; Check (not Valid_Request (Bad, Contract));
   for I in Request.Padding'Range loop
      Bad := Request; Bad.Padding (I) := 1; Check (not Valid_Request (Bad, Contract));
   end loop;
   for I in Natural (Request.Name_Length) + 1 .. Maximum_Name loop
      Bad := Request; Bad.Name (I) := 'x'; Check (not Valid_Request (Bad, Contract));
   end loop;
   for I in Natural (Request.Context_Length) + 1 .. Maximum_Name loop
      Bad := Request; Bad.Context (I) := 'x'; Check (not Valid_Request (Bad, Contract));
   end loop;
   Bad := Reply; Bad.Session := 11; Check (not Valid_Reply (Bad, Request, Contract));
   Bad := Reply; Bad.Token := 21; Check (not Valid_Reply (Bad, Request, Contract));
   Bad := Reply; Bad.Name (1) := 'x'; Check (not Valid_Reply (Bad, Request, Contract));
   Bad := Reply; Bad.Context (1) := 'x'; Check (not Valid_Reply (Bad, Request, Contract));
   Bad := Reply; Bad.Action := Operation'Enum_Rep (Load); Check (not Valid_Reply (Bad, Request, Contract));
   Bad := Reply; Bad.Reply := Unsigned_32'Last; Check (not Valid_Reply (Bad, Request, Contract));
   Bad := Reply; Bad.Value := Value; Check (not Valid_Reply (Bad, Request, Contract));
   Bad := Reply; Bad.Revision := Number'Last; Check (not Valid_Reply (Bad, Request, Contract));
   Start (Commit, Maximum_Revision - 1);
   Make_Reply (Request, Committed, Maximum_Revision, Contract, Value, Reply, OK); Check (OK);
   Start (Load, 0);
   Make_Reply (Request, Loaded, Maximum_Revision, Contract, Value, Reply, OK); Check (OK);
   Make_Reply (Request, Loaded, 1, Contract, Bad_Value, Reply, OK); Check (not OK);
   Make_Request (Commit, 10, 20, 1, "a", "b", Contract, Bad_Value, Request, OK); Check (not OK);
   Make_Request (Load, 10, 20, 1, "a", "b", Contract, Value, Request, OK); Check (not OK);
   Ada.Text_IO.Put_Line ("Config worker protocol:" & Count'Image & " checks PASS");
end Protocol_Tests;
