pragma Ada_2022;
with Ada.Text_IO;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.Dependent_Groups;
procedure Resource_Dependent_Tests is
   package G renames ACPI_Resource_Data.Dependent_Groups;
   use type G.Analysis_Status;
   use type G.Priority;
   use type G.Alternative;
   use type Firmware_Tables.Byte;
   Checks : Natural:=0;
   procedure Verify(Good : Boolean; Label : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Label; end if; end Verify;
   type Token is (Start_Token, End_Token, Ordinary_Token);
   type Token_Array is array(Positive range <>) of Token;
   function Item(T : Token) return Firmware_Tables.Bytes is
     (case T is when Start_Token=>[16#30#],when End_Token=>[16#38#],when Ordinary_Token=>[16#71#,16#AA#]);
   procedure Sequence(Tokens : Token_Array) is
      Data : Firmware_Tables.Bytes(1 .. 2+Tokens'Length*2):=[others=>0];
      Size, Starts, Ends, Last_Start, End_Position : Natural:=0;
      Valid : Boolean;
   begin
      for I in Tokens'Range loop
         declare Encoded : constant Firmware_Tables.Bytes:=Item(Tokens(I)); begin
            Data(Size+1 .. Size+Encoded'Length):=Encoded; Size:=Size+Encoded'Length;
         end;
         if Tokens(I)=Start_Token then Starts:=Starts+1; Last_Start:=I;
         elsif Tokens(I)=End_Token then Ends:=Ends+1; End_Position:=I; end if;
      end loop;
      Data(Size+1 .. Size+2):=[16#79#,0]; Size:=Size+2;
      -- Independent positional grammar: one end, after every start; source
      -- order determines alternatives. No mutable parser-state simulation.
      Valid:=(if Starts=0 then Ends=0 else Ends=1 and then End_Position>Last_Start);
      for Capacity in Natural range 0 .. 4 loop
         declare
            Alternatives : G.Alternative_Array(1 .. Capacity);
            Result : G.Analysis_Result;
         begin
            G.Analyze(Data(1 .. Size),Alternatives,Result);
            Verify((Result.Status=G.Grouped)=(Valid and then Starts<=Capacity),"independent token grammar/capacity");
            if Result.Status=G.Grouped then
               Verify(Result.Value.Alternatives=Item_Count(Starts) and then Result.Value.End_Tag=Byte_Offset(Size-2),"group count/end tag");
               declare Cursor, Number, Ordinary_Count, Before, After : Natural:=0;
                 Active : Boolean:=False;
               begin
                  for T of Tokens loop
                     if T=Start_Token then
                        if Active then
                           Verify(Alternatives(Number).Limit=Byte_Offset(Cursor)
                             and then Alternatives(Number).Items=Item_Count(Ordinary_Count),"sibling span and count");
                        end if;
                        Number:=Number+1; Ordinary_Count:=0; Active:=True;
                        Verify(Alternatives(Number).Marker=Byte_Offset(Cursor)
                          and then Alternatives(Number).First=Byte_Offset(Cursor+1)
                          and then not Alternatives(Number).Information_Present
                          and then Alternatives(Number).Compatibility=G.Acceptable
                          and then Alternatives(Number).Performance=G.Acceptable,"default priorities/source order");
                     elsif T=End_Token then
                        Verify(Alternatives(Number).Limit=Byte_Offset(Cursor)
                          and then Alternatives(Number).Items=Item_Count(Ordinary_Count),"last alternative span/count");
                        Active:=False;
                     elsif Active then Ordinary_Count:=Ordinary_Count+1;
                     elsif Number=0 then Before:=Before+1; else After:=After+1; end if;
                     Cursor:=Cursor+Item(T)'Length;
                  end loop;
                  Verify(Result.Value.Common_Before=Item_Count(Before)
                    and then Result.Value.Common_After=Item_Count(After),"common items prefix/suffix");
               end;
               for I in Starts+1 .. Capacity loop
                  Verify(Alternatives(I)=G.Alternative'(others=><>),"unused slots defaulted");
               end loop;
            else
               for A of Alternatives loop Verify(A=G.Alternative'(others=><>),"failure output atomic/default"); end loop;
            end if;
         end;
      end loop;
   end Sequence;
   procedure Expect(Data : Firmware_Tables.Bytes; Status : G.Analysis_Status; Position : Byte_Offset) is
      Alternatives : G.Alternative_Array(1 .. 4);
      Result : G.Analysis_Result;
   begin
      G.Analyze(Data,Alternatives,Result);
      Verify(Result.Status=Status and then Result.Position=Position,"explicit failure/status position");
      if Status/=G.Grouped then
         for A of Alternatives loop Verify(A=G.Alternative'(others=><>),"explicit failure no partial output"); end loop;
      end if;
   end Expect;
begin
   for Length in Natural range 0 .. 7 loop
      for Code in Natural range 0 .. 3**Length-1 loop
         declare Tokens : Token_Array(1 .. Length); Remaining : Natural:=Code; begin
            for I in Tokens'Range loop Tokens(I):=Token'Val(Remaining mod 3); Remaining:=Remaining/3; end loop;
            Sequence(Tokens);
         end;
      end loop;
   end loop;
   for Info in Firmware_Tables.Byte loop
      declare Alternatives : G.Alternative_Array(1 .. 1); Result : G.Analysis_Result; begin
         G.Analyze([16#31#,Info,16#71#,16#AA#,16#38#,16#79#,0],Alternatives,Result);
         if Info>=16 then Verify(Result.Status=G.Reserved_Priority_Bits,"reserved priority bits");
         elsif Info mod 4=3 or else Info/4=3 then Verify(Result.Status=G.Reserved_Priority,"reserved priority fields");
         else
            Verify(Result.Status=G.Grouped and then Result.Value.Alternatives=1,"explicit priorities grouped");
            Verify(Alternatives(1).Information_Present and then Alternatives(1).Information=Info
              and then Alternatives(1).Compatibility=G.Priority'Val(Natural(Info mod 4))
              and then Alternatives(1).Performance=G.Priority'Val(Natural(Info/4)),"all priority pairs");
         end if;
      end;
   end loop;
   Expect([],G.Framing_Error,0);
   Expect([16#30#,16#79#,0],G.Missing_End,1);
   Expect([16#38#,16#79#,0],G.End_Without_Start,0);
   Expect([16#30#,16#38#,16#38#,16#79#,0],G.Repeated_End,2);
   Expect([16#30#,16#38#,16#30#,16#79#,0],G.Start_After_End,2);
   Expect([16#32#,0,0,16#38#,16#79#,0],G.Bad_Dependent_Length,0);
   Expect([16#30#,16#39#,0,16#79#,0],G.Bad_Dependent_Length,1);
   Expect([16#30#,16#38#,16#79#,1],G.Framing_Error,2);
   declare
      Data : constant Firmware_Tables.Bytes:=[16#30#,16#30#,16#38#,16#79#,0];
      High : constant Firmware_Tables.Bytes(Positive'Last-Data'Length+1 .. Positive'Last):=Data;
      Alternatives : G.Alternative_Array(Positive'Last-1 .. Positive'Last);
      Result : G.Analysis_Result;
   begin
      G.Analyze(High,Alternatives,Result);
      Verify(Result.Status=G.Grouped and then Result.Value.Alternatives=2,"high input/output array bounds");
      Verify(Alternatives(Alternatives'First).First=1 and then Alternatives(Alternatives'First).Limit=1
        and then Alternatives(Alternatives'Last).First=2 and then Alternatives(Alternatives'Last).Limit=2,
        "empty alternatives retained");
      G.Analyze(Data,Alternatives,Result,2);
      Verify(Result.Status=G.Framing_Error and then Result.Cause=Item_Limit,"marker items included in quota");
      G.Analyze(Data,Alternatives,Result,3);
      Verify(Result.Status=G.Grouped,"exact item quota");
   end;
   Ada.Text_IO.Put_Line("Resource dependent checks" & Natural'Image(Checks));
end Resource_Dependent_Tests;
