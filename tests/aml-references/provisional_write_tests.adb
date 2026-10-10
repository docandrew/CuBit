with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Provisional_Write_Tests is
   use type Integer_Value;
   use type AML_Objects.State;
   use type AML_Objects.Allocation_Status;
   use type AML_References.Reference;
   use type AML_References.Node_Position;
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package NS is new AML_Namespace (128, AML_Delays.Unavailable_Provider, Capture);
   use NS; use NS.Owned;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   type Scenario is (Plain_Scalar, Copy_Direct, Copy_Argument, Direct_Rewrite,
     Argument_Rewrite, Complete_Existing, Abort_Reference,
     Quota_Object, Quota_Byte, Quota_Element,
     Quota_Argument_Object, Quota_Argument_Byte, Quota_Argument_Element,
     Quota_Scalar, Quota_In_Place);
   Current : Scenario;
   Width : Integer_Width;
   Active_Method : Node_ID;
   Calls : Natural := 0;
   Checks : Natural := 0;
   Token : Name_Reservation;
   Source, Constant_Source, Cloned_Reference : AML_References.Object_Handle;
   Initial_ID : AML_Objects.Object_ID;
   Before_Call : NS.State;
   S : Execution_Status := No_Return;
   function Segment (Name : String) return Bytes is
      Data : Bytes (1 .. Name'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Data;
   end Segment;
   function Path (Name : String) return AML_Names.Name_Result is
     (AML_Names.Read_Name (Segment (Name)));
   function Method (Name : String; Code : Bytes; Args : Byte := 0) return Bytes is
     (Bytes'(16#14#, Byte (Code'Length + 6)) & Segment (Name) & Bytes'(1 => Args) & Code);
   Timer : constant Bytes := [16#5B#,16#33#];
   Temp : constant Bytes := Segment ("TEMP");
   Read_Temp : constant Bytes := Bytes'(1 => 16#A4#) & Temp;
   Fixture : constant Bytes :=
     Method ("SCAL", Timer & Bytes'(16#70#,1) & Temp & Timer & Read_Temp) &
     Method ("COPY", Timer & Bytes'(16#9D#,1) & Temp & Timer & Read_Temp) &
     Method ("AONE", [16#9D#,1,16#68#,16#A4#,0], 1) &
     Method ("CPAR", Timer & Segment ("AONE") & Bytes'(1 => 16#71#) & Temp & Timer & Read_Temp) &
     Method ("RWRT", Timer & Bytes'(16#70#,16#0A#,7) & Temp & Timer & Read_Temp) &
     Method ("AST7", [16#70#,16#0A#,7,16#68#,16#A4#,0], 1) &
     Method ("RARG", Timer & Segment ("AST7") & Bytes'(1 => 16#71#) & Temp & Timer & Read_Temp) &
     Method ("OBS0", Timer & Read_Temp) &
     Method ("CPBF", Timer & Bytes'(1 => 16#9D#) & Segment ("BUF0") & Temp & Timer & Bytes'(16#A4#,1)) &
     Method ("ABUF", Bytes'(1 => 16#9D#) & Segment ("BUF0") & Bytes'(16#68#,16#A4#,0), 1) &
     Method ("CAB0", Timer & Segment ("ABUF") & Bytes'(1 => 16#71#) & Temp & Timer & Bytes'(16#A4#,1)) &
     Method ("CPPK", Timer & Bytes'(1 => 16#9D#) & Segment ("PKG0") & Temp & Timer & Bytes'(16#A4#,1)) &
     Method ("APKG", Bytes'(1 => 16#9D#) & Segment ("PKG0") & Bytes'(16#68#,16#A4#,0), 1) &
     Method ("CAP0", Timer & Segment ("APKG") & Bytes'(1 => 16#71#) & Temp & Timer & Bytes'(16#A4#,1)) &
     Bytes'(1 => 16#08#) & Segment ("CONE") & Bytes'(1 => 1) &
     Bytes'(1 => 16#08#) & Segment ("BUF0") & Bytes'(16#11#,4,16#0A#,1,7) &
     Bytes'(1 => 16#08#) & Segment ("PKG0") & Bytes'(16#12#,7,1,16#11#,4,16#0A#,1,7) &
     Bytes'(1 => 16#08#) & Segment ("FILL") & Bytes'(16#12#,3,1,0);
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with Current'Image & Width'Image & Checks'Image & S'Image;
      end if;
   end Check;
   function Located (Name : String) return Node_ID is
      Result : constant Lookup_Result := Resolve (Snapshot (A), Root, Path (Name));
   begin
      Check (Result.Status = Found);
      return Result.Node;
   end Located;
   procedure Source_For (Name : String; Handle : out AML_References.Object_Handle) is
      OK : Boolean;
   begin
      Make_Source (A, Data_Object (Snapshot (A), Located (Name)), Handle, OK);
      Check (OK);
   end Source_For;
   procedure Fill_Objects (Remaining : Natural) is
      ID : AML_Objects.Object_ID;
      Status : AML_Objects.Allocation_Status;
   begin
      while Values_Used (A).Objects < AML_Objects.Max_Objects - Remaining loop
         Append (A, Bytes'(1 .. 0 => 0), ID, Status);
         Check (Status = AML_Objects.Allocated);
      end loop;
   end Fill_Objects;
   procedure Prepare_Quota is
      ID : AML_Objects.Object_ID;
      Status : AML_Objects.Allocation_Status;
      Loaded_Status : Load_Status;
      Index : Natural := 0;
   begin
      case Current is
         when Quota_Object | Quota_Argument_Object | Quota_In_Place => Fill_Objects (1);
         when Quota_Scalar => Fill_Objects (0);
         when Quota_Byte | Quota_Argument_Byte =>
            Append (A, Bytes'(1 .. AML_Objects.Max_Bytes - Values_Used (A).Bytes - 1 => 0), ID, Status);
            Check (Status = AML_Objects.Allocated);
         when Quota_Element | Quota_Argument_Element =>
            while Values_Used (A).Elements < AML_Objects.Max_Elements - 1 loop
               declare
                  Count : constant Natural := Natural'Min (Byte'Pos (Byte'Last),
                    AML_Objects.Max_Elements - 1 - Values_Used (A).Elements);
               begin
                  Load (A, Bytes'(16#08#,90,90,
                    Byte (Character'Pos ('A') + Index / 26), Byte (Character'Pos ('A') + Index mod 26),
                    16#12#,2,Byte (Count)), Width, Loaded_Status);
                  Check (Loaded_Status = Loaded);
               end;
               Index := Index + 1;
            end loop;
         when others => null;
      end case;
   end Prepare_Quota;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
      Prior : NS.State;
      Item, Copy, Readback : Datum;
      Old_Reference, Package_Reference : Reference;
      Holder : AML_References.Object_Handle;
      Saved_Node : AML_References.Node_Position;
      OK : Boolean;
      Used_Before : AML_Objects.Object_ID;
      Is_Indirect : constant Boolean := Current in Quota_Argument_Object | Quota_Argument_Byte | Quota_Argument_Element;
   begin
      Value := 0; Available := True; Calls := Calls + 1;
      if Calls = 1 then
         Reserve_Name (A, Active_Method, Path ("TEMP"), Token, S);
         Check (S = Returned and then Reservation_Matches (A, Token));
         if Current in Direct_Rewrite | Argument_Rewrite | Quota_In_Place then
            Store_Reference_Value (A, Reservation_Reference (Token), Width,
              (Integer_Datum, 1, AML_Constant), S);
            Check (S = Returned and then Reservation_Matches (A, Token));
            Initial_ID := Data_Object (Snapshot (A), Node_ID (AML_References.Named_Node (Reservation_Reference (Token))));
         elsif Current = Complete_Existing then
            Prior := Snapshot (A);
            Complete_Name (A, Token, Constant_Source, S);
            Check (S = Returned and then Value_Store (Snapshot (A)) = Value_Store (Prior));
            Check (Data_Object (Snapshot (A), Node_ID (AML_References.Named_Node (Reservation_Reference (Token)))) =
              AML_References.Source (Constant_Source));
         elsif Current = Abort_Reference then
            Old_Reference := Reservation_Reference (Token);
            Saved_Node := AML_References.Named_Node (Old_Reference);
            Source_For ("FILL", Holder);
            Make_Index (A, Holder, 0, Package_Reference, S); Check (S = Returned);
            Store_Reference_Value (A, Package_Reference, Width, (Reference_Datum, Old_Reference), S);
            Check (S = Returned);
            Make_Source (A, AML_Objects.Element (Value_Store (Snapshot (A)), AML_References.Source (Holder), 0), Holder, OK);
            Check (OK);
            Clone_Source (A, Holder, Cloned_Reference, S); Check (S = Returned);
            Prior := Snapshot (A); Abort_Name (A, Token, S);
            Check (S = Returned and then Value_Store (Snapshot (A)) = Value_Store (Prior));
            Reserve_Name (A, Active_Method, Path ("TEMP"), Token, S); Check (S = Returned);
            Check (AML_References.Named_Node (Reservation_Reference (Token)) = Saved_Node
              and then Reservation_Reference (Token) /= Old_Reference);
            Read_Source (A, Cloned_Reference, Readback, S);
            Check (S = Returned and then Readback.Value_Kind = Reference_Datum and then Readback.Ref = Old_Reference);
            Resolve_Value (A, Readback.Ref, Item, S); Check (S = Unsupported_Value);
            Prior := Snapshot (A);
            Store_Reference_Value (A, Old_Reference, Width, (Integer_Datum,9,Ordinary_Integer), S);
            Check (S = Unsupported_Value and then Snapshot (A) = Prior);
            Complete_Name (A, Token, Constant_Source, S); Check (S = Returned);
         elsif Current in Quota_Object .. Quota_Argument_Element then
            Read_Source (A, Source, Item, S); Check (S = Returned);
            Prior := Snapshot (A);
            Copy_And_Attach (A,
              (if Is_Indirect then (Kind => Referenced_Destination, Ref => Reservation_Reference (Token))
               else (Kind => Named_Destination, Scope => Natural (Active_Method), Path => Path ("TEMP"))),
              Width, Item, Copy, S);
            Check (S = Value_Limit and then Snapshot (A) = Prior
              and then Reservation_Matches (A, Token)
              and then Copy.Value_Kind = Integer_Datum and then Copy.Number = 0);
         end if;
      else
         Check (Calls = 2 and then Reservation_Matches (A, Token));
         Resolve_Value (A, Reservation_Reference (Token), Item, S);
         Check (S = Returned and then Item.Value_Kind = Integer_Datum);
         Check (Item.Number = (if Current in Direct_Rewrite | Argument_Rewrite | Quota_In_Place then 7 else 1));
         Check (Item.Origin = (if Current = Argument_Rewrite then Ordinary_Integer else AML_Constant));
         if Current in Direct_Rewrite | Argument_Rewrite | Quota_In_Place then
            Used_Before := Data_Object (Snapshot (A), Node_ID (AML_References.Named_Node (Reservation_Reference (Token))));
            Check ((Used_Before = Initial_ID) = (Current /= Argument_Rewrite));
         end if;
         Prior := Snapshot (A);
         Complete_Name (A, Token, AML_References.No_Object_Handle, S);
         Check (S = Returned and then Value_Store (Snapshot (A)) = Value_Store (Prior));
         Resolve_Value (A, Reservation_Reference (Token), Readback, S);
         Check (S = Returned and then Readback = Item);
      end if;
   end Capture;
begin
   for W in Integer_Width loop
      Width := W;
      for Test in Scenario loop
         Current := Test; Calls := 0;
         declare
            OK : Boolean;
            Loaded_Status : Load_Status;
            Result : Execution_Result;
            Name : constant String := (case Current is
              when Plain_Scalar | Quota_Scalar => "SCAL",
              when Copy_Direct => "COPY", when Copy_Argument => "CPAR",
              when Direct_Rewrite | Quota_In_Place => "RWRT", when Argument_Rewrite => "RARG",
              when Complete_Existing | Abort_Reference => "OBS0",
              when Quota_Object | Quota_Byte => "CPBF",
              when Quota_Argument_Object | Quota_Argument_Byte => "CAB0",
              when Quota_Element => "CPPK", when Quota_Argument_Element => "CAP0");
         begin
            Reset (A, OK); Check (OK); Load (A, Fixture, Width, Loaded_Status); Check (Loaded_Status = Loaded);
            Prepare_Quota;
            Active_Method := Located (Name);
            Source_For ("CONE", Constant_Source);
            Source_For ((if Current in Quota_Element | Quota_Argument_Element then "PKG0" else "BUF0"), Source);
            Before_Call := Snapshot (A);
            Invoke (A, Input, Active_Method, [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100, Result);
            if Current in Quota_Object .. Quota_Scalar then
               Check (Result.Status = Value_Limit and then Calls = 1);
               Check (Value_Store (Snapshot (A)) = Value_Store (Before_Call));
               Check (Cleanup_Frame (Snapshot (A), Before_Call));
            else
               Check (Result.Status = Returned);
               Check (Result.Value = (if Current in Direct_Rewrite | Argument_Rewrite | Quota_In_Place then 7 else 1));
               Check (Result.Origin = (if Current = Argument_Rewrite then Ordinary_Integer else AML_Constant));
            end if;
            Check (not Reservation_Matches (A, Token) and then not Matches (A, Reservation_Reference (Token)));
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PROVISIONAL-WRITE PASS" & Checks'Image);
end Provisional_Write_Tests;
