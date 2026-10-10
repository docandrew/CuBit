pragma Ada_2022;
with AML_Integers;
with AML_BCD;
with AML_Reference_Frames;
with AML_Logic;
with AML_Simple_Targets;
package body AML_Execute with SPARK_Mode is
   use AML_Decode;
   Copy_Object_Op : constant Byte := 16#9D#;
   To_Integer_Op : constant Byte := 16#99#;
   Concatenate_Resources_Op : constant Byte := 16#84#;
   Match_Op : constant Byte := 16#89#;
   To_Buffer_Op : constant Byte := 16#96#;
   Mid_Op : constant Byte := 16#9E#;
   Decimal_String_Op : constant Byte := 16#97#;
   Hexadecimal_String_Op : constant Byte := 16#98#;
   To_String_Op : constant Byte := 16#9C#;
   From_BCD_Extension : constant Byte := 16#28#;
   To_BCD_Extension : constant Byte := 16#29#;
   Concatenate_Op : constant Byte := 16#73#;
   Conditional_Reference_Op : constant Byte := 16#12#;
   use type AML_Decode.Byte;
   use type AML_Coercions.Conversion_Status;
   use type AML_References.Reference_Kind;
   use type AML_References.Reference;
   use type AML_Frame_Handles.Frame_Handle;
   function Method_Level (Flags : Byte) return Sync_Level is
     (if (Flags and 8) /= 0 then Natural (Flags / 16) else 0);
   procedure Execute_With_Input
     (Code : Bytes; Width : Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Input : aliased Read_Context; Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
   is
      package Frames is new AML_Reference_Frames
        (Datum, (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer));
      use type Frames.Result_Status;
      use type Frames.State;
      Domain : AML_Frame_Handles.Invocation_Domain;
      Issued : Invocation_Status;
      Registry : Frames.State;
      Root_Call_Budget : constant Call_Budget := Calls_Left;
      procedure Invoke_Delay (Item : AML_Delays.Request; Result : out AML_Delays.Outcome)
        with Pre => Context_Valid (Environment),
             Post => Context_Valid (Environment)
      is
      begin
         Wait_For_Delay (Environment, Item, Result);
      end Invoke_Delay;
      procedure Write_Frame_Cell
        (Frame : AML_Frame_Handles.Frame_Handle; Cell : AML_Frame_Handles.Cell_ID;
         Value : Datum; Status : out Frames.Result_Status)
        with Pre => Context_Valid (Environment) and then Frames.Valid (Registry),
          Post => Context_Valid (Environment) and then Frames.Valid (Registry)
      is
      begin
         Frames.Write_Cell (Registry, Frame, Cell, Value, Status);
         if Status = Frames.Ready then
            Publish_Frame_Root (Environment, Frame, Cell, True, Value);
         end if;
      end Write_Frame_Cell;
      procedure Write_Frame_Reference
        (Reference : AML_Frame_Handles.Cell_Handle; Value : Datum;
         Status : out Frames.Result_Status)
        with Pre => Context_Valid (Environment) and then Frames.Valid (Registry),
          Post => Context_Valid (Environment) and then Frames.Valid (Registry)
      is
      begin
         Frames.Write_Reference (Registry, Reference, Value, Status);
         if Status = Frames.Ready then
            -- A successful registry write authenticated this cell against Domain.
            Publish_Frame_Root (Environment,
              AML_Frame_Handles.Bind_Frame (Domain,
                AML_Frame_Handles.Index_Of (Reference), AML_Frame_Handles.Generation_Of (Reference)),
              AML_Frame_Handles.Cell_Of (Reference), True, Value);
         end if;
      end Write_Frame_Reference;
      procedure Run
        (Code : Bytes; Width : Integer_Width;
         Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
         Scope : Natural; Result_Out : out Execution_Result;
         Calls_Left : Call_Budget; Current_Sync : Sync_Level;
         Caller_Frame : AML_Frame_Handles.Frame_Handle)
        with Pre => Context_Valid (Environment) and then Frames.Valid (Registry)
          and then Argument_Count <= 7 and then not Result_Out'Constrained,
          Post => Context_Valid (Environment) and then Frames.Valid (Registry)
            and then Result_Out.Charged <= Budget,
          Always_Terminates,
          Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(4))
      is
      First_Argument_Cell : constant Natural :=
        AML_Frame_Handles.Cell_ID'Pos (AML_Frame_Handles.Arg_0);
      subtype Cell_Ordinal is Natural range 0 ..
        AML_Frame_Handles.Cell_ID'Pos (AML_Frame_Handles.Cell_ID'Last);
      Call_Frame : AML_Frame_Handles.Frame_Handle;
      Frame_Status : Frames.Result_Status;
      Allowed : Boolean;
      procedure Execute_Body (Body_Result : out Execution_Result)
        with Pre => Context_Valid (Environment) and then not Body_Result'Constrained,
             Post => Context_Valid (Environment) and then Body_Result.Charged <= Budget,
             Always_Terminates,
             Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(3))
      is
      subtype Failure_Status is Execution_Status range No_Return .. Execution_Status'Last;
      function Failure (Status : Failure_Status; Charged : Natural)
         return Execution_Result is ((Status => Status, Charged => Charged));
      function Cell (Slot : Cell_Ordinal) return AML_Frame_Handles.Cell_ID
      is (AML_Frame_Handles.Cell_ID'Val (Slot));
      function Slot_Value (Slot : Cell_Ordinal) return Frames.Read_Result
      is (Frames.Read_Cell (Registry, Call_Frame, Cell (Slot)));
      Offset : Natural := 0;
      Charged : Natural := 0;
      Limit : Natural := Code'Length;
      type Block_Frame is record
         Outer_Limit : Natural := 0;
         Resume : Natural := 0;
         Restart : Natural := 0;
         Is_Loop : Boolean := False;
      end record;
      Blocks : array (Positive range 1 .. 64) of Block_Frame;
      Block_Depth : Natural range 0 .. 64 := 0;
      Op : Byte;
      Value : Datum;
      State : Execution_Status;
      procedure Read_Reference_Value
        (Ref : AML_References.Reference; Value : out Datum; Status : out Execution_Status)
        with Pre => not Value'Constrained and then Context_Valid (Environment),
          Post => Context_Valid (Environment)
            and then Status in Returned | Uninitialized | Unsupported_Value
      is
      begin
         Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
         if AML_References.Kind (Ref) = AML_References.Frame_Cell then
            declare Stored : constant Frames.Read_Result := Frames.Read_Reference
              (Registry, AML_References.Frame_Item (Ref)); begin
               if Stored.Status /= Frames.Ready then Status := Unsupported_Value;
               elsif not Stored.Initialized then Status := Uninitialized;
               else Value := Stored.Value; Status := Returned; end if;
            end;
         else
            Resolve_Reference (Environment, Ref, Value, Status);
            if Status not in Returned | Uninitialized | Unsupported_Value then Status := Unsupported_Value; end if;
         end if;
      end Read_Reference_Value;
      String_Type_Code : constant Natural := 2;
      Buffer_Type_Code : constant Natural := 3;
      Package_Type_Code : constant Natural := 4;
      function Mutable_Compound (Value : Datum) return Boolean is
        (Value.Value_Kind = Object_Datum and then
         Value.Object.Type_Code in String_Type_Code | Buffer_Type_Code | Package_Type_Code);
      procedure Capture_Slot_Value
        (Item : Datum; Previous : Datum; Initialized : Boolean;
         Prepared_Copy : Boolean; Value : out Datum; Status : out Execution_Status)
      is
         Prior : Datum := Previous;
         Prior_Status : Execution_Status;
         use type AML_References.Object_Handle;
      begin
         Value := Item;
         Refresh_Value (Environment, Value, Status);
         if Status /= Returned then Status := Unsupported_Value; return; end if;
         if Prepared_Copy or else not Mutable_Compound (Value) then return; end if;
         if Initialized and then Prior.Value_Kind = Object_Datum then
            Refresh_Value (Environment, Prior, Prior_Status);
            if Prior_Status = Returned and then Prior.Value_Kind = Object_Datum
              and then Prior.Object.Source = Value.Object.Source
              and then Prior.Object.ID = Value.Object.ID
            then
               -- Authenticated same object: Store does not allocate a copy.
               return;
            end if;
         end if;
         -- Freshness/refcounts are not represented here. Capture conservatively;
         -- reference leaves retain identity in the owned transactional copier.
         declare Source : constant Datum := Value; begin
            Clone_Value (Environment, Width, Source, Value, Status);
         end;
      end Capture_Slot_Value;
      procedure Write_Reference_Value
        (Ref : AML_References.Reference; Item : Datum; Status : out Execution_Status;
         Prepared_Copy : Boolean := False;
         Mode : Reference_Store_Mode := Direct_Target)
      is
         Value : Datum := Item;
         Readback : Datum;
         Stored : Frames.Result_Status;
      begin
         Status := Unsupported_Value;
         if AML_References.Kind (Ref) = AML_References.Frame_Cell then
            Read_Reference_Value (Ref, Readback, Status);
            if Status not in Returned | Uninitialized then return; end if;
            Capture_Slot_Value
              (Item, Readback, Status = Returned, Prepared_Copy, Value, Status);
            if Status /= Returned then return; end if;
            Write_Frame_Reference (AML_References.Frame_Item (Ref), Value, Stored);
            Status := (if Stored = Frames.Ready then Returned else Unsupported_Value);
         else
            Store_Reference (Environment, Ref, Width, Item, Status, Mode);
            if Status not in Returned | Unsupported_Value | Value_Limit | Empty_Buffer then Status := Unsupported_Value; end if;
         end if;
      end Write_Reference_Value;
      procedure Assign_Slot (Target : Byte; Item : Datum)
        with Pre => Target in 16#60# .. 16#6E# and then Frames.Valid (Registry),
          Post => Frames.Valid (Registry)
      is
         Status : Frames.Result_Status;
      begin
         Write_Frame_Cell (Call_Frame, Cell (Natural (Target - 16#60#)), Item, Status);
         pragma Assert (Status = Frames.Ready);
      end Assign_Slot;
      -- All destination forms share the exact-cell argument indirection policy.
      procedure Write_Slot
        (Target : Byte; Item : Datum; Status : out Execution_Status;
         Prepared_Copy : Boolean := False)
        with Pre => Target in 16#60# .. 16#6E#
      is
         Stored : Datum := Item;
      begin
         if Target >= 16#68# then
            declare Destination : constant Frames.Read_Result := Slot_Value (Natural (Target - 16#60#)); begin
               if Destination.Initialized and then Destination.Value.Value_Kind = Reference_Datum
                 and then AML_References.Kind (Destination.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell
               then
                  Write_Reference_Value (Destination.Value.Ref, Item, Status, Prepared_Copy, Argument_Indirect_Target); return;
               end if;
            end;
         end if;
         declare Destination : constant Frames.Read_Result := Slot_Value (Natural (Target - 16#60#)); begin
            Capture_Slot_Value
              (Item, Destination.Value, Destination.Initialized, Prepared_Copy, Stored, Status);
         end;
         if Status /= Returned then return; end if;
         Assign_Slot (Target, Stored); Status := Returned;
      end Write_Slot;
      procedure Write_Target (Item : Datum; Status : out Execution_Status)
        with Pre => Offset <= Limit and then Limit <= Code'Length and then Context_Valid (Environment),
          Post => Context_Valid (Environment) and then Offset <= Limit and then Offset >= Offset'Old
            and then Status in Returned | Truncated | Unsupported | Unknown_Name | Value_Limit | Empty_Buffer
            and then (if Status /= Returned then
              Offset = Offset'Old and then Registry = Registry'Old)
            and then (if Status = Returned then Offset > Offset'Old)
      is
         Target : Byte;
         Path : AML_Names.Name_Result;
         Written_Status : Write_Status;
         use type AML_Names.Parse_Status;
      begin
         if Offset = Limit then Status := Truncated; return; end if;
         Target := Code (Code'First + Offset);
         if Target in 16#60# .. 16#6E# then
            Write_Slot (Target, Item, Status);
            if Status /= Returned then
               if Status /= Value_Limit then Status := Unsupported; end if;
               return;
            end if;
         elsif Target = Extended_Op then
            if Limit - Offset < Debug_Target_Bytes then Status := Truncated; return; end if;
            if Code (Code'First + Offset + 1) /= Debug_Extension then
               Status := Unsupported; return;
            end if;
            Observe_Debug (Environment, Scope, Offset, Width, Item);
            Offset := Offset + Debug_Target_Bytes;
            Status := Returned; return;
         elsif Target not in 0 | 1 | 16#FF# then
            if not AML_Names.Lead (Target) and then Target not in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
               Status := Unsupported; return;
            end if;
            Path := AML_Names.Read_Name
              (Code (Code'First + Offset .. Code'First + (Limit - 1)));
            if Path.Kind = AML_Names.Truncated then Status := Truncated; return; end if;
            if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
               Status := Unsupported; return;
            end if;
            Write (Environment, Scope, Path, Width, Item, Written_Status);
            case Written_Status is
               when Write_Missing => Status := Unknown_Name; return;
               when Write_Value_Limit => Status := Value_Limit; return;
               when Write_Empty_Buffer => Status := Empty_Buffer; return;
               when Write_Unsupported => Status := Unsupported; return;
               when Written => null;
            end case;
            Offset := Offset + Path.Consumed;
            Status := Returned; return;
         end if;
         Offset := Offset + 1;
         Status := Returned;
      end Write_Target;
      subtype Slot_Number is Natural range 0 .. 15;
      -- Local/Arg reads are deferred until sibling expressions complete.
      -- Named integer reads capture their value when first evaluated.
      Name_Reference_Hop_Limit : constant Positive := 64;
      procedure Resolve_Name_Value (Item : in out Datum; Status : out Execution_Status)
        with Pre => not Item'Constrained,
          Post => Status in Returned | Uninitialized | Unsupported_Value
      is
         Seen : array (Positive range 1 .. Name_Reference_Hop_Limit) of AML_References.Reference;
         Used : Natural range 0 .. Name_Reference_Hop_Limit := 0;
         Ref : AML_References.Reference;
      begin
         Status := Returned;
         while Item.Value_Kind = Reference_Datum
           and then AML_References.Kind (Item.Ref) = AML_References.Name_Member
         loop
            pragma Loop_Variant (Decreases => Name_Reference_Hop_Limit - Used);
            if Used = Name_Reference_Hop_Limit then Status := Unsupported_Value; return; end if;
            Ref := Item.Ref;
            for I in 1 .. Used loop
               if Seen (I) = Ref then Status := Unsupported_Value; return; end if;
            end loop;
            Used := Used + 1; Seen (Used) := Ref;
            Read_Reference_Value (Ref, Item, Status);
            if Status not in Returned | Uninitialized then Status := Unsupported_Value; return; end if;
            if Status /= Returned then return; end if;
         end loop;
      end Resolve_Name_Value;
      procedure Resolve_Slot (Slot : Slot_Number; Item : in out Datum; Status : out Execution_Status)
        with Pre => not Item'Constrained,
          Post => Status in Returned | Uninitialized | Missing_Argument | Unsupported_Value
      is
      begin
         Status := Returned;
         if Slot > 0 then
            declare Stored : constant Frames.Read_Result := Slot_Value (Slot - 1); begin
               if Stored.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
               if not Stored.Initialized then
                  Status := (if Slot <= 8 then Uninitialized else Missing_Argument); return;
               end if;
               Item := Stored.Value;
            end;
         end if;
         declare Refreshed : Execution_Status; begin
            Refresh_Value (Environment, Item, Refreshed);
            if Refreshed /= Returned then Status := Unsupported_Value; return; end if;
         end;
         Resolve_Name_Value (Item, Status);
      end Resolve_Slot;
      -- Root Buffer has one count; nested method calls hold their own small
      -- lexical reservation record. No per-frame count array or new registry.
      function Active_Buffer_Count (Data : Bytes) return Boolean is
         P : Package_Result;
      begin
         if Data'Length < 3 or else Data (Data'First) /= 16#11# then return False; end if;
         P := Read_Package (Data (Data'First + 1 .. Data'Last));
         if P.Kind /= Accepted or else P.Extent = P.Encoding_Bytes then return False; end if;
         if Data (Data'First + 1 + P.Encoding_Bytes) = Extended_Op then
            if P.Extent - P.Encoding_Bytes < Revision_Bytes then return False; end if;
            if Data (Data'First + (2 + P.Encoding_Bytes)) = Revision_Extension then return False; end if;
         end if;
         return Data (Data'First + 1 + P.Encoding_Bytes) not in 0 | 1 | 16#FF# | 16#0A# .. 16#0C# | 16#0E#;
      end Active_Buffer_Count;
      -- Conversion only: callers choose their own operand-reference policy.
      procedure Convert_Integer_Value (Item : in out Datum; Status : out Execution_Status)
        with Pre => Context_Valid (Environment) and then not Item'Constrained,
          Post => Context_Valid (Environment)
            and then (if Status = Returned then Item.Value_Kind = Integer_Datum)
      is
         Converted : AML_Coercions.Result;
      begin
         Refresh_Value (Environment, Item, Status);
         if Status /= Returned then return; end if;
         if Item.Value_Kind = Object_Datum then
            Converted := (if Width = Bits_32 then Item.Object.Conversion_32 else Item.Object.Conversion_64);
            if Converted.Status /= AML_Coercions.Converted then
               Status := (if Converted.Status = AML_Coercions.Empty_Buffer then Empty_Buffer else Unsupported_Value); return;
            end if;
            Item := (Value_Kind => Integer_Datum, Number => Converted.Value, Origin => Ordinary_Integer);
         end if;
         if Item.Value_Kind /= Integer_Datum then Status := Unsupported_Value; end if;
      end Convert_Integer_Value;
      procedure Resolve_Buffer_Count (Item : in out Datum; Status : out Execution_Status)
        with Pre => Context_Valid (Environment) and then not Item'Constrained,
          Post => Context_Valid (Environment)
            and then (if Status = Returned then Item.Value_Kind = Integer_Datum)
      is
         Count_Reference_Limit : constant Positive := 64;
         Seen : array (Positive range 1 .. Count_Reference_Limit) of AML_References.Reference;
         Used : Natural range 0 .. Count_Reference_Limit := 0;
      begin
         Status := Returned;
         while Item.Value_Kind = Reference_Datum loop
            if Used = Count_Reference_Limit then Status := Expression_Limit; return; end if;
            for I in 1 .. Used loop
               if Seen (I) = Item.Ref then Status := Unsupported_Value; return; end if;
            end loop;
            Used := Used + 1; Seen (Used) := Item.Ref;
            declare Ref : constant AML_References.Reference := Item.Ref; begin
               Read_Reference_Value (Ref, Item, Status);
            end;
            if Status /= Returned then return; end if;
         end loop;
         Convert_Integer_Value (Item, Status);
      end Resolve_Buffer_Count;
      procedure Evaluate_Operand (V : out Datum; S : out Execution_Status; Allow_No_Return : Boolean := False; Require_Integer : Boolean := False)
        with Always_Terminates,
             Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(1)),
             Pre => Context_Valid (Environment) and then not V'Constrained and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget,
             Post => Context_Valid (Environment) and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget
               and then Charged >= Charged'Old and then Offset >= Offset'Old
               and then (if S = Returned then Charged > Charged'Old)
               and then S /= Object_Returned and then S /= Reference_Returned
      is
         B : Byte;
         Literal : Integer_Result;
         Path : AML_Names.Name_Result;
         Bound : Binding_Result;
         use type AML_Names.Parse_Status;
         type Argument_Slots is array (Natural range 0 .. 6) of Slot_Number;
         type Target_Kind is (Discard_Target, Slot_Target, Name_Target, Reference_Target, Debug_Target);
         type Target_Descriptor is record
            Kind : Target_Kind := Discard_Target;
            Slot : Byte := 0;
            Position : Natural := 0;
            Path : AML_Names.Name_Result := (Kind => AML_Names.Truncated);
            Ref : AML_References.Reference := AML_References.No_Reference;
         end record;
         type Target_List is array (Positive range 1 .. 2) of Target_Descriptor;
         subtype Target_Number is Natural range 0 .. 2;
         type Concatenation_Provenance is (Stored_Value, Expression_Result);
         type BCD_Operation is (Not_BCD, Decode_BCD, Encode_BCD);
         type Frame is record
            Op : Byte := 16#72#;
            BCD : BCD_Operation := Not_BCD;
            Is_Conditional_Reference : Boolean := False;
            Source_Slot : Byte := 0;
            Source_Name : AML_Names.Name_Result := (Kind => AML_Names.Truncated);
            Left : Datum := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
            Left_Slot : Slot_Number := 0;
            Left_Concat, Right_Concat : Concatenation_Operand;
            Left_Provenance, Right_Provenance : Concatenation_Provenance := Stored_Value;
            Has_Left : Boolean := False;
            Has_Middle : Boolean := False;
            Has_Right : Boolean := False;
            Match_Byte_1, Match_Byte_2 : Byte := 0;
            Middle : Datum := (Integer_Datum, 0, Ordinary_Integer);
            Middle_Slot : Slot_Number := 0;
            Middle_Provenance : Concatenation_Provenance := Stored_Value;
            Collecting_Targets : Boolean := False;
            Targets_Ready : Boolean := False;
            Target_Count : Target_Number := 0;
            Target_Total : Target_Number := 0;
            Targets : Target_List := [others => <>];
            Right : Datum := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
            Right_Slot : Slot_Number := 0;
            Is_Call : Boolean := False;
            Method_ID : Natural := 0;
            Parameters : Natural range 0 .. 7 := 0;
            Given : Natural range 0 .. 7 := 0;
            Actuals : Value_Arguments := [others => (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer)];
            Slots : Argument_Slots := [others => 0];
         end record;
         procedure Collect_Targets
           (Count : in out Target_Number; Total : Target_Number;
            Targets : in out Target_List; Waiting : out Boolean;
            Status : out Execution_Status)
           with Pre => Context_Valid (Environment)
                  and then Offset <= Limit and then Limit <= Code'Length
                  and then Count <= Total,
                Post => Context_Valid (Environment)
                  and then Offset <= Limit and then Offset >= Offset'Old
                  and then Count <= Total
                  and then Status in Returned | Truncated | Unsupported
                  and then (if Waiting then Status = Returned)
         is
            B : Byte;
            T : Target_Descriptor;
         begin
            Waiting := False;
            Status := Returned;
            while Count < Total loop
               pragma Loop_Invariant (Offset <= Limit);
               pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
               pragma Loop_Invariant (Count <= Total);
               pragma Loop_Variant (Decreases => Total - Count);
               if Offset = Limit then Status := Truncated; return; end if;
               B := Code (Code'First + Offset);
               if B in 16#71# | 16#83# | 16#88# then Waiting := True; return; end if;
               T := (others => <>);
               if B in 16#60# .. 16#6E# then
                  T.Kind := Slot_Target; T.Slot := B;
                  Offset := Offset + 1;
               elsif B = Extended_Op then
                  if Limit - Offset < Debug_Target_Bytes then Status := Truncated; return; end if;
                  if Code (Code'First + Offset + 1) /= Debug_Extension then
                     Status := Unsupported; return;
                  end if;
                  T.Kind := Debug_Target; T.Position := Offset;
                  Offset := Offset + Debug_Target_Bytes;
               elsif B in 0 | 1 | 16#FF# then
                  Offset := Offset + 1;
               else
                  if not AML_Names.Lead (B) and then B not in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                     Status := Unsupported; return;
                  end if;
                  T.Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  if T.Path.Kind = AML_Names.Truncated then Status := Truncated; return; end if;
                  if T.Path.Kind /= AML_Names.Accepted or else T.Path.Count = 0 then
                     Status := Unsupported; return;
                  end if;
                  T.Kind := Name_Target;
                  Offset := Offset + T.Path.Consumed;
               end if;
               Count := Count + 1;
               Targets (Count) := T;
            end loop;
         end Collect_Targets;
         procedure Publish_Live (Extra : Expression_Values := [])
           with Pre => Extra'Length <= Value_Arguments'Length;
         procedure Apply_Target (T : Target_Descriptor; Item : Datum; Status : out Execution_Status;
                                 Mode : Reference_Store_Mode := Direct_Target)
           with Pre => Context_Valid (Environment),
                Post => Context_Valid (Environment)
                  and then Status in Returned | Unsupported | Unknown_Name | Unsupported_Value | Value_Limit | Empty_Buffer
         is
            Written_Status : Write_Status;
            Binding : Binding_Result;
         begin
            Status := Returned;
            if T.Kind = Reference_Target then Publish_Live ([Item, (Reference_Datum, T.Ref)]);
            else Publish_Live ([Item]); end if;
            case T.Kind is
               when Discard_Target => null;
               when Debug_Target =>
                  Observe_Debug (Environment, Scope, T.Position, Width, Item);
               when Slot_Target =>
                  if T.Slot not in 16#60# .. 16#6E# then Status := Unsupported; return; end if;
                  Write_Slot (T.Slot, Item, Status);
               when Name_Target =>
                  if Mode = Explicit_Result_Target then
                     Lookup (Environment, Input, Scope, T.Path, Width,
                             AML_Execute.Reference_Target, Binding);
                     if Binding.Status = Reference_Binding then
                        Write_Reference_Value (Binding.Ref, Item, Status, Mode => Mode);
                        return;
                     elsif Binding.Status = Missing_Binding then
                        Status := Unknown_Name; return;
                     elsif Binding.Status /= Integer_Binding or else Item.Value_Kind /= Integer_Datum then
                        -- Legacy integer-only adapters can update an integer;
                        -- no replacement capability is inferred for other types.
                        Status := Unsupported_Value; return;
                     end if;
                  end if;
                  Write (Environment, Scope, T.Path, Width, Item, Written_Status);
                  case Written_Status is
                     when Written => null;
                     when Write_Missing => Status := Unknown_Name;
                     when Write_Value_Limit => Status := Value_Limit;
                     when Write_Empty_Buffer => Status := Empty_Buffer;
                     when Write_Unsupported => Status := Unsupported;
                  end case;
               when Reference_Target =>
                  Write_Reference_Value (T.Ref, Item, Status, Mode => Mode);
                  if Status not in Returned | Unsupported_Value | Value_Limit | Empty_Buffer then Status := Unsupported_Value; end if;
            end case;
         end Apply_Target;
         function Root_Value (Value : Concatenation_Operand) return Datum is
           (if Value.Kind = Data_Operand then Value.Value
            else (Reference_Datum, Value.Identity));
         procedure Attach_Concatenation
           (Left, Right : Concatenation_Operand; Target : Target_Descriptor;
            Value : out Datum; Status : out Execution_Status)
         is
            Destination : Concatenation_Destination;
            Prepared : Datum;
            Cell_Reference : AML_References.Reference := AML_References.No_Reference;
            Publish_Slot : Boolean := False;
            Stored : Frames.Result_Status;
            procedure Select_Reference (Ref : AML_References.Reference; Mode : Reference_Store_Mode) is
            begin
               if AML_References.Kind (Ref) = AML_References.Frame_Cell then
                  declare Cell_Data : constant Frames.Read_Result := Frames.Read_Reference
                    (Registry, AML_References.Frame_Item (Ref)); begin
                     if Cell_Data.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                  end;
                  Cell_Reference := Ref;
                  Destination := (Kind => Prepared_Cell_Copy);
               else Destination := (Reference_Attachment, Ref, Mode); end if;
            end Select_Reference;
         begin
            Value := (Integer_Datum, 0, Ordinary_Integer);
            Status := Returned;
            case Target.Kind is
               when Discard_Target | Debug_Target => Destination := (Kind => Detached_Result);
               when Name_Target => Destination := (Named_Attachment, Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Direct_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frames.Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell
                     then Select_Reference (Previous.Value.Ref, Argument_Indirect_Target);
                     else Destination := (Kind => Detached_Result); Publish_Slot := True; end if;
                  end;
            end case;
            if Status /= Returned then return; end if;
            pragma Assert (Context_Valid (Environment));
            pragma Assert (not Value'Constrained and then not Prepared'Constrained);
            if Destination.Kind = Reference_Attachment then
               Publish_Live ([Root_Value (Left), Root_Value (Right), (Reference_Datum, Destination.Ref)]);
            else Publish_Live ([Root_Value (Left), Root_Value (Right)]); end if;
            Concatenate_And_Attach (Environment, Width, Left, Right, Destination, Value, Prepared, Status);
            if Status /= Returned then return; end if;
            if Destination.Kind = Prepared_Cell_Copy then
               Write_Frame_Reference (AML_References.Frame_Item (Cell_Reference), Prepared, Stored);
               pragma Assert (Stored = Frames.Ready);
            elsif Publish_Slot then Assign_Slot (Target.Slot, Value);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Scope, Target.Position, Width, Value);
            end if;
         end Attach_Concatenation;
         procedure Attach_Mid
           (Item : Datum; Start, Count : Integer_Value; Target : Target_Descriptor;
            Value : out Datum; Status : out Execution_Status)
         is
            Destination : Concatenation_Destination;
            Prepared : Datum;
            Cell_Reference : AML_References.Reference := AML_References.No_Reference;
            Publish_Slot : Boolean := False;
            Stored : Frames.Result_Status;
            procedure Select_Reference (Ref : AML_References.Reference; Mode : Reference_Store_Mode) is
            begin
               if AML_References.Kind (Ref) = AML_References.Frame_Cell then
                  declare Cell_Data : constant Frames.Read_Result := Frames.Read_Reference
                    (Registry, AML_References.Frame_Item (Ref)); begin
                     if Cell_Data.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                  end;
                  Cell_Reference := Ref;
                  Destination := (Kind => Prepared_Cell_Copy);
               else Destination := (Reference_Attachment, Ref, Mode); end if;
            end Select_Reference;
         begin
            Value := (Integer_Datum, 0, Ordinary_Integer);
            Status := Returned;
            case Target.Kind is
               when Discard_Target | Debug_Target => Destination := (Kind => Detached_Result);
               when Name_Target => Destination := (Named_Attachment, Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Direct_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frames.Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell
                     then Select_Reference (Previous.Value.Ref, Argument_Indirect_Target);
                     else Destination := (Kind => Detached_Result); Publish_Slot := True; end if;
                  end;
            end case;
            if Status /= Returned then return; end if;
            pragma Assert (Context_Valid (Environment));
            pragma Assert (not Value'Constrained and then not Prepared'Constrained);
            if Destination.Kind = Reference_Attachment then
               Publish_Live ([Item, (Reference_Datum, Destination.Ref)]);
            else Publish_Live ([Item]); end if;
            Mid_And_Attach (Environment, Width, Item, Start, Count, Destination, Value, Prepared, Status);
            if Status /= Returned then return; end if;
            if Destination.Kind = Prepared_Cell_Copy then
               Write_Frame_Reference (AML_References.Frame_Item (Cell_Reference), Prepared, Stored);
               pragma Assert (Stored = Frames.Ready);
            elsif Publish_Slot then Assign_Slot (Target.Slot, Value);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Scope, Target.Position, Width, Value);
            end if;
         end Attach_Mid;
         procedure Attach_Resources
           (Left, Right : Datum; Target : Target_Descriptor;
            Value : out Datum; Status : out Execution_Status)
         is
            Destination : Concatenation_Destination;
            Prepared : Datum;
            Cell_Reference : AML_References.Reference := AML_References.No_Reference;
            Publish_Slot : Boolean := False;
            Stored : Frames.Result_Status;
            procedure Select_Reference (Ref : AML_References.Reference; Mode : Reference_Store_Mode) is
            begin
               if AML_References.Kind (Ref) = AML_References.Frame_Cell then
                  declare Cell_Data : constant Frames.Read_Result := Frames.Read_Reference
                    (Registry, AML_References.Frame_Item (Ref)); begin
                     if Cell_Data.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                  end;
                  Cell_Reference := Ref;
                  Destination := (Kind => Prepared_Cell_Copy);
               else Destination := (Reference_Attachment, Ref, Mode); end if;
            end Select_Reference;
         begin
            Value := (Integer_Datum, 0, Ordinary_Integer);
            Status := Returned;
            case Target.Kind is
               when Discard_Target | Debug_Target => Destination := (Kind => Detached_Result);
               when Name_Target => Destination := (Named_Attachment, Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Direct_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frames.Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell
                     then Select_Reference (Previous.Value.Ref, Argument_Indirect_Target);
                     else Destination := (Kind => Detached_Result); Publish_Slot := True; end if;
                  end;
            end case;
            if Status /= Returned then return; end if;
            pragma Assert (Context_Valid (Environment));
            pragma Assert (not Value'Constrained and then not Prepared'Constrained);
            if Destination.Kind = Reference_Attachment then
               Publish_Live ([Left, Right, (Reference_Datum, Destination.Ref)]);
            else Publish_Live ([Left, Right]); end if;
            Concatenate_Resources_And_Attach (Environment, Width, Left, Right, Destination, Value, Prepared, Status);
            if Status /= Returned then return; end if;
            if Destination.Kind = Prepared_Cell_Copy then
               Write_Frame_Reference (AML_References.Frame_Item (Cell_Reference), Prepared, Stored);
               pragma Assert (Stored = Frames.Ready);
            elsif Publish_Slot then Assign_Slot (Target.Slot, Value);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Scope, Target.Position, Width, Value);
            end if;
         end Attach_Resources;
         procedure Attach_String
           (Item : Datum; Length : Integer_Value; Target : Target_Descriptor;
            Value : out Datum; Status : out Execution_Status)
         is
            Destination : Concatenation_Destination;
            Prepared : Datum;
            Cell_Reference : AML_References.Reference := AML_References.No_Reference;
            Publish_Slot : Boolean := False;
            Stored : Frames.Result_Status;
            procedure Select_Reference (Ref : AML_References.Reference; Mode : Reference_Store_Mode) is
            begin
               if AML_References.Kind (Ref) = AML_References.Frame_Cell then
                  declare Cell_Data : constant Frames.Read_Result := Frames.Read_Reference
                    (Registry, AML_References.Frame_Item (Ref)); begin
                     if Cell_Data.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                  end;
                  Cell_Reference := Ref;
                  Destination := (Kind => Prepared_Cell_Copy);
               else Destination := (Reference_Attachment, Ref, Mode); end if;
            end Select_Reference;
         begin
            Value := (Integer_Datum, 0, Ordinary_Integer);
            Status := Returned;
            case Target.Kind is
               when Discard_Target | Debug_Target => Destination := (Kind => Detached_Result);
               when Name_Target => Destination := (Named_Attachment, Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Explicit_Result_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frames.Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell
                     then Select_Reference (Previous.Value.Ref, Argument_Indirect_Target);
                     else Destination := (Kind => Detached_Result); Publish_Slot := True; end if;
                  end;
            end case;
            if Status /= Returned then return; end if;
            pragma Assert (Context_Valid (Environment));
            pragma Assert (not Value'Constrained and then not Prepared'Constrained);
            if Destination.Kind = Reference_Attachment then
               Publish_Live ([Item, (Reference_Datum, Destination.Ref)]);
            else Publish_Live ([Item]); end if;
            To_String_And_Attach (Environment, Width, Item, Length, Destination, Value, Prepared, Status);
            if Status /= Returned then return; end if;
            if Destination.Kind = Prepared_Cell_Copy then
               Write_Frame_Reference (AML_References.Frame_Item (Cell_Reference), Prepared, Stored);
               pragma Assert (Stored = Frames.Ready);
            elsif Publish_Slot then Assign_Slot (Target.Slot, Value);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Scope, Target.Position, Width, Value);
            end if;
         end Attach_String;
         procedure Attach_Buffer
           (Item : Datum; Target : Target_Descriptor;
            Value : out Datum; Status : out Execution_Status)
         is
            Destination : Concatenation_Destination;
            Prepared : Datum;
            Cell_Reference : AML_References.Reference := AML_References.No_Reference;
            Publish_Slot : Boolean := False;
            Stored : Frames.Result_Status;
            function Same_Buffer (Previous : Datum; Initialized : Boolean) return Boolean is
               Canonical : Datum := Item;
               Prior : Datum := Previous;
               Source_Status, Prior_Status : Execution_Status;
               use type AML_References.Object_Handle;
            begin
               if not Initialized or else Previous.Value_Kind /= Object_Datum then return False; end if;
               Refresh_Value (Environment, Canonical, Source_Status);
               if Source_Status /= Returned or else Canonical.Value_Kind /= Object_Datum
                 or else Canonical.Object.Type_Code /= Buffer_Type_Code
               then return False; end if;
               Refresh_Value (Environment, Prior, Prior_Status);
               return Prior_Status = Returned and then Prior.Value_Kind = Object_Datum
                 and then Prior.Object.Type_Code = Buffer_Type_Code
                 and then Canonical.Object.Source = Prior.Object.Source
                 and then Canonical.Object.ID = Prior.Object.ID;
            end Same_Buffer;
            procedure Select_Reference (Ref : AML_References.Reference; Mode : Reference_Store_Mode) is
            begin
               if AML_References.Kind (Ref) = AML_References.Frame_Cell then
                  declare Cell_Data : constant Frames.Read_Result := Frames.Read_Reference
                    (Registry, AML_References.Frame_Item (Ref)); begin
                     if Cell_Data.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Same_Buffer (Cell_Data.Value, Cell_Data.Initialized) then
                        Destination := (Kind => Detached_Result); return;
                     end if;
                  end;
                  Cell_Reference := Ref;
                  Destination := (Kind => Prepared_Cell_Copy);
               else Destination := (Reference_Attachment, Ref, Mode); end if;
            end Select_Reference;
         begin
            Value := (Integer_Datum, 0, Ordinary_Integer);
            Status := Returned;
            case Target.Kind is
               when Discard_Target | Debug_Target => Destination := (Kind => Detached_Result);
               when Name_Target => Destination := (Named_Attachment, Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Explicit_Result_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frames.Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell
                     then Select_Reference (Previous.Value.Ref, Argument_Indirect_Target);
                     elsif Same_Buffer (Previous.Value, Previous.Initialized) then
                        Destination := (Kind => Detached_Result);
                     else Destination := (Kind => Prepared_Cell_Copy); Publish_Slot := True; end if;
                  end;
            end case;
            if Status /= Returned then return; end if;
            pragma Assert (Context_Valid (Environment));
            pragma Assert (not Value'Constrained and then not Prepared'Constrained);
            if Destination.Kind = Reference_Attachment then
               Publish_Live ([Item, (Reference_Datum, Destination.Ref)]);
            else Publish_Live ([Item]); end if;
            To_Buffer_And_Attach (Environment, Width, Item, Destination, Value, Prepared, Status);
            if Status /= Returned then return; end if;
            if Publish_Slot then Assign_Slot (Target.Slot, Prepared);
            elsif Destination.Kind = Prepared_Cell_Copy then
               Write_Frame_Reference (AML_References.Frame_Item (Cell_Reference), Prepared, Stored);
               pragma Assert (Stored = Frames.Ready);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Scope, Target.Position, Width, Value);
            end if;
         end Attach_Buffer;
         procedure Attach_Formatted_String
           (Item : Datum; Mode : Explicit_String_Mode; Target : Target_Descriptor;
            Value : out Datum; Status : out Execution_Status)
         is
            Destination : Concatenation_Destination;
            Prepared : Datum;
            Cell_Reference : AML_References.Reference := AML_References.No_Reference;
            Publish_Slot : Boolean := False;
            Stored : Frames.Result_Status;
            function Same_String (Previous : Datum; Initialized : Boolean) return Boolean is
               Canonical : Datum := Item;
               Prior : Datum := Previous;
               Source_Status, Prior_Status : Execution_Status;
               use type AML_References.Object_Handle;
            begin
               if not Initialized or else Previous.Value_Kind /= Object_Datum then return False; end if;
               Refresh_Value (Environment, Canonical, Source_Status);
               if Source_Status /= Returned or else Canonical.Value_Kind /= Object_Datum
                 or else Canonical.Object.Type_Code /= String_Type_Code
               then return False; end if;
               Refresh_Value (Environment, Prior, Prior_Status);
               return Prior_Status = Returned and then Prior.Value_Kind = Object_Datum
                 and then Prior.Object.Type_Code = String_Type_Code
                 and then Canonical.Object.Source = Prior.Object.Source
                 and then Canonical.Object.ID = Prior.Object.ID;
            end Same_String;
            procedure Select_Reference (Ref : AML_References.Reference; Mode : Reference_Store_Mode) is
            begin
               if AML_References.Kind (Ref) = AML_References.Frame_Cell then
                  declare Cell_Data : constant Frames.Read_Result := Frames.Read_Reference
                    (Registry, AML_References.Frame_Item (Ref)); begin
                     if Cell_Data.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Same_String (Cell_Data.Value, Cell_Data.Initialized) then
                        Destination := (Kind => Detached_Result); return;
                     end if;
                  end;
                  Cell_Reference := Ref;
                  Destination := (Kind => Prepared_Cell_Copy);
               else Destination := (Reference_Attachment, Ref, Mode); end if;
            end Select_Reference;
         begin
            Value := (Integer_Datum, 0, Ordinary_Integer);
            Status := Returned;
            case Target.Kind is
               when Discard_Target | Debug_Target => Destination := (Kind => Detached_Result);
               when Name_Target => Destination := (Named_Attachment, Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Explicit_Result_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frames.Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell
                     then Select_Reference (Previous.Value.Ref, Argument_Indirect_Target);
                     elsif Same_String (Previous.Value, Previous.Initialized) then
                        Destination := (Kind => Detached_Result);
                     else Destination := (Kind => Prepared_Cell_Copy); Publish_Slot := True; end if;
                  end;
            end case;
            if Status /= Returned then return; end if;
            pragma Assert (Context_Valid (Environment));
            pragma Assert (not Value'Constrained and then not Prepared'Constrained);
            if Destination.Kind = Reference_Attachment then
               Publish_Live ([Item, (Reference_Datum, Destination.Ref)]);
            else Publish_Live ([Item]); end if;
            Format_String_And_Attach (Environment, Width, Mode, Item, Destination, Value, Prepared, Status);
            if Status /= Returned then return; end if;
            if Publish_Slot then Assign_Slot (Target.Slot, Prepared);
            elsif Destination.Kind = Prepared_Cell_Copy then
               Write_Frame_Reference (AML_References.Frame_Item (Cell_Reference), Prepared, Stored);
               pragma Assert (Stored = Frames.Ready);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Scope, Target.Position, Width, Value);
            end if;
         end Attach_Formatted_String;
         procedure Resolve_Concatenation
           (Operand : in out Concatenation_Operand; Item : Datum; Slot : Slot_Number;
            Provenance : Concatenation_Provenance; Status : out Execution_Status)
         is
            Value : Datum := Item;
         begin
            Status := Returned;
            if Operand.Kind = Namespace_Operand then return; end if;
            Resolve_Slot (Slot, Value, Status);
            if Status /= Returned then return; end if;
            if Provenance = Expression_Result and then Slot = 0 and then Value.Value_Kind = Reference_Datum
              and then AML_References.Kind (Value.Ref) = AML_References.Package_Slot
            then
               declare Ref : constant AML_References.Reference := Value.Ref; begin
                  Read_Reference_Value (Ref, Value, Status);
                  if Status /= Returned then return; end if;
               end;
            end if;
            if Value.Value_Kind = Reference_Datum
              and then AML_References.Kind (Value.Ref) = AML_References.Frame_Cell
            then
               declare Cell_Data : constant Frames.Read_Result := Frames.Read_Reference
                 (Registry, AML_References.Frame_Item (Value.Ref)); begin
                  if Cell_Data.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
               end;
            end if;
            Operand := (Data_Operand, Value);
         end Resolve_Concatenation;
         procedure Resolve_Mid_Operand
           (Item : in out Datum; Slot : Slot_Number;
            Provenance : Concatenation_Provenance; Status : out Execution_Status)
         is
         begin
            Resolve_Slot (Slot, Item, Status);
            if Status /= Returned then return; end if;
            -- ACPICA resolves an expression-result package Index once. A
            -- reference loaded from a frame cell remains a reference operand.
            if Provenance = Expression_Result and then Slot = 0
              and then Item.Value_Kind = Reference_Datum
              and then AML_References.Kind (Item.Ref) = AML_References.Package_Slot
            then
               declare Ref : constant AML_References.Reference := Item.Ref; begin
                  Read_Reference_Value (Ref, Item, Status);
               end;
            end if;
         end Resolve_Mid_Operand;
         procedure Complete_Update
           (Target : Target_Descriptor; Op : Byte; Value : out Datum;
            Status : out Execution_Status)
           with Pre => Context_Valid (Environment) and then not Value'Constrained
             and then Op in Increment_Op | Decrement_Op,
             Post => Context_Valid (Environment)
               and then (if Status = Returned then Value.Value_Kind = Integer_Datum)
         is
            Binding : Binding_Result;
         begin
            Value := (Value_Kind => Integer_Datum, Number => 0, Origin => Ordinary_Integer);
            -- Keep the original destination while reading/coercing its value.
            -- These reads never broaden the owner's reference authority.
            case Target.Kind is
               when Slot_Target =>
                  Resolve_Slot (Natural (Target.Slot - 16#60#) + 1, Value, Status);
               when Name_Target =>
                  Lookup (Environment, Input, Scope, Target.Path, Width,
                          AML_Execute.Reference_Target, Binding);
                  case Binding.Status is
                     when Reference_Binding => Read_Reference_Value (Binding.Ref, Value, Status);
                     when Missing_Binding => Status := Unknown_Name;
                     when Failed_Binding => Status := Binding.Failure;
                     when others => Status := Unsupported_Value;
                  end case;
               when Reference_Target => Read_Reference_Value (Target.Ref, Value, Status);
               when Discard_Target | Debug_Target => Status := Unsupported_Value;
            end case;
            if Status /= Returned then return; end if;
            -- ACPICA resolves the original target once. A reference-valued
            -- cell is not another target to follow for Increment/Decrement.
            if Value.Value_Kind = Reference_Datum then Status := Unsupported_Value; return; end if;
            Convert_Integer_Value (Value, Status);
            if Status /= Returned then return; end if;
            Value.Number := AML_Integers.Normalize
              ((if Op = Increment_Op then Value.Number + 1 else Value.Number - 1), Width);
            Value.Origin := Ordinary_Integer;
            Apply_Target (Target, Value, Status);
         end Complete_Update;
         procedure Complete_Arithmetic
           (F : Frame; Left, Right : Integer_Value;
            Value : out Datum; Status : out Execution_Status)
           with Pre => Context_Valid (Environment)
             and then AML_Integers.Supported (F.Op) and then not Value'Constrained,
             Post => Context_Valid (Environment)
               and then Value.Value_Kind = Integer_Datum
               and then Status in Returned | Unsupported | Unknown_Name |
                 Unsupported_Value | Value_Limit | Division_By_Zero
               and then (if F.Op in 16#78# | 16#85# and then Right = 0 then
                 Status = Division_By_Zero and then Value.Number = 0
               else Status /= Division_By_Zero and then
                 Value.Number = AML_Integers.Apply (F.Op, Left, Right, Width))
         is
            Remainder_Value : Integer_Value := 0;
         begin
            Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
            Status := Returned;
            if F.Op in 16#78# | 16#85# and then Right = 0 then
               Status := Division_By_Zero; return;
            end if;
            if F.Op = 16#78# then Remainder_Value := Left mod Right; end if;
            Value := (Value_Kind => Integer_Datum,
              Number => AML_Integers.Apply (F.Op, Left, Right, Width), Origin => AML_Decode.Ordinary_Integer);
            -- Target expressions have already completed. Divide stores the
            -- remainder before the quotient, stopping on the first failure.
            if F.Target_Total >= 1 then
               Apply_Target (F.Targets (1),
                 (if F.Op = 16#78# then
                    (Value_Kind => Integer_Datum, Number => Remainder_Value, Origin => AML_Decode.Ordinary_Integer)
                  else Value), Status);
               if Status /= Returned then return; end if;
            end if;
            if F.Target_Total = 2 then
               Apply_Target (F.Targets (2), Value, Status);
            end if;
         end Complete_Arithmetic;
         procedure Complete_Numeric
           (F : Frame; Right_Slot : Slot_Number;
            Value : in out Datum; Status : out Execution_Status)
           with Pre => Context_Valid (Environment) and then not Value'Constrained,
             Post => Context_Valid (Environment)
               and then Status /= Object_Returned and then Status /= Reference_Returned
               and then (if Status = Returned then Value.Value_Kind = Integer_Datum)
         is
            Left_Item : Datum;
            Conversion : AML_Coercions.Result;
            Comparison : Integer_Value;
         begin
            Left_Item := F.Left;
            Resolve_Slot (F.Left_Slot, Left_Item, Status);
            if Status /= Returned then return; end if;
            Resolve_Slot (Right_Slot, Value, Status);
            if Status /= Returned then return; end if;
            if Value.Value_Kind = Reference_Datum or else Left_Item.Value_Kind = Reference_Datum then
               Status := Unsupported_Value; return;
            end if;
            if Left_Item.Value_Kind = Object_Datum then
               if F.Op in 16#93# .. 16#95# then
                  Compare_Objects (Environment, F.Op, Left_Item, Value, Width, Comparison, Status);
                  if Status = Returned then Value := (Integer_Datum, Comparison, AML_Decode.Ordinary_Integer);
                  else Status := Unsupported_Value; end if;
                  return;
               end if;
               Conversion := (if Width = Bits_32 then Left_Item.Object.Conversion_32 else Left_Item.Object.Conversion_64);
               case Conversion.Status is
                  when AML_Coercions.Converted => Left_Item := (Value_Kind => Integer_Datum, Number => Conversion.Value, Origin => AML_Decode.Ordinary_Integer);
                  when AML_Coercions.Empty_Buffer => Status := Empty_Buffer; return;
                  when AML_Coercions.Not_Convertible => Status := Unsupported_Value; return;
               end case;
            end if;
            if Value.Value_Kind = Object_Datum then
               Conversion := (if Width = Bits_32 then Value.Object.Conversion_32 else Value.Object.Conversion_64);
               case Conversion.Status is
                  when AML_Coercions.Converted => Value := (Value_Kind => Integer_Datum, Number => Conversion.Value, Origin => AML_Decode.Ordinary_Integer);
                  when AML_Coercions.Empty_Buffer => Status := Empty_Buffer; return;
                  when AML_Coercions.Not_Convertible => Status := Unsupported_Value; return;
               end case;
            end if;
            if Left_Item.Value_Kind /= Integer_Datum or else Value.Value_Kind /= Integer_Datum then
               Status := Unsupported_Value; return;
            end if;
            if AML_Logic.Supported (F.Op) then
               Value := (Value_Kind => Integer_Datum, Number => AML_Logic.Apply (F.Op, Left_Item.Number, Value.Number, Width), Origin => AML_Decode.Ordinary_Integer);
            else
               if not AML_Integers.Supported (F.Op) then
                  Status := Unsupported; return;
               end if;
               declare
                  Left_Number : constant Integer_Value := Left_Item.Number;
                  Right_Number : constant Integer_Value := Value.Number;
               begin
                  Complete_Arithmetic (F, Left_Number, Right_Number, Value, Status);
               end;
            end if;
         end Complete_Numeric;
         procedure Advance_Numeric
           (F : in out Frame; Right_Slot : Slot_Number;
            Value : in out Datum; Waiting : out Boolean;
            Status : out Execution_Status)
           with Pre => Context_Valid (Environment) and then not Value'Constrained
               and then Offset <= Limit and then Limit <= Code'Length,
             Post => Context_Valid (Environment)
               and then Offset <= Limit and then Offset >= Offset'Old
               and then Status /= Object_Returned and then Status /= Reference_Returned
               and then (if Waiting then Status = Returned)
               and then (if not Waiting and then Status = Returned then
                 Value.Value_Kind = Integer_Datum)
         is
         begin
            Waiting := False;
            Status := Returned;
            if not (AML_Logic.Unary (F.Op) or AML_Integers.Unary (F.Op))
              and then not F.Has_Left
            then
               F.Left := Value;
               F.Left_Slot := Right_Slot;
               F.Has_Left := True;
               Waiting := True;
               return;
            end if;
            if AML_Integers.Supported (F.Op) and then not F.Targets_Ready then
               F.Right := Value;
               F.Right_Slot := Right_Slot;
               F.Target_Count := 0;
               F.Target_Total := (if F.Op = 16#78# then 2 else 1);
               F.Collecting_Targets := True;
               Collect_Targets
                 (F.Target_Count, F.Target_Total, F.Targets, Waiting, Status);
               if Status /= Returned or else Waiting then return; end if;
               F.Collecting_Targets := False;
               F.Targets_Ready := True;
            end if;
            Complete_Numeric (F, Right_Slot, Value, Status);
         end Advance_Numeric;
         Max_Expression_Depth : constant := 64;
         Stack : array (Positive range 1 .. Max_Expression_Depth) of Frame;
         Depth : Natural range 0 .. Max_Expression_Depth := 0;
         Have_Value : Boolean;
         Waiting_For_Target : Boolean;
         Pending_Slot : Slot_Number;
         Pending_Concat : Concatenation_Operand;
         Pending_Provenance : Concatenation_Provenance := Stored_Value;
         Left_Item, Argument_Item : Datum;
         Conversion : AML_Coercions.Result;
         Text : String_Result;
         Buffer_Item : Buffer_Result;
         Package_Item : Package_Result;
         function Comparison_Expects_Integer return Boolean is
            Slot : constant Slot_Number := Stack (Depth).Left_Slot;
         begin
            if Slot = 0 then return Stack (Depth).Left.Value_Kind = Integer_Datum;
            elsif Slot <= 8 then
               return Slot_Value (Slot - 1).Initialized and then Slot_Value (Slot - 1).Value.Value_Kind = Integer_Datum;
            else
               return Slot_Value (Slot - 1).Initialized and then Slot_Value (Slot - 1).Value.Value_Kind = Integer_Datum;
            end if;
         end Comparison_Expects_Integer;
         function Integer_Expected return Boolean is
           ((Depth = 0 and Require_Integer) or else
            (Depth > 0 and then not Stack (Depth).Is_Call and then
             -- BCD defers conversion until its target expressions complete.
             (Stack (Depth).BCD = Not_BCD and then
              (AML_Integers.Supported (Stack (Depth).Op)
              or else Stack (Depth).Op in 16#90# .. 16#92#
              or else (Stack (Depth).Op = 16#88# and then Stack (Depth).Has_Left)
              or else (AML_Logic.Supported (Stack (Depth).Op) and then Stack (Depth).Has_Left
                and then Comparison_Expects_Integer)))));
         Reference_Hop_Limit : constant Positive := 64;
         procedure Describe (Item : Datum; Type_Code, Size : out Natural; Status : out Execution_Status)
         is
            Current : Datum := Item;
            Seen : array (Positive range 1 .. Reference_Hop_Limit) of AML_References.Reference;
            Used : Natural range 0 .. Reference_Hop_Limit := 0;
         begin
            Type_Code := 0; Size := 0; Status := Returned;
            while Current.Value_Kind = Reference_Datum loop
               if Used = Reference_Hop_Limit then Status := Expression_Limit; return; end if;
               for I in 1 .. Used loop
                  if Seen (I) = Current.Ref then Status := Unsupported_Value; return; end if;
               end loop;
               Used := Used + 1; Seen (Used) := Current.Ref;
               declare
                  Metadata : Reference_Metadata;
               begin
                  pragma Assert (Context_Valid (Environment) and then not Metadata'Constrained);
                  Describe_Named_Identity (Environment, Current.Ref, Metadata);
                  case Metadata.Kind is
                     when Invalid_Reference => Status := Unsupported_Value; return;
                     when Metadata_Only =>
                        Type_Code := Named_Metadata_Type'Enum_Rep (Metadata.Object_Type); return;
                     when Continue_Reference => null;
                  end case;
               end;
               declare Ref : constant AML_References.Reference := Current.Ref; begin
                  Read_Reference_Value (Ref, Current, Status);
                  if Status = Uninitialized then Status := Returned; return; end if;
                  if Status /= Returned then return; end if;
                  if AML_References.Kind (Ref) = AML_References.Byte_Slot then
                     Type_Code := 14; return; -- Index denotes BufferField, not its integer value.
                  end if;
               end;
            end loop;
            Refresh_Value (Environment, Current, Status);
            if Status /= Returned then Status := Unsupported_Value; return; end if;
            case Current.Value_Kind is
               when Integer_Datum => Type_Code := 1;
               when Object_Datum => Type_Code := Current.Object.Type_Code; Size := Current.Object.Size;
               when Reference_Datum => Status := Unsupported_Value;
            end case;
         end Describe;
         procedure Query_Value (Query : Byte; Type_Code, Size : Natural;
                                Value : out Integer_Value; Status : out Execution_Status) is
         begin
            Value := 0; Status := Returned;
            if Query = 16#8E# then Value := Integer_Value (Type_Code);
            elsif Type_Code = 1 then Value := (if Width = Bits_32 then 4 else 8);
            elsif Type_Code in 2 .. 4 then Value := Integer_Value (Size);
            else Status := (if Type_Code = 0 then Uninitialized else Unsupported_Value); end if;
         end Query_Value;
         procedure Inspect (Query : Byte; Value : out Integer_Value; Status : out Execution_Status)
           with Pre => Context_Valid (Environment) and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget,
                Post => Context_Valid (Environment) and then Offset <= Limit and then Charged <= Budget
                  and then Offset >= Offset'Old and then Charged >= Charged'Old
                  and then Status /= Object_Returned and then Status /= Reference_Returned
         is
            Source : Byte;
            Type_Code : Natural;
            Size : Natural := 0;
            Name : AML_Names.Name_Result;
            Object : Binding_Result;
            Current : Datum;
         begin
            Value := 0; Status := Returned;
            if Charged = Budget then Status := Budget_Exceeded; return; end if;
            Charged := Charged + 1;
            if Offset = Limit then Status := Truncated; return; end if;
            Source := Code (Code'First + Offset);
            if Source in 16#60# .. 16#67# then
               Offset := Offset + 1;
               if Slot_Value (Natural (Source - 16#60#)).Initialized then
                  Current := Slot_Value (Natural (Source - 16#60#)).Value;
                  Refresh_Value (Environment, Current, Status);
                  if Status /= Returned then Status := Unsupported_Value; return; end if;
                  Describe (Current, Type_Code, Size, Status);
                  if Status /= Returned then return; end if;
                  if Query = 16#87# and then Type_Code = 0 and then Current.Value_Kind = Reference_Datum then
                     Status := Unsupported_Value; return;
                  end if;
               else Type_Code := 0; end if;
            elsif Source in 16#68# .. 16#6E# then
               Offset := Offset + 1;
               if Slot_Value (Natural (Source - 16#60#)).Initialized then
                  Current := Slot_Value (Natural (Source - 16#60#)).Value;
                  Refresh_Value (Environment, Current, Status);
                  if Status /= Returned then Status := Unsupported_Value; return; end if;
                  Describe (Current, Type_Code, Size, Status);
                  if Status /= Returned then return; end if;
                  if Query = 16#87# and then Type_Code = 0 and then Current.Value_Kind = Reference_Datum then
                     Status := Unsupported_Value; return;
                  end if;
               else Type_Code := 0; end if;
            elsif Source = 16#5B# then
               Offset := Offset + 1;
               if Offset = Limit then Status := Truncated; return; end if;
               if Code (Code'First + Offset) /= 16#31# then Status := Unsupported; return; end if;
               Offset := Offset + 1;
               Type_Code := 16; -- Debug object, no debug output or evaluation.
            elsif AML_Names.Lead (Source) or else Source in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
               Name := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
               if Name.Kind /= AML_Names.Accepted then
                  Status := (if Name.Kind = AML_Names.Truncated then Truncated else Bad_Name);
                  return;
               end if;
               Offset := Offset + Name.Consumed;
               Lookup (Environment, Input, Scope, Name, Width, Inspect_Binding, Object);
               case Object.Status is
                  when Reference_Binding =>
                     Describe ((Value_Kind => Reference_Datum, Ref => Object.Ref), Type_Code, Size, Status);
                     if Status /= Returned then return; end if;
                     if Query = 16#87# and then Type_Code = 0 then Status := Unsupported_Value; return; end if;
                  when Failed_Binding => Status := (if Object.Failure in No_Return .. Execution_Status'Last then Object.Failure else Unsupported_Value); return;
                  when Missing_Binding => Status := Unknown_Name; return;
                  when Integer_Binding => Type_Code := 1;
                  when Method_Binding => Type_Code := 8;
                  when Non_Integer_Binding => Type_Code := Object.Object.Type_Code; Size := Object.Object.Size;
               end case;
            else
               Status := Unsupported; return;
            end if;
            Query_Value (Query, Type_Code, Size, Value, Status);
         end Inspect;
         procedure Publish_Live (Extra : Expression_Values := []) is
            Saved_Operands_Per_Frame : constant := 3;
            Concat_Operands_Per_Frame : constant := 2;
            Roots_Per_Frame : constant := Saved_Operands_Per_Frame + Concat_Operands_Per_Frame
              + Value_Arguments'Length + Target_List'Length;
            Max_Root_Values : constant := Max_Expression_Depth * Roots_Per_Frame + Value_Arguments'Length;
            Values : Expression_Values (1 .. Max_Root_Values);
            Used : Natural range 0 .. Max_Root_Values := 0;
            procedure Add (Value : Datum) is
            begin
               if Used = Max_Root_Values then raise Program_Error with "expression root capacity invariant"; end if;
               Used := Used + 1; Values (Used) := Value;
            end Add;
            procedure Add_Concat (Value : Concatenation_Operand) is
            begin
               if Value.Kind = Namespace_Operand then Add ((Reference_Datum, Value.Identity));
               else Add (Value.Value); end if;
            end Add_Concat;
         begin
            for Level in 1 .. Depth loop
               if Stack (Level).Is_Call then
                  for I in 1 .. Stack (Level).Given loop Add (Stack (Level).Actuals (I - 1)); end loop;
               else
                  if Stack (Level).Has_Left then
                     Add (Stack (Level).Left);
                     if Stack (Level).Op = Concatenate_Op then Add_Concat (Stack (Level).Left_Concat); end if;
                  end if;
                  if Stack (Level).Has_Middle then Add (Stack (Level).Middle); end if;
                  if Stack (Level).Has_Right or else Stack (Level).Collecting_Targets or else Stack (Level).Targets_Ready then
                     Add (Stack (Level).Right);
                     if Stack (Level).Op = Concatenate_Op then Add_Concat (Stack (Level).Right_Concat); end if;
                  end if;
                  for I in 1 .. Stack (Level).Target_Count loop
                     if Stack (Level).Targets (I).Kind = Reference_Target then
                        Add ((Reference_Datum, Stack (Level).Targets (I).Ref));
                     end if;
                  end loop;
               end if;
            end loop;
            for Value of Extra loop Add (Value); end loop;
            Publish_Expression_Roots (Environment, Call_Frame, Values (1 .. Used));
         end Publish_Live;
         procedure Dispatch
           (ID : Natural; Actuals : Value_Arguments; Count : Natural;
            Need_Value : Boolean; Value : out Datum;
            Status : out Execution_Status)
           with Always_Terminates,
                Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(0)),
                Pre => Context_Valid (Environment) and then not Value'Constrained and then Count <= 7 and then Charged > 0 and then Charged <= Budget,
                Post => Context_Valid (Environment) and then Charged <= Budget and then Charged >= Charged'Old
                  and then Status /= Object_Returned and then Status /= Reference_Returned
         is
            Result : Execution_Result;
         begin
            Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
            if Calls_Left = 0 then Status := Call_Limit; return; end if;
            declare
            Method : constant Method_Definition := Get_Method (Environment, ID);
            begin
            if not Method.Exists then Status := Invalid_Method; return; end if;
            if Natural (Method.Flags and 7) /= Count then
               Status := Argument_Mismatch; return;
            end if;
            if (Method.Flags and 8) /= 0 and then Current_Sync > Method_Level (Method.Flags) then
               Status := Mutex_Order; return;
            end if;
            Publish_Live ([for I in 1 .. Count => Actuals (I - 1)]);
            Run
              (Method.Code (1 .. Method.Length), Method.Width, Actuals, Count,
               Budget - Charged, Method.Scope, Result, Calls_Left - 1,
               (if (Method.Flags and 8) /= 0 then Method_Level (Method.Flags) else Current_Sync), Call_Frame);
            -- Compound results have already crossed the return-root bridge.
            -- No collecting callback occurs between return and this clear.
            Publish_Expression_Roots (Environment, Call_Frame, []);
            Charged := Charged + Result.Charged;
            Status := Result.Status;
            if Result.Status = Returned then
               Value := (Value_Kind => Integer_Datum, Number => Result.Value, Origin => Result.Origin);
            elsif Result.Status = Object_Returned then
               Value := (Value_Kind => Object_Datum, Object => Result.Object);
               Status := Returned;
            elsif Result.Status = Reference_Returned then
               Value := (Value_Kind => Reference_Datum, Ref => Result.Ref);
               Status := Returned;
            elsif Result.Status = No_Return and Need_Value then
               Status := Missing_Result;
            end if;
            end;
         end Dispatch;
      begin
         V := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
         S := Returned;
         loop
            pragma Loop_Invariant (Context_Valid (Environment));
            pragma Loop_Invariant (S /= Object_Returned and then S /= Reference_Returned);
            pragma Loop_Invariant (Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget);
            pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
            pragma Loop_Invariant (Charged >= Charged'Loop_Entry);
            pragma Loop_Variant (Decreases => Budget - Charged);
            if Charged = Budget then S := Budget_Exceeded; return; end if;
            Charged := Charged + 1;
            if Offset = Limit then S := Truncated; return; end if;
            B := Code (Code'First + Offset);
            Have_Value := True;
            Pending_Slot := 0;
            Pending_Concat := (Kind => Data_Operand, Value => <>);
            Pending_Provenance := Stored_Value;
            if B in 16#70# | 16#83# | 16#88# | Copy_Object_Op | To_Integer_Op | To_Buffer_Op | Mid_Op | To_String_Op | Decimal_String_Op | Hexadecimal_String_Op | Match_Op | Concatenate_Resources_Op | Concatenate_Op or else AML_Integers.Supported (B) or else AML_Logic.Supported (B)
              or else (B in 16#87# | 16#8E# and then Limit - Offset > 1
                and then Code (Code'First + Offset + 1) in 16#71# | 16#83# | 16#88#)
            then
               if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
               Depth := Depth + 1;
               Stack (Depth) := (Op => B, others => <>);
               Offset := Offset + 1;
            else
               if B in Increment_Op | Decrement_Op then
                  Offset := Offset + 1;
                  if Offset = Limit then S := Truncated; return; end if;
                  if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
                  Depth := Depth + 1;
                  Stack (Depth) := (Op => B, Target_Total => 1, others => <>);
                  -- A simple target is one operand. Complex reference targets
                  -- are charged by their own single evaluation below.
                  if Code (Code'First + Offset) not in 16#71# | 16#83# | 16#88# then
                     if Charged = Budget then S := Budget_Exceeded; return; end if;
                     Charged := Charged + 1;
                  end if;
                  Collect_Targets (Stack (Depth).Target_Count, 1,
                    Stack (Depth).Targets, Waiting_For_Target, S);
                  if S /= Returned then return; end if;
                  Stack (Depth).Collecting_Targets := Waiting_For_Target;
                  Stack (Depth).Targets_Ready := not Waiting_For_Target;
                  Have_Value := not Waiting_For_Target;
               elsif B = 16#71# then
                  Offset := Offset + 1;
                  if Offset = Limit then S := Truncated; return; end if;
                  if Charged = Budget then S := Budget_Exceeded; return; end if;
                  Charged := Charged + 1;
                  declare Target : constant Byte := Code (Code'First + Offset);
                     Ref : AML_References.Reference;
                     Cell_Ref : AML_Frame_Handles.Cell_Handle;
                     Made : Frames.Result_Status;
                  begin
                     if Target in 16#60# .. 16#6E# then
                        Frames.Make_Reference (Registry, Call_Frame, Cell (Natural (Target - 16#60#)), Cell_Ref, Made);
                        if Made /= Frames.Ready then S := Unsupported_Value; return; end if;
                        Ref := AML_References.Bind_Frame (Cell_Ref); Offset := Offset + 1;
                     elsif AML_Names.Lead (Target) or else Target in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                        Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                        if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                           S := (if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name); return;
                        end if;
                        Lookup (Environment, Input, Scope, Path, Width, AML_Execute.Namespace_Identity, Bound);
                        if Bound.Status = Missing_Binding then S := Unknown_Name; return;
                        elsif Bound.Status /= Reference_Binding then S := Unsupported_Value; return; end if;
                        Ref := Bound.Ref; Offset := Offset + Path.Consumed;
                     else S := Unsupported_Value; return; end if;
                     V := (Value_Kind => Reference_Datum, Ref => Ref);
                  end;
               elsif B = 16#5B# then
                  if Limit - Offset < 2 then S := Truncated; return; end if;
                  if Code (Code'First + Offset + 1) = Revision_Extension then
                     Offset := Offset + Revision_Bytes;
                     V := (Value_Kind => Integer_Datum, Number => Interpreter_Revision,
                           Origin => Ordinary_Integer);
                  elsif Code (Code'First + Offset + 1) in From_BCD_Extension | To_BCD_Extension then
                     if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
                     Depth := Depth + 1;
                     Stack (Depth) :=
                       (Op => 0,
                        BCD => (if Code (Code'First + Offset + 1) = From_BCD_Extension
                                then Decode_BCD else Encode_BCD), others => <>);
                     Offset := Offset + 2;
                     Have_Value := False;
                  elsif Code (Code'First + Offset + 1) = Conditional_Reference_Op then
                     Offset := Offset + 2;
                     if Offset = Limit then S := Truncated; return; end if;
                     if Charged = Budget then S := Budget_Exceeded; return; end if;
                     Charged := Charged + 1;
                     if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
                     Depth := Depth + 1;
                     Stack (Depth) := (Is_Conditional_Reference => True, Op => 0,
                       Target_Total => 1, others => <>);
                     declare Source : constant Byte := Code (Code'First + Offset); begin
                        if Source in 16#60# .. 16#6E# then
                           Stack (Depth).Source_Slot := Source; Offset := Offset + 1;
                        elsif AML_Names.Lead (Source) or else Source in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                           Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                           if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                              S := (if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name); return;
                           end if;
                           Stack (Depth).Source_Name := Path; Offset := Offset + Path.Consumed;
                        else S := Unsupported_Value; return; end if;
                     end;
                     Collect_Targets (Stack (Depth).Target_Count, 1,
                       Stack (Depth).Targets, Waiting_For_Target, S);
                     if S /= Returned then return; end if;
                     Stack (Depth).Collecting_Targets := Waiting_For_Target;
                     Stack (Depth).Targets_Ready := not Waiting_For_Target;
                     Have_Value := not Waiting_For_Target;
                     V := (Value_Kind => Integer_Datum, Number => 0, Origin => Ordinary_Integer);
                  else
                  if Code (Code'First + Offset + 1) /= 16#33# then
                     S := Unsupported; return;
                  end if;
                  Offset := Offset + 2;
                  declare
                     Reading : Integer_Value;
                     Available : Boolean;
                  begin
                     Read_Timer (Environment, Reading, Available);
                     if not Available then S := Unsupported; return; end if;
                     V := (Value_Kind => Integer_Datum,
                           Number => AML_Integers.Normalize (Reading, Width), Origin => AML_Decode.Ordinary_Integer);
                  end;
                  end if;
               elsif B in 16#87# | 16#8E# then
                  Offset := Offset + 1;
                  declare
                     N : Integer_Value;
                  begin
                     Inspect (B, N, S);
                     V := (Value_Kind => Integer_Datum, Number => N, Origin => AML_Decode.Ordinary_Integer);
                  end;
                  if S /= Returned then return; end if;
               elsif B in 16#60# .. 16#6E# then
                  Offset := Offset + 1;
                  Pending_Slot := Natural (B - 16#5F#);
                  V := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
               elsif B in 16#0D# | 16#11# and then Integer_Expected then
                  if B = 16#0D# then
                     Text := Read_String (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                     if Text.Kind /= Accepted then
                        S := (if Text.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Offset := Offset + Text.Consumed;
                     declare
                        L : constant Natural := Text.Length;
                        Data : Bytes (1 .. L);
                     begin
                        for I in Data'Range loop Data (I) := Character'Pos (Text.Text (I)); end loop;
                        Conversion := AML_Coercions.From_String (Data, Width);
                     end;
                  else
                     Buffer_Item := Read_Buffer (Code (Code'First + Offset .. Code'First + (Limit - 1)), Width);
                     if Buffer_Item.Kind /= Accepted then
                        S := (if Buffer_Item.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Offset := Offset + Buffer_Item.Consumed;
                     Conversion := AML_Coercions.From_Buffer
                       (Buffer_Item.Content (1 .. Buffer_Item.Length), Width);
                  end if;
                  if Conversion.Status /= AML_Coercions.Converted then S := Empty_Buffer; return; end if;
                  V := (Value_Kind => Integer_Datum, Number => Conversion.Value, Origin => AML_Decode.Ordinary_Integer);
               elsif B in 16#12# | 16#13# then
                  if Limit - Offset <= 1 then S := Truncated; return; end if;
                  Package_Item := Read_Package (Code (Code'First + Offset + 1 .. Code'First + (Limit - 1)));
                  if Package_Item.Kind /= Accepted then S := Bad_Package; return; end if;
                  Publish_Live;
                  Materialize (Environment, Scope, Package_Literal, Width,
                    Code (Code'First + Offset .. Code'First + Offset + Package_Item.Extent), Bound);
                  Offset := Offset + 1 + Package_Item.Extent;
                  if Bound.Status = Failed_Binding then
                     S := (if Bound.Failure in No_Return .. Execution_Status'Last then Bound.Failure else Unsupported_Value); return;
                  elsif Bound.Status /= Non_Integer_Binding or else Bound.Object.ID = 0
                    or else Bound.Object.Type_Code /= 4 then S := Unsupported_Value; return;
                  end if;
                  V := (Value_Kind => Object_Datum, Object => Bound.Object);
               elsif B in 16#0D# | 16#11# then
                  if Allow_No_Return and Depth = 0 then S := Unsupported; return; end if;
                  if B = 16#0D# then
                     Text := Read_String (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                     if Text.Kind /= Accepted then
                        S := (if Text.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Offset := Offset + Text.Consumed;
                     declare
                        L : constant Natural := Text.Length;
                        Data : Bytes (1 .. L);
                     begin
                        for I in Data'Range loop Data (I) := Character'Pos (Text.Text (I)); end loop;
                        Publish_Live;
                        Materialize (Environment, Scope, String_Literal, Width, Data, Bound);
                     end;
                  else
                     Buffer_Item := Read_Buffer (Code (Code'First + Offset .. Code'First + (Limit - 1)), Width);
                     if Buffer_Item.Kind /= Accepted then
                        S := (if Buffer_Item.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Offset := Offset + Buffer_Item.Consumed;
                     Publish_Live;
                     Materialize (Environment, Scope, Buffer_Literal, Width,
                       Buffer_Item.Content (1 .. Buffer_Item.Length), Bound);
                  end if;
                  if Bound.Status = Failed_Binding then
                     S := (if Bound.Failure in No_Return .. Execution_Status'Last then Bound.Failure else Unsupported_Value);
                     return;
                  elsif Bound.Status /= Non_Integer_Binding or else Bound.Object.ID = 0
                    or else Bound.Object.Type_Code /= (if B = 16#0D# then 2 else 3)
                  then S := Unsupported_Value; return; end if;
                  V := (Value_Kind => Object_Datum, Object => Bound.Object);
               elsif AML_Names.Lead (B) or else B in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                  Path := AML_Names.Read_Name
                    (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  if Path.Kind /= AML_Names.Accepted then
                     S := (if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name);
                     return;
                  end if;
                  Offset := Offset + Path.Consumed;
                  if Depth > 0 and then Stack (Depth).Op = Concatenate_Op
                    and then not Stack (Depth).Collecting_Targets
                  then
                     declare Identity : Binding_Result; Metadata : Reference_Metadata; begin
                        Lookup (Environment, Input, Scope, Path, Width, Namespace_Identity, Identity);
                        if Identity.Status = Reference_Binding then
                           Describe_Named_Identity (Environment, Identity.Ref, Metadata);
                           if Metadata.Kind = Metadata_Only
                             and then Metadata.Object_Type in Device_Metadata | Region_Metadata | Event_Metadata | Mutex_Metadata | Power_Metadata | Processor_Metadata | Thermal_Metadata
                           then
                              Pending_Concat := (Namespace_Operand, Identity.Ref,
                                (case Metadata.Object_Type is
                                   when Device_Metadata => Device_Descriptor,
                                   when Region_Metadata => Region_Descriptor,
                                   when Event_Metadata => Event_Descriptor,
                                   when Mutex_Metadata => Mutex_Descriptor,
                                   when Power_Metadata => Power_Descriptor,
                                   when Processor_Metadata => Processor_Descriptor,
                                   when Thermal_Metadata => Thermal_Descriptor,
                                   when others => Device_Descriptor));
                           end if;
                        end if;
                     end;
                  end if;
                  if Pending_Concat.Kind = Namespace_Operand then
                     -- This placeholder never leaves the concat operand union.
                     -- Methods and fields take the ordinary evaluation path.
                     Bound := (Status => Integer_Binding, Value => 0, Origin => Ordinary_Integer);
                  else
                     -- Wide table fields materialize a buffer during evaluation.
                     Publish_Live;
                     Lookup (Environment, Input, Scope, Path, Width, Evaluate_Binding, Bound);
                  end if;
                  case Bound.Status is
                     when Failed_Binding => S := (if Bound.Failure in No_Return .. Execution_Status'Last then Bound.Failure else Unsupported_Value); return;
                     when Reference_Binding => V := (Value_Kind => Reference_Datum, Ref => Bound.Ref);
                     when Integer_Binding =>
                        if Allow_No_Return and Depth = 0 then S := Unsupported; return; end if;
                        V := (Value_Kind => Integer_Datum, Number => Bound.Value, Origin => Bound.Origin);
                     when Method_Binding =>
                        if Bound.Parameters = 0 then
                           Dispatch (Bound.Method_ID, [others => (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer)], 0,
                                     not Allow_No_Return or Depth > 0, V, S);
                           if S /= Returned then return; end if;
                           Pending_Provenance := Expression_Result;
                        else
                           if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
                           Depth := Depth + 1;
                           Stack (Depth) := (Is_Call => True,
                             Method_ID => Bound.Method_ID, Parameters => Bound.Parameters,
                             others => <>);
                           Have_Value := False;
                        end if;
                     when Missing_Binding => S := Unknown_Name; return;
                     when Non_Integer_Binding =>
                        if Bound.Object.Type_Code not in 2 .. 4 or else Bound.Object.ID = 0 then
                           S := Unsupported_Value; return;
                        end if;
                        if Allow_No_Return and Depth = 0 then S := Unsupported; return; end if;
                        V := (Value_Kind => Object_Datum, Object => Bound.Object);
                  end case;
               else
                  Literal := Read_Integer (Code (Code'First + Offset .. Code'First + (Limit - 1)), Width);
                  if Literal.Kind /= Accepted then
                     S := (if Literal.Kind = AML_Decode.Truncated then
                             AML_Execute.Truncated else AML_Execute.Unsupported);
                     return;
                  end if;
                  V := (Value_Kind => Integer_Datum, Number => Literal.Value, Origin => Literal_Origin (Code (Code'First + Offset)));
                  Offset := Offset + Literal.Consumed;
               end if;
               if Have_Value then
               loop
                  pragma Loop_Invariant (Context_Valid (Environment));
                  pragma Loop_Invariant (S /= Object_Returned and then S /= Reference_Returned);
                  pragma Loop_Invariant (Charged > 0 and then Charged <= Budget);
                  pragma Loop_Invariant (Charged >= Charged'Loop_Entry);
                  pragma Loop_Invariant (Offset <= Limit and then Limit <= Code'Length);
                  pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
                  pragma Loop_Variant (Decreases => Depth);
                  if Depth = 0 then
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     if V.Value_Kind = Reference_Datum and Require_Integer then S := Unsupported_Value; return; end if;
                     if V.Value_Kind = Object_Datum and Require_Integer then
                        Conversion := (if Width = Bits_32 then V.Object.Conversion_32 else V.Object.Conversion_64);
                        case Conversion.Status is
                           when AML_Coercions.Converted => V := (Value_Kind => Integer_Datum, Number => Conversion.Value, Origin => AML_Decode.Ordinary_Integer);
                           when AML_Coercions.Empty_Buffer => S := Empty_Buffer; return;
                           when AML_Coercions.Not_Convertible => S := Unsupported_Value; return;
                        end case;
                     end if;
                     return;
                  end if;
                  if Stack (Depth).Collecting_Targets then
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     if (V.Value_Kind /= Reference_Datum and then
                       (V.Value_Kind /= Integer_Datum or else V.Origin /= AML_Constant))
                       or else Stack (Depth).Target_Count >= Stack (Depth).Target_Total then
                        S := Unsupported_Value; return;
                     end if;
                     Stack (Depth).Target_Count := Stack (Depth).Target_Count + 1;
                     Stack (Depth).Targets (Stack (Depth).Target_Count) :=
                       (if V.Value_Kind = Reference_Datum then
                          (Kind => Reference_Target, Ref => V.Ref, others => <>)
                        else (Kind => Discard_Target, others => <>));
                     Collect_Targets
                       (Stack (Depth).Target_Count, Stack (Depth).Target_Total,
                        Stack (Depth).Targets, Waiting_For_Target, S);
                     if S /= Returned then return; end if;
                     if Waiting_For_Target then exit; end if;
                     Stack (Depth).Collecting_Targets := False;
                     Stack (Depth).Targets_Ready := True;
                     V := Stack (Depth).Right;
                     Pending_Slot := Stack (Depth).Right_Slot;
                     Pending_Concat := Stack (Depth).Right_Concat;
                     Pending_Provenance := Stack (Depth).Right_Provenance;
                  end if;
                  pragma Assert (Context_Valid (Environment));
                  if Stack (Depth).Op in Increment_Op | Decrement_Op then
                     Complete_Update (Stack (Depth).Targets (1), Stack (Depth).Op, V, S);
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Is_Conditional_Reference then
                     declare
                        Ref : AML_References.Reference := AML_References.No_Reference;
                        Cell_Ref : AML_Frame_Handles.Cell_Handle;
                        Made : Frames.Result_Status;
                        Present : Boolean := False;
                        Target : constant Target_Descriptor := Stack (Depth).Targets (1);
                     begin
                        -- ACPICA creates the target operand before looking up the source.
                        if Target.Kind = Name_Target then
                           Lookup (Environment, Input, Scope, Target.Path, Width, Namespace_Identity, Bound);
                           if Bound.Status = Missing_Binding then S := Unknown_Name; return;
                           elsif Bound.Status /= Reference_Binding then S := Unsupported_Value; return; end if;
                        end if;
                        if Stack (Depth).Source_Slot /= 0 then
                           Frames.Make_Reference (Registry, Call_Frame,
                             Cell (Natural (Stack (Depth).Source_Slot - 16#60#)), Cell_Ref, Made);
                           if Made /= Frames.Ready then S := Unsupported_Value; return; end if;
                           Ref := AML_References.Bind_Frame (Cell_Ref); Present := True;
                        else
                           Lookup (Environment, Input, Scope, Stack (Depth).Source_Name, Width, Namespace_Identity, Bound);
                           case Bound.Status is
                              when Missing_Binding => null;
                              when Reference_Binding => Ref := Bound.Ref; Present := True;
                              when others => S := Unsupported_Value; return;
                           end case;
                        end if;
                        if Present then
                           Apply_Target (Target, (Value_Kind => Reference_Datum, Ref => Ref), S);
                           if S /= Returned then return; end if;
                        end if;
                        V := (Value_Kind => Integer_Datum,
                          Number => (if Present then AML_Integers.Normalize (Integer_Value'Last, Width) else 0),
                          Origin => Ordinary_Integer);
                     end;
                  elsif Stack (Depth).Is_Call then
                     if Stack (Depth).Given >= Stack (Depth).Parameters then
                        S := Argument_Mismatch; return;
                     end if;
                     Stack (Depth).Actuals (Stack (Depth).Given) := V;
                     Stack (Depth).Slots (Stack (Depth).Given) := Pending_Slot;
                     Stack (Depth).Given := Stack (Depth).Given + 1;
                     if Stack (Depth).Given < Stack (Depth).Parameters then exit; end if;
                     for I in 0 .. Stack (Depth).Given - 1 loop
                        Argument_Item := Stack (Depth).Actuals (I);
                        Resolve_Slot (Stack (Depth).Slots (I), Argument_Item, S);
                        if S /= Returned then return; end if;
                        Stack (Depth).Actuals (I) := Argument_Item;
                     end loop;
                     Dispatch (Stack (Depth).Method_ID, Stack (Depth).Actuals,
                               Stack (Depth).Given, not Allow_No_Return or Depth > 1, V, S);
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op = Concatenate_Op then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Concat := Pending_Concat; Stack (Depth).Left_Provenance := Pending_Provenance;
                        Stack (Depth).Has_Left := True; exit;
                     end if;
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Concat := Pending_Concat; Stack (Depth).Right_Provenance := Pending_Provenance;
                        Stack (Depth).Target_Total := 1; Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1, Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False; Stack (Depth).Targets_Ready := True;
                     end if;
                     Resolve_Concatenation (Stack (Depth).Left_Concat, Stack (Depth).Left,
                       Stack (Depth).Left_Slot, Stack (Depth).Left_Provenance, S);
                     if S /= Returned then return; end if;
                     Resolve_Concatenation (Stack (Depth).Right_Concat, Stack (Depth).Right,
                       Stack (Depth).Right_Slot, Stack (Depth).Right_Provenance, S);
                     if S /= Returned then return; end if;
                     Attach_Concatenation (Stack (Depth).Left_Concat, Stack (Depth).Right_Concat,
                       Stack (Depth).Targets (1), V, S);
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op = Match_Op then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Provenance := Pending_Provenance;
                        Stack (Depth).Has_Left := True;
                        if Offset = Limit then S := Truncated; return; end if;
                        Stack (Depth).Match_Byte_1 := Code (Code'First + Offset);
                        Offset := Offset + 1; exit;
                     elsif not Stack (Depth).Has_Middle then
                        Stack (Depth).Middle := V; Stack (Depth).Middle_Slot := Pending_Slot;
                        Stack (Depth).Middle_Provenance := Pending_Provenance;
                        Stack (Depth).Has_Middle := True;
                        if Offset = Limit then S := Truncated; return; end if;
                        Stack (Depth).Match_Byte_2 := Code (Code'First + Offset);
                        Offset := Offset + 1; exit;
                     elsif not Stack (Depth).Has_Right then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Stack (Depth).Has_Right := True; exit;
                     end if;
                     declare
                        Package_Value : Datum := Stack (Depth).Left;
                        Match_1 : Datum := Stack (Depth).Middle;
                        Match_2 : Datum := Stack (Depth).Right;
                        Start : Datum := V;
                        Value : Integer_Value;
                        Visited : Match_Visit_Count;
                        Allowance : constant Match_Visit_Count := Budget - Charged;
                        procedure Admit (Item : in out Datum; Is_Package : Boolean) is
                        begin
                           if Item.Value_Kind = Integer_Datum and then not Is_Package then return; end if;
                           if Item.Value_Kind /= Object_Datum then S := Unsupported_Value; return; end if;
                           Refresh_Value (Environment, Item, S);
                           if S /= Returned then return; end if;
                           if Item.Value_Kind /= Object_Datum or else
                             (if Is_Package then Item.Object.Type_Code /= Package_Type_Code
                              else Item.Object.Type_Code not in String_Type_Code | Buffer_Type_Code)
                           then S := Unsupported_Value; end if;
                        end Admit;
                     begin
                        Resolve_Mid_Operand (Start, Pending_Slot, Pending_Provenance, S);
                        if S /= Returned then return; end if;
                        Convert_Integer_Value (Start, S);
                        if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Match_2, Stack (Depth).Right_Slot, Stack (Depth).Right_Provenance, S);
                        if S /= Returned then return; end if;
                        Admit (Match_2, False); if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Match_1, Stack (Depth).Middle_Slot, Stack (Depth).Middle_Provenance, S);
                        if S /= Returned then return; end if;
                        Admit (Match_1, False); if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Package_Value, Stack (Depth).Left_Slot, Stack (Depth).Left_Provenance, S);
                        if S /= Returned then return; end if;
                        Admit (Package_Value, True); if S /= Returned then return; end if;
                        if Stack (Depth).Match_Byte_1 > Match_Operation'Pos (Match_Operation'Last)
                          or else Stack (Depth).Match_Byte_2 > Match_Operation'Pos (Match_Operation'Last)
                        then S := Invalid_Match_Operation; return; end if;
                        Match_Package (Environment, Width, Package_Value, Match_1, Match_2,
                          Match_Operation'Val (Stack (Depth).Match_Byte_1),
                          Match_Operation'Val (Stack (Depth).Match_Byte_2),
                          AML_Integers.Normalize (Start.Number, Width), Allowance, Value, Visited, S);
                        if Visited > Allowance then S := Unsupported_Value; return; end if;
                        Charged := Charged + Visited;
                        if S /= Returned then return; end if;
                        V := (Integer_Datum, AML_Integers.Normalize (Value, Width), Ordinary_Integer);
                     end;
                  elsif Stack (Depth).Op = Concatenate_Resources_Op then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Provenance := Pending_Provenance;
                        Stack (Depth).Has_Left := True; exit;
                     end if;
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Stack (Depth).Target_Total := 1; Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1, Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False; Stack (Depth).Targets_Ready := True;
                     end if;
                     declare
                        Left : Datum := Stack (Depth).Left;
                        Right : Datum := Stack (Depth).Right;
                     begin
                        Resolve_Mid_Operand (Right, Stack (Depth).Right_Slot, Stack (Depth).Right_Provenance, S);
                        if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Left, Stack (Depth).Left_Slot, Stack (Depth).Left_Provenance, S);
                        if S /= Returned then return; end if;
                        Attach_Resources (Left, Right, Stack (Depth).Targets (1), V, S);
                        if S /= Returned then return; end if;
                     end;
                  elsif Stack (Depth).Op = To_String_Op then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Provenance := Pending_Provenance;
                        Stack (Depth).Has_Left := True; exit;
                     end if;
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Stack (Depth).Target_Total := 1; Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1, Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False; Stack (Depth).Targets_Ready := True;
                     end if;
                     declare
                        Item : Datum := Stack (Depth).Left;
                        Length : Datum := Stack (Depth).Right;
                     begin
                        Resolve_Mid_Operand (Length, Stack (Depth).Right_Slot, Stack (Depth).Right_Provenance, S);
                        if S /= Returned then return; end if;
                        Convert_Integer_Value (Length, S);
                        if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Item, Stack (Depth).Left_Slot, Stack (Depth).Left_Provenance, S);
                        if S /= Returned then return; end if;
                        Attach_String (Item, AML_Integers.Normalize (Length.Number, Width),
                          Stack (Depth).Targets (1), V, S);
                        if S /= Returned then return; end if;
                     end;
                  elsif Stack (Depth).Op = Mid_Op then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Provenance := Pending_Provenance;
                        Stack (Depth).Has_Left := True; exit;
                     elsif not Stack (Depth).Has_Middle then
                        Stack (Depth).Middle := V; Stack (Depth).Middle_Slot := Pending_Slot;
                        Stack (Depth).Middle_Provenance := Pending_Provenance;
                        Stack (Depth).Has_Middle := True; exit;
                     end if;
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Stack (Depth).Target_Total := 1; Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1, Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False; Stack (Depth).Targets_Ready := True;
                     end if;
                     declare
                        Item : Datum := Stack (Depth).Left;
                        Start : Datum := Stack (Depth).Middle;
                        Count : Datum := Stack (Depth).Right;
                     begin
                        -- All operand/target effects precede reverse-order
                        -- argument conversion, including on conversion failure.
                        Resolve_Mid_Operand (Count, Stack (Depth).Right_Slot, Stack (Depth).Right_Provenance, S);
                        if S /= Returned then return; end if;
                        Convert_Integer_Value (Count, S);
                        if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Start, Stack (Depth).Middle_Slot, Stack (Depth).Middle_Provenance, S);
                        if S /= Returned then return; end if;
                        Convert_Integer_Value (Start, S);
                        if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Item, Stack (Depth).Left_Slot, Stack (Depth).Left_Provenance, S);
                        if S /= Returned then return; end if;
                        Attach_Mid (Item, AML_Integers.Normalize (Start.Number, Width),
                          AML_Integers.Normalize (Count.Number, Width), Stack (Depth).Targets (1), V, S);
                        if S /= Returned then return; end if;
                     end;
                  elsif Stack (Depth).BCD /= Not_BCD then
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V;
                        Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Target_Total := 1;
                        Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1,
                          Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False;
                        Stack (Depth).Targets_Ready := True;
                     end if;
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     Convert_Integer_Value (V, S);
                     if S /= Returned then return; end if;
                     declare
                        use type AML_BCD.Conversion_Status;
                        Converted : constant AML_BCD.Result :=
                          (if Stack (Depth).BCD = Decode_BCD then AML_BCD.From_BCD (V.Number, Width)
                           else AML_BCD.To_BCD (V.Number, Width));
                     begin
                        if Converted.Status = AML_BCD.Numeric_Overflow then S := Numeric_Overflow; return; end if;
                        V := (Value_Kind => Integer_Datum, Number => Converted.Value, Origin => Ordinary_Integer);
                     end;
                     Apply_Target (Stack (Depth).Targets (1), V, S, Explicit_Result_Target);
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op in Decimal_String_Op | Hexadecimal_String_Op then
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V;
                        Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Stack (Depth).Target_Total := 1;
                        Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1,
                          Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False;
                        Stack (Depth).Targets_Ready := True;
                     end if;
                     declare
                        Item : Datum := Stack (Depth).Right;
                        Mode : constant Explicit_String_Mode :=
                          (if Stack (Depth).Op = Decimal_String_Op then Decimal_String else Hexadecimal_String);
                     begin
                        Resolve_Mid_Operand (Item, Stack (Depth).Right_Slot,
                          Stack (Depth).Right_Provenance, S);
                        if S /= Returned then return; end if;
                        Attach_Formatted_String (Item, Mode, Stack (Depth).Targets (1), V, S);
                        if S /= Returned then return; end if;
                     end;
                  elsif Stack (Depth).Op in To_Integer_Op | To_Buffer_Op then
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V;
                        Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Target_Total := 1;
                        Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1,
                          Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False;
                        Stack (Depth).Targets_Ready := True;
                     end if;
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     if Stack (Depth).Op = To_Buffer_Op then
                        declare Source : constant Datum := V; begin
                           Attach_Buffer (Source, Stack (Depth).Targets (1), V, S);
                        end;
                     else
                        Convert_To_Integer (Environment, Width, V, S);
                        if S /= Returned then return; end if;
                        Apply_Target (Stack (Depth).Targets (1), V, S, Explicit_Result_Target);
                     end if;
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op in 16#87# | 16#8E# then
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     declare Type_Code, Size : Natural; N : Integer_Value; begin
                        Describe (V, Type_Code, Size, S);
                        if S /= Returned then return; end if;
                        if Stack (Depth).Op = 16#87# and then Type_Code = 0
                          and then V.Value_Kind = Reference_Datum
                        then S := Unsupported_Value; return; end if;
                        Query_Value (Stack (Depth).Op, Type_Code, Size, N, S);
                        if S /= Returned then return; end if;
                        V := (Value_Kind => Integer_Datum, Number => N, Origin => AML_Decode.Ordinary_Integer);
                     end;
                  elsif Stack (Depth).Op = 16#88# then
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V;
                        Stack (Depth).Has_Left := True;
                        exit;
                     end if;
                     Left_Item := Stack (Depth).Left;
                     if Left_Item.Value_Kind /= Object_Datum then S := Unsupported_Value; return; end if;
                     if V.Value_Kind = Object_Datum then
                        Conversion := (if Width = Bits_32 then V.Object.Conversion_32 else V.Object.Conversion_64);
                        if Conversion.Status /= AML_Coercions.Converted then S := Unsupported_Value; return; end if;
                        V := (Value_Kind => Integer_Datum, Number => Conversion.Value, Origin => AML_Decode.Ordinary_Integer);
                     end if;
                     if V.Value_Kind /= Integer_Datum then S := Unsupported_Value; return; end if;
                     declare
                        Ref : AML_References.Reference;
                     begin
                        Create_Index (Environment, Left_Item.Object.Source, V.Number, Ref, S);
                        if S /= Returned then S := Unsupported_Value; return; end if;
                        V := (Value_Kind => Reference_Datum, Ref => Ref);
                     end;
                     Write_Target (V, S);
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op = 16#83# then
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     if V.Value_Kind /= Reference_Datum then S := Unsupported_Value; return; end if;
                     declare
                        Resolved : Datum;
                     begin
                        Read_Reference_Value (V.Ref, Resolved, S);
                        if S = Uninitialized and then AML_References.Kind (V.Ref) = AML_References.Frame_Cell then
                           S := No_Return; return;
                        end if;
                        if S not in Returned | Uninitialized | Unsupported_Value then S := Unsupported_Value; end if;
                        if S /= Returned then return; end if;
                        V := Resolved;
                     end;
                  elsif Stack (Depth).Op = Copy_Object_Op then
                     Resolve_Slot (Pending_Slot, V, S);
                     if S /= Returned then return; end if;
                     if Offset = Limit then S := Truncated; return; end if;
                     declare
                        use AML_Simple_Targets;
                        Target : constant Target_Result := Read_Target
                          (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                        Copied : Datum;
                     begin
                        case Target.Kind is
                           when AML_Simple_Targets.Truncated => S := Truncated; return;
                           when Malformed | Limit_Exceeded => S := Bad_Name; return;
                           when Name_Target =>
                              if Target.Path.Count = 0 then S := Bad_Name; return; end if;
                           when Local_Target | Argument_Target => null;
                        end case;
                        if Target.Kind = Argument_Target then
                           declare Destination : constant Frames.Read_Result := Slot_Value
                             (First_Argument_Cell + Target.Slot); Checked_Value : Datum; begin
                              if Destination.Initialized and then Destination.Value.Value_Kind = Reference_Datum
                                and then AML_References.Kind (Destination.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell
                              then
                                 Read_Reference_Value (Destination.Value.Ref, Checked_Value, S);
                                 if S not in Returned | Uninitialized then return; end if;
                              end if;
                           end;
                        end if;
                        if Target.Kind = Name_Target then
                           Publish_Live ([V]);
                           Copy_And_Attach (Environment,
                             (Kind => Named_Destination, Scope => Scope, Path => Target.Path), Width, V, Copied, S);
                        elsif Target.Kind = Argument_Target then
                           declare Destination : constant Frames.Read_Result := Slot_Value
                             (First_Argument_Cell + Target.Slot); begin
                              if Destination.Initialized and then Destination.Value.Value_Kind = Reference_Datum
                                and then AML_References.Kind (Destination.Value.Ref) = AML_References.Named_Cell
                              then
                                 Publish_Live ([V, Destination.Value]);
                                 Copy_And_Attach (Environment,
                                   (Kind => Referenced_Destination, Ref => Destination.Value.Ref), Width, V, Copied, S);
                              else
                                 Publish_Live ([V]);
                                 Clone_Value (Environment, Width, V, Copied, S);
                                 if S = Returned then Write_Slot
                                   (16#68# + Byte (Target.Slot), Copied, S, Prepared_Copy => True); end if;
                              end if;
                           end;
                        else
                           Publish_Live ([V]);
                           Clone_Value (Environment, Width, V, Copied, S);
                           if S = Returned then Write_Slot
                             (16#60# + Byte (Target.Slot), Copied, S, Prepared_Copy => True); end if;
                        end if;
                        if S /= Returned then return; end if;
                        Offset := Offset + Target.Consumed;
                        V := Copied;
                     end;
                  elsif Stack (Depth).Op = 16#70# then
                     if Stack (Depth).Has_Left then
                        Resolve_Slot (Pending_Slot, V, S);
                        if S /= Returned then return; end if;
                        if V.Value_Kind /= Reference_Datum and then
                          (V.Value_Kind /= Integer_Datum or else V.Origin /= AML_Constant)
                        then S := Unsupported_Value; return; end if;
                        Left_Item := Stack (Depth).Left;
                        Resolve_Slot (Stack (Depth).Left_Slot, Left_Item, S);
                        if S /= Returned then return; end if;
                        if V.Value_Kind = Reference_Datum then
                           Publish_Live ([Left_Item, V]);
                           Write_Reference_Value (V.Ref, Left_Item, S);
                        else
                           S := Returned;
                        end if;
                        if S not in Returned | Unsupported_Value | Value_Limit | Empty_Buffer then S := Unsupported_Value; end if;
                        if S /= Returned then return; end if;
                        V := Left_Item;
                     elsif Offset < Limit and then Code (Code'First + Offset) in 16#71# | 16#83# | 16#88# then
                        -- Preserve local/argument identity until target side
                        -- effects finish, matching sibling operand resolution.
                        Stack (Depth).Left := V;
                        Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Has_Left := True;
                        exit;
                     else
                        Resolve_Slot (Pending_Slot, V, S);
                        if S /= Returned then return; end if;
                        Publish_Live ([V]);
                        Write_Target (V, S);
                        if S /= Returned then return; end if;
                     end if;
                  else
                  Advance_Numeric
                    (Stack (Depth), Pending_Slot, V, Waiting_For_Target, S);
                  if S /= Returned then return; end if;
                  if Waiting_For_Target then exit; end if;
                  end if;
                  Pending_Slot := 0;
                  Pending_Concat := (Kind => Data_Operand, Value => <>);
                  Pending_Provenance := Expression_Result;
                  Depth := Depth - 1;
               end loop;
               end if;
            end if;
         end loop;
      end Evaluate_Operand;
      procedure Operand (V : out Datum; S : out Execution_Status; Allow_No_Return : Boolean := False; Require_Integer : Boolean := False)
        with Always_Terminates,
             Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(2)),
             Pre => Context_Valid (Environment) and then not V'Constrained and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget,
             Post => Context_Valid (Environment) and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget
               and then Charged >= Charged'Old and then Offset >= Offset'Old
               and then (if S = Returned then Charged > Charged'Old)
               and then S /= Object_Returned and then S /= Reference_Returned
      is
      begin
         Evaluate_Operand (V, S, Allow_No_Return, Require_Integer);
         -- Evaluation has returned, including all early-failure paths. No
         -- collecting callback runs before the caller consumes/publishes V.
         Publish_Expression_Roots (Environment, Call_Frame, []);
      end Operand;
   begin
      loop
         pragma Loop_Invariant (Context_Valid (Environment));
         pragma Loop_Invariant (Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget);
         pragma Loop_Invariant
           (for all I in 1 .. Block_Depth =>
              Blocks (I).Resume <= Blocks (I).Outer_Limit and then
              Blocks (I).Restart <= Blocks (I).Outer_Limit and then
              Blocks (I).Outer_Limit <= Code'Length);
         pragma Loop_Variant (Decreases => Budget - Charged, Decreases => Block_Depth);
         if Offset = Limit then
            exit when Block_Depth = 0;
            Offset := (if Blocks (Block_Depth).Is_Loop then
                         Blocks (Block_Depth).Restart else Blocks (Block_Depth).Resume);
            Limit := Blocks (Block_Depth).Outer_Limit;
            Block_Depth := Block_Depth - 1;
         else
         if Charged = Budget then Body_Result := Failure (Budget_Exceeded, Charged); return; end if;
         Charged := Charged + 1;
         Op := Code (Code'First + Offset);
         Offset := Offset + 1;
         case Op is
            when 16#5B# =>
               if Offset = Limit then Body_Result := Failure (Truncated, Charged); return; end if;
               if Code (Code'First + Offset) in Conditional_Reference_Op | From_BCD_Extension | To_BCD_Extension | 16#33# then
                  Offset := Offset - 1;
                  Operand (Value, State);
                  if State /= Returned then
                     Body_Result := Failure (State, Charged); return;
                  end if;
               elsif Code (Code'First + Offset) = 16#88# then
                  Offset := Offset + 1;
                  declare
                     use type AML_Names.Parse_Status;
                     Path : AML_Names.Name_Result;
                     Token : Natural;
                     Declared_Status : Execution_Status;
                     Signature, OEM, Table_ID : Datum;
                  begin
                     if Offset = Limit then Body_Result := Failure (Bad_Name, Charged); return; end if;
                     Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                     if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                        Body_Result := Failure (Bad_Name, Charged); return;
                     end if;
                     Offset := Offset + Path.Consumed;
                     Reserve_Region (Environment, Scope, Path, Token, Declared_Status);
                     if Declared_Status /= Returned or else Token = 0 then
                        Body_Result := Failure
                          ((if Declared_Status in Failure_Status then Declared_Status else Unsupported), Charged);
                        return;
                     end if;
                     Operand (Signature, Declared_Status);
                     if Declared_Status /= Returned then Body_Result := Failure (Declared_Status, Charged); return; end if;
                     Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Region_Signature, True, Signature);
                     Operand (OEM, Declared_Status);
                     if Declared_Status /= Returned then Body_Result := Failure (Declared_Status, Charged); return; end if;
                     Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Region_OEM, True, OEM);
                     Operand (Table_ID, Declared_Status);
                     if Declared_Status /= Returned then Body_Result := Failure (Declared_Status, Charged); return; end if;
                     Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Region_Table_ID, True, Table_ID);
                     Complete_Region (Environment, Input, Token, Width, Signature, OEM, Table_ID, Declared_Status);
                     for Root in AML_Root_Slots.Region_Signature .. AML_Root_Slots.Region_Table_ID loop
                        Publish_Held_Root (Environment, Call_Frame, Root, False,
                          (Integer_Datum, 0, Ordinary_Integer));
                     end loop;
                     if Declared_Status /= Returned then
                        Body_Result := Failure
                          ((if Declared_Status in Failure_Status then Declared_Status else Unsupported), Charged);
                        return;
                     end if;
                  end;
               else
               if Code (Code'First + Offset) in 16#21# | 16#22# then
                  declare
                     Kind : constant AML_Delays.Delay_Kind :=
                       (if Code (Code'First + Offset) = 16#22# then AML_Delays.Sleep_Delay else AML_Delays.Stall_Delay);
                     Item : AML_Delays.Normalization_Result;
                     Outcome : AML_Delays.Outcome;
                     use type AML_Delays.Normalization_Status;
                  begin
                     Offset := Offset + 1;
                     Operand (Value, State);
                     if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
                     Convert_Integer_Value (Value, State);
                     if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
                     Item := AML_Delays.Normalize (Kind, Width, Value.Number);
                     if Item.Status /= AML_Delays.Accepted then
                        Body_Result := Failure (Invalid_Delay, Charged); return;
                     end if;
                     Invoke_Delay (Item.Item, Outcome);
                     case Outcome is
                        when AML_Delays.Completed => null;
                        when AML_Delays.Unavailable => Body_Result := Failure (Unsupported, Charged); return;
                        when AML_Delays.Failed => Body_Result := Failure (Delay_Failed, Charged); return;
                     end case;
                  end;
               else
               if Code (Code'First + Offset) /= 16#81# then
                  Body_Result := Failure (Unsupported, Charged); return;
               end if;
               Offset := Offset + 1;
               if Offset = Limit then Body_Result := Failure (Bad_Package, Charged); return; end if;
               declare
                  use type AML_Names.Parse_Status;
                  P : constant Package_Result := Read_Package
                    (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  Finish : Natural;
                  Region : AML_Names.Name_Result;
                  Flags : Byte;
                  Declared_Status : Execution_Status;
               begin
                  if P.Kind /= Accepted then Body_Result := Failure (Bad_Package, Charged); return; end if;
                  Finish := Offset + P.Extent;
                  Offset := Offset + P.Encoding_Bytes;
                  if Offset = Finish then Body_Result := Failure (Bad_Name, Charged); return; end if;
                  Region := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Finish - 1)));
                  if Region.Kind /= AML_Names.Accepted then Body_Result := Failure (Bad_Name, Charged); return; end if;
                  Offset := Offset + Region.Consumed;
                  if Offset = Finish then Body_Result := Failure (Truncated, Charged); return; end if;
                  Flags := Code (Code'First + Offset);
                  Offset := Offset + 1;
                  -- Charge every FieldList byte before any namespace mutation.
                  if Finish - Offset > Budget - Charged then
                     Body_Result := Failure (Budget_Exceeded, Charged); return;
                  end if;
                  Charged := Charged + (Finish - Offset);
                  if Offset = Finish then
                     Define_Fields (Environment, Scope, Region, Flags, [], Declared_Status);
                  else
                     Define_Fields (Environment, Scope, Region, Flags,
                       Code (Code'First + Offset .. Code'First + (Finish - 1)), Declared_Status);
                  end if;
                  if Declared_Status /= Returned then
                     Body_Result := Failure
                       ((if Declared_Status in Failure_Status then Declared_Status else Unsupported), Charged);
                     return;
                  end if;
                  Offset := Finish;
               end;
               end if;
               end if;
            when 16#08# => -- Name: publish one bounded data initializer.
               declare
                  Path : AML_Names.Name_Result;
                  Used : Natural;
                  Declared_Status : Execution_Status;
                  use type AML_Names.Parse_Status;
               begin
                  if Offset = Limit then Body_Result := Failure (Truncated, Charged); return; end if;
                  Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                     Body_Result := Failure ((if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name), Charged); return;
                  end if;
                  Offset := Offset + Path.Consumed;
                  if Offset = Limit then Body_Result := Failure (Truncated, Charged); return; end if;
                  if Active_Buffer_Count (Code (Code'First + Offset .. Code'First + (Limit - 1))) then
                     declare
                        Initializer_Start : constant Natural := Offset;
                        P : constant Package_Result := Read_Package
                          (Code (Code'First + Offset + 1 .. Code'First + (Limit - 1)));
                        Finish : constant Natural := Offset + P.Extent + 1;
                        Outer_Limit : constant Natural := Limit;
                        Count_Start : Natural;
                        Token : Name_Reservation_Token := No_Name_Reservation_Token;
                        Count : AML_Data.Count_Result;
                        Abort_Status : Execution_Status;
                     begin
                        pragma Assert (Context_Valid (Environment));
                        Reserve_Runtime_Name (Environment, Scope, Path, Token, Declared_Status);
                        if Declared_Status /= Returned then
                           Body_Result := Failure ((if Declared_Status in Failure_Status then Declared_Status else Unsupported_Value), Charged); return;
                        end if;
                        Offset := Offset + 1 + P.Encoding_Bytes;
                        Count_Start := Offset; Limit := Finish;
                        Operand (Value, Declared_Status);
                        Limit := Outer_Limit;
                        if Declared_Status = Returned then
                           Resolve_Buffer_Count (Value, Declared_Status);
                        end if;
                        if Declared_Status = Returned then
                           Count := (Kind => Accepted, Value => AML_Integers.Normalize (Value.Number, Width),
                             Consumed => Offset - Count_Start);
                           pragma Assert (Context_Valid (Environment));
                        Complete_Runtime_Buffer (Environment, Token, Width,
                             Code (Code'First + Initializer_Start .. Code'First + (Finish - 1)), Count, Declared_Status);
                        end if;
                        if Declared_Status /= Returned then
                           pragma Assert (Context_Valid (Environment));
                        Abort_Runtime_Name (Environment, Token, Abort_Status);
                           pragma Assert (Abort_Status = Returned);
                           if Abort_Status /= Returned then Declared_Status := Unsupported_Value; end if;
                           Body_Result := Failure ((if Declared_Status in Failure_Status then Declared_Status else Unsupported_Value), Charged); return;
                        end if;
                        Offset := Finish;
                     end;
                  else
                  Define_Name (Environment, Scope, Path, Width,
                    Code (Code'First + Offset .. Code'First + (Limit - 1)), Used, Declared_Status);
                  if Declared_Status /= Returned then
                     Body_Result := Failure ((if Declared_Status in Failure_Status then Declared_Status else Unsupported_Value), Charged); return;
                  end if;
                  if Used = 0 or else Used > Limit - Offset then
                     Body_Result := Failure (Unsupported_Value, Charged); return;
                  end if;
                  Offset := Offset + Used;
                  end if;
               end;
            when 16#14# => -- Method: declaration takes effect when executed.
               declare
                  P : Package_Result;
                  Path : AML_Names.Name_Result;
                  Finish : Natural;
                  Flags : Byte;
                  Declared_Status : Declaration_Status;
                  use type AML_Names.Parse_Status;
               begin
                  if Offset = Limit then Body_Result := Failure (Bad_Package, Charged); return; end if;
                  P := Read_Package (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  if P.Kind /= Accepted then Body_Result := Failure (Bad_Package, Charged); return; end if;
                  Finish := Offset + P.Extent;
                  Offset := Offset + P.Encoding_Bytes;
                  if Offset = Finish then Body_Result := Failure (Truncated, Charged); return; end if;
                  Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Finish - 1)));
                  if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                     Body_Result := Failure (Unsupported, Charged); return;
                  end if;
                  Offset := Offset + Path.Consumed;
                  if Offset = Finish then Body_Result := Failure (Truncated, Charged); return; end if;
                  Flags := Code (Code'First + Offset);
                  Offset := Offset + 1;
                  if Offset = Finish then
                     Define_Method (Environment, Scope, Path, Flags, Width, [], Declared_Status);
                  else
                     Define_Method (Environment, Scope, Path, Flags, Width,
                       Code (Code'First + Offset .. Code'First + (Finish - 1)), Declared_Status);
                  end if;
                  case Declared_Status is
                     when Declared => null;
                     when Declaration_Duplicate => Body_Result := Failure (Duplicate_Name, Charged); return;
                     when Declaration_Missing => Body_Result := Failure (Unknown_Name, Charged); return;
                     when Declaration_Full => Body_Result := Failure (Namespace_Limit, Charged); return;
                     when Declaration_Unsupported => Body_Result := Failure (Unsupported, Charged); return;
                  end case;
                  Offset := Finish;
               end;
            when 16#A0# | 16#A2# => --  If / While
               declare
                  P : Package_Result;
                  If_End, Else_Start, Resume, Outer : Natural;
                  Has_Else : Boolean := False;
                  Start : constant Natural := Offset - 1;
                  Is_Loop : constant Boolean := Op = 16#A2#;
               begin
                  if Offset = Limit then Body_Result := Failure (Bad_Package, Charged); return; end if;
                  P := Read_Package (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  if P.Kind /= Accepted then Body_Result := Failure (Bad_Package, Charged); return; end if;
                  If_End := Offset + P.Extent;
                  Offset := Offset + P.Encoding_Bytes;
                  Resume := If_End;
                  Else_Start := If_End;
                  if not Is_Loop and then If_End < Limit and then Code (Code'First + If_End) = 16#A1# then
                     Has_Else := True;
                     Else_Start := If_End + 1;
                     if Else_Start = Limit then Body_Result := Failure (Bad_Package, Charged); return; end if;
                     P := Read_Package (Code (Code'First + Else_Start .. Code'First + (Limit - 1)));
                     if P.Kind /= Accepted then Body_Result := Failure (Bad_Package, Charged); return; end if;
                     Resume := Else_Start + P.Extent;
                     Else_Start := Else_Start + P.Encoding_Bytes;
                  end if;
                  Outer := Limit;
                  Limit := If_End;
                  Operand (Value, State, Require_Integer => True);
                  if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
                  --  Predicate conversion observes the AML integer width.
                  if Value.Value_Kind /= Integer_Datum then Body_Result := Failure (Unsupported_Value, Charged); return; end if;
                  Value := (Value_Kind => Integer_Datum, Number => AML_Integers.Normalize (Value.Number, Width), Origin => AML_Decode.Ordinary_Integer);
                  if Value.Number = 0 then
                     if Has_Else then
                        Offset := Else_Start;
                        Limit := Resume;
                     else
                        Offset := Resume;
                        Limit := Outer;
                     end if;
                  end if;
                  --  A tail If completes this region directly. Its enclosing
                  --  frame already owns the continuation; While must retain
                  --  its restart frame even at the end of a region.
                  if (Value.Number /= 0 or Has_Else)
                    and then (Is_Loop or else Resume /= Outer)
                  then
                     if Block_Depth = 64 then Body_Result := Failure (Block_Limit, Charged); return; end if;
                     Block_Depth := Block_Depth + 1;
                     Blocks (Block_Depth) :=
                       (Outer_Limit => Outer, Resume => Resume,
                        Restart => Start, Is_Loop => Is_Loop);
                  end if;
               end;
            when 16#A5# | 16#9F# => --  Break / Continue, nearest loop only.
               declare
                  Found : Boolean := False;
               begin
                  for I in reverse 1 .. Block_Depth loop
                     if Blocks (I).Is_Loop then
                        Offset := (if Op = 16#A5# then Blocks (I).Resume
                                   else Blocks (I).Restart);
                        Limit := Blocks (I).Outer_Limit;
                        Block_Depth := I - 1;
                        Found := True;
                        exit;
                     end if;
                  end loop;
                  if not Found then Body_Result := Failure (Invalid_Control, Charged); return; end if;
               end;
            when 16#A3# => null; --  Noop
            when 16#A4# => -- Return
               Operand (Value, State);
               if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
               if Value.Value_Kind = Reference_Datum then
                  Body_Result := (Status => Reference_Returned, Charged => Charged, Ref => Value.Ref); return;
               elsif Value.Value_Kind = Object_Datum then
                  Body_Result := (Status => Object_Returned, Charged => Charged, Object => Value.Object); return;
               end if;
               Body_Result := (Status => Returned, Charged => Charged, Value => Value.Number, Origin => Value.Origin); return;
            when 16#70# | Copy_Object_Op => -- Shared statement/expression evaluation.
               Offset := Offset - 1;
               Charged := Charged - 1;
               Operand (Value, State);
               if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
            when 16#72# | 16#74# | Increment_Op | Decrement_Op | 16#77# .. 16#82# | 16#85#
               | 16#83# | 16#87# | 16#88# | 16#8E# | 16#90# .. 16#95# | To_Integer_Op | To_Buffer_Op | Mid_Op | To_String_Op | Decimal_String_Op | Hexadecimal_String_Op | Match_Op | Concatenate_Resources_Op | Concatenate_Op =>
               Offset := Offset - 1;
               Operand (Value, State);
               if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
            when 16#41# .. 16#5A# | 16#5F# | 16#5C# | 16#5E# | 16#2E# | 16#2F# =>
               Offset := Offset - 1;
               Operand (Value, State, Allow_No_Return => True);
               if State /= Returned and State /= No_Return then
                  Body_Result := Failure (State, Charged); return;
               end if;
            when others => Body_Result := Failure (Unsupported, Charged); return;
         end case;
         end if;
      end loop;
      Body_Result := Failure (No_Return, Charged); return;
      end Execute_Body;
   begin
      if not Frame_Root_Capacity (Environment) then
         Result_Out := (Status => Value_Limit, Charged => 0); return;
      end if;
      Frames.Open_Frame (Registry, Call_Frame, Frame_Status);
      if Frame_Status /= Frames.Ready then
         if Frame_Status = Frames.Generation_Limit then
            Result_Out := (Status => Value_Limit, Charged => 0);
         else Result_Out := (Status => Call_Limit, Charged => 0); end if;
         return;
      end if;
      Open_Frame_Root (Environment, Call_Frame);
      for I in 0 .. 6 loop
         if I < Argument_Count then
            Write_Frame_Cell (Call_Frame,
              AML_Frame_Handles.Cell_ID'Val (First_Argument_Cell + I), Args (I), Frame_Status);
            pragma Assert (Frame_Status = Frames.Ready);
         end if;
      end loop;
      Begin_Call (Environment, Scope, Allowed);
      if Allowed then
         Execute_Body (Result_Out);
         if Result_Out.Status = Reference_Returned
           and then AML_References.Kind (Result_Out.Ref) = AML_References.Frame_Cell
           and then AML_Frame_Handles.Matches (AML_References.Frame_Item (Result_Out.Ref), Call_Frame)
         then Result_Out := (Status => No_Return, Charged => Result_Out.Charged); end if;
         -- Transfer the result before the callee's method-owned names and
         -- cells disappear. This single bridge is conservative until frame exit
         -- or a later compound return; expression-stack roots remain separate.
         if Caller_Frame /= AML_Frame_Handles.No_Frame then
            if Result_Out.Status = Object_Returned then
               Publish_Held_Root (Environment, Caller_Frame, AML_Root_Slots.Returned_Value,
                 True, (Object_Datum, Result_Out.Object));
            elsif Result_Out.Status = Reference_Returned then
               Publish_Held_Root (Environment, Caller_Frame, AML_Root_Slots.Returned_Value,
                 True, (Reference_Datum, Result_Out.Ref));
            end if;
         end if;
         if Calls_Left = Root_Call_Budget
           and then Result_Out.Status in Object_Returned | Reference_Returned
         then Handoff_Result (Environment, Result_Out); end if;
         End_Call (Environment, Scope);
      else
         Result_Out := (Status => Namespace_Limit, Charged => 0);
      end if;
      Frames.Close_Frame (Registry, Call_Frame, Frame_Status);
      pragma Assert (Frame_Status = Frames.Ready);
      if Frame_Status = Frames.Ready then
         for C in AML_Frame_Handles.Cell_ID loop
            Publish_Frame_Root (Environment, Call_Frame, C, False,
              (Integer_Datum, 0, Ordinary_Integer));
         end loop;
         Close_Frame_Root (Environment, Call_Frame);
      end if;
   end Run;
   begin
      Begin_Invocation (Environment, Domain, Issued);
      if Issued = Exhausted then
         Result_Out := (Status => Value_Limit, Charged => 0); return;
      end if;
      if Issued = Available and then not AML_Frame_Handles.Has_Authority (Domain) then
         Result_Out := (Status => Unsupported_Value, Charged => 0); return;
      end if;
      if Issued = Unsupported_Context then Domain := AML_Frame_Handles.No_Domain; end if;
      Registry := Frames.Empty (Domain);
      Run (Code, Width, Args, Argument_Count, Budget, Scope, Result_Out, Calls_Left, Current_Sync,
        AML_Frame_Handles.No_Frame);
   end Execute_With_Input;

   procedure Execute_Typed
     (Code : Bytes; Width : Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
   is
      type No_Input is limited null record;
      Empty : aliased No_Input;
      procedure Read_Binding
        (Environment : in out Context; Input : aliased No_Input; Scope : Natural;
         Path : AML_Names.Name_Result; Width : Integer_Width;
         Purpose : Binding_Purpose; Binding : out Binding_Result)
        with Pre => Context_Valid (Environment) and then not Binding'Constrained,
             Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Input);
      begin
         if Purpose = Namespace_Identity then
            Binding := (Status => Failed_Binding, Failure => Unsupported_Value); return;
         end if;
         Binding := Lookup (Environment, Scope, Path, Width);
      end Read_Binding;
      procedure Reject_Fields
        (Environment : in out Context; Scope : Natural; Region : AML_Names.Name_Result;
         Flags : Byte; Entries : Bytes; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Region, Flags, Entries);
      begin
         Status := Unsupported;
      end Reject_Fields;
      procedure Reject_Literal
        (Environment : in out Context; Scope : Natural; Kind : Literal_Kind; Width : Integer_Width; Data : Bytes;
         Binding : out Binding_Result)
        with Pre => Context_Valid (Environment) and then not Binding'Constrained,
             Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Kind, Width, Data);
      begin
         Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
      end Reject_Literal;
      procedure Reject_Region
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Natural; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Path);
      begin
         Token := 0; Status := Unsupported;
      end Reject_Region;
      procedure Reject_Completion
        (Environment : in out Context; Input : aliased No_Input; Token : Natural;
         Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Input, Token, Width, Signature, OEM, Table_ID);
      begin
         Status := Unsupported;
      end Reject_Completion;
      procedure Unavailable_Delay (Environment : in out Context; Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
         pragma Unreferenced (Environment);
      begin
         AML_Delays.Unavailable_Provider (Item, Result);
      end Unavailable_Delay;
      procedure No_Timer
        (Environment : in out Context; Value : out Integer_Value;
         Available : out Boolean)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
      begin
         Value := 0; Available := False;
      end No_Timer;
      procedure Reject_Reference
        (Environment : in out Context; Ref : AML_References.Reference;
         Value : out Datum; Status : out Execution_Status)
        with Pre => Context_Valid (Environment) and then not Value'Constrained,
             Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Ref);
      begin Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Reject_Reference;
      procedure Reject_Index
        (Environment : in out Context; Source : AML_References.Object_Handle;
         Index : Integer_Value; Ref : out AML_References.Reference; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Source, Index);
      begin Ref := AML_References.No_Reference; Status := Unsupported_Value; end Reject_Index;
      procedure Reject_Store
        (Environment : in out Context; Ref : AML_References.Reference;
         Width : Integer_Width; Item : Datum; Status : out Execution_Status; Mode : Reference_Store_Mode)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Ref, Width, Item, Mode);
      begin Status := Unsupported_Value; end Reject_Store;
      procedure Clone_Value
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum; Copy : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
        with Pre => Context_Valid (Environment) and then not Copy'Constrained,
             Post => Context_Valid (Environment)
      is
      begin
         Copy := (Value_Kind => AML_Execute.Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
         if Item.Value_Kind = AML_Execute.Integer_Datum then
            Copy.Number := AML_Integers.Normalize (Item.Number, Width);
            Copy.Origin := Item.Origin;
            Status := AML_Execute.Returned;
         end if;
      end Clone_Value;

      procedure Reject_Compare
        (Environment : in out Context; Op : Byte; Left, Right : Datum;
         Width : Integer_Width; Value : out Integer_Value; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Op, Left, Right, Width);
      begin Value := 0; Status := Unsupported_Value; end Reject_Compare;
      procedure Keep_Value
        (Environment : Context; Item : in out Datum; Status : out Execution_Status)
      is
         pragma Unreferenced (Environment, Item);
      begin Status := Returned; end Keep_Value;
      procedure No_Invocation
        (Environment : in out Context; Domain : out AML_Frame_Handles.Invocation_Domain;
         Status : out Invocation_Status)
      is
         pragma Unreferenced (Environment);
      begin Domain := AML_Frame_Handles.No_Domain; Status := Unsupported_Context; end No_Invocation;
      procedure Reject_Name
        (E : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes;
         Consumed : out Natural; Status : out Execution_Status) is
         pragma Unreferenced (E, Scope, Path, Width, Data);
      begin Consumed := 0; Status := Unsupported_Value; end Reject_Name;
      procedure Reject_Copy_Attachment
        (Environment : in out Context; Destination : Copy_Destination;
         Width : Integer_Width; Item : Datum; Copy : out Datum; Status : out Execution_Status)
      is
         pragma Unreferenced (Environment, Destination, Width, Item);
      begin Copy := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
         Status := Unsupported_Value; end Reject_Copy_Attachment;
      procedure Describe_Identity
        (E : Context; Ref : AML_References.Reference;
         Result : out AML_Execute.Reference_Metadata)
        with Pre => not Result'Constrained
      is
         pragma Unreferenced (E, Ref);
      begin Result := (Kind => AML_Execute.Continue_Reference); end Describe_Identity;
      procedure Reserve_Dynamic_Name
        (E : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Boolean; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (E, Scope, Path);
      begin Token := False; Status := AML_Execute.Unsupported_Value; end Reserve_Dynamic_Name;
      procedure Complete_Dynamic_Buffer
        (E : in out Context; Token : Boolean; Width : AML_Decode.Integer_Width;
         Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (E, Token, Width, Initializer, Count);
      begin Status := AML_Execute.Unsupported_Value; end Complete_Dynamic_Buffer;
      procedure Abort_Dynamic_Name
        (E : in out Context; Token : Boolean; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (E, Token);
      begin Status := AML_Execute.Unsupported_Value; end Abort_Dynamic_Name;


      procedure Reject_Explicit_Integer
        (Environment : Context; Width : AML_Decode.Integer_Width;
         Item : in out AML_Execute.Datum; Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width);
      begin
         Status := (if Item.Value_Kind = AML_Execute.Integer_Datum then
           AML_Execute.Returned else AML_Execute.Unsupported_Value);
      end Reject_Explicit_Integer;
      procedure Disabled_Debug
        (Environment : Context; Scope, Position : Natural;
         Width : Integer_Width; Value : Datum)
      is
         pragma Unreferenced (Environment, Scope, Position, Width, Value);
      begin null; end Disabled_Debug;
      procedure Reject_Concatenation
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Left, Right : AML_Execute.Concatenation_Operand;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Left, Right, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_Concatenation;
      procedure Reject_To_Buffer
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Item, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_To_Buffer;
      procedure Reject_Mid
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum; Start, Count : Integer_Value;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Item, Start, Count, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_Mid;
      procedure Reject_To_String
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Item : AML_Execute.Datum; Length : Integer_Value;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Item, Length, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_To_String;
      procedure Reject_Format_String
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Mode : AML_Execute.Explicit_String_Mode; Item : AML_Execute.Datum;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Mode, Item, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_Format_String;
      procedure Reject_Resources
        (Environment : in out Context; Width : AML_Decode.Integer_Width;
         Left, Right : AML_Execute.Datum;
         Destination : AML_Execute.Concatenation_Destination;
         Result_Value, Cell_Value : out AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Left, Right, Destination);
      begin
         Result_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Cell_Value := (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer);
         Status := AML_Execute.Unsupported_Value;
      end Reject_Resources;
      procedure Reject_Match
        (Environment : Context; Width : AML_Decode.Integer_Width;
         Package_Value, Match_1, Match_2 : AML_Execute.Datum;
         Operation_1, Operation_2 : AML_Execute.Match_Operation;
         Start : AML_Decode.Integer_Value; Max_Visited : AML_Execute.Match_Visit_Count;
         Value : out AML_Decode.Integer_Value; Visited : out AML_Execute.Match_Visit_Count;
         Status : out AML_Execute.Execution_Status)
      is
         pragma Unreferenced (Environment, Width, Package_Value, Match_1, Match_2, Operation_1, Operation_2, Start, Max_Visited);
      begin
         Value := 0; Visited := 0; Status := AML_Execute.Unsupported_Value;
      end Reject_Match;
      procedure Execute is new Execute_With_Input
        (Context, No_Input, Context_Valid, Read_Binding, Get_Method, Write,
         Begin_Call, End_Call, Define_Method, Reject_Fields, Reject_Literal, Reject_Region, Reject_Completion, No_Timer, Reject_Reference, Reject_Index, Reject_Store, Clone_Value, Reject_Compare, Keep_Value, No_Invocation, Reject_Name, Reject_Copy_Attachment, Describe_Identity, Boolean, False, Reserve_Dynamic_Name, Complete_Dynamic_Buffer, Abort_Dynamic_Name, Disabled_Debug, Reject_Explicit_Integer, Reject_Concatenation, Reject_To_Buffer, Reject_Mid, Reject_To_String, Reject_Format_String, Reject_Match, Reject_Resources, Wait_For_Delay => Unavailable_Delay);
   begin
      Execute (Code, Width, Args, Argument_Count, Budget, Empty, Environment,
               Scope, Result_Out, Calls_Left, Current_Sync);
   end Execute_Typed;

   function Run_Typed
     (Code : Bytes; Width : Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : Context; Scope : Natural; Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
      return Execution_Result
   is
      procedure Reject_Write
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : Integer_Width; Item : Datum; Status : out Write_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Path, Width, Item);
      begin
         Status := Write_Unsupported;
      end Reject_Write;
      procedure Begin_Read (Environment : in out Context; Scope : Natural; Allowed : out Boolean)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope);
      begin
         Allowed := True;
      end Begin_Read;
      procedure End_Read (Environment : in out Context; Scope : Natural)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope);
      begin
         null;
      end End_Read;
      procedure Reject_Definition
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Flags : Byte; Width : Integer_Width; Code : Bytes; Status : out Declaration_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Path, Flags, Width, Code);
      begin
         Status := Declaration_Unsupported;
      end Reject_Definition;
      procedure Execute is new Execute_Typed
        (Context, Context_Valid, Lookup, Get_Method, Reject_Write, Begin_Read, End_Read, Reject_Definition);
      Copy : Context := Environment;
      Result : Execution_Result;
   begin
      Execute (Code, Width, Args, Argument_Count, Budget, Copy, Scope, Result, Calls_Left, Current_Sync);
      return Result;
   end Run_Typed;

   function As_Values (Args : Arguments) return Value_Arguments is
      Result : Value_Arguments := [others => (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer)];
   begin
      for I in Args'Range loop
         Result (I) := (Value_Kind => Integer_Datum, Number => Args (I), Origin => AML_Decode.Ordinary_Integer);
         pragma Loop_Invariant (for all J in Args'First .. I =>
           Result (J).Value_Kind = Integer_Datum and then Result (J).Number = Args (J));
      end loop;
      return Result;
   end As_Values;

   function Run_Bound
     (Code : Bytes; Width : Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : Context; Scope : Natural; Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
      return Execution_Result
   is
      function Execute is new Run_Typed (Context, Context_Valid, Lookup, Get_Method);
   begin
      return Execute (Code, Width, As_Values (Args), Argument_Count, Budget,
                      Environment, Scope, Calls_Left, Current_Sync);
   end Run_Bound;

   type Empty_Context is null record;
   function No_Binding
     (Environment : Empty_Context; Scope : Natural; Path : AML_Names.Name_Result;
      Width : AML_Decode.Integer_Width)
      return Binding_Result
   is
      pragma Unreferenced (Environment, Scope, Path, Width);
   begin
      return (Status => Missing_Binding);
   end No_Binding;
   function No_Method (Environment : Empty_Context; ID : Natural)
      return Method_Definition
   is
      pragma Unreferenced (Environment, ID);
   begin
      return (Exists => False, Length => 0);
   end No_Method;
   function Empty_Valid (Environment : Empty_Context) return Boolean
     with Post => Empty_Valid'Result
   is
      pragma Unreferenced (Environment);
   begin
      return True;
   end Empty_Valid;
   function Unbound is new Run_Bound (Empty_Context, Empty_Valid, No_Binding, No_Method);
   function Run
     (Code : Bytes; Width : Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural)
      return Execution_Result is
     (Unbound (Code, Width, Args, Argument_Count, Budget, (null record), 0));
end AML_Execute;
