pragma Ada_2022;
with AML_Execute.Scalar_Transport;
with AML_Integers;
with AML_BCD;
with AML_Reference_Frames;
with AML_Logic;
with AML_Simple_Targets;
package body AML_Execute with SPARK_Mode is
   use AML_Decode;
   Event_Extension : constant Byte := 16#02#;
   Signal_Extension : constant Byte := 16#24#;
   Wait_Extension : constant Byte := 16#25#;
   Reset_Extension : constant Byte := 16#26#;
   Event_Type_Code : constant := 7;
   Alias_Op : constant Byte := 16#06#;
   Notify_Op : constant Byte := 16#86#;
   BreakPoint_Op : constant Byte := 16#CC#;
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
   Index_Field_Extension : constant Byte := 16#86#;
   Bank_Field_Extension : constant Byte := 16#87#;
   use type AML_Decode.Byte;
   use type AML_Coercions.Conversion_Status;
   use type AML_References.Reference_Kind;
   package String_Reader is new AML_Coercions.String_Readers
     (AML_Byte_Sources.Storage, AML_Byte_Sources.Source_Length, AML_Byte_Sources.Source_Byte);
   use type AML_References.Reference;
   use type AML_Frame_Handles.Frame_Handle;
   function Method_Level (Flags : Byte) return Sync_Level is
     (if (Flags and 8) /= 0 then Natural (Flags / 16) else 0);
   procedure Execute_From_Source
     (Code : Bytes; Width : Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Input : aliased Read_Context; Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0;
      Request : AML_Declaration_Contexts.Request := AML_Declaration_Contexts.Method_Request;
      Implementation : Method_Implementation := AML_Implementation)
   is
      Pending_Fatal : AML_Fatal.Observation;
      -- Invocation-local copied data, shared by recursive Run calls; no host effect.
      -- Loading policy belongs to the entire invocation, including real nested
      -- methods; declaration lifetime still follows each Run's own Intent.
      Loading_Effects_Restricted : constant Boolean := AML_Declaration_Contexts.Is_Module (Request);
      Frame_Cell_Count : constant Positive := AML_Frame_Handles.Cell_ID'Pos
        (AML_Frame_Handles.Cell_ID'Last) + 1;
      Incoming_Argument_Slots : constant Positive := Value_Arguments'Length;
      Return_Slots : constant Positive := 1;
      Scratch_Slots : constant Positive := Value_Arguments'Length;
      type Scratch_Role is
        (Pending_Result, Declaration_Source, Declaration_Index, Declaration_Count,
         Notification_Target, Reserved_Scratch_2, Reserved_Scratch_3);
      pragma Unreferenced (Reserved_Scratch_2, Reserved_Scratch_3);
      pragma Assert (Scratch_Role'Pos (Scratch_Role'Last) + 1 = Scratch_Slots);
      subtype Declaration_Scratch is Scratch_Role range Declaration_Source .. Declaration_Count;
      type Expression_Root is
        (Actual_0, Actual_1, Actual_2, Actual_3, Actual_4, Actual_5, Actual_6,
         Saved_Left, Saved_Middle, Saved_Right, Saved_Target, Saved_Reference,
         Saved_Result, Reserved_Root);
      -- Reserved positions are retained for the next operand-family slice.
      pragma Unreferenced
        (Actual_0, Actual_1, Actual_2, Actual_3, Actual_4, Actual_5, Actual_6);
      Roots_Per_Expression_Frame : constant Positive :=
        Expression_Root'Pos (Expression_Root'Last) + 1;
      Transport_Slots_Per_Frame : constant Positive :=
        Frame_Cell_Count + Incoming_Argument_Slots + Return_Slots + Scratch_Slots
          + Max_Expression_Depth * Roots_Per_Expression_Frame;
      Transport_Slot_Count : constant Positive :=
        AML_Frame_Handles.Max_Active_Frames * Transport_Slots_Per_Frame;
      package Transport is new AML_Execute.Scalar_Transport
        (Transport_Slot_Count, Transport_Slot_Count);
      use type Transport.Outcome;
      use type Transport.Identity_Kind;
      subtype Scalar_Location is Natural range 0 .. Transport_Slot_Count;
      type Argument_Locations is array (Value_Arguments'Range) of Scalar_Location;
      Root_Result_Location : Scalar_Location := 0;
      type Frame_Value (Tracked : Boolean := False) is record
         case Tracked is
            when True => Location : Transport.Slot;
            when False => Detached : Datum := (Integer_Datum, 0, Ordinary_Integer);
         end case;
      end record;
      package Frames is new AML_Reference_Frames
        (Frame_Value, (Tracked => False, Detached => (Integer_Datum, 0, Ordinary_Integer)));
      use type Frames.Result_Status;
      type Frame_Read_Result is record
         Value : Datum := (Integer_Datum, 0, Ordinary_Integer);
         Location : Natural range 0 .. Transport_Slot_Count := 0;
         Initialized : Boolean := False;
         Status : Frames.Result_Status := Frames.Invalid_Frame;
      end record;
      Values : Transport.State;
      Tracking : Boolean := False;
      function Frame_Location
        (Frame : AML_Frame_Handles.Frame_Handle; Cell : AML_Frame_Handles.Cell_ID)
         return Transport.Slot is
        ((Natural (AML_Frame_Handles.Index_Of (Frame)) - 1) * Transport_Slots_Per_Frame
          + AML_Frame_Handles.Cell_ID'Pos (Cell) + 1);
      function Argument_Location
        (Frame : AML_Frame_Handles.Frame_Handle; Index : Natural) return Transport.Slot is
        ((Natural (AML_Frame_Handles.Index_Of (Frame)) - 1) * Transport_Slots_Per_Frame
          + Frame_Cell_Count + Index + 1);
      function Result_Location (Frame : AML_Frame_Handles.Frame_Handle) return Transport.Slot is
        ((Natural (AML_Frame_Handles.Index_Of (Frame)) - 1) * Transport_Slots_Per_Frame
          + Frame_Cell_Count + Incoming_Argument_Slots + 1);
      function Scratch_Location
        (Frame : AML_Frame_Handles.Frame_Handle; Role : Scratch_Role) return Transport.Slot is
        (Result_Location (Frame) + 1 + Scratch_Role'Pos (Role));
      function Pending_Location (Frame : AML_Frame_Handles.Frame_Handle) return Transport.Slot is
        (Scratch_Location (Frame, Pending_Result));
      function Actual_Location
        (Frame : AML_Frame_Handles.Frame_Handle; Depth : Positive; Index : Natural)
         return Transport.Slot is
        ((Natural (AML_Frame_Handles.Index_Of (Frame)) - 1) * Transport_Slots_Per_Frame
          + Frame_Cell_Count + Incoming_Argument_Slots + Return_Slots + Scratch_Slots
          + (Depth - 1) * Roots_Per_Expression_Frame + Index + 1);
      function Expression_Location
        (Frame : AML_Frame_Handles.Frame_Handle; Depth : Positive; Role : Expression_Root)
         return Transport.Slot is
        (Actual_Location (Frame, Depth, Value_Arguments'First) + Expression_Root'Pos (Role));
      -- Empty reserved slots do not require a full ledger contract traversal.
      -- Clearing live roots still retains every helper pre/postcondition.
      procedure Clear_If_Present (Location : Transport.Slot) is
      begin
         if Tracking and then Transport.Occupied (Values, Location) then
            Transport.Clear (Values, Location);
         end if;
      end Clear_If_Present;
      procedure Refresh_Scalar_Location
        (Location : Scalar_Location; Item : in out Datum; Status : out Execution_Status)
      is
         Number : Integer_Value;
         Origin : Integer_Origin;
      begin
         Status := Returned;
         if not Tracking or else Location = 0 then return; end if;
         if not Transport.Occupied (Values, Location) then Status := Unsupported_Value; return; end if;
         if Transport.Kind (Values, Location) /= Transport.Owned_Scalar then return; end if;
         Read_Integer_Source (Environment, Transport.Source (Values, Location), Number, Origin, Status);
         if Status /= Returned then Status := Unsupported_Value; return; end if;
         Transport.Refresh_Owned_Cache (Values, Location, Number, Origin);
         Item := (Integer_Datum, Number, Origin);
      end Refresh_Scalar_Location;
      procedure Refresh_Compound_Location
        (Location : Scalar_Location; Item : Datum; Status : out Execution_Status)
      is
         Current : Datum := Item;
         Refreshed : Transport.Outcome;
      begin
         Status := Returned;
         if not Tracking or else Location = 0 then return; end if;
         if not Transport.Occupied (Values, Location) then Status := Unsupported_Value; return; end if;
         if Item.Value_Kind /= Object_Datum
           or else Transport.Snapshot (Values, Location) = Item
           or else not Transport.Compound_Identity_Matches (Values, Location, Item.Object)
         then return; end if;
         -- Only an owner-authenticated descriptor can refresh the cache. The
         -- exact source/generation and object kind must remain unchanged.
         Refresh_Value (Environment, Current, Status);
         if Status /= Returned or else Current /= Item then Status := Unsupported_Value; return; end if;
         Transport.Refresh_Compound_Cache (Values, Location, Item.Object, Refreshed);
         if Refreshed /= Transport.Ready then Status := Unsupported_Value; end if;
      end Refresh_Compound_Location;
      function Decoded (Item : Frames.Read_Result) return Frame_Read_Result is
      begin
         if Item.Status /= Frames.Ready or else not Item.Initialized then
            return (Initialized => Item.Initialized, Status => Item.Status, others => <>);
         elsif Item.Value.Tracked then
            return (Transport.Snapshot (Values, Item.Value.Location), Item.Value.Location,
                    True, Item.Status);
         else return (Item.Value.Detached, 0, True, Item.Status);
         end if;
      end Decoded;
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
      function Read_Frame_Reference
        (Store : Frames.State; Reference : AML_Frame_Handles.Cell_Handle)
         return Frame_Read_Result is
        (Decoded (Frames.Read_Reference (Store, Reference)));
      procedure Write_Frame_Cell
        (Frame : AML_Frame_Handles.Frame_Handle; Cell : AML_Frame_Handles.Cell_ID;
         Value : Datum; Status : out Frames.Result_Status;
         From : Scalar_Location := 0)
        with Pre => Context_Valid (Environment) and then Frames.Valid (Registry),
          Post => Context_Valid (Environment) and then Frames.Valid (Registry)
      is
         Existing : constant Frames.Read_Result := Frames.Read_Cell (Registry, Frame, Cell);
         Stored : Transport.Outcome;
      begin
         Status := Existing.Status;
         if Existing.Status /= Frames.Ready then return; end if;
         if Tracking then
            declare Location : constant Transport.Slot := Frame_Location (Frame, Cell); begin
               if From = 0 then Transport.Put (Values, Location, Value, Stored);
               else Transport.Share (Values, From, Location, Stored); end if;
               if Stored /= Transport.Ready then Status := Frames.Generation_Limit; return; end if;
               Frames.Write_Cell (Registry, Frame, Cell, (True, Location), Status);
               pragma Assert (Status = Frames.Ready);
               Publish_Frame_Root (Environment, Frame, Cell, True,
                 Transport.Root_Projection (Values, Location));
            end;
         else
            -- Legacy adapters without an invocation domain retain their
            -- detached arithmetic behavior; no identity authority is minted.
            Frames.Write_Cell (Registry, Frame, Cell, (False, Value), Status);
            pragma Assert (Status = Frames.Ready);
            Publish_Frame_Root (Environment, Frame, Cell, True, Value);
         end if;
      end Write_Frame_Cell;
      procedure Write_Frame_Reference
        (Reference : AML_Frame_Handles.Cell_Handle; Value : Datum;
         Status : out Frames.Result_Status)
        with Pre => Context_Valid (Environment) and then Frames.Valid (Registry),
          Post => Context_Valid (Environment) and then Frames.Valid (Registry)
      is
         Existing : constant Frames.Read_Result := Frames.Read_Reference (Registry, Reference);
      begin
         Status := Existing.Status;
         if Existing.Status /= Frames.Ready then return; end if;
         Write_Frame_Cell
           (AML_Frame_Handles.Bind_Frame (Domain,
              AML_Frame_Handles.Index_Of (Reference), AML_Frame_Handles.Generation_Of (Reference)),
            AML_Frame_Handles.Cell_Of (Reference), Value, Status);
      end Write_Frame_Reference;
      procedure Run
        (Code : Bytes; Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
         Width : Integer_Width;
         Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
         Scope : Natural; Result_Out : out Execution_Result;
         Calls_Left : Call_Budget; Method_ID : Natural; Method_Flags : Byte;
         Caller_Frame : AML_Frame_Handles.Frame_Handle;
         Intent : AML_Declaration_Contexts.Request;
         Body_Implementation : Method_Implementation;
         Sources : Argument_Locations; Returned_Location : out Scalar_Location)
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
      Arguments_Ready : Boolean := True;
      Operand_Location : Scalar_Location := 0;
      Return_Source : Scalar_Location := 0;
      Allowed : Boolean;
      Serialized_Entered : Boolean := False;
      Entry_Status : Execution_Status := Returned;
      Module_Root : constant Boolean := AML_Declaration_Contexts.Is_Module (Intent);
      Active_Lease : AML_Declaration_Contexts.Lease := AML_Declaration_Contexts.No_Lease;
      use type AML_Declaration_Contexts.Region_Reservation;
      Current_Lexical_Scope : Natural := Scope;
      Lexical_Depth : AML_Declaration_Contexts.Lexical_Depth := 0;
      procedure Pop_Lexical (Status : out Execution_Status) is
         Restored : Natural;
      begin
         Status := Unsupported_Value;
         if Lexical_Depth = 0 then return; end if;
         Leave_Container (Environment, Active_Lease, Current_Lexical_Scope, Restored, Status);
         if Status = Returned then
            Current_Lexical_Scope := Restored;
            Lexical_Depth := Lexical_Depth - 1;
         end if;
      end Pop_Lexical;
      procedure Execute_Body (Body_Result : out Execution_Result)
        with Pre => Context_Valid (Environment) and then not Body_Result'Constrained,
             Post => Context_Valid (Environment) and then Body_Result.Charged <= Budget,
             Always_Terminates,
             Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(3))
      is
      subtype Failure_Status is Execution_Failure;
      function Failure (Status : Execution_Status; Charged : Natural)
         return Execution_Result is
         Safe_Status : constant Payload_Free_Failure :=
           (if Status in Payload_Free_Failure then Status else Unsupported_Value);
      begin
         if Pending_Fatal.Present then
            return (Status => Fatal_Requested, Charged => Charged, Fatal => Pending_Fatal.Item);
         end if;
         return (Status => Safe_Status, Charged => Charged);
      end Failure;
      function Cell (Slot : Cell_Ordinal) return AML_Frame_Handles.Cell_ID
      is (AML_Frame_Handles.Cell_ID'Val (Slot));
      function Slot_Value (Slot : Cell_Ordinal) return Frame_Read_Result
      is (Decoded (Frames.Read_Cell (Registry, Call_Frame, Cell (Slot))));
      Offset : Natural := 0;
      Charged : Natural := 0;
      Limit : Natural := Code'Length;
      type Block_Kind is (Conditional_Block, Loop_Block, Container_Block);
      subtype Block_Depth_Count is Natural range 0 .. Block_Capacity;
      type Block_Frame is record
         Outer_Limit : Natural := 0;
         Resume : Natural := 0;
         Restart : Natural := 0;
         Kind : Block_Kind := Conditional_Block;
         Prior_Scope : Natural := 0;
      end record;
      Blocks : array (Positive range 1 .. Block_Capacity) of Block_Frame;
      Block_Depth : Block_Depth_Count := 0;
      Op : Byte;
      Value : Datum;
      State : Execution_Status;
      procedure Open_Container (Start : Natural; Status : out Execution_Status) is
         Header : constant AML_Container_Headers.Header_Result :=
           AML_Container_Headers.Read_Header
             (Code (Code'First + Start .. Code'First + (Limit - 1)));
         Next_Scope : Natural;
         Header_Cost : Natural;
      begin
         case Header.Status is
            when AML_Container_Headers.Accepted => null;
            when AML_Container_Headers.Truncated => Status := Truncated; return;
            when AML_Container_Headers.Bad_Package => Status := Bad_Package; return;
            when AML_Container_Headers.Bad_Name => Status := Bad_Name; return;
            when AML_Container_Headers.Unsupported => Status := Unsupported; return;
         end case;
         if Block_Depth = Block_Capacity then Status := Block_Limit; return; end if;
         -- Lexical scopes consume a distinct fixed owner ledger. Reject before
         -- the callback, so larger continuation storage cannot overflow it.
         if Lexical_Depth = AML_Declaration_Contexts.Lexical_Depth'Last then
            Status := Block_Limit; return;
         end if;
         Header_Cost := Header.Body_Offset - 1;
         if Header_Cost > Budget - Charged then Status := Budget_Exceeded; return; end if;
         Charged := Charged + Header_Cost;
         Enter_Container (Environment, Active_Lease, Current_Lexical_Scope,
           Header.Name, Header.Metadata, Next_Scope, Status);
         if Status /= Returned then return; end if;
         Block_Depth := Block_Depth + 1;
         Blocks (Block_Depth) := (Outer_Limit => Limit, Resume => Start + Header.Extent,
           Restart => Start, Kind => Container_Block, Prior_Scope => Current_Lexical_Scope);
         Current_Lexical_Scope := Next_Scope;
         Lexical_Depth := Lexical_Depth + 1;
         Limit := Start + Header.Extent;
         Offset := Start + Header.Body_Offset;
      end Open_Container;
      function Mutex_Super_Name_Lead (Position, Bound : Natural) return Boolean is
        (Position < Bound and then
          (AML_Names.Lead (Code (Code'First + Position))
           or else Code (Code'First + Position) in 16#5C# | 16#5E# | 16#2E# | 16#2F#
             | 16#60# .. 16#6E# | 16#71# | 16#83# | 16#88#));
      procedure Read_Reference_Value
        (Ref : AML_References.Reference; Value : out Datum;
         Provenance : out Scalar_Provenance; Status : out Execution_Status)
        with Pre => not Value'Constrained and then Context_Valid (Environment),
          Post => Context_Valid (Environment)
            and then Status in Returned | Uninitialized | Unsupported_Value | Value_Limit | Buffer_Field_Limit | Field_Indeterminate
      is
      begin
         Provenance := Detached_Provenance;
         Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
         if AML_References.Kind (Ref) = AML_References.Frame_Cell then
            declare Stored : constant Frame_Read_Result := Read_Frame_Reference
              (Registry, AML_References.Frame_Item (Ref)); begin
               if Stored.Status /= Frames.Ready then Status := Unsupported_Value;
               elsif not Stored.Initialized then Status := Uninitialized;
               else
                  Value := Stored.Value; Status := Returned;
                  if Stored.Location /= 0 and then Transport.Kind (Values, Stored.Location) = Transport.Owned_Scalar then
                     declare Number : Integer_Value; Origin : Integer_Origin; begin
                        Read_Integer_Source (Environment, Transport.Source (Values, Stored.Location), Number, Origin, Status);
                        if Status = Returned then
                           Value := (Integer_Datum, Number, Origin);
                           Provenance := (Owned_Integer_Scalar, Transport.Source (Values, Stored.Location));
                        else Status := Unsupported_Value;
                        end if;
                     end;
                  end if;
               end if;
            end;
         else
            Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Reference_Read,
              True, (Reference_Datum, Ref));
            Resolve_Reference (Environment, Ref, Width, Value, Provenance, Status);

            if Status not in Returned | Uninitialized | Unsupported_Value | Value_Limit | Buffer_Field_Limit | Field_Indeterminate then Status := Unsupported_Value; end if;
         end if;
         if Status /= Returned then Provenance := Detached_Provenance; end if;
      end Read_Reference_Value;
      -- Existing detached users retain their behavior until each ownership
      -- transfer/publication path is converted; this wrapper grants no authority.
      procedure Read_Reference_Value
        (Ref : AML_References.Reference; Value : out Datum; Status : out Execution_Status)
        with Pre => not Value'Constrained and then Context_Valid (Environment),
          Post => Context_Valid (Environment)
            and then Status in Returned | Uninitialized | Unsupported_Value | Value_Limit | Buffer_Field_Limit | Field_Indeterminate
      is
         Provenance : Scalar_Provenance;
      begin
         Read_Reference_Value (Ref, Value, Provenance, Status);
         Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Reference_Read,
           False, (Integer_Datum, 0, Ordinary_Integer));
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
            Status := (if Stored = Frames.Ready then Returned
                       elsif Stored = Frames.Generation_Limit then Value_Limit else Unsupported_Value);
         else
            Store_Reference (Environment, Ref, Width, Item, Status, Mode);
            if Status not in Returned | Unsupported_Value | Value_Limit | Empty_Buffer | Buffer_Field_Limit | Field_Indeterminate then Status := Unsupported_Value; end if;
         end if;
      end Write_Reference_Value;
      procedure Observe_Owned_Census
        (Source_Location : Transport.Slot; Result : out Owner_Census_Status)
      is
         Stamp : constant Transport.Slot_Identity :=
           Transport.Current_Slot_Identity (Values, Source_Location);
         Other_Scalar : Boolean;
         Scan : Transport.Outcome;
         Added : Owner_Census_Status := Census_Collecting;
         Finished : Owner_Census_Status;
         procedure Visit (Value : Datum; Accepted : out Boolean) is
         begin
            -- Only canonical transport envelopes enter this callback; held GC
            -- publications and local Datum copies are intentionally absent.
            Owner_Census_Add (Environment, Value, Added);
            Accepted := Added = Census_Collecting;
         end Visit;
         procedure Enumerate is new Transport.Visit_Compound_Owners (Visit);
      begin
         Result := Census_Invalid;
         Transport.Other_Owned_Lease (Values, Stamp, Other_Scalar, Scan);
         if Scan /= Transport.Ready then return; end if;
         Owner_Census_Begin (Environment, Transport.Source (Values, Source_Location), Result);
         if Result /= Census_Collecting then
            -- Finish consumes even an invalid/incomplete started workspace.
            Owner_Census_Finish (Environment, Finished);
            if Result /= Census_Incomplete then Result := Census_Invalid; end if;
            return;
         end if;
         Enumerate (Values, Stamp, Scan);
         Owner_Census_Finish (Environment, Finished);
         if Scan /= Transport.Ready then
            Result := (if Added = Census_Incomplete then Census_Incomplete else Census_Invalid);
         elsif Finished in Census_Shared | Census_No_Owner then
            Result := (if Other_Scalar then Census_Shared else Finished);
         elsif Finished = Census_Incomplete then Result := Census_Incomplete;
         else Result := Census_Invalid; end if;
      end Observe_Owned_Census;
      procedure Assign_Slot (Target : Byte; Item : Datum; Outcome : out Execution_Status;
         Store_Source : Scalar_Location := 0)
        with Pre => Target in 16#60# .. 16#6E# and then Frames.Valid (Registry),
          Post => Frames.Valid (Registry)
      is
         Status : Frames.Result_Status;
      begin
         declare
            Into : constant Transport.Slot := Frame_Location
              (Call_Frame, Cell (Natural (Target - 16#60#)));
            From : Scalar_Location := 0;
         begin
            if Tracking and then Store_Source /= 0 and then Item.Value_Kind = Integer_Datum then
               if not Transport.Occupied (Values, Store_Source)
                 or else Transport.Snapshot (Values, Store_Source) /= Item
               then Outcome := Unsupported_Value; return; end if;
               if Transport.Occupied (Values, Into)
                 and then Transport.Same_Identity (Values, Store_Source, Into)
               then Outcome := Returned; return; end if;
               -- Capture_Result has already moved a computed operand or added
               -- the lexical operand owner. Only one semantic owner is fresh.
               if Transport.Kind (Values, Store_Source) = Transport.Temporary_Scalar
                 and then Transport.Temporary_Owners (Values, Store_Source) = 1
               then From := Store_Source;
               elsif Transport.Kind (Values, Store_Source) = Transport.Owned_Scalar then
                  declare Census : Owner_Census_Status; begin
                     Observe_Owned_Census (Store_Source, Census);
                     if Census = Census_Invalid then Outcome := Unsupported_Value; return; end if;
                     -- Observation scaffolding only. No-owner is not Fresh:
                     -- cyclic-history/captured-name compatibility is unresolved.
                  end;
               end if;
            end if;
            Write_Frame_Cell (Call_Frame, Cell (Natural (Target - 16#60#)), Item, Status, From);
         end;
         Outcome := (if Status = Frames.Ready then Returned else Value_Limit);
      end Assign_Slot;
      -- All destination forms share the exact-cell argument indirection policy.
      procedure Write_Slot
        (Target : Byte; Item : Datum; Status : out Execution_Status;
         Prepared_Copy : Boolean := False; Store_Source : Scalar_Location := 0)
        with Pre => Target in 16#60# .. 16#6E#
      is
         Stored : Datum := Item;
      begin
         if Module_Root then Status := Unsupported_Value; return; end if;
         if Target >= 16#68# then
            declare Destination : constant Frame_Read_Result := Slot_Value (Natural (Target - 16#60#)); begin
               if Destination.Initialized and then Destination.Value.Value_Kind = Reference_Datum
                 and then AML_References.Kind (Destination.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell | AML_References.Root_Namespace
               then
                  Write_Reference_Value (Destination.Value.Ref, Item, Status, Prepared_Copy, Argument_Indirect_Target); return;
               end if;
            end;
         end if;
         declare Destination : constant Frame_Read_Result := Slot_Value (Natural (Target - 16#60#)); begin
            Capture_Slot_Value
              (Item, Destination.Value, Destination.Initialized, Prepared_Copy, Stored, Status);
         end;
         if Status /= Returned then return; end if;
         Assign_Slot (Target, Stored, Status, Store_Source);
      end Write_Slot;
      procedure Write_Target (Item : Datum; Status : out Execution_Status; Bound : Natural;
                              Store_Source : Scalar_Location := 0)
        with Pre => Offset <= Bound and then Bound <= Code'Length and then Context_Valid (Environment),
          Post => Context_Valid (Environment) and then Offset <= Bound and then Offset >= Offset'Old
            and then Status in Returned | Truncated | Unsupported | Unsupported_Value | Unknown_Name | Value_Limit | Empty_Buffer | Buffer_Field_Limit | Field_Indeterminate
            and then (if Status /= Returned then
              Offset = Offset'Old and then Registry = Registry'Old)
            and then (if Status = Returned then Offset > Offset'Old)
      is
         Target : Byte;
         Path : AML_Names.Name_Result;
         Written_Status : Write_Status;
         use type AML_Names.Parse_Status;
      begin
         if Offset = Bound then Status := Truncated; return; end if;
         Target := Code (Code'First + Offset);
         if Target in 16#60# .. 16#6E# then
            Write_Slot (Target, Item, Status, Store_Source => Store_Source);
            if Status /= Returned then
               if Status not in Value_Limit | Unsupported_Value | Field_Indeterminate then Status := Unsupported; end if;
               return;
            end if;
         elsif Target = Extended_Op then
            if Bound - Offset < Debug_Target_Bytes then Status := Truncated; return; end if;
            if Code (Code'First + Offset + 1) /= Debug_Extension then
               Status := Unsupported; return;
            end if;
            if Loading_Effects_Restricted then Status := Unsupported; return; end if;
            Observe_Debug (Environment, Current_Lexical_Scope, Offset, Width, Item);
            Offset := Offset + Debug_Target_Bytes;
            Status := Returned; return;
         elsif Target not in 0 | 1 | 16#FF# then
            if not AML_Names.Lead (Target) and then Target not in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
               Status := Unsupported; return;
            end if;
            Path := AML_Names.Read_Name
              (Code (Code'First + Offset .. Code'First + (Bound - 1)));
            if Path.Kind = AML_Names.Truncated then Status := Truncated; return; end if;
            if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
               Status := Unsupported; return;
            end if;
            Write (Environment, Current_Lexical_Scope, Path, Width, Item, Written_Status);
            case Written_Status is
               when Write_Missing => Status := Unknown_Name; return;
               when Write_Value_Limit => Status := Value_Limit; return;
               when Write_Buffer_Field_Limit => Status := Buffer_Field_Limit; return;
               when Write_Empty_Buffer => Status := Empty_Buffer; return;
               when Write_Field_Indeterminate => Status := Field_Indeterminate; return;
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
      type Concatenation_Provenance is (Stored_Value, Expression_Result);
      Staged_Slot : Slot_Number := 0;
      Staged_Identity : Scalar_Location := 0;
      Staged_Provenance : Concatenation_Provenance := Stored_Value;
      -- Local/Arg reads are deferred until sibling expressions complete.
      -- Named integer reads capture their value when first evaluated.
      Name_Reference_Hop_Limit : constant Positive := 64;
      procedure Resolve_Name_Value (Item : in out Datum; Provenance : out Scalar_Provenance; Status : out Execution_Status)
        with Pre => not Item'Constrained,
          Post => Status in Returned | Uninitialized | Unsupported_Value | Value_Limit | Buffer_Field_Limit | Field_Indeterminate
      is
         Seen : array (Positive range 1 .. Name_Reference_Hop_Limit) of AML_References.Reference;
         Used : Natural range 0 .. Name_Reference_Hop_Limit := 0;
         Ref : AML_References.Reference;
      begin
         Status := Returned;
         Provenance := Detached_Provenance;
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
            Read_Reference_Value (Ref, Item, Provenance, Status);
            if Status not in Returned | Uninitialized | Value_Limit | Buffer_Field_Limit | Field_Indeterminate then Status := Unsupported_Value; return; end if;
            if Status /= Returned then return; end if;
         end loop;
      end Resolve_Name_Value;
      procedure Resolve_Slot_With_Source
        (Slot : Slot_Number; Item : in out Datum;
         Provenance : out Scalar_Provenance; Status : out Execution_Status)
        with Pre => not Item'Constrained,
          Post => Status in Returned | Uninitialized | Missing_Argument | Unsupported_Value | Value_Limit | Buffer_Field_Limit | Field_Indeterminate
      is
      begin
         Status := Returned;
         Provenance := Detached_Provenance;
         if Slot > 0 then
            declare Stored : constant Frame_Read_Result := Slot_Value (Slot - 1); begin
               if Stored.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
               if not Stored.Initialized then
                  Status := (if Slot <= 8 then Uninitialized else Missing_Argument); return;
               end if;
               Item := Stored.Value;
               Refresh_Scalar_Location (Stored.Location, Item, Status);
               if Status /= Returned then return; end if;
            end;
         end if;
         declare Refreshed : Execution_Status; begin
            Refresh_Value (Environment, Item, Refreshed);
            if Refreshed /= Returned then Status := Unsupported_Value; return; end if;
         end;
         Resolve_Name_Value (Item, Provenance, Status);
      end Resolve_Slot_With_Source;
      -- Admit the result of a real resolution, not an equal-bits substitute.
      -- Lexical cells remain unchanged; changed objects live in operand roles.
      procedure Admit_Resolved_Value
        (Item : in out Datum; Provenance : Scalar_Provenance;
         Identity : in out Scalar_Location; Destination : Transport.Slot;
         Status : out Execution_Status; Preserve_Prior : Boolean := False)
      is
         Prior_Stamp : constant Transport.Slot_Identity :=
           (if Tracking and then Identity /= 0 then
              Transport.Current_Slot_Identity (Values, Identity)
            else Transport.No_Slot_Identity) with Ghost;
         Number : Integer_Value;
         Origin : Integer_Origin;
         Stored : Transport.Outcome;
      begin
         Status := Returned;
         if not Tracking then return; end if;
         if Provenance.Kind = Owned_Integer_Scalar then
            Read_Integer_Source (Environment, Provenance.Source, Number, Origin, Status);
            if Status /= Returned or else Item.Value_Kind /= Integer_Datum
              or else Number /= Item.Number or else Origin /= Item.Origin
            then Status := Unsupported_Value; return; end if;
            Transport.Put_Owned (Values, Destination, Number, Origin, Provenance.Source, Stored);
         else
            Refresh_Compound_Location (Identity, Item, Status);
            if Status /= Returned then return; end if;
            if Identity /= 0 then
               if not Transport.Occupied (Values, Identity) then
                  Status := Unsupported_Value; return;
               end if;
               if Transport.Kind (Values, Identity) /= Transport.Non_Scalar then
                  Refresh_Scalar_Location (Identity, Item, Status);
                  if Status /= Returned then return; end if;
               end if;
               if Transport.Snapshot (Values, Identity) = Item then return; end if;
               -- Only a resolved non-scalar can legitimately change kind or
               -- object here. Scalar identity is never inferred from new bits.
               if Transport.Kind (Values, Identity) /= Transport.Non_Scalar then
                  Status := Unsupported_Value; return;
               end if;
            end if;
            Transport.Put (Values, Destination, Item, Stored);
         end if;
         if Stored /= Transport.Ready then Status := Value_Limit; return; end if;
         if not Preserve_Prior and then Identity /= 0 and then Identity /= Destination then
            -- The resolved operand now owns the value. Its consumed reference
            -- descriptor is no longer an additional semantic owner.
            Clear_If_Present (Identity);
            pragma Assert (not Transport.Occupied (Values, Identity));
         elsif Preserve_Prior and then Identity /= 0 and then Identity /= Destination then
            pragma Assert (Transport.Matches_Slot (Values, Prior_Stamp));
         end if;
         Identity := Destination;
      end Admit_Resolved_Value;
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
      procedure Read_Reference_At_Entry
        (Ref : AML_References.Reference; Value : out Datum; Status : out Execution_Status)
        renames Read_Reference_Value;
      procedure Read_Reference_Provenance_At_Entry
        (Ref : AML_References.Reference; Value : out Datum;
         Provenance : out Scalar_Provenance; Status : out Execution_Status)
        renames Read_Reference_Value;
      procedure Evaluate_Operand (V : out Datum; S : out Execution_Status; Allow_No_Return : Boolean := False; Require_Integer : Boolean := False; Defer_Final : Boolean := False; Package_Name : Name_Reservation_Token := No_Name_Reservation_Token)
        with Always_Terminates,
             Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(1)),
             Pre => Context_Valid (Environment) and then not V'Constrained and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget,
             Post => Context_Valid (Environment) and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget
               and then Charged >= Charged'Old and then Offset >= Offset'Old
               and then (if S = Returned then Charged > Charged'Old)
               and then S /= Object_Returned and then S /= Reference_Returned and then S /= Metadata_Returned
      is
         -- This bound is local to one expression evaluation. Early returns
         -- discard it; the outer statement/block Limit never needs unwinding.
         Parse_Limit : Natural := Limit;
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
         type BCD_Operation is (Not_BCD, Decode_BCD, Encode_BCD);
         type Frame is record
            Op : Byte := 16#72#;
            BCD : BCD_Operation := Not_BCD;
            Is_Conditional_Reference : Boolean := False;
            Is_Buffer_Count : Boolean := False;
            Is_Package : Boolean := False;
            Package_Count_Pending : Boolean := False;
            Package_Skip_Child : Boolean := False;
            Package_Is_Variable : Boolean := False;
            Package_End, Package_Outer_Bound, Package_Declared, Package_Seen : Natural := 0;
            Package_Token : Runtime_Package_Token := No_Runtime_Package;
            Package_Parent : Runtime_Package_Token := No_Runtime_Package;
            Package_Name : Name_Reservation_Token := No_Name_Reservation_Token;
            Buffer_Start, Buffer_End, Buffer_Count_Start : Natural := 0;
            Buffer_Parent_Bound : Natural := 0;
            Buffer_Integer_Result : Boolean := False;
            Source_Slot : Byte := 0;
            Source_Name : AML_Names.Name_Result := (Kind => AML_Names.Truncated);
            Left : Datum := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
            Left_Slot : Slot_Number := 0;
            Left_Location : Scalar_Location := 0;
            Left_Concat, Right_Concat : Concatenation_Operand :=
              (Kind => Data_Operand,
               Value => (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer));
            Left_Provenance, Right_Provenance : Concatenation_Provenance := Stored_Value;
            Has_Left : Boolean := False;
            Has_Middle : Boolean := False;
            Has_Right : Boolean := False;
            Match_Byte_1, Match_Byte_2 : Byte := 0;
            Middle : Datum := (Integer_Datum, 0, Ordinary_Integer);
            Middle_Slot : Slot_Number := 0;
            Middle_Location : Scalar_Location := 0;
            Middle_Provenance : Concatenation_Provenance := Stored_Value;
            Collecting_Targets : Boolean := False;
            Targets_Ready : Boolean := False;
            Target_Count : Target_Number := 0;
            Target_Total : Target_Number := 0;
            Targets : Target_List := [others => <>];
            Right : Datum := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
            Right_Slot : Slot_Number := 0;
            Right_Location : Scalar_Location := 0;
            Is_Call : Boolean := False;
            Method_ID : Natural := 0;
            Parameters : Natural range 0 .. 7 := 0;
            Given : Natural range 0 .. 7 := 0;
            Actuals : Value_Arguments := [others => (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer)];
            Locations : Argument_Locations := [others => 0];
            Slots : Argument_Slots := [others => 0];
         end record;
         Stack : array (Positive range 1 .. Max_Expression_Depth) of Frame;
         Depth : Natural range 0 .. Max_Expression_Depth := 0;
         procedure Collect_Targets
           (Count : in out Target_Number; Total : Target_Number;
            Targets : in out Target_List; Waiting : out Boolean;
            Status : out Execution_Status)
           with Pre => Context_Valid (Environment)
                  and then Offset <= Parse_Limit and then Parse_Limit <= Limit and then Limit <= Code'Length
                  and then Count <= Total,
                Post => Context_Valid (Environment)
                  and then Offset <= Parse_Limit and then Offset >= Offset'Old
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
               pragma Loop_Invariant (Offset <= Parse_Limit);
               pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
               pragma Loop_Invariant (Count <= Total);
               pragma Loop_Variant (Decreases => Total - Count);
               if Offset = Parse_Limit then Status := Truncated; return; end if;
               B := Code (Code'First + Offset);
               if B in 16#71# | 16#83# | 16#88# then Waiting := True; return; end if;
               T := (others => <>);
               if B in 16#60# .. 16#6E# then
                  if Module_Root then Status := Unsupported_Value; return; end if;
                  T.Kind := Slot_Target; T.Slot := B;
                  Offset := Offset + 1;
               elsif B = Extended_Op then
                  if Parse_Limit - Offset < Debug_Target_Bytes then Status := Truncated; return; end if;
                  if Code (Code'First + Offset + 1) /= Debug_Extension then
                     Status := Unsupported; return;
                  end if;
                  if Loading_Effects_Restricted then Status := Unsupported; return; end if;
                  T.Kind := Debug_Target; T.Position := Offset;
                  Offset := Offset + Debug_Target_Bytes;
               elsif B in 0 | 1 | 16#FF# then
                  Offset := Offset + 1;
               else
                  if not AML_Names.Lead (B) and then B not in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                     Status := Unsupported; return;
                  end if;
                  T.Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)));
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
         procedure Read_Reference_Value
           (Ref : AML_References.Reference; Value : out Datum; Status : out Execution_Status)
         is
         begin
            Publish_Live ([(Reference_Datum, Ref)]);
            Read_Reference_At_Entry (Ref, Value, Status);
         end Read_Reference_Value;
         procedure Resolve_Expression_Slot
           (Slot : Slot_Number; Item : in out Datum; Status : out Execution_Status;
            Identity : in out Scalar_Location; Role : Expression_Root := Reserved_Root)
         is
            Provenance : Scalar_Provenance;
         begin
            Publish_Live ([Item]);
            Resolve_Slot_With_Source (Slot, Item, Provenance, Status);
            if Status /= Returned then return; end if;
            if Tracking and then Slot /= 0 then
               Identity := Frame_Location (Call_Frame, Cell (Slot - 1));
            end if;
            Admit_Resolved_Value (Item, Provenance, Identity,
              Expression_Location (Call_Frame, Positive'Max (1, Depth), Role), Status,
              Preserve_Prior => Slot /= 0);
         end Resolve_Expression_Slot;
         procedure Resolve_Expression_Slot
           (Slot : Slot_Number; Item : in out Datum; Status : out Execution_Status)
         is
            Identity : Scalar_Location := 0;
         begin
            Resolve_Expression_Slot (Slot, Item, Status, Identity);
         end Resolve_Expression_Slot;
         procedure Resolve_Store_Source
           (Slot : in out Slot_Number; Item : in out Datum;
            Identity : in out Scalar_Location; Location : Transport.Slot;
            Status : out Execution_Status)
         is
            Provenance : Scalar_Provenance;
            Number : Integer_Value;
            Origin : Integer_Origin;
            Stored : Transport.Outcome;
         begin
            Publish_Live ([Item]);
            Resolve_Slot_With_Source (Slot, Item, Provenance, Status);
            if Status /= Returned then return; end if;
            if Provenance.Kind = Owned_Integer_Scalar then
               Read_Integer_Source (Environment, Provenance.Source, Number, Origin, Status);
               if Status /= Returned or else Item.Value_Kind /= Integer_Datum
                 or else Number /= Item.Number or else Origin /= Item.Origin
               then Status := Unsupported_Value; return; end if;
               if Tracking then
                  Transport.Put_Owned (Values, Location, Number, Origin, Provenance.Source, Stored);
                  if Stored /= Transport.Ready then Status := Value_Limit; return; end if;
                  Identity := Location;
                  -- The resolved object is not the lexical reference cell.
                  Slot := 0;
               end if;
            elsif Slot = 0 then
               Refresh_Scalar_Location (Identity, Item, Status);
            end if;
         end Resolve_Store_Source;
         procedure Apply_Target (T : Target_Descriptor; Item : Datum; Status : out Execution_Status;
                                 Mode : Reference_Store_Mode := Direct_Target)
           with Pre => Context_Valid (Environment),
                Post => Context_Valid (Environment)
                  and then Status in Returned | Unsupported | Unknown_Name | Unsupported_Value | Value_Limit | Empty_Buffer | Buffer_Field_Limit | Field_Indeterminate
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
                  if Loading_Effects_Restricted then Status := Unsupported; return; end if;
                  Observe_Debug (Environment, Current_Lexical_Scope, T.Position, Width, Item);
               when Slot_Target =>
                  if T.Slot not in 16#60# .. 16#6E# then Status := Unsupported; return; end if;
                  Write_Slot (T.Slot, Item, Status);
               when Name_Target =>
                  if Mode = Explicit_Result_Target then
                     Lookup (Environment, Input, Current_Lexical_Scope, T.Path, Width,
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
                  Write (Environment, Current_Lexical_Scope, T.Path, Width, Item, Written_Status);
                  case Written_Status is
                     when Written => null;
                     when Write_Missing => Status := Unknown_Name;
                     when Write_Value_Limit => Status := Value_Limit;
                     when Write_Buffer_Field_Limit => Status := Buffer_Field_Limit;
                     when Write_Empty_Buffer => Status := Empty_Buffer;
                     when Write_Field_Indeterminate => Status := Field_Indeterminate;
                     when Write_Unsupported => Status := Unsupported;
                  end case;
               when Reference_Target =>
                  Write_Reference_Value (T.Ref, Item, Status, Mode => Mode);
                  if Status not in Returned | Unsupported_Value | Value_Limit | Empty_Buffer | Buffer_Field_Limit | Field_Indeterminate then Status := Unsupported_Value; end if;
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
                  declare Cell_Data : constant Frame_Read_Result := Read_Frame_Reference
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
               when Name_Target => Destination := (Named_Attachment, Current_Lexical_Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Direct_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frame_Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell | AML_References.Root_Namespace
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
               if Stored /= Frames.Ready then Status := Value_Limit; return; end if;
               pragma Assert (Stored = Frames.Ready);
            elsif Publish_Slot then Assign_Slot (Target.Slot, Value, Status);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Current_Lexical_Scope, Target.Position, Width, Value);
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
                  declare Cell_Data : constant Frame_Read_Result := Read_Frame_Reference
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
               when Name_Target => Destination := (Named_Attachment, Current_Lexical_Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Direct_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frame_Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell | AML_References.Root_Namespace
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
               if Stored /= Frames.Ready then Status := Value_Limit; return; end if;
               pragma Assert (Stored = Frames.Ready);
            elsif Publish_Slot then Assign_Slot (Target.Slot, Value, Status);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Current_Lexical_Scope, Target.Position, Width, Value);
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
                  declare Cell_Data : constant Frame_Read_Result := Read_Frame_Reference
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
               when Name_Target => Destination := (Named_Attachment, Current_Lexical_Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Direct_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frame_Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell | AML_References.Root_Namespace
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
               if Stored /= Frames.Ready then Status := Value_Limit; return; end if;
               pragma Assert (Stored = Frames.Ready);
            elsif Publish_Slot then Assign_Slot (Target.Slot, Value, Status);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Current_Lexical_Scope, Target.Position, Width, Value);
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
                  declare Cell_Data : constant Frame_Read_Result := Read_Frame_Reference
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
               when Name_Target => Destination := (Named_Attachment, Current_Lexical_Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Explicit_Result_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frame_Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell | AML_References.Root_Namespace
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
               if Stored /= Frames.Ready then Status := Value_Limit; return; end if;
               pragma Assert (Stored = Frames.Ready);
            elsif Publish_Slot then Assign_Slot (Target.Slot, Value, Status);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Current_Lexical_Scope, Target.Position, Width, Value);
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
                  declare Cell_Data : constant Frame_Read_Result := Read_Frame_Reference
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
               when Name_Target => Destination := (Named_Attachment, Current_Lexical_Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Explicit_Result_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frame_Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell | AML_References.Root_Namespace
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
            if Publish_Slot then Assign_Slot (Target.Slot, Prepared, Status);
            elsif Destination.Kind = Prepared_Cell_Copy then
               Write_Frame_Reference (AML_References.Frame_Item (Cell_Reference), Prepared, Stored);
               if Stored /= Frames.Ready then Status := Value_Limit; return; end if;
               pragma Assert (Stored = Frames.Ready);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Current_Lexical_Scope, Target.Position, Width, Value);
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
                  declare Cell_Data : constant Frame_Read_Result := Read_Frame_Reference
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
               when Name_Target => Destination := (Named_Attachment, Current_Lexical_Scope, Target.Path);
               when Reference_Target => Select_Reference (Target.Ref, Explicit_Result_Target);
               when Slot_Target =>
                  if Target.Slot not in 16#60# .. 16#6E# then Status := Unsupported_Value; return; end if;
                  declare Previous : constant Frame_Read_Result := Slot_Value (Natural (Target.Slot - 16#60#)); begin
                     if Previous.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
                     if Target.Slot >= 16#68# and then Previous.Initialized
                       and then Previous.Value.Value_Kind = Reference_Datum
                       and then AML_References.Kind (Previous.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell | AML_References.Root_Namespace
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
            if Publish_Slot then Assign_Slot (Target.Slot, Prepared, Status);
            elsif Destination.Kind = Prepared_Cell_Copy then
               Write_Frame_Reference (AML_References.Frame_Item (Cell_Reference), Prepared, Stored);
               if Stored /= Frames.Ready then Status := Value_Limit; return; end if;
               pragma Assert (Stored = Frames.Ready);
            elsif Target.Kind = Debug_Target then Observe_Debug (Environment, Current_Lexical_Scope, Target.Position, Width, Value);
            end if;
         end Attach_Formatted_String;
         procedure Resolve_Expression_Reference
           (Ref : AML_References.Reference; Item : out Datum;
            Identity : in out Scalar_Location; Role : Expression_Root;
            Status : out Execution_Status)
         is
            Provenance : Scalar_Provenance;
         begin
            Publish_Live ([(Reference_Datum, Ref)]);
            Read_Reference_Provenance_At_Entry (Ref, Item, Provenance, Status);
            if Status /= Returned then return; end if;
            Admit_Resolved_Value (Item, Provenance, Identity,
              Expression_Location (Call_Frame, Positive'Max (1, Depth), Role), Status);
         end Resolve_Expression_Reference;
         procedure Resolve_Concatenation
           (Operand : in out Concatenation_Operand; Item : Datum; Slot : Slot_Number;
            Provenance : Concatenation_Provenance; Status : out Execution_Status;
            Identity : in out Scalar_Location; Role : Expression_Root)
         is
            Value : Datum := Item;
         begin
            Status := Returned;
            if Operand.Kind = Namespace_Operand then return; end if;
            Resolve_Expression_Slot (Slot, Value, Status, Identity, Role);
            if Status /= Returned then return; end if;
            if Provenance = Expression_Result and then Slot = 0 and then Value.Value_Kind = Reference_Datum
              and then AML_References.Kind (Value.Ref) = AML_References.Package_Slot
            then
               declare Ref : constant AML_References.Reference := Value.Ref; begin
                  Resolve_Expression_Reference (Ref, Value, Identity, Role, Status);
                  if Status /= Returned then return; end if;
               end;
            end if;
            if Value.Value_Kind = Reference_Datum
              and then AML_References.Kind (Value.Ref) = AML_References.Frame_Cell
            then
               declare Cell_Data : constant Frame_Read_Result := Read_Frame_Reference
                 (Registry, AML_References.Frame_Item (Value.Ref)); begin
                  if Cell_Data.Status /= Frames.Ready then Status := Unsupported_Value; return; end if;
               end;
            end if;
            Operand := (Data_Operand, Value);
         end Resolve_Concatenation;
         procedure Resolve_Mid_Operand
           (Item : in out Datum; Slot : Slot_Number;
            Provenance : Concatenation_Provenance; Status : out Execution_Status;
            Identity : in out Scalar_Location; Role : Expression_Root)
         is
         begin
            Resolve_Expression_Slot (Slot, Item, Status, Identity, Role);
            if Status /= Returned then return; end if;
            -- ACPICA resolves an expression-result package Index once. A
            -- reference loaded from a frame cell remains a reference operand.
            if Provenance = Expression_Result and then Slot = 0
              and then Item.Value_Kind = Reference_Datum
              and then AML_References.Kind (Item.Ref) = AML_References.Package_Slot
            then
               declare Ref : constant AML_References.Reference := Item.Ref; begin
                  Resolve_Expression_Reference (Ref, Item, Identity, Role, Status);
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
                  Resolve_Expression_Slot (Natural (Target.Slot - 16#60#) + 1, Value, Status);
                  -- ACPI 6.6 19.3.5.8.1: an Arg ObjectReference read yields
                  -- its target. Locals retain references on ordinary reads.
                  -- Keep the syntactic Arg target for the indirect storeback.
                  if Status = Returned and then Target.Slot in 16#68# .. 16#6E#
                    and then Value.Value_Kind = Reference_Datum
                    and then AML_References.Kind (Value.Ref) in
                      AML_References.Frame_Cell | AML_References.Named_Cell
                  then
                     declare Ref : constant AML_References.Reference := Value.Ref; begin
                        Read_Reference_Value (Ref, Value, Status);
                     end;
                     if Status = Returned then Publish_Live ([Value]); end if;
                  end if;
               when Name_Target =>
                  Lookup (Environment, Input, Current_Lexical_Scope, Target.Path, Width,
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
            -- Only the Arg read above follows its authenticated RefOf target.
            -- Remaining references are not recursively chased or converted.
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
                 Unsupported_Value | Value_Limit | Buffer_Field_Limit | Field_Indeterminate | Division_By_Zero
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
           (F : in out Frame; Right_Slot : Slot_Number;
            Value : in out Datum; Status : out Execution_Status)
           with Pre => Context_Valid (Environment) and then not Value'Constrained,
             Post => Context_Valid (Environment)
               and then Status /= Object_Returned and then Status /= Reference_Returned and then Status /= Metadata_Returned
               and then (if Status = Returned then Value.Value_Kind = Integer_Datum)
         is
            Left_Item : Datum;
            Conversion : AML_Coercions.Result;
            Comparison : Integer_Value;
         begin
            Left_Item := F.Left;
            Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Numeric_Right,
              True, Value);
            Resolve_Expression_Slot (F.Left_Slot, Left_Item, Status, F.Left_Location, Saved_Left);
            if Status /= Returned then
               Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Numeric_Right,
                 False, (Integer_Datum, 0, Ordinary_Integer));
               return;
            end if;
            -- The left field read can return a fresh Buffer absent from F.Left.
            -- Keep it across the independently allocating right resolution.
            Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Numeric_Left,
              True, Left_Item);
            Resolve_Expression_Slot (Right_Slot, Value, Status, F.Right_Location, Saved_Right);
            Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Numeric_Left,
              False, (Integer_Datum, 0, Ordinary_Integer));
            Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Numeric_Right,
              False, (Integer_Datum, 0, Ordinary_Integer));
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
               and then Offset <= Parse_Limit and then Parse_Limit <= Limit and then Limit <= Code'Length,
             Post => Context_Valid (Environment)
               and then Offset <= Parse_Limit and then Offset >= Offset'Old
               and then Status /= Object_Returned and then Status /= Reference_Returned and then Status /= Metadata_Returned
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
         Have_Value : Boolean;
         Waiting_For_Target : Boolean;
         Pending_Slot : Slot_Number;
         Pending_Identity : Scalar_Location := 0;
         Pending_Concat : Concatenation_Operand;
         Pending_Provenance : Concatenation_Provenance := Stored_Value;
         Left_Item, Argument_Item : Datum;
         Conversion : AML_Coercions.Result;
         Text : String_Span_Result;
         Buffer_Item : AML_Buffer_Layouts.Layout_Result;
         Package_Item : Package_Result;
         Result_Identity : Scalar_Location := 0;
         Final_Provenance : Scalar_Provenance := Detached_Provenance;
         No_Op_Integer : Boolean := False;
         Real_Integer_Conversion : Boolean := False;
         -- A lexical slot remains unresolved until the opcode consumes it.
         -- Captured expression results instead own a distinct semantic lease.
         procedure Capture_Operand
           (Item : Datum; Lexical : Slot_Number; Identity : Scalar_Location;
            Role : Expression_Root; Captured : out Scalar_Location)
         is
            Stored : Transport.Outcome;
            Location : constant Transport.Slot := Expression_Location (Call_Frame, Depth, Role);
         begin
            Captured := 0;
            if not Tracking then return; end if;
            if Lexical /= 0 then
               Clear_If_Present (Location);
               return;
            end if;
            if Identity = 0 then
               Transport.Put (Values, Location, Item, Stored);
            elsif Identity = Pending_Location (Call_Frame) then
               Transport.Move (Values, Identity, Location, Stored);
            else
               Transport.Share (Values, Identity, Location, Stored);
            end if;
            if Stored /= Transport.Ready then S := Value_Limit; return; end if;
            Captured := Location;
         end Capture_Operand;
         procedure Check_Computed_Integer
           (Item : Datum; Lexical : Slot_Number; Identity : Scalar_Location;
            Provenance : Concatenation_Provenance) is
         begin
            if Tracking and then Lexical = 0 and then Item.Value_Kind = Integer_Datum
              and then Provenance = Expression_Result
            then
               pragma Assert (Identity /= 0);
               pragma Assert (Transport.Occupied (Values, Identity));
               pragma Assert (Transport.Kind (Values, Identity) in Transport.Temporary_Scalar | Transport.Owned_Scalar);
               pragma Assert (Transport.Snapshot (Values, Identity) = Item);
            end if;
         end Check_Computed_Integer;
         -- Resolving a reference may create a fresh scalar. Only a direct
         -- integer cell/result whose full payload is unchanged can supply the
         -- existing temporary identity. Owner-backed scalar admission is separate.
         type Result_Capture_Policy is (Transfer_Operand, Share_Conversion_Result);
         procedure Capture_Result
           (Item : Datum; Lexical : Slot_Number; Identity : Scalar_Location;
            Policy : Result_Capture_Policy)
         is
            Candidate : Scalar_Location := Identity;
            Stored : Transport.Outcome;
            Location : constant Transport.Slot := Expression_Location (Call_Frame, Depth, Saved_Result);
         begin
            Result_Identity := 0;
            if not Tracking then return; end if;
            if Lexical /= 0 and then Candidate = 0 then
               Candidate := Frame_Location (Call_Frame, Cell (Lexical - 1));
            end if;
            declare Status : Execution_Status; begin
               Refresh_Compound_Location (Candidate, Item, Status);
               if Status /= Returned then S := Status; return; end if;
            end;
            if Candidate /= 0 and then Transport.Occupied (Values, Candidate)
              and then Transport.Kind (Values, Candidate) = Transport.Owned_Scalar
            then
               declare Current : Datum := Item; Status : Execution_Status; begin
                  Refresh_Scalar_Location (Candidate, Current, Status);
                  if Status /= Returned or else Current /= Item then S := Unsupported_Value; return; end if;
               end;
            end if;
            if Candidate /= 0 and then Transport.Occupied (Values, Candidate)
              and then Transport.Snapshot (Values, Candidate) = Item
            then
               if Policy = Transfer_Operand and then Lexical = 0 then
                  -- Store returns its operand object directly. Moving its owning
                  -- slot avoids counting an extra result owner during the write.
                  Transport.Move (Values, Candidate, Location, Stored);
                  if Stored = Transport.Ready then
                     pragma Assert (Transport.Occupied (Values, Location));
                     pragma Assert (Candidate = Location or else
                       not Transport.Occupied (Values, Candidate));
                  end if;
               else
                  -- A lexical read acquires an operand owner from its cell;
                  -- no-op conversion adds a result reference to its operand.
                  Transport.Share (Values, Candidate, Location, Stored);
                  if Stored = Transport.Ready then
                     pragma Assert
                       (if Transport.Kind (Values, Candidate) = Transport.Non_Scalar
                        then Transport.Kind (Values, Location) = Transport.Non_Scalar
                          and then Transport.Snapshot (Values, Candidate) = Item
                          and then Transport.Snapshot (Values, Location) = Item
                        else Transport.Same_Identity (Values, Candidate, Location));
                  end if;
               end if;
            else
               Transport.Put (Values, Location, Item, Stored);
            end if;
            if Stored /= Transport.Ready then S := Value_Limit; return; end if;
            Result_Identity := Location;
         end Capture_Result;
         -- Explicit integer-producing opcode completion acts on the original
         -- result identity. Ordinary reads and public dereference do not call it.
         procedure Complete_Integer_Result
           (Item : in out Datum; Status : out Execution_Status)
         is
            Changed : Transport.Outcome;
            Number : Integer_Value;
            Origin : Integer_Origin;
         begin
            Status := Returned;
            if Item.Value_Kind /= Integer_Datum or else Width /= Bits_32 then return; end if;
            if not Tracking then
               Item.Number := AML_Integers.Normalize (Item.Number, Width);
               return;
            end if;
            if Result_Identity = 0 or else not Transport.Occupied (Values, Result_Identity)
            then Status := Unsupported_Value; return; end if;
            if Transport.Kind (Values, Result_Identity) = Transport.Temporary_Scalar then
               Transport.Normalize_Temporary_Result (Values, Result_Identity, Width, Changed);
               if Changed /= Transport.Ready then Status := Unsupported_Value; return; end if;
            elsif Transport.Kind (Values, Result_Identity) = Transport.Owned_Scalar then
               Normalize_Integer_Source (Environment,
                 Transport.Source (Values, Result_Identity), Width, Number, Origin, Status);
               if Status /= Returned then return; end if;
               Transport.Refresh_Owned_Cache (Values, Result_Identity, Number, Origin);
            else Status := Unsupported_Value; return;
            end if;
            Item := Transport.Snapshot (Values, Result_Identity);
         end Complete_Integer_Result;
         procedure Read_Reference_Result
           (Ref : AML_References.Reference; Value : out Datum; Status : out Execution_Status)
         is
            Provenance : Scalar_Provenance;
            Number : Integer_Value;
            Origin : Integer_Origin;
            Stored : Transport.Outcome;
            Location : constant Transport.Slot := Expression_Location (Call_Frame, Depth, Saved_Result);
         begin
            Publish_Live ([(Reference_Datum, Ref)]);
            Read_Reference_Provenance_At_Entry (Ref, Value, Provenance, Status);
            if Status = Returned and then Provenance.Kind = Owned_Integer_Scalar then
               -- Authentication is authoritative; a malformed descriptor never
               -- falls back to accepting an unowned integer with equal bits.
               Read_Integer_Source (Environment, Provenance.Source, Number, Origin, Status);
               if Status /= Returned or else Value.Value_Kind /= Integer_Datum
                 or else Number /= Value.Number or else Origin /= Value.Origin
               then Status := Unsupported_Value;
               elsif Tracking then
                  Transport.Put_Owned (Values, Location, Number, Origin, Provenance.Source, Stored);
                  if Stored /= Transport.Ready then Status := Value_Limit;
                  else
                     Result_Identity := Location;
                     Publish_Live;
                  end if;
               end if;
            end if;
            if Status = Returned and then Value.Value_Kind = Integer_Datum
              and then Provenance.Kind /= Owned_Integer_Scalar and then Tracking
            then
               if AML_References.Kind (Ref) = AML_References.Frame_Cell then
                  -- The authenticated live cell supplies identity, not equal bits.
                  -- Keep that semantic owner while acquiring the opcode result.
                  declare Cell_Value : constant Frame_Read_Result := Read_Frame_Reference
                    (Registry, AML_References.Frame_Item (Ref)); begin
                     if Cell_Value.Status /= Frames.Ready or else not Cell_Value.Initialized
                       or else Cell_Value.Location = 0
                       or else not Transport.Occupied (Values, Cell_Value.Location)
                       or else Transport.Kind (Values, Cell_Value.Location) /= Transport.Temporary_Scalar
                       or else Transport.Snapshot (Values, Cell_Value.Location) /= Value
                     then Status := Unsupported_Value;
                     else
                        Transport.Share (Values, Cell_Value.Location, Location, Stored);
                        if Stored /= Transport.Ready then Status := Value_Limit;
                        else Result_Identity := Location; end if;
                     end if;
                  end;
               else
                  -- A detached scalar read (for example a Buffer byte) creates
                  -- its own integer result; it cannot acquire an unrelated slot.
                  Transport.Put (Values, Location, Value, Stored);
                  if Stored /= Transport.Ready then Status := Value_Limit;
                  else Result_Identity := Location; end if;
               end if;
            end if;
            -- The original reference remains rooted through admission and the
            -- replacement snapshot, including failed stamp/ledger operations.
            Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Reference_Read,
              False, (Integer_Datum, 0, Ordinary_Integer));
         end Read_Reference_Result;
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
            (Depth > 0 and then not Stack (Depth).Is_Call and then not Stack (Depth).Is_Buffer_Count and then not Stack (Depth).Is_Package and then
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
               when Metadata_Datum =>
                  declare Metadata : Reference_Metadata; begin
                     Describe_Metadata_Value (Environment, Current.Metadata, Metadata);
                     if Metadata.Kind /= Metadata_Only then Status := Unsupported_Value;
                     else Type_Code := Named_Metadata_Type'Enum_Rep (Metadata.Object_Type); end if;
                  end;
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
           with Pre => Context_Valid (Environment) and then Offset <= Parse_Limit and then Parse_Limit <= Limit and then Limit <= Code'Length and then Charged <= Budget,
                Post => Context_Valid (Environment) and then Offset <= Parse_Limit and then Charged <= Budget
                  and then Offset >= Offset'Old and then Charged >= Charged'Old
                  and then Status /= Object_Returned and then Status /= Reference_Returned and then Status /= Metadata_Returned
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
            if Offset = Parse_Limit then Status := Truncated; return; end if;
            Source := Code (Code'First + Offset);
            if Module_Root and then Source in 16#60# .. 16#6E# then
               Status := Unsupported_Value; return;
            end if;
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
               if Offset = Parse_Limit then Status := Truncated; return; end if;
               if Code (Code'First + Offset) /= 16#31# then Status := Unsupported; return; end if;
               Offset := Offset + 1;
               Type_Code := 16; -- Debug object, no debug output or evaluation.
            elsif AML_Names.Lead (Source) or else Source in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
               Name := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)));
               if Name.Kind /= AML_Names.Accepted then
                  Status := (if Name.Kind = AML_Names.Truncated then Truncated else Bad_Name);
                  return;
               end if;
               Offset := Offset + Name.Consumed;
               Lookup (Environment, Input, Current_Lexical_Scope, Name, Width, Inspect_Binding, Object);
               case Object.Status is
                  when Reference_Binding =>
                     Describe ((Value_Kind => Reference_Datum, Ref => Object.Ref), Type_Code, Size, Status);
                  when Metadata_Binding =>
                     Describe ((Metadata_Datum, Object.Metadata), Type_Code, Size, Status);
                     if Status /= Returned then return; end if;
                     if Query = 16#87# and then Type_Code = 0 then Status := Unsupported_Value; return; end if;
                  when Failed_Binding => Status := (if Object.Failure in Execution_Failure then Object.Failure else Unsupported_Value); return;
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
              + Value_Arguments'Length + Target_List'Length + Roots_Per_Expression_Frame;
            Max_Root_Values : constant := Max_Expression_Depth * Roots_Per_Frame + Value_Arguments'Length + 1;
            Root_Values : Expression_Values (1 .. Max_Root_Values);
            Used : Natural range 0 .. Max_Root_Values := 0;
            procedure Add (Value : Datum) is
            begin
               if Used = Max_Root_Values then raise Program_Error with "expression root capacity invariant"; end if;
               Used := Used + 1; Root_Values (Used) := Value;
            end Add;
            procedure Add_Concat (Value : Concatenation_Operand) is
            begin
               if Value.Kind = Namespace_Operand then Add ((Reference_Datum, Value.Identity));
               else Add (Value.Value); end if;
            end Add_Concat;
         begin
            for Level in 1 .. Depth loop
               if Tracking then
                  for Role in Expression_Root loop
                     declare Location : constant Transport.Slot := Expression_Location (Call_Frame, Level, Role); begin
                        if Transport.Occupied (Values, Location)
                          and then Transport.Kind (Values, Location) = Transport.Owned_Scalar
                        then Add (Transport.Root_Projection (Values, Location)); end if;
                     end;
                  end loop;
               end if;
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
            if Tracking and then Transport.Occupied (Values, Pending_Location (Call_Frame))
              and then Transport.Kind (Values, Pending_Location (Call_Frame)) = Transport.Owned_Scalar
            then Add (Transport.Root_Projection (Values, Pending_Location (Call_Frame))); end if;
            for Value of Extra loop Add (Value); end loop;
            Publish_Expression_Roots (Environment, Call_Frame, Root_Values (1 .. Used));
         end Publish_Live;
         procedure Start_Package (Count : Natural) is
         begin
            Publish_Live;
            Begin_Runtime_Package (Environment, Active_Lease, Current_Lexical_Scope,
              Count, Stack (Depth).Package_End - Offset,
              Stack (Depth).Package_Parent, Stack (Depth).Package_Name, Width,
              Stack (Depth).Package_Token, Bound);
            if Bound.Status /= Non_Integer_Binding then
               S := (if Bound.Status = Failed_Binding then Bound.Failure else Unsupported_Value); return;
            end if;
            Stack (Depth).Left := (Object_Datum, Bound.Object);
            Stack (Depth).Has_Left := True;
            Stack (Depth).Package_Declared := Count;
            Stack (Depth).Package_Count_Pending := False;
            Capture_Operand (Stack (Depth).Left, 0, 0, Saved_Left, Stack (Depth).Left_Location);
            if S /= Returned then return; end if;
            Publish_Live;
         end Start_Package;
         procedure Finish_Package is
         begin
            Publish_Live;
            Finish_Runtime_Package (Environment, Active_Lease, Stack (Depth).Package_Token, Bound);
            if Bound.Status /= Non_Integer_Binding then
               S := (if Bound.Status = Failed_Binding then Bound.Failure else Unsupported_Value); return;
            end if;
            V := (Object_Datum, Bound.Object);
            Parse_Limit := Stack (Depth).Package_Outer_Bound;
            Publish_Live ([V]);
         end Finish_Package;
         procedure Dispatch
           (ID : Natural; Actuals : Value_Arguments; Count : Natural;
            Need_Value : Boolean; Value : out Datum;
            Status : out Execution_Status;
            Sources : Argument_Locations := [others => 0])
           with Always_Terminates,
                Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(0)),
                Pre => Context_Valid (Environment) and then not Value'Constrained and then Count <= 7 and then Charged > 0 and then Charged <= Budget,
                Post => Context_Valid (Environment) and then Charged <= Budget and then Charged >= Charged'Old
                  and then Status /= Object_Returned and then Status /= Reference_Returned and then Status /= Metadata_Returned
         is
            Result : Execution_Result;
            Incoming : Argument_Locations := [others => 0];
            Returned_Source : Scalar_Location := 0;
            Stored : Transport.Outcome;
         begin
            Pending_Identity := 0;
            Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
            if Calls_Left = 0 then Status := Call_Limit; return; end if;
            declare
            Method : constant Method_Definition := Get_Method (Environment, ID);
            begin
            if not Method.Exists then Status := Invalid_Method; return; end if;
            if Natural (Method.Flags and 7) /= Count then
               Status := Argument_Mismatch; return;
            end if;
            Publish_Live ([for I in 1 .. Count => Actuals (I - 1)]);
            if Tracking and then Count > 0 then
               for I in 0 .. Count - 1 loop
                  Incoming (I) := Argument_Location (Call_Frame, I);
                  if Sources (I) = 0 then Transport.Put (Values, Incoming (I), Actuals (I), Stored);
                  else Transport.Share (Values, Sources (I), Incoming (I), Stored); end if;
                  if Stored /= Transport.Ready then
                     for J in 0 .. I loop Clear_If_Present (Argument_Location (Call_Frame, J)); end loop;
                     Status := Value_Limit; return;
                  end if;
               end loop;
               -- Saved expression actuals transfer their operand ownership to
               -- Incoming. Frame-cell sources remain owned by their frames.
               for I in 0 .. Count - 1 loop
                  if Sources (I) in
                    Actual_Location (Call_Frame, 1, Value_Arguments'First) ..
                    Expression_Location (Call_Frame, Max_Expression_Depth, Expression_Root'Last)
                  then
                     Clear_If_Present (Sources (I));
                  end if;
               end loop;
            end if;
            Run
              ((if Method.Implementation = AML_Implementation then Method.Code.Bytes (1 .. Method.Length)
                 else Bytes'(1 .. 0 => 0)), Method.Code, 0, Method.Width, Actuals, Count,
               Budget - Charged, Method.Scope, Result, Calls_Left - 1,
               ID, Method.Flags, Call_Frame, AML_Declaration_Contexts.Method_Request, Method.Implementation, Incoming, Returned_Source);
            -- Compound results have already crossed the return-root bridge.
            -- No collecting callback occurs between return and this clear.
            Publish_Expression_Roots (Environment, Call_Frame, []);
            Charged := Charged + Result.Charged;
            if Tracking then
               -- Also covers a child rejected before its argument-binding phase.
               for I in Value_Arguments'Range loop
                  if I < Count then Clear_If_Present (Argument_Location (Call_Frame, I)); end if;
               end loop;
               if Result.Status in Returned | Object_Returned | Reference_Returned | Metadata_Returned
                 and then Returned_Source /= 0 then
                  Transport.Move (Values, Returned_Source, Pending_Location (Call_Frame), Stored);
                  if Stored /= Transport.Ready then
                     Clear_If_Present (Returned_Source); Status := Value_Limit; return;
                  end if;
                  Pending_Identity := Pending_Location (Call_Frame);
               end if;
            end if;
            if Result.Status = Fatal_Requested and then not Pending_Fatal.Present then
               Pending_Fatal := (Present => True, Item => Result.Fatal);
            end if;
            Status := Result.Status;
            if Result.Status = Returned then
               Value := (Value_Kind => Integer_Datum, Number => Result.Value, Origin => Result.Origin);
            elsif Result.Status = Object_Returned then
               Value := (Value_Kind => Object_Datum, Object => Result.Object);
               Status := Returned;
            elsif Result.Status = Metadata_Returned then
               Value := (Metadata_Datum, Result.Metadata);
            elsif Result.Status = Reference_Returned then
               Value := (Value_Kind => Reference_Datum, Ref => Result.Ref);
               Status := Returned;
            elsif Result.Status = No_Return and Need_Value then
               Status := Missing_Result;
            end if;
            end;
         end Dispatch;
         procedure Evaluate_Core is
         begin
         V := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
         S := Returned;
         loop
            pragma Loop_Invariant (Context_Valid (Environment));
            pragma Loop_Invariant (S /= Object_Returned and then S /= Reference_Returned and then S /= Metadata_Returned);
            pragma Loop_Invariant (Offset <= Parse_Limit and then Parse_Limit <= Limit and then Limit <= Code'Length and then Charged <= Budget);
            pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
            pragma Loop_Invariant (Charged >= Charged'Loop_Entry);
            pragma Loop_Variant (Decreases => Budget - Charged);
            if Depth > 0 and then Stack (Depth).Is_Package
              and then not Stack (Depth).Package_Count_Pending
              and then not Stack (Depth).Package_Is_Variable
              and then Stack (Depth).Package_Seen >= Stack (Depth).Package_Declared then
               S := Bad_Package; return;
            end if;
            if Charged = Budget then S := Budget_Exceeded; return; end if;
            Charged := Charged + 1;
            if Offset = Parse_Limit then S := Truncated; return; end if;
            B := Code (Code'First + Offset);
            Have_Value := True;
            Pending_Slot := 0;
            Pending_Identity := 0;
            Clear_If_Present (Pending_Location (Call_Frame));
            Pending_Concat := (Kind => Data_Operand, Value => <>);
            Pending_Provenance := Stored_Value;
            if B in 16#70# | 16#83# | 16#88# | Copy_Object_Op | To_Integer_Op | To_Buffer_Op | Mid_Op | To_String_Op | Decimal_String_Op | Hexadecimal_String_Op | Match_Op | Concatenate_Resources_Op | Concatenate_Op or else AML_Integers.Supported (B) or else AML_Logic.Supported (B)
              or else (B in 16#87# | 16#8E# and then Parse_Limit - Offset > 1
                and then Code (Code'First + Offset + 1) in 16#71# | 16#83# | 16#88#)
            then
               if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
               Depth := Depth + 1;
               Stack (Depth) := (Op => B, others => <>);
               Offset := Offset + 1;
            else
               if Depth > 0 and then Stack (Depth).Is_Package
                 and then not Stack (Depth).Package_Count_Pending
                 and then (AML_Names.Lead (B) or else B in 16#5C# | 16#5E# | 16#2E# | 16#2F#) then
                  Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)));
                  if Path.Kind /= AML_Names.Accepted then S := Bad_Name; return; end if;
                  Offset := Offset + Path.Consumed;
                  Defer_Runtime_Package_Name (Environment, Active_Lease,
                    Stack (Depth).Package_Token, Current_Lexical_Scope, Path, S);
                  if S /= Returned then return; end if;
                  Stack (Depth).Package_Seen := Stack (Depth).Package_Seen + 1;
                  Stack (Depth).Package_Skip_Child := True;
                  V := (Integer_Datum, 0, Ordinary_Integer);
               elsif B in Increment_Op | Decrement_Op then
                  Offset := Offset + 1;
                  if Offset = Parse_Limit then S := Truncated; return; end if;
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
                  if Offset = Parse_Limit then S := Truncated; return; end if;
                  if Charged = Budget then S := Budget_Exceeded; return; end if;
                  Charged := Charged + 1;
                  declare Target : constant Byte := Code (Code'First + Offset);
                     Ref : AML_References.Reference;
                     Cell_Ref : AML_Frame_Handles.Cell_Handle;
                     Made : Frames.Result_Status;
                  begin
                     if Target in 16#60# .. 16#6E# then
                        if Module_Root then S := Unsupported_Value; return; end if;
                        Frames.Make_Reference (Registry, Call_Frame, Cell (Natural (Target - 16#60#)), Cell_Ref, Made);
                        if Made /= Frames.Ready then S := Unsupported_Value; return; end if;
                        Ref := AML_References.Bind_Frame (Cell_Ref); Offset := Offset + 1;
                     elsif AML_Names.Lead (Target) or else Target in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                        Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)));
                        if Path.Kind /= AML_Names.Accepted or else
                     (Path.Count = 0 and then not (Path.Rooted and then Path.Parents = 0)) then
                           S := (if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name); return;
                        end if;
                        Lookup (Environment, Input, Current_Lexical_Scope, Path, Width, AML_Execute.Namespace_Identity, Bound);
                        if Bound.Status = Missing_Binding then S := Unknown_Name; return;
                        elsif Bound.Status /= Reference_Binding then S := Unsupported_Value; return; end if;
                        Ref := Bound.Ref; Offset := Offset + Path.Consumed;
                     else S := Unsupported_Value; return; end if;
                     V := (Value_Kind => Reference_Datum, Ref => Ref);
                  end;
               elsif B = 16#5B# then
                  if Parse_Limit - Offset < 2 then S := Truncated; return; end if;
                  if Code (Code'First + Offset + 1) = Revision_Extension then
                     Offset := Offset + Revision_Bytes;
                     V := (Value_Kind => Integer_Datum, Number => Interpreter_Revision,
                           Origin => Ordinary_Integer);
                  elsif Code (Code'First + Offset + 1) = AML_Mutexes.Acquire_Opcode then
                     if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
                     Offset := Offset + 2;
                     if Offset = Parse_Limit then S := Truncated; return; end if;
                     if not Mutex_Super_Name_Lead (Offset, Parse_Limit) then S := Unsupported_Value; return; end if;
                     Depth := Depth + 1;
                     Stack (Depth) := (Op => AML_Mutexes.Acquire_Opcode, others => <>);
                     Have_Value := False;
                  elsif Code (Code'First + Offset + 1) = Wait_Extension then
                     if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
                     Depth := Depth + 1;
                     Stack (Depth) := (Op => Wait_Extension, others => <>);
                     Offset := Offset + 2; Have_Value := False;
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
                     if Offset = Parse_Limit then S := Truncated; return; end if;
                     if Charged = Budget then S := Budget_Exceeded; return; end if;
                     Charged := Charged + 1;
                     if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
                     Depth := Depth + 1;
                     Stack (Depth) := (Is_Conditional_Reference => True, Op => 0,
                       Target_Total => 1, others => <>);
                     declare Source : constant Byte := Code (Code'First + Offset); begin
                        if Source in 16#60# .. 16#6E# then
                           if Module_Root then S := Unsupported_Value; return; end if;
                           Stack (Depth).Source_Slot := Source; Offset := Offset + 1;
                        elsif AML_Names.Lead (Source) or else Source in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                           Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)));
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
                     if Loading_Effects_Restricted then S := Unsupported; return; end if;
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
                  if Module_Root then S := Unsupported_Value; return; end if;
                  Offset := Offset + 1;
                  Pending_Slot := Natural (B - 16#5F#);
                  V := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer);
               elsif B = 16#11# and then Active_Buffer_Count
                 (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1))) then
                  if Allow_No_Return and Depth = 0 then S := Unsupported; return; end if;
                  declare
                     Header : constant Package_Result := Read_Package
                       (Code (Code'First + Offset + 1 .. Code'First + (Parse_Limit - 1)));
                     Start : constant Natural := Offset;
                     Parent_Bound : constant Natural := Parse_Limit;
                     As_Integer : constant Boolean := Integer_Expected;
                  begin
                     if Header.Kind /= Accepted then S := Bad_Package; return; end if;
                     if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
                     Depth := Depth + 1;
                     Parse_Limit := Start + Header.Extent + 1;
                     Offset := Start + Header.Encoding_Bytes + 1;
                     Stack (Depth) := (Op => 0, Is_Buffer_Count => True,
                       Buffer_Start => Start, Buffer_End => Parse_Limit,
                       Buffer_Count_Start => Offset, Buffer_Parent_Bound => Parent_Bound,
                       Buffer_Integer_Result => As_Integer, others => <>);
                     Have_Value := False;
                  end;
               elsif B in 16#0D# | 16#11# and then Integer_Expected then
                  if B = 16#0D# then
                     Text := Read_String_Span (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)));
                     if Text.Kind /= Accepted then
                        S := (if Text.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Conversion := String_Reader.From_String
                       (Source, Source_First + Offset + Text.Payload_Offset, Text.Length, Width);
                     Offset := Offset + Text.Consumed;
                  else
                     Buffer_Item := AML_Buffer_Layouts.Read (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)), Width);
                     if Buffer_Item.Kind /= Accepted then
                        S := (if Buffer_Item.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     declare
                        Literal_Start : constant Natural := Offset;
                        Width_Bytes : constant Positive := (if Width = Bits_32 then 4 else 8);
                        Prefix_Length : constant Natural := Natural (Integer_Value'Min
                          (Buffer_Item.Effective_Length, Integer_Value (Width_Bytes)));
                        Prefix : Bytes (1 .. Prefix_Length) := [others => 0];
                     begin
                        Offset := Offset + Buffer_Item.Consumed;
                        if Prefix_Length = 0 then S := Empty_Buffer; return; end if;
                        for I in 1 .. Natural'Min (Prefix_Length, Buffer_Item.Initializer_Length) loop
                           Prefix (I) := Code (Code'First +
                             (Literal_Start + Buffer_Item.Initializer_Offset + (I - 1)));
                        end loop;
                        Conversion := AML_Coercions.From_Buffer (Prefix, Width);
                     end;
                  end if;
                  if Conversion.Status /= AML_Coercions.Converted then S := Empty_Buffer; return; end if;
                  V := (Value_Kind => Integer_Datum, Number => Conversion.Value, Origin => AML_Decode.Ordinary_Integer);
               elsif B in 16#12# | 16#13# and then Runtime_Package_Supported then
                  if Parse_Limit - Offset <= 1 then S := Truncated; return; end if;
                  Package_Item := Read_Package (Code (Code'First + Offset + 1 .. Code'First + (Parse_Limit - 1)));
                  if Package_Item.Kind /= Accepted then S := Bad_Package; return; end if;
                  if Depth = Max_Expression_Depth then S := Expression_Limit; return; end if;
                  declare
                     Parent : constant Runtime_Package_Token :=
                       (if Depth > 0 and then Stack (Depth).Is_Package
                         and then not Stack (Depth).Package_Count_Pending
                        then Stack (Depth).Package_Token else No_Runtime_Package);
                     Name : constant Name_Reservation_Token :=
                       (if Depth = 0 then Package_Name else No_Name_Reservation_Token);
                     Outer_Bound : constant Natural := Parse_Limit;
                     Finish : constant Natural := Offset + 1 + Package_Item.Extent;
                  begin
                     Offset := Offset + 1 + Package_Item.Encoding_Bytes;
                     if Offset >= Finish then S := Truncated; return; end if;
                     Depth := Depth + 1;
                     Stack (Depth) := (Op => 0, Is_Package => True,
                       Package_Is_Variable => B = 16#13#,
                       Package_Count_Pending => B = 16#13#,
                       Package_End => Finish, Package_Outer_Bound => Outer_Bound,
                       Package_Parent => Parent, Package_Name => Name, others => <>);
                     Parse_Limit := Finish;
                     if B = 16#12# then
                        declare Count : constant Natural := Natural (Code (Code'First + Offset)); begin
                           Offset := Offset + 1;
                           Start_Package (Count);
                        end;
                        if S /= Returned then return; end if;
                        Stack (Depth).Package_Skip_Child := Offset = Finish;
                        Have_Value := Offset = Finish;
                     else Have_Value := False;
                     end if;
                  end;
               elsif B in 16#12# | 16#13# then
                  if Parse_Limit - Offset <= 1 then S := Truncated; return; end if;
                  Package_Item := Read_Package (Code (Code'First + Offset + 1 .. Code'First + (Parse_Limit - 1)));
                  if Package_Item.Kind /= Accepted then S := Bad_Package; return; end if;
                  Publish_Live;
                  Materialize (Environment, Current_Lexical_Scope, Package_Literal, Width,
                    Code (Code'First + Offset .. Code'First + Offset + Package_Item.Extent),
                    Source, Source_First + Offset, Bound);
                  Offset := Offset + 1 + Package_Item.Extent;
                  if Bound.Status = Failed_Binding then
                     S := (if Bound.Failure in Execution_Failure then Bound.Failure else Unsupported_Value); return;
                  elsif Bound.Status /= Non_Integer_Binding or else Bound.Object.ID = 0
                    or else Bound.Object.Type_Code /= 4 then S := Unsupported_Value; return;
                  end if;
                  V := (Value_Kind => Object_Datum, Object => Bound.Object);
               elsif B in 16#0D# | 16#11# then
                  if Allow_No_Return and Depth = 0 then S := Unsupported; return; end if;
                  if B = 16#0D# then
                     Text := Read_String_Span (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)));
                     if Text.Kind /= Accepted then
                        S := (if Text.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     Publish_Live;
                     Materialize (Environment, Current_Lexical_Scope, String_Literal, Width,
                       Code (Code'First + Offset + Text.Payload_Offset ..
                             Code'First + Offset + (Text.Consumed - 2)),
                       Source, Source_First + Offset + Text.Payload_Offset, Bound);
                     Offset := Offset + Text.Consumed;
                  else
                     Buffer_Item := AML_Buffer_Layouts.Read (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)), Width);
                     if Buffer_Item.Kind /= Accepted then
                        S := (if Buffer_Item.Kind = AML_Decode.Truncated then Truncated else Unsupported_Value);
                        return;
                     end if;
                     declare
                        Literal_Start : constant Natural := Offset;
                     begin
                        Offset := Offset + Buffer_Item.Consumed;
                        Publish_Live;
                        if Buffer_Item.Initializer_Length = 0 then
                           Materialize_Buffer (Environment, Current_Lexical_Scope, Buffer_Item.Effective_Length,
                             Bytes'(1 .. 0 => 0), Bound);
                        else
                           Materialize_Buffer (Environment, Current_Lexical_Scope, Buffer_Item.Effective_Length,
                             Code (Code'First + (Literal_Start + Buffer_Item.Initializer_Offset) ..
                                   Code'First + (Literal_Start + (Buffer_Item.Consumed - 1))), Bound);
                        end if;
                     end;
                  end if;
                  if Bound.Status = Failed_Binding then
                     S := (if Bound.Failure in Execution_Failure then Bound.Failure else Unsupported_Value);
                     return;
                  elsif Bound.Status /= Non_Integer_Binding or else Bound.Object.ID = 0
                    or else Bound.Object.Type_Code /= (if B = 16#0D# then 2 else 3)
                  then S := Unsupported_Value; return; end if;
                  V := (Value_Kind => Object_Datum, Object => Bound.Object);
               elsif AML_Names.Lead (B) or else B in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                  Path := AML_Names.Read_Name
                    (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)));
                  if Path.Kind /= AML_Names.Accepted then
                     S := (if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name);
                     return;
                  end if;
                  Offset := Offset + Path.Consumed;
                  if Depth > 0 and then Stack (Depth).Op = Concatenate_Op
                    and then not Stack (Depth).Collecting_Targets
                  then
                     declare Identity : Binding_Result; Metadata : Reference_Metadata; begin
                        Lookup (Environment, Input, Current_Lexical_Scope, Path, Width, Namespace_Identity, Identity);
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
                  elsif Depth > 0 and then Stack (Depth).Op = AML_Mutexes.Acquire_Opcode then
                     Lookup (Environment, Input, Current_Lexical_Scope, Path, Width, Namespace_Identity, Bound);
                  elsif Depth > 0 and then Stack (Depth).Op = Wait_Extension
                    and then not Stack (Depth).Has_Left
                  then
                     -- Wait's SuperName is an authenticated namespace operand,
                     -- resolved after its timeout operand. A method invocation
                     -- instead produces an ordinary retained value.
                     Lookup (Environment, Input, Current_Lexical_Scope, Path, Width, Namespace_Identity, Bound);
                     if Bound.Status = Reference_Binding then
                        declare
                           use type Named_Metadata_Type;
                           Metadata : Reference_Metadata;
                        begin
                           Describe_Named_Identity (Environment, Bound.Ref, Metadata);
                           if Metadata.Kind = Metadata_Only and then Metadata.Object_Type = Method_Metadata then
                              Publish_Live;
                              Lookup (Environment, Input, Current_Lexical_Scope, Path, Width, Evaluate_Binding, Bound);
                           end if;
                        end;
                     end if;
                  else
                     -- Wide table fields materialize a buffer during evaluation.
                     Publish_Live;
                     Lookup (Environment, Input, Current_Lexical_Scope, Path, Width, Evaluate_Binding, Bound);
                  end if;
                  case Bound.Status is
                     when Failed_Binding => S := (if Bound.Failure in Execution_Failure then Bound.Failure else Unsupported_Value); return;
                     when Reference_Binding => V := (Value_Kind => Reference_Datum, Ref => Bound.Ref);
                     when Metadata_Binding => V := (Metadata_Datum, Bound.Metadata);
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
                        if Bound.Object.Type_Code not in String_Type_Code | Buffer_Type_Code | Package_Type_Code | Event_Type_Code | AML_Mutexes.Object_Type_Code
                          or else Bound.Object.ID = 0 then
                           S := Unsupported_Value; return;
                        end if;
                        if Allow_No_Return and Depth = 0 then S := Unsupported; return; end if;
                        V := (Value_Kind => Object_Datum, Object => Bound.Object);
                  end case;
               else
                  Literal := Read_Integer (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)), Width);
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
                  pragma Loop_Invariant (S /= Object_Returned and then S /= Reference_Returned and then S /= Metadata_Returned);
                  pragma Loop_Invariant (Charged > 0 and then Charged <= Budget);
                  pragma Loop_Invariant (Charged >= Charged'Loop_Entry);
                  pragma Loop_Invariant (Offset <= Parse_Limit and then Parse_Limit <= Limit and then Limit <= Code'Length);
                  pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
                  pragma Loop_Variant (Decreases => Depth);
                  if Depth = 0 then
                     if Defer_Final then
                        Staged_Slot := Pending_Slot;
                        Staged_Identity := Pending_Identity;
                        Staged_Provenance := Pending_Provenance;
                        return;
                     end if;
                     Publish_Live ([V]);
                     Resolve_Slot_With_Source (Pending_Slot, V, Final_Provenance, S);
                     if S /= Returned then return; end if;
                     if Pending_Slot = 0 and then Final_Provenance.Kind /= Owned_Integer_Scalar then
                        Refresh_Scalar_Location (Pending_Identity, V, S);
                        if S /= Returned then return; end if;
                     end if;
                     Operand_Location :=
                       (if Tracking and then Pending_Slot /= 0 then
                          Frame_Location (Call_Frame, Cell (Pending_Slot - 1)) else Pending_Identity);
                     if V.Value_Kind in Reference_Datum | Metadata_Datum and Require_Integer then S := Unsupported_Value; return; end if;
                     if V.Value_Kind = Object_Datum and Require_Integer then
                        Conversion := (if Width = Bits_32 then V.Object.Conversion_32 else V.Object.Conversion_64);
                        case Conversion.Status is
                           when AML_Coercions.Converted =>
                              Operand_Location := 0;
                              Final_Provenance := Detached_Provenance;
                              V := (Value_Kind => Integer_Datum, Number => Conversion.Value, Origin => AML_Decode.Ordinary_Integer);
                           when AML_Coercions.Empty_Buffer => S := Empty_Buffer; return;
                           when AML_Coercions.Not_Convertible => S := Unsupported_Value; return;
                        end case;
                     end if;
                     Refresh_Compound_Location (Operand_Location, V, S);
                     if S /= Returned then return; end if;
                     if Tracking then
                        declare
                           Stored : Transport.Outcome;
                           Number : Integer_Value;
                           Origin : Integer_Origin;
                           Location : constant Transport.Slot := Pending_Location (Call_Frame);
                        begin
                           if Final_Provenance.Kind = Owned_Integer_Scalar then
                              -- A Name_Member read exposes the original owner's
                              -- exact identity; it is not a fresh numeric copy.
                              Read_Integer_Source (Environment, Final_Provenance.Source, Number, Origin, S);
                              if S /= Returned or else V.Value_Kind /= Integer_Datum
                                or else Number /= V.Number or else Origin /= V.Origin
                              then S := Unsupported_Value; return; end if;
                              Transport.Put_Owned (Values, Location, Number, Origin,
                                Final_Provenance.Source, Stored);
                              if Stored /= Transport.Ready then S := Value_Limit; return; end if;
                              Operand_Location := Location;
                           elsif Operand_Location = 0 or else
                             (Transport.Occupied (Values, Operand_Location)
                              and then Transport.Kind (Values, Operand_Location) = Transport.Non_Scalar
                              and then Transport.Snapshot (Values, Operand_Location) /= V)
                           then
                              -- Parsed values and actual conversions receive a
                              -- canonical result. Do not borrow the old reference.
                              Transport.Put (Values, Location, V, Stored);
                              if Stored /= Transport.Ready then S := Value_Limit; return; end if;
                              Operand_Location := Location;
                           elsif not Transport.Occupied (Values, Operand_Location)
                             or else Transport.Snapshot (Values, Operand_Location) /= V
                           then S := Unsupported_Value; return;
                           end if;
                        end;
                     end if;
                     return;
                  end if;
                  if Stack (Depth).Collecting_Targets then
                     Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
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
                     if V.Value_Kind = Reference_Datum and then Tracking then
                        declare
                           Captured : Scalar_Location;
                           Candidate : constant Scalar_Location := Pending_Identity;
                        begin
                           if Candidate /= 0 and then
                             (not Transport.Occupied (Values, Candidate)
                              or else Transport.Snapshot (Values, Candidate) /= V)
                           then S := Unsupported_Value; return; end if;
                           Capture_Operand (V, 0, Candidate,
                             (if Stack (Depth).Target_Count = 1 then Saved_Target else Saved_Reference), Captured);
                           if S /= Returned then return; end if;
                           if Captured = 0 then S := Unsupported_Value; return; end if;
                        end;
                     end if;
                     Collect_Targets
                       (Stack (Depth).Target_Count, Stack (Depth).Target_Total,
                        Stack (Depth).Targets, Waiting_For_Target, S);
                     if S /= Returned then return; end if;
                     if Waiting_For_Target then exit; end if;
                     Stack (Depth).Collecting_Targets := False;
                     Stack (Depth).Targets_Ready := True;
                     V := Stack (Depth).Right;
                     Pending_Slot := Stack (Depth).Right_Slot;
                     Pending_Identity := Stack (Depth).Right_Location;
                     Pending_Concat := Stack (Depth).Right_Concat;
                     Pending_Provenance := Stack (Depth).Right_Provenance;
                  end if;
                  Result_Identity := 0;
                  No_Op_Integer := False;
                  Real_Integer_Conversion := False;
                  pragma Assert (Context_Valid (Environment));
                  if Stack (Depth).Is_Package then
                     if Stack (Depth).Package_Count_Pending then
                        Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
                        if S /= Returned then return; end if;
                        Publish_Live ([V]);
                        Resolve_Buffer_Count (V, S);
                        if S /= Returned then return; end if;
                        if V.Number > Integer_Value (Natural'Last) then S := Value_Limit; return; end if;
                        Start_Package (Natural (V.Number));
                        if S /= Returned then return; end if;
                     elsif not Stack (Depth).Package_Skip_Child then
                        Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
                        if S /= Returned then return; end if;
                        Publish_Live ([V]);
                        Append_Runtime_Package (Environment, Active_Lease, Stack (Depth).Package_Token, V, S);
                        if S /= Returned then return; end if;
                        Stack (Depth).Package_Seen := Stack (Depth).Package_Seen + 1;
                        Publish_Live;
                     end if;
                     Stack (Depth).Package_Skip_Child := False;
                     if Offset < Stack (Depth).Package_End then
                        if not Stack (Depth).Package_Is_Variable and then
                          Stack (Depth).Package_Seen = Stack (Depth).Package_Declared then
                           S := Bad_Package; return;
                        end if;
                        exit;
                     end if;
                     Finish_Package;
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Is_Buffer_Count then
                     pragma Assert (Stack (Depth).Buffer_Count_Start <= Offset
                       and then Offset <= Stack (Depth).Buffer_End
                       and then Parse_Limit = Stack (Depth).Buffer_End
                       and then Stack (Depth).Buffer_End <= Stack (Depth).Buffer_Parent_Bound);
                     Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
                     if S /= Returned then return; end if;
                     Stack (Depth).Left := V; Stack (Depth).Has_Left := True;
                     Publish_Live ([V]);
                     Resolve_Buffer_Count (V, S);
                     if S /= Returned then return; end if;
                     declare
                        Frame_Value : constant Frame := Stack (Depth);
                        Layout : constant AML_Buffer_Layouts.Layout_Result :=
                          AML_Buffer_Layouts.With_Count
                            (Code (Code'First + Frame_Value.Buffer_Start ..
                                   Code'First + (Frame_Value.Buffer_End - 1)),
                             AML_Integers.Normalize (V.Number, Width),
                             Offset - Frame_Value.Buffer_Count_Start);
                     begin
                        if Layout.Kind /= Accepted then S := Unsupported_Value; return; end if;
                        Parse_Limit := Frame_Value.Buffer_Parent_Bound;
                        Offset := Frame_Value.Buffer_End;
                        if Frame_Value.Buffer_Integer_Result then
                           declare
                              Width_Bytes : constant Positive := (if Width = Bits_32 then 4 else 8);
                              Size : constant Natural := Natural (Integer_Value'Min
                                (Layout.Effective_Length, Integer_Value (Width_Bytes)));
                              Prefix : Bytes (1 .. Size) := [others => 0];
                           begin
                              if Size = 0 then S := Empty_Buffer; return; end if;
                              for I in 1 .. Natural'Min (Size, Layout.Initializer_Length) loop
                                 Prefix (I) := Code (Code'First + Frame_Value.Buffer_Start
                                   + Layout.Initializer_Offset + (I - 1));
                              end loop;
                              Conversion := AML_Coercions.From_Buffer (Prefix, Width);
                              if Conversion.Status /= AML_Coercions.Converted then
                                 S := Empty_Buffer; return;
                              end if;
                              V := (Integer_Datum, Conversion.Value, Ordinary_Integer);
                           end;
                        else
                           Publish_Live;
                           if Layout.Initializer_Length = 0 then
                              Materialize_Buffer (Environment, Current_Lexical_Scope, Layout.Effective_Length,
                                Bytes'(1 .. 0 => 0), Bound);
                           else
                              Materialize_Buffer (Environment, Current_Lexical_Scope, Layout.Effective_Length,
                                Code (Code'First + Frame_Value.Buffer_Start + Layout.Initializer_Offset ..
                                      Code'First + (Frame_Value.Buffer_End - 1)), Bound);
                           end if;
                           if Bound.Status = Failed_Binding then
                              S := (if Bound.Failure in Execution_Failure
                                    then Bound.Failure else Unsupported_Value); return;
                           elsif Bound.Status /= Non_Integer_Binding or else Bound.Object.ID = 0
                             or else Bound.Object.Type_Code /= Buffer_Type_Code then
                              S := Unsupported_Value; return;
                           end if;
                           V := (Object_Datum, Bound.Object);
                        end if;
                     end;
                  elsif Stack (Depth).Op in Increment_Op | Decrement_Op then
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
                           Lookup (Environment, Input, Current_Lexical_Scope, Target.Path, Width, Namespace_Identity, Bound);
                           if Bound.Status = Missing_Binding then S := Unknown_Name; return;
                           elsif Bound.Status /= Reference_Binding then S := Unsupported_Value; return; end if;
                        end if;
                        if Stack (Depth).Source_Slot /= 0 then
                           Frames.Make_Reference (Registry, Call_Frame,
                             Cell (Natural (Stack (Depth).Source_Slot - 16#60#)), Cell_Ref, Made);
                           if Made /= Frames.Ready then S := Unsupported_Value; return; end if;
                           Ref := AML_References.Bind_Frame (Cell_Ref); Present := True;
                        else
                           Lookup (Environment, Input, Current_Lexical_Scope, Stack (Depth).Source_Name, Width, Namespace_Identity, Bound);
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
                     if Tracking and then Pending_Slot = 0 and then Pending_Identity /= 0 then
                        declare Stored : Transport.Outcome; Location : constant Transport.Slot :=
                          Actual_Location (Call_Frame, Depth, Stack (Depth).Given); begin
                           Transport.Move (Values, Pending_Identity, Location, Stored);
                           if Stored /= Transport.Ready then S := Value_Limit; return; end if;
                           Stack (Depth).Locations (Stack (Depth).Given) := Location;
                        end;
                     end if;
                     Stack (Depth).Given := Stack (Depth).Given + 1;
                     if Stack (Depth).Given < Stack (Depth).Parameters then exit; end if;
                     for I in 0 .. Stack (Depth).Given - 1 loop
                        Argument_Item := Stack (Depth).Actuals (I);
                        Resolve_Expression_Slot (Stack (Depth).Slots (I), Argument_Item, S, Stack (Depth).Locations (I), Expression_Root'Val (I));
                        if S /= Returned then return; end if;
                        Stack (Depth).Actuals (I) := Argument_Item;
                     end loop;
                     Dispatch (Stack (Depth).Method_ID, Stack (Depth).Actuals,
                               Stack (Depth).Given, not Allow_No_Return or Depth > 1, V, S, Stack (Depth).Locations);
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op = Concatenate_Op then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Concat := Pending_Concat; Stack (Depth).Left_Provenance := Pending_Provenance;
                        if Pending_Concat.Kind = Data_Operand then
                           Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Left,
                             Stack (Depth).Left_Location);
                           if S /= Returned then return; end if;
                        end if;
                        Stack (Depth).Has_Left := True; exit;
                     end if;
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Concat := Pending_Concat; Stack (Depth).Right_Provenance := Pending_Provenance;
                        if Pending_Concat.Kind = Data_Operand then
                           Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Right,
                             Stack (Depth).Right_Location);
                           if S /= Returned then return; end if;
                        end if;
                        Stack (Depth).Target_Total := 1; Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1, Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False; Stack (Depth).Targets_Ready := True;
                     end if;
                     Resolve_Concatenation (Stack (Depth).Left_Concat, Stack (Depth).Left,
                       Stack (Depth).Left_Slot, Stack (Depth).Left_Provenance, S, Stack (Depth).Left_Location, Saved_Left);
                     if S /= Returned then return; end if;
                     Resolve_Concatenation (Stack (Depth).Right_Concat, Stack (Depth).Right,
                       Stack (Depth).Right_Slot, Stack (Depth).Right_Provenance, S, Stack (Depth).Right_Location, Saved_Right);
                     if S /= Returned then return; end if;
                     Attach_Concatenation (Stack (Depth).Left_Concat, Stack (Depth).Right_Concat,
                       Stack (Depth).Targets (1), V, S);
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op = Match_Op then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Provenance := Pending_Provenance;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Left,
                          Stack (Depth).Left_Location);
                        if S /= Returned then return; end if;
                        Stack (Depth).Has_Left := True;
                        if Offset = Parse_Limit then S := Truncated; return; end if;
                        Stack (Depth).Match_Byte_1 := Code (Code'First + Offset);
                        Offset := Offset + 1; exit;
                     elsif not Stack (Depth).Has_Middle then
                        Stack (Depth).Middle := V; Stack (Depth).Middle_Slot := Pending_Slot;
                        Stack (Depth).Middle_Provenance := Pending_Provenance;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Middle,
                          Stack (Depth).Middle_Location);
                        if S /= Returned then return; end if;
                        Stack (Depth).Has_Middle := True;
                        if Offset = Parse_Limit then S := Truncated; return; end if;
                        Stack (Depth).Match_Byte_2 := Code (Code'First + Offset);
                        Offset := Offset + 1; exit;
                     elsif not Stack (Depth).Has_Right then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Right,
                          Stack (Depth).Right_Location);
                        if S /= Returned then return; end if;
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
                        Resolve_Mid_Operand (Start, Pending_Slot, Pending_Provenance, S,
                          Pending_Identity, Reserved_Root);
                        if S /= Returned then return; end if;
                        Convert_Integer_Value (Start, S);
                        if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Match_2, Stack (Depth).Right_Slot, Stack (Depth).Right_Provenance, S,
                          Stack (Depth).Right_Location, Saved_Right);
                        if S /= Returned then return; end if;
                        Stack (Depth).Right := Match_2;
                        Admit (Match_2, False); if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Match_1, Stack (Depth).Middle_Slot, Stack (Depth).Middle_Provenance, S,
                          Stack (Depth).Middle_Location, Saved_Middle);
                        if S /= Returned then return; end if;
                        Stack (Depth).Middle := Match_1;
                        Admit (Match_1, False); if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Package_Value, Stack (Depth).Left_Slot, Stack (Depth).Left_Provenance, S,
                          Stack (Depth).Left_Location, Saved_Left);
                        if S /= Returned then return; end if;
                        Stack (Depth).Left := Package_Value;
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
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Left,
                          Stack (Depth).Left_Location);
                        if S /= Returned then return; end if;
                        Stack (Depth).Has_Left := True; exit;
                     end if;
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Right,
                          Stack (Depth).Right_Location);
                        if S /= Returned then return; end if;
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
                        Resolve_Mid_Operand (Right, Stack (Depth).Right_Slot, Stack (Depth).Right_Provenance, S, Stack (Depth).Right_Location, Saved_Right);
                        if S /= Returned then return; end if;
                        Stack (Depth).Right := Right;
                        Resolve_Mid_Operand (Left, Stack (Depth).Left_Slot, Stack (Depth).Left_Provenance, S, Stack (Depth).Left_Location, Saved_Left);
                        if S /= Returned then return; end if;
                        Stack (Depth).Left := Left;
                        Attach_Resources (Left, Right, Stack (Depth).Targets (1), V, S);
                        if S /= Returned then return; end if;
                     end;
                  elsif Stack (Depth).Op = To_String_Op then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Provenance := Pending_Provenance;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Left,
                          Stack (Depth).Left_Location);
                        if S /= Returned then return; end if;
                        Stack (Depth).Has_Left := True; exit;
                     end if;
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Right,
                          Stack (Depth).Right_Location);
                        if S /= Returned then return; end if;
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
                        Resolve_Mid_Operand (Length, Stack (Depth).Right_Slot,
                          Stack (Depth).Right_Provenance, S, Stack (Depth).Right_Location, Saved_Right);
                        if S /= Returned then return; end if;
                        Stack (Depth).Right := Length;
                        Convert_Integer_Value (Length, S);
                        if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Item, Stack (Depth).Left_Slot,
                          Stack (Depth).Left_Provenance, S, Stack (Depth).Left_Location, Saved_Left);
                        if S /= Returned then return; end if;
                        Stack (Depth).Left := Item;
                        Attach_String (Item, AML_Integers.Normalize (Length.Number, Width),
                          Stack (Depth).Targets (1), V, S);
                        if S /= Returned then return; end if;
                     end;
                  elsif Stack (Depth).Op = Mid_Op then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Provenance := Pending_Provenance;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Left,
                          Stack (Depth).Left_Location);
                        if S /= Returned then return; end if;
                        Stack (Depth).Has_Left := True; exit;
                     elsif not Stack (Depth).Has_Middle then
                        Stack (Depth).Middle := V; Stack (Depth).Middle_Slot := Pending_Slot;
                        Stack (Depth).Middle_Provenance := Pending_Provenance;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Middle,
                          Stack (Depth).Middle_Location);
                        if S /= Returned then return; end if;
                        Stack (Depth).Has_Middle := True; exit;
                     end if;
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V; Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Right,
                          Stack (Depth).Right_Location);
                        if S /= Returned then return; end if;
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
                        Resolve_Mid_Operand (Count, Stack (Depth).Right_Slot,
                          Stack (Depth).Right_Provenance, S, Stack (Depth).Right_Location, Saved_Right);
                        if S /= Returned then return; end if;
                        Stack (Depth).Right := Count;
                        Convert_Integer_Value (Count, S);
                        if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Start, Stack (Depth).Middle_Slot,
                          Stack (Depth).Middle_Provenance, S, Stack (Depth).Middle_Location, Saved_Middle);
                        if S /= Returned then return; end if;
                        Stack (Depth).Middle := Start;
                        Convert_Integer_Value (Start, S);
                        if S /= Returned then return; end if;
                        Resolve_Mid_Operand (Item, Stack (Depth).Left_Slot,
                          Stack (Depth).Left_Provenance, S, Stack (Depth).Left_Location, Saved_Left);
                        if S /= Returned then return; end if;
                        Stack (Depth).Left := Item;
                        Attach_Mid (Item, AML_Integers.Normalize (Start.Number, Width),
                          AML_Integers.Normalize (Count.Number, Width), Stack (Depth).Targets (1), V, S);
                        if S /= Returned then return; end if;
                     end;
                  elsif Stack (Depth).BCD /= Not_BCD then
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V;
                        Stack (Depth).Right_Slot := Pending_Slot;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Right,
                          Stack (Depth).Right_Location);
                        if S /= Returned then return; end if;
                        Stack (Depth).Target_Total := 1;
                        Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1,
                          Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False;
                        Stack (Depth).Targets_Ready := True;
                     end if;
                     -- Capture may have moved Pending even when the target
                     -- was immediate. Both paths consume the saved source slot.
                     V := Stack (Depth).Right;
                     Pending_Slot := Stack (Depth).Right_Slot;
                     Pending_Identity := Stack (Depth).Right_Location;
                     Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
                     if S /= Returned then return; end if;
                     Stack (Depth).Right := V;
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
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Right,
                          Stack (Depth).Right_Location);
                        if S /= Returned then return; end if;
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
                          Stack (Depth).Right_Provenance, S, Stack (Depth).Right_Location, Saved_Right);
                        if S /= Returned then return; end if;
                        Stack (Depth).Right := Item;
                        Attach_Formatted_String (Item, Mode, Stack (Depth).Targets (1), V, S);
                        if S /= Returned then return; end if;
                     end;
                  elsif Stack (Depth).Op in To_Integer_Op | To_Buffer_Op then
                     if not Stack (Depth).Targets_Ready then
                        Stack (Depth).Right := V;
                        Stack (Depth).Right_Slot := Pending_Slot;
                        Stack (Depth).Right_Provenance := Pending_Provenance;
                        Check_Computed_Integer (V, Pending_Slot, Pending_Identity, Pending_Provenance);
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Right,
                          Stack (Depth).Right_Location);
                        if S /= Returned then return; end if;
                        Pending_Identity := Stack (Depth).Right_Location;
                        Stack (Depth).Target_Total := 1;
                        Stack (Depth).Collecting_Targets := True;
                        Collect_Targets (Stack (Depth).Target_Count, 1,
                          Stack (Depth).Targets, Waiting_For_Target, S);
                        if S /= Returned then return; end if;
                        if Waiting_For_Target then exit; end if;
                        Stack (Depth).Collecting_Targets := False;
                        Stack (Depth).Targets_Ready := True;
                     end if;
                     Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
                     if S /= Returned then return; end if;
                     if Stack (Depth).Op = To_Buffer_Op then
                        declare Source : constant Datum := V; begin
                           Attach_Buffer (Source, Stack (Depth).Targets (1), V, S);
                        end;
                     else
                        No_Op_Integer := V.Value_Kind = Integer_Datum;
                        Real_Integer_Conversion := V.Value_Kind = Object_Datum;
                        if No_Op_Integer then
                           if Tracking and then Pending_Slot /= 0 then
                              declare Candidate : Scalar_Location :=
                                Frame_Location (Call_Frame, Cell (Pending_Slot - 1));
                              begin
                                 if Transport.Snapshot (Values, Candidate) /= V then Candidate := 0; end if;
                                 Capture_Operand (V, 0, Candidate, Saved_Right, Stack (Depth).Right_Location);
                                 if S /= Returned then return; end if;
                                 Pending_Identity := Stack (Depth).Right_Location;
                                 Pending_Slot := 0;
                              end;
                           end if;
                           Check_Computed_Integer (V, Pending_Slot, Pending_Identity, Stack (Depth).Right_Provenance);
                           Capture_Result (V, Pending_Slot, Pending_Identity, Share_Conversion_Result);
                           if S /= Returned then return; end if;
                        end if;
                        Convert_To_Integer (Environment, Width, V, S);
                        if S /= Returned then return; end if;
                        if Result_Identity /= 0 and then Transport.Snapshot (Values, Result_Identity) /= V then
                           Clear_If_Present (Result_Identity);
                           Result_Identity := 0;
                        end if;
                        Apply_Target (Stack (Depth).Targets (1), V, S, Explicit_Result_Target);
                        if S /= Returned then return; end if;
                        -- Integer input keeps its original result identity. The
                        -- target receives its attachment before completion can
                        -- truncate that source; real conversions stay separate.
                        if No_Op_Integer then Complete_Integer_Result (V, S); end if;
                     end if;
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op in 16#87# | 16#8E# then
                     Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
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
                     Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
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
                     Write_Target (V, S, Parse_Limit);
                     if S /= Returned then return; end if;
                  elsif Stack (Depth).Op = 16#83# then
                     Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
                     if S /= Returned then return; end if;
                     if V.Value_Kind = Metadata_Datum and then Pending_Slot /= 0 then
                        -- DerefOf(Local/Arg) first unwraps that slot. A metadata
                        -- expression result is not itself a reference.
                        declare Metadata : Reference_Metadata; begin
                           Describe_Metadata_Value (Environment, V.Metadata, Metadata);
                           if Metadata.Kind /= Metadata_Only then S := Unsupported_Value; return; end if;
                        end;
                     elsif V.Value_Kind /= Reference_Datum then S := Unsupported_Value; return;
                     else
                     declare
                        Resolved : Datum;
                     begin
                        Read_Reference_Result (V.Ref, Resolved, S);
                        if S = Uninitialized and then AML_References.Kind (V.Ref) = AML_References.Frame_Cell then
                           S := No_Return; return;
                        end if;
                        if S not in Returned | Uninitialized | Unsupported_Value | Value_Limit | Buffer_Field_Limit | Field_Indeterminate then S := Unsupported_Value; end if;
                        if S /= Returned then return; end if;
                        V := Resolved;
                        Complete_Integer_Result (V, S);
                        if S /= Returned then return; end if;
                     end;
                     end if;
                  elsif Stack (Depth).Op = Copy_Object_Op then
                     Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
                     if S /= Returned then return; end if;
                     -- Argument-target validation may allocate another field
                     -- value; retain the resolved source in authoritative roots.
                     Stack (Depth).Right := V; Stack (Depth).Has_Right := True;
                     Check_Computed_Integer (V, Pending_Slot, Pending_Identity, Pending_Provenance);
                     declare Candidate : Scalar_Location := Pending_Identity; begin
                        if Tracking and then Pending_Slot /= 0 then
                           Candidate := Frame_Location (Call_Frame, Cell (Pending_Slot - 1));
                        end if;
                        if Tracking and then Candidate /= 0
                          and then Transport.Snapshot (Values, Candidate) /= V
                        then Candidate := 0; end if;
                        Capture_Operand (V, 0, Candidate, Saved_Right, Stack (Depth).Right_Location);
                        if S /= Returned then return; end if;
                     end;
                     if Offset = Parse_Limit then S := Truncated; return; end if;
                     declare
                        use AML_Simple_Targets;
                        Target : constant Target_Result := Read_Target
                          (Code (Code'First + Offset .. Code'First + (Parse_Limit - 1)));
                        Copied : Datum;
                     begin
                        case Target.Kind is
                           when AML_Simple_Targets.Truncated => S := Truncated; return;
                           when Malformed | Limit_Exceeded => S := Bad_Name; return;
                           when Name_Target =>
                              if Target.Path.Count = 0 then S := Bad_Name; return; end if;
                           when Local_Target | Argument_Target =>
                              if Module_Root then S := Unsupported_Value; return; end if;
                        end case;
                        if Target.Kind = Argument_Target then
                           declare Destination : constant Frame_Read_Result := Slot_Value
                             (First_Argument_Cell + Target.Slot); Checked_Value : Datum; begin
                              if Destination.Initialized and then Destination.Value.Value_Kind = Reference_Datum
                                and then AML_References.Kind (Destination.Value.Ref) in AML_References.Named_Cell | AML_References.Frame_Cell | AML_References.Root_Namespace
                              then
                                 Read_Reference_Value (Destination.Value.Ref, Checked_Value, S);
                                 if S not in Returned | Uninitialized then return; end if;
                              end if;
                           end;
                        end if;
                        if Target.Kind = Name_Target then
                           Publish_Live ([V]);
                           Copy_And_Attach (Environment,
                             (Kind => Named_Destination, Scope => Current_Lexical_Scope, Path => Target.Path), Width, V, Copied, S);
                        elsif Target.Kind = Argument_Target then
                           declare Destination : constant Frame_Read_Result := Slot_Value
                             (First_Argument_Cell + Target.Slot); begin
                              if Destination.Initialized and then Destination.Value.Value_Kind = Reference_Datum
                                and then AML_References.Kind (Destination.Value.Ref) in AML_References.Named_Cell | AML_References.Root_Namespace
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
                  elsif Stack (Depth).Op = AML_Mutexes.Acquire_Opcode then
                     -- WordData is a literal two-byte encoding, not TermArg.
                     if Parse_Limit - Offset < AML_Mutexes.Timeout_Bytes then S := Truncated; return; end if;
                     if Budget - Charged < AML_Mutexes.Timeout_Bytes then S := Budget_Exceeded; return; end if;
                     declare
                        Timeout : constant AML_Mutexes.Timeout := AML_Mutexes.Timeout
                          (Natural (Code (Code'First + Offset)) + 256 * Natural (Code (Code'First + Offset + 1)));
                        Target : Datum := V;
                     begin
                        Charged := Charged + AML_Mutexes.Timeout_Bytes;
                        Offset := Offset + AML_Mutexes.Timeout_Bytes;
                        Resolve_Expression_Slot (Pending_Slot, Target, S, Pending_Identity);
                        if S /= Returned then return; end if;
                        if Target.Value_Kind = Reference_Datum and then AML_References.Kind (Target.Ref) /= AML_References.Named_Cell then
                           Read_Reference_Value (Target.Ref, V, S);
                           if S /= Returned then return; end if;
                           Target := V;
                        end if;
                        Publish_Live ([Target]);
                        S := Unsupported_Value;
                        Operate_Mutex (Environment, Target, AML_Mutexes.Acquire_Mutex, Timeout, S);
                        if S /= Returned then return; end if;
                        V := (Integer_Datum, 0, Ordinary_Integer);
                     end;
                  elsif Stack (Depth).Op = Wait_Extension then
                     if not Stack (Depth).Has_Left then
                        Stack (Depth).Left := V; Stack (Depth).Left_Slot := Pending_Slot;
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Left, Stack (Depth).Left_Location);
                        if S /= Returned then return; end if;
                        Stack (Depth).Has_Left := True;
                        exit;
                     end if;
                     Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
                     if S /= Returned then return; end if;
                     Convert_Integer_Value (V, S);
                     if S /= Returned then return; end if;
                     declare
                        Timeout : constant AML_Events.Timeout := AML_Events.Normalize_Timeout (V.Number);
                        Event_Value : Datum := Stack (Depth).Left;
                        Timed_Out : Boolean := False;
                     begin
                        Resolve_Expression_Slot (Stack (Depth).Left_Slot, Event_Value, S, Stack (Depth).Left_Location);
                        if S /= Returned then return; end if;
                        Publish_Live ([Event_Value]);
                        S := Unsupported;
                        Operate_Event (Environment, Event_Value, AML_Events.Wait_Event, Timeout, Timed_Out, S);
                        if S /= Returned then return; end if;
                        V := (Integer_Datum,
                          (if Timed_Out then AML_Integers.Normalize (Integer_Value'Last, Width) else 0), Ordinary_Integer);
                     end;
                  elsif Stack (Depth).Op = 16#70# then
                     if Stack (Depth).Has_Left then
                        Resolve_Expression_Slot (Pending_Slot, V, S, Pending_Identity);
                        if S /= Returned then return; end if;
                        if V.Value_Kind /= Reference_Datum and then
                          (V.Value_Kind /= Integer_Datum or else V.Origin /= AML_Constant)
                        then S := Unsupported_Value; return; end if;
                        Stack (Depth).Right := V; Stack (Depth).Has_Right := True;
                        Left_Item := Stack (Depth).Left;
                        Resolve_Store_Source (Stack (Depth).Left_Slot, Left_Item, Stack (Depth).Left_Location,
                          Expression_Location (Call_Frame, Depth, Reserved_Root), S);
                        if S /= Returned then return; end if;
                        -- Target expressions have run, but no target write is
                        -- permitted for a metadata Store source.
                        if Left_Item.Value_Kind = Metadata_Datum or else
                          (Left_Item.Value_Kind = Object_Datum and then Left_Item.Object.Type_Code = Event_Type_Code)
                        then S := Unsupported_Value; return; end if;
                        Check_Computed_Integer (Left_Item, Stack (Depth).Left_Slot,
                          Stack (Depth).Left_Location, Stack (Depth).Left_Provenance);
                        Capture_Result (Left_Item, Stack (Depth).Left_Slot, Stack (Depth).Left_Location, Transfer_Operand);
                        if S /= Returned then return; end if;
                        if V.Value_Kind = Reference_Datum then
                           Publish_Live ([Left_Item, V]);
                           Write_Reference_Value (V.Ref, Left_Item, S);
                        else
                           S := Returned;
                        end if;
                        if S not in Returned | Unsupported_Value | Value_Limit | Empty_Buffer | Buffer_Field_Limit | Field_Indeterminate then S := Unsupported_Value; end if;
                        if S /= Returned then return; end if;
                        V := Left_Item;
                     elsif Offset < Parse_Limit and then Code (Code'First + Offset) in 16#71# | 16#83# | 16#88# then
                        -- Preserve local/argument identity until target side
                        -- effects finish, matching sibling operand resolution.
                        Stack (Depth).Left := V;
                        Stack (Depth).Left_Slot := Pending_Slot;
                        Stack (Depth).Left_Provenance := Pending_Provenance;
                        Check_Computed_Integer (V, Pending_Slot, Pending_Identity, Pending_Provenance);
                        Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Left,
                          Stack (Depth).Left_Location);
                        if S /= Returned then return; end if;
                        Stack (Depth).Has_Left := True;
                        exit;
                     else
                        Resolve_Store_Source (Pending_Slot, V, Pending_Identity,
                          Expression_Location (Call_Frame, Depth, Reserved_Root), S);
                        if S /= Returned then return; end if;
                        if V.Value_Kind = Metadata_Datum then S := Unsupported_Value; return; end if;
                        if V.Value_Kind = Object_Datum and then V.Object.Type_Code = Event_Type_Code
                          and then not (Parse_Limit - Offset >= Debug_Target_Bytes
                            and then Code (Code'First + Offset) = Extended_Op
                            and then Code (Code'First + Offset + 1) = Debug_Extension)
                        then S := Unsupported_Value; return; end if;
                        Check_Computed_Integer (V, Pending_Slot, Pending_Identity, Pending_Provenance);
                        Capture_Result (V, Pending_Slot, Pending_Identity, Transfer_Operand);
                        if S /= Returned then return; end if;
                        Publish_Live ([V]);
                        Write_Target (V, S, Parse_Limit, Result_Identity);
                        if S /= Returned then return; end if;
                     end if;
                     -- Store returns its original source object. Complete width
                     -- processing only after the target write has succeeded.
                     Complete_Integer_Result (V, S);
                     if S /= Returned then return; end if;
                  else
                  -- Preserve expression owners before later operands/targets
                  -- can mutate their source. Lexical cells stay deferred.
                  if not (AML_Logic.Unary (Stack (Depth).Op)
                    or AML_Integers.Unary (Stack (Depth).Op))
                    and then not Stack (Depth).Has_Left
                  then
                     Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Left,
                       Stack (Depth).Left_Location);
                     if S /= Returned then return; end if;
                  elsif not Stack (Depth).Targets_Ready then
                     Capture_Operand (V, Pending_Slot, Pending_Identity, Saved_Right,
                       Stack (Depth).Right_Location);
                     if S /= Returned then return; end if;
                  end if;
                  Advance_Numeric
                    (Stack (Depth), Pending_Slot, V, Waiting_For_Target, S);
                  if S /= Returned then return; end if;
                  if Waiting_For_Target then exit; end if;
                  end if;
                  Pending_Slot := 0;
                  if not Stack (Depth).Is_Call and then Tracking then
                     declare Stored : Transport.Outcome; begin
                        if V.Value_Kind = Integer_Datum and then (Stack (Depth).Op = 16#70# or else No_Op_Integer) then
                           pragma Assert (Result_Identity /= 0);
                        end if;
                        if Result_Identity /= 0 then
                           Transport.Move (Values, Result_Identity, Pending_Location (Call_Frame), Stored);
                        else
                           -- Arithmetic and CopyObject return newly created
                           -- scalar objects. Compound payloads retain their own
                           -- authenticated handles; owner counts are not inferred.
                           Transport.Put (Values, Pending_Location (Call_Frame), V, Stored);
                        end if;
                        if Stored /= Transport.Ready then S := Value_Limit; return; end if;
                        Pending_Identity := Pending_Location (Call_Frame);
                        if Stack (Depth).Op = Copy_Object_Op and then V.Value_Kind = Integer_Datum then
                           pragma Assert (Stack (Depth).Right_Location /= 0);
                           pragma Assert (not Transport.Same_Identity
                             (Values, Stack (Depth).Right_Location, Pending_Identity));
                        elsif Real_Integer_Conversion and then Stack (Depth).Right_Location /= 0 then
                           pragma Assert (Transport.Kind (Values, Stack (Depth).Right_Location) = Transport.Non_Scalar);
                           pragma Assert (Transport.Kind (Values, Pending_Identity) = Transport.Temporary_Scalar);
                           pragma Assert (not Transport.Same_Identity
                             (Values, Stack (Depth).Right_Location, Pending_Identity));
                        end if;
                        Transport.Clear_Range (Values,
                          Expression_Location (Call_Frame, Depth, Expression_Root'First),
                          Expression_Location (Call_Frame, Depth, Expression_Root'Last));
                     end;
                  end if;
                  Pending_Concat := (Kind => Data_Operand, Value => <>);
                  Pending_Provenance := Expression_Result;
                  Depth := Depth - 1;
               end loop;
               end if;
            end if;
         end loop;
         end Evaluate_Core;
      begin
         Evaluate_Core;
         declare Cleanup : Execution_Status; begin
            Abort_Runtime_Packages (Environment, Active_Lease, Cleanup);
            if Cleanup /= Returned then S := Unsupported_Value; end if;
         end;
      end Evaluate_Operand;
      procedure Operand (V : out Datum; S : out Execution_Status; Allow_No_Return : Boolean := False; Require_Integer : Boolean := False; Package_Name : Name_Reservation_Token := No_Name_Reservation_Token)
        with Always_Terminates,
             Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(2)),
             Pre => Context_Valid (Environment) and then not V'Constrained and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget,
             Post => Context_Valid (Environment) and then Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget
               and then Charged >= Charged'Old and then Offset >= Offset'Old
               and then (if S = Returned then Charged > Charged'Old)
               and then S /= Object_Returned and then S /= Reference_Returned and then S /= Metadata_Returned
      is
      begin
         Operand_Location := 0;
         Evaluate_Operand (V, S, Allow_No_Return, Require_Integer, Package_Name => Package_Name);
         -- Clear saved actual roots on every expression exit, including errors.
         if Tracking then
            Transport.Clear_Range (Values,
              Actual_Location (Call_Frame, 1, Value_Arguments'First),
              Expression_Location (Call_Frame, Max_Expression_Depth, Expression_Root'Last));
         end if;
         -- Evaluation has returned, including all early-failure paths. No
         -- collecting callback runs before the caller consumes/publishes V.
         Publish_Expression_Roots (Environment, Call_Frame, []);
      end Operand;
      -- Source draft: value normalization policy awaits the pinned oracle.
      procedure Notify_Object (Status : out Execution_Status) is
         Target : Datum := (Integer_Datum, 0, Ordinary_Integer);
         Notify_Value : Datum;
         Binding : Binding_Result;
         Path : AML_Names.Name_Result;
         Named_Target : Boolean := False;
         procedure Complete is
            use type AML_Names.Parse_Status;
            B : Byte;
         begin
            if Offset = Limit then Status := Truncated; return; end if;
            B := Code (Code'First + Offset);
            if AML_Names.Lead (B) or else B in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
               if Charged = Budget then Status := Budget_Exceeded; return; end if;
               Charged := Charged + 1;
               Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
               if Path.Kind /= AML_Names.Accepted or else
                     (Path.Count = 0 and then not (Path.Rooted and then Path.Parents = 0)) then
                  Status := (if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name); return;
               end if;
               Offset := Offset + Path.Consumed;
               Lookup (Environment, Input, Current_Lexical_Scope, Path, Width, Namespace_Identity, Binding);
               if Binding.Status = Missing_Binding then Status := Unknown_Name; return;
               elsif Binding.Status = Failed_Binding then
                  Status := (if Binding.Failure in Failure_Status then Binding.Failure else Unsupported_Value); return;
               elsif Binding.Status = Reference_Binding then
                  Target := (Reference_Datum, Binding.Ref); Named_Target := True;
               end if;
            elsif B in 16#60# .. 16#6E# then
               -- SuperName Local/Arg is a cell reference, not its stored value.
               -- ACPICA rejects it as a Notify object after evaluating Value.
               if Charged = Budget then Status := Budget_Exceeded; return; end if;
               Charged := Charged + 1; Offset := Offset + 1;
            elsif B in 16#71# | 16#83# | 16#88# then
               Evaluate_Operand (Target, Status, Defer_Final => True);
               if Status /= Returned then return; end if;
            elsif B = Extended_Op then
               if Limit - Offset < Debug_Target_Bytes then Status := Truncated; return; end if;
               if Code (Code'First + Offset + 1) /= Debug_Extension then Status := Unsupported; return; end if;
               if Charged = Budget then Status := Budget_Exceeded; return; end if;
               Charged := Charged + 1; Offset := Offset + Debug_Target_Bytes;
            else Status := Unsupported; return;
            end if;
            if Tracking then
               declare Stored : Transport.Outcome; begin
                  Transport.Put (Values, Scratch_Location (Call_Frame, Notification_Target), Target, Stored);
                  if Stored /= Transport.Ready then Status := Value_Limit; return; end if;
               end;
            end if;
            Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Notify_Target, True, Target);
            if Tracking then
               Transport.Clear_Range (Values,
                 Actual_Location (Call_Frame, 1, Value_Arguments'First),
                 Expression_Location (Call_Frame, Max_Expression_Depth, Expression_Root'Last));
               Clear_If_Present (Pending_Location (Call_Frame));
            end if;
            Publish_Expression_Roots (Environment, Call_Frame, []);
            Operand (Notify_Value, Status, Require_Integer => True);
            if Status /= Returned then return; end if;
            if not Named_Target then Status := Unsupported_Value; return; end if;
            -- Owner revalidates identity, type and interest at admission, after
            -- all value effects. Never resolve an old path to a new object.
            pragma Assert (Context_Valid (Environment));
            Queue_Notify (Environment, Target.Ref, Notify_Value.Number, Status);
            pragma Assert (Context_Valid (Environment));
            if Status not in Returned | Unsupported | Unsupported_Value | Notify_Limit then
               Status := Unsupported_Value;
            end if;
         end Complete;
      begin
         Complete;
         if Tracking then Clear_If_Present (Scratch_Location (Call_Frame, Notification_Target)); end if;
         Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Notify_Target,
           False, (Integer_Datum, 0, Ordinary_Integer));
         Publish_Expression_Roots (Environment, Call_Frame, []);
      end Notify_Object;
      type Staged_Operand is record
         Destination : Scalar_Location := 0;
         Item : Datum;
         Slot : Slot_Number := 0;
         Location : Scalar_Location := 0;
         Provenance : Concatenation_Provenance := Stored_Value;
      end record;
      procedure Publish_Staged
        (Item : Staged_Operand; Root : AML_Root_Slots.Held_Root) is
         Projection : Datum := Item.Item;
      begin
         if Tracking and then Item.Location /= 0
           and then Transport.Occupied (Values, Item.Location)
           and then Transport.Kind (Values, Item.Location) = Transport.Owned_Scalar
         then Projection := Transport.Root_Projection (Values, Item.Location); end if;
         Publish_Held_Root (Environment, Call_Frame, Root, True, Projection);
      end Publish_Staged;
      procedure Read_Staged
        (Item : out Staged_Operand; Root : AML_Root_Slots.Held_Root;
         Role : Declaration_Scratch; Status : out Execution_Status)
      is
         Stored : Transport.Outcome;
         Location : constant Transport.Slot := Scratch_Location (Call_Frame, Role);
      begin
         Staged_Identity := 0;
         Evaluate_Operand (Item.Item, Status, Defer_Final => True);
         Item.Slot := Staged_Slot; Item.Provenance := Staged_Provenance;
         Item.Location := 0; Item.Destination := Location;
         if Status = Returned then
            if Tracking and then Item.Slot = 0 then
               if Staged_Identity = 0 then
                  Transport.Put (Values, Location, Item.Item, Stored);
               elsif Staged_Identity = Pending_Location (Call_Frame) then
                  Transport.Move (Values, Staged_Identity, Location, Stored);
               else
                  Transport.Share (Values, Staged_Identity, Location, Stored);
               end if;
               if Stored /= Transport.Ready then Status := Value_Limit;
               else Item.Location := Location; end if;
            end if;
            if Status = Returned then
               -- Publish the authenticated owner before dropping expression
               -- mirrors; the next operand may clear or replace Pending.
               Publish_Staged (Item, Root);
            end if;
         end if;
         if Tracking then
            -- The declaration scratch or deferred lexical cell now owns the
            -- value. Expression slots are no longer semantic operand owners.
            Transport.Clear_Range (Values,
              Actual_Location (Call_Frame, 1, Value_Arguments'First),
              Expression_Location (Call_Frame, Max_Expression_Depth, Expression_Root'Last));
            Clear_If_Present (Pending_Location (Call_Frame));
         end if;
         Publish_Expression_Roots (Environment, Call_Frame, []);
      end Read_Staged;
      procedure Resolve_Staged (Item : in out Staged_Operand; Root : AML_Root_Slots.Held_Root;
         Status : out Execution_Status)
      is
         Provenance : Scalar_Provenance;
      begin
         Resolve_Slot_With_Source (Item.Slot, Item.Item, Provenance, Status);
         if Status /= Returned then return; end if;
         if Tracking and then Item.Slot /= 0 then
            Item.Location := Frame_Location (Call_Frame, Cell (Item.Slot - 1));
         end if;
         Admit_Resolved_Value (Item.Item, Provenance, Item.Location,
           Transport.Slot (Item.Destination), Status, Preserve_Prior => Item.Slot /= 0);
         if Status /= Returned then return; end if;
         if Item.Provenance = Expression_Result and then Item.Slot = 0
           and then Item.Item.Value_Kind = Reference_Datum
           and then AML_References.Kind (Item.Item.Ref) = AML_References.Package_Slot
         then
            declare Ref : constant AML_References.Reference := Item.Item.Ref; begin
               Read_Reference_Value (Ref, Item.Item, Provenance, Status);
            end;
            if Status /= Returned then return; end if;
            Admit_Resolved_Value (Item.Item, Provenance, Item.Location,
              Transport.Slot (Item.Destination), Status);
            if Status /= Returned then return; end if;
         end if;
         Publish_Staged (Item, Root);
      end Resolve_Staged;
      type Buffer_Field_Declaration is
        (Bit_Field, Byte_Field, Word_Field, DWord_Field, QWord_Field, Generic_Field);
      procedure Create_Buffer_Field
        (Kind : Buffer_Field_Declaration; Status : out Execution_Status)
      is
         Bits_Per_Byte : constant Integer_Value := 8;
         Source, Index, Count : Staged_Operand;
         Path : AML_Names.Name_Result;
         Bit_Offset, Bit_Length : Integer_Value;
         procedure Complete is
            use type AML_Names.Parse_Status;
         begin
            Read_Staged (Source, AML_Root_Slots.Field_Source, Declaration_Source, Status);
            if Status /= Returned then return; end if;
            Read_Staged (Index, AML_Root_Slots.Field_Index, Declaration_Index, Status);
            if Status /= Returned then return; end if;
            if Kind = Generic_Field then
               Read_Staged (Count, AML_Root_Slots.Field_Count, Declaration_Count, Status);
               if Status /= Returned then return; end if;
            end if;
            if Offset = Limit then Status := Truncated; return; end if;
            Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
            if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
               Status := (if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name); return;
            end if;
            Offset := Offset + Path.Consumed;
            if Kind = Generic_Field then
               Resolve_Staged (Count, AML_Root_Slots.Field_Count, Status);
               if Status /= Returned then return; end if;
               Convert_Integer_Value (Count.Item, Status);
               if Status /= Returned then return; end if;
               Bit_Length := AML_Integers.Normalize (Count.Item.Number, Width);
            else
               Bit_Length := (case Kind is
                 when Bit_Field => 1, when Byte_Field => 8, when Word_Field => 16,
                 when DWord_Field => 32, when QWord_Field => 64, when Generic_Field => 0);
            end if;
            Resolve_Staged (Index, AML_Root_Slots.Field_Index, Status);
            if Status /= Returned then return; end if;
            Convert_Integer_Value (Index.Item, Status);
            if Status /= Returned then return; end if;
            Bit_Offset := AML_Integers.Normalize (Index.Item.Number, Width);
            Resolve_Staged (Source, AML_Root_Slots.Field_Source, Status);
            if Status /= Returned then return; end if;
            if Kind not in Bit_Field | Generic_Field then
               if Bit_Offset > Integer_Value'Last / Bits_Per_Byte then
                  Status := Buffer_Field_Limit; return;
               end if;
               Bit_Offset := Bit_Offset * Bits_Per_Byte;
            end if;
            pragma Assert (Context_Valid (Environment));
            Define_Buffer_Field (Environment, Active_Lease, Current_Lexical_Scope, Path, Width, Source.Item,
              Bit_Offset, Bit_Length, Kind = Generic_Field, Status);
            if Status /= Returned and then Status not in Failure_Status then
               Status := Unsupported_Value;
            end if;
         end Complete;
      begin
         Complete;
         if Tracking then
            Transport.Clear_Range (Values,
              Scratch_Location (Call_Frame, Declaration_Source),
              Scratch_Location (Call_Frame, Declaration_Count));
         end if;
         for Root in AML_Root_Slots.Field_Source .. AML_Root_Slots.Field_Count loop
            Publish_Held_Root (Environment, Call_Frame, Root, False,
              (Integer_Datum, 0, Ordinary_Integer));
         end loop;
      end Create_Buffer_Field;
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
            if Blocks (Block_Depth).Kind = Container_Block then
               Pop_Lexical (State);
               if State /= Returned or else Current_Lexical_Scope /= Blocks (Block_Depth).Prior_Scope then
                  Body_Result := Failure (Unsupported_Value, Charged); return;
               end if;
            end if;
            Offset := (if Blocks (Block_Depth).Kind = Loop_Block then
                         Blocks (Block_Depth).Restart else Blocks (Block_Depth).Resume);
            Limit := Blocks (Block_Depth).Outer_Limit;
            Block_Depth := Block_Depth - 1;
         else
         if Charged = Budget then Body_Result := Failure (Budget_Exceeded, Charged); return; end if;
         Charged := Charged + 1;
         Op := Code (Code'First + Offset);
         Offset := Offset + 1;
         case Op is
            when 16#10# =>
               Open_Container (Offset - 1, State);
               if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
            when Alias_Op =>
               if not Module_Root then Body_Result := Failure (Unsupported, Charged); return; end if;
               declare
                  use type AML_Names.Parse_Status;
                  Source_Path, Target_Path : AML_Names.Name_Result;
                  Alias_Status : Execution_Status;
                  procedure Read_Alias_Path (Path : out AML_Names.Name_Result) is
                  begin
                     if Offset = Limit then
                        Path := (Kind => AML_Names.Truncated); return;
                     end if;
                     Path := AML_Names.Read_Name
                       (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                  end Read_Alias_Path;
               begin
                  Read_Alias_Path (Source_Path);
                  if Source_Path.Kind /= AML_Names.Accepted or else
                    (Source_Path.Count = 0 and then not (Source_Path.Rooted and then Source_Path.Parents = 0))
                  then
                     Body_Result := Failure ((if Source_Path.Kind = AML_Names.Truncated then Truncated else Bad_Name), Charged); return;
                  end if;
                  if Source_Path.Consumed > Budget - Charged then Body_Result := Failure (Budget_Exceeded, Charged); return; end if;
                  Charged := Charged + Source_Path.Consumed;
                  Offset := Offset + Source_Path.Consumed;
                  Read_Alias_Path (Target_Path);
                  if Target_Path.Kind /= AML_Names.Accepted or else Target_Path.Count = 0 then
                     Body_Result := Failure ((if Target_Path.Kind = AML_Names.Truncated then Truncated else Bad_Name), Charged); return;
                  end if;
                  if Target_Path.Consumed > Budget - Charged then Body_Result := Failure (Budget_Exceeded, Charged); return; end if;
                  Charged := Charged + Target_Path.Consumed;
                  Offset := Offset + Target_Path.Consumed;
                  Define_Alias (Environment, Active_Lease, Current_Lexical_Scope, Source_Path, Target_Path, Alias_Status);
                  if Alias_Status /= Returned then Body_Result := Failure (Alias_Status, Charged); return; end if;
               end;
            when Notify_Op =>
               -- Notify remains an evaluated AML operation while loading. The
               -- authenticated owner validates the target and drops admission
               -- when notification interest is disabled; no hardware callback.
               Notify_Object (State);
               if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
            when Create_Bit_Field_Op | Create_Byte_Field_Op | Create_Word_Field_Op |
                 Create_DWord_Field_Op | Create_QWord_Field_Op =>
               Create_Buffer_Field
                 ((case Op is
                    when Create_Bit_Field_Op => Bit_Field,
                    when Create_Byte_Field_Op => Byte_Field,
                    when Create_Word_Field_Op => Word_Field,
                    when Create_DWord_Field_Op => DWord_Field,
                    when others => QWord_Field), State);
               if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
            when 16#5B# =>
               if Offset = Limit then Body_Result := Failure (Truncated, Charged); return; end if;
               if Code (Code'First + Offset) = Index_Field_Extension then
                  declare
                     use type AML_Names.Parse_Status;
                     Start : constant Natural := Offset;
                     P : Package_Result;
                     Finish : Natural;
                     Index_Path, Data_Path : AML_Names.Name_Result;
                     Flags : Byte;
                     Declared_Status : Execution_Status := Unsupported;
                  begin
                     Offset := Offset + 1;
                     if Offset = Limit then Body_Result := Failure (Bad_Package, Charged); return; end if;
                     P := Read_Package (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                     if P.Kind /= Accepted then Body_Result := Failure (Bad_Package, Charged); return; end if;
                     Finish := Offset + P.Extent;
                     Offset := Offset + P.Encoding_Bytes;
                     if Offset = Finish then Body_Result := Failure (Bad_Name, Charged); return; end if;
                     Index_Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Finish - 1)));
                     if Index_Path.Kind /= AML_Names.Accepted or else Index_Path.Count = 0 then
                        Body_Result := Failure (Bad_Name, Charged); return;
                     end if;
                     Offset := Offset + Index_Path.Consumed;
                     if Offset = Finish then Body_Result := Failure (Bad_Name, Charged); return; end if;
                     Data_Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Finish - 1)));
                     if Data_Path.Kind /= AML_Names.Accepted or else Data_Path.Count = 0 then
                        Body_Result := Failure (Bad_Name, Charged); return;
                     end if;
                     Offset := Offset + Data_Path.Consumed;
                     if Offset = Finish then Body_Result := Failure (Truncated, Charged); return; end if;
                     Flags := Code (Code'First + Offset);
                     Offset := Offset + 1;
                     -- Reserve the full remaining encoding charge before any
                     -- namespace mutation, including empty FieldLists.
                     if Finish - Start > Budget - Charged then
                        Body_Result := Failure (Budget_Exceeded, Charged); return;
                     end if;
                     Charged := Charged + (Finish - Start);
                     if Offset = Finish then
                        Define_Indexed_Fields (Environment, Active_Lease, Current_Lexical_Scope,
                          Index_Path, Data_Path, Flags, [], Declared_Status);
                     else
                        Define_Indexed_Fields (Environment, Active_Lease, Current_Lexical_Scope,
                          Index_Path, Data_Path, Flags,
                          Code (Code'First + Offset .. Code'First + (Finish - 1)), Declared_Status);
                     end if;
                     if Declared_Status /= Returned then
                        Body_Result := Failure
                          ((if Declared_Status in Failure_Status then Declared_Status else Unsupported), Charged);
                        return;
                     end if;
                     Offset := Finish;
                  end;
               elsif Code (Code'First + Offset) = Bank_Field_Extension then
                  declare
                     use type AML_Names.Parse_Status;
                     Header_Start : constant Natural := Offset;
                     Outer_Limit : constant Natural := Limit;
                     P : Package_Result;
                     Finish : Natural;
                     Region_Path, Bank_Path : AML_Names.Name_Result;
                     Selection : Datum;
                     Flags : Byte;
                     Declared_Status : Execution_Status := Unsupported;
                  begin
                     Offset := Offset + 1;
                     if Offset = Limit then Body_Result := Failure (Bad_Package, Charged); return; end if;
                     P := Read_Package (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                     if P.Kind /= Accepted then Body_Result := Failure (Bad_Package, Charged); return; end if;
                     Finish := Offset + P.Extent;
                     Offset := Offset + P.Encoding_Bytes;
                     if Offset = Finish then Body_Result := Failure (Bad_Name, Charged); return; end if;
                     Region_Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Finish - 1)));
                     if Region_Path.Kind /= AML_Names.Accepted or else Region_Path.Count = 0 then
                        Body_Result := Failure (Bad_Name, Charged); return;
                     end if;
                     Offset := Offset + Region_Path.Consumed;
                     if Offset = Finish then Body_Result := Failure (Bad_Name, Charged); return; end if;
                     Bank_Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Finish - 1)));
                     if Bank_Path.Kind /= AML_Names.Accepted or else Bank_Path.Count = 0 then
                        Body_Result := Failure (Bad_Name, Charged); return;
                     end if;
                     Offset := Offset + Bank_Path.Consumed;
                     if Offset - Header_Start > Budget - Charged then
                        Body_Result := Failure (Budget_Exceeded, Charged); return;
                     end if;
                     Charged := Charged + (Offset - Header_Start);
                     -- BankValue is a full TermArg. Its evaluation must remain
                     -- inside this package and may execute nested methods.
                     Limit := Finish;
                     Operand (Selection, Declared_Status, Require_Integer => True);
                     Limit := Outer_Limit;
                     if Declared_Status /= Returned then
                        Body_Result := Failure (Declared_Status, Charged); return;
                     end if;
                     if Offset = Finish then Body_Result := Failure (Truncated, Charged); return; end if;
                     if Finish - Offset > Budget - Charged then
                        Body_Result := Failure (Budget_Exceeded, Charged); return;
                     end if;
                     Charged := Charged + (Finish - Offset);
                     Flags := Code (Code'First + Offset);
                     Offset := Offset + 1;
                     Declared_Status := Unsupported;
                     if Offset = Finish then
                        Define_Banked_Fields (Environment, Active_Lease, Current_Lexical_Scope,
                          Region_Path, Bank_Path, AML_Integers.Normalize (Selection.Number, Width),
                          Flags, [], Declared_Status);
                     else
                        Define_Banked_Fields (Environment, Active_Lease, Current_Lexical_Scope,
                          Region_Path, Bank_Path, AML_Integers.Normalize (Selection.Number, Width),
                          Flags, Code (Code'First + Offset .. Code'First + (Finish - 1)), Declared_Status);
                     end if;
                     if Declared_Status /= Returned then
                        Body_Result := Failure
                          ((if Declared_Status in Failure_Status then Declared_Status else Unsupported), Charged);
                        return;
                     end if;
                     Offset := Finish;
                  end;
               elsif Code (Code'First + Offset) in 16#82# .. 16#85# then
                  Open_Container (Offset - 1, State);
                  if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
               elsif Code (Code'First + Offset) = AML_Fatal.Fatal_Opcode then
                  declare
                     use type AML_Fatal.Framing_Status;
                     Header : constant AML_Fatal.Framing_Result := AML_Fatal.Read_Fixed
                       (Code (Code'First + Offset - 1 .. Code'First + Limit - 1));
                     Remaining_Fixed : constant Positive := AML_Fatal.Fixed_Bytes - 1;
                  begin
                     if Header.Status /= AML_Fatal.Accepted then
                        Body_Result := Failure (Truncated, Charged); return;
                     end if;
                     if Remaining_Fixed > Budget - Charged then
                        Body_Result := Failure (Budget_Exceeded, Charged); return;
                     end if;
                     Charged := Charged + Remaining_Fixed;
                     Offset := Offset + Remaining_Fixed;
                     Operand (Value, State, Require_Integer => True);
                     if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
                     if Value.Value_Kind /= Integer_Datum then
                        Body_Result := Failure (Unsupported_Value, Charged); return;
                     end if;
                     if not Pending_Fatal.Present then
                        Pending_Fatal := (Present => True,
                          Item => (Literal => Header.Item, Evaluated_Argument => Value.Number, Width => Width));
                     end if;
                     Body_Result := Failure (Fatal_Requested, Charged); return;
                  end;
               elsif Code (Code'First + Offset) = AML_Mutexes.Acquire_Opcode then
                  Offset := Offset - 1;
                  Operand (Value, State);
                  if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
               elsif Code (Code'First + Offset) = AML_Mutexes.Release_Opcode then
                  Offset := Offset + 1;
                  if Offset = Limit then Body_Result := Failure (Truncated, Charged); return; end if;
                  if not Mutex_Super_Name_Lead (Offset, Limit) then
                     Body_Result := Failure (Unsupported_Value, Charged); return;
                  end if;
                  declare
                     use type AML_Names.Parse_Status;
                     Path : AML_Names.Name_Result;
                     Binding : Binding_Result;
                     Source_Byte : constant Byte := Code (Code'First + Offset);
                     Resolved : Datum;
                  begin
                     if AML_Names.Lead (Source_Byte) or else Source_Byte in 16#5C# | 16#5E# | 16#2E# | 16#2F# then
                        if Charged = Budget then Body_Result := Failure (Budget_Exceeded, Charged); return; end if;
                        Charged := Charged + 1;
                        Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + Limit - 1));
                        if Path.Kind /= AML_Names.Accepted then
                           Body_Result := Failure ((if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name), Charged); return;
                        end if;
                        Lookup (Environment, Input, Current_Lexical_Scope, Path, Width, Namespace_Identity, Binding);
                        if Binding.Status /= Reference_Binding then Body_Result := Failure (Unsupported_Value, Charged); return; end if;
                        Value := (Reference_Datum, Binding.Ref); Offset := Offset + Path.Consumed;
                     else
                        Operand (Value, State);
                        if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
                     end if;
                     if Value.Value_Kind = Reference_Datum and then AML_References.Kind (Value.Ref) /= AML_References.Named_Cell then
                        Read_Reference_Value (Value.Ref, Resolved, State);
                        if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
                        Value := Resolved;
                     end if;
                     State := Unsupported_Value;
                     Operate_Mutex (Environment, Value, AML_Mutexes.Release_Mutex, 0, State);
                     if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
                  end;
               elsif Code (Code'First + Offset) in Signal_Extension | Reset_Extension then
                  declare
                     Action : constant AML_Events.Action :=
                       (if Code (Code'First + Offset) = Signal_Extension then AML_Events.Signal_Event else AML_Events.Reset_Event);
                     Timed_Out : Boolean := False;
                  begin
                     Offset := Offset + 1;
                     Operand (Value, State);
                     if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
                     State := Unsupported;
                     Operate_Event (Environment, Value, Action, AML_Events.Poll_Only, Timed_Out, State);
                     if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
                  end;
               elsif Code (Code'First + Offset) = AML_Mutexes.Declaration_Opcode then -- Mutex
                  Offset := Offset + 1;
                  declare
                     use type AML_Names.Parse_Status;
                     Path : AML_Names.Name_Result;
                     Declared_Status : Execution_Status := Unsupported;
                  begin
                     if Offset = Limit then Body_Result := Failure (Truncated, Charged); return; end if;
                     Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + Limit - 1));
                     if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                        Body_Result := Failure
                          ((if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name), Charged); return;
                     end if;
                     if Path.Consumed > Budget - Charged then
                        Body_Result := Failure (Budget_Exceeded, Charged); return;
                     end if;
                     Charged := Charged + Path.Consumed;
                     Offset := Offset + Path.Consumed;
                     if Offset = Limit then Body_Result := Failure (Truncated, Charged); return; end if;
                     if Charged = Budget then Body_Result := Failure (Budget_Exceeded, Charged); return; end if;
                     if not AML_Mutexes.Canonical_Flags (Code (Code'First + Offset)) then
                        Body_Result := Failure (Unsupported_Value, Charged); return;
                     end if;
                     Charged := Charged + 1;
                     Define_Mutex (Environment, Active_Lease, Current_Lexical_Scope, Path,
                       Code (Code'First + Offset), Declared_Status);
                     Offset := Offset + 1;
                     if Declared_Status /= Returned then
                        Body_Result := Failure (Declared_Status, Charged); return;
                     end if;
                  end;
               elsif Code (Code'First + Offset) = Event_Extension then -- Event
                  Offset := Offset + 1;
                  declare
                     use type AML_Names.Parse_Status;
                     Path : AML_Names.Name_Result;
                     Declared_Status : Execution_Status := Unsupported;
                  begin
                     if Offset = Limit then Body_Result := Failure (Truncated, Charged); return; end if;
                     Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + Limit - 1));
                     if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                        Body_Result := Failure
                          ((if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name), Charged); return;
                     end if;
                     if Path.Consumed > Budget - Charged then
                        Body_Result := Failure (Budget_Exceeded, Charged); return;
                     end if;
                     Charged := Charged + Path.Consumed;
                     Offset := Offset + Path.Consumed;
                     Define_Event (Environment, Active_Lease, Current_Lexical_Scope, Path, Declared_Status);
                     if Declared_Status /= Returned then
                        Body_Result := Failure (Declared_Status, Charged); return;
                     end if;
                  end;
               elsif Code (Code'First + Offset) in Conditional_Reference_Op | From_BCD_Extension | To_BCD_Extension | Wait_Extension | 16#33# then
                  Offset := Offset - 1;
                  Operand (Value, State);
                  if State /= Returned then
                     Body_Result := Failure (State, Charged); return;
                  end if;
               elsif Code (Code'First + Offset) = Create_Field_Extension then
                  Offset := Offset + 1;
                  Create_Buffer_Field (Generic_Field, State);
                  if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
               elsif Code (Code'First + Offset) = Operation_Region_Extension then
                  Offset := Offset + 1;
                  declare
                     use type AML_Names.Parse_Status;
                     use type AML_Declaration_Contexts.Operation_Region_Reservation;
                     Path : AML_Names.Name_Result;
                     Space : AML_Decode.Byte;
                     Token : AML_Declaration_Contexts.Operation_Region_Reservation :=
                       AML_Declaration_Contexts.No_Operation_Region;
                     Declared_Status : Execution_Status := Unsupported_Value;
                     Address, Length : Staged_Operand;
                     procedure Initialize_Operation_Region is
                     begin
                        if Offset = Limit then Declared_Status := Truncated; return; end if;
                        Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                        if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                           Declared_Status := (if Path.Kind = AML_Names.Truncated then Truncated else Bad_Name);
                           return;
                        end if;
                        if Path.Consumed > Budget - Charged then
                           Declared_Status := Budget_Exceeded; return;
                        end if;
                        Charged := Charged + Path.Consumed;
                        Offset := Offset + Path.Consumed;
                        if Offset = Limit then Declared_Status := Truncated; return; end if;
                        if Charged = Budget then Declared_Status := Budget_Exceeded; return; end if;
                        Charged := Charged + 1;
                        Space := Code (Code'First + Offset); Offset := Offset + 1;
                        Reserve_Operation_Region (Environment, Active_Lease,
                          Current_Lexical_Scope, Path, Space, Token, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        if Token = AML_Declaration_Contexts.No_Operation_Region then
                           Declared_Status := Unsupported_Value; return;
                        end if;
                        Read_Staged (Address, AML_Root_Slots.Region_Signature,
                          Declaration_Source, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Read_Staged (Length, AML_Root_Slots.Region_OEM,
                          Declaration_Index, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        -- Evaluate left-to-right; resolve completed operands
                        -- from the last operand, as for other declarations.
                        Resolve_Staged (Length, AML_Root_Slots.Region_OEM, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Convert_Integer_Value (Length.Item, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Resolve_Staged (Address, AML_Root_Slots.Region_Signature, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Convert_Integer_Value (Address.Item, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Declared_Status := Unsupported_Value;
                        Complete_Operation_Region (Environment, Active_Lease, Token, Width,
                          AML_Integers.Normalize (Address.Item.Number, Width),
                          AML_Integers.Normalize (Length.Item.Number, Width), Declared_Status);
                     end Initialize_Operation_Region;
                  begin
                     Initialize_Operation_Region;
                     if Tracking then
                        Transport.Clear_Range (Values,
                          Scratch_Location (Call_Frame, Declaration_Source),
                          Scratch_Location (Call_Frame, Declaration_Count));
                     end if;
                     for Root in AML_Root_Slots.Region_Signature .. AML_Root_Slots.Region_Table_ID loop
                        Publish_Held_Root (Environment, Call_Frame, Root, False,
                          (Integer_Datum, 0, Ordinary_Integer));
                     end loop;
                     if Declared_Status /= Returned then
                        Body_Result := Failure
                          ((if Declared_Status in Failure_Status then Declared_Status else Unsupported_Value), Charged);
                        return;
                     end if;
                  end;
               elsif Code (Code'First + Offset) = 16#88# then
                  Offset := Offset + 1;
                  declare
                     use type AML_Names.Parse_Status;
                     Path : AML_Names.Name_Result;
                     Token : AML_Declaration_Contexts.Region_Reservation;
                     Declared_Status : Execution_Status;
                     Signature, OEM, Table_ID : Staged_Operand;
                     procedure Initialize_Region is
                     begin
                        if Offset = Limit then Declared_Status := Bad_Name; return; end if;
                        Path := AML_Names.Read_Name (Code (Code'First + Offset .. Code'First + (Limit - 1)));
                        if Path.Kind /= AML_Names.Accepted or else Path.Count = 0 then
                           Declared_Status := Bad_Name; return;
                        end if;
                        Offset := Offset + Path.Consumed;
                        Reserve_Region (Environment, Active_Lease, Current_Lexical_Scope, Path, Token, Declared_Status);
                        if Declared_Status /= Returned then
                           if Declared_Status not in Failure_Status then Declared_Status := Unsupported; end if;
                           return;
                        end if;
                        if Token = AML_Declaration_Contexts.No_Region then
                           Declared_Status := Unsupported; return;
                        end if;
                        Read_Staged (Signature, AML_Root_Slots.Region_Signature, Declaration_Source, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Read_Staged (OEM, AML_Root_Slots.Region_OEM, Declaration_Index, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Read_Staged (Table_ID, AML_Root_Slots.Region_Table_ID, Declaration_Count, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        -- ACPICA resolves completed selectors from the last operand.
                        Resolve_Staged (Table_ID, AML_Root_Slots.Region_Table_ID, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Resolve_Staged (OEM, AML_Root_Slots.Region_OEM, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Resolve_Staged (Signature, AML_Root_Slots.Region_Signature, Declared_Status);
                        if Declared_Status /= Returned then return; end if;
                        Complete_Region (Environment, Active_Lease, Input, Token, Width,
                          Signature.Item, OEM.Item, Table_ID.Item, Declared_Status);
                     end Initialize_Region;
                  begin
                     Initialize_Region;
                     if Tracking then
                        Transport.Clear_Range (Values,
                          Scratch_Location (Call_Frame, Declaration_Source),
                          Scratch_Location (Call_Frame, Declaration_Count));
                     end if;
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
                  if Loading_Effects_Restricted then Body_Result := Failure (Unsupported, Charged); return; end if;
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
                     Define_Fields (Environment, Active_Lease, Current_Lexical_Scope, Region, Flags, [], Declared_Status);
                  else
                     Define_Fields (Environment, Active_Lease, Current_Lexical_Scope, Region, Flags,
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
                  if Code (Code'First + Offset) in 16#12# | 16#13# and then Runtime_Package_Supported then
                     declare
                        Token : Name_Reservation_Token := No_Name_Reservation_Token;
                        Cleanup : Execution_Status;
                     begin
                        Reserve_Runtime_Name (Environment, Active_Lease, Current_Lexical_Scope, Path, Token, Declared_Status);
                        if Declared_Status = Returned then
                           Operand (Value, Declared_Status, Package_Name => Token);
                           if Declared_Status /= Returned then
                              Abort_Runtime_Name (Environment, Active_Lease, Token, Cleanup);
                              if Cleanup /= Returned then Declared_Status := Unsupported_Value; end if;
                           end if;
                        end if;
                        if Declared_Status /= Returned then
                           Body_Result := Failure ((if Declared_Status in Failure_Status then Declared_Status else Unsupported_Value), Charged); return;
                        end if;
                     end;
                  elsif Active_Buffer_Count (Code (Code'First + Offset .. Code'First + (Limit - 1))) then
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
                        Reserve_Runtime_Name (Environment, Active_Lease, Current_Lexical_Scope, Path, Token, Declared_Status);
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
                        Complete_Runtime_Buffer (Environment, Active_Lease, Token, Width,
                             Code (Code'First + Initializer_Start .. Code'First + (Finish - 1)), Count, Declared_Status);
                        end if;
                        if Declared_Status /= Returned then
                           pragma Assert (Context_Valid (Environment));
                        Abort_Runtime_Name (Environment, Active_Lease, Token, Abort_Status);
                           pragma Assert (Abort_Status = Returned);
                           if Abort_Status /= Returned then Declared_Status := Unsupported_Value; end if;
                           Body_Result := Failure ((if Declared_Status in Failure_Status then Declared_Status else Unsupported_Value), Charged); return;
                        end if;
                        Offset := Finish;
                     end;
                  else
                  Define_Name (Environment, Active_Lease, Current_Lexical_Scope, Path, Width,
                    Code (Code'First + Offset .. Code'First + (Limit - 1)),
                    Source, Source_First + Offset, Used, Declared_Status);
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
                     Define_Method (Environment, Active_Lease, Current_Lexical_Scope, Path, Flags, Width, [], Declared_Status);
                  else
                     Define_Method (Environment, Active_Lease, Current_Lexical_Scope, Path, Flags, Width,
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
                     if Block_Depth = Block_Capacity then Body_Result := Failure (Block_Limit, Charged); return; end if;
                     Block_Depth := Block_Depth + 1;
                     Blocks (Block_Depth) :=
                       (Outer_Limit => Outer, Resume => Resume,
                        Restart => Start, Kind => (if Is_Loop then Loop_Block else Conditional_Block),
                        Prior_Scope => Current_Lexical_Scope);
                  end if;
               end;
            when 16#A5# | 16#9F# => --  Break / Continue, nearest loop only.
               declare
                  Found : Boolean := False;
               begin
                  for I in reverse 1 .. Block_Depth loop
                     if Blocks (I).Kind = Loop_Block then
                        for J in reverse I + 1 .. Block_Depth loop
                           if Blocks (J).Kind = Container_Block then
                              Pop_Lexical (State);
                              if State /= Returned or else Current_Lexical_Scope /= Blocks (J).Prior_Scope then
                                 Body_Result := Failure (Unsupported_Value, Charged); return;
                              end if;
                           end if;
                        end loop;
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
            when BreakPoint_Op =>
               -- ACPI 6.5 19.6.9: without an AML debugger, BreakPoint is Noop.
               -- GNAT assertion profiles do not enable an AML debugger. The
               -- common statement dispatch has already charged this opcode.
               null;
            when 16#A4# => -- Return
               if Module_Root then Body_Result := Failure (Unsupported, Charged); return; end if;
               Operand (Value, State);
               if State /= Returned then Body_Result := Failure (State, Charged); return; end if;
               Return_Source := Operand_Location;
               if Value.Value_Kind = Reference_Datum then
                  Body_Result := (Status => Reference_Returned, Charged => Charged, Ref => Value.Ref); return;
               elsif Value.Value_Kind = Metadata_Datum then
                  Body_Result := (Metadata_Returned, Charged, Value.Metadata); return;
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
         if Tracking then Clear_If_Present (Pending_Location (Call_Frame)); end if;
         Operand_Location := 0;
         end if;
      end loop;
      if Module_Root then
         Body_Result := (Status => Module_Completed, Charged => Charged);
      else Body_Result := Failure (No_Return, Charged); end if;
      return;
      end Execute_Body;
   begin
      Returned_Location := 0;
      if not AML_Byte_Sources.Corresponds (Source, Source_First, Code) then
         Result_Out := (Status => Unsupported_Value, Charged => 0); return;
      end if;
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
              AML_Frame_Handles.Cell_ID'Val (First_Argument_Cell + I), Args (I), Frame_Status, Sources (I));
            if Frame_Status /= Frames.Ready then Arguments_Ready := False; exit; end if;
            pragma Assert (Frame_Status = Frames.Ready);
         end if;
      end loop;
      -- ACPICA InitArgs installs the argument references before clearing the
      -- caller's operand array, prior to executing any callee instruction.
      -- The incoming locations are consumed even if a later binding failed.
      if Tracking then
         for I in Sources'Range loop
            if Sources (I) /= 0 then Clear_If_Present (Sources (I)); end if;
         end loop;
      end if;
      if Arguments_Ready then
         Begin_Call (Environment, Intent, Scope, Call_Frame, Active_Lease, Allowed);
      else Allowed := False;
      end if;
      if Allowed then
         if not Module_Root and then (Method_Flags and 8) /= 0 then
            Entry_Status := Unsupported_Value;
            Enter_Serialized_Method (Environment, Method_ID, Call_Frame, Entry_Status);
            Serialized_Entered := Entry_Status = Returned;
         end if;
         if Entry_Status /= Returned then
            declare
               Safe_Entry_Status : constant Payload_Free_Failure :=
                 (if Entry_Status in Payload_Free_Failure then Entry_Status else Unsupported_Value);
            begin
               Result_Out := (Status => Safe_Entry_Status, Charged => 0);
            end;
         elsif Body_Implementation = AML_Implementation then
            Execute_Body (Result_Out);
         elsif AML_Declaration_Contexts.Is_Module (Intent) then
            Result_Out := (Status => Invalid_Method, Charged => 0);
         elsif Budget = 0 then
            Result_Out := (Status => Budget_Exceeded, Charged => 0);
         else
            declare
               Internal_Body_Charge : constant Positive := 1;
               Reply : Internal_Method_Reply;
            begin
               Evaluate_Internal_Method (Environment, Active_Lease, Scope,
                 Body_Implementation, Args, Argument_Count, Reply);
               case Reply.Status is
                  when Internal_Unavailable =>
                     Result_Out := (Status => Invalid_Method, Charged => Internal_Body_Charge);
                  when Internal_Rejected =>
                     Result_Out := (Status => Unsupported_Value, Charged => Internal_Body_Charge);
                  when Internal_Returned =>
                     Result_Out := (Status => Returned, Charged => Internal_Body_Charge,
                       Value => Reply.Value, Origin => AML_Decode.Ordinary_Integer);
               end case;
            end;
         end if;
         if Pending_Fatal.Present then
            Result_Out := (Status => Fatal_Requested, Charged => Result_Out.Charged, Fatal => Pending_Fatal.Item);
         end if;
         -- Scope teardown must not collect the selected return value. This
         -- publication borrows the result; canonical transport owns its lease.
         case Result_Out.Status is
            when Returned =>
               if Return_Source /= 0 then
                  Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Returned_Value,
                    True, Transport.Root_Projection (Values, Return_Source));
               end if;
            when Object_Returned =>
               Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Returned_Value,
                 True, (Object_Datum, Result_Out.Object));
            when Reference_Returned =>
               Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Returned_Value,
                 True, (Reference_Datum, Result_Out.Ref));
            when Metadata_Returned =>
               Publish_Held_Root (Environment, Call_Frame, AML_Root_Slots.Returned_Value,
                 True, (Metadata_Datum, Result_Out.Metadata));
            when others => null;
         end case;
         -- Restore lexical authority before publishing a result or closing its owner.
         -- End_Call closes the whole lease ledger even if a pop fails.
         while Lexical_Depth > 0 loop
            declare Cleanup_Status : Execution_Status; begin
               Pop_Lexical (Cleanup_Status);
               if Cleanup_Status /= Returned then
                  if Result_Out.Status not in Execution_Failure or else Result_Out.Status = No_Return then
                     Result_Out := (Status => Unsupported_Value, Charged => Result_Out.Charged);
                  end if;
                  exit;
               end if;
            end;
         end loop;
         if Result_Out.Status = Reference_Returned
           and then AML_References.Kind (Result_Out.Ref) = AML_References.Frame_Cell
           and then AML_Frame_Handles.Matches (AML_References.Frame_Item (Result_Out.Ref), Call_Frame)
         then Result_Out := (Status => No_Return, Charged => Result_Out.Charged); end if;
         if Tracking and then Result_Out.Status in
           Returned | Object_Returned | Reference_Returned | Metadata_Returned
         then
            declare
               Stored : Transport.Outcome;
               Returned_Value : constant Datum :=
                 (case Result_Out.Status is
                    when Returned => (Integer_Datum, Result_Out.Value, Result_Out.Origin),
                    when Object_Returned => (Object_Datum, Result_Out.Object),
                    when Reference_Returned => (Reference_Datum, Result_Out.Ref),
                    when Metadata_Returned => (Metadata_Datum, Result_Out.Metadata),
                    when others => (Integer_Datum, 0, Ordinary_Integer));
            begin
               if Return_Source = 0 then
                  Transport.Put (Values, Result_Location (Call_Frame), Returned_Value, Stored);
               else
                  -- Return_Source is a live canonical owner, not merely a
                  -- location that formerly held a value with matching bits.
                  declare
                     Current : Datum := Returned_Value;
                     Read_Status : Execution_Status;
                  begin
                     Refresh_Compound_Location (Return_Source, Returned_Value, Read_Status);
                     if Read_Status = Returned then
                        Refresh_Scalar_Location (Return_Source, Current, Read_Status);
                     end if;
                     if Read_Status /= Returned
                       or else Current /= Returned_Value
                       or else Transport.Snapshot (Values, Return_Source) /= Returned_Value
                     then Stored := Transport.Invalid_Source;
                     else
                        Transport.Share (Values, Return_Source,
                          Result_Location (Call_Frame), Stored);
                     end if;
                  end;
               end if;
               if Stored = Transport.Ready then Returned_Location := Result_Location (Call_Frame);
               elsif Stored = Transport.Invalid_Source then
                  Result_Out := (Status => Unsupported_Value, Charged => Result_Out.Charged);
               else Result_Out := (Status => Value_Limit, Charged => Result_Out.Charged); end if;
            end;
         end if;
         -- Transfer the result before the callee's method-owned names and
         -- cells disappear. This single bridge is conservative until frame exit
         -- or a later compound return; expression-stack roots remain separate.
         if Caller_Frame /= AML_Frame_Handles.No_Frame then
            if Result_Out.Status in Returned | Object_Returned | Reference_Returned | Metadata_Returned
              and then Returned_Location /= 0 then
               Publish_Held_Root (Environment, Caller_Frame, AML_Root_Slots.Returned_Value,
                 True, Transport.Root_Projection (Values, Returned_Location));
            elsif Result_Out.Status = Object_Returned then
               Publish_Held_Root (Environment, Caller_Frame, AML_Root_Slots.Returned_Value,
                 True, (Object_Datum, Result_Out.Object));
            elsif Result_Out.Status = Metadata_Returned then
               Publish_Held_Root (Environment, Caller_Frame, AML_Root_Slots.Returned_Value,
                 True, (Metadata_Datum, Result_Out.Metadata));
            elsif Result_Out.Status = Reference_Returned then
               Publish_Held_Root (Environment, Caller_Frame, AML_Root_Slots.Returned_Value,
                 True, (Reference_Datum, Result_Out.Ref));
            end if;
         end if;
         if Calls_Left = Root_Call_Budget
           and then Result_Out.Status in Object_Returned | Reference_Returned | Metadata_Returned
         then Handoff_Result (Environment, Result_Out); end if;
         if Serialized_Entered then
            declare
               Leave_Status : Execution_Status := Unsupported_Value;
            begin
               Leave_Serialized_Method (Environment, Call_Frame, Leave_Status);
               if Leave_Status /= Returned then
                  -- Owner state must be discharged before retiring the method.
                  raise Program_Error with "serialized method leave invariant";
               end if;
            end;
         end if;
         End_Call (Environment, Active_Lease);
      else
         if Arguments_Ready then Result_Out := (Status => Namespace_Limit, Charged => 0);
         else Result_Out := (Status => Value_Limit, Charged => 0); end if;
      end if;
      if Tracking then
         -- The returned root is the sole permitted survivor of frame close;
         -- Dispatch moves it before any later child can reuse this frame index.
         declare
            First : constant Transport.Slot := Frame_Location (Call_Frame, AML_Frame_Handles.Cell_ID'First);
            Last : constant Transport.Slot := First + Transport_Slots_Per_Frame - 1;
         begin
            if Returned_Location = 0 then Transport.Clear_Range (Values, First, Last);
            else
               Transport.Clear_Range (Values, First, Returned_Location - 1);
               Transport.Clear_Range (Values, Returned_Location + 1, Last);
            end if;
         end;
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
      if not AML_Byte_Sources.Corresponds (Source, Source_First, Code) then
         Result_Out := (Status => Unsupported_Value, Charged => 0); return;
      end if;
      if Current_Sync /= 0 then
         Result_Out := (Status => Unsupported_Value, Charged => 0); return;
      end if;
      Begin_Invocation (Environment, Domain, Issued);
      if Issued = Exhausted then
         Result_Out := (Status => Value_Limit, Charged => 0); return;
      end if;
      if Issued = Available and then not AML_Frame_Handles.Has_Authority (Domain) then
         Result_Out := (Status => Unsupported_Value, Charged => 0); return;
      end if;
      if Issued = Unsupported_Context then
         if Require_Invocation_Authority then
            Result_Out := (Status => Unsupported_Value, Charged => 0); return;
         end if;
         Domain := AML_Frame_Handles.No_Domain;
      end if;
      Registry := Frames.Empty (Domain);
      Transport.Initialize (Values, Domain);
      Tracking := AML_Frame_Handles.Has_Authority (Domain);
      declare
         Root_Method : constant Method_Definition := Get_Method (Environment, Scope);
      begin
         Run (Code, Source, Source_First, Width, Args, Argument_Count, Budget, Scope, Result_Out, Calls_Left,
           Scope, (if AML_Declaration_Contexts.Is_Module (Request) or else not Root_Method.Exists then 0 else Root_Method.Flags),
           AML_Frame_Handles.No_Frame, Request, Implementation, [others => 0], Root_Result_Location);
      end;
      Transport.Clear_All (Values);
   end Execute_From_Source;

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
         pragma Unmodified (Environment);
         pragma Unreferenced (Scope, Region, Flags, Entries);
      begin
         Status := Unsupported;
      end Reject_Fields;
      procedure Reject_Buffer_Literal
        (Environment : in out Context; Scope : Natural;
         Length : AML_Buffer_Layouts.Buffer_Length; Initializer : Bytes;
         Binding : out Binding_Result) is
         pragma Unreferenced (Environment, Scope, Length, Initializer);
      begin
         Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
      end Reject_Buffer_Literal;
      procedure Reject_Literal
        (Environment : in out Context; Scope : Natural; Kind : Literal_Kind; Width : Integer_Width; Data : Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
         Binding : out Binding_Result)
        with Pre => Context_Valid (Environment) and then not Binding'Constrained,
             Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Kind, Width, Data, Source, Source_First);
      begin
         Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
      end Reject_Literal;
      procedure Reject_Region
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Natural; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unmodified (Environment);
         pragma Unreferenced (Scope, Path);
      begin
         Token := 0; Status := Unsupported;
      end Reject_Region;
      procedure Reject_Completion
        (Environment : in out Context; Input : aliased No_Input; Token : Natural;
         Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unmodified (Environment);
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
         Width : Integer_Width;
         Value : out Datum; Status : out Execution_Status)
        with Pre => Context_Valid (Environment) and then not Value'Constrained,
             Post => Context_Valid (Environment)
      is
         pragma Unmodified (Environment);
         pragma Unreferenced (Ref, Width);
      begin Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Reject_Reference;
      procedure Reject_Buffer_Field
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : Integer_Width; Source : Datum; Bit_Offset, Bit_Length : Integer_Value;
         Always_Buffer : Boolean; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unmodified (Environment);
         pragma Unreferenced (Scope, Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer);
      begin Status := Unsupported_Value; end Reject_Buffer_Field;
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
      procedure Reject_Metadata_Value
        (Environment : Context; Item : Metadata_Value; Result : out Reference_Metadata) is
         pragma Unreferenced (Environment, Item);
      begin Result := (Kind => Invalid_Reference); end Reject_Metadata_Value;
      procedure Reject_Notify
        (Environment : in out Context; Target : AML_References.Reference;
         Value : Integer_Value; Status : out Execution_Status)
      is
         pragma Unreferenced (Environment, Target, Value);
      begin
         Status := Unsupported;
      end Reject_Notify;
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
            -- Method-only explicit declaration leases. No module lifetime is enabled.
      Execute_Declaration_Lease_Store : AML_Declaration_Contexts.State := AML_Declaration_Contexts.Empty;
      use type AML_Declaration_Contexts.Request;
      use type AML_Declaration_Contexts.Lease;
      type Execute_Declaration_Name_Token is record
         Owner : AML_Declaration_Contexts.Lease := AML_Declaration_Contexts.No_Lease;
         Original : Boolean := False;
      end record;
      Execute_Declaration_No_Name : constant Execute_Declaration_Name_Token := (others => <>);
      function Execute_Declaration_Valid (Environment : Context; Lease : AML_Declaration_Contexts.Lease) return Boolean
      is
         pragma Unreferenced (Environment);
      begin
         return AML_Declaration_Contexts.Is_Live (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Valid;
      procedure Execute_Declaration_Enter
        (Environment : in out Context; Intent : AML_Declaration_Contexts.Request; Scope : Natural;
         Frame : AML_Frame_Handles.Frame_Handle; Lease : out AML_Declaration_Contexts.Lease; Allowed : out Boolean) is
      begin
         Lease := AML_Declaration_Contexts.No_Lease; Allowed := False;
         if Intent /= AML_Declaration_Contexts.Method_Request then return; end if;
         Begin_Call (Environment, Scope, Allowed);
         if not Allowed then return; end if;
         AML_Declaration_Contexts.Open (Execute_Declaration_Lease_Store, Intent, Frame, Scope, 0, Lease, Allowed);
         if not Allowed then End_Call (Environment, Scope); end if;
      end Execute_Declaration_Enter;
      procedure Execute_Declaration_Leave (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) then return; end if;
         End_Call (Environment, AML_Declaration_Contexts.Scope_Of (Lease));
         AML_Declaration_Contexts.Close (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Leave;
      procedure Execute_Declaration_Method
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width; Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Declaration_Unsupported; return;
         end if;
         Define_Method (Environment, Scope, Path, Flags, Width, Code, Status);
      end Execute_Declaration_Method;
      procedure Execute_Declaration_Fields
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Region : AML_Names.Name_Result; Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Fields (Environment, Scope, Region, Flags, Entries, Status);
      end Execute_Declaration_Fields;
      procedure Execute_Declaration_Buffer_Field
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum; Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Buffer_Field (Environment, Scope, Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer, Status);
      end Execute_Declaration_Buffer_Field;
      procedure Execute_Declaration_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes; Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Consumed : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Source, Source_First);
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Consumed := 0; Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Name (Environment, Scope, Path, Width, Data, Consumed, Status);
      end Execute_Declaration_Name;
      procedure Execute_Declaration_Reserve_Region
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out AML_Declaration_Contexts.Region_Reservation; Status : out AML_Execute.Execution_Status) is
         Node : Natural;
      begin
         Token := AML_Declaration_Contexts.No_Region; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Reject_Region (Environment, Scope, Path, Node, Status);
         if Status in AML_Execute.Returned and then Node /= 0 then
            Token := AML_Declaration_Contexts.Reserve_Region (Lease, Node, 0);
         end if;
      end Execute_Declaration_Reserve_Region;
      procedure Execute_Declaration_Complete_Region
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Input : aliased No_Input;
         Token : AML_Declaration_Contexts.Region_Reservation; Width : AML_Decode.Integer_Width;
         Signature, OEM, Table_ID : AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         Node : constant Natural := AML_Declaration_Contexts.Node_Of (Token);
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else not AML_Declaration_Contexts.Issued_By (Token, Lease) then return; end if;
         Reject_Completion (Environment, Input, Node, Width, Signature, OEM, Table_ID, Status);
      end Execute_Declaration_Complete_Region;
      procedure Execute_Declaration_Reserve_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Execute_Declaration_Name_Token; Status : out AML_Execute.Execution_Status) is
      begin
         Token := Execute_Declaration_No_Name; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Reserve_Dynamic_Name (Environment, Scope, Path, Token.Original, Status);
         if Status in AML_Execute.Returned then Token.Owner := Lease; end if;
      end Execute_Declaration_Reserve_Name;
      procedure Execute_Declaration_Complete_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Complete_Dynamic_Buffer (Environment, Token.Original, Width, Initializer, Count, Status);
      end Execute_Declaration_Complete_Name;
      procedure Execute_Declaration_Abort_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Abort_Dynamic_Name (Environment, Token.Original, Status);
      end Execute_Declaration_Abort_Name;
      procedure Reject_Alias
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Source_Path, Target_Path : AML_Names.Name_Result;
         Status : out AML_Execute.Execution_Status) is
         pragma Unmodified (Environment);
         pragma Unreferenced (Lease, Scope, Source_Path, Target_Path);
      begin Status := AML_Execute.Unsupported; end Reject_Alias;
      procedure Execute_Unsupported_Container_Enter
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Metadata : AML_Container_Headers.Attributes; New_Scope : out Natural;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Path, Metadata);
      begin
         New_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Enter;
      procedure Execute_Unsupported_Container_Leave
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Restored_Scope : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Restored_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Leave;
      procedure Provenance_Execute_Resolve
        (Environment : in out Context; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width; Value : out AML_Execute.Datum;
         Provenance : out AML_Execute.Scalar_Provenance;
         Status : out AML_Execute.Execution_Status) is
      begin
         Provenance := AML_Execute.Detached_Provenance;
         Reject_Reference (Environment, Ref, Width, Value, Status);
      end Provenance_Execute_Resolve;
      procedure Provenance_Execute_Read_Integer
        (Environment : Context; Source : AML_References.Object_Handle;
         Value : out AML_Decode.Integer_Value; Origin : out AML_Decode.Integer_Origin;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Source);
      begin
         Value := 0; Origin := AML_Decode.Ordinary_Integer;
         Status := AML_Execute.Unsupported_Value;
      end Provenance_Execute_Read_Integer;
      procedure Reject_Integer_Normalization
        (Environment : in out Context; Source : AML_References.Object_Handle;
         Width : Integer_Width; Value : out Integer_Value; Origin : out Integer_Origin;
         Status : out Execution_Status) is
         pragma Unreferenced (Environment, Source, Width);
      begin
         Value := 0; Origin := Ordinary_Integer; Status := Unsupported_Value;
      end Reject_Integer_Normalization;
      procedure Reject_Integer_Normalization_Census_Begin
        (Environment : Context; Source : AML_References.Object_Handle;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Source);
      begin Status := AML_Execute.Census_Invalid; end Reject_Integer_Normalization_Census_Begin;
      procedure Reject_Integer_Normalization_Census_Add
        (Environment : Context; Value : AML_Execute.Datum;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Value);
      begin Status := AML_Execute.Census_Invalid; end Reject_Integer_Normalization_Census_Add;
      procedure Reject_Integer_Normalization_Census_Finish
        (Environment : Context; Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment);
      begin Status := AML_Execute.Census_Invalid; end Reject_Integer_Normalization_Census_Finish;

      procedure Package_Adapter_Begin
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Count, Child_Byte_Bound : Natural;
         Parent : Natural; Name : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Token : out Natural;
         Binding : out AML_Execute.Binding_Result) is
         pragma Unreferenced (Environment, Lease, Scope, Count, Child_Byte_Bound, Parent, Name, Width);
      begin
         Token := 0;
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      end Package_Adapter_Begin;
      procedure Package_Adapter_Append
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Item : AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Item);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Append;
      procedure Package_Adapter_Defer
        (Environment : Context; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Scope : Natural;
         Path : AML_Names.Name_Result; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Scope, Path);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Defer;
      procedure Package_Adapter_Finish
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Binding : out AML_Execute.Binding_Result) is
         pragma Unreferenced (Environment, Lease, Token);
      begin
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      end Package_Adapter_Finish;
      -- Rejected builders never own an allocation or root to unwind.
      procedure Package_Adapter_Abort
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Status := AML_Execute.Returned;
      end Package_Adapter_Abort;
      procedure Execute is new Execute_From_Source
        (Context,
         No_Input,
         Context_Valid,
         Read_Binding,
         Get_Method,
         Write,
         Execute_Declaration_Enter,
         Execute_Declaration_Leave,
         Execute_Declaration_Method,
         Execute_Declaration_Fields,
         Reject_Literal,
         Execute_Declaration_Reserve_Region,
         Execute_Declaration_Complete_Region,
         No_Timer,
         Provenance_Execute_Resolve,
         Provenance_Execute_Read_Integer,
         Reject_Integer_Normalization,
         Reject_Integer_Normalization_Census_Begin, Reject_Integer_Normalization_Census_Add, Reject_Integer_Normalization_Census_Finish,
         Execute_Declaration_Buffer_Field,
         Reject_Index,
         Reject_Store,
         Clone_Value,
         Reject_Compare,
         Keep_Value,
         No_Invocation,
         Execute_Declaration_Name,
         Reject_Copy_Attachment,
         Describe_Identity,
         Execute_Declaration_Name_Token,
         Execute_Declaration_No_Name,
         Execute_Declaration_Reserve_Name,
         Execute_Declaration_Complete_Name,
         Execute_Declaration_Abort_Name,
         Disabled_Debug,
         Reject_Explicit_Integer,
         Reject_Concatenation,
         Reject_To_Buffer,
         Reject_Mid,
         Reject_To_String,
         Reject_Format_String,
         Reject_Match,
         Reject_Resources,
         Wait_For_Delay => Unavailable_Delay,
         Queue_Notify => Reject_Notify,
         Describe_Metadata_Value => Reject_Metadata_Value,
         Materialize_Buffer => Reject_Buffer_Literal,
         Define_Alias => Reject_Alias,
         Enter_Container => Execute_Unsupported_Container_Enter,
         Leave_Container => Execute_Unsupported_Container_Leave,
         Runtime_Package_Token => Natural,
         No_Runtime_Package => 0,
         Begin_Runtime_Package => Package_Adapter_Begin,
         Append_Runtime_Package => Package_Adapter_Append,
         Defer_Runtime_Package_Name => Package_Adapter_Defer,
         Finish_Runtime_Package => Package_Adapter_Finish,
         Abort_Runtime_Packages => Package_Adapter_Abort);
      Source : aliased constant AML_Byte_Sources.Storage :=
        (Length => Code'Length, Bytes => Code);
   begin
      Execute (Code, Width, Args, Argument_Count, Budget, Empty, Environment,
               Scope, Result_Out, Source, 0, Calls_Left, Current_Sync);
   end Execute_Typed;

   procedure Execute_Session_Typed
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
         pragma Unmodified (Environment);
         pragma Unreferenced (Scope, Region, Flags, Entries);
      begin
         Status := Unsupported;
      end Reject_Fields;
      procedure Reject_Buffer_Literal
        (Environment : in out Context; Scope : Natural;
         Length : AML_Buffer_Layouts.Buffer_Length; Initializer : Bytes;
         Binding : out Binding_Result) is
         pragma Unreferenced (Environment, Scope, Length, Initializer);
      begin
         Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
      end Reject_Buffer_Literal;
      procedure Reject_Literal
        (Environment : in out Context; Scope : Natural; Kind : Literal_Kind; Width : Integer_Width; Data : Bytes;
         Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
         Binding : out Binding_Result)
        with Pre => Context_Valid (Environment) and then not Binding'Constrained,
             Post => Context_Valid (Environment)
      is
         pragma Unreferenced (Scope, Kind, Width, Data, Source, Source_First);
      begin
         Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
      end Reject_Literal;
      procedure Reject_Region
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Natural; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unmodified (Environment);
         pragma Unreferenced (Scope, Path);
      begin
         Token := 0; Status := Unsupported;
      end Reject_Region;
      procedure Reject_Completion
        (Environment : in out Context; Input : aliased No_Input; Token : Natural;
         Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unmodified (Environment);
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
         Width : Integer_Width;
         Value : out Datum; Status : out Execution_Status)
        with Pre => Context_Valid (Environment) and then not Value'Constrained,
             Post => Context_Valid (Environment)
      is
         pragma Unmodified (Environment);
         pragma Unreferenced (Ref, Width);
      begin Value := (Value_Kind => Integer_Datum, Number => 0, Origin => AML_Decode.Ordinary_Integer); Status := Unsupported_Value; end Reject_Reference;
      procedure Reject_Buffer_Field
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : Integer_Width; Source : Datum; Bit_Offset, Bit_Length : Integer_Value;
         Always_Buffer : Boolean; Status : out Execution_Status)
        with Pre => Context_Valid (Environment), Post => Context_Valid (Environment)
      is
         pragma Unmodified (Environment);
         pragma Unreferenced (Scope, Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer);
      begin Status := Unsupported_Value; end Reject_Buffer_Field;
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
      procedure Reject_Metadata_Value
        (Environment : Context; Item : Metadata_Value; Result : out Reference_Metadata) is
         pragma Unreferenced (Environment, Item);
      begin Result := (Kind => Invalid_Reference); end Reject_Metadata_Value;
      procedure Reject_Notify
        (Environment : in out Context; Target : AML_References.Reference;
         Value : Integer_Value; Status : out Execution_Status)
      is
         pragma Unreferenced (Environment, Target, Value);
      begin
         Status := Unsupported;
      end Reject_Notify;
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
            -- Method-only explicit declaration leases. No module lifetime is enabled.
      Execute_Declaration_Lease_Store : AML_Declaration_Contexts.State := AML_Declaration_Contexts.Empty;
      use type AML_Declaration_Contexts.Request;
      use type AML_Declaration_Contexts.Lease;
      type Execute_Declaration_Name_Token is record
         Owner : AML_Declaration_Contexts.Lease := AML_Declaration_Contexts.No_Lease;
         Original : Boolean := False;
      end record;
      Execute_Declaration_No_Name : constant Execute_Declaration_Name_Token := (others => <>);
      function Execute_Declaration_Valid (Environment : Context; Lease : AML_Declaration_Contexts.Lease) return Boolean
      is
         pragma Unreferenced (Environment);
      begin
         return AML_Declaration_Contexts.Is_Live (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Valid;
      procedure Execute_Declaration_Enter
        (Environment : in out Context; Intent : AML_Declaration_Contexts.Request; Scope : Natural;
         Frame : AML_Frame_Handles.Frame_Handle; Lease : out AML_Declaration_Contexts.Lease; Allowed : out Boolean) is
      begin
         Lease := AML_Declaration_Contexts.No_Lease; Allowed := False;
         if Intent /= AML_Declaration_Contexts.Method_Request then return; end if;
         Begin_Call (Environment, Scope, Allowed);
         if not Allowed then return; end if;
         AML_Declaration_Contexts.Open (Execute_Declaration_Lease_Store, Intent, Frame, Scope, 0, Lease, Allowed);
         if not Allowed then End_Call (Environment, Scope); end if;
      end Execute_Declaration_Enter;
      procedure Execute_Declaration_Leave (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) then return; end if;
         End_Call (Environment, AML_Declaration_Contexts.Scope_Of (Lease));
         AML_Declaration_Contexts.Close (Execute_Declaration_Lease_Store, Lease);
      end Execute_Declaration_Leave;
      procedure Execute_Declaration_Method
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width; Code : AML_Decode.Bytes; Status : out AML_Execute.Declaration_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Declaration_Unsupported; return;
         end if;
         Define_Method (Environment, Scope, Path, Flags, Width, Code, Status);
      end Execute_Declaration_Method;
      procedure Execute_Declaration_Fields
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Region : AML_Names.Name_Result; Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Fields (Environment, Scope, Region, Flags, Entries, Status);
      end Execute_Declaration_Fields;
      procedure Execute_Declaration_Buffer_Field
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Source : AML_Execute.Datum; Bit_Offset, Bit_Length : AML_Decode.Integer_Value; Always_Buffer : Boolean; Status : out AML_Execute.Execution_Status) is
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Buffer_Field (Environment, Scope, Path, Width, Source, Bit_Offset, Bit_Length, Always_Buffer, Status);
      end Execute_Declaration_Buffer_Field;
      procedure Execute_Declaration_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result; Width : AML_Decode.Integer_Width; Data : AML_Decode.Bytes; Source : aliased AML_Byte_Sources.Storage; Source_First : Natural; Consumed : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Source, Source_First);
      begin
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then
            Consumed := 0; Status := AML_Execute.Unsupported_Value; return;
         end if;
         Reject_Name (Environment, Scope, Path, Width, Data, Consumed, Status);
      end Execute_Declaration_Name;
      procedure Execute_Declaration_Reserve_Region
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out AML_Declaration_Contexts.Region_Reservation; Status : out AML_Execute.Execution_Status) is
         Node : Natural;
      begin
         Token := AML_Declaration_Contexts.No_Region; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Reject_Region (Environment, Scope, Path, Node, Status);
         if Status in AML_Execute.Returned and then Node /= 0 then
            Token := AML_Declaration_Contexts.Reserve_Region (Lease, Node, 0);
         end if;
      end Execute_Declaration_Reserve_Region;
      procedure Execute_Declaration_Complete_Region
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Input : aliased No_Input;
         Token : AML_Declaration_Contexts.Region_Reservation; Width : AML_Decode.Integer_Width;
         Signature, OEM, Table_ID : AML_Execute.Datum; Status : out AML_Execute.Execution_Status) is
         Node : constant Natural := AML_Declaration_Contexts.Node_Of (Token);
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else not AML_Declaration_Contexts.Issued_By (Token, Lease) then return; end if;
         Reject_Completion (Environment, Input, Node, Width, Signature, OEM, Table_ID, Status);
      end Execute_Declaration_Complete_Region;
      procedure Execute_Declaration_Reserve_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Scope : Natural; Path : AML_Names.Name_Result;
         Token : out Execute_Declaration_Name_Token; Status : out AML_Execute.Execution_Status) is
      begin
         Token := Execute_Declaration_No_Name; Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Scope /= AML_Declaration_Contexts.Scope_Of (Lease) then return; end if;
         Reserve_Dynamic_Name (Environment, Scope, Path, Token.Original, Status);
         if Status in AML_Execute.Returned then Token.Owner := Lease; end if;
      end Execute_Declaration_Reserve_Name;
      procedure Execute_Declaration_Complete_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Initializer : AML_Decode.Bytes; Count : AML_Data.Count_Result;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Complete_Dynamic_Buffer (Environment, Token.Original, Width, Initializer, Count, Status);
      end Execute_Declaration_Complete_Name;
      procedure Execute_Declaration_Abort_Name
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease; Token : Execute_Declaration_Name_Token;
         Status : out AML_Execute.Execution_Status) is
      begin
         Status := AML_Execute.Unsupported_Value;
         if not Execute_Declaration_Valid (Environment, Lease) or else Token.Owner /= Lease then return; end if;
         Abort_Dynamic_Name (Environment, Token.Original, Status);
      end Execute_Declaration_Abort_Name;
      procedure Reject_Alias
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Source_Path, Target_Path : AML_Names.Name_Result;
         Status : out AML_Execute.Execution_Status) is
         pragma Unmodified (Environment);
         pragma Unreferenced (Lease, Scope, Source_Path, Target_Path);
      begin Status := AML_Execute.Unsupported; end Reject_Alias;
      procedure Execute_Unsupported_Container_Enter
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Path : AML_Names.Name_Result;
         Metadata : AML_Container_Headers.Attributes; New_Scope : out Natural;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Path, Metadata);
      begin
         New_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Enter;
      procedure Execute_Unsupported_Container_Leave
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Restored_Scope : out Natural; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Restored_Scope := Scope; Status := AML_Execute.Unsupported;
      end Execute_Unsupported_Container_Leave;
      procedure Provenance_Execute_Resolve
        (Environment : in out Context; Ref : AML_References.Reference;
         Width : AML_Decode.Integer_Width; Value : out AML_Execute.Datum;
         Provenance : out AML_Execute.Scalar_Provenance;
         Status : out AML_Execute.Execution_Status) is
      begin
         Provenance := AML_Execute.Detached_Provenance;
         Reject_Reference (Environment, Ref, Width, Value, Status);
      end Provenance_Execute_Resolve;
      procedure Provenance_Execute_Read_Integer
        (Environment : Context; Source : AML_References.Object_Handle;
         Value : out AML_Decode.Integer_Value; Origin : out AML_Decode.Integer_Origin;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Source);
      begin
         Value := 0; Origin := AML_Decode.Ordinary_Integer;
         Status := AML_Execute.Unsupported_Value;
      end Provenance_Execute_Read_Integer;
      procedure Reject_Integer_Normalization
        (Environment : in out Context; Source : AML_References.Object_Handle;
         Width : Integer_Width; Value : out Integer_Value; Origin : out Integer_Origin;
         Status : out Execution_Status) is
         pragma Unreferenced (Environment, Source, Width);
      begin
         Value := 0; Origin := Ordinary_Integer; Status := Unsupported_Value;
      end Reject_Integer_Normalization;
      procedure Reject_Integer_Normalization_Census_Begin
        (Environment : Context; Source : AML_References.Object_Handle;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Source);
      begin Status := AML_Execute.Census_Invalid; end Reject_Integer_Normalization_Census_Begin;
      procedure Reject_Integer_Normalization_Census_Add
        (Environment : Context; Value : AML_Execute.Datum;
         Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment, Value);
      begin Status := AML_Execute.Census_Invalid; end Reject_Integer_Normalization_Census_Add;
      procedure Reject_Integer_Normalization_Census_Finish
        (Environment : Context; Status : out AML_Execute.Owner_Census_Status) is
         pragma Unreferenced (Environment);
      begin Status := AML_Execute.Census_Invalid; end Reject_Integer_Normalization_Census_Finish;

      procedure Package_Adapter_Begin
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Scope : Natural; Count, Child_Byte_Bound : Natural;
         Parent : Natural; Name : Execute_Declaration_Name_Token;
         Width : AML_Decode.Integer_Width; Token : out Natural;
         Binding : out AML_Execute.Binding_Result) is
         pragma Unreferenced (Environment, Lease, Scope, Count, Child_Byte_Bound, Parent, Name, Width);
      begin
         Token := 0;
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      end Package_Adapter_Begin;
      procedure Package_Adapter_Append
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Item : AML_Execute.Datum;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Item);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Append;
      procedure Package_Adapter_Defer
        (Environment : Context; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Scope : Natural;
         Path : AML_Names.Name_Result; Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease, Token, Scope, Path);
      begin
         Status := AML_Execute.Unsupported_Value;
      end Package_Adapter_Defer;
      procedure Package_Adapter_Finish
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Token : in out Natural; Binding : out AML_Execute.Binding_Result) is
         pragma Unreferenced (Environment, Lease, Token);
      begin
         Binding := (Status => AML_Execute.Failed_Binding, Failure => AML_Execute.Unsupported_Value);
      end Package_Adapter_Finish;
      -- Rejected builders never own an allocation or root to unwind.
      procedure Package_Adapter_Abort
        (Environment : in out Context; Lease : AML_Declaration_Contexts.Lease;
         Status : out AML_Execute.Execution_Status) is
         pragma Unreferenced (Environment, Lease);
      begin
         Status := AML_Execute.Returned;
      end Package_Adapter_Abort;
      procedure Execute is new Execute_From_Source
        (Context,
         No_Input,
         Context_Valid,
         Read_Binding,
         Get_Method,
         Write,
         Execute_Declaration_Enter,
         Execute_Declaration_Leave,
         Execute_Declaration_Method,
         Execute_Declaration_Fields,
         Reject_Literal,
         Execute_Declaration_Reserve_Region,
         Execute_Declaration_Complete_Region,
         No_Timer,
         Provenance_Execute_Resolve,
         Provenance_Execute_Read_Integer,
         Reject_Integer_Normalization,
         Reject_Integer_Normalization_Census_Begin, Reject_Integer_Normalization_Census_Add, Reject_Integer_Normalization_Census_Finish,
         Execute_Declaration_Buffer_Field,
         Reject_Index,
         Reject_Store,
         Clone_Value,
         Reject_Compare,
         Keep_Value,
         Admit_Session,
         Execute_Declaration_Name,
         Reject_Copy_Attachment,
         Describe_Identity,
         Execute_Declaration_Name_Token,
         Execute_Declaration_No_Name,
         Execute_Declaration_Reserve_Name,
         Execute_Declaration_Complete_Name,
         Execute_Declaration_Abort_Name,
         Disabled_Debug,
         Reject_Explicit_Integer,
         Reject_Concatenation,
         Reject_To_Buffer,
         Reject_Mid,
         Reject_To_String,
         Reject_Format_String,
         Reject_Match,
         Reject_Resources,
         Wait_For_Delay => Unavailable_Delay,
         Queue_Notify => Reject_Notify,
         Describe_Metadata_Value => Reject_Metadata_Value,
         Materialize_Buffer => Reject_Buffer_Literal,
         Define_Alias => Reject_Alias,
         Enter_Container => Execute_Unsupported_Container_Enter,
         Leave_Container => Execute_Unsupported_Container_Leave,
         Require_Invocation_Authority => True,
         Open_Frame_Root => Open_Session_Frame,
         Close_Frame_Root => Close_Session_Frame,
         Enter_Serialized_Method => Enter_Session_Method,
         Leave_Serialized_Method => Leave_Session_Method,
         Runtime_Package_Token => Natural,
         No_Runtime_Package => 0,
         Begin_Runtime_Package => Package_Adapter_Begin,
         Append_Runtime_Package => Package_Adapter_Append,
         Defer_Runtime_Package_Name => Package_Adapter_Defer,
         Finish_Runtime_Package => Package_Adapter_Finish,
         Abort_Runtime_Packages => Package_Adapter_Abort);
      Source : aliased constant AML_Byte_Sources.Storage :=
        (Length => Code'Length, Bytes => Code);
   begin
      Execute (Code, Width, Args, Argument_Count, Budget, Empty, Environment,
               Scope, Result_Out, Source, 0, Calls_Left, Current_Sync);
   end Execute_Session_Typed;

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
