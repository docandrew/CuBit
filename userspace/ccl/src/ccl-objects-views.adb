with CCL.Bounded_Stacks;
with CCL.Types.Correspondence;

package body CCL.Objects.Views with SPARK_Mode is
   use CCL.Types;
   type Visit_Kind is (Enter_Value, Leave_Value);
   --  Count values in a row of type Kind to enter (a list's elements are
   --  one visit), or the value at Position to leave.
   type Visit is record
      Action : Visit_Kind := Enter_Value;
      Kind : Type_Reference := Invalid_Type;
      Position : Positive range 1 .. VALUE_CELL_CEILING := 1;
      Count : Unsigned_64 := 1;
   end record;
   --  As in Validate: the work never gets deeper than a type's nesting
   --  times its fields, whatever the cells.
   WORK_DEPTH : constant := 2 ** 13;
   type Work_Index is mod WORK_DEPTH;
   package Work_Stacks is new CCL.Bounded_Stacks (Work_Index, Visit, (others => <>));
   use type Work_Stacks.Operation_Result;

   procedure Clear (Object : in out Sized_Snapshot) is
   begin
      Object.Ready := False; Object.Used := 0;
      Object.Contract := (others => <>);
      --  Field by field, in place: a snapshot may be large.
      Object.Value.Schema := No_Schema;
      Object.Value.Version := Format_Version;
      Object.Value.Used_Cells := 0;
      Object.Value.Used_Bytes := 0;
      Object.Value.Reserved := 0;
      for I in Object.Value.Cells'Range loop
         Object.Value.Cells (I) := Unit_Cell;
      end loop;
      for I in Object.Value.Text'Range loop
         Object.Value.Text (I) := Character'Val (0);
      end loop;
      for I in Object.Entries'Range loop
         Object.Entries (I) := (others => <>);
      end loop;
      -- Epoch is never reset: recapturing into the same owner cannot revive
      -- a position saved before Clear or a failed Capture.
   end Clear;

   procedure Capture
     (Object : in out Sized_Snapshot; Contract : Binding; Value : Sized_Image; Accepted : out Boolean)
   is
      Work : Work_Stacks.Stack;
      Next : Visit;
      Status : Work_Stacks.Operation_Result;
      Seen : Position_Count := 0;
      Text_End : Natural range 0 .. VALUE_TEXT_CEILING := 0;
      D : Types.Description;
   begin
      Clear (Object); Accepted := False;
      if Object.Epoch = Unsigned_64'Last then return; end if;
      Object.Epoch := Object.Epoch + 1;
      if Value.Used_Cells not in 1 .. Unsigned_32 (Natural'Min (Object.Cell_Room, Value.Cell_Room)) or else
        Value.Used_Bytes > Unsigned_32 (Natural'Min (Object.Text_Room, Value.Text_Room))
      then return; end if;
      declare
         Cells : constant Positive := Positive (Value.Used_Cells);
         Bytes : constant Natural := Natural (Value.Used_Bytes);
      begin
         Object.Value.Schema := Value.Schema;
         Object.Value.Version := Value.Version;
         Object.Value.Used_Cells := Value.Used_Cells;
         Object.Value.Used_Bytes := Value.Used_Bytes;
         Object.Value.Reserved := Value.Reserved;
         Object.Value.Cells (1 .. Cells) := Value.Cells (1 .. Cells);
         Object.Value.Text (1 .. Bytes) := Value.Text (1 .. Bytes);
      end;
      if not Validate (Object.Value, Contract) then return; end if;
      Object.Contract := Contract;
      Object.Used := Natural (Object.Value.Used_Cells);
      Work_Stacks.Initialize (Work);
      Work_Stacks.Push (Work, (Enter_Value, Contract.Root, 1, 1), Status);
      if Status /= Work_Stacks.Stack_Ok then return; end if;
      -- Every cell generates exactly one enter and one leave event, plus
      -- the final empty pop. No recursive walk or input-dependent unbounded loop.
      for Step in 1 .. 2 * Object.Used + 1 loop
         Work_Stacks.Pop (Work, Next, Status);
         if Status = Work_Stacks.Stack_Empty then
            Accepted := Seen = Object.Used;
            Object.Ready := Accepted;
            return;
         elsif Status /= Work_Stacks.Stack_Ok then return;
         end if;
         if Next.Action = Leave_Value then
            if Next.Position > Object.Cell_Room then return; end if;
            Object.Entries (Next.Position).Last := Seen;
         else
            if Seen >= Object.Used or else Next.Count = 0 then return; end if;
            if Next.Count > 1 then
               Work_Stacks.Push (Work, (Next.Action, Next.Kind, Next.Position, Next.Count - 1), Status);
               if Status /= Work_Stacks.Stack_Ok then return; end if;
            end if;
            Seen := Seen + 1;
            Object.Entries (Seen).Kind := Next.Kind;
            Work_Stacks.Push (Work, (Leave_Value, Next.Kind, Seen, 1), Status);
            if Status /= Work_Stacks.Stack_Ok then return; end if;
            case Next.Kind is
               when String_Type =>
                  declare
                     Raw_Length : constant Unsigned_64 := Object.Value.Cells (Seen).Second;
                  begin
                     if Raw_Length > Unsigned_64 (Object.Text_Room) then return; end if;
                     declare
                        Length : constant Natural range 0 .. VALUE_TEXT_CEILING := Natural (Raw_Length);
                     begin
                        if Length > Object.Text_Room - Text_End then return; end if;
                        Object.Entries (Seen).Text_First := Text_End + 1;
                        Text_End := Text_End + Length;
                        Object.Entries (Seen).Text_Last := Text_End;
                     end;
                  end;
               when Integer_Type | Boolean_Type | Character_Type | Unit_Type => null;
               when Declared_Type =>
                  D := Types.Describe (Contract.Types, Next.Kind);
                  case D.Form is
                     --  A range is one Integer cell (Validate checked its bounds).
                     when Bounded => null;
                     when Primitive | Resource | Callable | Stream | Async => return;
                     when Sequence =>
                        --  Validate bounded the count by the cells left.
                        if D.Count /= 1 or else Object.Value.Cells (Seen).First > Unsigned_64 (Object.Used) then
                           return;
                        end if;
                        if Object.Value.Cells (Seen).First > 0 then
                           Work_Stacks.Push
                             (Work, (Enter_Value, D.Parts (1).Payload, 1, Object.Value.Cells (Seen).First), Status);
                           if Status /= Work_Stacks.Stack_Ok then return; end if;
                        end if;
                     when Product =>
                        for P in reverse 1 .. D.Count loop
                           Work_Stacks.Push (Work, (Enter_Value, D.Parts (P).Payload, 1, 1), Status);
                           if Status /= Work_Stacks.Stack_Ok then return; end if;
                        end loop;
                     when Sum =>
                        for P in 1 .. D.Count loop
                           if Object.Value.Cells (Seen).First = Unsigned_64 (P) then
                              Object.Entries (Seen).Choice := P;
                              Work_Stacks.Push (Work, (Enter_Value, D.Parts (P).Payload, 1, 1), Status);
                              if Status /= Work_Stacks.Stack_Ok then return; end if;
                              exit;
                           end if;
                        end loop;
                        if Object.Entries (Seen).Choice = 0 then return; end if;
                  end case;
               when others => return;
            end case;
         end if;
      end loop;
   end Capture;

   procedure Capture
     (Object : in out Sized_Snapshot; Contract : Binding; Value : Image; Accepted : out Boolean) is
   begin
      --  The page's padding is part of what it validates.
      if Value.Padding /= [1 .. Padding_Length => 0] then
         Clear (Object); Accepted := False;
         if Object.Epoch < Unsigned_64'Last then Object.Epoch := Object.Epoch + 1; end if;
         return;
      end if;
      Capture (Object, Contract, To_Sized (Value), Accepted);
   end Capture;

   procedure Capture
     (Object : in out Sized_Snapshot; Contract : Binding; Value : Holders.Holder; Accepted : out Boolean)
   is
      procedure Take (Held : Sized_Image);
      procedure Take (Held : Sized_Image) is
      begin
         Capture (Object, Contract, Held, Accepted);
      end Take;
      procedure Take_Held is new Holders.Inspect (Take);
   begin
      Accepted := False;
      Take_Held (Value);
   end Capture;

   procedure Capture_Local
     (Object : in out Sized_Snapshot; Local_Types : Types.Registry;
      Kind : Types.Type_Reference; Value : Image; Accepted : out Boolean)
   is
      Local_Contract : constant Binding :=
        (Local_Types, Kind, No_Schema, Persistable (Local_Types, Kind));
   begin
      -- This private binding never leaves the snapshot. Public Bind still
      -- rejects No_Schema, as do all approved IPC schema admission paths.
      Capture (Object, Local_Contract, Value, Accepted);
   end Capture_Local;

   function Is_Valid (Object : Sized_Snapshot; Position : Cursor) return Boolean is
     (Object.Ready and then Position.Epoch = Object.Epoch and then
      Position.Position in 1 .. Object.Used and then
      Object.Entries (Position.Position).Kind /= Invalid_Type and then
      Object.Entries (Position.Position).Last >= Position.Position);
   function Root (Object : Sized_Snapshot) return Cursor is
     (if Object.Ready then (Object.Epoch, 1) else No_Value);
   function Cells_Used (Object : Sized_Snapshot) return Cell_Room_Count is
     (if Object.Ready then Object.Used else 0);
   function At_Cell (Object : Sized_Snapshot; Position : Positive) return Cursor is
     (if Object.Ready and then Position <= Object.Used then (Object.Epoch, Position) else No_Value);
   function Type_Of (Object : Sized_Snapshot; Position : Cursor) return Type_Reference is
     (if Is_Valid (Object, Position) then Object.Entries (Position.Position).Kind else Invalid_Type);
   function Local_Type
     (Object : Sized_Snapshot; Position : Cursor; Local_Types : Types.Registry) return Type_Reference is
     (if Is_Valid (Object, Position) then CCL.Types.Correspondence.Resolve
        (Object.Contract.Types, Type_Of (Object, Position), Local_Types)
      else Invalid_Type);
   function Describe (Object : Sized_Snapshot; Position : Cursor) return Types.Description is
     (Types.Describe (Object.Contract.Types, Type_Of (Object, Position)));
   function Field
     (Object : Sized_Snapshot; Position : Cursor; Index : Component_Index) return Cursor
   is
      D : constant Types.Description := Describe (Object, Position);
      Previous : Position_Count := Position.Position;
   begin
      if not Is_Valid (Object, Position) or else D.Form /= Product or else Index > D.Count then
         return No_Value;
      end if;
      for P in 1 .. Index loop
         if Previous >= Object.Entries (Position.Position).Last then return No_Value; end if;
         if P = Index then return (Object.Epoch, Previous + 1); end if;
         Previous := Object.Entries (Previous + 1).Last;
      end loop;
      return No_Value;
   end Field;
   function Length (Object : Sized_Snapshot; Position : Cursor) return Element_Count is
     (if Is_Valid (Object, Position) and then Describe (Object, Position).Form = Sequence and then
        Object.Value.Cells (Position.Position).First <= Unsigned_64 (VALUE_CELL_CEILING)
      then Element_Count (Object.Value.Cells (Position.Position).First) else 0);
   function First_Element (Object : Sized_Snapshot; Position : Cursor) return Cursor is
     (if Length (Object, Position) > 0 and then Position.Position < Object.Entries (Position.Position).Last
      then (Object.Epoch, Position.Position + 1) else No_Value);
   function Next_Element (Object : Sized_Snapshot; Position, Item : Cursor) return Cursor is
     (if Is_Valid (Object, Position) and then Describe (Object, Position).Form = Sequence and then
        Is_Valid (Object, Item) and then Item.Position > Position.Position and then
        Object.Entries (Item.Position).Last < Object.Entries (Position.Position).Last
      then (Object.Epoch, Object.Entries (Item.Position).Last + 1) else No_Value);
   function Element
     (Object : Sized_Snapshot; Position : Cursor; Index : Positive) return Cursor
   is
      Previous : Position_Count := Position.Position;
   begin
      if not Is_Valid (Object, Position) or else Index > Length (Object, Position) then
         return No_Value;
      end if;
      --  Like Field: step over the earlier elements' subtrees.
      for E in 1 .. Index loop
         if Previous >= Object.Entries (Position.Position).Last then return No_Value; end if;
         if E = Index then return (Object.Epoch, Previous + 1); end if;
         Previous := Object.Entries (Previous + 1).Last;
      end loop;
      return No_Value;
   end Element;
   function Payload (Object : Sized_Snapshot; Position : Cursor) return Cursor is
     (if Is_Valid (Object, Position) and then Describe (Object, Position).Form = Sum and then
        Position.Position < Object.Entries (Position.Position).Last
      then (Object.Epoch, Position.Position + 1) else No_Value);
   function Alternative (Object : Sized_Snapshot; Position : Cursor) return Component_Count is
     (if Is_Valid (Object, Position) then Object.Entries (Position.Position).Choice else 0);
   function Scalar (Object : Sized_Snapshot; Position : Cursor) return Cell is
     (if Is_Valid (Object, Position) and then
        (Type_Of (Object, Position) in Integer_Type | Boolean_Type | Character_Type | Unit_Type or else
         Types.Is_Range (Object.Contract.Types, Type_Of (Object, Position)))
      then Object.Value.Cells (Position.Position) else Unit_Cell);
   procedure Append_Value
     (Object : Sized_Snapshot; Position : Cursor; Value : in out Image;
      Result : out Build_Result)
   is
   begin
      Result := Invalid_Image;
      if not Is_Valid (Object, Position) then return; end if;
      for I in Position.Position .. Object.Entries (Position.Position).Last loop
         if Object.Entries (I).Kind = String_Type then
            Append_Text (Value,
              Object.Value.Text (Object.Entries (I).Text_First .. Object.Entries (I).Text_Last), Result);
         else
            Append (Value, Object.Value.Cells (I), Result);
         end if;
         if Result /= Added then return; end if;
      end loop;
   end Append_Value;

   procedure Append_Value
     (Object : Sized_Snapshot; Position : Cursor; Value : in out Sized_Image;
      Result : out Build_Result)
   is
   begin
      Result := Invalid_Image;
      if not Is_Valid (Object, Position) then return; end if;
      for I in Position.Position .. Object.Entries (Position.Position).Last loop
         if Object.Entries (I).Kind = String_Type then
            Append_Text (Value,
              Object.Value.Text (Object.Entries (I).Text_First .. Object.Entries (I).Text_Last), Result);
         else
            Append (Value, Object.Value.Cells (I), Result);
         end if;
         if Result /= Added then return; end if;
      end loop;
   end Append_Value;

   procedure Copy_Value
     (Object : Sized_Snapshot; Position : Cursor; Target : Binding;
      Value : out Image; Accepted : out Boolean)
   is
      Built : Build_Result;
   begin
      Value := Empty (Target);
      Accepted := False;
      if not Is_Valid (Object, Position) or else not Is_Bound (Target) or else
        Local_Type (Object, Position, Target.Types) /= Target.Root
      then return; end if;
      Append_Value (Object, Position, Value, Built);
      Accepted := Built = Added and then Validate (Value, Target);
      if not Accepted then Value := Empty (Target); end if;
   end Copy_Value;

   function Text (Object : Sized_Snapshot; Position : Cursor) return String is
     (if Is_Valid (Object, Position) and then Type_Of (Object, Position) = String_Type then
        Object.Value.Text (Object.Entries (Position.Position).Text_First .. Object.Entries (Position.Position).Text_Last)
      else "");

   function Text_Length (Object : Sized_Snapshot; Position : Cursor) return Text_Size is
     (if Is_Valid (Object, Position) and then Type_Of (Object, Position) = String_Type and then
         Object.Entries (Position.Position).Text_Last >= Object.Entries (Position.Position).Text_First
      then Object.Entries (Position.Position).Text_Last - Object.Entries (Position.Position).Text_First + 1
      else 0);

   procedure Copy_Text
     (Object : Sized_Snapshot; Position : Cursor; Target : out String; Accepted : out Boolean) is
   begin
      Accepted := Is_Valid (Object, Position) and then Type_Of (Object, Position) = String_Type and then
        Target'Length = Text_Length (Object, Position);
      if Accepted then Target := Text (Object, Position);
      else Target := [others => Character'Val (0)];
      end if;
   end Copy_Text;

   procedure Read_Text
     (Object : Sized_Snapshot; Position : Cursor; Index : Positive;
      Value : out Character; Accepted : out Boolean) is
   begin
      Accepted := Index <= Text_Length (Object, Position);
      Value := Character'Val (0);
      if Accepted then
         Value := Object.Value.Text (Object.Entries (Position.Position).Text_First + (Index - 1));
      end if;
   end Read_Text;
end CCL.Objects.Views;
