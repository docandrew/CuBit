with CCL.Bounded_Stacks;
with CCL.Types.Correspondence;

package body CCL.Objects.Views with SPARK_Mode is
   use CCL.Types;
   type Visit_Kind is (Enter_Value, Leave_Value);
   type Visit is record
      Action : Visit_Kind := Enter_Value;
      Kind : Type_Reference := Invalid_Type;
      Position : Cell_Index := 1;
   end record;
   type Work_Index is mod Maximum_Cells * 2;
   package Work_Stacks is new CCL.Bounded_Stacks (Work_Index, Visit, (others => <>));
   use type Work_Stacks.Operation_Result;

   procedure Clear (Object : in out Snapshot) is
   begin
      Object.Ready := False; Object.Used := 0;
      Object.Contract := (others => <>); Object.Value := (others => <>);
      Object.Entries := [others => (others => <>)];
      -- Epoch is never reset: recapturing into the same owner cannot revive
      -- a position saved before Clear or a failed Capture.
   end Clear;

   procedure Capture
     (Object : in out Snapshot; Contract : Binding; Value : Image; Accepted : out Boolean)
   is
      Work : Work_Stacks.Stack;
      Next : Visit;
      Status : Work_Stacks.Operation_Result;
      Seen : Position_Count := 0;
      Text_End : Natural range 0 .. Maximum_Text_Bytes := 0;
      D : Types.Description;
   begin
      Clear (Object); Accepted := False;
      if Object.Epoch = Unsigned_64'Last then return; end if;
      Object.Epoch := Object.Epoch + 1;
      Object.Value := Value;
      if Object.Value.Used_Cells not in 1 .. Maximum_Cells or else
        not Validate (Object.Value, Contract)
      then return; end if;
      Object.Contract := Contract;
      Object.Used := Natural (Object.Value.Used_Cells);
      Work_Stacks.Push (Work, (Enter_Value, Contract.Root, 1), Status);
      if Status /= Work_Stacks.Stack_Ok then return; end if;
      -- Every cell generates exactly one enter and one leave event, plus
      -- the final empty pop. No recursive walk or input-dependent unbounded loop.
      for Step in 1 .. 2 * Maximum_Cells + 1 loop
         Work_Stacks.Pop (Work, Next, Status);
         if Status = Work_Stacks.Stack_Empty then
            Accepted := Seen = Object.Used;
            Object.Ready := Accepted;
            return;
         elsif Status /= Work_Stacks.Stack_Ok then return;
         end if;
         if Next.Action = Leave_Value then
            Object.Entries (Next.Position).Last := Seen;
         else
            if Seen >= Object.Used then return; end if;
            Seen := Seen + 1;
            Object.Entries (Seen).Kind := Next.Kind;
            Work_Stacks.Push (Work, (Leave_Value, Next.Kind, Seen), Status);
            if Status /= Work_Stacks.Stack_Ok then return; end if;
            case Next.Kind is
               when String_Type =>
                  declare
                     Raw_Length : constant Unsigned_64 := Object.Value.Cells (Seen).Second;
                  begin
                     if Raw_Length > Maximum_Text_Bytes then return; end if;
                     declare
                        Length : constant Natural range 0 .. Maximum_Text_Bytes := Natural (Raw_Length);
                     begin
                        if Length > Maximum_Text_Bytes - Text_End then return; end if;
                        Object.Entries (Seen).Text_First := Text_End + 1;
                        Text_End := Text_End + Length;
                        Object.Entries (Seen).Text_Last := Text_End;
                     end;
                  end;
               when Integer_Type | Boolean_Type | Character_Type | Unit_Type => null;
               when Declared_Type =>
                  D := Types.Describe (Contract.Types, Next.Kind);
                  case D.Form is
                     when Primitive | Resource => return;
                     when Product =>
                        for P in reverse 1 .. D.Count loop
                           Work_Stacks.Push (Work, (Enter_Value, D.Parts (P).Payload, 1), Status);
                           if Status /= Work_Stacks.Stack_Ok then return; end if;
                        end loop;
                     when Sum =>
                        for P in 1 .. D.Count loop
                           if Object.Value.Cells (Seen).First = Unsigned_64 (P) then
                              Object.Entries (Seen).Choice := P;
                              Work_Stacks.Push (Work, (Enter_Value, D.Parts (P).Payload, 1), Status);
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

   procedure Capture_Local
     (Object : in out Snapshot; Local_Types : Types.Registry;
      Kind : Types.Type_Reference; Value : Image; Accepted : out Boolean)
   is
      Local_Contract : constant Binding :=
        (Local_Types, Kind, No_Schema, Persistable (Local_Types, Kind));
   begin
      -- This private binding never leaves the snapshot. Public Bind still
      -- rejects No_Schema, as do all approved IPC schema admission paths.
      Capture (Object, Local_Contract, Value, Accepted);
   end Capture_Local;

   function Is_Valid (Object : Snapshot; Position : Cursor) return Boolean is
     (Object.Ready and then Position.Epoch = Object.Epoch and then
      Position.Position in 1 .. Object.Used and then
      Object.Entries (Position.Position).Kind /= Invalid_Type and then
      Object.Entries (Position.Position).Last >= Position.Position);
   function Root (Object : Snapshot) return Cursor is
     (if Object.Ready then (Object.Epoch, 1) else No_Value);
   function Type_Of (Object : Snapshot; Position : Cursor) return Type_Reference is
     (if Is_Valid (Object, Position) then Object.Entries (Position.Position).Kind else Invalid_Type);
   function Local_Type
     (Object : Snapshot; Position : Cursor; Local_Types : Types.Registry) return Type_Reference is
     (if Is_Valid (Object, Position) then CCL.Types.Correspondence.Resolve
        (Object.Contract.Types, Type_Of (Object, Position), Local_Types)
      else Invalid_Type);
   function Describe (Object : Snapshot; Position : Cursor) return Types.Description is
     (Types.Describe (Object.Contract.Types, Type_Of (Object, Position)));
   function Field
     (Object : Snapshot; Position : Cursor; Index : Component_Index) return Cursor
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
   function Payload (Object : Snapshot; Position : Cursor) return Cursor is
     (if Is_Valid (Object, Position) and then Describe (Object, Position).Form = Sum and then
        Position.Position < Object.Entries (Position.Position).Last
      then (Object.Epoch, Position.Position + 1) else No_Value);
   function Alternative (Object : Snapshot; Position : Cursor) return Component_Count is
     (if Is_Valid (Object, Position) then Object.Entries (Position.Position).Choice else 0);
   function Scalar (Object : Snapshot; Position : Cursor) return Cell is
     (if Is_Valid (Object, Position) and then
        Type_Of (Object, Position) in Integer_Type | Boolean_Type | Character_Type | Unit_Type
      then Object.Value.Cells (Position.Position) else Unit_Cell);
   procedure Append_Value
     (Object : Snapshot; Position : Cursor; Value : in out Image;
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
     (Object : Snapshot; Position : Cursor; Target : Binding;
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

   function Text (Object : Snapshot; Position : Cursor) return String is
     (if Is_Valid (Object, Position) and then Type_Of (Object, Position) = String_Type then
        Object.Value.Text (Object.Entries (Position.Position).Text_First .. Object.Entries (Position.Position).Text_Last)
      else "");

   function Text_Length (Object : Snapshot; Position : Cursor) return Text_Size is
     (if Is_Valid (Object, Position) and then Type_Of (Object, Position) = String_Type and then
         Object.Entries (Position.Position).Text_Last >= Object.Entries (Position.Position).Text_First
      then Object.Entries (Position.Position).Text_Last - Object.Entries (Position.Position).Text_First + 1
      else 0);

   procedure Copy_Text
     (Object : Snapshot; Position : Cursor; Target : out String; Accepted : out Boolean) is
   begin
      Accepted := Is_Valid (Object, Position) and then Type_Of (Object, Position) = String_Type and then
        Target'Length = Text_Length (Object, Position);
      if Accepted then Target := Text (Object, Position);
      else Target := [others => Character'Val (0)];
      end if;
   end Copy_Text;

   procedure Read_Text
     (Object : Snapshot; Position : Cursor; Index : Positive;
      Value : out Character; Accepted : out Boolean) is
   begin
      Accepted := Index <= Text_Length (Object, Position);
      Value := Character'Val (0);
      if Accepted then
         Value := Object.Value.Text (Object.Entries (Position.Position).Text_First + (Index - 1));
      end if;
   end Read_Text;
end CCL.Objects.Views;
