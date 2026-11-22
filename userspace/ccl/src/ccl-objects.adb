with CCL.Bounded_Stacks;
with CCL.Types.Correspondence;

package body CCL.Objects with SPARK_Mode is
   use CCL.Types;

   --  A page's padding is zero (a read-only comparison target).
   Zero_Padding : constant Padding_Bytes := [others => 0];

   function Same_Schema (Left, Right : Binding) return Boolean is
     (Left.Bound and then Right.Bound and then Left.Key = Right.Key and then
      CCL.Types.Correspondence.Resolve (Left.Types, Left.Root, Right.Types) = Right.Root);

   function Matches_Type
     (Contract : Binding; Local_Types : Registry; Local_Root : Type_Reference) return Boolean is
     (Contract.Bound and then Local_Root /= Invalid_Type and then
      CCL.Types.Correspondence.Resolve (Contract.Types, Contract.Root, Local_Types) = Local_Root);

   function Persistable (Types : Registry; Root : Type_Reference) return Boolean is
     (CCL.Types.Persistable (Types, Root));

   --  Whether Payload is the list of Owner itself.
   function Self_List (Types : Registry; Owner, Payload : Type_Reference) return Boolean is
     (Known (Types, Payload)
      and then Describe (Types, Payload).Form = Sequence
      and then Describe (Types, Payload).Count = 1
      and then Describe (Types, Payload).Parts (1).Payload = Owner);

   function Storable (Types : Registry; Root : Type_Reference) return Boolean is
      Allowed : array (Type_Reference) of Boolean := [others => False];
      D : Description;
   begin
      if not Known (Types, Root) then return False; end if;
      Allowed (Integer_Type) := True;
      Allowed (Boolean_Type) := True;
      Allowed (String_Type) := True;
      Allowed (Character_Type) := True;
      Allowed (Unit_Type) := True;
      --  Backward references, except that a record or variant may hold a
      --  list of itself (CCL.Types.Complete_Self_List). A list's element
      --  may be any storable type except another list.
      for Ref in Declared_Type'First .. Last (Types) loop
         D := Describe (Types, Ref);
         if D.Form = Bounded then
            --  A range subtype of Integer.
            Allowed (Ref) := True;
         elsif D.Form in Stream | Async then
            --  A stream or task handle, held as a binding; its elements (or
            --  its result) are data.
            Allowed (Ref) := D.Count = 1 and then D.Parts (1).Payload < Ref
              and then Persistable (Types, D.Parts (1).Payload);
         elsif D.Form = Sequence then
            Allowed (Ref) := D.Count = 1 and then D.Parts (1).Payload < Ref
              and then Allowed (D.Parts (1).Payload)
              and then Describe (Types, D.Parts (1).Payload).Form not in Sequence | Stream | Async;
         else
            Allowed (Ref) := D.Form in Product | Sum;
            for I in 1 .. D.Count loop
               if D.Parts (I).Payload >= Ref then
                  if not Self_List (Types, Ref, D.Parts (I).Payload) then
                     Allowed (Ref) := False;
                  end if;
               elsif not Allowed (D.Parts (I).Payload) or else
                 Describe (Types, D.Parts (I).Payload).Form in Stream | Async
               then
                  --  A stream or task is a session's handle, never a field or payload.
                  Allowed (Ref) := False;
               end if;
            end loop;
         end if;
      end loop;
      return Allowed (Root);
   end Storable;

   procedure Bind
     (Types : Registry; Root : Type_Reference; Key : Schema_Key;
      Contract : out Binding; Accepted : out Boolean) is
   begin
      Contract := (others => <>);
      Accepted := Key /= No_Schema and then Persistable (Types, Root);
      if Accepted then Contract := (Types, Root, Key, True); end if;
   end Bind;

   function Empty (Contract : Binding) return Image is
     ((Schema => Contract.Key, others => <>));

   function Empty
     (Contract : Binding; Cell_Room : Cell_Room_Count; Text_Room : Text_Room_Count) return Sized_Image is
     ((Cell_Room => Cell_Room, Text_Room => Text_Room, Schema => Contract.Key, others => <>));

   function To_Sized (Object : Image) return Sized_Image is
      Cells : constant Cell_Room_Count := Cells_Held (Object);
      Bytes : constant Text_Room_Count := Bytes_Held (Object);
   begin
      return (Cell_Room => Cells, Text_Room => Bytes, Schema => Object.Schema, Version => Object.Version,
              Used_Cells => Object.Used_Cells, Used_Bytes => Object.Used_Bytes, Reserved => Object.Reserved,
              Cells => Object.Cells (1 .. Cells), Text => Object.Text (1 .. Bytes));
   end To_Sized;

   function To_Page (Object : Sized_Image) return Image is
      Cells : constant Natural := Natural (Object.Used_Cells);
      Bytes : constant Natural := Natural (Object.Used_Bytes);
      Page : Image := (Schema => Object.Schema, Version => Object.Version, Used_Cells => Object.Used_Cells,
                       Used_Bytes => Object.Used_Bytes, Reserved => Object.Reserved, others => <>);
   begin
      Page.Cells (1 .. Cells) := Object.Cells (1 .. Cells);
      Page.Text (1 .. Bytes) := Object.Text (1 .. Bytes);
      return Page;
   end To_Page;

   procedure Append (Object : in out Sized_Image; Value : Cell; Result : out Build_Result) is
   begin
      if Object.Version /= Format_Version or else Object.Reserved /= 0 or else
        Object.Used_Cells > Unsigned_32 (Object.Cell_Room) or else
        Object.Used_Bytes > Unsigned_32 (Object.Text_Room)
      then Result := Invalid_Image;
      elsif Natural (Object.Used_Cells) >= Object.Cell_Room then Result := Full;
      else
         declare
            Next : constant Positive := Natural (Object.Used_Cells) + 1;
         begin
            Object.Cells (Next) := Value;
            Object.Used_Cells := Unsigned_32 (Next);
         end;
         Result := Added;
      end if;
   end Append;

   procedure Append_Text
     (Object : in out Sized_Image; Value : String; Result : out Build_Result) is
      Offset : Natural;
   begin
      if Object.Version /= Format_Version or else Object.Reserved /= 0 or else
        Object.Used_Cells > Unsigned_32 (Object.Cell_Room) or else
        Object.Used_Bytes > Unsigned_32 (Object.Text_Room)
      then Result := Invalid_Image; return; end if;
      Offset := Natural (Object.Used_Bytes);
      if Natural (Object.Used_Cells) >= Object.Cell_Room or else
        Offset > Object.Text_Room or else Value'Length > Object.Text_Room - Offset
      then Result := Full; return; end if;
      declare
         Next : constant Positive := Natural (Object.Used_Cells) + 1;
      begin
         Object.Cells (Next) := (Unsigned_64 (Offset), Unsigned_64 (Value'Length));
         Object.Used_Cells := Unsigned_32 (Next);
      end;
      Object.Text (Offset + 1 .. Offset + Value'Length) := Value;
      Object.Used_Bytes := Object.Used_Bytes + Unsigned_32 (Value'Length);
      Result := Added;
   end Append_Text;

   procedure Append (Object : in out Image; Value : Cell; Result : out Build_Result) is
   begin
      if Object.Version /= Format_Version or else Object.Reserved /= 0 or else
        Object.Used_Cells > Maximum_Cells or else Object.Used_Bytes > Maximum_Text_Bytes
      then Result := Invalid_Image;
      elsif Object.Used_Cells = Maximum_Cells then Result := Full;
      else
         Object.Used_Cells := Object.Used_Cells + 1;
         Object.Cells (Cell_Index (Object.Used_Cells)) := Value;
         Result := Added;
      end if;
   end Append;

   procedure Append_Text
     (Object : in out Image; Value : String; Result : out Build_Result) is
      Offset : Natural;
   begin
      if Object.Version /= Format_Version or else Object.Reserved /= 0 or else
        Object.Used_Cells > Maximum_Cells or else Object.Used_Bytes > Maximum_Text_Bytes
      then Result := Invalid_Image; return; end if;
      Offset := Natural (Object.Used_Bytes);
      if Object.Used_Cells = Maximum_Cells or else
        Value'Length > Maximum_Text_Bytes - Offset
      then Result := Full; return; end if;
      Object.Used_Cells := Object.Used_Cells + 1;
      Object.Cells (Cell_Index (Object.Used_Cells)) :=
        (Unsigned_64 (Offset), Unsigned_64 (Value'Length));
      Object.Text (Offset + 1 .. Offset + Value'Length) := Value;
      Object.Used_Bytes := Object.Used_Bytes + Unsigned_32 (Value'Length);
      Result := Added;
   end Append_Text;

   function Integer_Cell (Value : Integer_64) return Cell is
   begin
      --  Avoid unchecked conversion and negating Integer_64'First.
      if Value >= 0 then return (Unsigned_64 (Value), 0);
      else return (Unsigned_64'Last - Unsigned_64 (-(Value + 1)), 0);
      end if;
   end Integer_Cell;

   function Integer_Of (Value : Cell) return Integer_64 is
   begin
      if Value.First <= Unsigned_64 (Integer_64'Last) then
         return Integer_64 (Value.First);
      else return -1 - Integer_64 (Unsigned_64'Last - Value.First);
      end if;
   end Integer_Of;

   --  The work stack holds what is still expected: a type, and how many
   --  values of it in a row (a list's elements are one entry). A product
   --  replaces itself by its fields, a value of a persistable type holds
   --  only earlier types, so the stack is never deeper than a type's
   --  nesting times its fields: never anywhere near the cells.
   type Expected_Run is record
      Kind : Type_Reference := Invalid_Type;
      Count : Unsigned_64 := 0;
   end record;
   WORK_DEPTH : constant := 2 ** 12;
   type Work_Index is mod WORK_DEPTH;
   package Work_Stacks is new CCL.Bounded_Stacks (Work_Index, Expected_Run, (others => <>));
   use type Work_Stacks.Operation_Result;

   --  Validation of any room: a header, Cells (its first Cell_Room) and
   --  Text (its first Text_Room bytes).
   function Valid_Layout
     (Schema : Schema_Key; Version, Used_Cells, Used_Bytes, Reserved : Unsigned_32;
      Cells : Cell_Sequence; Text : String; Contract : Binding) return Boolean
     with Pre => Cells'First = 1 and then Text'First = 1 and then
                 Cells'Last <= VALUE_CELL_CEILING and then Text'Last <= VALUE_TEXT_CEILING;

   function Valid_Layout
     (Schema : Schema_Key; Version, Used_Cells, Used_Bytes, Reserved : Unsigned_32;
      Cells : Cell_Sequence; Text : String; Contract : Binding) return Boolean
   is
      Cell_Room : constant Natural := Cells'Length;
      Text_Room : constant Natural := Text'Length;
      Work : Work_Stacks.Stack;
      Status : Work_Stacks.Operation_Result;
      Next : Expected_Run;
      Expected : Type_Reference;
      D : Description;
      Text_End : Natural range 0 .. VALUE_TEXT_CEILING := 0;
      Used : Natural;
   begin
      if not Contract.Bound or else Schema /= Contract.Key or else
        Version /= Format_Version or else Reserved /= 0 or else
        Used_Cells < 1 or else Used_Cells > Unsigned_32 (VALUE_CELL_CEILING) or else
        Used_Bytes > Unsigned_32 (VALUE_TEXT_CEILING)
      then return False; end if;
      Used := Natural (Used_Cells);
      if Used > Cell_Room or else Natural (Used_Bytes) > Text_Room then return False; end if;
      Work_Stacks.Initialize (Work);
      Work_Stacks.Push (Work, (Contract.Root, 1), Status);
      if Status /= Work_Stacks.Stack_Ok then return False; end if;
      for I in 1 .. Used loop
         Work_Stacks.Pop (Work, Next, Status);
         if Status /= Work_Stacks.Stack_Ok or else Next.Count = 0 then return False; end if;
         Expected := Next.Kind;
         if Next.Count > 1 then
            Work_Stacks.Push (Work, (Next.Kind, Next.Count - 1), Status);
            if Status /= Work_Stacks.Stack_Ok then return False; end if;
         end if;
         declare
            C : constant Cell := Cells (I);
         begin
            if Expected = String_Type then
               if C.First /= Unsigned_64 (Text_End) or else
                 C.Second > Unsigned_64 (Text_Room)
               then return False; end if;
               declare
                  --  Narrow the wire length once, then use bounded native
                  --  arithmetic for cursor accounting (not modular addition).
                  Length : constant Natural range 0 .. VALUE_TEXT_CEILING := Natural (C.Second);
               begin
                  if Length > Text_Room - Text_End then return False; end if;
                  Text_End := Text_End + Length;
               end;
            else
               if C.Second /= 0 then return False; end if;
               case Expected is
                  when Integer_Type => null;
                  when Boolean_Type => if C.First > 1 then return False; end if;
                  when Character_Type => if C.First > 255 then return False; end if;
                  when Unit_Type => if C.First /= 0 then return False; end if;
                  when Declared_Type =>
                     D := Describe (Contract.Types, Expected);
                     case D.Form is
                        when Bounded =>
                           --  A range of Integer: within its bounds.
                           if Integer_Of (C) not in Low_Of (Contract.Types, Expected) ..
                                                    High_Of (Contract.Types, Expected)
                           then return False; end if;
                        when Primitive | Resource | Callable | Stream | Async => return False;
                        when Sequence =>
                           --  Each element takes at least one cell, so a count
                           --  past the cells left is malformed.
                           if D.Count /= 1 or else C.First > Unsigned_64 (Used - I) then
                              return False;
                           end if;
                           if C.First > 0 then
                              Work_Stacks.Push (Work, (D.Parts (1).Payload, C.First), Status);
                              if Status /= Work_Stacks.Stack_Ok then return False; end if;
                           end if;
                        when Product =>
                           if C.First /= Unsigned_64 (D.Count) then return False; end if;
                           for P in reverse 1 .. D.Count loop
                              Work_Stacks.Push (Work, (D.Parts (P).Payload, 1), Status);
                              if Status /= Work_Stacks.Stack_Ok then return False; end if;
                           end loop;
                        when Sum =>
                           if C.First not in 1 .. Unsigned_64 (D.Count) then return False; end if;
                           Work_Stacks.Push
                             (Work, (D.Parts (Component_Index (C.First)).Payload, 1), Status);
                           if Status /= Work_Stacks.Stack_Ok then return False; end if;
                     end case;
                  when others => return False;
               end case;
            end if;
         end;
      end loop;
      Work_Stacks.Pop (Work, Next, Status);
      if Status /= Work_Stacks.Stack_Empty or else Unsigned_32 (Text_End) /= Used_Bytes then
         return False;
      end if;
      --  Canonical unused storage prevents an accepted object from carrying
      --  unrelated bytes to another authorized reader.
      for I in Used + 1 .. Cell_Room loop
         if Cells (I) /= Unit_Cell then return False; end if;
      end loop;
      for I in Text_End + 1 .. Text_Room loop
         if Text (I) /= Character'Val (0) then return False; end if;
      end loop;
      return True;
   end Valid_Layout;

   function Validate (Object : Image; Contract : Binding) return Boolean is
     (Valid_Layout (Object.Schema, Object.Version, Object.Used_Cells, Object.Used_Bytes, Object.Reserved,
                    Object.Cells, Object.Text, Contract) and then
      Object.Padding = Zero_Padding);

   function Validate (Object : Sized_Image; Contract : Binding) return Boolean is
     (Valid_Layout (Object.Schema, Object.Version, Object.Used_Cells, Object.Used_Bytes, Object.Reserved,
                    Object.Cells, Object.Text, Contract));
end CCL.Objects;
