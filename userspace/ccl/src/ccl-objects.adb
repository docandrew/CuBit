with CCL.Bounded_Stacks;
with CCL.Types.Correspondence;

package body CCL.Objects with SPARK_Mode is
   use CCL.Types;

   --  Read-only comparison targets, not per-validation temporary buffers.
   --  Array equality lets the target runtime compare blocks efficiently while
   --  retaining Ada/SPARK semantics (including an empty tail at full capacity).
   Zero_Text : constant String (1 .. Maximum_Text_Bytes) := [others => Character'Val (0)];
   Zero_Padding : constant Padding_Bytes := [others => 0];
   subtype Text_Buffer is String (1 .. Maximum_Text_Bytes);
   subtype Text_Length is Natural range 0 .. Maximum_Text_Bytes;

   function Canonical_Bytes
     (Text : Text_Buffer; Used : Text_Length; Padding : Padding_Bytes) return Boolean
     with Post => Canonical_Bytes'Result =
       ((for all I in Used + 1 .. Maximum_Text_Bytes => Text (I) = Character'Val (0)) and
        (for all B of Padding => B = 0));

   function Canonical_Bytes
     (Text : Text_Buffer; Used : Text_Length; Padding : Padding_Bytes) return Boolean is
     (Text (Used + 1 .. Maximum_Text_Bytes) = Zero_Text (Used + 1 .. Maximum_Text_Bytes) and then
      Padding = Zero_Padding);

   function Same_Schema (Left, Right : Binding) return Boolean is
     (Left.Bound and then Right.Bound and then Left.Key = Right.Key and then
      CCL.Types.Correspondence.Resolve (Left.Types, Left.Root, Right.Types) = Right.Root);

   function Matches_Type
     (Contract : Binding; Local_Types : Registry; Local_Root : Type_Reference) return Boolean is
     (Contract.Bound and then Local_Root /= Invalid_Type and then
      CCL.Types.Correspondence.Resolve (Contract.Types, Contract.Root, Local_Types) = Local_Root);

   function Persistable (Types : Registry; Root : Type_Reference) return Boolean is
      Allowed : array (Type_Reference) of Boolean := [others => False];
      D : Description;
   begin
      if not Known (Types, Root) then return False; end if;
      Allowed (Integer_Type) := True;
      Allowed (Boolean_Type) := True;
      Allowed (String_Type) := True;
      Allowed (Character_Type) := True;
      Allowed (Unit_Type) := True;
      --  Define publishes only backward references. Check every alternative,
      --  not just the active one: a dormant handler is not persistable data.
      for Ref in Declared_Type'First .. Last (Types) loop
         D := Describe (Types, Ref);
         Allowed (Ref) := D.Form in Product | Sum;
         for I in 1 .. D.Count loop
            if D.Parts (I).Payload >= Ref or else not Allowed (D.Parts (I).Payload) then
               Allowed (Ref) := False;
            end if;
         end loop;
      end loop;
      return Allowed (Root);
   end Persistable;

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

   type Work_Index is mod Maximum_Cells;
   package Work_Stacks is new CCL.Bounded_Stacks (Work_Index, Type_Reference, Invalid_Type);
   use type Work_Stacks.Operation_Result;

   function Validate (Object : Image; Contract : Binding) return Boolean is
      Work : Work_Stacks.Stack;
      Status : Work_Stacks.Operation_Result;
      Expected : Type_Reference;
      D : Description;
      Text_End : Natural range 0 .. Maximum_Text_Bytes := 0;
   begin
      if not Contract.Bound or else Object.Schema /= Contract.Key or else
        Object.Version /= Format_Version or else Object.Reserved /= 0 or else
        Object.Used_Cells not in 1 .. Maximum_Cells or else
        Object.Used_Bytes > Maximum_Text_Bytes
      then return False; end if;
      Work_Stacks.Push (Work, Contract.Root, Status);
      if Status /= Work_Stacks.Stack_Ok then return False; end if;
      for I in 1 .. Natural (Object.Used_Cells) loop
         Work_Stacks.Pop (Work, Expected, Status);
         if Status /= Work_Stacks.Stack_Ok then return False; end if;
         declare
            C : constant Cell := Object.Cells (I);
         begin
            if Expected = String_Type then
               if C.First /= Unsigned_64 (Text_End) or else
                 C.Second > Maximum_Text_Bytes
               then return False; end if;
               declare
                  --  Narrow the wire length once, then use bounded native
                  --  arithmetic for cursor accounting (not modular addition).
                  Length : constant Natural range 0 .. Maximum_Text_Bytes := Natural (C.Second);
               begin
                  if Length > Maximum_Text_Bytes - Text_End then return False; end if;
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
                        when Primitive | Resource => return False;
                        when Product =>
                           if C.First /= Unsigned_64 (D.Count) then return False; end if;
                           for P in reverse 1 .. D.Count loop
                              Work_Stacks.Push (Work, D.Parts (P).Payload, Status);
                              if Status /= Work_Stacks.Stack_Ok then return False; end if;
                           end loop;
                        when Sum =>
                           if C.First not in 1 .. Unsigned_64 (D.Count) then return False; end if;
                           Work_Stacks.Push
                             (Work, D.Parts (Component_Index (C.First)).Payload, Status);
                           if Status /= Work_Stacks.Stack_Ok then return False; end if;
                     end case;
                  when others => return False;
               end case;
            end if;
         end;
      end loop;
      Work_Stacks.Pop (Work, Expected, Status);
      if Status /= Work_Stacks.Stack_Empty or else Unsigned_32 (Text_End) /= Object.Used_Bytes then
         return False;
      end if;
      --  Canonical unused storage prevents an accepted whole-page object from
      --  carrying unrelated bytes to another authorized reader.
      for I in Natural (Object.Used_Cells) + 1 .. Maximum_Cells loop
         if Object.Cells (I) /= Unit_Cell then return False; end if;
      end loop;
      return Canonical_Bytes (Object.Text, Text_End, Object.Padding);
   end Validate;
end CCL.Objects;
