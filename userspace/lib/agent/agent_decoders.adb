pragma Ada_2022;
package body Agent_Decoders with SPARK_Mode is
   use Interfaces;
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Shape;
   use type CCL.Types.Default_Kind;
   use type CCL.Objects.Build_Result;
   use type Agent_Json.Node_Kind;
   use type Agent_Json.Parse_Result;

   subtype Document is Agent_Json.Document;
   Maximum_Key : constant := 64;
   Maximum_Depth : constant := 16;

   function From_Model (Text : String) return Untrusted_Input is
      Result : Untrusted_Input;
   begin
      Result.Size := Text'Length;
      if Text'Length > Agent_Json.Maximum_Input then
         Result.Oversized := True;
      else
         Result.Text (1 .. Text'Length) := Text;
         Result.Length := Text'Length;
      end if;
      return Result;
   end From_Model;

   function Size_Of (Input : Untrusted_Input) return Natural is (Input.Size);

   function Join (Path, Name : String) return String
   with Post => Join'Result'Length <= Maximum_Path
   is
      Result : String (1 .. Maximum_Path) := [others => ' '];
      Last : Natural range 0 .. Maximum_Path := 0;
      procedure Put (Text : String) is
      begin
         for C of Text loop
            exit when Last = Maximum_Path;
            Last := Last + 1;
            Result (Last) := C;
         end loop;
      end Put;
   begin
      Put (Path);
      if Path'Length > 0 then
         Put (".");
      end if;
      Put (Name);
      return Result (1 .. Last);
   end Join;

   procedure Set_Error (Result : in out Decode_Result; Error : Decode_Error; Path : String)
   with Pre => Error /= No_Error, Post => Result.Error /= No_Error
   is
      N : constant Natural := Natural'Min (Path'Length, Maximum_Path);
   begin
      if Result.Error = No_Error then
         Result.Error := Error;
         Result.Path := [others => ' '];
         for K in 1 .. N loop
            Result.Path (K) := Path (Path'First + (K - 1));
         end loop;
         Result.Path_Length := N;
      end if;
   end Set_Error;

   procedure Parse_Input
     (Input : Untrusted_Input; Doc : out Document; Result : in out Decode_Result)
   with Post => (if Result.Error = No_Error then Doc.Count >= 1)
   is
      Parsed : Agent_Json.Parse_Result;
   begin
      Doc := (Count => 0, Nodes => [others => (others => <>)]);
      if Input.Oversized then
         Set_Error (Result, Too_Large, "");
         return;
      end if;
      Agent_Json.Parse (Input.Text (1 .. Input.Length), Doc, Parsed);
      case Parsed is
         when Agent_Json.Parsed =>
            if Doc.Count = 0 then
               Set_Error (Result, Not_Json, "");
            end if;
         when Agent_Json.Not_Json => Set_Error (Result, Not_Json, "");
         when Agent_Json.Too_Deep => Set_Error (Result, Too_Deep, "");
         when Agent_Json.Too_Large => Set_Error (Result, Too_Large, "");
      end case;
   end Parse_Input;

   --  Whether node Index of Doc is in range and shaped (a string with its
   --  quotes inside Text).
   function Sound (Text : String; Doc : Document; Index : Natural) return Boolean is
     (Index in 1 .. Doc.Count and then
      Doc.Nodes (Index).First in Text'Range and then
      Doc.Nodes (Index).Last in Doc.Nodes (Index).First .. Text'Last and then
      Doc.Nodes (Index).Next in Index + 1 .. Doc.Count + 1 and then
      (Doc.Nodes (Index).Kind /= Agent_Json.String_Node or else Doc.Nodes (Index).Last > Doc.Nodes (Index).First));

   --  A key (a string node) as text, at most Maximum_Key bytes.
   procedure Key_Of
     (Text : String; Doc : Document; Index : Natural; Key : out String; Last : out Natural; OK : out Boolean)
   with Pre => Key'First = 1 and then Key'Length = Maximum_Key,
        Post => Last <= Key'Last
   is
   begin
      Key := [others => ' '];
      Last := 0;
      OK := Sound (Text, Doc, Index) and then Doc.Nodes (Index).Kind = Agent_Json.String_Node;
      if OK then
         Agent_Json.Decode_String (Text, Doc.Nodes (Index), Key, Last, OK);
      end if;
   end Key_Of;

   --  The member of object node Object named Name (0: none); Duplicate when
   --  it appears twice.
   procedure Member
     (Text : String; Doc : Document; Object : Natural; Name : String;
      Value : out Natural; Duplicate : out Boolean)
   is
      Child : Natural;
      Key : String (1 .. Maximum_Key);
      Last : Natural;
      OK : Boolean;
   begin
      Value := 0;
      Duplicate := False;
      if not Sound (Text, Doc, Object) then
         return;
      end if;
      Child := Object + 1;
      while Child < Doc.Nodes (Object).Next and then Child + 1 <= Doc.Count loop
         pragma Loop_Invariant (Child > Object);
         Key_Of (Text, Doc, Child, Key, Last, OK);
         exit when not OK or else not Sound (Text, Doc, Child + 1);
         if Key (1 .. Last) = Name then
            if Value /= 0 then
               Duplicate := True;
            end if;
            Value := Child + 1;
         end if;
         exit when Doc.Nodes (Child + 1).Next <= Child;
         Child := Doc.Nodes (Child + 1).Next;
      end loop;
   end Member;

   procedure Decode_Value
     (Text : String; Doc : Document; Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference;
      Index : Natural; Path : String; Depth : Natural;
      Image : in out CCL.Objects.Image; Result : in out Decode_Result)
   with Pre => Depth <= Maximum_Depth and then Path'Length <= Maximum_Path and then
               Text'Last <= Agent_Json.Maximum_Input,
        Subprogram_Variant => (Decreases => Maximum_Depth - Depth)
   is
      procedure Put (Cell : CCL.Objects.Cell) is
         Step : CCL.Objects.Build_Result;
      begin
         if Result.Error = No_Error then
            CCL.Objects.Append (Image, Cell, Step);
            if Step /= CCL.Objects.Added then
               Set_Error (Result, Too_Large, Path);
            end if;
         end if;
      end Put;

      function Kind_Is (Kind : Agent_Json.Node_Kind) return Boolean is
        (Sound (Text, Doc, Index) and then Doc.Nodes (Index).Kind = Kind);

      Shape : CCL.Types.Description;
   begin
      if Result.Error /= No_Error then
         return;
      elsif not Sound (Text, Doc, Index) then
         Set_Error (Result, Not_Json, Path);
         return;
      end if;
      if Ref = CCL.Types.Integer_Type then
         if not Kind_Is (Agent_Json.Number_Node) then
            Set_Error (Result, Wrong_Type, Path);
            return;
         end if;
         declare
            Value : Integer_64;
            OK : Boolean;
         begin
            Agent_Json.Integer_Value (Text, Doc.Nodes (Index), Value, OK);
            if not OK then
               Set_Error (Result, Out_Of_Range, Path);
            else
               Put (CCL.Objects.Integer_Cell (Value));
            end if;
         end;
         return;
      elsif Ref = CCL.Types.Boolean_Type then
         if Kind_Is (Agent_Json.True_Node) then
            Put (CCL.Objects.Boolean_Cell (True));
         elsif Kind_Is (Agent_Json.False_Node) then
            Put (CCL.Objects.Boolean_Cell (False));
         else
            Set_Error (Result, Wrong_Type, Path);
         end if;
         return;
      elsif Ref = CCL.Types.Unit_Type then
         if Kind_Is (Agent_Json.Null_Node) then
            Put (CCL.Objects.Unit_Cell);
         else
            Set_Error (Result, Wrong_Type, Path);
         end if;
         return;
      elsif Ref = CCL.Types.String_Type or else Ref = CCL.Types.Character_Type then
         if not Kind_Is (Agent_Json.String_Node) then
            Set_Error (Result, Wrong_Type, Path);
            return;
         end if;
         declare
            Buffer : String (1 .. Agent_Schemas.Typed_Text_Limit);
            Last : Natural;
            OK : Boolean;
            Step : CCL.Objects.Build_Result;
         begin
            Agent_Json.Decode_String (Text, Doc.Nodes (Index), Buffer, Last, OK);
            if not OK then
               Set_Error (Result, Too_Long, Path);
            elsif Ref = CCL.Types.Character_Type then
               if Last /= 1 then
                  Set_Error (Result, Too_Long, Path);
               else
                  Put (CCL.Objects.Character_Cell (Buffer (1)));
               end if;
            else
               CCL.Objects.Append_Text (Image, Buffer (1 .. Last), Step);
               if Step /= CCL.Objects.Added then
                  Set_Error (Result, Too_Large, Path);
               end if;
            end if;
         end;
         return;
      elsif Ref not in CCL.Types.Declared_Type or else not CCL.Types.Known (Types, Ref)
        or else Depth = Maximum_Depth
      then
         Set_Error (Result, Unsupported_Type, Path);
         return;
      end if;
      Shape := CCL.Types.Describe (Types, Ref);
      case Shape.Form is
         when CCL.Types.Bounded =>
            if not Kind_Is (Agent_Json.Number_Node) then
               Set_Error (Result, Wrong_Type, Path);
               return;
            end if;
            declare
               Value : Integer_64;
               OK : Boolean;
            begin
               Agent_Json.Integer_Value (Text, Doc.Nodes (Index), Value, OK);
               if not OK or else Value < CCL.Types.Low_Of (Types, Ref)
                 or else Value > CCL.Types.High_Of (Types, Ref)
               then
                  Set_Error (Result, Out_Of_Range, Path);
               else
                  Put (CCL.Objects.Integer_Cell (Value));
               end if;
            end;
         when CCL.Types.Product =>
            if not Kind_Is (Agent_Json.Object_Node) then
               Set_Error (Result, Wrong_Type, Path);
               return;
            end if;
            --  Every member names a field, once.
            declare
               Child : Natural := Index + 1;
               Key : String (1 .. Maximum_Key);
               Last : Natural;
               OK : Boolean;
            begin
               while Child < Doc.Nodes (Index).Next and then Result.Error = No_Error loop
                  pragma Loop_Invariant (Child > Index);
                  Key_Of (Text, Doc, Child, Key, Last, OK);
                  if not OK or else
                    not (for some F in 1 .. Shape.Count =>
                           CCL.Types.Image (Shape.Parts (F).Identifier) = Key (1 .. Last))
                  then
                     Set_Error (Result, Unknown_Field, Join (Path, Key (1 .. Last)));
                     return;
                  end if;
                  exit when not Sound (Text, Doc, Child + 1) or else Doc.Nodes (Child + 1).Next <= Child;
                  Child := Doc.Nodes (Child + 1).Next;
               end loop;
            end;
            Put (CCL.Objects.Product_Cell (Shape.Count));
            for F in 1 .. Shape.Count loop
               exit when Result.Error /= No_Error;
               declare
                  Name : constant String := CCL.Types.Image (Shape.Parts (F).Identifier);
                  Value : Natural;
                  Duplicate : Boolean;
                  Default : constant CCL.Types.Field_Default := CCL.Types.Default_Of (Types, Ref, F);
               begin
                  Member (Text, Doc, Index, Name, Value, Duplicate);
                  if Duplicate then
                     Set_Error (Result, Duplicate_Field, Join (Path, Name));
                  elsif Value /= 0 then
                     Decode_Value (Text, Doc, Types, Shape.Parts (F).Payload, Value, Join (Path, Name),
                                   Depth + 1, Image, Result);
                  else
                     case Default.Kind is
                        when CCL.Types.No_Default =>
                           Set_Error (Result, Missing_Field, Join (Path, Name));
                        when CCL.Types.Integer_Default =>
                           Put (CCL.Objects.Integer_Cell (Default.Value));
                        when CCL.Types.Boolean_Default =>
                           Put (CCL.Objects.Boolean_Cell (Default.Value = 1));
                        when CCL.Types.Alternative_Default =>
                           if Default.Value in 1 .. CCL.Types.Maximum_Components then
                              Put (CCL.Objects.Variant_Cell (Positive (Default.Value)));
                              Put (CCL.Objects.Unit_Cell);
                           else
                              Set_Error (Result, Unsupported_Type, Join (Path, Name));
                           end if;
                        when CCL.Types.Empty_List_Default =>
                           Put (CCL.Objects.Sequence_Cell (0));
                     end case;
                  end if;
               end;
            end loop;
         when CCL.Types.Sum =>
            declare
               Key : String (1 .. Maximum_Key);
               Last : Natural;
               OK : Boolean;
               Payload_Node : Natural := 0;
               Choice : Natural := 0;
            begin
               if CCL.Types.Is_Enumeration (Types, Ref) then
                  if not Kind_Is (Agent_Json.String_Node) then
                     Set_Error (Result, Wrong_Type, Path);
                     return;
                  end if;
                  Key_Of (Text, Doc, Index, Key, Last, OK);
               else
                  if not Kind_Is (Agent_Json.Object_Node) or else Doc.Nodes (Index).Members /= 1 then
                     Set_Error (Result, Wrong_Type, Path);
                     return;
                  end if;
                  Key_Of (Text, Doc, Index + 1, Key, Last, OK);
                  Payload_Node := Index + 2;
               end if;
               if OK then
                  for A in 1 .. Shape.Count loop
                     if CCL.Types.Image (Shape.Parts (A).Identifier) = Key (1 .. Last) then
                        Choice := A;
                     end if;
                  end loop;
               end if;
               if Choice = 0 then
                  Set_Error (Result, Unknown_Member, Join (Path, Key (1 .. Last)));
                  return;
               end if;
               Put (CCL.Objects.Variant_Cell (Choice));
               if Payload_Node = 0
                 or else Shape.Parts (Choice).Payload in CCL.Types.Unit_Type | CCL.Types.Invalid_Type
               then
                  if Payload_Node /= 0 and then not
                    (Sound (Text, Doc, Payload_Node) and then Doc.Nodes (Payload_Node).Kind = Agent_Json.Null_Node)
                  then
                     Set_Error (Result, Wrong_Type, Join (Path, Key (1 .. Last)));
                     return;
                  end if;
                  Put (CCL.Objects.Unit_Cell);
               else
                  Decode_Value (Text, Doc, Types, Shape.Parts (Choice).Payload, Payload_Node,
                                Join (Path, Key (1 .. Last)), Depth + 1, Image, Result);
               end if;
            end;
         when CCL.Types.Sequence =>
            if not Kind_Is (Agent_Json.Array_Node) then
               Set_Error (Result, Wrong_Type, Path);
               return;
            end if;
            Put (CCL.Objects.Sequence_Cell (Doc.Nodes (Index).Members));
            declare
               Child : Natural := Index + 1;
               Element : constant CCL.Types.Type_Reference := CCL.Types.Element_Of (Types, Ref);
            begin
               for K in 1 .. Doc.Nodes (Index).Members loop
                  exit when Result.Error /= No_Error or else not Sound (Text, Doc, Child)
                    or else Child >= Doc.Nodes (Index).Next;
                  Decode_Value (Text, Doc, Types, Element, Child, Path, Depth + 1, Image, Result);
                  Child := Doc.Nodes (Child).Next;
               end loop;
            end;
         when others =>
            Set_Error (Result, Unsupported_Type, Path);
      end case;
   end Decode_Value;

   --  The value of the one member Label of a top-level object.
   procedure Labelled
     (Text : String; Doc : Document; Label : String; Value : out Natural; Result : in out Decode_Result)
   is
      Duplicate : Boolean;
   begin
      Value := 0;
      if not (Sound (Text, Doc, 1) and then Doc.Nodes (1).Kind = Agent_Json.Object_Node) then
         Set_Error (Result, Not_An_Object, "");
         return;
      end if;
      Member (Text, Doc, 1, Label, Value, Duplicate);
      if Duplicate then
         Set_Error (Result, Duplicate_Field, Label);
      elsif Value = 0 then
         Set_Error (Result, Missing_Field, Label);
      elsif Doc.Nodes (1).Members /= 1 then
         Set_Error (Result, Unknown_Field, "");
      end if;
   end Labelled;

   procedure Decode_Typed
     (Input : Untrusted_Input; Types : CCL.Types.Registry; Root : CCL.Types.Type_Reference;
      Label : String; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Image; Result : out Decode_Result)
   is
      Doc : Document;
      Text : constant String := Input.Text (1 .. Input.Length);
      Start : Natural := 1;
   begin
      Result := (others => <>);
      Image := CCL.Objects.Empty (Contract);
      Parse_Input (Input, Doc, Result);
      if Result.Error /= No_Error then
         return;
      end if;
      if not (Sound (Text, Doc, 1) and then Doc.Nodes (1).Kind = Agent_Json.Object_Node) then
         Set_Error (Result, Not_An_Object, "");
         return;
      end if;
      if not (Root in CCL.Types.Declared_Type and then CCL.Types.Known (Types, Root)
              and then CCL.Types.Describe (Types, Root).Form = CCL.Types.Product)
      then
         Labelled (Text, Doc, Label, Start, Result);
         if Result.Error /= No_Error then
            return;
         end if;
      end if;
      Decode_Value (Text, Doc, Types, Root, Start, (if Start = 1 then "" else Label), 0, Image, Result);
      if Result.Error = No_Error and then not CCL.Objects.Validate (Image, Contract) then
         Set_Error (Result, Wrong_Type, "");
      end if;
   end Decode_Typed;

   procedure Decode_Text
     (Input : Untrusted_Input; Label : String; Limit : Natural;
      Into : out String; Last : out Natural; Result : out Decode_Result)
   is
      Doc : Document;
      Text : constant String := Input.Text (1 .. Input.Length);
      Value : Natural;
      OK : Boolean;
   begin
      Result := (others => <>);
      Into := [others => ' '];
      Last := 0;
      Parse_Input (Input, Doc, Result);
      if Result.Error /= No_Error then
         return;
      end if;
      Labelled (Text, Doc, Label, Value, Result);
      if Result.Error /= No_Error then
         return;
      elsif not (Sound (Text, Doc, Value) and then Doc.Nodes (Value).Kind = Agent_Json.String_Node) then
         Set_Error (Result, Wrong_Type, Label);
         return;
      end if;
      Agent_Json.Decode_String (Text, Doc.Nodes (Value), Into, Last, OK);
      if not OK or else Last > Limit then
         Set_Error (Result, Too_Long, Label);
         Into := [others => ' '];
         Last := 0;
      end if;
   end Decode_Text;

   procedure Decode_Integer
     (Input : Untrusted_Input; Label : String; Value : out Integer_64; Result : out Decode_Result)
   is
      Doc : Document;
      Text : constant String := Input.Text (1 .. Input.Length);
      At_Node : Natural;
      OK : Boolean;
   begin
      Result := (others => <>);
      Value := 0;
      Parse_Input (Input, Doc, Result);
      if Result.Error /= No_Error then
         return;
      end if;
      Labelled (Text, Doc, Label, At_Node, Result);
      if Result.Error /= No_Error then
         return;
      elsif not (Sound (Text, Doc, At_Node) and then Doc.Nodes (At_Node).Kind = Agent_Json.Number_Node) then
         Set_Error (Result, Wrong_Type, Label);
         return;
      end if;
      Agent_Json.Integer_Value (Text, Doc.Nodes (At_Node), Value, OK);
      if not OK then
         Set_Error (Result, Out_Of_Range, Label);
         Value := 0;
      end if;
   end Decode_Integer;

   procedure Decode_Nothing (Input : Untrusted_Input; Result : out Decode_Result) is
      Doc : Document;
      Text : constant String := Input.Text (1 .. Input.Length);
   begin
      Result := (others => <>);
      Parse_Input (Input, Doc, Result);
      if Result.Error /= No_Error then
         return;
      elsif not (Sound (Text, Doc, 1) and then Doc.Nodes (1).Kind = Agent_Json.Object_Node) then
         Set_Error (Result, Not_An_Object, "");
      elsif Doc.Nodes (1).Members /= 0 then
         Set_Error (Result, Unknown_Field, "");
      end if;
   end Decode_Nothing;
end Agent_Decoders;
