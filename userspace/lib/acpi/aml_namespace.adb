pragma Ada_2022;
package body AML_Namespace with SPARK_Mode is
   function Count (Tree : State) return Node_ID is (Tree.Used);
   function Parent (Tree : State; Node : Node_ID) return Node_ID is
     (if Node = Root then Root else Tree.Items (Node).Up);
   function Name (Tree : State; Node : Node_ID) return AML_Names.Segment is
     (Tree.Items (Node).Part);
   function Empty return State is
     ((Used => 0, Items => [others => (Up => Root, Part => "____", others => <>)]));

   function Child
     (Tree : State; Scope : Node_ID; Part : AML_Names.Segment) return Node_ID
   is
   begin
      for I in 1 .. Tree.Used loop
         pragma Loop_Invariant
           (for all J in 1 .. I - 1 =>
              Tree.Items (J).Up /= Scope or else Tree.Items (J).Part /= Part);
         if Tree.Items (I).Up = Scope and then Tree.Items (I).Part = Part then
            return I;
         end if;
      end loop;
      return Root;
   end Child;

   function Resolve
     (Tree : State; Scope : Node_ID; Path : AML_Names.Name_Result)
      return Lookup_Result
   is
      Base : Node_ID := Scope;
      Next : Node_ID;
   begin
      if Path.Kind /= AML_Names.Accepted then
         return (Status => Invalid_Path);
      end if;
      if Path.Rooted and then Path.Parents /= 0 then
         return (Status => Invalid_Path);
      end if;
      for I in 1 .. Path.Count loop
         if not AML_Names.Valid (Path.Parts (I)) then
            return (Status => Invalid_Path);
         end if;
      end loop;
      if Path.Rooted then
         Base := Root;
      end if;
      for I in 1 .. Path.Parents loop
         pragma Loop_Invariant (Base <= Count (Tree));
         if Base = Root then
            return (Status => Above_Root);
         end if;
         Base := Parent (Tree, Base);
      end loop;
      if not Path.Rooted and then Path.Parents = 0 and then Path.Count = 1 then
         loop
            pragma Loop_Invariant (Base <= Count (Tree));
            pragma Loop_Variant (Decreases => Base);
            Next := Child (Tree, Base, Path.Parts (1));
            if Next /= Root then
               return (Status => Found, Node => Next);
            elsif Base = Root then
               return (Status => Not_Found);
            end if;
            Base := Parent (Tree, Base);
         end loop;
      end if;
      for I in 1 .. Path.Count loop
         pragma Loop_Invariant (Base <= Count (Tree));
         pragma Loop_Invariant
           (if I > 1 then Base /= Root and then
              Name (Tree, Base) = Path.Parts (I - 1));
         Next := Child (Tree, Base, Path.Parts (I));
         if Next = Root then
            return (Status => Not_Found);
         end if;
         Base := Next;
      end loop;
      return (Status => Found, Node => Base);
   end Resolve;

   procedure Insert
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Node : out Node_ID; Result : out Insert_Status)
   is
   begin
      Node := Root;
      if not AML_Names.Valid (Part) then
         Result := Invalid_Name;
      elsif Child (Tree, Scope, Part) /= Root then
         Result := Duplicate;
      elsif Tree.Used = Capacity then
         Result := Full;
      else
         Tree.Used := Tree.Used + 1;
         Tree.Items (Tree.Used) := (Up => Scope, Part => Part, others => <>);
         Node := Tree.Used;
         Result := Inserted;
      end if;
   end Insert;
   function Kind (Tree : State; Node : Node_ID) return Object_Kind is
     (if Node = Root then Scope_Object else Tree.Items (Node).Object_Type);
   function Has_Integer (Tree : State; Node : Node_ID) return Boolean is
     (Node /= Root and then Tree.Items (Node).Object_Type = Integer_Object);
   function Integer_Data (Tree : State; Node : Node_ID)
      return AML_Decode.Integer_Value is (Tree.Items (Node).Value);

   function String_Data (Tree : State; Node : Node_ID) return String is
     (Tree.Items (Node).Text (1 .. Tree.Items (Node).Text_Length));

   function Buffer_Data (Tree : State; Node : Node_ID) return AML_Decode.Bytes is
     (Tree.Items (Node).Buffer_Value (1 .. Tree.Items (Node).Buffer_Length));

   procedure Load_Names
     (Tree : in out State; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width; Result : out Load_Status)
   is
      use type AML_Decode.Byte;
      use type AML_Decode.Status;
      Candidate : State := Tree;
      Offset : Natural := 0;
      Path : AML_Names.Name_Result;
      Value : AML_Decode.Integer_Result;
      Text : AML_Decode.String_Result;
      Buffer_Item : AML_Decode.Buffer_Result;
      Scope, Node : Node_ID;
      Added : Insert_Status;
      type Frame is record
         Limit : Natural;
         Scope : Node_ID;
      end record;
      Frames : array (Natural range 0 .. 64) of Frame :=
        [others => (Limit => Data'Length, Scope => Root)];
      Depth : Natural range 0 .. 64 := 0;
      Op : AML_Decode.Byte;
      Is_Scope, Is_Device : Boolean;
      Limit : Natural;
      Package_Info : AML_Decode.Package_Result;
      Located : Lookup_Result;
   begin
      Result := Loaded;
      loop
         pragma Loop_Invariant (Offset <= Data'Length);
         pragma Loop_Invariant (Tree = Tree'Loop_Entry);
         pragma Loop_Invariant
           (for all I in 1 .. Candidate.Used => Candidate.Items (I).Up < I);
         pragma Loop_Invariant
           (for all I in 0 .. Depth =>
              Offset <= Frames (I).Limit and then
              Frames (I).Limit <= Data'Length and then
              Frames (I).Scope <= Candidate.Used);
         pragma Loop_Invariant
           (for all I in 0 .. Depth =>
              (for all J in I .. Depth => Frames (J).Limit <= Frames (I).Limit));
         pragma Loop_Variant
           (Decreases => Data'Length - Offset, Decreases => Depth);
         if Offset = Frames (Depth).Limit then
            exit when Depth = 0;
            Depth := Depth - 1;
         else
            Limit := Frames (Depth).Limit;
            Op := Data (Data'First + Offset);
            Offset := Offset + 1;
            Is_Scope := Op = 16#10#;
            Is_Device := False;
            if Op = 16#5B# and then Offset < Limit then
               Is_Device := Data (Data'First + Offset) = 16#82#;
               Offset := Offset + 1;
            end if;
            if not Is_Scope and then not Is_Device and then Op /= 16#08# then
               Result := Unsupported_Opcode; return;
            end if;
            if Is_Scope or Is_Device then
               if Offset = Limit then Result := Bad_Package; return; end if;
               Package_Info := AML_Decode.Read_Package
                 (Data (Data'First + Offset .. Data'First + (Limit - 1)));
               if Package_Info.Kind /= AML_Decode.Accepted then
                  Result := Bad_Package; return;
               end if;
               Limit := Offset + Package_Info.Extent;
               Offset := Offset + Package_Info.Encoding_Bytes;
            end if;
            if Offset = Limit then Result := Bad_Name; return; end if;
            Path := AML_Names.Read_Name
              (Data (Data'First + Offset .. Data'First + (Limit - 1)));
            if Path.Kind /= AML_Names.Accepted then
               Result := Bad_Name; return;
            end if;
            Offset := Offset + Path.Consumed;
            if Is_Scope then
               Located := Resolve (Candidate, Frames (Depth).Scope, Path);
               if Located.Status /= Found then
                  Result := Missing_Scope; return;
               end if;
               Node := Located.Node;
               if Kind (Candidate, Node) not in Scope_Object | Device_Object then
                  Result := Missing_Scope; return;
               end if;
            else
               if Path.Count = 0 then Result := Bad_Name; return; end if;
               Scope := (if Path.Rooted then Root else Frames (Depth).Scope);
               for I in 1 .. Path.Parents loop
                  pragma Loop_Invariant (Scope <= Candidate.Used);
                  if Scope = Root then Result := Missing_Scope; return; end if;
                  Scope := Parent (Candidate, Scope);
               end loop;
               for I in 1 .. Path.Count - 1 loop
                  pragma Loop_Invariant (Scope <= Candidate.Used);
                  Scope := Child (Candidate, Scope, Path.Parts (I));
                  if Scope = Root or else Kind (Candidate, Scope) not in Scope_Object | Device_Object then
                     Result := Missing_Scope; return;
                  end if;
               end loop;
               Insert (Candidate, Scope, Path.Parts (Path.Count), Node, Added);
               case Added is
                  when Duplicate => Result := Duplicate_Name; return;
                  when Full => Result := Storage_Full; return;
                  when Invalid_Name => Result := Bad_Name; return;
                  when Inserted => null;
               end case;
            end if;
            if Is_Device then
               Candidate.Items (Node).Object_Type := Device_Object;
            end if;
            if Is_Scope or Is_Device then
               if Depth = 64 then Result := Nesting_Limit; return; end if;
               Depth := Depth + 1;
               Frames (Depth) := (Limit => Limit, Scope => Node);
            else
               if Offset = Limit then Result := Bad_Integer; return; end if;
               if Data (Data'First + Offset) = 16#0D# then
                  Text := AML_Decode.Read_String
                    (Data (Data'First + Offset .. Data'First + (Limit - 1)));
                  if Text.Kind /= AML_Decode.Accepted then
                     Result := (if Text.Kind = AML_Decode.Limit_Exceeded then
                                  Value_Limit else Bad_String);
                     return;
                  end if;
                  Offset := Offset + Text.Consumed;
                  Candidate.Items (Node).Object_Type := String_Object;
                  Candidate.Items (Node).Text := Text.Text;
                  Candidate.Items (Node).Text_Length := Text.Length;
               elsif Data (Data'First + Offset) = 16#11# then
                  Buffer_Item := AML_Decode.Read_Buffer
                    (Data (Data'First + Offset .. Data'First + (Limit - 1)), Width);
                  if Buffer_Item.Kind /= AML_Decode.Accepted then
                     Result := (if Buffer_Item.Kind = AML_Decode.Limit_Exceeded then
                                  Value_Limit else Bad_Buffer);
                     return;
                  end if;
                  Offset := Offset + Buffer_Item.Consumed;
                  Candidate.Items (Node).Object_Type := Buffer_Object;
                  Candidate.Items (Node).Buffer_Value := Buffer_Item.Content;
                  Candidate.Items (Node).Buffer_Length := Buffer_Item.Length;
               else
                  Value := AML_Decode.Read_Integer
                    (Data (Data'First + Offset .. Data'First + (Limit - 1)), Width);
                  if Value.Kind /= AML_Decode.Accepted then
                     Result := Bad_Integer; return;
                  end if;
                  Offset := Offset + Value.Consumed;
                  Candidate.Items (Node).Object_Type := Integer_Object;
                  Candidate.Items (Node).Value := Value.Value;
               end if;
            end if;
         end if;
      end loop;
      Tree := Candidate;
   end Load_Names;
end AML_Namespace;
