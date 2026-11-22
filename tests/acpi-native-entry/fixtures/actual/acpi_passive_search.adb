pragma Ada_2022;
with ACPI_Passive_Query.Fields;
package body ACPI_Passive_Search with SPARK_Mode is
   package W renames Wire;
   package V renames Core.Values;
   package N renames Core.Namespace;
   package Fields renames ACPI_Passive_Query.Fields;
   use type W.Word;
   use type W.Instance_ID;
   use type W.Generation;
   use type W.Operation;
   use type W.Decode_Status;
   use type W.Device_ID;
   use type V.Access_Status;
   use type N.Object_Kind;
   use type Reader.Outcome;
   use type Reader.Atom_Kind;
   use type Reader.Member_Name;
   Limit : constant W.Node_Limit := W.Node_Limit (Core.Max_Namespace_Nodes);
   Canonical_ID_Max_Length : constant := 8;
   type Match_Result is (Matched, Not_Matched, Bad_Value);
   procedure Search
     (Service : in out Core.State; Scope : Scope_State; Request_Data : Pages.Byte_Array;
      Maximum_Node_Visits : Positive; Nodes : out Node_Array;
      Scratch_Data : out Firmware_Tables.Bytes; Scratch_Atoms : out Reader.Atoms;
      Result : out Report) is
      Query : constant W.Decode_Result := Pages.Decode (Request_Data, Limit);
      Root, Parent, Device : Core.Node_Descriptor;
      Status : V.Access_Status;
      Last : N.Node_ID;
      Position : N.Node_ID := 1;
      Observation : Reader.Report;
      Failed : Boolean := False;

      procedure Reject (Reason : Outcome) is
      begin
         Nodes := [others => No_Reference]; Result.Copied_Nodes := 0;
         Result.Next_Cursor := (Present => False); Result.Status := Reason;
         Failed := True;
      end Reject;

      function Query_Equals (Text : String) return Boolean
        with Pre => Query.Status = W.Decoded
      is
         Offset : Natural;
      begin
         if not W.Is_External (Query.Value.Key.ID) then
            declare ID : constant W.ID_Result := W.From_Text (Text); begin
               return ID.Accepted and then ID.Value = Query.Value.Key.ID;
            end;
         end if;
         if W.Word (Text'Length) /= W.Word (W.Text_Length (Query.Value.Key.ID))
           or else W.Word (W.Text_Offset (Query.Value.Key.ID)) > W.Word (Natural'Last)
         then return False; end if;
         Offset := Natural (W.Text_Offset (Query.Value.Key.ID));
         if Request_Data'Length = 0 or else Offset > Request_Data'Last - Request_Data'First
           or else Text'Length > Request_Data'Last - Request_Data'First - Offset + 1
         then return False; end if;
         for I in 0 .. Text'Length - 1 loop
            if W.Word (Request_Data (Request_Data'First + (Offset + I))) /= Character'Pos (Text (Text'First + I))
            then return False; end if;
         end loop;
         return True;
      end Query_Equals;

      function Match_Atom (Atom : Reader.Atom; Member : Reader.Member_Name) return Match_Result
        with Pre => Query.Status = W.Decoded
      is
      begin
         if Atom.Kind = Reader.Integer_Atom then
            declare ID : constant W.ID_Result := Fields.From_EISA (W.Word (Atom.Number)); begin
               if not ID.Accepted then return Not_Matched; end if;
               return (if Query_Equals (W.Image (ID.Value)) then Matched else Not_Matched);
            end;
         elsif Atom.Kind /= Reader.Text_Atom then return Bad_Value;
         end if;
         if Atom.Offset > Observation.Copied_Bytes or else Atom.Length > Observation.Copied_Bytes - Atom.Offset
           or else Atom.Offset > Scratch_Data'Length or else Atom.Length > Scratch_Data'Length - Atom.Offset
         then return Bad_Value; end if;
         if not W.Is_External (Query.Value.Key.ID) or else Member = Reader.Hardware_ID then
            if Atom.Length not in 7 | 8 then return Not_Matched; end if;
            declare
               Text : String (1 .. Canonical_ID_Max_Length) := [others => ' '];
               ID : W.ID_Result;
            begin
               for I in 0 .. Atom.Length - 1 loop
                  Text (1 + I) := Character'Val (Scratch_Data (Scratch_Data'First + (Atom.Offset + I)));
               end loop;
               ID := W.From_Text (Text (1 .. Atom.Length));
               if not ID.Accepted then return Not_Matched; end if;
               if not W.Is_External (Query.Value.Key.ID) then
                  return (if ID.Value = Query.Value.Key.ID then Matched else Not_Matched);
               end if;
               return (if Query_Equals (Text (1 .. Atom.Length)) then Matched else Not_Matched);
            end;
         end if;
         if W.Word (Atom.Length) /= W.Word (W.Text_Length (Query.Value.Key.ID)) then return Not_Matched; end if;
         declare Offset : constant W.Word := W.Word (W.Text_Offset (Query.Value.Key.ID)); begin
            if Offset > W.Word (Natural'Last) or else Request_Data'Length = 0
              or else Natural (Offset) > Request_Data'Last - Request_Data'First
              or else Atom.Length > Request_Data'Last - Request_Data'First - Natural (Offset) + 1
            then return Bad_Value; end if;
            for I in 0 .. Atom.Length - 1 loop
               if W.Word (Scratch_Data (Scratch_Data'First + (Atom.Offset + I))) /=
                 W.Word (Request_Data (Request_Data'First + (Natural (Offset) + I)))
               then return Not_Matched; end if;
            end loop;
         end;
         return Matched;
      end Match_Atom;

      procedure Read_Match (Member : Reader.Member_Name; Hit : out Boolean)
        with Pre => Query.Status = W.Decoded
      is
      begin
         Hit := False;
         Reader.Read (Service, Device, Member, Scratch_Data, Scratch_Atoms, Observation);
         Result.Read_Status := Observation.Status; Result.Access_Result := Observation.Access_Result;
         if Observation.Access_Result /= V.Available or else Observation.Cleanup_Result /= V.Available then
            Reject (Observation_Failed); return;
         elsif Observation.Status = Reader.Capacity_Required then
            Result.Required_Atoms := Observation.Required_Atoms;
            Result.Required_Bytes := Observation.Required_Bytes;
            Result.Byte_Size_Known := Observation.Size_Known;
            Reject (Scratch_Required); return;
         elsif Observation.Status in Reader.Missing | Reader.Method_Valued | Reader.Unsupported then return;
         elsif Observation.Status /= Reader.Static_Value or else not Observation.Size_Known
           or else Observation.Access_Result /= V.Available or else Observation.Cleanup_Result /= V.Available
           or else Observation.Copied_Atoms /= Observation.Required_Atoms
           or else Observation.Copied_Bytes /= Observation.Required_Bytes
           or else Observation.Copied_Atoms > Scratch_Atoms'Length
           or else Observation.Copied_Bytes > Scratch_Data'Length
         then Reject (Observation_Failed); return; end if;
         if Member = Reader.Address then
            if Observation.Copied_Atoms /= 1 or else Scratch_Atoms (Scratch_Atoms'First).Kind /= Reader.Integer_Atom
            then Reject (Observation_Failed); return; end if;
            Hit := W.Word (Scratch_Atoms (Scratch_Atoms'First).Number) = Query.Value.Key.Address;
            return;
         elsif Member = Reader.Hardware_ID and then Observation.Copied_Atoms /= 1 then
            Reject (Observation_Failed); return;
         end if;
         for I in 0 .. Observation.Copied_Atoms - 1 loop
            case Match_Atom (Scratch_Atoms (Scratch_Atoms'First + I), Member) is
               when Matched => Hit := True; return;
               when Not_Matched => null;
               when Bad_Value => Reject (Observation_Failed); return;
            end case;
         end loop;
      end Read_Match;

      procedure More (Next : N.Node_ID) is
      begin
         Result.Next_Cursor := (True, (Result.Key, W.Node_Scan, W.Node_Index (Next), 0, 0));
      end More;
   begin
      Nodes := [others => No_Reference]; Scratch_Data := [others => 0];
      Scratch_Atoms := [others => (others => <>)]; Result := (others => <>);
      if Query.Status /= W.Decoded or else Query.Value.Key.Kind not in W.Find_By_ID | W.Find_By_Address then return; end if;
      Result.Key := Query.Value.Key;
      if not Scope.Ready then Reject (Namespace_Not_Ready); return; end if;
      if Scope.Instance = 0 or else Scope.Revision = 0 then Reject (Invalid_Request); return; end if;
      if Query.Value.Key.Instance /= 0 and then
        (Query.Value.Key.Instance /= Scope.Instance or else Query.Value.Key.Revision /= Scope.Revision)
      then Reject (Stale_Reference); return; end if;
      Result.Key.Instance := Scope.Instance; Result.Key.Revision := Scope.Revision;
      Core.Describe_Node (Service, N.Root, Root, Status);
      if Status /= V.Available or else not Core.Current_Node (Service, Root) then
         Result.Access_Result := Status; Reject (Namespace_Not_Ready); return;
      end if;
      Last := Core.Namespace_Node_Count (Service);
      if Query.Value.Key.Kind = W.Find_By_Address then
         if W.Word (Query.Value.Key.Node) > W.Word (Last) then Reject (Invalid_Parent); return; end if;
         Core.Describe_Node (Service, N.Node_ID (Query.Value.Key.Node), Parent, Status);
         if Status /= V.Available or else not Core.Current_Node (Service, Parent)
           or else V.Node_Kind (Parent) not in N.Device_Object | N.Scope_Object
         then Result.Access_Result := Status; Reject (Invalid_Parent); return; end if;
      end if;
      if Query.Value.Continuation.Present then
         if W.Word (Query.Value.Continuation.Value.Next_Node) > W.Word (Last)
         then Reject (Bad_Continuation); return; end if;
         Position := N.Node_ID (Query.Value.Continuation.Value.Next_Node);
      end if;
      if Nodes'Length = 0 then Reject (Output_Required); return; end if;
      Result.Status := Page_Ready;
      while Position <= Last loop
         if Result.Copied_Nodes = Nodes'Length or else Result.Visited_Nodes = Maximum_Node_Visits then
            More (Position); exit;
         end if;
         Result.Visited_Nodes := Result.Visited_Nodes + 1;
         Core.Describe_Node (Service, Position, Device, Status);
         if Status = V.Invalid_Value then null; -- Reserved hole below high-water count.
         elsif Status /= V.Available or else not Core.Current_Node (Service, Device) then
            Result.Access_Result := Status; Reject (Observation_Failed); return;
         elsif V.Node_Kind (Device) = N.Device_Object then
            declare Hit : Boolean := False; begin
               if Query.Value.Key.Kind = W.Find_By_ID then
                  Read_Match (Reader.Hardware_ID, Hit);
                  if not Hit and then not Failed then Read_Match (Reader.Compatible_ID, Hit); end if;
               elsif V.Parent_Position (Device) = N.Node_ID (Query.Value.Key.Node) then
                  Read_Match (Reader.Address, Hit);
               end if;
               if Failed then return; end if;
               if not Core.Current_Node (Service, Device) then Reject (Observation_Failed); return; end if;
               if Hit then
                  Nodes (Nodes'First + Result.Copied_Nodes) := (Scope.Instance, Scope.Revision, W.Node_Index (Position));
                  Result.Copied_Nodes := Result.Copied_Nodes + 1;
               end if;
            end;
         end if;
         if Position = Last then exit; end if;
         Position := Position + 1;
      end loop;
      if Core.Namespace_Node_Count (Service) /= Last then Reject (Observation_Failed); return; end if;
      if Result.Copied_Nodes = 0 and then not Result.Next_Cursor.Present then Result.Status := No_Static_Match; end if;
   end Search;
end ACPI_Passive_Search;
