with CCL.Host_Values;
with CCL.Language;
with CCL.Objects.Catalog;
with CCL.Types;
with CCL.VM;
with SPARKNaCl;
with SPARKTLSCrypto.Hashing.SHA256;

package body CCL.Interfaces.Programs is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Language.Analysis_Status;
   use type CCL.Objects.Catalog.Publication_Result;
   use type PD.Kind;
   use type PD.Connector_Direction;
   use type PD.Element_Kind;

   --  The last component of a launch-table name ("toolchain/bin/gcc" is
   --  "gcc"): programs installed in a tree are named as they are called.
   function Base_Name (Program : String) return String is
      Separator : constant Character := '/';
      First : Positive := Program'First;
   begin
      for K in Program'Range loop
         if Program (K) = Separator then
            First := K + 1;
         end if;
      end loop;
      declare
         Name : constant String := Program (First .. Program'Last);
      begin
         return (if Name'Length > 4
                   and then (Name (Name'Last - 3 .. Name'Last) = ".app"
                             or else Name (Name'Last - 3 .. Name'Last) = ".svc")
                 then Name (Name'First .. Name'Last - 4) else Name);
      end;
   end Base_Name;

   function Interface_Name (Program : String) return String is (Base_Name (Program));

   function Parameters_Type (Program : String) return String is
      Name : constant String := Base_Name (Program);
      Result : String (1 .. Name'Length);
      Upper : Boolean := True;
   begin
      for K in Name'Range loop
         declare
            C : constant Character := Name (K);
            R : Character renames Result (K - Name'First + 1);
         begin
            if C in '-' | '_' | '.' then
               R := '_';
               Upper := True;
            elsif Upper and then C in 'a' .. 'z' then
               R := Character'Val (Character'Pos (C) - 32);
               Upper := False;
            else
               R := C;
               Upper := False;
            end if;
         end;
      end loop;
      return Result & "_Parameters";
   end Parameters_Type;

   function Key_Of (Text : String; Suffix : String := "") return CCL.Objects.Schema_Key is
      Message : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (Text'Length + Suffix'Length) - 1);
      Digest : SPARKTLSCrypto.Hashing.SHA256.Digest;
      Key : CCL.Objects.Schema_Key := [others => 0];
   begin
      for K in Text'Range loop
         Message (SPARKNaCl.N32 (K - Text'First)) := Character'Pos (Text (K));
      end loop;
      for K in Suffix'Range loop
         Message (SPARKNaCl.N32 (Text'Length + K - Suffix'First)) := Character'Pos (Suffix (K));
      end loop;
      SPARKTLSCrypto.Hashing.SHA256.Hash (Digest, Message);
      for Word in Key'Range loop
         for B in 0 .. 7 loop
            Key (Word) := Shift_Left (Key (Word), 8) or
              Unsigned_64 (Digest (SPARKNaCl.Index_32 (Word * 8 + B)));
         end loop;
      end loop;
      return Key;
   end Key_Of;

   --  A parameter's field: "(name Type)", with a default when it may be
   --  absent; many or optional values are a List.
   function Field (P : PD.Parameter) return String is
      Name : constant String := P.Name (1 .. P.Name_Length);
      Base : constant String :=
        (case P.Of_Kind is
            when PD.Input_File => "Input_File", when PD.Output_File => "Output_File",
            when PD.Input_Directory => "Input_Directory",
            when PD.Output_Directory => "Output_Directory",
            when PD.Flag => "Boolean", when PD.Text => "String");
   begin
      if P.Of_Kind = PD.Flag then
         return " (" & Name & " Boolean false)";
      elsif P.Optional then
         return " (" & Name & " (List " & Base & ") [])";
      elsif P.Many then
         return " (" & Name & " (List " & Base & "))";
      else
         return " (" & Name & " " & Base & ")";
      end if;
   end Field;

   function Fields (Description : PD.Signature; From : Natural) return String is
     (if From >= Description.Parameter_Total then ""
      else Field (Description.Parameters (From)) & Fields (Description, From + 1));

   function Type_Source (Program : String; Description : PD.Signature) return String is
     (SHARED_SOURCE & " (type " & Parameters_Type (Program) & " (record" &
      Fields (Description, 0) & "))");

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings;
      Index : Program_Index; Program : String; Description : PD.Signature;
      Bound : out Contracts; Error : out CCL.Catalog.Catalog_Error)
   is
      Source : constant String := Type_Source (Program, Description);
      Record_Name : constant String := Parameters_Type (Program);
      Name : constant String := Interface_Name (Program);
      Checked : CCL.Language.Analysis_Result;
      Types : CCL.Types.Registry;
      Accepted : Boolean;
      Result : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Operation : CCL.Catalog.Operation_Descriptor;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
      Grant : CCL.Catalog.Grant_Result;
      Digest : constant CCL.Objects.Schema_Key := Key_Of (Source);

      procedure Bind (Type_Name : String; Key : CCL.Objects.Schema_Key;
                      Contract : out CCL.Objects.Binding) is
         Unbound : CCL.Objects.Binding;
      begin
         Contract := Unbound;
         if Accepted then
            CCL.Objects.Bind (Types, CCL.Types.Find (Types, CCL.Types.Named (Type_Name)),
                              Key, Contract, Accepted);
         end if;
         if Accepted then
            CCL.Catalog.Publish_Schema (Item, Contract, Result);
            Accepted := Result in CCL.Objects.Catalog.Published
                                | CCL.Objects.Catalog.Already_Published;
         end if;
      end Bind;

      procedure Add (Operation_Name : String; Contract : CCL.Host_Values.Import_Declaration) is
      begin
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Define_Host_Operation (Operation_Name, 1, Contract, Operation, Error);
         end if;
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Operation, Error);
         end if;
      end Add;

      function Outlet_Contract (P : PD.Connector) return CCL.Host_Values.Import_Declaration is
        ((Argument => CCL.Host_Values.Object_Value,
          Argument_Schema => CCL.Objects.Identity (Bound.Run),
          Result => (if P.Element = PD.Integers then CCL.Host_Values.Integer_Value
                     else CCL.Host_Values.Text_Value),
          Result_Stream => True,
          Authority => CCL.VM.Observe_Authority, others => <>));
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      CCL.Language.Analyze (Source & " 0", Checked);
      Accepted := CCL.Language.Analysis_Status_Of (Checked) = CCL.Language.Analysis_Succeeded;
      if not Accepted then return; end if;
      Types := CCL.Language.Analysis_Types (Checked);
      Bind ("Input_File", Key_Of (SHARED_SOURCE, "#Input_File"), Bound.Input_File);
      Bind ("Output_File", Key_Of (SHARED_SOURCE, "#Output_File"), Bound.Output_File);
      Bind ("Input_Directory", Key_Of (SHARED_SOURCE, "#Input_Directory"), Bound.Input_Directory);
      Bind ("Output_Directory", Key_Of (SHARED_SOURCE, "#Output_Directory"), Bound.Output_Directory);
      Bind ("Run", Key_Of (SHARED_SOURCE, "#Run"), Bound.Run);
      Bind (Record_Name, Key_Of (Source, "#" & Record_Name), Bound.Parameters);
      Bind ("Outlet_State", Key_Of (SHARED_SOURCE, "#Outlet_State"), Bound.Outlet_State);
      Bind ("Run_Outcome", Key_Of (SHARED_SOURCE, "#Run_Outcome"), Bound.Run_Outcome);
      if Accepted then
         declare
            Listed : CCL.Types.List_Result;
            List_Type : CCL.Types.Type_Reference;
            use type CCL.Types.List_Result;
         begin
            CCL.Types.Specialize_List
              (Types, CCL.Types.Find (Types, CCL.Types.Named ("Outlet_State")), List_Type, Listed);
            Accepted := Listed in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
            if Accepted then
               CCL.Objects.Bind (Types, List_Type, Key_Of (SHARED_SOURCE, "#List-Outlet_State"),
                                 Bound.Outlet_States, Accepted);
            end if;
            if Accepted then
               CCL.Catalog.Publish_Schema (Item, Bound.Outlet_States, Result);
               Accepted := Result in CCL.Objects.Catalog.Published
                                   | CCL.Objects.Catalog.Already_Published;
            end if;
         end;
      end if;
      if not Accepted then return; end if;

      CCL.Catalog.Define_Interface
        (Name, 1, 0, [for W in CCL.Catalog.Descriptor_Digest'Range => Digest (W)],
         Descriptor, Error);
      --  Starting a program is an effect on the system: it needs control.
      Add ("run", (Argument => CCL.Host_Values.Object_Value,
                   Argument_Schema => CCL.Objects.Identity (Bound.Parameters),
                   Result => CCL.Host_Values.Object_Value,
                   Result_Schema => CCL.Objects.Identity (Bound.Run),
                   Authority => CCL.VM.Control_Authority, others => <>));
      for P in 0 .. Description.Connector_Total - 1 loop
         if Description.Connectors (P).Direction = PD.Outlet then
            Add (Description.Connectors (P).Name (1 .. Description.Connectors (P).Name_Length),
                 Outlet_Contract (Description.Connectors (P)));
         end if;
      end loop;
      Add (OUTLETS_OPERATION,
           (Argument => CCL.Host_Values.Object_Value,
            Argument_Schema => CCL.Objects.Identity (Bound.Run),
            Result => CCL.Host_Values.Object_Value,
            Result_Schema => CCL.Objects.Identity (Bound.Outlet_States),
            Authority => CCL.VM.Observe_Authority, others => <>));
      Add (OUTCOME_OPERATION,
           (Argument => CCL.Host_Values.Object_Value,
            Argument_Schema => CCL.Objects.Identity (Bound.Run),
            Result => CCL.Host_Values.Object_Value,
            Result_Schema => CCL.Objects.Identity (Bound.Run_Outcome), Result_Task => True,
            Authority => CCL.VM.Observe_Authority, others => <>));
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
      if Error /= CCL.Catalog.Catalog_Valid then return; end if;

      --  Grant: run at number 0, the outlets from 1 in declaration
      --  order (inlets have no accessor yet), then outcome and outlets.
      declare
         Number : Natural := 0;
         procedure Grant_Named (Operation_Name : String) is
         begin
            if Error /= CCL.Catalog.Catalog_Valid then return; end if;
            CCL.Catalog.Resolve (Item, Name & "." & Operation_Name, Resolved, Found);
            if Found then
               CCL.Catalog.Install (Grants, Resolved, Binding_Of (Index, Number), Grant);
            end if;
            if not Found or else Grant /= CCL.Catalog.Grant_Added then
               Error := CCL.Catalog.Invalid_Host_Contract;
            end if;
            Number := Number + 1;
         end Grant_Named;
      begin
         Grant_Named ("run");
         for P in 0 .. Description.Connector_Total - 1 loop
            Number := P + 1;
            if Description.Connectors (P).Direction = PD.Outlet then
               Grant_Named (Description.Connectors (P).Name (1 .. Description.Connectors (P).Name_Length));
            end if;
         end loop;
         Number := OUTCOME_NUMBER;
         Grant_Named (OUTCOME_OPERATION);
         Number := OUTLETS_NUMBER;
         Grant_Named (OUTLETS_OPERATION);
      end;
   end Publish;
end CCL.Interfaces.Programs;
