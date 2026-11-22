with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with Config_Authority;
with Config_Collections; use Config_Collections;

with CuBit.Process_IDs;
procedure Collection_Tests is
   use type CuBit.Process_IDs.Process_ID;
   function PID (Word : Interfaces.Unsigned_64) return CuBit.Process_IDs.Process_ID
     renames CuBit.Process_IDs.From_Word;
   package A renames Config_Authority;
   use type A.Install_Result;
   use type CCL.Objects.Binding;
   Table : State;
   Authority : A.Authority_State;
   Rules, Other_Rules, Global : A.Rule_Set;
   Types : CCL.Types.Registry;
   Contract, Other_Contract, Retrieved, Unbound : CCL.Objects.Binding;
   Schema : constant CCL.Objects.Schema_Key := [1, 2, 3, 4];
   ID, Read_ID : Collection_ID;
   Token, Other, Fresh, Read_Only : Handle;
   Status : Result;
   Installed : A.Install_Result;
   Good : Boolean;
   Name : Collection_Name;
   Length, Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with "collection check" & Checks'Image; end if;
   end Check;
   procedure Allowed (Subject : Subject_ID; H : Handle; Operation : A.Operation; Expected : Result) is
   begin
      Resolve (Table, Authority, Subject, H, Operation, Read_ID, Status);
      Check (Status = Expected);
      Check (Read_ID = (if Expected = Resolved then ID else No_Collection));
   end Allowed;
begin
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, Schema, Contract, Good); Check (Good);
   CCL.Objects.Bind (Types, CCL.Types.Boolean_Type, Schema, Other_Contract, Good); Check (Good);
   Register (Table, "org.cubit.settings", Unbound, ID, Status); Check (Status = Invalid_Definition);
   Register (Table, "org..settings", Contract, ID, Status); Check (Status = Invalid_Definition);
   Check_Registration (Table, "org.cubit.settings", Contract, ID, Status);
   Check (Status = Registered and ID /= 0);
   Describe (Table, ID, Name, Length, Retrieved, Good);
   Check (not Good); -- Admission reserves/publishes nothing before durable I/O.
   Register (Table, "org.cubit.settings", Contract, ID, Status); Check (Status = Registered and ID /= 0);
   Register (Table, "org.cubit.settings", Contract, Read_ID, Status);
   Check (Status = Already_Registered and Read_ID = ID);
   Register (Table, "org.cubit.settings", Other_Contract, Read_ID, Status);
   Check (Status = Schema_Conflict and Read_ID = 0);
   Register (Table, "org.cubit.other", Other_Contract, Read_ID, Status);
   Check (Status = Schema_Conflict and Read_ID = 0);
   Describe (Table, ID, Name, Length, Retrieved, Good);
   Check (Good and Name (1 .. Length) = "org.cubit.settings" and Retrieved = Contract);
   Open (Table, Authority, PID (42), "org.cubit.settings", 0, A.Read_Write, Schema, Token, Status);
   Check (Status = Denied and Token = 0);
   A.Append (Rules, "org.cubit", A.Read_Write, Good); Check (Good);
   A.Append (Other_Rules, "org.other", A.Read_Write, Good); Check (Good);
   A.Append (Global, "", A.Read_Only, Good); Check (Good);
   A.Install (Authority, PID (42), Rules, Installed); Check (Installed = A.Installed);
   A.Install (Authority, PID (43), Rules, Installed); Check (Installed = A.Installed);
   Open (Table, Authority, PID (42), "org.cubit.settings", 0, A.Read_Write, Schema, Token, Status);
   Check (Status = Opened and Token /= 0);
   Open (Table, Authority, PID (43), "org.cubit.settings", 0, A.Read_Only, Schema, Other, Status);
   Check (Status = Opened and Other /= Token);
   Allowed (PID (42), Token, A.Read_Config, Resolved);
   Allowed (PID (42), Token, A.Write_Config, Resolved);
   Allowed (PID (43), Token, A.Read_Config, Denied);
   Allowed (PID (42), Other, A.Read_Config, Denied);
   Allowed (PID (43), Other, A.Write_Config, Denied);
   Allowed (PID (43), Other, A.Read_Config, Resolved);
   Allowed (PID (0), 0, A.Read_Config, Denied);
   Allowed (PID (42), Number'Last, A.Read_Config, Denied);
   Close (Table, PID (43), Token, Status); Check (Status = Denied);
   Allowed (PID (42), Token, A.Write_Config, Resolved);
   Open (Table, Authority, PID (42), "org.cubit.settings", 1, A.Read_Only, Schema, Fresh, Status);
   Check (Status = Unsupported_Context and Fresh = 0);
   Open (Table, Authority, PID (42), "org.cubit.settings", 0, [others => False], Schema, Fresh, Status);
   Check (Status = Denied and Fresh = 0);
   Open (Table, Authority, PID (42), "org.cubit.settings", 0, A.Read_Only, [others => 9], Fresh, Status);
   Check (Status = Schema_Conflict and Fresh = 0);
   Open (Table, Authority, PID (42), "org.cubit.missing", 0, A.Read_Only, Schema, Fresh, Status);
   Check (Status = Missing and Fresh = 0);
   Open (Table, Authority, PID (42), "org.other.missing", 0, A.Read_Only, Schema, Fresh, Status);
   Check (Status = Denied and Fresh = 0);
   Open (Table, Authority, PID (42), "org.cubit2.settings", 0, A.Read_Only, Schema, Fresh, Status);
   Check (Status = Denied);
   A.Revoke (Authority, PID (42));
   Allowed (PID (42), Token, A.Read_Config, Denied);
   Allowed (PID (43), Other, A.Read_Config, Resolved);
   A.Install (Authority, PID (42), Rules, Installed); Check (Installed = A.Installed);
   Allowed (PID (42), Token, A.Read_Config, Denied);
   Open (Table, Authority, PID (42), "org.cubit.settings", 0, A.Read_Write, Schema, Fresh, Status);
   Check (Status = Opened and Fresh > Token);
   Allowed (PID (42), Fresh, A.Write_Config, Resolved);
   A.Install (Authority, PID (42), Other_Rules, Installed); Check (Installed = A.Installed);
   Allowed (PID (42), Fresh, A.Read_Config, Denied);
   A.Install (Authority, PID (42), Rules, Installed); Check (Installed = A.Installed);
   Allowed (PID (42), Fresh, A.Read_Config, Denied);
   Close (Table, PID (42), Fresh, Status); Check (Status = Closed);
   Close (Table, PID (42), Fresh, Status); Check (Status = Denied);
   Revoke_Subject (Table, PID (42));
   Allowed (PID (43), Other, A.Read_Config, Resolved);
   Allowed (PID (42), Token, A.Read_Config, Denied);
   Close (Table, PID (43), Other, Status); Check (Status = Closed);
   A.Install (Authority, PID (44), Global, Installed); Check (Installed = A.Installed);
   Open (Table, Authority, PID (44), "org.cubit.settings", 0, A.Read_Write, Schema, Read_Only, Status);
   Check (Status = Denied);
   Open (Table, Authority, PID (44), "org.cubit.settings", 0, A.Read_Only, Schema, Read_Only, Status);
   Check (Status = Opened);
   Allowed (PID (44), Read_Only, A.Read_Config, Resolved);
   Allowed (PID (44), Read_Only, A.Write_Config, Denied);
   for I in 2 .. Maximum_Handles loop
      Open (Table, Authority, PID (42), "org.cubit.settings", 0, A.Read_Only, Schema, Fresh, Status);
      Check (Status = Opened);
   end loop;
   Open (Table, Authority, PID (42), "org.cubit.settings", 0, A.Read_Only, Schema, Token, Status);
   Check (Status = Capacity_Exceeded and Token = 0);
   Close (Table, PID (42), Fresh, Status); Check (Status = Closed);
   Open (Table, Authority, PID (42), "org.cubit.settings", 0, A.Read_Only, Schema, Token, Status);
   Check (Status = Opened and Token > Fresh);
   Allowed (PID (42), Fresh, A.Read_Config, Denied);
   Allowed (PID (42), Token, A.Read_Config, Resolved);
   for I in 2 .. Maximum_Collections loop
      declare
         Suffix : constant String := I'Image;
      begin
         Register (Table, "org.cubit.collection" & Suffix (2 .. Suffix'Last), Contract, Read_ID, Status);
         Check (Status = Registered);
      end;
   end loop;
   Register (Table, "org.cubit.overflow", Contract, Read_ID, Status);
   Check (Status = Capacity_Exceeded and Read_ID = 0);
   Check_Registration (Table, "org.cubit.overflow", Contract, Read_ID, Status);
   Check (Status = Capacity_Exceeded and Read_ID = 0);
   Check_Registration (Table, "org.cubit.settings", Contract, Read_ID, Status);
   Check (Status = Already_Registered and Read_ID = ID);
   Register (Table, "org.cubit.settings", Contract, Read_ID, Status);
   Check (Status = Already_Registered and Read_ID = ID);
   declare
      use CCL.Types;
      Other_Types : Registry;
      Ref : Type_Reference;
      Defined_As : Definition_Result;
      Equivalent : CCL.Objects.Binding;
   begin
      Define (Other_Types, (Identifier => Named ("Unrelated"), Form => Product, others => <>), Ref, Defined_As);
      Check (Defined_As = Defined);
      CCL.Objects.Bind (Other_Types, Integer_Type, Schema, Equivalent, Good); Check (Good);
      Check (Equivalent /= Contract);
      Register (Table, "org.cubit.settings", Equivalent, Read_ID, Status);
      Check (Status = Already_Registered and Read_ID = ID);
      Describe (Table, ID, Name, Length, Retrieved, Good);
      Check (Good and Retrieved = Contract); -- Do not replace the first contract.
   end;
   Ada.Text_IO.Put_Line ("Typed Config collection handles: PASS" & Checks'Image & " checks");
end Collection_Tests;
