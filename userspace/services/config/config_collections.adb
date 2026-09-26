with Config_Worker_Protocol;

package body Config_Collections with SPARK_Mode is
   use type Number;
   use type CCL.Objects.Schema_Key;
   use type Config_Authority.Rights;

   function Authorized_For
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; Token : Handle; Operation : Config_Authority.Operation;
      ID : Collection_ID) return Boolean is
     (Token /= No_Handle and then Subject /= Config_Authority.No_Subject and then
      (for some H of Object.Handles =>
         H.Token = Token and then H.Owner = Subject and then H.ID = ID and then
         H.Allowed (Operation) and then
         H.Grant_Revision = Config_Authority.Revision (Authority, Subject) and then
         Config_Authority.Allows (Authority, Subject,
           Object.Definitions (H.ID).Name (1 .. Object.Definitions (H.ID).Length), Operation)));

   function Allows
     (Authority : Config_Authority.Authority_State; Subject : Subject_ID;
      Name : String; Requested : Config_Authority.Rights) return Boolean is
     (Subject /= Config_Authority.No_Subject and then
      Requested /= Config_Authority.Rights'[others => False] and then
      (for all Op in Config_Authority.Operation =>
        (not Requested (Op) or else Config_Authority.Allows (Authority, Subject, Name, Op))));

   procedure Check_Registration
     (Object : State; Name : String; Contract : CCL.Objects.Binding;
      ID : out Collection_ID; Status : out Result)
   is
      Free : Collection_ID := No_Collection;
   begin
      ID := No_Collection;
      Status := Invalid_Definition;
      if not Config_Worker_Protocol.Valid_Name (Name) or else
        not CCL.Objects.Is_Bound (Contract)
      then return; end if;
      for I in Registered_ID loop
         if Object.Definitions (I).Length = 0 then
            if Free = No_Collection then Free := I; end if;
         elsif CCL.Objects.Identity (Object.Definitions (I).Contract) = CCL.Objects.Identity (Contract)
           and then not CCL.Objects.Same_Schema (Object.Definitions (I).Contract, Contract)
         then
            --  A schema identity has one meaning throughout this approved
            --  catalog, even when several collections use that identity.
            Status := Schema_Conflict;
            return;
         elsif Object.Definitions (I).Name (1 .. Object.Definitions (I).Length) = Name then
            Status := Schema_Conflict;
            --  Compare the complete nominal root, not local numbering or
            --  unrelated registry entries; key equality alone is insufficient.
            if CCL.Objects.Same_Schema (Object.Definitions (I).Contract, Contract) then
               ID := I; Status := Already_Registered;
            end if;
            return;
         end if;
      end loop;
      Status := Capacity_Exceeded;
      if Free = No_Collection then return; end if;
      ID := Free;
      Status := Registered;
   end Check_Registration;

   procedure Register
     (Object : in out State; Name : String; Contract : CCL.Objects.Binding;
      ID : out Collection_ID; Status : out Result)
   is
   begin
      Check_Registration (Object, Name, Contract, ID, Status);
      if Status = Registered then
         Object.Definitions (ID).Name (1 .. Name'Length) := Name;
         Object.Definitions (ID).Length := Name'Length;
         Object.Definitions (ID).Contract := Contract;
      end if;
   end Register;

   procedure Open
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; Name : String; Context : Number;
      Requested : Config_Authority.Rights; Expected : CCL.Objects.Schema_Key;
      Token : out Handle; Status : out Result)
   is
      ID : Collection_ID := No_Collection;
   begin
      Token := No_Handle;
      Status := Denied;
      --  Authorization precedes lookup: denied callers cannot probe catalog
      --  membership, schemas, or whether the handle table is full.
      if not Allows (Authority, Subject, Name, Requested) then return; end if;
      Status := Unsupported_Context;
      if Context /= Machine_Context then return; end if;
      Status := Missing;
      for I in Registered_ID loop
         if Object.Definitions (I).Length > 0 and then
           Object.Definitions (I).Name (1 .. Object.Definitions (I).Length) = Name
         then ID := I; exit; end if;
      end loop;
      if ID = No_Collection then return; end if;
      Status := Schema_Conflict;
      if CCL.Objects.Identity (Object.Definitions (ID).Contract) /= Expected then return; end if;
      Status := Identity_Exhausted;
      if Object.Last_Token = Number'Last then return; end if;
      Status := Capacity_Exceeded;
      for I in Object.Handles'Range loop
         if Object.Handles (I).Token = No_Handle then
            Object.Last_Token := Object.Last_Token + 1;
            Token := Object.Last_Token;
            Object.Handles (I) :=
              (Token => Token, Owner => Subject,
               Grant_Revision => Config_Authority.Revision (Authority, Subject),
               ID => ID, Allowed => Requested);
            Status := Opened;
            return;
         end if;
      end loop;
   end Open;

   procedure Resolve
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; Token : Handle; Operation : Config_Authority.Operation;
      ID : out Collection_ID; Status : out Result)
   is
   begin
      ID := No_Collection;
      Status := Denied;
      if Token = No_Handle or Subject = Config_Authority.No_Subject then return; end if;
      for H of Object.Handles loop
         if H.Token = Token and then H.Owner = Subject and then H.Allowed (Operation) and then
           H.Grant_Revision = Config_Authority.Revision (Authority, Subject)
         then
            declare
               Item : Definition renames Object.Definitions (H.ID);
            begin
               if Config_Authority.Allows (Authority, Subject, Item.Name (1 .. Item.Length), Operation) then
                  ID := H.ID; Status := Resolved;
               end if;
            end;
            return;
         end if;
      end loop;
   end Resolve;

   procedure Close
     (Object : in out State; Subject : Subject_ID; Token : Handle; Status : out Result) is
   begin
      Status := Denied;
      if Token = No_Handle or Subject = Config_Authority.No_Subject then return; end if;
      for H of Object.Handles loop
         if H.Token = Token and H.Owner = Subject then
            H := (others => <>);
            Status := Closed;
            return;
         end if;
      end loop;
   end Close;

   procedure Revoke_Subject (Object : in out State; Subject : Subject_ID) is
   begin
      for H of Object.Handles loop
         if H.Owner = Subject then H := (others => <>); end if;
      end loop;
   end Revoke_Subject;

   procedure Describe
     (Object : State; ID : Registered_ID; Name : out Collection_Name;
      Length : out Name_Length; Contract : out CCL.Objects.Binding; Found : out Boolean) is
   begin
      Name := [others => Character'Val (0)];
      Length := 0;
      Contract := Object.Definitions (ID).Contract;
      Found := Object.Definitions (ID).Length > 0;
      if Found then
         Length := Object.Definitions (ID).Length;
         Name (1 .. Length) := Object.Definitions (ID).Name (1 .. Length);
      end if;
   end Describe;

   function Schema (Object : State; ID : Registered_ID) return CCL.Objects.Schema_Key is
     (CCL.Objects.Identity (Object.Definitions (ID).Contract));

end Config_Collections;
