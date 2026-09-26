pragma Ada_2022;
with Interfaces;

--  Owned authorization state. Only the trusted IPC administration path may
--  call Install/Revoke; this package does not authenticate message senders.
package Config_Authority with SPARK_Mode, Pure is
   subtype Subject_ID is Interfaces.Unsigned_64;
   No_Subject : constant Subject_ID := 0;
   Maximum_Rules : constant := 16;
   Maximum_Subjects : constant := 32;
   Maximum_Scope : constant := 64;
   type Operation is (Read_Config, Write_Config);
   type Rights is array (Operation) of Boolean;
   Read_Only : constant Rights := [Read_Config => True, Write_Config => False];
   Read_Write : constant Rights := [others => True];
   type Rule_Set is private;
   type Authority_State is private;
   type Install_Result is (Installed, Invalid_Subject, Capacity_Exceeded, Identity_Exhausted);

   procedure Append
     (Rules : in out Rule_Set; Scope : String; Allowed : Rights;
      Accepted : out Boolean)
     with Post => (if not Accepted then Rules = Rules'Old);
   --  Empty Scope explicitly means all keys. An empty rule set grants nothing.
   function Has_Profile (State : Authority_State; Subject : Subject_ID) return Boolean;
   --  Service-lifetime nonreusing revision of an installed grant set. Zero is
   --  absent. Replacement/regrant cannot resurrect handles from an old set.
   function Revision (State : Authority_State; Subject : Subject_ID)
      return Interfaces.Unsigned_64;
   function Allows
     (State : Authority_State; Subject : Subject_ID; Key : String;
      Requested : Operation) return Boolean;
   function Other_Profiles_Unchanged
     (Before, After : Authority_State; Subject : Subject_ID) return Boolean
     with Ghost;
   procedure Install
     (State : in out Authority_State; Subject : Subject_ID; Rules : Rule_Set;
      Result : out Install_Result)
     with Post =>
       (if Result = Installed then Has_Profile (State, Subject)
        else State = State'Old) and then
       Other_Profiles_Unchanged (State'Old, State, Subject);
   procedure Revoke (State : in out Authority_State; Subject : Subject_ID)
     with Post => not Has_Profile (State, Subject) and then
       Other_Profiles_Unchanged (State'Old, State, Subject);
private
   subtype Rule_Count is Natural range 0 .. Maximum_Rules;
   subtype Scope_Length is Natural range 0 .. Maximum_Scope;
   type Rule is record
      Scope : String (1 .. Maximum_Scope) := [others => ' '];
      Length : Scope_Length := 0;
      Allowed : Rights := [others => False];
   end record;
   type Rule_Array is array (Positive range 1 .. Maximum_Rules) of Rule;
   type Rule_Set is record
      Entries : Rule_Array;
      Count : Rule_Count := 0;
   end record;
   type Profile is record
      Subject : Subject_ID := No_Subject;
      Revision : Interfaces.Unsigned_64 := 0;
      Rules : Rule_Set;
   end record;
   type Profile_Array is array (Positive range 1 .. Maximum_Subjects) of Profile;
   type Authority_State is record
      Profiles : Profile_Array;
      Last_Revision : Interfaces.Unsigned_64 := 0;
   end record;
end Config_Authority;
