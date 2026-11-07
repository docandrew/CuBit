with Interfaces; use Interfaces;

--  Internal block-volume names and bindings, not client authority.
--  Append-only for one filesystem-service lifetime: no replacement or reuse.
--  Generic over the endpoint type: natively CuBit.Handles.Endpoint
--  (Volume_List), a number in the hosted tests.
generic
   type Endpoint_Type is private;
   No_Endpoint : Endpoint_Type;
   with function Valid (Item : Endpoint_Type) return Boolean;
package Volume_Lists with SPARK_Mode is
   Maximum_Volumes : constant := 16;
   Maximum_Name_Bytes : constant := 48;
   subtype Volume_Reference is Natural range 0 .. Maximum_Volumes;
   subtype Volume_Index is Volume_Reference range 1 .. Maximum_Volumes;
   No_Volume : constant Volume_Reference := 0;
   subtype Transfer_Page_Count is Positive range 1 .. 4096;

   type Device_Binding is record
      --  The block driver's endpoint: null until devmgr attaches it
      --  (Fs_Attachments) for a driver that starts after the filesystem.
      Endpoint : Endpoint_Type := No_Endpoint;
      Ready_Role : Unsigned_64 := 0; -- bootstrap readiness hint, not authority
      Transfer_Pages : Transfer_Page_Count;
   end record;
   type State is private;
   type Registration_Result is
     (Registered, Invalid_Name, Name_In_Use, Endpoint_In_Use, List_Full);
   --  The read-only stores that are not block volumes have fixed names:
   --  "@boot" is the bootstrap archive, "@cd:0" the optical disc's apps/
   --  tree (the part the service mounts). Names
   --  without a volume still search both and then every volume, for the
   --  services that use them; the libc always names a volume.
   Boot_Archive_Name : constant String := "boot";
   Optical_Name      : constant String := "cd:0";
   --  "@system" is the system volume (docs/security-model.md, "Aliases";
   --  FS-020's @system/Storehouse): the volume the libc's absolute names
   --  ("/toolchain") are on. It is an alias of one registered volume,
   --  never a volume of its own, chosen by the boot's own facts (Rank):
   --  an installed root devmgr attached as the system disk outranks the
   --  live medium's RAM root, which outranks the development disk
   --  (@nvme:0, the headless images' root and the libc's name before the
   --  alias). The choice only rises, and stays for the service's
   --  lifetime: a running program's "/" never moves to another device
   --  under it.
   System_Name : constant String := "system";
   type System_Rank is (No_System, Development_Disk, Live_Root, Installed_Root);
   type Path_Selection is
     (Unqualified, Known_Volume, Boot_Archive, Optical_Volume, Unknown_Volume,
      Invalid_Path);

   function Count (List : State) return Volume_Reference;
   function Name (List : State; Volume : Volume_Index) return String
     with Pre => Volume <= Count (List);
   function Binding (List : State; Volume : Volume_Index) return Device_Binding
     with Pre => Volume <= Count (List);
   --  The fixed store names and the alias are never registered as volumes.
   procedure Register
     (List : in out State; Name : String; Device : Device_Binding;
      Volume : out Volume_Reference; Result : out Registration_Result)
     with Post => (if Name = Boot_Archive_Name or else Name = Optical_Name
                     or else Name = System_Name
                   then Result = Invalid_Name);

   --  The system volume (No_Volume until one is chosen) and its rank.
   function System_Volume (List : State) return Volume_Reference
     with Post => System_Volume'Result <= Count (List);
   function System_Choice (List : State) return System_Rank;
   --  Make Volume the system volume if Rank is above the current choice's;
   --  otherwise nothing changes.
   procedure Choose_System (List : in out State; Volume : Volume_Index; Rank : System_Rank)
     with Pre  => Volume <= Count (List) and then Rank /= No_System,
          Post => Count (List) = Count (List'Old)
                  and then (if Rank > System_Choice (List'Old)
                            then System_Volume (List) = Volume and then System_Choice (List) = Rank
                            else System_Volume (List) = System_Volume (List'Old)
                                 and then System_Choice (List) = System_Choice (List'Old));

   --  The same name spelled the other way round: "@system/x" as the
   --  system volume's own name ("@nvme:0/x"), or that name as "@system/x".
   --  Found is False for every other name, and while no system volume is
   --  chosen. Access scopes written either way then cover a name written
   --  either way (Fs_Server.Access_Control).
   procedure Other_Spelling
     (List : State; Path : String; Result : out String; Length : out Natural;
      Found : out Boolean)
     with Pre  => Path'Last < Positive'Last and then Result'First = 1,
          Post => Length <= Result'Length and then (if not Found then Length = 0);
   --  Give a registered volume its driver's endpoint, once. Attached is
   --  False when the volume already has one or another volume uses it.
   procedure Attach
     (List : in out State; Volume : Volume_Index; Endpoint : Endpoint_Type;
      Attached : out Boolean)
     with Pre => Volume <= Count (List);
   --  The volume registered under Name, or No_Volume.
   function Find (List : State; Name : String) return Volume_Reference
     with Post => Find'Result <= Count (List);
   --  Volume is No_Volume unless Selection is Known_Volume.
   procedure Select_Path
     (List : State; Path : String; Selection : out Path_Selection;
      Volume : out Volume_Reference; Relative_First : out Integer)
     with Pre  => Path'Last < Positive'Last,
          Post => (if Selection /= Known_Volume then Volume = No_Volume);
   --  Relative_First uses the index base type: a null String can have bounds
   --  outside Positive. Nonempty qualified names always return a valid suffix.

private
   type Entry_Record is record
      Text : String (1 .. Maximum_Name_Bytes) := [others => ' '];
      Length : Natural range 0 .. Maximum_Name_Bytes := 0;
      Device : Device_Binding :=
        (Endpoint => No_Endpoint, Ready_Role => 0, Transfer_Pages => 1);
   end record;
   type Entries is array (Volume_Index) of Entry_Record;
   type State is record
      Used : Volume_Reference := 0;
      Items : Entries;
      System : Volume_Reference := No_Volume;
      Rank : System_Rank := No_System;
   end record
     with Type_Invariant => System <= Used and then (System = No_Volume) = (Rank = No_System);
   function Count (List : State) return Volume_Reference is (List.Used);
   function System_Volume (List : State) return Volume_Reference is (List.System);
   function System_Choice (List : State) return System_Rank is (List.Rank);
end Volume_Lists;
