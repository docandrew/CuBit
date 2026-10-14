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
   type Path_Selection is
     (Unqualified, Known_Volume, Boot_Archive, Optical_Volume, Unknown_Volume,
      Invalid_Path);

   function Count (List : State) return Volume_Reference;
   function Name (List : State; Volume : Volume_Index) return String
     with Pre => Volume <= Count (List);
   function Binding (List : State; Volume : Volume_Index) return Device_Binding
     with Pre => Volume <= Count (List);
   --  The fixed store names are never registered as volumes.
   procedure Register
     (List : in out State; Name : String; Device : Device_Binding;
      Volume : out Volume_Reference; Result : out Registration_Result)
     with Post => (if Name = Boot_Archive_Name or else Name = Optical_Name
                   then Result = Invalid_Name);
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
   end record;
   function Count (List : State) return Volume_Reference is (List.Used);
end Volume_Lists;
