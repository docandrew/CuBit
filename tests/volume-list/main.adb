with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Volume_List; use Volume_List;
with Shared_Objects;

procedure Main is
   List : State;
   Before : State;
   Volume, First, Second, Third : Volume_Reference;
   Result : Registration_Result;
   Selection : Path_Selection;
   Relative_First : Integer;
   type Object_Identity is record
      Volume : Volume_Index;
      Inode : Unsigned_32;
   end record;
   function Identity_Hash (Key : Object_Identity) return Unsigned_32 is
     (Key.Inode * 16#9E37_79B9# xor Unsigned_32 (Key.Volume));
   package Objects is new Shared_Objects
     (Capacity => 4, Object_Key => Object_Identity,
      Empty_Key => (Volume_Index'First, 0),
      Object_Value => Natural, Empty_Value => 0, Hash => Identity_Hash);
   use type Objects.Attach_Result;
   Opened : Objects.State;
   Attached : Objects.Attach_Result;

   procedure Resolve
     (Path : String; Expected : Path_Selection;
      Identity : Volume_Reference; Relative : String)
   is
   begin
      Select_Path (List, Path, Selection, Volume, Relative_First);
      pragma Assert (Selection = Expected and Volume = Identity);
      if Selection in Known_Volume | Unqualified then
         pragma Assert (Path (Relative_First .. Path'Last) = Relative);
      end if;
   end Resolve;

   procedure Reject_Name (Text : String) is
   begin
      Before := List;
      Register (List, Text, (30, 0, 8), Volume, Result);
      pragma Assert (Result = Invalid_Name and Volume = No_Volume and List = Before);
   end Reject_Name;
begin
   Resolve ("", Unqualified, No_Volume, "");
   Resolve ("config.dat", Unqualified, No_Volume, "config.dat");
   declare
      Empty : String (0 .. -1);
   begin
      --  Null Ada strings can have bounds outside Positive; preserve them.
      Resolve (Empty, Unqualified, No_Volume, "");
   end;
   Resolve ("@absent/file", Unknown_Volume, No_Volume, "");
   Reject_Name ("");
   Reject_Name ("/bad");
   Reject_Name ("two/parts");
   Reject_Name ("@bad");
   Reject_Name (".");
   Reject_Name ("bad" & Character'Val (0));
   Reject_Name ((1 .. Maximum_Name_Bytes + 1 => 'x'));
   Reject_Name (System_Name);
   --  No system volume yet: "@system" names nothing, and nothing is respelled.
   Resolve ("@system/toolchain", Unknown_Volume, No_Volume, "");

   Register (List, "mem:0", (13, 0, 128), First, Result);
   pragma Assert (Result = Registered and First = 1);
   Register (List, "nvme:0", (11, 5, 128), Second, Result);
   pragma Assert (Result = Registered and Second /= First);
   --  Same readiness role and storage type, different endpoint and identity.
   Register (List, "nvme:1", (12, 5, 128), Third, Result);
   pragma Assert (Result = Registered and Third /= Second);
   Before := List;
   Register (List, "nvme:0", (30, 0, 8), Volume, Result);
   pragma Assert (Result = Name_In_Use and Volume = 0 and List = Before);
   Register (List, "alias", (11, 0, 8), Volume, Result);
   pragma Assert (Result = Endpoint_In_Use and Volume = 0 and List = Before);
   pragma Assert (Binding (List, Second).Endpoint = 11);
   pragma Assert (Binding (List, Third).Endpoint = 12);

   Resolve ("@mem:0/work/file", Known_Volume, First, "work/file");
   Resolve ("@nvme:0/", Known_Volume, Second, "");
   Resolve ("@nvme:0", Known_Volume, Second, "");
   Resolve ("@nvme:1/123", Known_Volume, Third, "123");
   Resolve ("@nvme:10/file", Unknown_Volume, No_Volume, "");
   Resolve ("@nvme:/file", Unknown_Volume, No_Volume, "");
   Resolve ("@nvme:0suffix/file", Unknown_Volume, No_Volume, "");
   Resolve ("@", Invalid_Path, No_Volume, "");
   Resolve ("@/file", Invalid_Path, No_Volume, "");
   declare
      Shifted : String (101 .. 111) := "@nvme:1/123";
   begin
      Resolve (Shifted, Known_Volume, Third, "123");
   end;

   --  "@system": the boot's root, by rank; the choice only rises.
   declare
      Other : String (1 .. 64);
      Length : Natural;
      Found : Boolean;
      procedure Spelled (Path, Expected : String) is
      begin
         Other_Spelling (List, Path, Other, Length, Found);
         pragma Assert (Found and then Other (1 .. Length) = Expected);
      end Spelled;
      procedure Not_Spelled (Path : String) is
      begin
         Other_Spelling (List, Path, Other, Length, Found);
         pragma Assert (not Found and Length = 0);
      end Not_Spelled;
   begin
      Not_Spelled ("@system/x");
      Choose_System (List, Second, Development_Disk);
      pragma Assert (System_Volume (List) = Second and System_Choice (List) = Development_Disk);
      Resolve ("@system/toolchain/bin/gcc", Known_Volume, Second, "toolchain/bin/gcc");
      Resolve ("@system", Known_Volume, Second, "");
      Resolve ("@systemx/a", Unknown_Volume, No_Volume, "");
      Spelled ("@system/toolchain", "@nvme:0/toolchain");
      Spelled ("@system", "@nvme:0");
      Spelled ("@system/", "@nvme:0/");
      Spelled ("@nvme:0/storehouse/x", "@system/storehouse/x");
      Spelled ("@nvme:0", "@system");
      Not_Spelled ("@nvme:1/x");
      Not_Spelled ("@nvme:0x/y");
      Not_Spelled ("@systemx/y");
      Not_Spelled ("toolchain/bin/gcc");
      Not_Spelled ("");
      --  Too long for Result: not respelled (the caller denies).
      Other_Spelling (List, "@system/" & (1 .. 60 => 'a'), Other, Length, Found);
      pragma Assert (not Found and Length = 0);
      --  The live RAM root outranks the development disk; a lower rank
      --  afterwards changes nothing; the installed root outranks both.
      Choose_System (List, First, Live_Root);
      pragma Assert (System_Volume (List) = First);
      Spelled ("@system/work", "@mem:0/work");
      Spelled ("@mem:0/work", "@system/work");
      Not_Spelled ("@nvme:0/work");
      Choose_System (List, Second, Development_Disk);
      pragma Assert (System_Volume (List) = First and System_Choice (List) = Live_Root);
      Choose_System (List, Third, Installed_Root);
      pragma Assert (System_Volume (List) = Third and System_Choice (List) = Installed_Root);
      Resolve ("@system/a", Known_Volume, Third, "a");
      Choose_System (List, First, Live_Root);
      pragma Assert (System_Volume (List) = Third);
   end;

   --  Equal inode numbers on different volumes must not share metadata.
   Objects.Attach (Opened, 0, (Second, 42), 100, Attached);
   pragma Assert (Attached = Objects.Created);
   Objects.Attach (Opened, 1, (Third, 42), 200, Attached);
   pragma Assert (Attached = Objects.Created);
   Objects.Attach (Opened, 2, (Second, 42), 999, Attached);
   pragma Assert (Attached = Objects.Shared and Objects.Value (Opened, 2) = 100);
   Objects.Replace (Opened, 0, 300);
   pragma Assert (Objects.Value (Opened, 2) = 300);
   pragma Assert (Objects.Value (Opened, 1) = 200);
   Objects.Detach (Opened, 0);
   Objects.Detach (Opened, 2);
   pragma Assert (Objects.Attached (Opened, 1) and Objects.Value (Opened, 1) = 200);

   for I in Volume_List.Count (List) + 1 .. Maximum_Volumes loop
      Register (List, "data:" & Character'Val (Character'Pos ('a') + I),
                (Unsigned_64 (I + 20), 5, 8), Volume, Result);
      pragma Assert (Result = Registered and Volume = I);
   end loop;
   Before := List;
   Register (List, "overflow", (62, 0, 8), Volume, Result);
   pragma Assert (Result = List_Full and Volume = 0 and List = Before);
   pragma Assert (Name (List, Second) = "nvme:0" and Binding (List, Second).Endpoint = 11);
   Resolve ("@nvme:1/still-here", Known_Volume, Third, "still-here");
   --  CAP-002: drivers that start later are registered without an endpoint
   --  and attached once; a null or reused endpoint is refused.
   declare
      Late : Volume_List.State;
      Ata, Nvme : Volume_Reference;
      Done : Boolean;
   begin
      Register (Late, "ata:0", (0, 2, 8), Ata, Result);
      pragma Assert (Result = Registered);
      Register (Late, "nvme:0", (0, 5, 128), Nvme, Result);
      pragma Assert (Result = Registered and Nvme /= Ata);
      pragma Assert (Find (Late, "ata:0") = Ata and Find (Late, "nvme:0") = Nvme);
      pragma Assert (Find (Late, "cd:0") = No_Volume);
      Attach (Late, Ata, 0, Done);
      pragma Assert (not Done and Binding (Late, Ata).Endpoint = 0);
      Attach (Late, Ata, 40, Done);
      pragma Assert (Done and Binding (Late, Ata).Endpoint = 40);
      Attach (Late, Ata, 41, Done);
      pragma Assert (not Done and Binding (Late, Ata).Endpoint = 40);
      Attach (Late, Nvme, 40, Done);
      pragma Assert (not Done and Binding (Late, Nvme).Endpoint = 0);
      Attach (Late, Nvme, 41, Done);
      pragma Assert (Done and Binding (Late, Nvme).Endpoint = 41);
   end;
   Put_Line ("VOLUME-LIST-CHECK: PASS");
end Main;
