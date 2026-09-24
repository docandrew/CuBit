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
   package Objects is new Shared_Objects
     (Capacity => 4, Object_Key => Object_Identity,
      Empty_Key => (Volume_Index'First, 0),
      Object_Value => Natural, Empty_Value => 0);
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
                (Endpoint_Slot (I + 20), 5, 8), Volume, Result);
      pragma Assert (Result = Registered and Volume = I);
   end loop;
   Before := List;
   Register (List, "overflow", (62, 0, 8), Volume, Result);
   pragma Assert (Result = List_Full and Volume = 0 and List = Before);
   pragma Assert (Name (List, Second) = "nvme:0" and Binding (List, Second).Endpoint = 11);
   Resolve ("@nvme:1/still-here", Known_Volume, Third, "still-here");
   Put_Line ("VOLUME-LIST-CHECK: PASS");
end Main;
