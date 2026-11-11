pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Physical_Ranges;
with Firmware_Tables.MCFG;
procedure Catalog_Tests is
   package P renames Physical_Ranges;
   package MC renames Firmware_Tables.MCFG;
   use type P.Firmware_Kind, P.Firmware_Phase;
   Base : constant Unsigned_64 := 16#E0000000#;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks+1;
      if not Good then raise Program_Error with "kernel PCI catalog mismatch"; end if;
   end Check;
   procedure Install (M : in out P.Map) is
   begin
      P.Add_PCI_Config (M,(Base,0,0,3,0));
   end Install;
   procedure Count_Roots (M : P.Map; PCI, NVS, Control : Natural; Omitted : Natural := 0) is
      Cursor : P.Firmware_Cursor := P.First_Firmware_Cursor (M);
      Found : Boolean;
      Run : P.Firmware_Run;
      Left : Natural := 0;
      PCI_Count, NVS_Count, Control_Count : Natural := 0;
      Previous : Unsigned_64 := 0;
   begin
      for Turn in 1 .. 20 loop
         P.Next_Firmware_Run (M,Cursor,Found,Run,Left);
         exit when not Found;
         case Run.Kind is
            when P.PCI_Config_Memory =>
               PCI_Count := PCI_Count+1;
               Check (Run.Last-Run.First+1 = P.PCI_Bus_Bytes);
               Check (Run.First >= Previous);
               Previous := Run.Last+1;
            when P.NVS_Memory => NVS_Count := NVS_Count+1;
            when P.Control_Memory => Control_Count := Control_Count+1;
            when P.Data_Memory => null;
         end case;
      end loop;
      Check (Cursor.Phase = P.Finished);
      Check (PCI_Count=PCI and then NVS_Count=NVS and then Control_Count=Control);
      Check (Left = Omitted);
   end Count_Roots;
   M : P.Map := P.Empty_Map;
begin
   Install (M);
   Check (M.PCI_Used=1 and then P.Well_Formed (M));
   Check (not P.Firmware_Bytes (M,Base,P.PCI_Bus_Bytes,P.PCI_Config_Memory));
   P.Seal (M); Count_Roots (M,4,0,0);
   for Bus in 0 .. 3 loop
      Check (P.Firmware_Bytes (M,Base+Unsigned_64(Bus)*P.PCI_Bus_Bytes,
        P.PCI_Bus_Bytes,P.PCI_Config_Memory));
   end loop;
   Check (not P.Firmware_Bytes (M,Base+4*P.PCI_Bus_Bytes,P.Page_Bytes,P.PCI_Config_Memory));
   Check (not P.Firmware_Bytes (M,Base-P.Page_Bytes,P.Page_Bytes,P.PCI_Config_Memory));
   M:=P.Empty_Map;
   P.Add_PCI_Config (M,(Base,2,3,4,0));
   P.Add_PCI_Config (M,(Base+8*P.PCI_Bus_Bytes,3,0,1,0));
   P.Seal (M);Count_Roots(M,4,0,0);
   Check (not P.In_PCI_Config (M,Base/P.Page_Bytes));
   Check (P.In_PCI_Config (M,(Base+3*P.PCI_Bus_Bytes)/P.Page_Bytes));
   M:=P.Empty_Map;Install(M);
   P.Add (M,Base+P.PCI_Bus_Bytes+10*P.Page_Bytes,
     Base+P.PCI_Bus_Bytes+11*P.Page_Bytes-1,P.ACPI_NVS);
   P.Seal(M);Count_Roots(M,3,1,0,1);
   Check (not P.Firmware_Bytes(M,Base+P.PCI_Bus_Bytes,P.PCI_Bus_Bytes,P.PCI_Config_Memory));
   M:=P.Empty_Map;Install(M);
   P.Add (M,Base+P.PCI_Bus_Bytes+10*P.Page_Bytes,
     Base+P.PCI_Bus_Bytes+11*P.Page_Bytes-1,P.Firmware_Reserved);
   P.Set_Control (M,Base+P.PCI_Bus_Bytes+10*P.Page_Bytes);
   P.Seal(M);Count_Roots(M,3,0,1,1);
   Check (not P.Firmware_Bytes(M,Base+P.PCI_Bus_Bytes,P.PCI_Bus_Bytes,P.PCI_Config_Memory));
   M:=P.Empty_Map;Install(M);
   P.Add(M,Base+P.PCI_Bus_Bytes,Base+P.PCI_Bus_Bytes+P.Page_Bytes-1,P.Usable_RAM);
   P.Seal(M);Count_Roots(M,3,0,0,1);
   M:=P.Empty_Map;Install(M);
   P.Add(M,Base-P.Page_Bytes,Base+4*P.PCI_Bus_Bytes+P.Page_Bytes-1,P.Firmware_Reserved);
   P.Seal(M);
   Check (P.Data_Page(M,Base/P.Page_Bytes-1));
   Check (not P.Data_Page(M,Base/P.Page_Bytes));
   Check (not P.Data_Page(M,(Base+4*P.PCI_Bus_Bytes)/P.Page_Bytes-1));
   Check (P.Data_Page(M,(Base+4*P.PCI_Bus_Bytes)/P.Page_Bytes));
   Count_Roots(M,4,0,0);
   M:=P.Empty_Map;Install(M);
   P.Add_PCI_Config (M,(Base,0,0,3,0));
   Check (not M.Complete and then M.PCI_Used=1 and then P.Well_Formed(M));
   P.Seal(M);Count_Roots(M,0,0,0);
   M:=P.Empty_Map;Install(M);
   P.Add_PCI_Config (M,(Base,1,0,3,0));
   Check (not M.Complete and then M.PCI_Used=1);
   M:=P.Empty_Map;P.Add_PCI_Config (M,(Base+1,0,0,3,0));
   Check (not M.Complete and then M.PCI_Used=0);
   M:=P.Empty_Map;
   for I in 1 .. P.Max_PCI_Allocations loop
      P.Add_PCI_Config(M,(Unsigned_64(I)*2*P.PCI_Bus_Bytes,MC.Segment_Number(I),0,0,0));
   end loop;
   Check(M.Complete and then M.PCI_Used=P.Max_PCI_Allocations);
   P.Add_PCI_Config(M,(0,0,0,0,0));
   Check(not M.Complete and then M.PCI_Used=P.Max_PCI_Allocations and then P.Well_Formed(M));
   -- A preceding NVS root advances From into two advertised buses. Each
   -- rejected whole bus must count once; the NVS root must remain available.
   M:=P.Empty_Map;Install(M);
   P.Add(M,Base-P.Page_Bytes,Base+P.PCI_Bus_Bytes+P.Page_Bytes-1,P.ACPI_NVS);
   P.Seal(M);Count_Roots(M,2,1,0,2);
   Check(not P.Firmware_Bytes(M,Base,P.PCI_Bus_Bytes,P.PCI_Config_Memory));
   Check(not P.Firmware_Bytes(M,Base+P.PCI_Bus_Bytes,P.PCI_Bus_Bytes,P.PCI_Config_Memory));
   Ada.Text_IO.Put_Line("Kernel PCI catalog checks" & Checks'Image);
end Catalog_Tests;
