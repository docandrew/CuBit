pragma Ada_2022;
package body ACPI_Backend_Windows with SPARK_Mode is
   function From_Memory_Pages
     (ID : Window_ID; First_Page, Page_Count : Unsigned_64;
      Readable, Writable : Boolean; Delegable : Boolean := False) return Window_Result
   is
   begin
      if not Memory_Description_Valid (ID, First_Page, Page_Count, Readable, Writable)
      then return (Allowed => False); end if;
      return (Allowed => True,
        Value => (ID, P.Memory_Space, First_Page * Map_Page_Bytes,
          Page_Count * Map_Page_Bytes, Readable, Writable, Delegable));
   end From_Memory_Pages;
   function From_IO_Ports
     (ID : Window_ID; First_Port, Count : Unsigned_64;
      Readable, Writable : Boolean; Delegable : Boolean := False) return Window_Result is
   begin
      if not IO_Description_Valid (ID, First_Port, Count, Readable, Writable)
      then return (Allowed => False); end if;
      return (Allowed => True,
        Value => (ID, P.IO_Space, First_Port, Count, Readable, Writable, Delegable));
   end From_IO_Ports;
   function Check
     (Window : Granted_Window; Item : P.Configuration;
      Writes : Write_Safety) return Outcome is
   begin
      if not Valid (Window) then return Invalid_Window; end if;
      if not Named_Record_Valid (Item) then return Invalid_Record; end if;
      if not Contains (Window, Item) or else not Register_Contained (Window, Item)
      then return Outside_Window; end if;
      if not Rights_Fit (Window, Item) then return Rights_Unavailable; end if;
      if Item.Writable and then Writes /= Exact_Writes_Admitted then
         return Writes_Not_Admitted;
      end if;
      return Admitted;
   end Check;
   procedure Install
     (Region : in out P.State; Window : Granted_Window;
      Item : P.Configuration; Writes : Write_Safety; Result : out Outcome)
   is
      Accepted : Boolean;
   begin
      Result := Check (Window, Item, Writes);
      if Result /= Admitted then return; end if;
      P.Install (Region, Item, Accepted);
      if not Accepted then Result := Slot_Unavailable; end if;
   end Install;
end ACPI_Backend_Windows;
