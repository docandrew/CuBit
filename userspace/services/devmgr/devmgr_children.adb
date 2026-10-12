pragma Ada_2022;
with CuBit.Process_Events;

package body Devmgr_Children is
   type Entry_Record is record
      Child  : Process_ID := No_Process;
      Name   : Child_Name := [others => ' '];
      Length : Natural range 0 .. Name_Bytes := 0;
   end record;
   Table : array (1 .. Capacity) of Entry_Record;
   Overflow : Natural := 0;

   procedure Remember (Child : Process_ID; Name : String) is
      Length : constant Natural := Natural'Min (Name'Length, Name_Bytes);
   begin
      if Child = No_Process then
         return;
      end if;
      for E of Table loop
         if E.Child = No_Process then
            E.Child := Child;
            E.Name := [others => ' '];
            E.Name (1 .. Length) := Name (Name'First .. Name'First + Length - 1);
            E.Length := Length;
            return;
         end if;
      end loop;
      if Overflow < Natural'Last then
         Overflow := Overflow + 1;
      end if;
   end Remember;

   procedure Take_Next_Exit
     (Child : out Process_ID; Name : out Child_Name; Length : out Natural;
      Report : out CuBit.Child_Exits.Report; Found : out Boolean) is
   begin
      Child := No_Process;
      Name := [others => ' '];
      Length := 0;
      Report := (others => <>);
      Found := False;
      for E of Table loop
         if E.Child /= No_Process then
            CuBit.Process_Events.Take_Exit (E.Child, Found, Report);
            if Found then
               Child := E.Child;
               Name := E.Name;
               Length := E.Length;
               E := (others => <>);
               return;
            end if;
         end if;
      end loop;
   end Take_Next_Exit;

   function Unnamed return Natural is (Overflow);
end Devmgr_Children;
