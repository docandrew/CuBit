------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Net_Link;

package body Desktop_Network_Rows with SPARK_Mode is
   use type CuBit.Net_Link.Link_Kind;

   function Build (Net : NMR.Report; Have_Net, Wifi_Shown : Boolean;
                   Networks : DNM.Network_Count; Passphrase_For : Natural;
                   Editor_For : NMR.Record_Count) return Layout is
      L : Layout;
      Wired_Listed : Natural := 0;
      Editor_Listed : Boolean := False;

      procedure Add (K : Row_Kind; Index : Natural) is
      begin
         if L.Count < Maximum_Rows then
            L.Count := L.Count + 1;
            L.Rows (L.Count) := (Kind => K, Index => Index);
         end if;
      end Add;

      procedure Add_Editor (Index : Natural) is
      begin
         if not Editor_Listed and then Index /= 0 and then Index = Editor_For then
            Editor_Listed := True;
            for K in Editor_Row loop
               Add (K, Index);
            end loop;
         end if;
      end Add_Editor;

      Wireless : constant Natural := (if Have_Net then DNM.Interface_Of (Net, CuBit.Net_Link.Wireless) else 0);
   begin
      if Have_Net then
         for I in 1 .. Net.Count loop
            if Net.Interfaces (I).Kind = CuBit.Net_Link.Ethernet and then Wired_Listed < Visible_Wired then
               Wired_Listed := Wired_Listed + 1;
               Add (Wired_Row, I);
               Add_Editor (I);
            end if;
         end loop;
      end if;
      if Wifi_Shown then
         Add (Wifi_Row, Wireless);
         Add_Editor (Wireless);
         for N in 1 .. Natural'Min (Networks, Visible_Networks) loop
            Add (Network_Row, N);
            if Passphrase_For = N then
               Add (Passphrase_Row, N);
            end if;
         end loop;
      end if;
      return L;
   end Build;

   function Total_Height (L : Layout) return Natural is
      H : Natural := 0;
   begin
      for I in 1 .. L.Count loop
         pragma Loop_Invariant (H <= (I - 1) * CCL_Height);
         H := H + Height_Of (L.Rows (I).Kind);
      end loop;
      return H;
   end Total_Height;
end Desktop_Network_Rows;
