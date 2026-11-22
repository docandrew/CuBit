pragma Ada_2022;

package body Devmgr_Data_Windows with SPARK_Mode is

   --  The first root of Kind touching the envelope, or 0.
   function Touching (Envelope : Page_Range; Firmware : Root_Array; Kind : Root_Kind) return Natural
   with Pre  => Valid_Range (Envelope) and then Roots_Valid (Firmware),
        Post => (if Touching'Result = 0 then
                   (for all I in Firmware'Range =>
                      (if Firmware (I).Kind = Kind then not Touches (Envelope, Firmware (I).Pages)))
                 else Touching'Result in Firmware'Range
                      and then Firmware (Touching'Result).Kind = Kind
                      and then Touches (Envelope, Firmware (Touching'Result).Pages))
   is
   begin
      for I in Firmware'Range loop
         if Firmware (I).Kind = Kind and then Touches (Envelope, Firmware (I).Pages) then
            return I;
         end if;
         pragma Loop_Invariant
           (for all J in Firmware'First .. I =>
              (if Firmware (J).Kind = Kind then not Touches (Envelope, Firmware (J).Pages)));
      end loop;
      return 0;
   end Touching;

   --  The first root of Kind holding the whole envelope, or 0.
   function Holding (Envelope : Page_Range; Firmware : Root_Array; Kind : Root_Kind) return Natural
   with Pre  => Valid_Range (Envelope) and then Roots_Valid (Firmware),
        Post => (if Holding'Result /= 0 then
                   Holding'Result in Firmware'Range
                   and then Firmware (Holding'Result).Kind = Kind
                   and then Inside (Envelope, Firmware (Holding'Result).Pages))
   is
   begin
      for I in Firmware'Range loop
         if Firmware (I).Kind = Kind and then Inside (Envelope, Firmware (I).Pages) then
            return I;
         end if;
      end loop;
      return 0;
   end Holding;

   --  Every page of the envelope is in some root, device or firmware: from
   --  the first page, jump past whichever root holds it, until past the
   --  envelope (yes) or at a page no root holds (no). A root is passed at
   --  most once, so the roots bound the jumps.
   function In_Some_Root
     (Envelope : Page_Range; Firmware : Root_Array; Devices : Range_Array) return Boolean
   with Pre => Valid_Range (Envelope) and then Roots_Valid (Firmware)
               and then Ranges_Valid (Devices)
   is
      Past : constant Unsigned_64 := Envelope.First_Page + Envelope.Pages;
      P : Unsigned_64 := Envelope.First_Page;
      Next : Unsigned_64;
   begin
      for Round in 0 .. Unsigned_64 (Devices'Length) + Unsigned_64 (Firmware'Length) loop
         pragma Loop_Invariant (P < Past and then P < Page_Limit);
         Next := P;
         for I in Devices'Range loop
            if Devices (I).First_Page <= P and then P - Devices (I).First_Page < Devices (I).Pages then
               Next := Devices (I).First_Page + Devices (I).Pages;
               exit;
            end if;
         end loop;
         if Next = P then
            for I in Firmware'Range loop
               if Firmware (I).Pages.First_Page <= P
                 and then P - Firmware (I).Pages.First_Page < Firmware (I).Pages.Pages
               then
                  Next := Firmware (I).Pages.First_Page + Firmware (I).Pages.Pages;
                  exit;
               end if;
            end loop;
         end if;
         if Next = P then
            return False;    --  a page no root holds
         elsif Next >= Past then
            return True;
         end if;
         P := Next;
      end loop;
      return False;
   end In_Some_Root;

   function Decide
     (Envelope : Page_Range;
      Firmware : Root_Array;
      Devices  : Range_Array;
      ECAM     : Range_Array;
      Device_Claimed : Boolean) return Decision
   is
      Index : Natural;
   begin
      if not Valid_Range (Envelope) then
         return (Answer => FDX.Malformed, Root => 0);
      elsif Touching (Envelope, Firmware, Control_Root) /= 0 then
         return (Answer => FDX.Overlaps_Control, Root => 0);
      elsif Touching (Envelope, Firmware, PCI_Config_Root) /= 0
        or else Touches_Any (Envelope, ECAM)
      then
         return (Answer => FDX.Overlaps_PCI_Config, Root => 0);
      elsif Device_Claimed then
         return (Answer => FDX.Overlaps_Device, Root => 0);
      end if;
      Index := Holding (Envelope, Firmware, NVS_Root);
      if Index /= 0 then
         return (Answer => FDX.Reused_NVS, Root => Index);
      elsif Touching (Envelope, Firmware, NVS_Root) /= 0 then
         return (Answer => FDX.No_Root, Root => 0);
      end if;
      Index := Holding (Envelope, Firmware, Data_Root);
      if Index /= 0 then
         return (Answer => FDX.Granted_Data, Root => Index);
      elsif not In_Some_Root (Envelope, Firmware, Devices) then
         return (Answer => FDX.Not_Firmware_Class, Root => 0);
      end if;
      return (Answer => FDX.No_Root, Root => 0);
   end Decide;

end Devmgr_Data_Windows;
