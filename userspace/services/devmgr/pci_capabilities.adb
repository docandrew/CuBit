pragma Ada_2022;
package body PCI_Capabilities with SPARK_Mode => On is

   package body Walks is

      subtype Entry_Slot is Natural range 0 .. Maximum_Entries - 1;
      type Visit_Set is array (Entry_Slot) of Boolean;

      function Control_Word (At_Entry : Entry_Reference) return Unsigned_16 is
        (Unsigned_16 (Read (At_Entry + 2)) or Shift_Left (Unsigned_16 (Read (At_Entry + 3)), 8))
      with Pre => At_Entry in First_Entry .. Last_Entry;

      --  One walk: the first entry with Want, and the interrupt facts.
      procedure Walk (Want : Unsigned_8; Found : out Entry_Reference; Facts : out Interrupt_Facts)
      with Post =>
        (if Found /= No_Entry then
           Found in First_Entry .. Last_Entry
           and then Found mod Entry_Stride = 0
           and then Read (Found) = Want)
        and then
        (if Facts.MSI = No_Entry then Facts.MSI_Vectors = 0
         else Read (Facts.MSI) = MSI_ID
              and then Facts.MSI in First_Entry .. Last_Entry
              and then Fits (Facts.MSI, MSI_Minimum_Bytes)
              and then Facts.MSI_Vectors >= 1)
        and then
        (if Facts.MSIX = No_Entry then Facts.MSIX_Vectors = 0
         else Read (Facts.MSIX) = MSIX_ID
              and then Facts.MSIX in First_Entry .. Last_Entry
              and then Fits (Facts.MSIX, MSIX_Structure_Bytes)
              and then Facts.MSIX_Vectors >= 1)
        and then
        (if Facts.Status = No_List then Facts.MSI = No_Entry and then Facts.MSIX = No_Entry)
      is
         Visited : Visit_Set := [others => False];
         Pointer : Unsigned_8;
         Here    : Entry_Reference;
         ID      : Unsigned_8;
      begin
         Found := No_Entry;
         Facts := (others => <>);
         if (Read (Status_Offset) and Status_Capabilities_List) = 0 then
            return;
         end if;
         Pointer := Read (Capabilities_Pointer_Offset) and Pointer_Mask;
         if Pointer = 0 then
            return;
         end if;
         --  Each pass visits a new entry or ends the walk; there are
         --  Maximum_Entries positions, so the last pass always ends it.
         for Pass in 0 .. Maximum_Entries loop
            pragma Loop_Invariant (Facts.Status = No_List);
            pragma Loop_Invariant
              (if Found /= No_Entry then
                 Found in First_Entry .. Last_Entry
                 and then Found mod Entry_Stride = 0
                 and then Read (Found) = Want);
            pragma Loop_Invariant
              (if Facts.MSI = No_Entry then Facts.MSI_Vectors = 0
               else Read (Facts.MSI) = MSI_ID
                    and then Facts.MSI in First_Entry .. Last_Entry
                    and then Fits (Facts.MSI, MSI_Minimum_Bytes)
                    and then Facts.MSI_Vectors >= 1);
            pragma Loop_Invariant
              (if Facts.MSIX = No_Entry then Facts.MSIX_Vectors = 0
               else Read (Facts.MSIX) = MSIX_ID
                    and then Facts.MSIX in First_Entry .. Last_Entry
                    and then Fits (Facts.MSIX, MSIX_Structure_Bytes)
                    and then Facts.MSIX_Vectors >= 1);
            pragma Loop_Invariant ((Pointer and not Pointer_Mask) = 0);
            if Pointer = 0 then
               Facts.Status := Complete;
               return;
            elsif Natural (Pointer) < First_Entry then
               Facts.Status := Truncated;
               return;
            end if;
            Here := Natural (Pointer);
            pragma Assert (Here mod Entry_Stride = 0);
            if Visited ((Here - First_Entry) / Entry_Stride) then
               Facts.Status := Looped;
               return;
            end if;
            Visited ((Here - First_Entry) / Entry_Stride) := True;
            ID := Read (Here);
            if Found = No_Entry and then ID = Want then
               Found := Here;
            end if;
            if ID = MSI_ID and then Facts.MSI = No_Entry then
               if Fits (Here, MSI_Minimum_Bytes) then
                  Facts.MSI := Here;
                  Facts.MSI_Vectors := MSI_Vectors_Of (Control_Word (Here));
               else
                  Facts.Status := Truncated;
                  return;
               end if;
            elsif ID = MSIX_ID and then Facts.MSIX = No_Entry then
               if Fits (Here, MSIX_Structure_Bytes) then
                  Facts.MSIX := Here;
                  Facts.MSIX_Vectors := PCI_Message_Interrupts.Table_Size (Control_Word (Here));
               else
                  Facts.Status := Truncated;
                  return;
               end if;
            end if;
            Pointer := Read (Here + Next_Pointer_Offset) and Pointer_Mask;
         end loop;
         --  Unreachable (every position visited, so the next is a repeat),
         --  but a walk that did not end is not Complete.
         Facts.Status := Looped;
      end Walk;

      pragma Warnings (GNATprove, Off, "*set by ""Walk"" but not used after the call*",
                       Reason => "Find and Interrupts each keep one result of the walk");
      function Find (ID : Unsigned_8) return Entry_Reference is
         Found : Entry_Reference;
         Facts : Interrupt_Facts;
      begin
         Walk (ID, Found, Facts);
         return Found;
      end Find;

      function Interrupts return Interrupt_Facts is
         Found : Entry_Reference;
         Facts : Interrupt_Facts;
      begin
         Walk (MSI_ID, Found, Facts);
         return Facts;
      end Interrupts;
      pragma Warnings (GNATprove, On, "*set by ""Walk"" but not used after the call*");
   end Walks;
end PCI_Capabilities;
