pragma Ada_2022;
with CuBit.Channel_Bells;
with ACPI_System_Events;
with ACPI_Native_Log;
with CuBit.Control_Events;
package body ACPI_Native_Events is
   package CV renames CuBit.Control_Events;
   Retained_Stop_Noted : Boolean := False;
   use type ACPI_System_Events.Action;
   procedure Note_Retained_Stop (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message; Owners : String) is
   begin
      if Retained_Stop_Noted or else
        ACPI_System_Events.Classify (Publisher, Item) /= ACPI_System_Events.Stop_Requested
      then return; end if;
      -- Consume before publication. Shed records must not cause an event
      -- stream to retry opening/publishing at an unresolved owner boundary.
      Retained_Stop_Noted := True;
      ACPI_Native_Log.Copied ("acpi: stop refused RETAINED " & Owners, Peer_Quiet => False);
   end Note_Retained_Stop;
   procedure Handle (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message; Stop : out Boolean; Handled : out Boolean) is
      Noted : Boolean;
   begin
      Stop := False; Handled := True;
      case ACPI_System_Events.Classify (Publisher, Item) is
         when ACPI_System_Events.Not_System => Handled := False;
         when ACPI_System_Events.Rejected | ACPI_System_Events.Ignored_Control => null;
         when ACPI_System_Events.Stop_Requested => Stop := True;
         when ACPI_System_Events.Channel_Wake =>
            CuBit.Channel_Bells.Note_Notice (Item.tag.label, Item.tag.length,
               [Item.words (0), Item.words (1), Item.words (2), Item.words (3)], Noted);
         when ACPI_System_Events.Grant_Notice =>
            ACPI_Native_Log.Grant_Notice (CV.Decode (Item.tag.label, Item.tag.length,
               Item.words (0), Item.words (1), Item.words (2)));
      end case;
   end Handle;
end ACPI_Native_Events;
