pragma Ada_2022;
with Channel_Wake_ABI;
with CuBit.Control_Events;
with CuBit.Grant_References;
with Interfaces;
package body HID_System_Events with SPARK_Mode is
   package M renames CuBit.Messages;
   package P renames CuBit.Process_IDs;
   package CV renames CuBit.Control_Events;
   package G renames CuBit.Grant_References;
   use type P.Process_ID, M.IPC_Flags, M.Protocol_Flags;
   use type CV.Control_Kind, CV.Event_Kind;
   use type Interfaces.Unsigned_32;
   function Classify (Publisher : P.Process_ID; Item : M.Message) return Action is
      Value : CV.Event;
   begin
      if Item.tag.label not in Channel_Wake_ABI.Wake_Label | CV.Grant_Revoked_Label |
        CV.Grant_Returned_Label | CV.Control_Label then return Not_System; end if;
      if Publisher /= P.No_Process or else Item.tag.protocol /= 0 or else
        Item.tag.ipc /= M.No_IPC_Flags or else Natural (Item.tag.reserved) /= 0
      then return Rejected; end if;
      if Item.tag.label = Channel_Wake_ABI.Wake_Label then
         if Natural (Item.tag.length) not in 1 .. Channel_Wake_ABI.Wakes_Per_Event then return Rejected; end if;
         for Index in 0 .. Natural (Item.tag.length) - 1 loop
            if not G.Valid_Wire (Item.words (Index)) then return Rejected; end if;
         end loop;
         return Channel_Wake;
      end if;
      Value := CV.Decode (Item.tag.label, Item.tag.length,
         Item.words (0), Item.words (1), Item.words (2));
      case Value.Kind is
         when CV.Grant_Revoked | CV.Grant_Returned =>
            if Value.Slot not in G.Global_Slot or else Value.Generation not in G.Generation then return Rejected; end if;
            return Grant_Notice;
         when CV.Control =>
            return (if Value.Control = CV.Stop then Stop_After_Drain else Ignored_Control);
         when CV.Not_Ours => return Rejected;
      end case;
   end Classify;
end HID_System_Events;
