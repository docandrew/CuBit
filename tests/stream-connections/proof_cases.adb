with Interfaces; use Interfaces;
with CuBit.Protocols.Stream_Connections;
package body Proof_Cases with SPARK_Mode is
   use CuBit.Protocols.Stream_Connections;
   procedure Check (Item : Request; Evidence : Approvals) is
   begin
      if CuBit.Protocols.Stream_Connections.Check (Item, Evidence) =
        Connection_Allowed
      then
         pragma Assert
           (for all A in Action => Authorizes (Evidence (A), Item));
         pragma Assert (Valid (Item.Source.Reference));
         pragma Assert (Valid (Item.Destination.Reference));
         pragma Assert (Item.Controller_Instance /= 0);
         pragma Assert
           (Item.Binding.Identity /= 0 and Item.Binding.Generation /= 0);
         pragma Assert
           (Item.Source.Direction = Output and
            Item.Destination.Direction = Input);
         pragma Assert
           (Delivery.Compatible
              (Item.Source.Profile, Item.Destination.Profile));
      end if;
   end Check;
end Proof_Cases;
