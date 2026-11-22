pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Cached_Battery_Values;
with ACPI_Cached_Power_Values;
with ACPI_Cached_Battery_Codec;
generic
   type Owner_State (<>) is limited private;
   with package Codec is new ACPI_Cached_Battery_Codec (<>);
   with procedure Read_Batteries (Owner : Owner_State;
      Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Battery_Values.View);
package ACPI_Cached_Battery_Dispatch with SPARK_Mode is
   use type Codec.Decision;
   use type ACPI_Cached_Power_Values.Generation, ACPI_Cached_Power_Values.Availability;
   use type Codec.P.Subpage;
   type Outcome is (Not_Handled, Denied, Malformed, Stale, Index_Out_Of_Range, OK);
   type Reply is record
      Status : Outcome := Not_Handled;
      Data : Codec.Reply_Words := Codec.Empty_Reply;
   end record;
   function Valid_Selection (Selected : Codec.Selection) return Boolean is
     (Selected.Kind /= Codec.Read_Inventory or else
       (Codec.Valid_Location (Selected.Page, Selected.Slot)
         and then (Selected.Page = Codec.P.Inventory or else Selected.Expected > 0)))
     with Global => null, Annotate => (GNATprove, Inline_For_Proof);
   function Expected_Outcome (Selected : Codec.Selection; Value : ACPI_Cached_Battery_Values.View) return Outcome is
     (case Selected.Kind is
        when Codec.Not_Handled => Not_Handled,
        when Codec.Denied => Denied,
        when Codec.Malformed => Malformed,
        when Codec.Read_Inventory =>
          (if Selected.Expected /= 0 and then Selected.Expected /= Value.Revision then Stale
           elsif Selected.Slot > 0 and then Codec.Effective_Status (Value) = ACPI_Cached_Power_Values.Available
             and then Selected.Slot > Value.Count then Index_Out_Of_Range else OK))
     with Ghost, Global => null, Pre => Valid_Selection (Selected);
   function Expected_Reply (Selected : Codec.Selection; Value : ACPI_Cached_Battery_Values.View) return Reply is
     (Status => Expected_Outcome (Selected, Value),
      Data => (if Selected.Kind = Codec.Read_Inventory and then Expected_Outcome (Selected, Value) = OK
        then Codec.Encode (Value, Selected.Page, Selected.Slot) else Codec.Empty_Reply))
     with Ghost, Global => null, Pre => Valid_Selection (Selected),
       Annotate => (GNATprove, Inline_For_Proof);
   -- Pure decision on one already copied inventory. Revision comparison comes
   -- before slot lookup; unavailable inventories reveal no slot topology.
   function Resolve (Selected : Codec.Selection; Value : ACPI_Cached_Battery_Values.View) return Reply
     with Global => null, Pre => Valid_Selection (Selected),
       Annotate => (GNATprove, Inline_For_Proof),
       Post => Resolve'Result = Expected_Reply (Selected, Value);
   -- One read at most. Refused framing/authority and closed admission read
   -- nothing. The callback is a trusted normally-returning, passive copy;
   -- it cannot refresh AML, claim receipts or reenter this owner.
   procedure Dispatch (Owner : Owner_State; Stamp : ACPI_Protocol.Role_Tag;
      Request : Codec.Packet; Result : out Reply; Read_Allowed : Boolean)
     with Post =>
       (if not Read_Allowed or else Codec.Inspect (Request, Stamp).Kind /= Codec.Read_Inventory
        then Result = Resolve (Codec.Inspect (Request, Stamp), (others => <>)))
       and then (case Codec.Inspect (Request, Stamp).Kind is
         when Codec.Not_Handled => Result.Status = Not_Handled,
         when Codec.Denied => Result.Status = Denied,
         when Codec.Malformed => Result.Status = Malformed,
         when Codec.Read_Inventory => Result.Status in Stale | Index_Out_Of_Range | OK);
end ACPI_Cached_Battery_Dispatch;
