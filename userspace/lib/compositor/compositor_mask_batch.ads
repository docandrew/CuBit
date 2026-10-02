with Compositor_Affine;
package Compositor_Mask_Batch with SPARK_Mode, Pure is
   package A renames Compositor_Affine;
   use type A.Word;
   use type A.G.Physical_Extent;
   Maximum : constant := 32;
   subtype Count is Natural range 0 .. Maximum;
   subtype Index is Positive range 1 .. Maximum;
   subtype Mask_Index is Natural range 0 .. 127;
   type Command is record
      Mask : Mask_Index := 0;
      Description : A.Draw;
      Tint : A.Word := 0;
   end record;
   type Commands is array (Index) of Command;
   type Packet is record
      Width, Height : A.G.Physical_Extent := 1;
      Length : Count := 0;
      Items : Commands;
   end record;
   function Valid (P : Packet) return Boolean is
     (for all I in 1 .. P.Length => P.Items (I).Description.Over = 1 and then
       A.Valid (P.Items (I).Description, P.Width, P.Height));
   -- Caller retains one read lease for every admitted command until confirmed
   -- batch completion. Packet metadata alone conveys no storage authority.
   procedure Append (P : in out Packet; Value : Command; Accepted : out Boolean)
     with Pre => Valid (P), Post => Valid (P) and
       P.Width = P.Width'Old and P.Height = P.Height'Old and
       Accepted = (P.Length'Old < Maximum and Value.Description.Over = 1 and
                    A.Valid (Value.Description, P.Width, P.Height)) and
       (if not Accepted then P = P'Old else P.Length = P.Length'Old + 1 and
         P.Items (P.Length) = Value and
         (for all I in Index => (if I /= P.Length then P.Items (I) = P.Items'Old (I))));
end Compositor_Mask_Batch;
