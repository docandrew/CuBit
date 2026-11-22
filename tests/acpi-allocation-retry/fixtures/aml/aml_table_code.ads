with AML_Table_Backing;
package AML_Table_Code with SPARK_Mode is
   -- Read-only transport of an owned table body. This validates physical bounds,
   -- not table provenance, AML semantics, execution authority or import order.
   type View_Status is (Invalid_Span, Body_Visited);
   generic
      with procedure Consume (Input : aliased AML_Table_Backing.State;
        Offset, Length : Natural);
   procedure Visit_Body (Input : aliased AML_Table_Backing.State;
     Table : Positive; Status : out View_Status);
end AML_Table_Code;
