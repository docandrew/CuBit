with CuBit.Config_Inspection;

package CuBit.Config_Reader is
   --  Owned result, no pointers into a shared singleton buffer. Each call
   --  creates its own generation-checked grant through the Config endpoint.
   procedure Query
     (Op : CuBit.Config_Inspection.Operation; Key : String;
      Value : out CuBit.Config_Inspection.Text;
      Result : out CuBit.Config_Inspection.Status;
      Context : CuBit.Config_Inspection.Context_ID :=
        CuBit.Config_Inspection.Machine_Context);
end CuBit.Config_Reader;
