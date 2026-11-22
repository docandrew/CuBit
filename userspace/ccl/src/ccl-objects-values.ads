with CCL.Host_Values;
with CCL.VM;
with CCL.Resources;

--  Host values can own complete native objects, including nested products and
--  sums. The VM representation is currently narrower: unsupported VM shapes
--  fail explicitly, without flattening or string coercion.
package CCL.Objects.Values with SPARK_Mode is
   use type CCL.VM.Value_Kind;
   use type CCL.Resources.Reference;
   use type CCL.Host_Values.Value_Kind;
   procedure From_Host
     (Contract : Binding; Value : CCL.Host_Values.Value;
      Object : out Image; Accepted : out Boolean)
     with Post => (Validate (Object, Contract) or else not Accepted) and
       (Value.Kind /= CCL.Host_Values.Resource_Value or else not Accepted);
   type Host_Result (Available : Boolean := False) is record
      case Available is
         when True => Value : CCL.Host_Values.Value;
         when False => null;
      end case;
   end record;
   --  Return a fresh value. An out parameter could name a constrained
   --  discriminated host value and raise when its kind changes.
   function To_Host (Contract : Binding; Object : Image) return Host_Result;
   procedure From_VM
     (Contract : Binding; Local_Types : CCL.Types.Registry; Value : CCL.VM.Value;
      Object : out Image; Accepted : out Boolean)
     with Post => (Validate (Object, Contract) or else not Accepted) and
       ((Value.Copyable and Value.Type_Tag = 0) or else not Accepted) and
       ((Value.Kind /= CCL.VM.Resource_Value and Value.Resource = CCL.Resources.No_Reference) or else not Accepted);
   procedure To_VM
     (Contract : Binding; Local_Types : CCL.Types.Registry; Object : Image;
      Value : out CCL.VM.Value; Accepted : out Boolean);
   --  Local_Types must be the value/program's actual registry. Match its full
   --  nominal definition to the approved Contract, never just the numeric ID.
   --  Neither operation installs definitions or changes the schema's identity.
end CCL.Objects.Values;
