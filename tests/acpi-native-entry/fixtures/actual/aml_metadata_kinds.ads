pragma Ada_2022;
package AML_Metadata_Kinds with SPARK_Mode, Pure is
   type Kind is (Scope_Metadata, Field_Metadata, Device_Metadata, Event_Metadata,
     Method_Metadata, Mutex_Metadata, Region_Metadata, Power_Metadata,
     Processor_Metadata, Thermal_Metadata, Buffer_Field_Metadata);
   for Kind use (Scope_Metadata => 0, Field_Metadata => 5, Device_Metadata => 6,
     Event_Metadata => 7, Method_Metadata => 8, Mutex_Metadata => 9,
     Region_Metadata => 10, Power_Metadata => 11, Processor_Metadata => 12,
     Thermal_Metadata => 13, Buffer_Field_Metadata => 14);
   subtype Snapshot_Kind is Kind
     with Static_Predicate => Snapshot_Kind in Device_Metadata | Thermal_Metadata;
   --  Captured descriptor subset: semantic class only. No source-node identity,
   --  hardware authority, or complete Device/Thermal descriptor clone is implied.
   --  The arena authenticates capture and owns the returned object handle.
   type Snapshot_Description is record
      Object_Type : Kind := Device_Metadata;
   end record;
   function Supported (Value : Snapshot_Description) return Boolean is
     (Value.Object_Type in Snapshot_Kind);
end AML_Metadata_Kinds;
