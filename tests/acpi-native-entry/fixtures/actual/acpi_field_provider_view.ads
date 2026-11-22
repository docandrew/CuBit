pragma Ada_2022;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
-- A typed callback view of one caller-owned provider. No context, hardware
-- authority, transport, namespace alias or lifetime is created here.
generic
   type Provider_Context is limited private;
   with function Is_Ready (IO : Provider_Context) return Boolean;
   with function Provider_Identity (IO : Provider_Context) return AML_Identity.Identity;
   with function Is_Pending (IO : Provider_Context) return Boolean;
   with function Count (IO : Provider_Context) return Natural;
   with procedure Export
     (IO : Provider_Context; Ordinal : Positive;
      Target : out AML_Field_Access.Target; Accepted : out Boolean);
   with procedure Select_Geometry
     (IO : in out Provider_Context; Geometry : AML_Field_Layout.Register_Geometry;
      For_Write : Boolean; Target : out AML_Field_Access.Target; Accepted : out Boolean);
   with procedure Describe_Target
     (IO : Provider_Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   with procedure Authorize_Target
     (IO : Provider_Context; Target : AML_Field_Access.Target;
      For_Write : Boolean; Accepted : out Boolean);
   with procedure Dispatch_Request
     (IO : in out Provider_Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response);
   with procedure Completion_Confirmed
     (IO : Provider_Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
   with procedure Resolve_Target
     (IO : in out Provider_Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean) is null;
   -- Whole-field callbacks preserve the same caller-owned provider context.
   -- Null defaults keep Lock-rule admission closed in existing providers.
   with procedure Begin_Transaction
     (IO : in out Provider_Context; Requires_Lock, For_Write : Boolean;
      Accepted : in out Boolean) is null;
   with procedure End_Transaction
     (IO : in out Provider_Context; Accepted : in out Boolean) is null;
   Locked_NVS_Reads : Boolean := False;
package ACPI_Field_Provider_View is
   Supports_Locked_NVS_Reads : constant Boolean := Locked_NVS_Reads;
   subtype Context is Provider_Context;
   function Ready (IO : Context) return Boolean renames Is_Ready;
   function Identity (IO : Context) return AML_Identity.Identity renames Provider_Identity;
   function Pending (IO : Context) return Boolean renames Is_Pending;
   function Register_Count (IO : Context) return Natural renames Count;
   procedure Export_Target
     (IO : Context; Ordinal : Positive; Target : out AML_Field_Access.Target; Accepted : out Boolean) renames Export;
   procedure Select_Target
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      For_Write : Boolean; Target : out AML_Field_Access.Target; Accepted : out Boolean) renames Select_Geometry;
   procedure Describe
     (IO : Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) renames Describe_Target;
   procedure Authorize
     (IO : Context; Target : AML_Field_Access.Target; For_Write : Boolean; Accepted : out Boolean) renames Authorize_Target;
   procedure Dispatch
     (IO : in out Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response) renames Dispatch_Request;
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean) renames Completion_Confirmed;
   procedure Resolve_Module_Target
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean) renames Resolve_Target;
   procedure Begin_Field
     (IO : in out Context; Requires_Lock, For_Write : Boolean;
      Accepted : in out Boolean) renames Begin_Transaction;
   procedure End_Field
     (IO : in out Context; Accepted : in out Boolean) renames End_Transaction;
end ACPI_Field_Provider_View;
