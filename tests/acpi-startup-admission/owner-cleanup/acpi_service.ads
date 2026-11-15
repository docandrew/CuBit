-- Hosted test model only; never a native runtime/provider implementation.
package ACPI_Service is type State is record Own,Other,Notifications : Natural:=0; end record; end ACPI_Service;