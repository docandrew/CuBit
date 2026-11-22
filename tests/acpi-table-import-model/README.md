# Historical grant-import test transport

These two units are an exact test-support copy of the retired native grant
adapter. They preserve scalar table/AML/import and grant-model regression tests.
They are not a production service, capability authentication implementation or
the protocol exposed by acpi.svc. Do not add this directory to a component.ccl.

The direct native entry uses Startup.Tables and the fixed observer badge. It
has no provider endpoint, Configure handshake or upload request authority.
Tests using this transport must identify grant/IPC behavior as modeled and must
not report it as current native-driver coverage. The actual Core/Requests under
the model remain production code and retain independent direct-startup coverage.
