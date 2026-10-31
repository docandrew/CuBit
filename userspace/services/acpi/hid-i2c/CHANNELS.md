# Typed HID/I2C channel descriptions

ACPI_HID_I2C_Channels prepares the existing I2C_Channel_Badges.Target and
GPIO_Channel_Badges.Interrupt_Input records from a copied HID resource
candidate. This is a pure data adapter, not a capability factory.

It preserves address mode, the full 32-bit speed, pin, trigger and polarity.
The complete original candidate remains alongside them, including descriptor
register, controller source paths, sharing, wake, pin configuration and vendor
bytes. It refuses producer resources, device-initiated transfers, zero speed,
out-of-domain seven-bit addresses, the no-pin marker and both-polarity level
inputs. Failure has no partial prepared payload. It never clamps speed, maps a
pin, performs IO or invents firmware values.

The owner must first establish a complete current Resolver observation and keep
the original resolved device and both controller descriptors. Preparation alone
does not authenticate a public Candidate value. Before use, revalidate that
observation in the same serialized Core lifetime. Controller admission must
separately accept the actual speed/address mode, sharing, vendor/configuration
semantics and GPIO geometry. The generic codec domain includes values that the
current DesignWare backend may refuse, such as ten-bit mode or excessive speed.
Do not interpret Prepared as backend acceptance, dependency completion,
initialization, power readiness, interrupt enrollment or touchpad readiness.

Only the trusted launcher can mint/stamp the actual endpoint authority. A
computed badge word does not grant access. Consumers need the hid-i2c, i2c-admission and gpio-admission source
directories. Native startup now instantiates the adapter and retains up to16
complete copied observations, including original device/controller identities.
The component lists both admission source directories. Launcher/export
integration remains separate work; the manifest grants no new authority.

Tests: tests/acpi-hid-owner joins actual AML discovery, source resolution and
channel preparation. It also checks address/speed/pin boundaries, all GPIO
trigger/polarity combinations, unsupported roles, exact codec round trips and
preservation of metadata not represented by the channel codecs. These are
hosted checks, not native execution, hardware validation or a new SPARK proof.
