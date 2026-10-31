# Shared HID logic

Transport-independent HID report-descriptor parsing, field usages, input values and bindings, metadata, contact-frame assembly and touchpad-to-pointer policy. Shared by the HID-over-I2C driver and available to USB HID report-protocol drivers.

The transport strips its envelope before calling these packages. Report IDs belong to the HID report and remain present when the parsed descriptor uses them. The caller owns device identity, buffering, transport loss/reset and input publication. These packages perform no hardware or IPC operations. HID_Touchpad_Pointer also uses the shared Pointer_Pending policy from userspace/lib/input.

The nine packages were moved without source or API changes. Existing SPARK_Mode annotations and contracts retain their original scope; relocation is not a new proof run. Hosted composition tests remain under tests/hid-i2c-native and tests/gpio-hid-{i2c,input,interop}.
