#include <ntddk.h>

#ifndef NDEBUG
#define NDEBUG
#endif
#include <debug.h>

VOID NTAPI DriverUnload(IN PDRIVER_OBJECT DriverObject);

NTSTATUS NTAPI DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING RegistryPath)
{
    DPRINT1("Okutin Denis IU9-41B\n");
    DriverObject->DriverUnload = DriverUnload;
    return STATUS_SUCCESS;
}

VOID NTAPI DriverUnload(IN PDRIVER_OBJECT DriverObject)
{
    DPRINT1("Okutin Denis IU9-41B\n");
}
